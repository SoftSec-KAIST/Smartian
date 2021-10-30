module EVMAnalysis.Semantics.Evaluate

open B2R2
open B2R2.BinIR
open B2R2.BinIR.LowUIR
open EVMAnalysis
open EVMAnalysis.Const
open EVMAnalysis.Domain.Taint
open EVMAnalysis.Domain.AbsVal
open EVMAnalysis.Domain.State

let rec isStackPtr exp =
  match exp with
  | Num _ -> false
  | Var (_, _, r, _) -> r = STACK_PTR_REG
  | TempVar _ -> false
  | UnOp _ -> false
  | BinOp (_, _, { E = e1 }, { E = e2 }, _) -> isStackPtr e1 || isStackPtr e2
  | RelOp _ -> false
  | Extract _ -> false
  | Cast (CastKind.ZeroExt, 256<rt>, { E = e }, _) -> isStackPtr e
  | Cast _ -> false
  | Load _ -> false
  | FuncName _ | Ite _ | Name _ | Nil | PCVar _ | Undefined _ -> false

let rec eval valExp state =
  match valExp with
  | Num bv -> AbsVal.ofBigInt (bv.BigValue())
  | Var (_, _, r, _) -> State.read r state
  | TempVar (_, tmpNo) -> State.readTmp tmpNo state
  | UnOp (opType, { E = e }, _) -> AbsVal.unOp opType (eval e state)
  | BinOp (BinOpType.APP, _, _, _, _) -> failwith "Unfiltered external function"
  | BinOp (opType, _, { E = exp1 }, { E = exp2 }, _) ->
    AbsVal.binOp opType (eval exp1 state) (eval exp2 state)
  | RelOp _ -> failwith "Unfiltered relational operation"
  | Extract (e, 1<rt>, 0, _) -> eval e.E state |> AbsVal.extractLSBit
  | Extract (e, 8<rt>, 0, _) -> eval e.E state |> AbsVal.extractLSByte
  | Extract _ -> AbsVal.unknown
  | Cast (CastKind.ZeroExt, 256<rt>, { E = e }, _) -> eval e state
  | Cast _ -> AbsVal.unknown
  | Load (_, _, { E = addrExp}, _) ->
    let addr = eval addrExp state
    if isStackPtr addrExp then State.loadStack addr state
    else State.loadMemory addr state
  | FuncName _ | Name _ | Nil | Undefined _ -> AbsVal.bot
  | Ite _ | PCVar _ -> AbsVal.unknown

let private isSenderCheck v1 v2 funcInfo =
  let isTaintedWithConstrVar v =
    let checker var = AbsVal.hasTaint (Taint.ofVars (Set.singleton var)) v
    Set.exists checker funcInfo.ConstrTainted
  // 'msg.sender == owner', or 'owner == msg.sender'.
  (AbsVal.hasTaint Taint.Caller v1 && isTaintedWithConstrVar v2) ||
  (isTaintedWithConstrVar v1 && AbsVal.hasTaint Taint.Caller v2)

let evalRelOp opType e1 e2 state funcInfo =
  let v1 = eval e1 state
  let v2 = eval e2 state
  let funcInfo = if opType = RelOpType.EQ && isSenderCheck v1 v2 funcInfo
                 then FuncInfo.addCheckSender funcInfo
                 else funcInfo
  funcInfo, AbsVal.relOp opType v1 v2
