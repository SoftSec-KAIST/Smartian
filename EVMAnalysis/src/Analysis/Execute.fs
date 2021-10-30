module EVMAnalysis.Semantics.Execute

open B2R2
open B2R2.BinIR
open B2R2.BinIR.LowUIR
open B2R2.MiddleEnd.BinEssence
open EVMAnalysis
open EVMAnalysis.Const
open EVMAnalysis.Domain.AbsVal
open EVMAnalysis.Domain.State
open EVMAnalysis.Semantics.Evaluate
open EVMAnalysis.Semantics.ExternFunc

type TransferType =
  | Call of Caller: Addr * Callee: Addr
  | RecursiveCall
  | Ret of Addr
  | Normal

let private isCall curAddr state =
  State.stackContainsVal (AbsVal.ofAddr (curAddr + 1UL)) state

let private isReturn dstAddr (ctx: Context) =
  match ctx with
  | headAddr :: _ -> dstAddr = headAddr + 1UL
  | _ -> false

let private execStmt ctx hasCall hasRec addr (state, funcInfo, transType) stmt =
  match stmt with
  | ISMark _ | IEMark _ |  LMark _ -> (state, funcInfo, transType)
  // First, we handle cases where expression evaluation can have side effect on
  // the abstract state 'state' or analysis result 'funcInfo'.
  // T_n = EXTERN_FUNC(...)
  | Put ({ E = TempVar (_, tmpNo)},
         { E = BinOp(BinOpType.APP, _, { E = e1 }, { E = e2 }, _) }
        ) ->
    let state, funcInfo, ret = runExtern addr e1 e2 state funcInfo
    (State.updateTmp tmpNo ret state, funcInfo, transType)
  // [...] = EXTERN_FUNC(...)
  | Store (_, { E = addrExp },
              { E = BinOp(BinOpType.APP, _, { E = e1 }, { E = e2 }, _) }
          ) ->
    let state, funcInfo, ret = runExtern addr e1 e2 state funcInfo
    let addr = eval addrExp state
    if isStackPtr addrExp
    then (State.storeStack addr ret state, funcInfo, transType)
    else (State.storeMemory addr ret state, funcInfo, transType)
  // T_n = zext(e1 relOp e2)
  | Put ({ E = TempVar (_, tmpNo)},
         { E = Cast (CastKind.ZeroExt, 256<rt>,
                     { E = RelOp(opType, { E = e1 }, { E = e2 }, _) }, _) }
        ) ->
    let funcInfo, v = evalRelOp opType e1 e2 state funcInfo
    (State.updateTmp tmpNo v state, funcInfo, transType)
  // [...] = zext(e1 relOp e2)
  | Store (_, { E = addrExp },
              { E = Cast (CastKind.ZeroExt, 256<rt>,
                          { E = RelOp(opType, { E = e1 }, { E = e2 }, _) }, _) }
          ) ->
    let funcInfo, v = evalRelOp opType e1 e2 state funcInfo
    let addr = eval addrExp state
    if isStackPtr addrExp
    then (State.storeStack addr v state, funcInfo, transType)
    else (State.storeMemory addr v state, funcInfo, transType)
  // For other cases, expression evaluation does not affect state or funcInfo.
  | Put ({ E = Var (_, _, reg, _)}, { E = valExp }) ->
    (State.update reg (eval valExp state) state, funcInfo, transType)
  | Put ({ E = TempVar (_, tmpNo)}, { E = valExp }) ->
    (State.updateTmp tmpNo (eval valExp state) state, funcInfo, transType)
  | Put _ -> failwith "Unexpected assignment syntax"
  | Store (_, { E = addrExp }, { E = valExp }) ->
    let addr = eval addrExp state
    let v = eval valExp state
    if isStackPtr addrExp
    then (State.storeStack addr v state, funcInfo, transType)
    else (State.storeMemory addr v state, funcInfo, transType)
  | Jmp _ | CJmp _ -> failwith "Unexpected intra-block control flow"
  | InterJmp ({ E = dstExp }, _) ->
    let dstVal = eval dstExp state
    match AbsVal.tryGetConst dstVal with
    | None -> // This can happen in a contract with arbitrary jumps (SWC-127).
      printfn "[Warning] InterJump to unknown dst @ %x" addr
      (state, funcInfo, transType)
    | Some i ->
      let dst = Addr.ofBigInt i
      // TODO: Try clean-up with 'hasCall' only.
      if hasCall && isCall addr state then (state, funcInfo, Call (addr, dst))
      elif hasRec then (state, funcInfo, RecursiveCall)
      elif isReturn dst ctx then (state, funcInfo, Ret dst)
      else (state, funcInfo, transType) // Normal jump.
  | InterCJmp _ -> (state, funcInfo, transType) // No pruning semantics.
  | SideEffect _ -> (state, funcInfo, transType)

let private checkValidSP addr state =
  let spVal = State.read STACK_PTR_REG state
  match AbsVal.tryGetConst spVal with
  | Some _ -> ()
  | None -> failwithf "Invalid SP: %s @ %x" (AbsVal.toString spVal) addr

let private execInstr ctx hasCall hasRec (state, funcInfo, trans) ins =
  let addr = ins.Instruction.Address
  checkValidSP addr state
  Array.map (fun s -> s.S) ins.Stmts
  |> Array.fold (execStmt ctx hasCall hasRec addr) (state, funcInfo, trans)

let execBB ctx hasCall hasRec bb state funcInfo =
  BasicBlock.getInstrs bb
  |> Array.fold (execInstr ctx hasCall hasRec) (state, funcInfo, Normal)
