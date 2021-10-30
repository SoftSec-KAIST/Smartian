module EVMAnalysis.Semantics.ExternFunc

open B2R2.BinIR
open B2R2.BinIR.LowUIR
open EVMAnalysis
open EVMAnalysis.Domain.Taint
open EVMAnalysis.Domain.AbsVal
open EVMAnalysis.Domain.State
open EVMAnalysis.Semantics.Evaluate

let rec private consToList argExp =
  match argExp with
  | Nil -> []
  | BinOp (BinOpType.CONS, _, { E = e1 }, { E = e2 }, _) -> e1 :: consToList e2
  | _ -> failwithf "Unexpected arg expr: %s" (Pp.expToString { E = argExp } )

// We are only interested in SHA3 calculation on 64-byte chunk, which is used
// for calculating the key address of a 'mapping' type variable.
let private runSha3 offset length state funcInfo =
  match AbsVal.tryGetConst offset, AbsVal.tryGetConst length with
  | Some off, Some len when len = 32I ->
    let idVal = State.loadMemory (AbsVal.ofBigInt off) state
    (state, funcInfo, AbsVal.sha32Byte idVal)
  | Some off, Some len when len = 64I ->
    let pos = off + 32I // This is where the ID of a map is stored.
    let idVal = State.loadMemory (AbsVal.ofBigInt pos) state
    (state, funcInfo, AbsVal.sha64Byte idVal)
  | _ -> (state, funcInfo, AbsVal.unknown) // TODO: consider taint propagation.

let rec private storeConstrArg accState dst argNum idx =
  if idx >= argNum then accState
  else let addr = AbsVal.ofBigInt (dst + idx * 32I)
       let accState = State.storeMemory addr AbsVal.ConstrArg accState
       storeConstrArg accState dst argNum (idx + 1I)

let private runCodeCopy dstVal state funcInfo =
  let argNum = funcInfo.FuncSpec.ArgSpecs.Length - 1 // Exclude ether value arg.
  printfn "Found codecopy(%s, _, _), arg# = %d" (AbsVal.toString dstVal) argNum
  match AbsVal.tryGetConst dstVal with
  | Some dst when dst <> 0I -> // dst != 0 means constructor arg copy
    storeConstrArg state dst (bigint argNum) 0I, funcInfo, AbsVal.bot
  | _ -> state, funcInfo, AbsVal.bot

let private handleFunc addr funcName args state funcInfo =
  // For now, only handle sstore/sload/sha3, and don't care about the return.
  match funcName, args with
  | "sstore", [k; v] ->
    printfn "Found sstore(%s, %s) @ %s"
      (AbsVal.toString k) (AbsVal.toString v) (Addr.toString addr)
    (state, FuncInfo.addSStore k v funcInfo, AbsVal.bot)
  | "sload", [k] ->
    printfn "Found sload(%s) @ %s" (AbsVal.toString k) (Addr.toString addr)
    let retVal = AbsVal.toVariables k |> Taint.ofVars |> AbsVal.ofTaint
    (state, FuncInfo.addSLoad k funcInfo, retVal)
  | "keccak256", [offset; len] -> runSha3 offset len state funcInfo
  | "codecopy", [dst; _; _] -> runCodeCopy dst state funcInfo
  | "msg.sender", [] -> state, funcInfo, AbsVal.Caller
  | "exp", [b; e] -> state, funcInfo, AbsVal.exp b e
  // Add more semantics if needed.
  | _ -> state, funcInfo, AbsVal.unknown

let runExtern addr funcExp argExp state funcInfo =
  match funcExp with
  | FuncName fName ->
    let args = consToList argExp |> List.map (fun e -> eval e state)
    handleFunc addr fName args state funcInfo
  | _ -> failwithf "Unexpected func expr: (%s)" (Pp.expToString { E = funcExp })
