namespace EVMAnalysis

open System.Collections.Generic

open EVMAnalysis.Domain.AbsVal
open EVMAnalysis.Domain.State

/// Information needed during the abstract interpretation algorithm.
type Record = {
  StackEntryMap : Dictionary<Context, Subrtn * AbsVal>
  StackLiftMap: Dictionary<Subrtn, bigint>
}

module Record =

  let init () =
    { StackEntryMap = new Dictionary<Context, Subrtn * AbsVal>()
      StackLiftMap = new Dictionary<Subrtn, bigint>() }

  let addCallInfo localInfo callCtx callerSubrtn state =
    let stackPtr = State.getStackPtr state
    localInfo.StackEntryMap.[callCtx] <- (callerSubrtn, stackPtr)

  let addRetInfo localInfo callCtx state =
    if not (localInfo.StackEntryMap.ContainsKey(callCtx)) then
      failwithf "Call context %s is not recorded" (Context.toString callCtx)
    let subrtn, initStackPtr = localInfo.StackEntryMap.[callCtx]
    let finStackPtr = State.getStackPtr state
    match AbsVal.tryGetConst initStackPtr, AbsVal.tryGetConst finStackPtr with
    | Some x, Some y -> localInfo.StackLiftMap.[subrtn] <- (y - x)
    | _ -> printfn "[Warning] Failed to analyze stack lift of %x" subrtn

  let hasLiftInfo subrtn localInfo =
    localInfo.StackLiftMap.ContainsKey(subrtn)

  let getLiftInfo subrtn localInfo =
    localInfo.StackLiftMap.[subrtn]
