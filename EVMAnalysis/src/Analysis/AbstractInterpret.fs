module EVMAnalysis.AbstractInterpret

open EVMAnalysis
open EVMAnalysis.Domain.State
open EVMAnalysis.Semantics.Execute

let private recordCallRet record ctx state transType =
  match transType with
  | Call (callerAddr, calleeAddr) ->
    Record.addCallInfo record (callerAddr :: ctx) calleeAddr state
  | Ret _ -> Record.addRetInfo record ctx state
  | RecursiveCall | Normal -> ()

let private postProcess record cfg state transType =
  match transType with
  | Call _ | Ret _ | Normal -> state
  | RecursiveCall ->
    let stackLift = Record.getLiftInfo (BasicBlock.getAddr cfg.Root) record
    State.liftStackPtr stackLift state

let private getPropagateIdx cfg ctx bb curAddr transType: (PartitionIdx list) =
  match transType with
  | Call (callerAddr, calleeAddr) -> [(callerAddr :: ctx, calleeAddr)]
  | Ret retAddr -> if List.isEmpty ctx then [] else [(List.tail ctx, retAddr)]
  | RecursiveCall | Normal -> // Note that recursive calls are also back-edges.
    let formsBackEdge succAddr = CFG.isBackEdge (curAddr, succAddr) cfg
    CFG.getSuccs bb cfg
    |> List.map BasicBlock.getAddr
    |> List.filter (not << formsBackEdge)
    |> List.map (fun addr -> (ctx, addr))

let private propagetState newState stateMap idx =
  let oldState = StateMap.find idx stateMap
  let joinedState = State.join oldState newState
  StateMap.add idx joinedState stateMap

let private propagate cfg ctx bb curAddr stateMap outState transType =
  let propagateIndices = getPropagateIdx cfg ctx bb curAddr transType
  List.fold (propagetState outState) stateMap propagateIndices

let private getNextWorks cfgs curCtx curCFG curBB transType =
  match transType with
  | Call (callerAddr, calleeAddr) -> // Add the BBs of the callee subroutine.
    let newCtx = callerAddr :: curCtx
    let calleeCFG = Map.find calleeAddr cfgs
    CFG.topologicalSort calleeCFG calleeCFG.Root
    |> List.map (fun bb -> (newCtx, calleeCFG, bb))
  | RecursiveCall -> // Add the reachable BBs from the current block.
    CFG.topologicalSort curCFG curBB // Take caution to not add 'curBB' again.
    |> List.filter (fun bb -> BasicBlock.getAddr bb <> BasicBlock.getAddr curBB)
    |> List.map (fun bb -> (curCtx, curCFG, bb))
  | Ret _ | Normal -> [] // Already added at the function entry.

let rec private fixpoint cfgs record (stateMap, fInfo) works =
  let hasState ctx bb = StateMap.contains (ctx, BasicBlock.getAddr bb) stateMap
  let hasRec cfg bb = List.contains cfg.Root (CFG.getSuccs bb cfg)
  let hasLiftInfo cfg = Record.hasLiftInfo (BasicBlock.getAddr cfg.Root) record
  match works with
  | [] -> fInfo
  | (ctx, _, bb) :: tailWorks when not (hasState ctx bb) ->
    // If execution had halted in the predecessor, no input state will come in.
    fixpoint cfgs record (stateMap, fInfo) tailWorks
  | (ctx, cfg, bb) :: tailWorks when hasRec cfg bb && not (hasLiftInfo cfg) ->
    let newWorkList = WorkList.deferUntilReturn (ctx, cfg, bb) tailWorks
    fixpoint cfgs record (stateMap, fInfo) newWorkList
  | (ctx, cfg, bb) :: tailWorks ->
    let hasCall = CFG.hasCall bb cfg
    let hasRec = hasRec cfg bb
    let addr = BasicBlock.getAddr bb
    let inState = StateMap.find (ctx, addr) stateMap
    let outState, fInfo, transType = execBB ctx hasCall hasRec bb inState fInfo
    recordCallRet record ctx outState transType
    let finalState = postProcess record cfg outState transType
    let stateMap = propagate cfg ctx bb addr stateMap finalState transType
    let nextWorks = getNextWorks cfgs ctx cfg bb transType
    let newWorkList = WorkList.push nextWorks tailWorks
    fixpoint cfgs record (stateMap, fInfo) newWorkList

let run cfgs constrTainted func =
  let name = FuncSpec.getName func
  printfn "[DEBUG] Analyzing %s @ %x" name func.Entry
  let cfg = Map.find func.Entry cfgs
  let entryBB = if func.Kind = Fallback then CFG.findFallback cfg else cfg.Root
  let entryAddr = BasicBlock.getAddr entryBB
  let record = Record.init()
  let initStateMap = StateMap.add ([], entryAddr) State.init StateMap.bot
  let funcInfo = FuncInfo.init func constrTainted
  let workList = WorkList.init cfg entryBB
  fixpoint cfgs record (initStateMap, funcInfo) workList
