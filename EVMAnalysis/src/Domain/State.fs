module EVMAnalysis.Domain.State

open EVMAnalysis
open EVMAnalysis.Const
open EVMAnalysis.Domain.AbsVal
open EVMAnalysis.Domain.RegMap
open EVMAnalysis.Domain.Stack
open EVMAnalysis.Domain.Memory

type PartitionIdxModule () =
  inherit Elem<PartitionIdx>()
  override __.toString idx = PartitionIdx.toString idx

let PartitionIdx = PartitionIdxModule () // Use 'PartitionIdx' like a module.

type State = RegMap * Stack * Memory

type StateModule () =
  inherit Prod3Domain<RegMap, Stack, Memory>(RegMap, Stack, Memory)

  override __.toString state =
    let regMap = __.getRegMap state
    let regMapStr = RegMap.toString regMap
    let stack = __.getStack state
    let stackStr = Stack.toString stack
    let memory = __.getMemory state
    let memStr = Memory.toString memory
    "(RegMap)" + regMapStr + "\n" +
    "(Stack)" + stackStr + "\n" +
    "(Memory)" + memStr + "\n" +
    "==========="

  member __.init: State =
    // You can use any value as the initial SP value. Use a distinctive (not 0)
    // value to help debugging.
    let initSP = AbsVal.ofBigInt INIT_STACK_PTR_VAL
    let initRegMap = RegMap.add STACK_PTR_REG initSP RegMap.bot
    (initRegMap, Stack.bot, Memory.bot)

  member __.getStackPtr (state: State): AbsVal =
    RegMap.find STACK_PTR_REG (__.getRegMap state)

  member __.getRegMap (state: State): RegMap =
    let regMap, _, _ = state
    regMap

  member __.setRegMap (state: State) (regMap: RegMap): State =
    let _, stack, memory = state
    regMap, stack, memory

  member __.getStack (state: State): Stack =
    let _, stack, _ = state
    stack

  member __.liftStackPtr delta state =
    match AbsVal.tryGetConst (__.getStackPtr state) with
    | None -> state
    | Some i ->
      let regMap = __.getRegMap state
      let newStackPtr = AbsVal.ofBigInt (i + delta)
      let newRegMap = RegMap.add STACK_PTR_REG newStackPtr regMap
      __.setRegMap state newRegMap

  member __.setStack (state: State) (stack: Stack): State =
    let regMap, _, memory = state
    regMap, stack, memory

  member __.getMemory (state: State): Memory =
    let _, _, mem = state
    mem

  member __.setMemory (state: State) (memory: Memory): State =
    let regMap, stack, _ = state
    regMap, stack, memory

  member __.read reg state =
    RegMap.find reg (__.getRegMap state)

  member __.readTmp tmpNo state =
    let reg = Register.ofTmpVar tmpNo
    __.read reg state

  member __.update reg v state =
    RegMap.add reg v (__.getRegMap state) |> __.setRegMap state

  member __.updateTmp tmpNo v state =
    let reg = Register.ofTmpVar tmpNo
    __.update reg v state

  member __.loadStack addr state =
    match AbsVal.tryGetConst addr with
    | None -> AbsVal.bot // XXX. Violate monotonicity inevitably.
    | Some i -> Stack.find i (__.getStack state)

  member __.storeStack addr v state =
    match AbsVal.tryGetConst addr with
    | None -> state // XXX. Violate monotonicity inevitably.
    | Some i -> Stack.add i v (__.getStack state) |> __.setStack state

  member __.loadMemory addr state =
    match AbsVal.tryGetConst addr with
    | None -> AbsVal.bot // XXX. Violate monotonicity inevitably.
    | Some i -> Memory.find i (__.getMemory state)

  member __.storeMemory addr v state =
    match AbsVal.tryGetConst addr with
    | None -> state // XXX. Violate monotonicity inevitably.
    | Some i -> Memory.add i v (__.getMemory state) |> __.setMemory state

  member __.stackContainsVal v state =
    Stack.containsVal v (__.getStack state)

let State = StateModule () // Use 'State' like a module.

type StateMap = Map<PartitionIdx, State>

type StateMapModule () =
  inherit FunDomain<PartitionIdx, State>(PartitionIdx, State)

let StateMap = StateMapModule () // Use 'StateMap' like a module.
