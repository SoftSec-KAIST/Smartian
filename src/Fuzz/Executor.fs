module Smartian.Executor

open System.IO
open System.Collections.Generic
open Nethermind.Dirichlet.Numerics
open Nethermind.Core.Test.Builders
open Nethermind.Core.Specs
open Nethermind.Evm
open Nethermind.Evm.Test
open Nethermind.Evm.Tracing
open Nethermind.Logging
open Nethermind.Store
open Config
open BytesUtils
open Options

type Env = {
  State : StateProvider
  SpecProvider: MainNetSpecProvider
  VM : VirtualMachine
  TxProcessor : TransactionProcessor
}

type Feedback = {
  CovGain : bool
  DUGain : bool
  // PC, Op, Oprnd1, Oprnd2.
  CmpList : (uint64 * string * bigint * bigint) list
  // Bug class, Bug PC, Triggerring TX index.
  BugSet : Set<(BugClass * int * int)>
}

// Set of edge hashes.
let mutable accumDeployEdges = SortedSet<int>()
let mutable accumRuntimeEdges = SortedSet<int>()
// Set of program counters.
let mutable accumRuntimeInstrs = SortedSet<int>()
// Set of (Def PC * Use PC * Storage Index)
let mutable accumDUChains = SortedSet<struct(int * int * UInt256)>()
// Set of (BugClass * PC)
let mutable accumBugs = Set.empty

let mutable deployFailCount = 0

let mutable receivedEther = false
let mutable useDelegateCall = false
let mutable canSendEther = false

let mutable private targCode = [||]
let mutable private smartianAgentCode = [||]
let mutable private sFuzzAgentCode = [||]

let initialize targetPath =
  targCode <- File.ReadAllText(targetPath) |> hexStrToBytes
  let srcDir = Directory.GetParent(__SOURCE_DIRECTORY__).FullName
  let smartianAgentPath = Path.Join(srcDir, "Agent/AttackerContract.bin")
  smartianAgentCode <- File.ReadAllText(smartianAgentPath) |> hexStrToBytes
  let sFuzzAgentPath = Path.Join(srcDir, "Agent/SFuzzContract.bin")
  sFuzzAgentCode <- File.ReadAllText(sFuzzAgentPath) |> hexStrToBytes

let private initTestingEnv () =
  let logger = LimboLogs.Instance
  let codeDb = new StateDb()
  let stateDb = new StateDb()
  let state = StateProvider(stateDb, codeDb, logger)
  let storage = StorageProvider(stateDb, state, logger)
  let spec = MainNetSpecProvider.Instance
  let blockHash = TestBlockhashProvider()
  let vm = VirtualMachine(state, storage, blockHash, spec, logger)
  let processor = TransactionProcessor(spec, state, storage, vm, logger)
  state.Commit(spec.GenesisSpec)
  { State = state
    SpecProvider = spec
    VM = vm
    TxProcessor = processor }

let private runTx env from ``to`` code reqAddr value data timestamp blocknum =
  let processor = env.TxProcessor
  let tracer = CallOutputTracer()
  let block = Build.A.Block.WithTimestamp(UInt256(timestamp: int64))
                           .WithNumber(blocknum)
                           .WithGasLimit(BLOCK_GASLIMIT)
                           .WithBeneficiary(Address.MINER)
                           .TestObject
  let tx = Nethermind.Core.Transaction(SenderAddress = from,
                                       To = ``to``,
                                       Init = code,
                                       DeployAddress = reqAddr,
                                       Value = value,
                                       Data = data,
                                       GasLimit = TX_GASLIMIT,
                                       GasPrice = UInt256 (TX_GASPRICE: int64))
  processor.Execute(tx, block.Header, tracer)
  tracer.StatusCode

let private deploy env deployer addr code value data timestamp blocknum =
  let code = Array.append code data
  let status = runTx env deployer null code addr value [| |] timestamp blocknum
  if status <> StatusCode.Success then deployFailCount <- deployFailCount + 1

let private setupAgent env deployer addr agentCode =
  let timestamp = DEFAULT_TIMESTAMP
  let blocknum = DEFAULT_BLOCKNUM
  deploy env deployer addr agentCode (UInt256 0L) [||] timestamp blocknum

let private setupEntity env tc entity =
  let state = env.State
  let vm = env.VM
  let specProvider = env.SpecProvider
  let targDeployer = tc.TargetDeployer
  let deployTx = tc.DeployTx
  let spec = specProvider.GetSpec(deployTx.Blocknum)
  match entity.Agent with
  | NoAgent ->
    state.CreateAccount(entity.Account, &entity.Balance)
    if targDeployer <> entity.Account then vm.RegisterUser(entity.Account)
  | SFuzzAgent contAddr ->
    let zero = UInt256(0I)
    state.CreateAccount(entity.Account, &zero)
    setupAgent env entity.Account contAddr sFuzzAgentCode
    state.AddToBalance(contAddr, &entity.Balance, spec)
    // sFuzz doesn't distinguish deployer and user.
  | SmartianAgent contAddr ->
    let zero = UInt256(0I)
    state.CreateAccount(entity.Account, &zero)
    setupAgent env entity.Account contAddr smartianAgentCode
    state.AddToBalance(contAddr, &entity.Balance, spec)
    if targDeployer <> contAddr then vm.RegisterUser(contAddr)

let private setupTarget env deployer addr tx =
  let vm = env.VM
  let value = tx.Value
  let data = tx.Data
  let timestamp = tx.Timestamp
  let blocknum = tx.Blocknum
  vm.IsDeployingTarget <- true
  deploy env deployer addr targCode value data timestamp blocknum
  vm.IsDeployingTarget <- false
  vm.TargetContractAddr <- addr
  vm.TargetOwnerAddr <- deployer

let private sendTx env isAcc hadDepTx isRedirect tx =
  let vm = env.VM
  vm.ResetPerTx()
  vm.HadDeployerTx <- hadDepTx
  vm.IsRedirected <- isRedirect
  runTx env tx.From tx.To null null tx.Value tx.Data tx.Timestamp tx.Blocknum
  |> ignore
  if isAcc then
    accumDeployEdges.UnionWith(vm.VisitedDeployEdges)
    accumRuntimeEdges.UnionWith(vm.VisitedRuntimeEdges)
    accumRuntimeInstrs.UnionWith(vm.VisitedRuntimeInstrs)
    accumDUChains.UnionWith(vm.DefUseChainSet)
    accumBugs <- Set.ofSeq vm.BugSet
                 |> Set.map (fun struct (bugClass, pc) -> bugClass, pc)
                 |> Set.union accumBugs

// Check ether gain of users only if there was no previous deployer TX, because
// such TX can transfer the ownership to other users. Also, we check against
// both the initial balance and the (immediate) previous balance to make sure
// that an attacker is actively, not passively, gaining ether.
let private checkEtherLeak env isAcc sender hadDepTx initBal prevBal accBugs =
  let bug = (BugClass.EtherLeak, env.VM.BugOracle.LastEtherSendPC)
  if not isAcc || Set.contains bug accumBugs || hadDepTx then accBugs
  elif env.State.GetBalance(sender) <= initBal then accBugs
  elif env.State.GetBalance(sender) <= prevBal then accBugs
  else accumBugs <- Set.add bug accumBugs
       Set.add bug accBugs

let private processTx env tc isAcc (accNewBugs, hadDepTx) i tx =
  // Since we removed the foremost deploying transaction, sould +1 to the index.
  let i = i + 1
  let sender = tx.From
  let isDepTx = (sender = tc.TargetDeployer)
  let hadDepTx = hadDepTx || isDepTx
  let initBal, isRedirect =
    match List.tryFind (fun e -> Entity.getSender e = sender) tc.Entities with
    | Some entity -> (entity.Balance, Entity.isTXRedirected tx.To entity)
    | None -> failwithf "Invalid sender: %s" (Address.toStr sender)
  let prevBal = env.State.GetBalance(sender)
  let prevBugs = accumBugs
  sendTx env isAcc hadDepTx isRedirect tx
  let accNewBugs = Set.difference accumBugs prevBugs
                   |> checkEtherLeak env isAcc sender hadDepTx initBal prevBal
                   |> Set.map (fun (bug, pc) -> (bug, pc, i))
                   |> Set.union accNewBugs
  (accNewBugs, hadDepTx)

let private filterBugs checkOptional useOthersOracle bugs =
  let shouldSkip (bug, pc, ith) =
    if not checkOptional && BugClassHelper.isOptional bug then true
    elif not useOthersOracle && BugClassHelper.isFromOtherTools bug then true
    else false
  Set.filter (not << shouldSkip) bugs

/// Execute a seed (= transaction list) on EVM and return feedback.
let execute tc isAcc traceDU checkOptional useOthersOracle =
  let env = initTestingEnv ()
  List.iter (setupEntity env tc) tc.Entities
  setupTarget env tc.TargetDeployer tc.TargetContract tc.DeployTx
  env.VM.TraceDU <- traceDU
  let oldDeployEdgeCount = accumDeployEdges.Count
  let oldRuntimeEdgeCount = accumRuntimeEdges.Count
  let oldDUChainCount = accumDUChains.Count
  let bugs = List.foldi (processTx env tc isAcc) (Set.empty, false) tc.Txs
             |> fst |> filterBugs checkOptional useOthersOracle
  let deployCovGain = accumDeployEdges.Count > oldDeployEdgeCount
  let runtimeCovGain = accumRuntimeEdges.Count > oldRuntimeEdgeCount
  let covGain = deployCovGain || runtimeCovGain
  let duGain = accumDUChains.Count > oldDUChainCount
  let conv struct (pc, op, oprnd1, oprnd2) = (uint64 pc, op, oprnd1, oprnd2)
  let cmpList = List.map conv (List.ofSeq env.VM.CmpList)
  let contractBalance = env.State.GetBalance(Address.TARG_CONTRACT)
  receivedEther <- receivedEther || contractBalance > (UInt256 0L)
  useDelegateCall <- useDelegateCall || env.VM.BugOracle.UseDelegateCall
  canSendEther <- canSendEther || env.VM.BugOracle.SendEtherIndependently
  { CovGain = covGain
    DUGain = duGain
    CmpList = cmpList
    BugSet = bugs }

(*** Statistics ***)

let mutable totalExecutions = 0
let mutable phaseExecutions = 0

let getTotalExecutions () = totalExecutions
let getPhaseExecutions () = phaseExecutions
let resetPhaseExecutions () = phaseExecutions <- 0

(*** Resource scheduling ***)

let mutable allowedExecutions = 0
let allocateResource n = allowedExecutions <- n
let isExhausted () = allowedExecutions <= 0
let incrExecutionCount () =
  allowedExecutions <- allowedExecutions - 1
  totalExecutions <- totalExecutions + 1
  phaseExecutions <- phaseExecutions + 1

let private parseBranchInfo tryVal cmp =
  let addr, opStr, (oprnd1: bigint), (oprnd2: bigint)= cmp
  let dist = oprnd1 - oprnd2
  let brType =
    match opStr with
    | "==" -> Equality
    | "!=" -> Equality
    | ">=s" -> SignedSize
    | ">s" -> SignedSize
    | "<=s" -> SignedSize
    | "<s" -> SignedSize
    | ">=u" -> UnsignedSize
    | ">u" -> UnsignedSize
    | "<=u" -> UnsignedSize
    | "<u" -> UnsignedSize
    | _ -> failwithf "Unimplemented operation string : %s" opStr
  { InstAddr = addr
    BrType = brType
    TryVal = tryVal
    OpSize = 32
    Oprnd1 = oprnd1
    Oprnd2 = oprnd2
    Distance = dist }

let rec private parseBranchInfoAtAux tryVal targPoint accMap cmpList =
  match cmpList with
  | [] -> None
  | headCmp :: tailCmpList ->
    let addr, opStr, oprnd1, oprnd2 = headCmp
    // Caution : we count first visit as '1', instead of '0'.
    let count = if Map.containsKey addr accMap then Map.find addr accMap else 1
    if (addr, count) = (targPoint.Addr, targPoint.Idx) then
      Some (parseBranchInfo tryVal headCmp)
    else
      let newMap = Map.add addr (count + 1) accMap
      parseBranchInfoAtAux tryVal targPoint newMap tailCmpList

let private parseBranchInfoAt tryVal targPoint cmpList =
  parseBranchInfoAtAux tryVal targPoint Map.empty cmpList

let private parseBranchTrace tryVal cmpList =
  List.map (parseBranchInfo tryVal) cmpList

(*** Tracer execute functions ***)

let private runEVM opt seed isAcc =
  incrExecutionCount ()
  let tc = Seed.concretize seed
  execute tc isAcc opt.DynamicDFA opt.CheckOptionalBugs opt.UseOthersOracle

(*** Top-level executor functions ***)

let getCoverage opt seed =
  let f = runEVM opt seed true
  f.CovGain, f.DUGain, f.BugSet

let getBranchTrace opt seed tryVal =
  let f = runEVM opt seed false
  parseBranchTrace tryVal f.CmpList

let getBranchInfoAt opt seed tryVal targPoint =
  let f = runEVM opt seed false
  parseBranchInfoAt tryVal targPoint f.CmpList
