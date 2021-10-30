module Smartian.Fuzz

open EVMAnalysis
open Utils
open Config
open Options

let private makeSingletonSeeds contSpec =
  let constrSpec = contSpec.Constructor
  let funcSpecs = contSpec.NormalFunctions |> Array.toList
  List.map (fun funcSpec -> Seed.init constrSpec [| funcSpec |]) funcSpecs

let private sequenceToSeed contSpec seq =
  let constrSpec = contSpec.Constructor
  let funcSpecs = contSpec.NormalFunctions
  let findSpec s = Array.find (fun spec -> FuncSpec.getName spec = s) funcSpecs
  let funcSpecs = List.map findSpec seq |> List.toArray
  Seed.init constrSpec funcSpecs

let private initializeWithDFA opt =
  let contSpec, seqs = TopLevel.parseAndAnalyze opt.ProgPath opt.ABIPath
  if List.isEmpty seqs // No DU chain at all.
  then (contSpec, makeSingletonSeeds contSpec)
  else (contSpec, List.map (sequenceToSeed contSpec) seqs)

let private initializeWithoutDFA opt =
  let contSpec = TopLevel.parseOnly opt.ProgPath opt.ABIPath
  (contSpec, makeSingletonSeeds contSpec)

/// Allocate testing resource for each strategy (grey-box concolic testing and
/// random fuzz testing). Resource is managed through 'the number of allowed
/// program execution'. If the number of instrumented program execution exceeds
/// the specified number, the strategy will be switched.
let private allocResource () =
  let concolicEff = GreyConcolic.evaluateEfficiency ()
  let randFuzzEff = RandomFuzz.evaluateEfficiency ()
  let concolicRatio = concolicEff / (concolicEff + randFuzzEff)
  // Bound alloc ratio with 'MinResourceAlloc', to avoid extreme biasing
  let concolicRatio = max MIN_RESOURCE_RATIO (min MAX_RESOURCE_RATIO concolicRatio)
  let randFuzzRatio = 1.0 - concolicRatio
  let totalBudget = EXEC_BUDGET_PER_ROUND
  let greyConcBudget = int (float totalBudget * concolicRatio)
  let randFuzzBudget = int (float totalBudget * randFuzzRatio)
  if greyConcBudget < 0 || randFuzzBudget < 0 then (200, 200)
  else (greyConcBudget, randFuzzBudget)

let private printNewSeeds newSeeds =
  let count = List.length newSeeds
  let concolicStr = String.concat "========\n" (List.map Seed.toString newSeeds)
  log "Generated %d seeds for grey-box concolic : [ %s ]" count concolicStr

let private rewindCursors seed =
  Array.collect Seed.rewindByteCursors (Array.ofList seed)
  |> Array.toList

let rec private greyConcolicLoop opt concQ randQ =
  if Executor.isExhausted () || ConcolicQueue.isEmpty concQ then (concQ, randQ)
  else let seed, concQ = ConcolicQueue.dequeue concQ
       if opt.Verbosity >= 3 then
         log "Grey-box concolic on seed : %s" (Seed.toString seed)
       let newSeeds = GreyConcolic.run seed opt
       // Move cursors of newly generated seeds.
       let rewindedSeeds = rewindCursors newSeeds
       // Also generate seeds by just stepping the cursor of original seed.
       let steppedSeeds = Seed.stepByteCursor seed
       let concQ = List.fold ConcolicQueue.enqueue concQ rewindedSeeds
       let concQ = List.fold ConcolicQueue.enqueue concQ steppedSeeds
       // Note that 'Stepped' seeds are not enqueued for random fuzzing.
       let randQ = List.fold RandFuzzQueue.enqueue randQ newSeeds
       if opt.Verbosity >= 4 then printNewSeeds (rewindedSeeds @ steppedSeeds)
       greyConcolicLoop opt concQ randQ

let private repeatGreyConcolic opt concQ randQ concolicBudget =
  if opt.Verbosity >= 2 then log "Grey-box concoclic testing phase starts"
  Executor.allocateResource concolicBudget
  Executor.resetPhaseExecutions ()
  let tcNumBefore = TCManage.getTestCaseCount ()
  let concQ, randQ = greyConcolicLoop opt concQ randQ
  let tcNumAfter = TCManage.getTestCaseCount ()
  let concolicExecNum = Executor.getPhaseExecutions ()
  let concolicNewTCNum = tcNumAfter - tcNumBefore
  GreyConcolic.updateStatus concolicExecNum concolicNewTCNum
  (concQ, randQ)

let rec private randFuzzLoop opt contSpec concQ randQ =
  // Random fuzzing seeds are involatile, so don't have to check emptiness.
  if Executor.isExhausted () then (concQ, randQ)
  else let seed, randQ = RandFuzzQueue.fetch randQ
       if opt.Verbosity >= 3 then
         log "Random fuzzing on seed : %s" (Seed.toString seed)
       let newSeeds = RandomFuzz.run seed opt contSpec
       let rewindedSeeds = rewindCursors newSeeds
       let concQ = List.fold ConcolicQueue.enqueue concQ rewindedSeeds
       let randQ = List.fold RandFuzzQueue.enqueue randQ newSeeds
       if opt.Verbosity >= 4 then printNewSeeds rewindedSeeds
       randFuzzLoop opt contSpec concQ randQ

let private repeatRandFuzz opt contSpec concQ randQ randFuzzBudget =
  if opt.Verbosity >= 2 then log "Random fuzzing phase starts"
  Executor.allocateResource randFuzzBudget
  Executor.resetPhaseExecutions ()
  let tcNumBefore = TCManage.getTestCaseCount ()
  let concQ, randQ = randFuzzLoop opt contSpec concQ randQ
  let tcNumAfter = TCManage.getTestCaseCount ()
  let randExecNum = Executor.getPhaseExecutions ()
  let randNewTCNum = tcNumAfter - tcNumBefore
  RandomFuzz.updateStatus randExecNum randNewTCNum
  (concQ, randQ)

let rec private fuzzLoop opt contSpec concQ randQ =
  let concolicBudget, randFuzzBudget = allocResource ()
  let concQSize = ConcolicQueue.size concQ
  let randQSize = RandFuzzQueue.size randQ
  if opt.Verbosity >= 2 then
    log "Concolic budget: %d, Rand budget: %d" concolicBudget randFuzzBudget
    log "Concolic queue size: %d, Random Queue size: %d" concQSize randQSize
  // Perform grey-box concolic testing
  let concQ, randQ = repeatGreyConcolic opt concQ randQ concolicBudget
  // Perform random fuzzing
  let concQ, randQ = repeatRandFuzz opt contSpec concQ randQ randFuzzBudget
  fuzzLoop opt contSpec concQ randQ

let private fuzzingTimer opt = async {
  let timespan = System.TimeSpan (0, 0, 0, opt.Timelimit)
  System.Threading.Thread.Sleep (timespan)
  printLine "Fuzzing timeout expired."
  if opt.CheckOptionalBugs then TCManage.checkFreezingEtherBug ()
  log "===== Statistics ====="
  TCManage.printStatistics ()
  log "Done, clean up and exit..."
  exit (0)
}

let run args =
  let opt = parseFuzzOption args
  assertFileExists opt.ProgPath
  log "Fuzz target : %s" opt.ProgPath
  log "Fuzzing starts at %s" (startTime.ToString("hh:mm:ss"))
  log "Time limit : %d s" opt.Timelimit
  Async.Start (fuzzingTimer opt)
  createDirectoryIfNotExists opt.OutDir
  TCManage.initialize opt.OutDir
  Executor.initialize opt.ProgPath
  let contSpec, initSeeds = if opt.StaticDFA then initializeWithDFA opt
                            else initializeWithoutDFA opt
  let concQ = List.fold ConcolicQueue.enqueue ConcolicQueue.empty initSeeds
  let randQ = List.fold RandFuzzQueue.enqueue (RandFuzzQueue.init ()) initSeeds
  log "Start main fuzzing phase"
  fuzzLoop opt contSpec concQ randQ
