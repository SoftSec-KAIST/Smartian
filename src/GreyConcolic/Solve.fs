namespace Smartian

open System.Collections.Generic
open System
open Config
open Utils
open BytesUtils

module GreySolver =

  (* Functions related to solving linear equations *)
  let rec findNextCharAux seed opt targPt accStr accBrInfos tryVals =
    match tryVals with
    | [] -> None
    | tryVal :: tailVals ->
      let tryStr = Array.append accStr [| byte tryVal |]
      let trySeed = Seed.fixCurBytes seed Right tryStr
      let brInfoOpt = Executor.getBranchInfoAt opt trySeed tryVal targPt
      match brInfoOpt with
      | None -> // Failed to observe target point, proceed with the next tryVal
        findNextCharAux seed opt targPt accStr accBrInfos tailVals
      | Some brInfo ->
        let accBrInfos = accBrInfos @ [brInfo]
        let ctx = { Bytes = [| |]; ByteDir = Right }
        match BranchTree.inferLinEq ctx accBrInfos with
        | None -> // No linear equation found yet, proceed with more brInfo
          findNextCharAux seed opt targPt accStr accBrInfos tailVals
        | Some linEq -> (* The solution of this equation is next character *)
          match linEq.Solutions with
          | [] -> failwith "Linear equation w/ empty solution"
          | sol :: _ -> Some sol

  let findNextChar seed opt targPt accStr =
    let sampleVals = sampleInt 0I 255I N_SPAWN
    findNextCharAux seed opt targPt accStr [] sampleVals

  let rec tryStrSol seed opt maxLen targPt accRes tryStr =
    let trySeed = Seed.fixCurBytes seed Right tryStr
    // Use dummy value as 'tryVal', since our interest is in branch distance.
    match Executor.getBranchInfoAt opt trySeed 0I targPt with
    | Some brInfo when brInfo.Distance = 0I -> (trySeed :: accRes)
    | Some _ -> // Non-zero branch distance, try next character.
      if Array.length tryStr >= maxLen then accRes else
        let nextCharOpt = findNextChar seed opt targPt tryStr
        match nextCharOpt with
        | None -> accRes
        | Some nextChar ->
          let tryStr = Array.append tryStr [| byte nextChar |]
          tryStrSol seed opt maxLen targPt accRes tryStr
    | None -> accRes // Target point disappeared, halt.

  let solveAsString seed opt targPt linEq accRes =
    let initStrs = List.map (bigIntToBytes BE 1) linEq.Solutions
    let maxLen = Seed.queryUpdateBound seed Right
    List.fold (tryStrSol seed opt maxLen targPt) accRes initStrs

  let solutionCache = new HashSet<bigint>()

  let clearSolutionCache () =
    solutionCache.Clear()

  let tryChunkSol seed opt dir targPt endian size accRes sol =
    if solutionCache.Contains(sol) then accRes
    else
      let tryBytes = bigIntToBytes endian size sol
      let trySeed = Seed.fixCurBytes seed dir tryBytes
      // Use dummy value as 'tryVal', since our interest is branch distance.
      match Executor.getBranchInfoAt opt trySeed 0I targPt with
      | Some brInfo when brInfo.Distance = 0I ->
        ignore (solutionCache.Add(sol))
        trySeed :: accRes
      | Some _ -> accRes // Non-zero branch distance, failed.
      | None -> accRes // Target point disappeared, halt.

  let solveAsChunk seed opt dir targPt linEq accRes =
    let sols = linEq.Solutions
    let size = linEq.ChunkSize
    let endian = linEq.Endian
    List.fold (tryChunkSol seed opt dir targPt endian size) accRes sols

  let solveEquation seed opt dir accRes (targPt, linEq: LinearEquation) =
    if Config.Debug then
      log "Solving @ 0x%x(#%d)" targPt.Addr targPt.Idx
      log "Linear equation : %s" (LinearEquation.toString linEq)
    if linEq.ChunkSize = 1
    then solveAsString seed opt targPt linEq accRes
    else solveAsChunk seed opt dir targPt linEq accRes

  let solveEquations seed opt dir linEqs =
    let solveN = N_SOLVE / 3
    let linEqsChosen = randomSelect linEqs solveN
    List.fold (solveEquation seed opt dir) [] linEqsChosen
    |> List.rev // To preserve original order

  (* Functions related to binary search on monotonic function *)

  let getFunctionValue monotonic brInfo =
    let sign = if brInfo.BrType = UnsignedSize then Unsigned else Signed
    let size = brInfo.OpSize
    (* Since we checked brInfo.Distance <> 0, if Oprnd1 is equal to target
    * value it means that Oprnd2 corresponds to the output of the monotonic
    * function, and vice versa.
    *)
    if BranchInfo.interpretAs sign size brInfo.Oprnd1 = monotonic.TargetY
    then BranchInfo.interpretAs sign size brInfo.Oprnd2
    else BranchInfo.interpretAs sign size brInfo.Oprnd1

  let rec binarySearch seed opt dir maxLen targPt accRes mono =
    let tryVal = (mono.LowerX + mono.UpperX) / 2I
    let endian = if dir = Left then LE else BE
    let tryBytes = bigIntToBytes endian mono.ByteLen tryVal
    let trySeed = Seed.fixCurBytes seed dir tryBytes
    match Executor.getBranchInfoAt opt trySeed tryVal targPt with
    | None -> accRes // Target point disappeared, halt.
    | Some brInfo when brInfo.Distance = 0I -> trySeed :: accRes
    | Some brInfo ->
      let newY = getFunctionValue mono brInfo
      (* TODO : check monotonicity violation, too. *)
      let newMono = Monotonicity.update mono tryVal newY
      if newMono.ByteLen <= maxLen then
        binarySearch seed opt dir maxLen targPt accRes newMono
      else accRes

  let solveMonotonic seed opt accRes (targPt, mono) =
    if Config.Debug then
      log "Solving @ 0x%x(#%d)" targPt.Addr targPt.Idx
      log "Monotonicity : %s" (Monotonicity.toString mono)
    let maxLenR = Seed.queryUpdateBound seed Right
    let maxLenL = Seed.queryUpdateBound seed Left
    let res = binarySearch seed opt Right maxLenR targPt [] mono
    if List.isEmpty res
    then binarySearch seed opt Left maxLenL targPt accRes mono
    else res @ accRes

  let solveMonotonics seed opt monotonics =
    let solveN = N_SOLVE / 3
    let monosChosen = randomSelect monotonics solveN
    List.fold (solveMonotonic seed opt) [] monosChosen
    |> List.rev // To preserve original order

  (* Functions related to solving linear inequalities *)

  let differentSign i1 i2 = i1 < 0I && i2 > 0I || i1 > 0I && i2 < 0I

  let sameSign i1 i2 = i1 < 0I && i2 < 0I || i1 > 0I && i2 > 0I

  let splitWithSolution (sol, sign) = (sol - 1I, sol + 1I, sign)

  let rec generateRangesSigned prevHigh prevSign splitPoints accRes1 accRes2 =
    match splitPoints with
    | [] ->
      let ceiling = if prevHigh <= 127I then 127I else 255I
      if prevSign = Positive
      then accRes1, (prevHigh, ceiling) :: accRes2
      else (prevHigh, ceiling) :: accRes1, accRes2
    | (low, high, sign) :: tailSplitPoints ->
      let prevHigh = if low >= 128I && prevHigh <= 128I then 128I else prevHigh
      let accRes1, accRes2 =
        if sign = Positive
        then (prevHigh, low) :: accRes1, accRes2
        else accRes1, (prevHigh, low) :: accRes2
      generateRangesSigned high sign tailSplitPoints accRes1 accRes2

  let rec generateRangesUnsigned prevHigh prevSign splitPoints accRes1 accRes2 =
    match splitPoints with
    | [] ->
      if prevSign = Positive
      then accRes1, (prevHigh, 255I) :: accRes2
      else (prevHigh, 255I) :: accRes1, accRes2
    | (low, high, sign) :: tailSplitPoints ->
      let accRes1, accRes2 =
        if sign = Positive
        then (prevHigh, low) :: accRes1, accRes2
        else accRes1, (prevHigh, low) :: accRes2
      generateRangesUnsigned high sign tailSplitPoints accRes1 accRes2

  let extractMSB size (i1:bigint, i2:bigint, sign) =
    (i1 >>> ((size - 1) * 8), i2 >>> ((size - 1) * 8), sign)

  // Currently we consider constraints just for MSB.
  let rec generateMSBRanges splitPoints size signedness =
    if List.isEmpty splitPoints then [], [] else
      let splitPoints = List.sortBy (fun (x, _, _) -> x) splitPoints
      let splitPoints = List.map (extractMSB size) splitPoints
      // 'Positive' is used as a dummy argument.
      if signedness = Signed
      then generateRangesSigned 0I Positive splitPoints [] []
      else generateRangesUnsigned 0I Positive splitPoints [] []

  let checkSolutionAux seed opt dir endian size targPt accRes sol =
    let tryBytes = bigIntToBytes endian size sol
    let trySeed = Seed.fixCurBytes seed dir tryBytes
    // Use dummy value as 'tryVal', since our interest is in branch distance.
    match Executor.getBranchInfoAt opt trySeed 0I targPt with
    | Some brInfo when brInfo.Distance = 0I ->
      let tryBytes' = bigIntToBytes endian size (sol - 1I)
      let trySeed' = Seed.fixCurBytes seed dir tryBytes'
      match Executor.getBranchInfoAt opt trySeed' 0I targPt with
      | Some brInfo' ->
        let sign = if brInfo'.Distance > 0I then Positive else Negative
        (sol, sign) :: accRes
      | _ -> accRes
    | _ -> accRes

  let checkSolution seed opt dir equation targPt =
    let solutions = equation.Solutions
    let endian = equation.Endian
    let size = equation.ChunkSize
    let sign = equation.Linearity.Slope.Numerator
    List.fold (checkSolutionAux seed opt dir endian size targPt) [] solutions

  let checkSplitAux seed opt dir endian size targPt accRes (sol1, sol2) =
    let tryBytes1 = bigIntToBytes endian size sol1
    let trySeed1 = Seed.fixCurBytes seed dir tryBytes1
    let tryBytes2 = bigIntToBytes endian size sol2
    let trySeed2 = Seed.fixCurBytes seed dir tryBytes2
    // Use dummy value as 'tryVal', since our interest is in branch distance.
    let brInfoOpt1 = Executor.getBranchInfoAt opt trySeed1 0I targPt
    let brInfoOpt2 = Executor.getBranchInfoAt opt trySeed2 0I targPt
    match brInfoOpt1, brInfoOpt2 with
    | Some brInfo1, Some brInfo2 ->
      if sameSign brInfo1.Distance brInfo2.Distance
      then accRes
      else let sign = if brInfo1.Distance > 0I then Positive else Negative
           (sol1, sol2, sign) :: accRes
    | _ -> accRes

  let checkSplitPoint seed opt dir ineq targPt =
    let splitPoints = ineq.SplitPoints
    let endian = ineq.Endian
    let size = ineq.ChunkSize
    List.fold (checkSplitAux seed opt dir endian size targPt) [] splitPoints

  let extractSplitPoint seed opt dir inequality targPt =
    match inequality.TightInequality, inequality.LooseInequality with
    | Some eq, Some ineq ->
      let tightSols = checkSolution seed opt dir eq targPt
      if not (List.isEmpty tightSols) then
        let splits = List.map splitWithSolution tightSols
        (eq.ChunkSize, eq.Endian, splits)
      else
        let splits = checkSplitPoint seed opt dir ineq targPt
        (ineq.ChunkSize, ineq.Endian, splits)
    | Some eq, None ->
      let tightSols = checkSolution seed opt dir eq targPt
      let splits = List.map splitWithSolution tightSols
      (eq.ChunkSize, eq.Endian, splits)
    | None, Some ineq ->
      let splits = checkSplitPoint seed opt dir ineq targPt
      (ineq.ChunkSize, ineq.Endian, splits)
    | None, None -> failwith "Unreachable"

  let extractCond seed opt dir ineq targPt =
    let size, endian, splitPoints = extractSplitPoint seed opt dir ineq targPt
    let sign = ineq.Sign
    let posMSBRanges, negMSBRanges = generateMSBRanges splitPoints size sign
    if Config.Debug then
      log "generateMSBRanges() returned: %A & %A" posMSBRanges negMSBRanges
    let posCond = Constraint.make posMSBRanges endian size
    let negCond = Constraint.make negMSBRanges endian size
    (posCond, negCond)

  let updateConditions pc distSign (condP:Constraint) (condN:Constraint) =
    if Config.Debug then
      log "updateConditions(pc : %A, condP : %A, condN : %A)" pc condP condN
    if distSign = Positive
    then (Constraint.conjunction pc condP, Constraint.conjunction pc condN)
    else (Constraint.conjunction pc condN, Constraint.conjunction pc condP)

  let encodeCondition seed opt dir condition =
    if Constraint.isTop condition then []
    elif Seed.queryLenToward seed dir < List.length condition then []
    else
      let byteConds =
        if dir = Right
        then List.mapi (fun i byteCond -> (i, byteCond)) condition
        else let len = List.length condition
             List.mapi (fun i byteCond -> (len - i - 1, byteCond)) condition
      let newSeeds =
        List.fold (fun accSeeds (offset, byteCond) ->
          if ByteConstraint.isTop byteCond then accSeeds else
            List.collect (fun range ->
              match range with
              | Between (low, high) ->
                let low = if low < 0I then 0uy
                          elif low > 255I then 255uy
                          else byte low
                let high = if high < 0I then 0uy
                           elif high > 255I then 255uy
                           else byte high
                let mapper s = Seed.constrainByteAt s dir offset low high
                List.map mapper accSeeds
              | Bottom -> []
              | Top -> failwith "Unreachable"
            ) byteCond
          ) [seed] byteConds
      newSeeds

  let solveInequality seed opt dir pc distSign branchPoint ineq =
    if Config.Debug then
      log "Solving @ 0x%x(#%d)" branchPoint.Addr branchPoint.Idx
      log "Linear inequality : %s" (LinearInequality.toString ineq)
    let condP, condN = extractCond seed opt dir ineq branchPoint
    if Config.Debug then
      log "extractCond() (1) = condP : %A, condN : %A" condP condN
    let accPc, flipCond = updateConditions pc distSign condP condN
    let seeds = encodeCondition seed opt dir flipCond
    (accPc, seeds)

  let solveBranchCond seed opt dir (pc: Constraint) branch =
    let branchCond, distSign = branch
    let cond, branchPoint = branchCond
    match cond with
    | LinEq linEq ->
      let seeds = solveEquation seed opt dir [] (branchPoint, linEq)
      (pc, seeds)
    | Mono mono ->
      let seeds = solveMonotonic seed opt [] (branchPoint, mono)
      (pc, seeds)
    | LinIneq ineq ->
      let pc, seeds = solveInequality seed opt dir pc distSign branchPoint ineq
      (pc, seeds)

  let solveBranchSeq seed opt dir pc branchSeq =
    List.fold (fun (accPc, accSeeds) branch ->
      let accPc, newSeeds = solveBranchCond seed opt dir accPc branch
      accPc, newSeeds @ accSeeds
    ) (pc, []) branchSeq.Branches

  let rec solveBranchTree seed opt dir pc branchTree =
    match branchTree with
    | Straight branchSeq ->
      let pc, newSeeds = solveBranchSeq seed opt dir pc branchSeq
      let terminalSeeds = encodeCondition seed opt dir pc
      newSeeds @ terminalSeeds
    | ForkedTree (branchSeq, (LinIneq ineq, branchPt), childs) ->
      let pc, newSeeds = solveBranchSeq seed opt dir pc branchSeq
      let condP, condN = extractCond seed opt dir ineq branchPt
      if Config.Debug then
        log "extractCond() (2) = condP : %A, condN : %A" condP condN
      let childSeeds =
        List.map (fun (distSign, childTree) ->
          let pc, _ = updateConditions pc distSign condP condN
          if Config.Debug then
            log "PC updated to %A while forking" pc
          solveBranchTree seed opt dir pc childTree
        ) childs
      List.concat (newSeeds :: childSeeds)
    | ForkedTree (branchSeq, _, childs) ->
      let pc, newSeeds = solveBranchSeq seed opt dir pc branchSeq
      let childSeeds = List.map (snd >> solveBranchTree seed opt dir pc) childs
      List.concat (newSeeds :: childSeeds)
    | DivergeTree (branchSeq, subTrees) ->
      let pc, newSeeds = solveBranchSeq seed opt dir pc branchSeq
      let subTreeSeeds = List.map (solveBranchTree seed opt dir pc) subTrees
      List.concat (newSeeds :: subTreeSeeds)

  let solve seed opt byteDir branchTree =
    let initPC = Constraint.top
    solveBranchTree seed opt byteDir initPC branchTree
