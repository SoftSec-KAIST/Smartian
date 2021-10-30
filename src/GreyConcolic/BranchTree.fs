namespace Smartian

open System.Collections.Generic
open System.Collections.Immutable
open Config
open Utils

type SelectSet = ImmutableHashSet<int>

type Condition =
  | LinEq of LinearEquation
  | LinIneq of LinearInequality
  | Mono of Monotonicity

type BranchCondition = Condition * BranchPoint

type DistanceSign = Sign

type BranchSeq = {
  Length : int
  Branches : (BranchCondition * DistanceSign) list
}

module BranchSeq =
  let empty = { Length = 0; Branches = [] }

  let append branchSeq branchCondOpt distSign =
    match branchCondOpt with
    | None -> branchSeq
    | Some branchCond ->
      let branch = (branchCond, distSign)
      let newLen = branchSeq.Length + 1
      let newBranches = branch :: branchSeq.Branches
      { branchSeq with Length = newLen; Branches = newBranches }

type BranchTree =
  | Straight of BranchSeq
  | ForkedTree of BranchSeq * BranchCondition * (DistanceSign * BranchTree) list
  | DivergeTree of BranchSeq * BranchTree list

module BranchTree =

  let rec genCombAux accCombs windowElems leftElems n =
    match leftElems with
    | [] -> accCombs
    | headElem :: tailElems ->
      let newCombs =
        combination (n - 1) windowElems // Select 'n-1' from window elements
        |> List.map (fun elems -> elems @ [headElem]) // Use 'headElem' as 'n'th
      let newWindowElems = List.tail windowElems @ [headElem]
      genCombAux (accCombs @ newCombs) newWindowElems tailElems n

  let genComb elems windowSize n =
    if List.length elems < windowSize then combination n elems else
      let headElems, tailElems = splitList windowSize elems
      let initialCombs = combination n headElems
      (* Pass element list of length 'windowSize - 1' as 'windowElems'. *)
      let initialWindow = List.tail headElems
      genCombAux initialCombs initialWindow tailElems n

  /// Check if provided BranchInfos are valid target to infer linearity or
  /// monotonicity. Note that we can skip inference if all the branch distances
  /// are the same.
  let checkValidTarget brInfos =
    if List.length brInfos < 3 then false else
      let brInfo = List.head brInfos
      let tailBrInfos = List.tail brInfos
      List.exists (fun f-> f.Distance <> brInfo.Distance) tailBrInfos

  let rec inferLinEqAux ctx brInfoCombinations =
    match brInfoCombinations with
    | brInfoTriple :: tailCombs ->
      let linEqOpt = LinearEquation.find ctx brInfoTriple
      if Option.isSome linEqOpt then linEqOpt else inferLinEqAux ctx tailCombs
    | [] -> None

  let inferLinEq ctx brInfos =
    if checkValidTarget brInfos then
      if Config.Debug then
        let brStr = String.concat ", " (List.map BranchInfo.toString brInfos)
        log "Infer equation with : %s" brStr
      genComb brInfos BRANCH_COMB_WINDOW 3 // XXX
      (* Now convert each [a,b,c] into (a,b,c) *)
      |> List.map (function [ a; b; c ] -> (a,b,c) | _ -> failwith "invalid")
      |> inferLinEqAux ctx
    else None

  let rec inferLinIneqAux ctx brInfoCombinations =
    match brInfoCombinations with
    | brInfoTriple :: tailCombs ->
      let linIneq = LinearInequality.find ctx brInfoTriple
      if Option.isSome linIneq then linIneq else inferLinIneqAux ctx tailCombs
    | [] -> None

  let inferLinIneq ctx brInfos =
    if checkValidTarget brInfos then
      if Config.Debug then
        let brStr = String.concat ", " (List.map BranchInfo.toString brInfos)
        log "Infer inequality with : %s" brStr
      genComb brInfos BRANCH_COMB_WINDOW 3 // XXX
      (* Now convert each [a,b,c] into (a,b,c) *)
      |> List.map (function a::b::c::[] -> (a,b,c) | _ -> failwith "invalid")
      |> inferLinIneqAux ctx
    else None

  let inferMonotonicity brInfos =
    if checkValidTarget brInfos then
      if Config.Debug then
        let brStr = String.concat ", " (List.map BranchInfo.toString brInfos)
        log "Infer equation with : %s" brStr
      Monotonicity.find brInfos
    else None

  let inspectBranchInfos opt ctx visitCntMap branchInfos =
    // We already filtered out cases where length of BranchInfo is less than 3
    let firstBrInfo = List.head branchInfos
    let targAddr = firstBrInfo.InstAddr
    let targIdx = Map.find targAddr visitCntMap
    let targPt = { Addr = targAddr; Idx = targIdx }
    let brType = firstBrInfo.BrType
    if brType = Equality then
      match inferLinEq ctx branchInfos with
      | Some linEq -> Some (LinEq linEq, targPt)
      | None ->
        let monoOpt = inferMonotonicity branchInfos
        Option.map (fun mono -> (Mono mono, targPt)) monoOpt
    else
      match inferLinIneq ctx branchInfos with
      | Some linIneq -> Some (LinIneq linIneq, targPt)
      | None -> None

  let decideSign x =
    if x > 0I then Positive
    elif x = 0I then Zero
    else Negative

  let haveSameAddr brInfos =
    match brInfos with
    | [] -> true
    | brInfo :: brInfos ->
      let instAddr = brInfo.InstAddr
      List.forall (fun br -> br.InstAddr = instAddr) brInfos

  let haveSameBranchDistanceSign brInfos =
    match brInfos with
    | [] -> true
    | brInfo :: brInfos ->
      let distSign = decideSign brInfo.Distance
      List.forall (fun br -> decideSign br.Distance = distSign) brInfos

  // Precondition : The first branchInfo of each branch trace should have the
  // same instuction address. Empty branch trace is not allowed.
  let rec extractStraightSeq opt ctx visitCntMap brTraceList accBranchSeq =
    // Split each BranchTrace into a tuple of its head and tail.
    let headBrInfos = List.map List.head brTraceList
    let tailBrTraces = List.map List.tail brTraceList
    if List.length brTraceList < 3 then failwith "Unreachable"
    if not (haveSameAddr headBrInfos) then failwith "Unreachable"
    // Leave branch traces which are not empty.
    let tailBrTraces = List.filter (not << List.isEmpty) tailBrTraces
    // Now examine the next address and decide whether to continue extracting.
    if List.length tailBrTraces >= 2 &&
       not (haveSameAddr (List.map List.head tailBrTraces))
    then
      // Pass 'brTraceList', instead of 'tailBrTraces' since we need information
      // about the branch distance of previous branch before forking.
      (visitCntMap, brTraceList, accBranchSeq)
    else
      let brInfo = List.head headBrInfos
      let addr = brInfo.InstAddr
      let cnt = try Map.find addr visitCntMap with :? KeyNotFoundException -> 0
      let visitCntMap = Map.add addr (cnt + 1) visitCntMap
      let brCondOpt = inspectBranchInfos opt ctx visitCntMap headBrInfos
      let distSign = decideSign brInfo.Distance
      let accBranchSeq = BranchSeq.append accBranchSeq brCondOpt distSign
      // Stop proceeding if no more than three branch traces are left.
      if List.length tailBrTraces < 3 then
        (visitCntMap, [], accBranchSeq)
      else extractStraightSeq opt ctx visitCntMap tailBrTraces accBranchSeq

  // Precondition : The first branchInfo of each branch trace should have the
  // same instuction address. Empty branch trace is not allowed.
  let rec makeAux opt ctx visitCntMap brTraceList =
    let visitCntMap, brTraceList, branchSeq =
      extractStraightSeq opt ctx visitCntMap brTraceList BranchSeq.empty
    // If there are no more branch trace to parse, construct 'Straight' tree.
    if List.isEmpty brTraceList then
      Straight branchSeq
    else
      // At this point, the first branches info of branch traces have the same
      // instruction address, and diverge/forks at the next branch.
      let headBrInfos = List.map List.head brTraceList
      if not (haveSameAddr headBrInfos) then failwith "Unreachable"
      // First, fetch the head branch info and infer the branch condition.
      let brInfo = List.head headBrInfos
      let addr = brInfo.InstAddr
      let cnt = try Map.find addr visitCntMap with :? KeyNotFoundException -> 0
      let visitCntMap = Map.add addr (cnt + 1) visitCntMap
      let branchCondOpt = inspectBranchInfos opt ctx visitCntMap headBrInfos
      match branchCondOpt with
      | None -> // If failed to infer branch condition, handle as a 'diverge'
        buildDivergeTree opt ctx visitCntMap branchSeq brTraceList
      | Some branchCond ->
        if haveSameBranchDistanceSign (List.map List.head brTraceList) then
          // Fork actually did not occur at this branch condition. Therefore,
          // append this branch to BranchSeq, and handle as a DivergeTree case.
          let brTrace = List.head brTraceList
          let distSign = decideSign (List.head brTrace).Distance
          let branchSeq = BranchSeq.append branchSeq branchCondOpt distSign
          buildDivergeTree opt ctx visitCntMap branchSeq brTraceList
        else buildForkTree opt ctx visitCntMap branchSeq branchCond brTraceList

  and buildDivergeTree opt ctx visitCntMap branchSeq brTraceList =
    // Now leave branch traces longer than 1, and group by its next InstAddr.
    let brTraceList = List.filter BranchTrace.isLongerThanOne brTraceList
    let groupedTraces = List.groupBy BranchTrace.getNextAddr brTraceList
                        |> List.unzip |> snd
                        |> List.filter (fun group -> List.length group >= 3)
    let subTrees = List.map (makeAux opt ctx visitCntMap) groupedTraces
    if List.isEmpty subTrees then
      Straight branchSeq
    else DivergeTree (branchSeq, subTrees)

  and buildForkTree opt ctx visitCntMap branchSeq branchCond brTraceList =
    // Now leave branch traces longer than 1, and group by its next InstAddr.
    let brTraceList = List.filter BranchTrace.isLongerThanOne brTraceList
    // Defer filtering group with no more than three traces.
    let groupedTraces = List.groupBy BranchTrace.getNextAddr brTraceList
                        |> List.unzip |> snd
    let childTrees =
      List.map (fun brTraceGroup ->
        let branchTrace = List.head brTraceGroup
        let distSign = decideSign (List.head branchTrace).Distance
        let tailBrTraceList = List.map List.tail brTraceGroup
        let subTree = if List.length tailBrTraceList >= 3 then
                        makeAux opt ctx visitCntMap tailBrTraceList
                      else Straight BranchSeq.empty
        (distSign, subTree)
      ) groupedTraces
    ForkedTree (branchSeq, branchCond, childTrees)

  let rec make opt ctx brTraceList =
    let brTraceList = List.filter (not << List.isEmpty) brTraceList
    let groupedTraces = List.groupBy BranchTrace.getHeadAddr brTraceList
                        |> List.unzip |> snd
                        |> List.filter (fun group -> List.length group >= 3)
    let subTrees = List.map (makeAux opt ctx Map.empty) groupedTraces
    match subTrees with
    | [ subTree ] -> subTree
    | _ -> DivergeTree (BranchSeq.empty, subTrees)

  let rec sizeOf branchTree =
    match branchTree with
    | Straight branchSeq -> branchSeq.Length
    | DivergeTree (branchSeq, subTrees) ->
      branchSeq.Length + List.sum (List.map sizeOf subTrees)
    | ForkedTree (branchSeq, _, childTrees) ->
      // Let us not count the branch itself at the fork point.
      let subTrees = snd (List.unzip childTrees)
      branchSeq.Length + List.sum (List.map sizeOf subTrees)

  let rec reverse branchTrace =
    match branchTrace with
    | Straight branchSeq ->
      let branchSeq = { branchSeq with Branches = List.rev branchSeq.Branches}
      Straight branchSeq
    | DivergeTree (branchSeq, subTrees) ->
      let branchSeq = { branchSeq with Branches = List.rev branchSeq.Branches}
      let subTrees = List.map reverse subTrees
      DivergeTree (branchSeq, subTrees)
    | ForkedTree (branchSeq, brCond, childTrees) ->
      let branchSeq = { branchSeq with Branches = List.rev branchSeq.Branches}
      let childTrees = List.map (fun (s, tree) -> (s, reverse tree)) childTrees
      ForkedTree (branchSeq, brCond, childTrees)

  let rec filterBranchSeqAux (selectSet: SelectSet) counter branches accList =
    let accBrs, accLen = accList
    match branches with
    | [] -> accList
    | headBranch :: tailBranchList ->
      let accList = if selectSet.Contains(counter) then
                      (headBranch :: accBrs, accLen + 1)
                    else accList
      filterBranchSeqAux selectSet (counter + 1) tailBranchList accList

  let filterBranchSeq selectSet counter branchSeq =
    let branches = branchSeq.Branches
    let newBrs, newLen = filterBranchSeqAux selectSet counter branches  ([], 0)
    let counter = counter + branchSeq.Length
    let branchSeq = { branchSeq with Branches = newBrs; Length = newLen}
    (counter, branchSeq)

  let rec filterAndReverseAux selectSet counter branchTrace =
    match branchTrace with
    | Straight branchSeq ->
      let counter, branchSeq = filterBranchSeq selectSet counter branchSeq
      (counter, Straight branchSeq)
    | DivergeTree (branchSeq, subTrees) ->
      let counter, branchSeq = filterBranchSeq selectSet counter branchSeq
      let counter, subTrees =
        List.fold (fun (counter, accSubTrees) subTree ->
          let counter, subTree = filterAndReverseAux selectSet counter subTree
          (counter, subTree :: accSubTrees)
        ) (counter, []) subTrees
      (counter, DivergeTree (branchSeq, List.rev subTrees))
    | ForkedTree (branchSeq, brCond, childTrees) ->
      let counter, branchSeq = filterBranchSeq selectSet counter branchSeq
      let counter, childTrees =
        List.fold (fun (counter, accChildTrees) (sign, subTree) ->
          let counter, subTree = filterAndReverseAux selectSet counter subTree
          (counter, (sign, subTree) :: accChildTrees)
        ) (counter, []) childTrees
      (counter, ForkedTree (branchSeq, brCond, List.rev childTrees))

  let filterAndReverse selectSet branchTree =
    let _, filteredBranchTree = filterAndReverseAux selectSet 0 branchTree
    filteredBranchTree

  let rec selectAndRepair opt branchTree =
    let size = sizeOf branchTree
    if N_SOLVE > size then reverse branchTree
    else
      let selectSet = randomSubset size N_SOLVE
      filterAndReverse selectSet branchTree
