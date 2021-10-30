namespace EVMAnalysis

open B2R2.MiddleEnd
open B2R2.MiddleEnd.BinEssence

type CFG = {
  Root: EVMAnalysis.BasicBlock
  Graph: BinGraph.DiGraph<IRBasicBlock,CFGEdgeKind>
  BackEdges : Set<Addr * Addr>
}

module CFG =

  let hasCall (bb: EVMAnalysis.BasicBlock) cfg =
    BinGraph.DiGraph.getSuccs cfg.Graph bb
    |> List.exists (fun bb -> bb.VData.IsFakeBlock())

  let getSuccs (bb: EVMAnalysis.BasicBlock) cfg =
    BinGraph.DiGraph.getSuccs cfg.Graph bb
    |> List.filter (fun bb -> not (bb.VData.IsFakeBlock()))

  let getPreds (bb: EVMAnalysis.BasicBlock) cfg =
    BinGraph.DiGraph.getPreds cfg.Graph bb
    |> List.filter (fun bb -> not (bb.VData.IsFakeBlock()))

  let isBackEdge edge (cfg: CFG) =
    Set.contains edge cfg.BackEdges

  let rec private findBackEdgesAux cfg stack (accBackEdges, accVisited) bb =
    let addr = BasicBlock.getAddr bb
    // Update stack and visited node set.
    let stack = addr :: stack
    let accVisited = Set.add addr accVisited
    // Now calculate backedges that start from current basic block, 'bb'.
    let succs = getSuccs bb cfg
    let succAddrs = List.map BasicBlock.getAddr succs
    let isOnStack addr = List.contains addr stack
    let newBackEdges = List.filter isOnStack succAddrs
                       |> List.map (fun succAddr -> (addr, succAddr))
    let accBackEdges = newBackEdges @ accBackEdges
    // Finally, call recursively on unvisited successor nodes.
    let isVisited b = Set.contains (BasicBlock.getAddr b) accVisited
    let toVisits = List.filter (not << isVisited) succs
    List.fold (findBackEdgesAux cfg stack) (accBackEdges, accVisited) toVisits

  let private findBackedges cfg entryBB =
    findBackEdgesAux cfg [] ([], Set.empty) entryBB |> fst |> Set.ofList

  let tryBuild (ess: BinEssence.BinEssence) entry =
    match ess.GetFunctionCFG(entry) with
    | Ok (graph, root) ->
      let cfg = { Root = root; Graph = graph; BackEdges = Set.empty }
      Some { cfg with BackEdges = findBackedges cfg root }
    | Error _ ->
      printfn "[WARNING] Failed to retrieve CFG"
      None

  let rec private topoSortAux cfg (accVisited, accList) bb =
    // Accumulate and perform DFS recursively.
    let addr = BasicBlock.getAddr bb
    if Set.contains addr accVisited then (accVisited, accList)
    else
      let folder = topoSortAux cfg
      let accVisited = Set.add addr accVisited
      // Exclude BBs that form back edges (we may start from a non-root BB).
      let formsBackEdge succ = isBackEdge (addr, BasicBlock.getAddr succ) cfg
      let toVisits = getSuccs bb cfg |> List.filter (not << formsBackEdge)
      let accVisited, accList = List.fold folder (accVisited, accList) toVisits
      (accVisited, bb :: accList)

  let topologicalSort cfg startBB: EVMAnalysis.BasicBlock list =
    topoSortAux cfg (Set.empty, []) startBB |> snd

  let private countAncestor cfg bb =
    let rec getAncestors visited bbs =
      match bbs with
      | [] -> []
      | bb :: tailBBs when Set.contains (BasicBlock.getAddr bb) visited ->
        getAncestors visited tailBBs
      | bb :: tailBBs ->
        let visited = Set.add (BasicBlock.getAddr bb) visited
        let preds = getPreds bb cfg
        preds @ (getAncestors visited (preds @ tailBBs))
    getAncestors Set.empty [bb] |> List.length

  let findFallback (cfg: CFG) =
    let root = cfg.Root
    let succs = getSuccs root cfg
    List.maxBy (countAncestor cfg) succs
