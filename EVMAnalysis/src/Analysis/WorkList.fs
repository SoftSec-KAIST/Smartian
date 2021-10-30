namespace EVMAnalysis

type WorkList = (Context * CFG * BasicBlock) list

module WorkList =

  let init cfg entryBB =
    let emptyCtx = []
    let sortedBBs = CFG.topologicalSort cfg entryBB
    List.map (fun bb -> (emptyCtx, cfg, bb)) sortedBBs

  let push newWorks workList =
    newWorks @ workList

  // Put the given work right after the works for the current function.
  let rec deferUntilReturn (ctx, cfg, bb) workList =
    match workList with
    | [] -> printfn "[Warning] Empty list in deferUntilReturn()"; []
    | (headCtx, headCFG, headBB) :: tailWorks when headCtx = ctx -> // Continue.
      (headCtx, headCFG, headBB) :: deferUntilReturn (ctx, cfg, bb) tailWorks
    | headWork :: tailWorks -> // Call context changes, put the work here.
      headWork :: (ctx, cfg, bb) :: tailWorks
