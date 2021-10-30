namespace EVMAnalysis

open B2R2.MiddleEnd
open B2R2.MiddleEnd.BinEssence

type BasicBlock = BinGraph.Vertex<IRBasicBlock>

module BasicBlock =

  let getAddr (bb: BasicBlock) =
    bb.VData.PPoint.Address

  let getInstrs (bb: BasicBlock) =
    bb.VData.GetInsInfos()

  let getStmts (bb: BasicBlock) =
    bb.VData.GetIRStatements() |> Array.concat |> Array.toList
