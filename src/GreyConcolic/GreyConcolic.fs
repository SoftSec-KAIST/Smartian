module Smartian.GreyConcolic

open Config
open Utils
open Options

// Mutable variables for statistics management.
let mutable private recentExecNums: Queue<int> = Queue.empty
let mutable private recentNewPathNums: Queue<int> = Queue.empty

let updateStatus execN newPathN =
  let recentExecNums' = if Queue.size recentExecNums > CHECK_LAST_N_ROUND
                        then Queue.drop recentExecNums
                        else recentExecNums
  recentExecNums <- Queue.enqueue recentExecNums' execN
  let recentNewPathNums' = if Queue.size recentNewPathNums > CHECK_LAST_N_ROUND
                           then Queue.drop recentNewPathNums
                           else recentNewPathNums
  recentNewPathNums <- Queue.enqueue recentNewPathNums' newPathN

let evaluateEfficiency () =
  let execNum = List.sum (Queue.elements recentExecNums)
  let newPathNum = List.sum (Queue.elements recentNewPathNums)
  if execNum = 0 then 1.0 else float newPathNum / float execNum

let run seed opt =
  let curByteVal = Seed.getCurByteVal seed
  let minByte, maxByte = ByteVal.getMinMax curByteVal
  if minByte = maxByte then []
  else
    let minVal, maxVal = bigint (int minByte), bigint (int maxByte)
    let branchTraces, spawnSeeds = BranchTrace.collect seed opt minVal maxVal
    let byteDir = Right // TODO: Cleanup.
    let bytes = Seed.queryNeighborBytes seed byteDir
    let ctx = { Bytes = bytes; ByteDir = byteDir }
    let branchTree = BranchTree.make opt ctx branchTraces
    let branchTree = BranchTree.selectAndRepair opt branchTree
    GreySolver.clearSolutionCache ()
    let solutions = GreySolver.solve seed opt byteDir branchTree
    solutions @ spawnSeeds
    |> List.map Seed.fixDeployTransaction
    |> List.filter (TCManage.evalAndSave opt)
