namespace Smartian

open Config
open Utils
open Options

type BranchTrace = BranchInfo list

module BranchTrace =

  let collect seed opt minVal maxVal =
    let tryVals = sampleInt minVal maxVal N_SPAWN
    let tryBytes = List.map (fun v -> Sampled (byte v)) tryVals
    let trySeeds = List.map (Seed.updateCurByte seed) tryBytes
    let traces = List.map2 (Executor.getBranchTrace opt) trySeeds tryVals
    (traces, trySeeds)

  let getHeadAddr (brTrace: BranchTrace) =
    match brTrace with
    | [] -> failwith "getHeadAddr() called with an empty list"
    | bInfo :: _ -> bInfo.InstAddr

  let getNextAddr (brTrace: BranchTrace) =
    match brTrace with
    | [] -> failwith "getNextAddr() called with an empty list"
    | [ _ ] -> failwith "getNextAddr() called with a length-one list"
    | _ :: bInfo :: _ -> bInfo.InstAddr

  let isLongerThanOne (brTrace: BranchTrace) =
    match brTrace with
    | [] | [ _ ] -> false
    | _ -> true
