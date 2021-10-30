namespace Smartian

open System
open Utils

type Tendency = Incr | Decr | Undetermined

/// Represents interval [a,b] where f is monotonic, and f(a) < k < f(b)
type Monotonicity = {
  LowerX   : bigint // a
  LowerY   : bigint option // f(a) (Used only in stringfy function)
  UpperX   : bigint // b
  UpperY   : bigint option // f(b) (Used only in stringfy function)
  TargetY  : bigint // k
  Tendency : Tendency
  ByteLen  : int
}

module Monotonicity =

  let private checkIntermediate tendency y1 y2 y3 =
    match tendency with
    | Incr -> y1 < y2 && y2 < y3
    | Decr -> y1 > y2 && y2 > y3
    | Undetermined -> failwith "Invalid tendency input"

  (* Note that call to checkIntermediate should be preceded *)
  let private make tendency a fa b fb k =
    { LowerX = a; LowerY = fa; UpperX = b; UpperY = fb
      TargetY = k; Tendency = tendency; ByteLen = 1 }

  let rec private checkMonotonicAux sign prevX prevY tendency coordinates =
    match coordinates with
    | [] -> Some tendency
    | (x, y:bigint) :: tailCoords ->
      if x <= prevX then failwith "Invalid coordinates"
      if tendency = Incr && prevY <= y then
        checkMonotonicAux sign x y Incr tailCoords
      elif tendency = Incr && sign = Signed && prevY > 0I && y < 0I then
        // Let's give one more chance, since there could be an overflow
        checkMonotonicAux sign x y Incr tailCoords
      elif tendency = Decr && prevY >= y then
        checkMonotonicAux sign x y Decr tailCoords
      elif tendency = Decr && sign = Signed && prevY < 0I && y > 0I then
        // Let's give one more chance, since there could be an underflow
        checkMonotonicAux sign x y Decr tailCoords
      elif tendency = Undetermined && prevY = y then
        checkMonotonicAux sign x y Undetermined tailCoords
      elif tendency = Undetermined && prevY < y then
        checkMonotonicAux sign x y Incr tailCoords
      elif tendency = Undetermined && prevY > y then
        checkMonotonicAux sign x y Decr tailCoords
      else None (* Monotonicity violated *)

  let checkMonotonic sign coordinates =
    match coordinates with
    | [] -> failwith "Empty coordinate list provided as input"
    | (firstX, firstY) :: tailCoordinates ->
      checkMonotonicAux sign firstX firstY Undetermined tailCoordinates

  let rec generateAux tendency targY prevX prevY coordinates =
    match coordinates with
    | [] -> None
    | (x, y) :: tailCoords ->
      if prevY = targY || y = targY then
        // One of the spawned seed already penetrates this EQ check. In this
        // case, we don't have to search on this monotonicity.
        None
      elif checkIntermediate tendency prevY targY y then
        Some (make tendency prevX (Some prevY) x (Some y) targY)
      else generateAux tendency targY x y tailCoords

  let generate tendency targY coordinates =
    if tendency = Undetermined then failwith "Invalid tendency input"
    match coordinates with
    | [] -> failwith "Empty coordinate list provided as input"
    | (firstX, firstY) :: tailCoords ->
      generateAux tendency targY firstX firstY tailCoords

  let find brInfos =
    let headBrInfo =
      match brInfos with
      | brInfo :: _ -> brInfo
      | _ -> failwith "Empty branchInfo list provided as input"
    let sign = if headBrInfo.BrType = UnsignedSize then Unsigned else Signed
    let size = headBrInfo.OpSize
    if List.forall (fun f -> f.Oprnd1 = headBrInfo.Oprnd1) brInfos then
      // *.Oprnd1 is constant, so infer monotonicity in *.Oprnd2
      let targetY = BranchInfo.interpretAs sign size headBrInfo.Oprnd1
      let toCoord br = (br.TryVal, BranchInfo.interpretAs sign size br.Oprnd2)
      let coordinates = List.map toCoord brInfos
      match checkMonotonic sign coordinates with
      | None -> None
      | Some tendency -> generate tendency targetY coordinates
    elif List.forall (fun f -> f.Oprnd2 = headBrInfo.Oprnd2) brInfos then
      // *.Oprnd2 is constant, so infer monotonicity in *.Oprnd1
      let targetY = BranchInfo.interpretAs sign size headBrInfo.Oprnd2
      let toCoord br = (br.TryVal, BranchInfo.interpretAs sign size br.Oprnd1)
      let coordinates = List.map toCoord brInfos
      match checkMonotonic sign coordinates with
      | None -> None
      | Some tendency -> generate tendency targetY coordinates
    else None

  let adjustByteLen monotonic =
    let lowerX = monotonic.LowerX
    let upperX = monotonic.UpperX
    if upperX - lowerX > 1I then monotonic else
      // Time to extend chunk size
      // Set the range conservatively, considering the adjacent bytes
      let newLowerX, newUpperX = lowerX <<< 8, (upperX <<< 8) + 255I
      { monotonic with
          LowerX = newLowerX
          LowerY = None
          UpperX = newUpperX
          UpperY = None
          ByteLen = monotonic.ByteLen + 1 }

  let updateInterval monotonic x y =
    match monotonic.Tendency with
    | Incr ->
      if y < monotonic.TargetY then // We're not there yet
        { monotonic with LowerX = x; LowerY = Some y }
      else // We've come too far
        { monotonic with UpperX = x; UpperY = Some y}
    | Decr ->
      if y < monotonic.TargetY then // We've come too far
        { monotonic with UpperX = x; UpperY = Some y }
      else // We're not there yet
        { monotonic with LowerX = x; LowerY = Some y }
    | Undetermined -> failwith "Unreachable"

  let update monotonic x y =
    updateInterval monotonic x y |> adjustByteLen

  let toString mono =
    let a = mono.LowerX.ToString()
    let b = mono.UpperX.ToString()
    let fa = match mono.LowerY with
             | None -> "?"
             | Some (bi: bigint) -> bi.ToString()
    let fb = match mono.UpperY with
             | None -> "?"
             | Some (bi: bigint) -> bi.ToString()
    let k = mono.TargetY.ToString()
    Printf.sprintf "f(%s)=%s < %s < f(%s)=%s" a fa k b fb
