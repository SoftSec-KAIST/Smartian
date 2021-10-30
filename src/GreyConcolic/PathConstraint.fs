namespace Smartian

open BytesUtils

(* Represents condition in interval domain. *)

type Interval = Between of (bigint * bigint) | Bottom | Top

module Interval =

  let bot = Bottom

  let top = Top

  let make (low, high) = Between (low, high)

  let conjunction range1 range2 =
    match range1, range2 with
    | Top, _ -> range2
    | _, Top -> range1
    | Bottom, _ | _, Bottom -> Bottom
    | Between (low1, high1), Between (low2, high2) ->
      if high1 < low2 || high2 < low1
      then Bottom
      else Between (max low1 low2, min high1 high2)

type ByteConstraint = Interval list // Disjunction of each range

module ByteConstraint =

  // keyword 'false' is reserved, so cannot be used.
  let bot = []

  // keyword 'true' is reserved, so cannot be used.
  let top = [ Interval.top ]

  let isBot = List.forall (fun range -> range = Bottom)

  let isTop = List.exists (fun range -> range = Top)

  let make pairs = List.map Interval.make pairs

  let rec normalizeAux ranges accCond =
    match ranges with
    | [] -> accCond
    | Bottom :: tailRanges -> normalizeAux tailRanges accCond
    | Top :: tailRanges -> top
    | range :: tailRanges -> normalizeAux tailRanges (range :: accCond)

  let normalize ranges = normalizeAux ranges []

  let conjunction cond1 cond2 =
    let ranges =
      List.collect (fun r1 ->
        List.map (fun r2 -> Interval.conjunction r1 r2) cond2
      ) cond1
    normalize ranges

type Constraint = ByteConstraint list // Conjunction of each byte condition

module Constraint =

  // keyword 'false' is reserved, so cannot be used.
  let bot = [ ByteConstraint.bot ]

  // keyword 'true' is reserved, so cannot be used.
  let top = [ ]

  let isBot x = List.exists ByteConstraint.isBot x

  let isTop x = List.forall ByteConstraint.isTop x

  let make msbRanges endian size =
    if endian = BE then [ ByteConstraint.make msbRanges ] else
      let padding =
        List.map (fun _ -> ByteConstraint.top) (List.ofSeq { 1 .. (size - 1) })
      padding @ [ ByteConstraint.make msbRanges ]

  // Algin condition into the same length of byte constraint list
  let alignCondition cond1 cond2 =
    if List.length cond1 < List.length cond2 then
      let n = List.length cond2 - List.length cond1
      let padding = List.map (fun _ -> ByteConstraint.top) (List.ofSeq {1 .. n})
      (cond1 @ padding, cond2)
    else
      let n = List.length cond1 - List.length cond2
      let padding = List.map (fun _ -> ByteConstraint.top) (List.ofSeq {1 .. n})
      (cond1, cond2 @ padding)

  let conjunction cond1 cond2 =
    let cond1, cond2 = alignCondition cond1 cond2
    List.map2 ByteConstraint.conjunction cond1 cond2