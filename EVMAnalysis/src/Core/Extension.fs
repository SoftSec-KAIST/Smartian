namespace EVMAnalysis

module Map =

  let keys map =
    Map.fold (fun acc k _ -> k :: acc) [] map

module Set =

  let diff = Set.difference

  let inter = Set.intersect

  let choose f set =
    let folder acc elem =
      match f elem with
      | None -> acc
      | Some x -> Set.add x acc
    Set.fold folder Set.empty set

  let collect f set =
    let folder acc elem = Set.union (f elem) acc
    Set.fold folder Set.empty set

module Dictionary =

  let toSeq d = d |> Seq.map (fun (KeyValue(k,v)) -> (k,v))

module BigInt =

  let rec exp b e =
    if e <= 0I then 1I else b * (exp b (e - 1I))
