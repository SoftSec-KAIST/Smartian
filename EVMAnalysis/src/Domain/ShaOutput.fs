module EVMAnalysis.Domain.ShaOutput

open EVMAnalysis
open EVMAnalysis.Const
open EVMAnalysis.Domain.FlatInt

type ShaOutput =
  // Abstracts Sha(id) + offset, where 'id' is a unique ID assigned to an array.
  | Sha32Byte of id: bigint * offset : bigint
  // Abstracts Sha(key || id) + offset, where 'id' is a unique ID assigned to a
  // map. Here, || means concat. We ignore 'key' for element-insensitiveness.
  | Sha64Byte of id: bigint * offset : bigint
  // Abstracts Sha(... (Sha(id) + offset) ...) + ?. We will distinguish arrays
  // or maps up to only one dimension when they are nested.
  | Sha32ByteMult of id: bigint * offset : bigint
  // Abstracts Sha(... (Sha(key || id) + offset) ...) + ?. We will distinguish
  // arrays or maps up to only one dimension when they are nested.
  | Sha64ByteMult of id: bigint * offset : bigint

type ShaOutputModule () =
  inherit Elem<ShaOutput>()
  override __.toString x =
    match x with
    | Sha32Byte (i, o) ->
      let shaPart = sprintf "Sha(0x" + i.ToString("X") + ")"
      let offPart = if o = 0I then "" else " + 0x" + o.ToString("X")
      shaPart + offPart
    | Sha64Byte (i, o) ->
      let shaPart = sprintf "Sha(*, 0x" + i.ToString("X") + ")"
      let offPart = if o = 0I then "" else " + 0x" + o.ToString("X")
      shaPart + offPart
    | Sha32ByteMult (i, o) ->
      let shaPart = sprintf "Sha(0x" + i.ToString("X") + ")"
      let offPart = if o = 0I then "" else " + 0x" + o.ToString("X")
      "Sha(... (" + shaPart + offPart + ") ...) + ?"
    | Sha64ByteMult (i, o) ->
      let shaPart = sprintf "Sha(*, 0x" + i.ToString("X") + ")"
      let offPart = if o = 0I then "" else " + 0x" + o.ToString("X")
      "Sha(... (" + shaPart + offPart + ") ...) + ?"

  member __.add x n =
    match x with
    | Sha32Byte (i, o) -> Sha32Byte (i, (o + n) &&& MAX_UINT256)
    | Sha64Byte  (i, o) -> Sha64Byte (i, (o + n) &&& MAX_UINT256)
    | Sha32ByteMult (i, o) -> Sha32ByteMult (i, o) // Should not add here.
    | Sha64ByteMult (i, o) -> Sha64ByteMult (i, o) // Should not add here.

  member __.sub x n =
    match x with
    | Sha32Byte (i, o) -> Sha32Byte (i, (o - n) &&& MAX_UINT256)
    | Sha64Byte  (i, o) -> Sha64Byte (i, (o - n) &&& MAX_UINT256)
    | Sha32ByteMult (i, o) -> Sha32ByteMult (i, o) // Should not subtract here.
    | Sha64ByteMult (i, o) -> Sha64ByteMult (i, o) // Should not subtract here.

  member __.sha x =
    match x with
    | Sha32Byte (i, o) -> Sha32ByteMult (i, o) // Promote to Sha32ByteMult
    | Sha64Byte  (i, o) -> Sha64ByteMult (i, o) // Promote to Sha64ByteMult
    | Sha32ByteMult (i, o) -> Sha32ByteMult (i, o) // Stays the same.
    | Sha64ByteMult (i, o) -> Sha64ByteMult (i, o) // Stays the same.

  member __.toVar x =
    match x with
    | Sha32Byte (i, o) -> ArrayVar(i, o)
    | Sha64Byte (i, o) -> MapVar (i, o)
    | Sha32ByteMult (i, o) -> ArrayVar (i, o)
    | Sha64ByteMult (i, o) -> MapVar (i, o)

let ShaOutput = ShaOutputModule () // Use 'ShaOutput' like a module.

type ShaOutSet = Set<ShaOutput>

type ShaOutSetModule () =
  inherit SetDomain<ShaOutput>(ShaOutput)

  member __.ofArrID arrID = Set.ofList [Sha32Byte (arrID, 0I)]

  member __.ofMapID mapID = Set.ofList [Sha64Byte (mapID, 0I)]

  // Caution: name 'add' is already used by SetDomain functor.
  member __.addOp shaSet flatInt =
    match FlatInt.tryGetConst flatInt with
    | None -> shaSet // Handle a top integer as an array index term.
    | Some n -> Set.map (fun x -> ShaOutput.add x n) shaSet

  member __.sub shaSet flatInt =
    match FlatInt.tryGetConst flatInt with
    | None -> shaSet // Handle a top integer as an array index term.
    | Some n -> Set.map (fun x -> ShaOutput.sub x n) shaSet

  member __.sha shaSet =
    Set.map ShaOutput.sha shaSet

  member __.toVar shaSet =
    Set.map ShaOutput.toVar shaSet

let ShaOutSet = ShaOutSetModule() // Use 'ShaOutSet' like a module.
