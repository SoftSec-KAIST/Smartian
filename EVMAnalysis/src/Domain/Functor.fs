namespace EVMAnalysis.Domain

open System.Collections.Generic

exception NotSingletonException

/// Set domain with threshold on the number of elements.
type SetDomain<'a when 'a : comparison> (Elem) =
  inherit AbstractDomain<Set<'a>>()
  let Elem = Elem :> Elem<'a>

  override __.bot = Set.empty

  override __.leq x y =
    Set.isSubset x y

  override __.join x y =
    Set.union x y

  override __.toString x =
    if Set.isEmpty x then "{ }"
    else "{ " + (Set.map Elem.toString x |> String.concat ", ") + " }"

  override __.isBot x = Set.isEmpty x

  member __.make elems = Set.ofList elems

  member __.add elem x = Set.add elem x

  member __.count x = Set.count x

  member __.map x = Set.map x

  member __.diff x y = Set.difference x y

  member __.exists predicate x = Set.exists predicate x

  member __.forall predicate x = Set.forall predicate x

  member __.filter predicate x = Set.filter predicate x

  member __.getSingleton (x: Set<'a>) =
    if Set.count x <> 1 then raise NotSingletonException else Set.minElement x

  member __.minBy f (x: Set<'a>) =
    Set.toList x
    |> List.minBy f

  member __.maxBy f (x: Set<'a>) =
    Set.toList x
    |> List.maxBy f

  member __.toList (x: Set<'a>) = Set.toList x

type Prod3Domain<'a, 'b, 'c> (ADom, BDom, CDom) =
  inherit AbstractDomain<'a * 'b * 'c>()
  let ADom = ADom :> AbstractDomain<'a>
  let BDom = BDom :> AbstractDomain<'b>
  let CDom = CDom :> AbstractDomain<'c>

  override __.bot = (ADom.bot, BDom.bot, CDom.bot)

  override __.leq x y =
    let xa, xb, xc = x
    let ya, yb, yc = y
    ADom.leq xa ya && BDom.leq xb yb && CDom.leq xc yc

  override __.join x y =
    let xa, xb, xc = x
    let ya, yb, yc = y
    (ADom.join xa ya, BDom.join xb yb, CDom.join xc yc)

  override __.toString x =
    let a, b, c = x
    sprintf "<%s, %s, %s>" (ADom.toString a) (BDom.toString b) (CDom.toString c)

  override __.isBot x =
    let a, b, c  = x
    ADom.isBot a && BDom.isBot b && CDom.isBot c

type FunDomain<'k,'v when 'k:comparison> (KeyElem, ValDom) =
  inherit AbstractDomain<Map<'k,'v>>()
  let KeyElem = KeyElem :> Elem<'k>
  let ValDom = ValDom :> AbstractDomain<'v>

  override __.bot = Map.empty

  override __.leq x y =
    Map.forall
      (fun k v_x ->
        try ValDom.leq v_x (Map.find k y) with :? KeyNotFoundException -> false
      ) x

  override __.join x y =
    let folder accMap k v =
      match Map.tryFind k accMap with
      | None -> Map.add k v accMap
      | Some v' -> Map.add k (ValDom.join v v') accMap
    Map.fold folder x y

  override __.toString x =
    let folder accStr k v =
      let keyStr = KeyElem.toString k
      let valStr = ValDom.toString v
      accStr + "\n" + (sprintf "%s -> %s" keyStr valStr)
    Map.fold folder "" x

  override __.isBot x = Map.isEmpty x

  // Map operation functions.

  member __.contains k (m: Map<'k,'v>) =
    Map.containsKey k m

  member __.find k (m: Map<'k,'v>) =
    try Map.find k m with :? KeyNotFoundException -> ValDom.bot

  member __.add k v (m : Map<'k,'v>) =
    Map.add k v m
