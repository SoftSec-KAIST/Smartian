namespace Smartian

open Utils

module List =

  let foldi f init list =
    let folder (i, acc) elem = (i + 1, f acc i elem)
    List.fold folder (0, init) list |> snd

module Array =

  let shuffle arr =
    let arr = Array.copy arr
    let swap i j =
      let tmp = arr.[i]
      arr.[i] <- arr.[j]
      arr.[j] <- tmp
    Array.iteri (fun i _ -> swap i (random.Next(i, arr.Length))) arr
    arr

  let collecti f arr =
    Array.mapi (fun i x -> (i, x)) arr
    |> Array.collect (fun (i, x) -> f i x)
