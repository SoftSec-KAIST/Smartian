namespace Smartian

open Utils

type CompareType = Equality | SignedSize | UnsignedSize

type BranchInfo = {
  InstAddr : uint64
  BrType   : CompareType
  TryVal   : bigint
  OpSize   : int
  Oprnd1   : bigint
  Oprnd2   : bigint
  Distance : bigint
}

type BranchPoint = {
  Addr : uint64
  Idx  : int
}

type Context = {
  Bytes : byte array
  ByteDir : Direction
}

module BranchInfo =

  let toString
    { TryVal = x; InstAddr = addr; BrType = typ; Oprnd1 = v1; Oprnd2 = v2} =
    Printf.sprintf "Try = %A : %A vs %A @ 0x%x (%A)" x v1 v2 addr typ

  let interpretAs sign size (x: bigint) =
    match sign with
    | Signed ->
      let signedMax = getSignedMax size
      if x > signedMax then x - (getUnsignedMax size) - 1I else x
    | Unsigned -> x