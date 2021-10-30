namespace EVMAnalysis

open EVMAnalysis.Const

// Temporary registers that appear in IR.
type Register = string

// Code address.
type Addr = uint64

module Addr =

  let toString (addr: Addr) = sprintf "0x%x" addr

  let ofBigInt (i: bigint) =
    if i > MAX_UINT64 then
      printfn "[WARNING] Addr.ofBigInt(0x%s)" (i.ToString("X"))
    uint64 (i &&& MAX_UINT64)

type Subrtn = Addr
module Subrtn = Addr

// Call context.
type Context = Addr list

module Context =

  let toString (ctx: Context) =
    let addrStrs = List.map Addr.toString ctx |> String.concat ", "
    "[" + addrStrs + "]"

// Partitioning index for context- and flow-sensitive analysis.
type PartitionIdx = Context * Addr

module PartitionIdx =

  let toString (idx: PartitionIdx) =
    let ctx, addr = idx
    sprintf "Ctx = %s Addr = %s" (Context.toString ctx) (Addr.toString addr)

// State variables.
type Variable =
  | Singleton of bigint
  | ArrayVar of id: bigint * offset: bigint
  | MapVar of id: bigint * offset: bigint

module Variable =

  let toString = function
    | Singleton addr -> "var_"  + addr.ToString()
    | ArrayVar (i, o) ->
      let arrPart = "arr_" + i.ToString()
      let offPart = if o = 0I then "" else ".off_" + o.ToString()
      arrPart + offPart
    | MapVar (i, o) ->
      let mapPart = "map_" + i.ToString()
      let offPart = if o = 0I then "" else ".off_" + o.ToString()
      mapPart + offPart

type DUChain = string * Variable * string // Def function * Var * Use function.

module DUChain =

  let toString (chain: DUChain) =
    let defFunc, var, useFunc = chain
    sprintf "%s -- (%s) --> %s" defFunc (Variable.toString var) useFunc
