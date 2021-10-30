module EVMAnalysis.Domain.RegMap

open EVMAnalysis
open EVMAnalysis.Domain.AbsVal

type RegisterModule () =
  inherit Elem<Register>()
  override __.toString reg = reg
  member __.ofTmpVar tmpVarNo =
    sprintf "T_%d" tmpVarNo

let Register = RegisterModule () // Use 'Register' like a module.

type RegMap = Map<Register,AbsVal>

type RegMapModule () =
  inherit FunDomain<Register,AbsVal>(Register, AbsVal)

let RegMap = RegMapModule () // Use 'RegMap' like a module.
