module EVMAnalysis.Domain.Taint

open EVMAnalysis

type Source =
  | Caller  // EVM's caller opcode.
  | ConstrArg
  | Storage of Variable

type SourceModule () =
  inherit Elem<Source>()
  override __.toString src =
    match src with
    | Caller -> "CALLER"
    | ConstrArg -> "CONSTR_ARG"
    | Storage var -> Variable.toString var

let Source = SourceModule () // Use 'Source' like a module.

type Taint = Set<Source>

type TaintModule () =
  inherit SetDomain<Source>(Source)

  member __.Caller: Taint = Set.singleton Caller

  member __.ConstrArg: Taint = Set.singleton ConstrArg

  member __.ofVars = Set.map (fun v -> Storage v)

let Taint = TaintModule() // Use 'Taint' like a module.
