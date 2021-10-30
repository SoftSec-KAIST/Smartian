namespace EVMAnalysis.Domain

[<AbstractClass>]
type AbstractDomain<'t>() =
  abstract member bot: 't
  abstract member leq: 't -> 't -> bool
  abstract member join: 't -> 't -> 't
  abstract member toString: 't -> string
  abstract member isBot: 't -> bool

[<AbstractClass>]
type Elem<'t>() =
  abstract member toString: 't -> string