namespace EVMAnalysis

open EVMAnalysis.Domain.Taint
open EVMAnalysis.Domain.AbsVal

/// Information obtained as a result of an analysis.
type FuncInfo = {
  FuncSpec : FuncSpec
  // Variables tainted by the constructor. Read-only for non-constructor funcs.
  ConstrTainted : Set<Variable>
  // Variables defined (i.e. SSTORE) by this function.
  Defs : Set<Variable>
  // Variables used (i.e. SLOAD) by this function.
  Uses : Set<Variable>
}

module FuncInfo =

  let init func constrTainted =
    { FuncSpec = func
      ConstrTainted = constrTainted
      Defs = Set.empty
      Uses = Set.empty }

  let print funcInfo =
    let name = FuncSpec.getName funcInfo.FuncSpec
    let ownerStr = if funcInfo.FuncSpec.OnlyOwner then " (onlyOwner)" else ""
    let defStr = Set.map Variable.toString funcInfo.Defs |> String.concat ", "
    let useStr = Set.map Variable.toString funcInfo.Uses |> String.concat ", "
    printfn "%s:%s Def = { %s }, Use = { %s }" name ownerStr defStr useStr

  let addCheckSender funcInfo =
    let newFuncSpec = { funcInfo.FuncSpec with OnlyOwner = true }
    { funcInfo with FuncSpec = newFuncSpec }

  let addSStore k v funcInfo =
    let vars = AbsVal.toVariables k
    let isConstr = (funcInfo.FuncSpec.Kind = Constructor)
    let isCallerTainted = AbsVal.hasTaint Taint.Caller v
    let isArgTainted = AbsVal.hasTaint Taint.ConstrArg v
    let constrTainted = if isConstr && (isCallerTainted || isArgTainted)
                        then Set.union vars funcInfo.ConstrTainted
                        else funcInfo.ConstrTainted
    let defs = Set.union vars funcInfo.Defs
    { funcInfo with ConstrTainted = constrTainted; Defs = defs }

  let addSLoad k funcInfo =
    let uses = Set.union funcInfo.Uses (AbsVal.toVariables k)
    { funcInfo with Uses = uses }
