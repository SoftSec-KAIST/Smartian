module EVMAnalysis.Domain.AbsVal

open B2R2.BinIR
open EVMAnalysis
open EVMAnalysis.Domain.FlatInt
open EVMAnalysis.Domain.ShaOutput
open EVMAnalysis.Domain.Taint

type AbsVal = FlatInt * ShaOutSet * Taint

type AbsValModule () =
  inherit Prod3Domain<FlatInt, ShaOutSet, Taint>(FlatInt, ShaOutSet, Taint)

  member __.getInt (v: AbsVal) =
    let i, _, _ = v
    i

  member __.getSha (v: AbsVal) =
    let _, s, _ = v
    s

  member __.getTaint (v: AbsVal) =
    let _, _, t = v
    t

  member __.hasTaint t v =
    Taint.leq t (__.getTaint v)

  member __.unknown: AbsVal =
    (FlatInt.top, ShaOutSet.bot, Taint.bot)

  member __.Caller: AbsVal =
    (FlatInt.top, ShaOutSet.bot, Taint.Caller)

  member __.ConstrArg: AbsVal =
    (FlatInt.top, ShaOutSet.bot, Taint.ConstrArg)

  member __.ofBigInt i: AbsVal =
    (FlatInt.ofBigInt i, ShaOutSet.bot, Taint.bot)

  member __.ofAddr addr: AbsVal =
    (FlatInt.ofAddr addr, ShaOutSet.bot, Taint.bot)

  member __.ofTaint t: AbsVal =
    (FlatInt.top, ShaOutSet.bot, t)

  member __.tryGetConst v: bigint option =
    FlatInt.tryGetConst (__.getInt v)

  member __.sha32Byte v: AbsVal =
    let sha1 = match __.tryGetConst v with
               | None -> ShaOutSet.bot
               | Some i -> ShaOutSet.ofArrID i // 32byte SHA3 for array access.
    let sha2 = ShaOutSet.sha (__.getSha v)
    (FlatInt.top, ShaOutSet.join sha1 sha2, Taint.bot)

  member __.sha64Byte v: AbsVal =
    let sha1 = match __.tryGetConst v with
               | None -> ShaOutSet.bot
               | Some i -> ShaOutSet.ofMapID i // 64byte SHA3 for map access.
    let sha2 = ShaOutSet.sha (__.getSha v)
    (FlatInt.top, ShaOutSet.join sha1 sha2, Taint.bot)

  member __.notOp v: AbsVal =
    (FlatInt.notOp (__.getInt v), ShaOutSet.bot, __.getTaint v)

  member __.unOp opType v: AbsVal =
    let i, t = __.getInt v, __.getTaint v
    let i' = match opType with
             | UnOpType.NOT -> FlatInt.notOp i
             | _ -> FlatInt.top
    (i', ShaOutSet.bot, t)

  member __.exp v1 v2 =
    let i1, i2 = __.getInt v1, __.getInt v2
    let t = Taint.join (__.getTaint v1) (__.getTaint v2)
    let i = FlatInt.exp i1 i2
    (i, ShaOutSet.bot, t)

  member __.binOp opType v1 v2: AbsVal =
    let i1, i2 = __.getInt v1, __.getInt v2
    let i = match opType with
            | BinOpType.ADD -> FlatInt.add i1 i2
            | BinOpType.SUB -> FlatInt.sub i1 i2
            | BinOpType.MUL-> FlatInt.mul i1 i2
            | BinOpType.DIV -> FlatInt.div i1 i2
            | BinOpType.OR -> FlatInt.orOp i1 i2
            | BinOpType.AND -> FlatInt.andOp i1 i2
            | BinOpType.XOR -> FlatInt.xor i1 i2
            | _ -> FlatInt.top
    let s1, s2 = __.getSha v1, __.getSha v2
    let s = match opType with
            | BinOpType.ADD ->
              ShaOutSet.join (ShaOutSet.addOp s1 i2) (ShaOutSet.addOp s2 i1)
            | BinOpType.SUB -> ShaOutSet.sub s1 i2
            | _ -> ShaOutSet.bot
    let t = Taint.join (__.getTaint v1) (__.getTaint v2)
    (i, s, t)

  member __.relOp opType v1 v2: AbsVal =
    let t1, t2 = __.getTaint v1, __.getTaint v2
    (FlatInt.top, ShaOutSet.bot, Taint.join t1 t2)

  member __.extractLSBit v: AbsVal =
    (FlatInt.extractLSBit (__.getInt v), ShaOutSet.bot, __.getTaint v)

  member __.extractLSByte v: AbsVal =
    (FlatInt.extractLSByte (__.getInt v), ShaOutSet.bot, __.getTaint v)

  member __.toVariables v: Set<Variable> =
    let aggrVars = ShaOutSet.toVar (__.getSha v) // ArrVar and MapVar.
    match __.tryGetConst v with
    | None -> aggrVars
    | Some x -> Set.add (Singleton x) aggrVars

let AbsVal = AbsValModule() // Use 'AbsVal' like a module.
