module EVMAnalysis.Const

let MAX_UINT64 = System.Numerics.BigInteger.Pow(2I, 64) - 1I
let MAX_UINT256 = System.Numerics.BigInteger.Pow(2I, 256) - 1I
let STACK_PTR_REG = "SP" // Must be synchronized with B2R2 convention.
let INIT_STACK_PTR_VAL = 1000I // Any value that is easily distinguishable.
