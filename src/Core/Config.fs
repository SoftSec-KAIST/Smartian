module Smartian.Config

let Debug = false

/// Scheduling related constants.
let EXEC_BUDGET_PER_ROUND = 400
let CHECK_LAST_N_ROUND = 10
let MIN_RESOURCE_RATIO = 0.1
let MAX_RESOURCE_RATIO = 1.0 - MIN_RESOURCE_RATIO
let DURABLE_QUEUE_SIZE = 1000

/// Seed mutation related constants.
let MAX_CHUNK_LEN = 32
let STRING_ELEM_LEN = 4
let RAND_FUZZ_TRY_PER_SEED = 100
let BRANCH_COMB_WINDOW = 6
let N_SOLVE = 600 // Grey-box concolic testing param (see the original paper).
let N_SPAWN = 10 // Grey-box concolic testing param (see the original paper).

/// Ethereum related constants.

let INITIAL_ETHER = 10000 /// Default ether in each account.
let TRY_REENTRANCY_PROB = 10 // Probability to try agnet contract (0 ~ 100).
let DEFAULT_TIMESTAMP = 10000000L
let TIMESTAMP_INC_MAX = 100
let DEFAULT_BLOCKNUM = 20000000L
let BLOCKNUM_INC_MAX = 100
let BLOCK_GASLIMIT = 80000000L
let TX_GASLIMIT  = 10000000L
let TX_GASPRICE  = 0L
