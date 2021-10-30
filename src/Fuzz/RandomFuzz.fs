module Smartian.RandomFuzz

open Config
open Utils
open BytesUtils
open EVMAnalysis

let private MUTATE_MAX_POW = 7
let private ARITH_MAX = 35

// Mutable variables for statistics management.
let mutable private recentExecNums: Queue<int> = Queue.empty
let mutable private recentNewPathNums: Queue<int> = Queue.empty

let updateStatus execN newPathN =
  let recentExecNums' = if Queue.size recentExecNums > CHECK_LAST_N_ROUND
                        then Queue.drop recentExecNums
                        else recentExecNums
  recentExecNums <- Queue.enqueue recentExecNums' execN
  let recentNewPathNums' = if Queue.size recentNewPathNums > CHECK_LAST_N_ROUND
                           then Queue.drop recentNewPathNums
                           else recentNewPathNums
  recentNewPathNums <- Queue.enqueue recentNewPathNums' newPathN

let evaluateEfficiency () =
  let execNum = List.sum (Queue.elements recentExecNums)
  let newPathNum = List.sum (Queue.elements recentNewPathNums)
  if execNum = 0 then 1.0 else float newPathNum / float execNum

(*** Transaction-level mutations ***)

let private insertTransaction contSpec seed =
  let funcSpecs = contSpec.NormalFunctions
  let funcSpec = funcSpecs.[ random.Next(funcSpecs.Length) ]
  let newTx = Transaction.init funcSpec
  let txNum = Seed.getTransactionCount seed
  // Avoid inserting in front of the deploying transaction.
  let insertIdx = random.Next(1, txNum)
  Seed.insertTransactionAt seed insertIdx newTx

let private shuffleTransaction seed =
  let txNum = Seed.getTransactionCount seed
  if txNum < 3 then seed
  else // Avoid shuffling with the deploying transaction.
    match randomSelect (List.ofSeq { 1 .. (txNum - 1) }) 2 with
    | [idx1; idx2] -> Seed.swapTransactions seed idx1 idx2
    | _ -> failwith "Unreachable"

let private removeTransaction seed =
  let txNum = Seed.getTransactionCount seed
  if txNum < 3 then seed
  else
    // Avoid removing the deploying transaction.
    let removeIdx = random.Next(1, txNum)
    Seed.removeTransactionAt seed removeIdx

let private mutateTransactionSender seed =
  let txNum = Seed.getTransactionCount seed
  // Avoid mutating the sender of the deploying transaction.
  let mutIdx = random.Next(1, txNum)
  Seed.mutateTranasctionSenderAt seed mutIdx

(*** Argument mutation. ***)

let private flipBit elem =
  let bytePos = random.Next(Element.getByteLen elem)
  let bitPos = random.Next(8)
  Element.flipBitAt elem bytePos bitPos

let private randomByte elem =
  let i = random.Next(Element.getByteLen elem)
  let newByte = allBytes.[random.Next(allBytes.Length)]
  Element.updateByteAt elem i newByte

let private arithMutate elem =
  let i = random.Next(Element.getByteLen elem)
  let curVal = Element.getByteValAt elem i |> ByteVal.getConcreteByte |> uint32
  let delta = uint32 (1 + random.Next(ARITH_MAX))
  let newVal = if random.Next(2) = 0 then curVal + delta else curVal - delta
  let newByte = byte (newVal &&& 0xffu)
  Element.updateByteAt elem i newByte

let private BOUNADRY_BYTES =
  [| 0uy; 1uy; 0x3fuy; 0x40uy; 0x41uy; 0x7fuy; 0x80uy; 0x81uy; 0xffuy;|]

let private pickBoundaryByte () =
  BOUNADRY_BYTES.[random.Next(BOUNADRY_BYTES.Length)]

let private tryInterestingByte elem =
  let i = random.Next(Element.getByteLen elem)
  let newByte = pickBoundaryByte ()
  Element.updateByteAt elem i newByte

// TODO: Cleanup
let private pickBoundaryIntBytes width =
  if width < 2 then failwithf "Invalid width: %d" width
  let ZEROS = Array.create (width - 2) 0uy
  let MASKS = Array.create (width - 2) 0xFFuy
  match random.Next(9) with
  | 0 -> Array.concat [ [| 0x00uy |]; ZEROS; [|0x00uy|] ] // 0000 .. 0000
  | 1 -> Array.concat [ [| 0x00uy |]; ZEROS; [|0x01uy|] ] // 0000 .. 0001
  | 2 -> Array.concat [ [| 0x3Fuy |]; MASKS; [|0xFFuy|] ] // 3FFF .. FFFF
  | 3 -> Array.concat [ [| 0x40uy |]; ZEROS; [|0x00uy|] ] // 4000 .. 0000
  | 4 -> Array.concat [ [| 0x40uy |]; ZEROS; [|0x01uy|] ] // 4000 .. 0001
  | 5 -> Array.concat [ [| 0x7Fuy |]; MASKS; [|0xFFuy|] ] // 7FFF .. FFFF
  | 6 -> Array.concat [ [| 0x80uy |]; ZEROS; [|0x00uy|] ] // 8000 .. 0000
  | 7 -> Array.concat [ [| 0x80uy |]; ZEROS; [|0x01uy|] ] // 8000 .. 0001
  | 8 -> Array.concat [ [| 0xFFuy |]; MASKS; [|0xFFuy|] ] // FFFF .. FFFF
  | _ -> failwith "Invalid mutation code"
  |> Array.rev // Since we use little endian for integer types.

let private pickInterestingElemBytes elemType =
  match elemType with
  | Int width | UInt width ->
    if width = 1 then [| pickBoundaryByte () |]
    else pickBoundaryIntBytes width
  | Address -> Address.pickInteresting() |> Address.toBytes LE
  | Bool -> [| 0uy |]
  | Byte -> [| pickBoundaryByte() |]
  | String -> [| 0x41uy; 0x42uy; 0x43uy; 0uy |]
  | Array _ -> failwith "Array type not allowed for an element"

let private tryInterestingElem elem =
  let newBytes = pickInterestingElemBytes elem.ElemType
  let newByteVals = Array.map ByteVal.newByteVal newBytes
  { elem with ByteVals = newByteVals }

let private mutateElem elem =
  match random.Next(5) with
  // Type-unaware mutations.
  | 0 -> flipBit elem
  | 1 -> randomByte elem
  | 2 -> arithMutate elem
  | 3 -> tryInterestingByte elem
  // Type-aware mutation.
  | 4 -> tryInterestingElem elem
  | _ -> failwith "Invalid mutation code"

let private mutateTransactionArg seed =
  let curElem = Seed.getCurElem seed
  let newElem = mutateElem curElem
  Seed.setCurElem seed newElem |> Seed.fixDeployTransaction

let private mutateSeed contSpec seed =
  if not (Seed.isInputCursorValid seed) then
    // If all the transactions in the seed have no argument.
    match random.Next(4) with
    | 0 -> insertTransaction contSpec seed
    | 1 -> shuffleTransaction seed
    | 2 -> removeTransaction seed
    | 3 -> mutateTransactionSender seed
    | _ -> failwith "Invalid mutation code"
  else
    let seed = Seed.shuffleCursor seed
    match random.Next(16) with
    | 0 -> insertTransaction contSpec seed
    | 1 -> shuffleTransaction seed
    | 2 -> removeTransaction seed
    | 3 -> mutateTransactionSender seed
    | _ -> mutateTransactionArg seed

let rec private repRandMutateAux contSpec seed depth depthLimit accumSeed =
  if depth >= depthLimit then accumSeed else
    let accumSeed = mutateSeed contSpec accumSeed
    repRandMutateAux contSpec seed (depth + 1) depthLimit accumSeed

let private repRandMutate contSpec seed =
  let mutateN = 1 <<< (random.Next(MUTATE_MAX_POW))
  repRandMutateAux contSpec seed 0 mutateN seed
  |> Seed.resetCursor
  |> Seed.resetBlockData

let run seed opt contSpec =
  List.init Config.RAND_FUZZ_TRY_PER_SEED (fun _ -> repRandMutate contSpec seed)
  |> List.filter (TCManage.evalAndSave opt)
