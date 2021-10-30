namespace Smartian

open Nethermind.Core.Extensions
open Config
open Utils

/// A collection of input, which corresponds to a transaction sequence.
type Seed = {
  /// An array of transactions.
  Transactions : Transaction array
  /// The index of 'Inputs' to mutate for the next grey-box concolic testing.
  TXCursor : int
}

module Seed =

  let empty =
    { Transactions = [| |]
      TXCursor = 0 }

  /// Initialize a seed with specified function specs.
  let init cnstrFunc funcs =
    let deployTx = Transaction.init cnstrFunc |> Transaction.fixForConstructor
    let normalTxs = Array.map Transaction.init funcs
    let txs = Array.append [| deployTx |] normalTxs
    let idx = try Array.findIndex (not << Transaction.isEmpty) txs with _ -> -1
    { Transactions = txs; TXCursor = idx }

  /// Postprocess to ensure that the seed has a valid for deploying transaction.
  let fixDeployTransaction seed =
    let txs = seed.Transactions
    let newDeployTx = Transaction.fixForConstructor txs.[0]
    let newTxs = Array.append [| newDeployTx |] txs.[1..]
    { seed with Transactions = newTxs }

  /// Return a deep-copied seed of a given seed.
  let copy seed =
    { seed with Transactions = Array.map Transaction.copy seed.Transactions }

  let resetBlockData seed =
    let rec loop n (curTime, curNum) accList =
      if n <= 0 then List.rev accList |> Array.ofList
      else let curTime = curTime + int64(random.Next(TIMESTAMP_INC_MAX))
           let curNum = curNum + int64(random.Next(BLOCKNUM_INC_MAX))
           let accList = (curTime, curNum) :: accList
           loop (n - 1) (curTime, curNum) accList
    // Note we can perform shallow copy here, since we don't change TX args.
    let newTxs = Array.copy seed.Transactions
    let blockData = loop newTxs.Length (DEFAULT_TIMESTAMP, DEFAULT_BLOCKNUM) []
    let mapper tx (t, b): Transaction = { tx with Timestamp = t; Blocknum = b }
    { seed with Transactions = Array.map2 mapper newTxs blockData }

  /// Concretize a seed into a test case.
  let concretize seed: TestCase =
    let initEther = INITIAL_ETHER.Ether()
    let makeEntity account contract =
      { Balance = initEther
        Account = account
        Agent = SmartianAgent contract}
    let accounts = Address.OWNER_ACCOUNT :: Address.USER_ACCOUNTS
    let contracts = Address.OWNER_CONTRACT :: Address.USER_CONTRACTS
    let entities = List.map2 makeEntity accounts contracts
    let txArr = Array.map Transaction.concretize seed.Transactions
    let txList = Array.toList txArr
    let deployTx = List.head txList
    let normalTxs = List.tail txList
    { Entities = entities
      TargetDeployer = Address.OWNER_CONTRACT
      TargetContract = Address.TARG_CONTRACT
      DeployTx = deployTx
      Txs = normalTxs }

  (**************************** Getter functions ****************************)

  let getCurTransaction seed =
    seed.Transactions.[seed.TXCursor]

  let getCurArg seed =
    let curArg = getCurTransaction seed
    Transaction.getCurArg curArg

  let getCurElem seed =
    let curField = getCurArg seed
    Arg.getCurElem curField

  let getTransactionCount seed =
    seed.Transactions.Length

  /// Get the current ByteVal pointed by the cursor.
  let getCurByteVal seed =
    let curElem = getCurElem seed
    Element.getCurByteVal curElem

  (**************************** Setter functions ****************************)

  /// Replace the current transaction pointed by cursor.
  let setCurTransaction seed newInput =
    let newInputs = Array.copy seed.Transactions
    newInputs.[seed.TXCursor] <- newInput
    { seed with Transactions = newInputs }

  /// Replace the current argument pointed by cursor.
  let setCurArg seed newField =
    let curInput = getCurTransaction seed
    let newInput = Transaction.setCurArg curInput newField
    setCurTransaction seed newInput

  /// Replace the current element pointed by cursor.
  let setCurElem seed newElem =
    let curArg = getCurArg seed
    let newArg = Arg.setCurElem curArg newElem
    setCurArg seed newArg

  let private setCursor seed newPos =
    { seed with TXCursor = newPos }

  (**************************** Query functions ****************************)

  /// Check if the input cursor has a valid index.
  let isInputCursorValid seed =
    seed.TXCursor <> (-1)

  /// Find the remaining length toward the given direction, starting from the
  /// current byte position.
  let queryLenToward seed direction =
    let curElem = getCurElem seed
    let bytePos = curElem.ByteCursor
    match direction with
    | Stay -> failwith "queryLenToward() cannot be called with 'Stay'"
    | Right -> curElem.ByteVals.Length - bytePos
    | Left -> bytePos + 1

  // Auxiliary function for queryUpdateBound()
  let private queryUpdateBoundLeft (byteVals: ByteVal []) byteCursor =
    let byteVals' =
      if byteCursor - MAX_CHUNK_LEN >= 0
      then byteVals.[byteCursor - MAX_CHUNK_LEN .. byteCursor]
      else byteVals.[ .. byteCursor]
    // We use an heuristic to bound update until the adjacent *fixed* ByteVal.
    match Array.tryFindIndexBack ByteVal.isFixed byteVals' with
    | None -> byteVals'.Length
    | Some idx -> byteVals'.Length - idx - 1

  // Auxiliary function for queryUpdateBound()
  let private queryUpdateBoundRight (byteVals: ByteVal []) byteCursor maxLen =
    let byteVals' =
      if byteCursor + MAX_CHUNK_LEN < byteVals.Length
      then byteVals.[byteCursor .. byteCursor + MAX_CHUNK_LEN]
      else byteVals.[byteCursor .. ]
    // We use an heuristic to bound update until the adjacent *fixed* ByteVal.
    match Array.tryFindIndex ByteVal.isFixed byteVals' with
    | None -> min MAX_CHUNK_LEN (maxLen - byteCursor)
    | Some idx -> idx

  /// Find the maximum length that can be updated for grey-box concolic testing.
  let queryUpdateBound seed direction =
    let curElem = getCurElem seed
    let byteVals = curElem.ByteVals
    let byteCursor = curElem.ByteCursor
    match direction with
    | Stay -> failwith "queryUpdateBound() cannot be called with 'Stay'"
    | Left -> queryUpdateBoundLeft byteVals byteCursor
    | Right -> queryUpdateBoundRight byteVals byteCursor curElem.MaxLength

  /// Get adjacent concrete byte values, toward the given direction.
  let queryNeighborBytes seed direction =
    let curElem = getCurElem seed
    let byteVals = curElem.ByteVals
    let byteCursor = curElem.ByteCursor
    match direction with
    | Stay -> failwith "queryNeighborBytes() cannot be called with 'Stay'"
    | Right ->
      let upperBound = min (byteVals.Length - 1) (byteCursor + MAX_CHUNK_LEN)
      Array.map ByteVal.getConcreteByte byteVals.[byteCursor + 1 .. upperBound]
    | Left ->
      let lowerBound = max 0 (byteCursor - MAX_CHUNK_LEN)
      Array.map ByteVal.getConcreteByte byteVals.[lowerBound .. byteCursor - 1]

  (********************* Functions for grey-box concolic *********************)

  /// Impose a constraint with lower and upper bounds, on the ByteVal at the
  /// given offset.
  let constrainByteAt seed direction offset low upper =
    let curElem = getCurElem seed
    let byteCursor =
      match direction with
      | Stay -> failwith "constrainByteAt() cannot be called with 'Stay'"
      | Right -> curElem.ByteCursor + offset
      | Left -> curElem.ByteCursor - offset
    let newByteVals = Array.copy curElem.ByteVals
    let newByteVal = if low <> upper then Interval (low, upper) else Fixed low
    newByteVals.[byteCursor] <- newByteVal
    let newElem = { curElem with ByteVals = newByteVals }
    setCurElem seed newElem |> fixDeployTransaction

  /// Fix the current ByteVals pointed by the cursor, with the provided bytes.
  let fixCurBytes seed dir bytes =
    let nBytes = Array.length bytes
    let curElem = getCurElem seed
    let curByteVals = curElem.ByteVals
    let byteCursor = curElem.ByteCursor
    let startPos = if dir = Right then byteCursor else byteCursor - nBytes + 1
    let newByteVals =
      // Note that 'MaxLen' is already checked in queryUpdateBound().
      if startPos + nBytes > curByteVals.Length then
        let reqSize = startPos + nBytes - curByteVals.Length
        Array.append curByteVals (Array.init reqSize (fun _ -> Undecided 0uy))
      else Array.copy curByteVals
    Array.iteri (fun i b -> newByteVals.[startPos + i] <- Fixed b) bytes
    let newElem = { curElem with ByteVals = newByteVals }
    setCurElem seed newElem |> fixDeployTransaction

  /// Update the current ByteVal pointed by the cursor.
  let updateCurByte seed byteVal =
    let curElem = getCurElem seed
    let curByteVals = curElem.ByteVals
    let byteCursor = curElem.ByteCursor
    let newByteVals = Array.copy curByteVals
    newByteVals.[byteCursor] <- byteVal
    let newElem = { curElem with ByteVals = newByteVals }
    setCurElem seed newElem |> fixDeployTransaction

  (************************ Cursor shifting functions ***********************)

  /// Proceed the byte cursor of the current element.
  let stepByteCursor seed =
    let curElem = getCurElem seed
    match Element.stepCursor curElem with
    | None -> []
    | Some newElem -> [setCurElem seed newElem]

  let rewindByteCursors seed =
    let collector i tx =
      let newSeed = setCursor seed i
      let newTxs = Transaction.rewindByteCursors tx
      Array.map (setCurTransaction newSeed) newTxs
    Array.collecti collector seed.Transactions

  let private rewindTxCursor seed =
    let txs = seed.Transactions
    let i = try Array.findIndex (not << Transaction.isEmpty) txs with _ -> -1
    { seed with TXCursor = i }

  (******************* Transaction-level mutate functions *******************)

  /// Insert a new input at the given index of the seed.
  let insertTransactionAt seed insertIdx tx =
    // Note we can perform shallow copy here, since we don't change TX args.
    let txs = seed.Transactions
    let headTxs = txs.[ .. insertIdx]
    let tailTss = txs.[ (insertIdx + 1) .. ]
    let newTxs = Array.concat [headTxs; [| tx |]; tailTss]
    rewindTxCursor { seed with Transactions = newTxs }

  /// Swap the two inputs of a seed at specified index.
  let swapTransactions seed idx1 idx2 =
    // Note we can perform shallow copy here, since we don't change TX args.
    let newTxs = Array.copy seed.Transactions
    let tmpInput = newTxs.[idx1]
    newTxs.[idx1] <- newTxs.[idx2]
    newTxs.[idx2] <- tmpInput
    rewindTxCursor { seed with Transactions = newTxs }

  let removeTransactionAt seed idx =
    // Note we can perform shallow copy here, since we don't change TX args.
    let txs = seed.Transactions
    let headTxs = if idx <= 0 then [| |] else txs.[ .. (idx - 1)]
    let tailTxs = if idx = txs.Length - 1 then [| |] else txs.[ (idx + 1) .. ]
    let newTxs = Array.concat [headTxs; tailTxs]
    rewindTxCursor { seed with Transactions = newTxs }

  let mutateTranasctionSenderAt seed idx =
    // Note we can perform shallow copy here, since we don't change TX args.
    let newTxs = Array.copy seed.Transactions
    let tx = newTxs.[idx]
    let newSender = Sender.pick()
    let useAgent = random.Next(100) < TRY_REENTRANCY_PROB
    let newTx = { tx with Sender = newSender; UseAgent = useAgent }
    newTxs.[idx] <- newTx
    { seed with Transactions = newTxs }

  (************************ Cursor shuffling functions ***********************)

  /// Randomly move element cursor position within current field.
  let private shuffleElemCursor seed =
    let curField = getCurArg seed
    let curElemLen = curField.Elems.Length
    let newElemCursor = random.Next(curElemLen)
    let newField = Arg.setCursor curField newElemCursor
    setCurArg seed newField

  /// Randomly move field cursor position within current input.
  let private shuffleArgCursor seed =
    let curInput = getCurTransaction seed
    let curFldLen = curInput.Args.Length
    let newFldCursor = random.Next(curFldLen)
    let newInput = Transaction.setCursor curInput newFldCursor
    setCurTransaction seed newInput

  let rec private chooseInputCursor prevCursor inputs tryN =
    let newInputCursor = random.Next(Array.length inputs)
    if not (Transaction.isEmpty inputs.[newInputCursor]) then newInputCursor
    elif tryN > 5 then prevCursor // Exceed maximum trial count, stop shuffling.
    else chooseInputCursor prevCursor inputs tryN // retry

  /// Randomly move input cursor position of the seed.
  let private shuffleTransactionCursor seed =
    let newInpCursor = chooseInputCursor seed.TXCursor seed.Transactions 0
    { seed with TXCursor = newInpCursor }

  let shuffleCursor seed =
    seed
    |> shuffleTransactionCursor
    |> shuffleArgCursor
    |> shuffleElemCursor

  (************************** Cursor reset functions *************************)

  let private resetByteCursor elem =
    { elem with ByteCursor = 0 }

  let private resetByteCursorAllElems field =
    let newElems = Array.map resetByteCursor field.Elems
    { field with Elems = newElems }

  let private resetByteCursorAllFields input =
    let newFields = Array.map resetByteCursorAllElems input.Args
    { input with Args = newFields }

  let resetCursor seed =
    let newInputs = Array.map resetByteCursorAllFields seed.Transactions
    { seed with Transactions = newInputs }

  (*************************** Stringfy function ***************************)

  let toString seed =
    let inputCursor = seed.TXCursor
    let mapper idx tx =
      if idx = inputCursor // Let's mark the current input cursor point.
      then sprintf "( => TX %d <= ) %s" idx (Transaction.toString tx)
      else sprintf "(    TX %d    ) %s" idx (Transaction.toString tx)
    "\n" + (Array.mapi mapper seed.Transactions |> String.concat "\n") + "\n"
