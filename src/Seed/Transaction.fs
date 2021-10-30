namespace Smartian

open Nethermind.Dirichlet.Numerics
open Config
open Utils
open BytesUtils
open EVMAnalysis

/// Represents a function call with args.
type Transaction = {
  /// Target function.
  FuncSpec : FuncSpec
  /// An array of fields.
  Args : Arg array
  /// The index of 'Fields' to mutate for the next grey-box concolic testing.
  ArgCursor : int
  /// Sender of the transaction
  Sender : Sender
  /// Send the transaction via agent contract.
  UseAgent: bool
  Timestamp: int64
  Blocknum: int64
}

module Transaction =

  /// Initialize an input for the specified input source.
  let init funcSpec =
    { FuncSpec = funcSpec
      Args = Array.map Arg.init funcSpec.ArgSpecs
      ArgCursor = 0
      Sender = if funcSpec.OnlyOwner then Sender.TargetOwner else Sender.pick ()
      UseAgent = random.Next(100) < TRY_REENTRANCY_PROB
      Timestamp = DEFAULT_TIMESTAMP
      Blocknum = DEFAULT_BLOCKNUM }

  /// Postprocess to ensure that the transaction is valid for a constructor.
  let fixForConstructor tx =
    let args = Array.map Arg.fixForConstructor tx.Args
    { tx with Args = args; Sender = Sender.TargetOwner }

  /// Return a deep-copied input of a given input.
  let copy tx =
    { tx with Args = Array.map Arg.copy tx.Args }

  /// Concretize an input into a TXData.
  let concretize tx =
    let funcSpec = tx.FuncSpec
    let funcName = FuncSpec.getName funcSpec
    let payable = funcSpec.Payable
    let args = Array.map Arg.concretize tx.Args
    let from = Address.contractOf tx.Sender // Always set the contract as sender
    let ``to`` = if tx.UseAgent then Address.contractOf tx.Sender
                 else Address.TARG_CONTRACT
    let origValue = if payable then bytesToUnsignedBigInt LE args.[0].Bytes
                    else 0I
    let origData = TXData.makeData funcSpec args.[1..]
    let value = if tx.UseAgent then UInt256 0I else UInt256 origValue
    let data = if tx.UseAgent
               then TXData.makeForAgent Address.TARG_CONTRACT origValue origData
               else origData
    { From = from
      To = ``to``
      Value = value
      Data = data
      Timestamp = tx.Timestamp
      Blocknum = tx.Blocknum
      Function = funcName
      OrigData = origData
      OrigValue = origValue }

  /// Check if the transaction has no arguments.
  let isEmpty tx =
    Array.isEmpty tx.Args

  /// Get the current field pointed by cursor.
  let getCurArg tx =
    tx.Args.[tx.ArgCursor]

  /// Replace the current field pointed by cursor.
  let setCurArg tx newArg =
    let newArgs = Array.copy tx.Args
    newArgs.[tx.ArgCursor] <- newArg
    { tx with Args = newArgs }

  /// Set the field cursor position of the given input.
  let setCursor tx newPos =
    { tx with Transaction.ArgCursor = newPos }

  let rewindByteCursors tx =
    let collector i arg =
      let newTx = setCursor tx i
      let newArgs = Arg.rewindByteCursors arg
      Array.map (setCurArg newTx) newArgs
    Array.collecti collector tx.Args

  /// Stringfy a transaction.
  let toString tx =
    let noPay = not tx.FuncSpec.Payable
    let funcStr = sprintf "Function: %s, " (FuncSpec.getName tx.FuncSpec)
    let senderStr = sprintf "From: %A, " tx.Sender
    let agentStr = sprintf "UseAgent: %b\n" tx.UseAgent
    let infoStr = funcStr + senderStr + agentStr
    let makeArgStr i arg =
      let argName = if i = 0 then "Value" else sprintf "Arg%d" i
      let argType = arg.Spec.TypeStr
      let argInfo = sprintf "          %s : %s" argName argType
      let argElems = if i = 0 && noPay then [| Element.ZERO |] else arg.Elems
      let mapper elem = "                 " + Element.toString elem
      let argData = Array.map mapper argElems |> String.concat "\n"
      argInfo + "\n" + argData
    let argStr = Array.mapi makeArgStr tx.Args |> String.concat "\n"
    infoStr + argStr
