namespace Smartian

open System
open Utils
open BytesUtils
open Config
open EVMAnalysis
open Nethermind.Core
open Nethermind.Dirichlet.Numerics
open Nethermind.Abi
open FSharp.Data

type ArgData = {
  ArgSpec : ArgSpec
  Bytes : byte array
}

module ArgData =

  let rec private splitBytesAux width (byteArr: byte array) accElems =
    if width > byteArr.Length then accElems
    else let elemBytes = byteArr.[ .. (width - 1)]
         let accElems = Array.append accElems [| elemBytes |]
         splitBytesAux width byteArr.[width .. ] accElems

  let private splitBytes width byteArr =
    splitBytesAux width byteArr [| |]

  let private makeUInt byteArr =
    bytesToUnsignedBigInt LE byteArr

  let private makeUIntArray width byteArr =
    splitBytes width byteArr |> Array.map makeUInt

  let private makeInt byteArr =
    bytesToSignedBigInt LE byteArr

  let private makeIntArray width byteArr =
    splitBytes width byteArr |> Array.map makeInt

  let private makeAddress byteArr =
    Address.fromBytes LE byteArr

  let private makeAddressArray byteArr =
    splitBytes 20 byteArr |> Array.map makeAddress

  let private makeBool (byteArr: byte array) =
    byteArr.[0] = 0uy

  let private makeBoolArray (byteArr: byte array) =
    Array.map (fun (b: byte) -> b = 0uy) byteArr

  let private makeString byteArr =
    bytesToStr byteArr

  let private makeStringArray byteArr =
    splitBytes STRING_ELEM_LEN byteArr |> Array.map makeString

  let relocArray outerSize (arr: 'a array) =
    let outerLen = SizeType.decideLen outerSize
    if arr.Length % outerLen <> 0 then failwithf "Invalid length: %d" arr.Length
    let width = arr.Length / outerLen
    let indices = { 0 .. (outerLen - 1) } |> Seq.toArray
    let mapper idx = arr.[width * idx .. width * (idx + 1) - 1]
    Array.map mapper indices

  let pack argData =
    let argKind = argData.ArgSpec.Kind
    let byteArr = argData.Bytes
    match argKind with
    | UInt _ -> makeUInt byteArr |> box
    | Int _ -> makeInt byteArr |> box
    | Address -> makeAddress byteArr |> box
    | Bool -> makeBool byteArr |> box
    | Byte -> box byteArr.[0]
    | String -> box (makeString byteArr)
    // 1-dimensional arrays
    | Array (_, UInt width) -> makeUIntArray width byteArr |> box
    | Array (_, Int width) -> makeIntArray width byteArr |> box
    | Array (_, Address) -> makeAddressArray byteArr |> box
    | Array (_, Bool) -> makeBoolArray byteArr |> box
    | Array (_, Byte) -> box byteArr
    | Array (_, String) -> makeStringArray byteArr |> box
    // 2-dimensional arrays
    | Array (outerSize, Array (_, UInt width)) ->
      makeUIntArray width byteArr |> relocArray outerSize |> box
    | Array (outerSize, Array (_, Int width)) ->
      makeIntArray width byteArr |> relocArray outerSize |> box
    | Array (outerSize, Array (_, Address)) ->
      makeAddressArray byteArr |> relocArray outerSize |> box
    | Array (outerSize, Array (_, Bool)) ->
      makeBoolArray byteArr |> relocArray outerSize |> box
    | Array (outerSize, Array (_, Byte)) ->
      byteArr |> relocArray outerSize |> box
    | Array (outerSize, Array (_, String)) ->
      makeStringArray byteArr |> relocArray outerSize |> box
    // 3-dimensional arrays
    | Array (outerSize1, Array (outerSize2, Array (_, UInt width))) ->
      makeUIntArray width byteArr
      |> relocArray outerSize1 |> Array.map (relocArray outerSize2)
      |> box
    | Array (outerSize1, Array (outerSize2, Array (_, Int width))) ->
      makeIntArray width byteArr
      |> relocArray outerSize1 |> Array.map (relocArray outerSize2)
      |> box
    | Array (outerSize1, Array (outerSize2, Array (_, Address))) ->
      makeAddressArray byteArr
      |> relocArray outerSize1 |> Array.map (relocArray outerSize2)
      |> box
    | Array (outerSize1, Array (outerSize2, Array (_, Bool))) ->
      makeBoolArray byteArr
      |> relocArray outerSize1 |> Array.map (relocArray outerSize2)
      |> box
    | Array (outerSize1, Array (outerSize2, Array (_, Byte))) ->
      byteArr
      |> relocArray outerSize1 |> Array.map (relocArray outerSize2)
      |> box
    | Array (outerSize1, Array (outerSize2, Array (_, String))) ->
      makeStringArray byteArr
      |> relocArray outerSize1 |> Array.map (relocArray outerSize2)
      |> box
    | Array (_, Array (_, Array (_, Array _))) -> failwith "Unsupported"

/// Reperesents a concrete transaction data that can be passed to EVM.
type TXData = {
  From : Address
  To: Address
  Value : UInt256
  Data : byte array
  Timestamp : int64
  Blocknum : int64
  // Informative fields used for debugging.
  Function : string
  OrigData : byte array
  OrigValue : bigint
}

module TXData =

  let private abiEncoder = AbiEncoder ()

  let private getSignature name typeStrs =
    abiEncoder.getSignature(name, toCsList typeStrs)

  let private encodeData signature data =
    abiEncoder.Encode(AbiEncodingStyle.None, signature, data)

  let private REDIRECT_FUNC_SPEC =
    { Name = "redirect"
      Signature = [| 0xA0uy; 0x49uy; 0x0Euy; 0xC7uy |]
      Kind = Normal
      Payable = true
      OnlyOwner = false
      // Belows are not used.
      Entry = 0UL
      ArgSpecs = [| |] }

  let makeData funcSpec args =
    if funcSpec.Kind = Fallback then Array.empty // fallback
    else let typeStrs = Array.map (fun arg -> arg.ArgSpec.TypeStr) args
         let sig1 = funcSpec.Signature
         let signature = getSignature funcSpec.Name (Array.toList typeStrs)
         let data = encodeData signature (Array.map ArgData.pack args)
         let sig2 = signature.Address
         if funcSpec.Kind = Constructor then data
         else Array.append funcSpec.Signature data

  let makeForAgent addr value data =
    let addrType = { TypeStr = "address"; Kind = ArgType.Address }
    let addrBytes = Address.toBytes LE addr
    let addrArg = { ArgSpec = addrType; Bytes = addrBytes }
    let valueType = { TypeStr = "uint256"; Kind = ArgType.UInt 32 }
    let valueBytes = bigIntToBytes LE 32 value
    let valueArg = { ArgSpec = valueType; Bytes = valueBytes }
    let bytesType = { TypeStr = "bytes"; Kind = Array (UnfixedSize, Byte) }
    let bytesArg = { ArgSpec = bytesType; Bytes = data }
    let args = [| addrArg; valueArg; bytesArg |]
    makeData REDIRECT_FUNC_SPEC args

  let toJson indent (tx: TXData) =
    indent + "{\n" +
    indent + "  " + "\"From\":\"" + Address.toStr tx.From + "\",\n" +
    indent + "  " + "\"To\":\"" + Address.toStr tx.To + "\",\n" +
    indent + "  " + "\"Value\":\"" + string tx.Value + "\",\n" +
    indent + "  " + "\"Data\":\"" + bytesToHexStr tx.Data + "\",\n" +
    indent + "  " + "\"Timestamp\":\"" + string tx.Timestamp + "\",\n" +
    indent + "  " + "\"Blocknum\":\"" + string tx.Blocknum + "\",\n" +
    indent + "  " + "\"Function\":\"" + tx.Function + "\",\n" +
    indent + "  " + "\"OrigData\":\"" + bytesToHexStr tx.OrigData + "\",\n" +
    indent + "  " + "\"OrigValue\":\"" + string tx.OrigValue + "\"\n" +
    indent + "}"

  let fromJson (json: JsonValue) =
    // Note that some tools emit timestamp/blocknumber within an invalid range.
    let toI64 s = try Int64.Parse(s) with :? OverflowException -> Int64.MaxValue
    let timestamp = match json.TryGetProperty("Timestamp") with
                    | Some s -> s.AsString() |> toI64
                    | None -> DEFAULT_TIMESTAMP
    let blocknum = match json.TryGetProperty("Blocknum") with
                   | Some s -> s.AsString() |> toI64
                   | None -> DEFAULT_BLOCKNUM
    { From = json.["From"].AsString() |> Address.fromStr
      To = json.["To"].AsString() |> Address.fromStr
      Value = UInt256.Parse(json.["Value"].AsString())
      Data = json.["Data"].AsString() |> hexStrToBytes
      Timestamp = timestamp
      Blocknum = blocknum
      // Informative fields for debugging dosn't have to be filled.
      Function = ""
      OrigData = [| |]
      OrigValue = 0I }

type Entity = {
  Balance: UInt256
  Account: Address
  Agent: AgentType
}

module Entity =

  let getSender entity =
    match entity.Agent with
    | NoAgent -> entity.Account
    | SFuzzAgent addr | SmartianAgent addr -> addr

  // Currently TX redirection occurs only when we send a TX to Smartian's agent.
  let isTXRedirected ``to`` entity =
    match entity.Agent with
    | NoAgent | SFuzzAgent _ -> false
    | SmartianAgent addr -> addr = ``to``

  let toJson indent (entity: Entity) =
    let agentStr, contractStr =
      match entity.Agent with
      | NoAgent -> "NoAgent", ""
      | SmartianAgent addr -> "SmartianAgent", Address.toStr addr
      | SFuzzAgent addr -> "SFuzzAgent", Address.toStr addr
    indent + "{\n" +
    indent + "  " + "\"Balance\":\"" + string entity.Balance + "\",\n" +
    indent + "  " + "\"Account\":\"" + Address.toStr entity.Account + "\",\n" +
    indent + "  " + "\"Agent\":\"" + agentStr + "\",\n" +
    indent + "  " + "\"Contract\":\"" + contractStr + "\"\n" +
    indent + "}"

  let fromJson (json: JsonValue) =
    let balance = UInt256.Parse(json.["Balance"].AsString())
    let account = json.["Account"].AsString() |> Address.fromStr
    let agentType =
      match json.["Agent"].AsString().ToLower() with
      | "noagent" -> NoAgent
      | "smartianagent" ->
        json.["Contract"].AsString() |> Address.fromStr |> SmartianAgent
      | "sfuzzagent" ->
        json.["Contract"].AsString() |> Address.fromStr |> SFuzzAgent
      | s -> failwithf "Unsupported agent type string: %s" s
    { Balance = balance
      Account = account
      Agent = agentType }

/// Represents a concrete test case composed of transaction data.
type TestCase = {
  Entities: Entity list
  TargetDeployer: Address
  TargetContract: Address
  DeployTx: TXData
  Txs: TXData list
}

module TestCase =

  let toJson (tc: TestCase) =
    "{\n" +
    "  " + "\"Entities\":\n" +
    "    [\n" +
    String.concat ",\n" (List.map (Entity.toJson "      ") tc.Entities) +
    "\n    ],\n" +
    "  " + "\"TargetDeployer\":\"" + Address.toStr tc.TargetDeployer + "\",\n" +
    "  " + "\"TargetContract\":\"" + Address.toStr tc.TargetContract + "\",\n" +
    "  " + "\"DeployTx\":\n" + TXData.toJson "    " tc.DeployTx + ",\n" +
    "  " + "\"Txs\":\n" +
    "    [\n" +
    String.concat ",\n" (List.map (TXData.toJson "      ") tc.Txs) +
    "\n    ]\n" +
    "}\n"

  let fromJson tcStr =
    let json = JsonValue.Parse(tcStr)
    { Entities = [ for v in json.["Entities"] -> Entity.fromJson v ]
      TargetDeployer = json.["TargetDeployer"].AsString() |> Address.fromStr
      TargetContract = json.["TargetContract"].AsString() |> Address.fromStr
      DeployTx = TXData.fromJson json.["DeployTx"]
      Txs = [ for v in json.["Txs"] -> TXData.fromJson v ] }
