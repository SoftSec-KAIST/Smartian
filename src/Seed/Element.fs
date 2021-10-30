namespace Smartian

open EVMAnalysis
open Config
open Utils
open BytesUtils

/// A building block of program input, composed of ByteVal array.
type Element = {
  /// Type
  ElemType : ArgType
  /// An array of ByteVal elements.
  ByteVals : ByteVal array
  /// Maximum lenght allowed.
  MaxLength : int
  /// Specifies the offset within the element (i.e. the index of 'ByteVals'),
  /// which will be used for the next grey-box concolic testing.
  ByteCursor : int
}

module Element =

  let private decideLen = function
    | UInt width | Int width -> width
    | Address -> 20
    | Bool | Byte -> 1
    | String -> STRING_ELEM_LEN
    | Array _ -> failwithf "Array type not allowed for an element"

  /// Initialize an element with the given specification.
  let init argKind =
    let len = decideLen argKind
    let bytes = Array.init len (fun _ -> 0uy)
    let byteVals = Array.map ByteVal.newByteVal bytes
    { ElemType = argKind
      ByteVals = byteVals
      MaxLength = len
      ByteCursor = 0 }

  let DEPLOYER_ADDR =
    let bytes = Address.toBytes LE Address.OWNER_CONTRACT
    let byteVals = Array.map ByteVal.newByteVal bytes
    { ElemType = Address
      ByteVals = byteVals
      MaxLength = 20
      ByteCursor = 0 }

  let ZERO =
    let byteVals = bigIntToBytes LE 32 0I |> Array.map ByteVal.newByteVal
    { ElemType = UInt 32
      ByteVals = byteVals
      MaxLength = 32
      ByteCursor = 0 }

  /// Return a deep-copied element of a given element.
  let copy elem =
    { elem with ByteVals = Array.copy elem.ByteVals }

  /// Concretize an element into a concrete byte array.
  let concretize elem =
    Array.map ByteVal.getConcreteByte elem.ByteVals

  /// Get current byte value pointed by element's cursor.
  let getCurByteVal elem = elem.ByteVals.[elem.ByteCursor]

  let getByteValAt elem pos =
    elem.ByteVals.[pos]

  /// Get the length of bytevals
  let getByteLen elem = elem.ByteVals.Length

  /// Check if the given element has any unfixed ByteVal.
  let hasUnfixedByte elem = Array.exists ByteVal.isUnfixed elem.ByteVals

  /// Set the byte cursor position of the given element.
  let setCursor elem newPos =
    { elem with ByteCursor = newPos }

  let rewindByteCursor elem =
    setCursor elem 0

  /// Step the byte cursor, following the cursor direction.
  let stepCursor elem =
    let byteCursor = elem.ByteCursor
    if byteCursor + 1 >= elem.ByteVals.Length then None
    else Some (setCursor elem (byteCursor + 1))

  /// Update the ByteVal at the given offset.
  let updateByteAt elem pos byte =
    let curByteVals = elem.ByteVals
    let newByteVals = Array.copy curByteVals
    newByteVals.[pos] <- (Undecided byte)
    { elem with ByteVals = newByteVals }

  /// Update the bytes of current input, starting from the given offset.
  /// Approximate path conditions of the updated ByteVals are abandoned.
  let updateBytesFrom elem pos bytes =
    let curByteVals = elem.ByteVals
    let newByteVals = Array.copy curByteVals
    Array.iteri (fun i b -> newByteVals.[pos + i] <- Undecided b) bytes
    { elem with ByteVals = newByteVals }

  /// Flip the bit at the given byte/bit offset of current input.
  let flipBitAt elem bytePos bitPos =
    let curByteVals = elem.ByteVals
    let newByteVals = Array.copy curByteVals
    let curByteVal = curByteVals.[bytePos]
    let curByte = ByteVal.getConcreteByte curByteVal
    let newByte = curByte ^^^ ((byte 1) <<< bitPos)
    newByteVals.[bytePos] <- Undecided newByte
    { elem with ByteVals = newByteVals }

  /// Stringfy an element.
  let toString elem =
    let bytes = Array.map ByteVal.getConcreteByte elem.ByteVals
    // We concretize integer and address in a little-endian style, so stringfy
    // them in the same way for the readability.
    match elem.ElemType with
    | UInt _ | Int _ | Address -> bytesToHexStr (Array.rev bytes)
    | Bool | Byte | String -> bytesToHexStr bytes
    | Array _ -> failwithf "Array type not allowed for an element"
