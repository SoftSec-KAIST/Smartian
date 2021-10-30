module Smartian.BytesUtils

open System
open Utils

/// Types and functions related to byte sequence manipulation.

type Endian = LE | BE

let allBytes = [ Byte.MinValue .. Byte.MaxValue ]

let bytesToStr (bytes: byte[]) = String (Array.map char bytes)

let strToBytes (str: string) = str.ToCharArray () |> Array.map byte

let hexStrToBytes (s: string) =
  let s = if s.StartsWith("0x") || s.StartsWith("0X") then s.[2..] else s
  if s.Length % 2 <> 0 then failwith "Odd length string as input"
  let indices = { 0 .. (s.Length / 2 - 1) } |> Seq.toArray
  let idxToByte i =
    let hexStr = s.[2 * i .. (2 * i + 1)]
    try Convert.ToInt32(hexStr, 16) |> byte with
    | _ -> failwithf "Failed to convert '%s'" hexStr
  Array.map idxToByte indices

let bytesToHexStr (bytes: byte[]) =
  bytes
  |> Array.map (fun (x : byte) -> String.Format("{0:X2}", x))
  |> String.concat String.Empty

// Auxiliary function for bytesTo*BigInt().
let rec private bytesToBigIntAux accumBigInt bytes =
  match bytes with
  | [] -> accumBigInt
  | headByte :: tailBytes ->
    let accumBigInt = (accumBigInt <<< 8) + bigint (uint32 headByte)
    bytesToBigIntAux accumBigInt tailBytes

/// Convert a byte array into a signed big BigInteger.
let bytesToSignedBigInt endian (bytes: byte[]) =
  let i = Array.toList bytes
          |> if endian = LE then List.rev else identity
          |> bytesToBigIntAux 0I
  if bytes.[0] < 128uy then i // Positive integer.
  else i - (getUnsignedMax bytes.Length + 1I) // Negative integer.

/// Convert a byte array into an unsigned BigInteger.
let bytesToUnsignedBigInt endian (bytes: byte[]) =
  Array.toList bytes
  |> if endian = LE then List.rev else identity
  |> bytesToBigIntAux 0I

// Auxiliary function for bigIntToBytes().
let rec private bigIntToBytesAux accBytes leftSize value =
  if leftSize = 0 then accBytes else
    let accBytes = (byte (value &&& bigint 0xFF)) :: accBytes
    bigIntToBytesAux accBytes (leftSize - 1) (value >>> 8)

/// Convert a big integer into a byte array using specified endianess. The
/// length of converted array is specified as 'size' argument. For example,
/// "bigIntToBytes BE 4 0x4142I" returns [| 0x00uy, 0x00uy 0x41uy, 0x42uy |]].
let bigIntToBytes endian size value =
  bigIntToBytesAux [] size value
  |> if endian = LE then List.rev else identity
  |> Array.ofList
