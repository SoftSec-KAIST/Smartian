namespace Smartian

open System
open Utils
open BytesUtils
open Linear

type LinearEquation = {
  Endian     : Endian
  ChunkSize  : int
  Linearity  : Linearity
  Solutions  : bigint list
}

module LinearEquation =

  type Result =
    | NonLinear // No linear change observed
    | Unsolvable // Linearity found but unsolvable (wrong endianess/size)
    | Solvable of LinearEquation // Linearity found and solvable

  /// Solve equation (targetY-y0) = slope * (x-x0)
  let rec private solveAux slope x0 y0 targetY =
    let candidate = x0 + (targetY - y0) * slope.Denominator / slope.Numerator
    if targetY - y0 = (candidate - x0) * slope.Numerator / slope.Denominator
    then Some candidate
    else None

  /// Solve linear constraint. We should consider the wrap-around due to
  /// overflow/underflow.
  let private solve slope x0 y0 targetY chunkSize cmpSize =
    let unsignedWrap = getUnsignedMax cmpSize + 1I
    let targetYs = [targetY; targetY + unsignedWrap; targetY - unsignedWrap]
    List.choose (solveAux slope x0 y0) targetYs
    |> List.distinct
    |> List.filter (fun sol -> 0I <= sol && sol <= getUnsignedMax chunkSize)

  /// Try to generate linear constraint with given 'slope', that passes the
  /// point (x, y). If no solution is found, then return Unsolvable.
  let private generate endian chunkSize cmpSize slope targetY x0 y0 =
    let sols = solve slope x0 y0 targetY chunkSize cmpSize
    if List.isEmpty sols then Unsolvable else
      Solvable
        { Linearity = Linear.generate slope x0 y0 targetY
          Endian = endian; ChunkSize = chunkSize; Solutions = sols
        }

  // TODO : Optimize by reversing the Byte array when constructing ctx.
  let private concatBytes chunkSize brInfo ctx =
    let tryByte = byte brInfo.TryVal
    match ctx.ByteDir with
    | Stay -> failwith "Byte cursor cannot be staying"
    | Left -> (* Cursor was moving left, and tryByte is rightmost byte. *)
      let len = Array.length ctx.Bytes
      let bytes = ctx.Bytes.[(len - chunkSize + 1) .. (len - 1)]
      Array.append bytes [| tryByte |]
    | Right -> (* Cursor was moving right, and tryByte is leftmost byte. *)
      let bytes = ctx.Bytes.[0 .. (chunkSize - 2)]
      Array.append [| tryByte |] bytes

  let private findAsNByteChunk ctx endian chunkSize (brInfo1, brInfo2, brInfo3) =
    (* The size of comparison operation (determined by cmpb, cmpw, cmpl..) may
     * not always match with the size of input field.
     *)
    let cmpSize = brInfo1.OpSize
    if Array.length ctx.Bytes < chunkSize - 1 then failwith "Invalid size"
    let x1 = bytesToUnsignedBigInt endian (concatBytes chunkSize brInfo1 ctx)
    let x2 = bytesToUnsignedBigInt endian (concatBytes chunkSize brInfo2 ctx)
    let x3 = bytesToUnsignedBigInt endian (concatBytes chunkSize brInfo3 ctx)
    if brInfo1.Oprnd1 = brInfo2.Oprnd1 && brInfo2.Oprnd1 = brInfo3.Oprnd1 then
      // *.Oprnd1 is constant, so infer linearity between *.Oprnd2
      let y1 = brInfo1.Oprnd2
      let y2 = brInfo2.Oprnd2
      let y3 = brInfo3.Oprnd2
      let slope = Linear.findCommonSlope cmpSize x1 x2 x3 y1 y2 y3
      if slope.Numerator = 0I then NonLinear else
        let targetY = brInfo1.Oprnd1
        generate endian chunkSize cmpSize slope targetY x1 y1
    elif brInfo1.Oprnd2 = brInfo2.Oprnd2 && brInfo2.Oprnd2 = brInfo3.Oprnd2 then
      // *.Oprnd2 is constant, so infer linearity between *.Oprnd1
      let y1 = brInfo1.Oprnd1
      let y2 = brInfo2.Oprnd1
      let y3 = brInfo3.Oprnd1
      let slope = Linear.findCommonSlope cmpSize x1 x2 x3 y1 y2 y3
      if slope.Numerator = 0I then NonLinear else
        let targetY = brInfo1.Oprnd2
        generate endian chunkSize cmpSize slope targetY x1 y1
    else NonLinear // Let's consider this as non-linear for now.

  let rec private findAux ctx types brInfoTriple =
    match types with
    | [] -> None
    | (endian, chunkSize) :: tailTypes ->
      // If linearity is not found in small chunk size, don't have to continue.
      match findAsNByteChunk ctx endian chunkSize brInfoTriple with
      | NonLinear -> None
      | Unsolvable -> findAux ctx tailTypes brInfoTriple
      | Solvable linearity ->
        if Config.Debug then
          log "Found equation: %A" linearity
        Some linearity

  let find ctx brInfoTriple =
    // Try to interpret the branch traces in the following order
    let types = [(LE, 1); (LE, 2); (LE, 4); (LE, 8); (LE, 16); (LE, 32)]
    // Filter out invalid chunk size
    let maxLen = Array.length ctx.Bytes + 1
    let types = List.filter (fun (endian, size) -> size <= maxLen) types
    findAux ctx types brInfoTriple

  let toString { Solutions = solutions; Linearity = linearity } =
    Printf.sprintf "%s (sol=%A)" (Linear.toString linearity) solutions
