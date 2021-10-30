namespace Smartian

open System
open Utils
open BytesUtils
open Linear

type SimpleLinearInequality = {
  Endian      : Endian
  ChunkSize   : int
  Linearity   : Linearity
  SplitPoints : (bigint * bigint) list
}

module SimpleLinearInequality =
  let toString (linIneq : SimpleLinearInequality) =
    let linearity = linIneq.Linearity
    let splits = linIneq.SplitPoints
    Printf.sprintf "%s (split=%A)" (Linear.toString linearity) splits

type LinearInequality = {
  TightInequality : LinearEquation option
  LooseInequality : SimpleLinearInequality option
  Sign            : Signedness
}

module LinearInequality =

  type Result =
    | NonLinear // No linear change observed
    | Unsolvable // Linearity found but unsolvable (wrong endianess/size)
    | Solvable of SimpleLinearInequality // Linearity found and solvable

  /// Solve equation (targetY-y0) = slope * (x-x0)
  let rec private solveAux slope x0 y0 sign targetY =
    let candidate = x0 + (targetY - y0) * slope.Denominator / slope.Numerator
    let checkY = y0 + (candidate - x0) * slope.Numerator / slope.Denominator
    if targetY = checkY then
      Some (candidate - 1I, candidate + 1I)
    elif checkY > targetY && slope.Numerator > 0I then
      Some (candidate - 1I, candidate)
    elif checkY > targetY && slope.Numerator < 0I then
      Some (candidate, candidate + 1I)
    elif checkY < targetY && slope.Numerator > 0I then
      Some (candidate, candidate + 1I)
    elif checkY < targetY && slope.Numerator < 0I then
      Some (candidate - 1I, candidate)
    else None

  /// Solve linear constraint. We should consider the wrap-around due to
  /// overflow/underflow.
  let private solve slope x0 y0 targetY chunkSize cmpSize sign =
    let targetYs =
      match sign with
      | Signed ->
        let signedWrap = getSignedMax cmpSize + 1I
        [-signedWrap; targetY; signedWrap]
      | Unsigned ->
        let unsignedWrap = getUnsignedMax cmpSize + 1I
        [0I; targetY; unsignedWrap]
    List.choose (solveAux slope x0 y0 sign) targetYs
    |> List.distinct
    |> List.filter
         (fun (low, high) -> 0I <= low && high <= getUnsignedMax chunkSize)

  /// Try to generate linear constraint with given 'slope', that passes the
  /// point (x, y). If no solution is found, then return Unsolvable.
  let private generate endian chunkSize cmpSize slope targetY x0 y0 sign =
    let sols = solve slope x0 y0 targetY chunkSize cmpSize sign
    if List.isEmpty sols then Unsolvable else
      Solvable
        { Linearity = Linear.generate slope x0 y0 targetY
          Endian = endian; ChunkSize = chunkSize; SplitPoints = sols
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
    let sign = if brInfo1.BrType = SignedSize then Signed else Unsigned
    if Array.length ctx.Bytes < chunkSize - 1 then failwith "Invalid size"
    let x1 = bytesToUnsignedBigInt endian (concatBytes chunkSize brInfo1 ctx)
    let x2 = bytesToUnsignedBigInt endian (concatBytes chunkSize brInfo2 ctx)
    let x3 = bytesToUnsignedBigInt endian (concatBytes chunkSize brInfo3 ctx)
    if brInfo1.Oprnd1 = brInfo2.Oprnd1 && brInfo2.Oprnd1 = brInfo3.Oprnd1 then
      // *.Oprnd1 is constant, so infer linearity between *.Oprnd2
      let y1 = BranchInfo.interpretAs sign cmpSize brInfo1.Oprnd2
      let y2 = BranchInfo.interpretAs sign cmpSize brInfo2.Oprnd2
      let y3 = BranchInfo.interpretAs sign cmpSize brInfo3.Oprnd2
      let slope = Linear.findCommonSlope cmpSize x1 x2 x3 y1 y2 y3
      if slope.Numerator = 0I then NonLinear else
        let targetY = brInfo1.Oprnd1
        generate endian chunkSize cmpSize slope targetY x1 y1 sign
    elif brInfo1.Oprnd2 = brInfo2.Oprnd2 && brInfo2.Oprnd2 = brInfo3.Oprnd2 then
      // *.Oprnd2 is constant, so infer linearity between *.Oprnd1
      let y1 = BranchInfo.interpretAs sign cmpSize brInfo1.Oprnd1
      let y2 = BranchInfo.interpretAs sign cmpSize brInfo2.Oprnd1
      let y3 = BranchInfo.interpretAs sign cmpSize brInfo3.Oprnd1
      let slope = Linear.findCommonSlope cmpSize x1 x2 x3 y1 y2 y3
      if slope.Numerator = 0I then NonLinear else
        let targetY = brInfo1.Oprnd2
        generate endian chunkSize cmpSize slope targetY x1 y1 sign
    else NonLinear // Let's consider this as non-linear for now.

  let rec private findAux ctx types branchInfoTriple =
    match types with
    | [] -> None
    | (endian, chunkSize) :: tailTypes ->
      // If linearity is not found in small chunk size, don't have to continue.
      match findAsNByteChunk ctx endian chunkSize branchInfoTriple with
      | NonLinear -> None
      | Unsolvable -> findAux ctx tailTypes branchInfoTriple
      | Solvable linearity -> Some linearity

  let private findLoose ctx branchInfoTriple =
    // Try to interpret the branch information in the following order
    let types = [(BE, 1); (BE, 2); (LE, 2); (BE, 4); (LE, 4); (BE, 8); (LE, 8)]
    // Filter out invalid chunk size
    let maxLen = Array.length ctx.Bytes + 1
    let types = List.filter (fun (endian, size) -> size <= maxLen) types
    findAux ctx types branchInfoTriple

  let find ctx brInfoTriple =
    if Config.Debug then
      printfn "Called LinearInequality.find()"
    match LinearEquation.find ctx brInfoTriple, findLoose ctx brInfoTriple with
    | None, None -> None
    | tightIneqOpt, looseIneqOpt ->
      let brInfo, _, _ = brInfoTriple
      let sign = if brInfo.BrType = SignedSize then Signed else Unsigned
      if Config.Debug then
        printfn "Found ineq: tight = %A loose = %A" tightIneqOpt looseIneqOpt
      Some { TightInequality = tightIneqOpt
             LooseInequality = looseIneqOpt
             Sign = sign }

  let toString ineq =
    let tightIneq = ineq.TightInequality
    let looseIneq = ineq.LooseInequality
    match tightIneq, looseIneq with
    | None, None -> failwith "unreachable"
    | Some tightIneq, None ->
      Printf.sprintf "(Tight) %s" (LinearEquation.toString tightIneq)
    | None, Some looseIneq ->
      Printf.sprintf "(Loose) %s" (SimpleLinearInequality.toString looseIneq)
    | Some tightIneq, Some looseIneq ->
      Printf.sprintf "(Tight) %s\n(Loose) %s"
        (LinearEquation.toString tightIneq)
        (SimpleLinearInequality.toString looseIneq)

