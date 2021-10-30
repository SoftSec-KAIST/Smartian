module EVMAnalysis.Parse

open System.IO
open FSharp.Data
open FSharp.Data.JsonExtensions
open B2R2
open B2R2.FrontEnd.BinFile
open B2R2.FrontEnd.BinInterface
open B2R2.MiddleEnd.BinEssence
open B2R2.MiddleEnd.Reclaimer
open EVMAnalysis

let rec private convertHexToBin accBinBytes hexChars =
  match hexChars with
  | [] -> List.rev accBinBytes |> Array.ofList
  | [ _ ] -> failwith "Odd-length byte code provided as input"
  | ch1 :: ch2 :: tailHexChars ->
    let binByte = ("0x" + System.String [|ch1; ch2|]) |> int |> byte
    convertHexToBin (binByte :: accBinBytes) tailHexChars

let private addCFG ess accCFGs entry =
  match CFG.tryBuild ess entry with
  | None -> accCFGs
  | Some cfg -> Map.add entry cfg accCFGs

let private uint32ToBytes (u32: uint32) =
  [| byte ((u32 >>> 24) &&& 0xffu);
     byte ((u32 >>> 16) &&& 0xffu);
     byte ((u32 >>> 8) &&& 0xffu);
     byte ((u32 >>> 0) &&& 0xffu) |]

let getFuncsFromABI ess abiFile =
  let symbols = ess.BinHandle.FileInfo.GetSymbols()
  let folder1 accMap (s: Symbol) = Map.add s.Name s.Address accMap
  let nameToAddr = Seq.fold folder1 Map.empty symbols
  let folder2 accMap (sign, name) = Map.add name (uint32ToBytes sign) accMap
  let nameToSig = Seq.fold folder2 Map.empty (Dictionary.toSeq ess.SigToName)
  let abiStr = System.IO.File.ReadAllText(abiFile)
  let abiJson = JsonValue.Parse(abiStr)
  let fJsons = [ for v in abiJson -> v ]
  let constructor = ABI.parseConstructor fJsons
  let normalFuncs = List.choose (ABI.tryParseFunc nameToAddr nameToSig) fJsons
  (constructor, normalFuncs)

let getFuncsFromBin ess =
  let constructor = FuncSpec.DEFAULT_CONSTURCTOR
  let fallback = FuncSpec.DEFAULT_FALLBACK
  let mapper (sign, addr) = FuncSpec.initDummy (uint32ToBytes sign) Normal addr
  let funcs = Dictionary.toSeq ess.SigToAddr |> Seq.map mapper |> Seq.toList
  (constructor, fallback :: funcs)

let getFunctions ess abiFile =
  if abiFile <> "" then getFuncsFromABI ess abiFile
  else getFuncsFromBin ess

let run binFile abiFile =
  let bytes = File.ReadAllText(binFile) |> Seq.toList |> convertHexToBin []
  let hdl = BinHandle.Init(ISA.OfString "evm", bytes)
  // First, construct the CFG of bytecode before the deployment.
  let ess = BinEssence.init hdl
  let preCFGs = Set.fold (addCFG ess) Map.empty ess.CalleeMap.Entries
  // Next, construct the CFG of bytecode after the deployment.
  let passes = [ EVMCodeCopyAnalysis () :> IAnalysis
                 EVMTrampolineAnalysis(abiFile) :> IAnalysis ]
  let ess = Reclaimer.run passes ess
  let postCFGs = Set.fold (addCFG ess) Map.empty ess.CalleeMap.Entries
  // Lastly, obtain the function list to analyze.
  let constructor, normalFuncs = getFunctions ess abiFile
  (preCFGs, postCFGs, constructor, normalFuncs)
