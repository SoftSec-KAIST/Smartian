let usage() =
  printfn "Usage: dotnet run <bin> <abi>"
  exit 0

[<EntryPoint>]
let main argv =
  if argv.Length <> 2 then usage()
  EVMAnalysis.TopLevel.parseAndAnalyze argv.[0] argv.[1] |> ignore
  0