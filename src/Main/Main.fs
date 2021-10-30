module Smartian.Main

open Utils

let printUsage () =
  printLine "Usage: 'dotnet Smartian.dll <fuzz|replay> <options...>'"
  printLine "fuzz : Mode for test case generation with fuzzing."
  printLine "       Use 'dotnet Smartian.dll fuzz --help' for details."
  printLine "replay : Mode for replaying generated test cases."
  printLine "         Use 'dotnet Smartian.dll replay --help' for details."

let runMode (mode: string) optArgs =
  match mode.ToLower() with
  | "fuzz" -> Fuzz.run optArgs
  | "replay" -> Replay.run optArgs
  | _ -> printUsage ()

[<EntryPoint>]
let main args =
  if Array.length args <= 1
  then printUsage (); 1
  else runMode args.[0] args.[1..]; 0
