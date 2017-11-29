module Benchmark.Program

open System.IO
open System.Diagnostics

let watch parserName parse format input =
  printfn "Parser: %s" parserName
  let watch = Stopwatch.StartNew()
  let result = parse input
  watch.Stop()
  let result = format result
  let time = watch.Elapsed
  printfn "result: %s\ntime: %A\n" result time

let benchmark input =
  watch "FParsec" FParsec.Json.parseJsonString FParsec.Json.formatResult input
  watch "ParsecClone" ParsecClone.Json.parseJsonString ParsecClone.Json.formatResult input
  watch "FsAttoparsec" Attoparsec.Json.parseJsonString Attoparsec.Json.formatResult input

let read (filePath: string) =
  use reader = new StreamReader(filePath)
  reader.ReadToEnd()

[<EntryPoint>]
let main argv = 
  if Array.length argv <> 1 then
    printfn "require arg: json file path"
  else
    benchmark (read argv.[0])
  0