﻿open FplGrammarCommons
open ErrDiagnostics
open FplParser
open FplInterpreter
open FplInterpreterTypes
open System.IO
open FParsec
open System.Text.RegularExpressions
open System.Collections.Generic

let deleteFilesWithExtension dir extension =
    if Directory.Exists(dir) then
        Directory.GetFiles(dir, "*." + extension)
        |> Array.iter File.Delete
    else
        printfn "Directory %s does not exist." dir


let prepareFplCode(fplCode:string, delete:bool) =
    let currDir = Directory.GetCurrentDirectory()

    File.WriteAllText(Path.Combine(currDir, "Test.fpl"), fplCode)
    let uri = PathEquivalentUri(Path.Combine(currDir, "Test.fpl"))
    ad.Clear()
    let fplLibUrl =
        "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib"
    if delete then 
        deleteFilesWithExtension currDir "fpl"
        None
    else
        let parsedAsts = ParsedAstList()
        let st = SymbolTable(parsedAsts, true)
        Some (FplInterpreter.fplInterpreter st fplCode uri fplLibUrl)

let loadFplFile(path:string) = 
    let uri = PathEquivalentUri(path)
    let fplLibUrl =
        "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib"
    let parsedAsts = ParsedAstList()
    let fplCode = File.ReadAllText(path)
    let st = SymbolTable(parsedAsts, false)
    FplInterpreter.fplInterpreter st fplCode uri fplLibUrl

let input = """uses Fpl.SetTheory;"""

(*
let result = fplParser input

printf "%O" result

ad.PrintDiagnostics

prepareFplCode(input,false) |> ignore
*)

loadFplFile(@"C:\Users\Peaq\source\repos\bookofproofs\fpl.net\theories\FoundationsOfAnalysisLandau\Landau.1.1.Axioms.fpl")
// loadFplFile(@"D:\Forschung\fpl.net\theories\FoundationsOfAnalysisLandau\Landau.1.1.Axioms.fpl")

printf "\n--------------------------------\n"
ad.PrintDiagnostics

