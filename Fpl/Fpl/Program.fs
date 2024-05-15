open FplGrammarCommons
open ErrDiagnostics
open FplParser
open FplInterpreter
open FplInterpreterTypes
open System.IO
open FParsec
open System.Text.RegularExpressions
open System.Collections.Generic

(*
let deleteFilesWithExtension dir extension =
    if Directory.Exists(dir) then
        Directory.GetFiles(dir, "*." + extension)
        |> Array.iter File.Delete
    else
        printfn "Directory %s does not exist." dir


let prepareFplCode(fplCode:string, delete:bool) =
    FplParser.parserDiagnostics.Clear()
    let currDir = Directory.GetCurrentDirectory()

    File.WriteAllText(Path.Combine(currDir, "Test.fpl"), fplCode)
    let uri = System.Uri(Path.Combine(currDir, "Test.fpl"))
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
    let uri = System.Uri(path)
    let fplLibUrl =
        "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib"
    let parsedAsts = ParsedAstList()
    let fplCode = File.ReadAllText(path)
    let st = SymbolTable(parsedAsts, false)
    FplInterpreter.fplInterpreter st fplCode uri fplLibUrl


let input = """
uses Fpl.SetTheory

def cl SomeFplClass: Set
{
    intr
}
;"""

let result = fplParser input

printf "%O" result

ad.PrintDiagnostics

// let st = prepareFplCode(@"D:\Forschung\fpl.net\theories\lib\Fpl.Commons.Structures.fpl")

let st = prepareFplCode(input,true) 


prepareFplCode("",false) |> ignore
printf "\n--------------------------------\n"


loadFplFile(@"D:\Forschung\fpl.net\theories\FoundationsOfAnalysisLandau\Landau.1.3.Ordering.fpl")

printf "\n--------------------------------\n"
ad.PrintDiagnostics
*)

let splitList (input: string list) =
    let evens = List.mapi (fun i x -> if i % 2 = 0 then Some x else None) input |> List.choose id
    let odds = List.mapi (fun i x -> if i % 2 <> 0 then Some x else None) input |> List.choose id
    (evens, odds)

let createRPN (evens: string list, odds: string list) =
    let rec helper (evens: string list) (odds: string list) (output: string list) =
        match evens, odds with
        | e :: es, o :: os -> (helper es os (output @ [o; "("; e])) @ [")"]
        | e, [] -> output @ e
        | _ -> []
    helper evens odds [] 

let precedence = Map.ofList [("+", 1); ("-", 2); ("~", 3); ("%", 1)]
let input = ["a"; "~"; "b"; "+"; "c"; "-"; "a"; "%"; "c"; "~"; "d"]
let (evens, odds) = splitList input
let sortedOdds = odds |> List.sortBy (fun op -> precedence.[op])
let output = createRPN (evens, sortedOdds)

printf "%A" output