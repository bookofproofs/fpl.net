module CommonTestHelpers
open System
open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open ErrDiagnostics
open FplInterpreterTypes
open FplInterpreterDiagnosticsEmitter

let deleteFilesWithExtension dir extension =
    if Directory.Exists(dir) then
        Directory.GetFiles(dir, "*." + extension)
        |> Array.iter File.Delete
    else
        printfn "Directory %s does not exist." dir

let filterByErrorCode (input: Diagnostics) errCode =
    input.Collection
    |> List.filter (fun d -> d.Code.Code = errCode)


let prepareFplCode(fplCode:string, delete:bool) =
    FplParser.parserDiagnostics.Clear()
    let currDir = Directory.GetCurrentDirectory()

    printf "\n"
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
        FplInterpreter.fplInterpreter st fplCode uri fplLibUrl |> ignore
        let syntaxErrorFound = 
            FplParser.parserDiagnostics.Collection
            |> Seq.exists(fun d -> d.Emitter = DiagnosticEmitter.FplParser)
        if syntaxErrorFound then 
            emitUnexpectedErrorDiagnostics (uri.AbsolutePath) ("Syntax error found.")
        Console.WriteLine(st.LoggedState)
        Some (st)

let runTestHelper fplCode (code:ErrDiagnostics.DiagnosticCode) expected =
    printf "Trying %s" code.Message
    prepareFplCode(fplCode, false) |> ignore
    let result = filterByErrorCode FplParser.parserDiagnostics code.Code
    if result.Length > 0 then 
        printf "Result %s" result.Head.Message
    Assert.AreEqual(expected, result.Length)
    prepareFplCode("", true) |> ignore