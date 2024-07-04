module CommonTestHelpers
open System
open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open ErrDiagnostics
open FplInterpreterTypes
open FplInterpreterDiagnosticsEmitter

let deleteFiles dir fileName =
    if Directory.Exists(dir) then
        Directory.GetFiles(dir, fileName)
        |> Array.iter File.Delete
    else
        printfn "Directory %s does not exist." dir

let deleteFilesWithExtension dir extension =
    if Directory.Exists(dir) then
        Directory.GetFiles(dir, "*." + extension)
        |> Array.iter File.Delete
    else
        printfn "Directory %s does not exist." dir

let filterByErrorCode (input: Diagnostics) errCode =
    input.Collection
    |> List.filter (fun d -> d.Code.Code = errCode)


let prepareFplCode(filename:string, fplCode:string, delete:bool) =
    FplParser.parserDiagnostics.Clear()
    let currDir = Directory.GetCurrentDirectory()

    printf "\n"
    File.WriteAllText(Path.Combine(currDir, filename), fplCode)
    let uri = System.Uri(Path.Combine(currDir, filename))
    let fplLibUrl =
        "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib"
    if delete then 
        deleteFiles currDir filename
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
        Console.WriteLine(st.AstsToString)
        Some (st)

let runTestHelper filename fplCode (code:ErrDiagnostics.DiagnosticCode) (expected:int) =
    printf "Trying %s" code.Message
    prepareFplCode(filename, fplCode, false) |> ignore
    let syntaxErrors = FplParser.parserDiagnostics.Collection |> List.filter (fun d -> d.Emitter = DiagnosticEmitter.FplParser)
    if syntaxErrors.Length > 0 then
        failwithf "Syntax errors detected."
    let result = filterByErrorCode FplParser.parserDiagnostics code.Code
    Assert.AreEqual<int>(expected, result.Length)
    prepareFplCode(filename, "", true) |> ignore
    if result.Length > 0 then 
        printf "Result %s" result.Head.Message
        result.Head.Message
    else    
        "missing error message"