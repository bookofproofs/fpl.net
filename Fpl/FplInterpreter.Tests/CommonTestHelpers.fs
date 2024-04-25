module CommonTestHelpers
open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open ErrDiagnostics
open FplInterpreterTypes

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

    File.WriteAllText(Path.Combine(currDir, "Test.fpl"), fplCode)
    let uri = System.Uri(Path.Combine(currDir, "Test.fpl"))
    let fplLibUrl =
        "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib"
    if delete then 
        deleteFilesWithExtension currDir "fpl"
        None
    else
        let parsedAsts = ParsedAstList()
        Some (FplInterpreter.fplInterpreter fplCode uri fplLibUrl parsedAsts false)

let runTestHelper fplCode (code:ErrDiagnostics.DiagnosticCode) expected =
    printf "Trying %s" code.Message
    prepareFplCode(fplCode, false) |> ignore
    let result = filterByErrorCode FplParser.parserDiagnostics code.Code
    if result.Length > 0 then 
        printf "Result %s" result.Head.Message
    Assert.AreEqual(expected, result.Length)
    prepareFplCode("", true) |> ignore