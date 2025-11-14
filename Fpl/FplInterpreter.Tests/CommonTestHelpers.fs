module CommonTestHelpers

open System
open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open ErrDiagnostics
open FplInterpreterTypes
open FplInterpreterDiagnosticsEmitterPre
open TestSharedConfig

let rec deleteDirectory path =
    if Directory.Exists(path) then
        // Delete all files in the directory
        Directory.GetFiles(path)
        |> Array.iter (fun f ->
            File.SetAttributes(f, FileAttributes.Normal)
            File.Delete f)
 
        // Recursively delete all subdirectories
        Directory.GetDirectories(path) |> Array.iter deleteDirectory

        // Finally, delete the directory itself
        Directory.Delete(path)

let isDirectoryEmpty path =
    Directory.EnumerateFileSystemEntries(path) |> Seq.isEmpty

let deleteFiles dir fileName =
    if Directory.Exists(dir) then
        Directory.GetFiles(dir, fileName) 
        |> Array.iter (fun f ->
            File.SetAttributes(f, FileAttributes.Normal)
            File.Delete f)
    else
        printfn "Directory %s does not exist." dir

let deleteFilesWithExtension dir extension =
    if Directory.Exists(dir) then
        Directory.GetFiles(dir, "*." + extension) |> Array.iter (fun f ->
            File.SetAttributes(f, FileAttributes.Normal)
            File.Delete f)
    else
        printfn "Directory %s does not exist." dir

let filterByErrorCode (input: Diagnostics) errCode =
    input.Collection |> List.filter (fun d -> d.Code.Code = errCode)

let fplCodeNeedsOnline (fplCode:string) = fplCode.Contains("uses Fpl")

let prepareFplCode (filename: string, fplCode: string, delete: bool) =
    ad.Clear()
    let currDir = Directory.GetCurrentDirectory()

    printf "\n"
    if fplCode <> "" then 
        File.WriteAllText(Path.Combine(currDir, filename), fplCode)
    let uri = PathEquivalentUri(Path.Combine(currDir, filename))

    let fplLibUrl =
        "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib"

    if delete then
        deleteFiles currDir "*.fpl"
        if not (TestConfig.OfflineMode) then
            deleteDirectory (Path.Combine(currDir,"lib"))
            deleteDirectory (Path.Combine(currDir,"repo"))
        None
    else
        let parsedAsts = ParsedAstList()
        let st = SymbolTable(parsedAsts, true, TestConfig.OfflineMode || not (fplCodeNeedsOnline fplCode))
        FplInterpreter.fplInterpreter st fplCode uri fplLibUrl |> ignore

        let syntaxErrorFound =
            ad.Collection
            |> Seq.exists (fun d -> d.Emitter = DiagnosticEmitter.FplParser)

        if syntaxErrorFound then
            if fplCode <> "" then File.AppendAllText(Path.Combine(currDir, "SyntaxErrorsLog.txt"), $"Syntax errors detected in test {filename}{Environment.NewLine}{fplCode}{Environment.NewLine}------{Environment.NewLine}")
            emitUnexpectedErrorDiagnostics "Syntax error found."

        Some(st)

let checkForUnexpectedErrors (code: ErrDiagnostics.DiagnosticCode) =
    let syntaxErrors =
        ad.Collection
        |> List.filter (fun d -> d.Emitter = DiagnosticEmitter.FplParser || d.Code.Code = "GEN00")

    if syntaxErrors.Length > 0 && code.Code <> "GEN00" then
        failwithf $"Syntax or other errors detected. {syntaxErrors.Head}"

    let contextErrors =
        ad.Collection
        |> List.filter (fun d -> d.Emitter = DiagnosticEmitter.FplInterpreter && d.Code.Code = "GEN01")

    if contextErrors.Length > 0 then
        failwithf $"Context errors detected. {contextErrors.Head}"

let runTestHelper filename fplCode (code: ErrDiagnostics.DiagnosticCode) (expected: int) =
    printf "Trying %s" code.Message
    prepareFplCode (filename, fplCode, false) |> ignore

    checkForUnexpectedErrors code

    let result = filterByErrorCode ad code.Code
    Assert.AreEqual<int>(expected, result.Length)
    prepareFplCode (filename, "", true) |> ignore

let runTestHelperWithText filename fplCode (code: ErrDiagnostics.DiagnosticCode) expected =
    printf "Trying %s" code.Message
    prepareFplCode (filename, fplCode, false) |> ignore

    let syntaxErrors =
        ad.Collection
        |> List.filter (fun d -> d.Emitter = DiagnosticEmitter.FplParser)

    if syntaxErrors.Length > 0 then
        failwithf "Syntax errors detected."

    let result = filterByErrorCode ad code.Code
    if result.Length <> expected then 
        printfn "%i errors found (%i expected)" result.Length expected
    else
        printfn "%i errors found" result.Length
    prepareFplCode (filename, "", true) |> ignore

    if result.Length > 0 then
        result.Head.Message
    else
        "missing error message"


let loadFplFile (path: string) =
    let uri = PathEquivalentUri(path)

    let fplLibUrl =
        "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib"

    let parsedAsts = ParsedAstList()
    let fplCode = File.ReadAllText(path)
    let st = SymbolTable(parsedAsts, true, TestConfig.OfflineMode)
    FplInterpreter.fplInterpreter st fplCode uri fplLibUrl
    Some(st)

let loadFplFileWithTheSameSymbolTable (st:SymbolTable) (path: string) =
    let uri = PathEquivalentUri(path)

    let fplLibUrl =
        "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib"

    let fplCode = File.ReadAllText(path)
    FplInterpreter.fplInterpreter st fplCode uri fplLibUrl
    Some(st)