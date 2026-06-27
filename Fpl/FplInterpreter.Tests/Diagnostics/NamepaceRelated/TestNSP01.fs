namespace Diagnostics.NamespaceRelated

open System.IO
open FParsec
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open TestFplInterpreter.Helpers.Common
open Fpl.Interpreter.SymbolTable.Storage.Asts
open Fpl.Interpreter.SymbolTable.Creation.UsesClauses

(* NSP01
   Purpose: Report failure to load a local theory file referenced by a `uses` clause.
   What it indicates: The interpreter found a candidate source but could not read or parse the local file (I/O error, permission, corrupt file, or unexpected format).
   Use: Emitted during `uses` processing to pinpoint local file load problems so the author or environment can correct access or file integrity.
   Action / Treat: Verify the file path and permissions, check the file contents/encoding, ensure the file is not locked or corrupted, and retry; treat NSP01 as an error that prevents loading the referenced theory. *)

[<TestClass>]
type TestNSP01() =

    [<TestInitialize>]
    member this.Initialize () =
        // ensure no diagnostics from previous tests remain
        ad.Clear()
    
    [<DataRow("01", "nonexistent.filename.txt", 1)>]
    [<TestMethod>]
    member this.TestNSP01(no:string, filename:string, expected:int) =
        let code = NSP01 ("", "")
        let pos = Position("", 0, 1, 1)
        let evalAlias = {
                EvalAlias.StartPos = pos
                EvalAlias.EndPos = pos
                EvalAlias.AliasOrStar = ""
            }

        let eval = { 
                EvalAliasedNamespaceIdentifier.StartPos = pos
                EvalAliasedNamespaceIdentifier.EndPos = pos
                EvalAliasedNamespaceIdentifier.EvalAlias = evalAlias
                EvalAliasedNamespaceIdentifier.PascalCaseIdList = []
                EvalAliasedNamespaceIdentifier.DebugMode = false
            }

        loadFile filename (eval:EvalAliasedNamespaceIdentifier) |> ignore


        let result = filterByErrorCode ad code.Code
        Assert.AreEqual<int>(expected, result.Length) 

    [<DataRow("01", "filename.txt", 0)>]
    [<TestMethod>]
    member this.TestNSP01ExistingFile(no:string, filename:string, expected:int) =
        let code = NSP01 ("", "")
        let pos = Position("", 0, 1, 1)
        let evalAlias = {
                EvalAlias.StartPos = pos
                EvalAlias.EndPos = pos
                EvalAlias.AliasOrStar = ""
            }

        let eval = { 
                EvalAliasedNamespaceIdentifier.StartPos = pos
                EvalAliasedNamespaceIdentifier.EndPos = pos
                EvalAliasedNamespaceIdentifier.EvalAlias = evalAlias
                EvalAliasedNamespaceIdentifier.PascalCaseIdList = []
                EvalAliasedNamespaceIdentifier.DebugMode = false
            }
        let currDir = Directory.GetCurrentDirectory()
        let path = Path.Combine(currDir, filename)
        File.WriteAllText(path, "")
        // use absolute path to avoid flaky resolution of relative paths during test runs
        loadFile path (eval:EvalAliasedNamespaceIdentifier) |> ignore

        let result = filterByErrorCode ad code.Code
        File.Delete(path)
        Assert.AreEqual<int>(expected, result.Length) 
