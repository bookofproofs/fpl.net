namespace Diagnostics.NamespaceRelated

open FParsec
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open TestFplInterpreter.Helpers.Common
open Fpl.Interpreter.SymbolTable.Storage.Asts
open Fpl.Interpreter.SymbolTable.Creation.UsesClauses


(* NSP02
   Purpose: Report that a referenced theory was found but could not be downloaded.
   What it indicates: The interpreter located a candidate source for the requested theory but the acquisition step failed (network, access, or remote-format issues).
   Use: Emitted during `uses`-clause resolution to surface fetch failures that prevent loading a dependency.
   Action / Treat: Verify the theory URL/source, network/access permissions and repository configuration, or provide a reachable source; NSP02 is an error that must be resolved to successfully load the referenced theory. *)

[<TestClass>]
type TestNSP02() =

    [<TestInitialize>]
    member this.Initialize () =
        // ensure no diagnostics from previous tests remain
        ad.Clear()
    
    [<DataRow("01", "https://google.com", 0)>]
    [<DataRow("02", "https://nonexistent.invalid", 1)>]
    [<TestMethod>]
    member this.TestNSP02(no:string, url:string, expected:int) =
        let code = NSP02 ("", "")
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

        downloadFile url (eval:EvalAliasedNamespaceIdentifier) |> ignore


        let result = filterByErrorCode ad code.Code
        Assert.AreEqual<int>(expected, result.Length) 
