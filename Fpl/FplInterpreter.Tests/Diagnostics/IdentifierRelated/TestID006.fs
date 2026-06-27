namespace Diagnostics.IdentifierRelated

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open FplInterpreter.Main
open TestFplInterpreter.Helpers.Common
open TestSharedConfig

(* ID006
   Purpose: Report a corollary declaration that has no associated block to attach to.
   What it indicates: A corollary was encountered that could not be associated with a parent theorem‑like block (the expected enclosing block or the correct association target was not found).
   Use: Emitted during embedding/association of corollaries to point authors to corollary declarations that lack a valid parent or were placed in the wrong scope.
   Action / Treat: Ensure the corollary is declared in the correct scope and associated with an existing theorem‑like statement (or add the missing parent block); treat ID006 as an error that must be resolved so the corollary can be correctly associated and processed. *)

[<TestClass>]
type TestID006() =

    [<DataRow("theorem Test {true} corollary Test$1 {true}", 0)>]
    [<DataRow("theorem TestTypo {true} corollary Test$1 {true}", 1)>]
    [<DataRow("theorem Test {true} corollary Test$1 {true} corollary Test$1$1 {true}", 0)>]
    [<DataRow("uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestID006(fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID006 ""
            runTestHelper "TestID006.fpl" fplCode code expected
