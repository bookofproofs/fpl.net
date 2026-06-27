namespace Diagnostics.ProofRelated

open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open TestFplInterpreter.Helpers.Common

(* PR004
   Purpose: Flag a justification item that duplicates an already-declared justification in the same argument scope.
   What it indicates: Two justification items with the same signature were embedded into the same argument list, producing a signature collision for justification entries.
   Use: Helps locate and remove or rename duplicate justification items so each justification in an argument is unique.
   Action / Treat: Remove the duplicate or change its signature so it does not collide with the existing justification; PR004 is an error that must be resolved to maintain unambiguous justification references. *)

[<TestClass>]
type TestPR004() =

    [<DataRow("""proof T$1 { 1: trivial qed}""", 0)>]
    [<DataRow("""proof T$1 { 1. 2, 3 |- trivial qed}""", 0)>]
    [<DataRow("""proof T$1 { 1. 1, 1 |- trivial qed}""", 1)>]
    [<DataRow("""proof T$1 { 1. 1, 1, 1 |- trivial qed}""", 2)>]
    [<DataRow("""proof T$1 { 1. 1, 2, 1 |- trivial qed}""", 1)>]
    [<DataRow("uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestPR004(fplCode:string, expected:int) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = PR004 ("", "")
            runTestHelper "TestPR004.fpl" fplCode code expected
