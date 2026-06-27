namespace Diagnostics.IdentifierRelated

open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open TestFplInterpreter.Helpers.Common

(* ID003
   Purpose: Signal that a proof signature lacks an associated theorem-like statement block.
   What it indicates: A proof identifier was encountered for which no corresponding theorem-like statement could attached during embedding/processing.
   Use: Helps locate proofs that lack an actual theorem-like statement to be associated with so authors can correct the proof signature or, less likely, add the missing theorem-like statement.
   Action / Treat: Change the proof declaration or add the missing theorem-like statement of the proof; treat ID003 as an error that prevents successful verification of the affected statement. *)

[<TestClass>]
type TestID003() =

    [<DataRow("theorem Test {true} proof Test$1 {1: trivial}", 0)>]
    [<DataRow("theorem TestTypo {true} proof Test$1 {1: trivial}", 1)>]
    [<DataRow("corollary Test$1 {true} proof Test$1$1 {1: trivial}", 0)>]
    [<DataRow("theorem Test {true} corollary Test$1 {true} proof Test$1$1 {1: trivial}", 0)>]
    [<DataRow("uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestID003(fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID003 ""
            runTestHelper "TestID003.fpl" fplCode code expected
