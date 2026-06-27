namespace Diagnostics.ProofRelated

open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open TestFplInterpreter.Helpers.Common

(* PR007
   Purpose: Report theorem‑like statements that were declared without an associated proof.
   What it indicates: A theorem/lemma/proposition/corollary node was processed and no proof block was attached. Such derived statements must have a proof (unless they are intended to be axioms).
   Use: Pinpoint theorem‑like blocks missing proofs so the author can add the required `prf` block or change the declaration to an axiom.
   Action / Treat: Attach a corresponding proof (for example `prf <signature> { ... }`) or, if the statement is intended as an unproven assumption, convert it to an axiom. Treat PR007 as an error that must be resolved for the statement to be considered verified. *)

[<TestClass>]
type TestPR007() =

    [<DataRow("01", """theorem T { true } proof T$1 {1: trivial}""", 0)>]
    [<DataRow("01a", """theorem T { true }""", 1)>]
    [<DataRow("02", """proposition T { true } proof T$1 {1: trivial}""", 0)>]
    [<DataRow("02a", """proposition T { true }""", 1)>]
    [<DataRow("03", """lemma T { true } proof T$1 {1: trivial}""", 0)>]
    [<DataRow("03a", """lemma T { true }""", 1)>]
    [<DataRow("04", """corollary T$1 { true } proof T$1$1 {1: trivial}""", 0)>]
    [<DataRow("04a", """corollary T$1 { true }""", 1)>]
    [<DataRow("04b", """corollary T$1 { true } proof T$1$1 {1. bydef A |- trivial}""", 0)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestPR007(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = PR007 ("", "") 
            runTestHelper "TestPR007.fpl" fplCode code expected
