namespace Diagnostics.ProofRelated

open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open TestFplInterpreter.Helpers.Common

(* PR014
   Purpose: Report that a justification refers to a more specific sub-item instead of a theorem‑like statement.
   What it indicates: A justification was given a reference that targets a subcomponent (argument index, corollary/proof argument reference, etc.) rather than the top‑level theorem/lemma/proposition/conjecture it must reference. Example: using `A:2` where `A` (the theorem) is expected.
   Use: Helps locate justification steps that reference the wrong granularity of a block so authors can point the justification to the correct theorem‑like statement.
   Action / Treat: Change the justification to reference the enclosing theorem‑like statement (e.g., `A` or `bycor A$1`) or use the appropriate justification form that accepts sub‑references. Treat PR014 as an error that must be fixed for the justification to be valid. *)

[<TestClass>]
type TestPR014() =

    [<DataRow("00", "proof T$1 {1. A |- trivial}", 0)>]
    [<DataRow("01", "proof T$1 {1. A:2 |- trivial}", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestPR014(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = PR014 
            runTestHelper "TestPR014.fpl" fplCode code expected
