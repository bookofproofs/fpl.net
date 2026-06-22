namespace Diagnostics.ProofRelated

open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open TestFplInterpreter.Helpers.Common

(* PR010
   Purpose: Enforce that a justification keyword references the correct kind of entity.
   What it indicates: A justification (for example `byax`, `bydef`, `byconj`, `byinf`, etc.) was given a reference that points to a proof or a corollary when the justification form expects a reference to a different kind of block (e.g., an axiom, definition, conjecture, theorem-like statement, or an argument reference).
   Message: `Justification `{keyword}` expects a reference to {expectedRef}, not to a proof or corollary.`
   Use: Pinpoint justification sites that reference a proof or corollary incorrectly so the author can change the target to the expected block kind.
   Action / Treat: Use the appropriate reference (point to the theorem/axiom/definition/etc.) or select a justification form that accepts proof/corollary references. Treat PR010 as an error that must be corrected for the justification to be valid. *)

[<TestClass>]
type TestPR010() =

    [<DataRow("00", "proof T$1 {1. byax A |- trivial}", 0)>]
    [<DataRow("01", "proof T$1 {1. byax A$1 |- trivial}", 1)>]
    [<DataRow("00", "proof T$1 {1. byconj A |- trivial}", 0)>]
    [<DataRow("01", "proof T$1 {1. byconj A$1 |- trivial}", 1)>]
    [<DataRow("04", "proof T$1 {1. bydef A |- trivial}", 0)>]
    [<DataRow("05", "proof T$1 {1. bydef A$1 |- trivial}", 1)>]
    [<DataRow("02", "proof T$1 {1. byinf A |- trivial}", 0)>]
    [<DataRow("03", "proof T$1 {1. byinf A$1 |- trivial}", 1)>]
    [<DataRow("1f", "cor A$1 {true} thm T {true} proof T$1 {1. bydef A$1 |- trivial }", 1)>]
    [<DataRow("1f_", "thm A {true} cor A$1 {true} thm T {true} proof T$1 {1. bydef A$1 |- trivial }", 1)>]
    [<DataRow("1g_", "thm A {true} proof A$1 {1: trivial } thm T {true} proof T$1 {1. bydef A$1 |- trivial }", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestPR010(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = PR010 ("", "")
            runTestHelper "TestPR010.fpl" fplCode code expected
