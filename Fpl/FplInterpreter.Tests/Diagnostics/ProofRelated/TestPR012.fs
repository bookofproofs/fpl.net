namespace Diagnostics.ProofRelated

open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open TestFplInterpreter.Helpers.Common

(* PR012
   Purpose: Enforce that the `bycor` justification targets a corollary.
   What it indicates: A `bycor` justification was given a reference that is not a corollary (for example, `bycor A` instead of `bycor A$1`). The justification form `bycor` specifically expects a corollary identifier.
   Use: Pinpoint incorrect uses of `bycor` so authors can reference the correct corollary or switch to a proper justification keyword.
   Action / Treat: Change the reference to an actual corollary (e.g., `bycor A$1`) or use the appropriate justification for the referenced item. Treat PR012 as an error that must be corrected for the justification to be valid. *)

[<TestClass>]
type TestPR012() =

    [<DataRow("00", "proof T$1 {1. bycor A$1 |- trivial}", 0)>]
    [<DataRow("01", "proof T$1 {1. bycor A |- trivial}", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestPR012(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = PR012 
            runTestHelper "TestPR012.fpl" fplCode code expected
