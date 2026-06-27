namespace Diagnostics.ProofRelated

open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open TestFplInterpreter.Helpers.Common

(* PR013
   Purpose: Recommend using `bycor` when referencing corollaries to improve readability.
   What it indicates: A proof justification referenced a corollary by its qualified identifier (e.g. `A$1`) without the explicit `bycor` keyword. While this can be parsed, the system prefers the explicit `bycor` form for clarity.
   Use: Warn authors that a corollary reference was used without the `bycor` keyword so they can make the justification more readable and self‑documenting.
   Action / Treat: Prefer `bycor A$1` instead of `A$1` in justifications that refer to corollaries. This is a readability suggestion (diagnostic) aimed at improving proof clarity. *)

[<TestClass>]
type TestPR013() =

    [<DataRow("00", "proof T$1 {1. bycor A$1 |- trivial}", 0)>]
    [<DataRow("01", "proof T$1 {1. A$1 |- trivial}", 1)>]
    [<DataRow("3k_", "thm A {true} proof A$1 {1: trivial}thm T {true} proof T$1 {1. A$1 |- trivial }", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestPR013(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = PR013 
            runTestHelper "TestPR013.fpl" fplCode code expected

