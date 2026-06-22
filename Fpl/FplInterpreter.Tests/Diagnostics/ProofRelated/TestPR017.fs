namespace Diagnostics.ProofRelated

open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open TestFplInterpreter.Helpers.Common

(* PR017
   Purpose: Reject the use of `trivial` in a proof argument unless it is the last argument in the proof.
   What it indicates: The proof contains an argument with the `{trivial}` justification that is not positioned as the final argument; such usage is unsupported and leads to ambiguous or invalid justification sequencing.
   Use: Locate proof steps where `{trivial}` was placed earlier than the final argument so the author can reorder or remove it.
   Action / Treat: Place `{trivial}` only as the last argument for a proof or remove/replace it with an appropriate justification; treat PR017 as an error that must be fixed for the proof to be valid. *)

[<TestClass>]
type TestPR017() =

    [<DataRow("01", """thm T { true } proof T$1 {1: trivial}""", 0)>]
    [<DataRow("02", """ax A { true } thm T {true} proof T$1 {1: trivial 2. byax A |- true}""", 1)>] // trivial is not the last one in proof
    [<DataRow("03", """ax A { true } thm T {true} proof T$1 {1. byax A |- true 2: trivial}""", 0)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestPR017(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = PR017 
            runTestHelper "TestPR017.fpl" fplCode code expected
