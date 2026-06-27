namespace Diagnostics.ProofRelated

open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open TestFplInterpreter.Helpers.Common


(* PR009
   Purpose: Warn that not all arguments of a proof could be verified.
   What it indicates: During proof checking some proof arguments failed verification (their justifications did not validate or could not be resolved), so the proof contains unverified arguments.
   Use: Pinpoint proof steps with missing/invalid justifications or argument verification failures so the author can correct or complete them.
   Action / Treat: Inspect the problematic arguments and their justifications (references, inferred expressions, preceding steps); fix incorrect references, supply missing justification items, or correct inference errors. PR009 is emitted as a warning to inform the author that verification is incomplete. *)

[<TestClass>]
type TestPR00() =

    [<DataRow("01", """thm T { true } proof T$1 {1: trivial}""", 0)>]
    [<DataRow("02a", """thm T { true } proof T$1 {1: true}""", 0)>]
    [<DataRow("02b", """thm T { true } proof T$1 {1. 2 |- false}""", 1)>]
    [<DataRow("02c", """thm T { true } proof T$1 {1. B |- true}""", 1)>]
    [<DataRow("02d", """thm T { true } proof T$1 {1. bydef S |- false}""", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestPR009(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = PR009 
            runTestHelper "TestPR009.fpl" fplCode code expected
