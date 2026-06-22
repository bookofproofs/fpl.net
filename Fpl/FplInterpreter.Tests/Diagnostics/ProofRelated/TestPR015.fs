namespace Diagnostics.ProofRelated

open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open TestFplInterpreter.Helpers.Common

(* PR015
   Purpose: Report an invalid revocation reference inside a proof.
   What it indicates: A `revoke` step referenced an argument identifier that was not previously assumed in the current proof (or was already revoked). The emitted message is: `Cannot revoke the argument `{argId}` because it wasn't assumed.` 
   Use: Helps locate incorrect `revoke` uses so the proof author can ensure revocations follow actual prior `assume` steps.
   Action / Treat: Ensure the argument was assumed earlier and not already revoked, or correct the referenced identifier; PR015 is an error that must be fixed for the proof to be valid. *)

[<TestClass>]
type TestPR015() =

    [<DataRow("00", "proof T$1 {1: assume x 2: revoke 1}", 0)>]
    [<DataRow("01", "proof T$1 {1: assume x 2: revoke 2}", 1)>] 
    [<DataRow("02", "proof T$1 {1: assume x 2: trivial 3: revoke 2}", 1)>]
    [<DataRow("03", "proof T$1 {1: assume x 2: trivial 3: revoke 1}", 0)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestPR015(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = PR015 ""
            runTestHelper "TestPR015.fpl" fplCode code expected
