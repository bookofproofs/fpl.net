namespace Diagnostics.ProofRelated

open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open TestFplInterpreter.Helpers.Common


(* PR016
   Purpose: Report invalid revocation of a previously assumed argument when it is not the most recently assumed one.
   What it indicates: A `revoke` step referenced an argument identifier that was not the last assumed argument in the current proof sequence. Revocation must follow LIFO order: only the most recently assumed argument can be revoked.
   Use: Helps locate proof steps with incorrect revoke ordering so authors can reorder assumptions/revocations or adjust proof structure.
   Action / Treat: Revoke only the last assumed argument (or reorder/remove assumptions so the intended argument becomes last). Treat PR016 as an error that must be fixed for the proof to be valid. *)

[<TestClass>]
type TestPR016() =

    [<DataRow("00", "proof T$1 {1: assume x 2: trivial 3: revoke 2}", 0)>]
    [<DataRow("01", "proof T$1 {1: assume x 2: trivial 3: assume y 4: trivial 5: revoke 1}", 1)>]
    [<DataRow("02", "proof T$1 {1: assume x 2: trivial 3: assume y 4: trivial 5: revoke 3}", 0)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestPR016(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = PR016 ""
            runTestHelper "TestPR016.fpl" fplCode code expected
