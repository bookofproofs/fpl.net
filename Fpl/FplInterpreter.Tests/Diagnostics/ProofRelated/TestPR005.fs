namespace Diagnostics.ProofRelated

open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open TestFplInterpreter.Helpers.Common

(* PR005
   Purpose: Report references to argument identifiers that are not defined in the current context.
   What it indicates: A code site attempted to refer to an argument identifier that does not exist (for example an argument reference that is not declared in the referenced signature or scope).
   Use: Helps locate broken argument references so authors can correct the identifier or the target declaration.
   Action / Treat: Fix the reference to point to an existing argument identifier or correct the target declaration; PR005 is an error that must be resolved for correct referencing. *)

[<TestClass>]
type TestPR005() =

    [<DataRow("""proof T$1 { 1: trivial qed}""", 0)>]
    [<DataRow("""proof T$1 { 1. 2, 3 |- trivial qed}""", 2)>]
    [<DataRow("""proof T$1 { 1: trivial 2. 1 |- trivial qed}""", 0)>]
    [<DataRow("""proof T$1 { 1: trivial 2. 1, 1a |- trivial qed}""", 1)>]
    [<DataRow("""proof T$1 { 1. 1, 1, 1 |- trivial qed}""", 3)>]
    [<DataRow("uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestPR005(fplCode:string, expected:int) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = PR005 ""
            runTestHelper "TestPR005.fpl" fplCode code expected
