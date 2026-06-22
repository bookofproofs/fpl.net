namespace Diagnostics.ProofRelated

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open FplInterpreter.Main
open TestFplInterpreter.Helpers.Common
open TestSharedConfig

(* PR006
   Purpose: Report a reference to a non‑existing proof argument identifier.
   What it indicates: A proof reference targeted an argument identifier that does not exist in the referenced proof (for example referencing `T$1`'s argument `x` when no such argument was declared).
   Use: Helps locate broken argument references so authors can correct the identifier or the target proof.
   Action / Treat: Fix the reference to point to an existing argument identifier in the referenced proof or correct the referenced proof signature. 
   Treat PR006 as an error that must be resolved for correct justification/argument referencing. *)

[<TestClass>]
type TestPR006() =

    [<DataRow("00", """thm A {true} proof A$1 {1: trivial}proof T$1 { 1: trivial qed}""", 0)>]
    [<DataRow("01", """thm A {true} proof A$1 {1: trivial}proof T$1 { 1. A$1:2, A$1:3 |- trivial qed}""", 2)>]
    [<DataRow("02", """thm A {true} proof A$1 {1: trivial}proof T$1 { 1: trivial 2. A$1:1 |- trivial qed}""", 0)>]
    [<DataRow("03", """thm A {true} proof A$1 {1: trivial}proof T$1 { 1: trivial 2. A$1:1, A$1:1a |- trivial qed}""", 1)>]
    [<DataRow("04", """thm A {true} proof A$1 {1: trivial}proof T$1 { 1. A$1:2, A$1:1 |- trivial qed}""", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestPR006(no: string, fplCode:string, expected:int) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = PR006 ("", "")
            runTestHelper "TestPR005.fpl" fplCode code expected
