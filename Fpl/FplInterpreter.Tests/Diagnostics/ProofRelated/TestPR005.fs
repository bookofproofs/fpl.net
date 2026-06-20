namespace Diagnostics.ProofRelated

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open FplInterpreter.Main
open TestFplInterpreter.Helpers.Common
open TestSharedConfig


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
