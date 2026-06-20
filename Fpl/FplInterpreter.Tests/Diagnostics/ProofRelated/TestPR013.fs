namespace Diagnostics.ProofRelated

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open FplInterpreter.Main
open TestFplInterpreter.Helpers.Common
open TestSharedConfig


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

