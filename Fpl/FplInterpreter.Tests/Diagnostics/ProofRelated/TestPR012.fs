namespace Diagnostics.ProofRelated

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open FplInterpreter.Main
open TestFplInterpreter.Helpers.Common
open TestSharedConfig


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
