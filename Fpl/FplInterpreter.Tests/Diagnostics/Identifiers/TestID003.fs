namespace Diagnostics.Identifiers

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open FplInterpreter.Main
open TestFplInterpreter.Helpers.Common
open TestSharedConfig


[<TestClass>]
type TestID003() =

    [<DataRow("theorem Test {true} proof Test$1 {1: trivial}", 0)>]
    [<DataRow("theorem TestTypo {true} proof Test$1 {1: trivial}", 1)>]
    [<DataRow("corollary Test$1 {true} proof Test$1$1 {1: trivial}", 0)>]
    [<DataRow("theorem Test {true} corollary Test$1 {true} proof Test$1$1 {1: trivial}", 0)>]
    [<DataRow("uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestID003(fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID003 ""
            runTestHelper "TestID003.fpl" fplCode code expected
