namespace Diagnostics.Identifiers

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open FplInterpreter.Main
open TestFplInterpreter.Helpers.Common
open TestSharedConfig


[<TestClass>]
type TestID006() =

    [<DataRow("theorem Test {true} corollary Test$1 {true}", 0)>]
    [<DataRow("theorem TestTypo {true} corollary Test$1 {true}", 1)>]
    [<DataRow("theorem Test {true} corollary Test$1 {true} corollary Test$1$1 {true}", 0)>]
    [<DataRow("uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestID006(fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID006 ""
            runTestHelper "TestID006.fpl" fplCode code expected
