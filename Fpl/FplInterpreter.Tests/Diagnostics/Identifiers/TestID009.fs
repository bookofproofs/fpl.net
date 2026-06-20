namespace Diagnostics.Identifiers

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open FplInterpreter.Main
open TestFplInterpreter.Helpers.Common
open TestSharedConfig


[<TestClass>]
type TestID009() =

    [<DataRow("def cl Test {intr}", 0)>]
    [<DataRow("def cl Test:Test {intr}", 1)>]
    [<DataRow("def cl Test:Test1, Test2, Test3 {intr}", 0)>]
    [<DataRow("def cl Test:Test1, Test2, Test3, Test {intr}", 1)>]
    [<DataRow("uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestID009(fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID009 ""
            runTestHelper "TestID009.fpl" fplCode code expected
