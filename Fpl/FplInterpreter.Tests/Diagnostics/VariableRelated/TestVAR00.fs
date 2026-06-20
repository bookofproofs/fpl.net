namespace Diagnostics.VariableRelated

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open FplInterpreter.Main
open TestFplInterpreter.Helpers.Common
open TestSharedConfig


[<TestClass>]
type TestVAR00() =

    [<DataRow("def predicate Test(x,y:* pred[ind]) {true}", 1)>]
    [<DataRow("def predicate Test(x,y:* pred[obj]) {true}", 1)>]
    [<DataRow("def predicate Test(x,y: pred) {true}", 0)>]
    [<DataRow("def predicate Test(x:* pred[ind]) {true}", 0)>]
    [<DataRow("def predicate Test(x:* pred[obj]) {true}", 0)>]
    [<DataRow("uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestVAR00(fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = VAR00
            runTestHelper "TestVAR00.fpl" fplCode code expected
