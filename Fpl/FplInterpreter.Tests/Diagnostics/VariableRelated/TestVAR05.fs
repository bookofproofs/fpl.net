namespace Diagnostics.VariableRelated

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open FplInterpreter.Main
open TestFplInterpreter.Helpers.Common
open TestSharedConfig


[<TestClass>]
type TestVAR05() =

    [<DataRow("def pred T() { all x:obj {true}}", 1)>]
    [<DataRow("def pred T() { all x:obj {x}}", 0)>]
    [<DataRow("def pred T() { ex x:obj {true}}", 1)>]
    [<DataRow("def pred T() { ex x:obj {x}}", 0)>]
    [<DataRow("def pred T() { exn$1 x:obj {true}}", 1)>]
    [<DataRow("def pred T() { exn$1 x:obj {x}}", 0)>]
    [<DataRow("uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestVAR05(fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = VAR05 ""
            runTestHelper "TestVAR05.fpl" fplCode code expected
