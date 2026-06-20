namespace Diagnostics.VariableRelated

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open FplInterpreter.Main
open TestFplInterpreter.Helpers.Common
open TestSharedConfig


[<TestClass>]
type TestVAR07() =

    [<DataRow("00", "def pred T() {exn$1 n:pred { n } }", 0)>]
    [<DataRow("01", "def pred T() {exn$1 n, m:pred { n } }", 1)>]
    [<DataRow("02", "def pred T() {exn$1 n:pred, m:pred { n } }", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestVAR07(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = VAR07 ""
            runTestHelper "TestVAR07.fpl" fplCode code expected
