namespace Diagnostics.StructureRelated

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open FplInterpreter.Main
open TestFplInterpreter.Helpers.Common
open TestSharedConfig


[<TestClass>]
type TestST004() =

    [<DataRow("01", """loc Equal(x,y) := !tex: x "=" y !eng: x " equals " y !ger: x " ist gleich " y !pol: x " równa się " y;""", 0)>]
    [<DataRow("02", """loc Equal(x,y) := !eng: x " equals " y !ger: x " ist gleich " y !pol: x " równa się " y;""", 1)>] // tex implementation missing
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestST004(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ST004 ""
            runTestHelper "TestST004.fpl" fplCode code expected
