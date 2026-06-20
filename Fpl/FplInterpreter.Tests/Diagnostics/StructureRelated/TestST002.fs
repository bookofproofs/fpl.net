namespace Diagnostics.StructureRelated

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open FplInterpreter.Main
open TestFplInterpreter.Helpers.Common
open TestSharedConfig


[<TestClass>]
type TestST002() =

    [<DataRow("00a", "def cl A {ctor A(x:obj){} } ", 1)>]
    [<DataRow("00b", "def cl A:B {ctor A(){dec base.B();} } ", 0)>]
    [<DataRow("00c", "def cl A {ctor A(){} } ", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestST002(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ST002 ""
            runTestHelper "TestST002.fpl" fplCode code expected
