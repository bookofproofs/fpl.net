namespace Diagnostics.Identifiers

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open FplInterpreter.Main
open TestFplInterpreter.Helpers.Common
open TestSharedConfig


[<TestClass>]
type TestID021() =

    [<DataRow("01", "def cl A {intr} def cl B:A {ctor B() {dec base.A(); } }", 0)>]
    [<DataRow("01a", "def cl A {intr} def cl B:A {ctor B() {dec base.A() base.A(); } }", 1)>]
    [<DataRow("02", "def cl A { ctor A() {dec base.Obj(); } }", 0)>]
    [<DataRow("02a", "def cl A { ctor A() {dec base.Obj() base.Obj(); } }", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestID021(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID021 ""
            runTestHelper "TestID021.fpl" fplCode code expected
