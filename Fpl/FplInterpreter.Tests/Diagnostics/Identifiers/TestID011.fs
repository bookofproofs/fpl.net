namespace Diagnostics.Identifiers

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open FplInterpreter.Main
open TestFplInterpreter.Helpers.Common
open TestSharedConfig


[<TestClass>]
type TestID011() =

    [<DataRow("00", "def cl A {intr}", 0)>]
    [<DataRow("01", "def cl A {intr} def cl B:A {intr}", 0)>]
    [<DataRow("02", "def cl A {intr} def cl B:A {intr} def cl C:B,A {intr}", 1)>]
    [<DataRow("03", "uses Fpl.SetTheory def cl Test:EmptySet,Set {intr}", 1)>]
    [<DataRow("04", "uses Fpl.SetTheory def cl Test:Set, EmptySet {intr}", 1)>]
    [<DataRow("05", "def cl A {intr} def cl B:A {intr} def cl C:A,B {intr}", 1)>]
    [<DataRow("06", "def cl A {intr} def cl B:A {intr} def cl C:B {intr}", 0)>]
    [<DataRow("07", "uses Fpl.SetTheory def cl Test:EmptySet {intr}", 0)>]
    [<DataRow("08", "uses Fpl.SetTheory def cl Test:Set {intr}", 0)>]
    [<DataRow("09", "uses Fpl.Commons uses Fpl.SetTheory def cl Test:Set {intr}", 0)>]
    [<DataRow("10", "def cl A {intr} def cl B:A {intr} def cl C:A {intr}", 0)>]
    [<DataRow("11", "def cl A {intr} def cl B:A {intr} def cl C:A,A {intr}", 1)>]
    [<DataRow("12", "def cl A {intr} def cl B:A {intr} def cl C {intr}", 0)>]
    [<DataRow("13", "def cl A {intr} def cl B:A {intr} def cl C:D,E {intr}", 0)>]
    [<DataRow("14", "uses Fpl.SetTheory def cl Test:Set {intr}", 0)>]
    [<DataRow("15", "uses Fpl.SetTheory def cl Test:EmptySet {intr}", 0)>]
    [<DataRow("16", "uses Fpl.SetTheory def cl Test:Set {intr}", 0)>]
    [<DataRow("17", "uses Fpl.SetTheory def cl Test:EmptySet {intr}", 0)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestID011(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID011 ("","")
            runTestHelper "TestID011.fpl" fplCode code expected
