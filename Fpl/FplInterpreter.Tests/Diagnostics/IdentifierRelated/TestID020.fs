namespace Diagnostics.IdentifierRelated

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open FplInterpreter.Main
open TestFplInterpreter.Helpers.Common
open TestSharedConfig

[<TestClass>]
type TestID020() =

    [<DataRow("00", "def cl A {intr} def cl B {intr} def cl C {intr} def cl D:A,B,C {ctor D() {dec base.A(); } }", 2)>]
    [<DataRow("00a", "def cl A {intr} def cl B {intr} def cl C {intr} def cl D:A,B,C {ctor D() {dec base.B(); } }", 2)>]
    [<DataRow("00b", "def cl A {intr} def cl B {intr} def cl C {intr} def cl D:A,B,C {ctor D() {dec base.C(); } }", 2)>]
    [<DataRow("00c", "def cl A {intr} def cl B {intr} def cl C {intr} def cl D:A,B,C {ctor D() {dec base.A() base.B(); } }", 1)>]
    [<DataRow("00d", "def cl A {intr} def cl B {intr} def cl C {intr} def cl D:A,B,C {ctor D() {dec base.A() base.C(); } }", 1)>]
    [<DataRow("00e", "def cl A {intr} def cl B {intr} def cl C {intr} def cl D:A,B,C {ctor D() {dec base.B() base.C(); } }", 1)>]
    [<DataRow("00f", "def cl A {intr} def cl B {intr} def cl C {intr} def cl D:A,B,C {ctor D() {dec base.A() base.B() base.C(); } }", 0)>]
    [<DataRow("01", "def cl A {intr} def cl B:A {ctor B() {dec base.A(); } }", 0)>]
    [<DataRow("01a", "def cl A {intr} def cl B:A {ctor B() {} }", 1)>]
    [<DataRow("02", "def cl A { ctor A() {dec base.Obj(); } }", 0)>]
    [<DataRow("02a", "def cl A { ctor A() {} }", 0)>]
    [<DataRow("03", "def cl A { ctor A() {dec base.Obj(); } }", 0)>]
    [<DataRow("03a", "def cl A:C { ctor A() {dec base.Obj(); } }", 1)>]
    [<DataRow("03b", "def cl A {intr} def cl B:A {intr} def cl C:B { ctor C() {dec base.Obj(); } }", 1)>]
    [<DataRow("04", "uses Fpl.SetTheory def cl Test:Set {ctor Test() {dec base.Obj(); } }", 1)>]
    [<DataRow("04a", "uses Fpl.SetTheory def cl Test:Set {ctor Test() {dec base.Set(); } }", 0)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestID020(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID020 ""
            runTestHelper "TestID020.fpl" fplCode code expected
