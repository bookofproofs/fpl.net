namespace Diagnostics.IdentifierRelated

open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open TestFplInterpreter.Helpers.Common

(* ID017
   Purpose: Report that a type or identifier could not be resolved for the call site.
   What it indicates: The system failed to determine the type or declaration for a referenced name in the current context (for example, a base/member reference or a type name used in a signature could not be resolved to a declared entity).
   Use: Emitted during inheritance/constructor validation to highlight unresolved type/identifier references so the author can locate the source of the missing declaration.
   Action / Treat: Ensure the referenced type/identifier is declared and visible from the current scope (add the declaration, correct the qualifier, or fix the signature), or adjust the code to avoid referencing undeclared names. ID017 is an error that must be resolved for successful type resolution. *)

[<TestClass>]
type TestID017() =

    [<DataRow("00", "def cl A {intr} def cl B {intr} def cl C {intr} def cl D:A,B,C,E {ctor D() {dec base.A() base.B() base.C() base.F(); } }", 1)>]
    [<DataRow("01", "def cl A {intr} def cl B:A {ctor B() {dec base.A(); } }", 0)>]
    [<DataRow("01a", "thm A {true} def cl A def cl B:A {ctor B() {dec base.A(); } }", 1)>]
    [<DataRow("02", "def cl A {intr} def cl B:A {ctor B() {dec base.C(); } }", 1)>]
    [<DataRow("03", "def cl A { ctor A() {dec base.Obj(); } }", 1)>]
    [<DataRow("04", "def cl A { ctor A() {dec base.B(); } }", 1)>]
    [<DataRow("05", "def cl A:C { ctor A() {dec base.Obj(); } }", 1)>]
    [<DataRow("07", "uses Fpl.SetTheory def cl Test:Set {ctor Test() {dec base.Obj(); } }", 1)>]
    [<DataRow("08", "uses Fpl.SetTheory def cl Test:Set {ctor Test() {dec base.Set(); } }", 0)>]
    [<DataRow("50", "def pred A() {true} def pred A(x:obj) {true} def pred T(x:A) {intr}", 1)>]
    [<DataRow("51", "def pred A() {true} def func A(x:obj)->obj {intr} def pred T(x:A) {intr}", 1)>]
    [<DataRow("52", "def pred A() {true} def func A()->obj {intr} def pred T(x:A) {intr}", 1)>]
    [<DataRow("53", "def pred A() {true} def pred T(x:A) {intr}", 0)>]
    [<DataRow("54", "def func A(x:obj)->obj {intr} def pred T(x:A) {intr}", 0)>]
    [<DataRow("55", "def pred B() {true} def func A()->obj {intr} def pred T(x:A) {intr}", 0)>]
    [<DataRow("56", "def cl Set def cl SetRoster:Set def pred T(x:Set) {intr}", 0)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestID017(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID017 ("","")
            runTestHelper "TestID017.fpl" fplCode code expected
