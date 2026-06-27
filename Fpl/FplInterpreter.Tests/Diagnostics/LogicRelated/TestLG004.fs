namespace Diagnostics.LogicRelated

open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open TestFplInterpreter.Helpers.Common

(* LG004
   Purpose: Warn about potential side effects caused by statements placed inside predicate expressions.
   What it indicates: A non-expression statement was encountered inside a predicate context where side effects are unexpected or unsafe.
   Use: Draws attention to places where imperative or effectful code appears inside predicates so authors can review intent.
   Action / Treat: Replace the statement with a pure expression where appropriate, or move side-effecting code out of the predicate. LG004 is emitted as a warning to encourage safer, side-effect-free predicate definitions. *)

[<TestClass>]
type TestLG004() =

    [<DataRow("00", """axiom T { true }""", 0)>]
    [<DataRow("01", """axiom T {dec x:obj; true }""", 0)>]
    [<DataRow("01a", """axiom T {dec x:obj x:=x; true }""", 1)>]
    [<DataRow("02", """theorem T { true }""", 0)>]
    [<DataRow("02", """theorem T {dec x:obj; true }""", 0)>]
    [<DataRow("02a", """theorem T {dec x:obj x:=x; true }""", 1)>]
    [<DataRow("03", """proposition T { true }""", 0)>]
    [<DataRow("03", """proposition T {dec x:obj; true }""", 0)>]
    [<DataRow("03a", """proposition T {dec x:obj x:=x; true }""", 1)>]
    [<DataRow("04", """lemma T { true }""", 0)>]
    [<DataRow("04", """lemma T {dec x:obj; true }""", 0)>]
    [<DataRow("04a", """lemma T {dec x:obj x:=x; true }""", 1)>]
    [<DataRow("05", """corollary T$1 { true }""", 0)>]
    [<DataRow("05", """corollary T$1 {dec x:obj; true }""", 0)>]
    [<DataRow("05a", """corollary T$1 {dec x:obj x:=x; true }""", 1)>]
    [<DataRow("06", """conjecture T { true }""", 0)>]
    [<DataRow("06", """conjecture T {dec x:obj; true }""", 0)>]
    [<DataRow("06a", """conjecture T {dec x:obj x:=x; true }""", 1)>]
    [<DataRow("07", """postulate T { true }""", 0)>]
    [<DataRow("07", """postulate T {dec x:obj; true }""", 0)>]
    [<DataRow("07a", """postulate T {dec x:obj x:=x; true }""", 1)>]
    [<DataRow("08", """def pred T() { true }""", 0)>]
    [<DataRow("08", """def pred T() {dec x:obj; true }""", 0)>]
    [<DataRow("08a", """def pred T() {dec x:obj x:=x; true }""", 0)>]
    [<DataRow("09", """def pred T(x:obj) { true }""", 0)>]
    [<DataRow("09", """def pred T(x:obj) {dec x:obj; true }""", 0)>]
    [<DataRow("09a", """def pred T(x:obj){dec x:obj x:=x; true }""", 0)>]
    [<DataRow("10", """def func T()->obj { intr }""", 0)>]
    [<DataRow("10", """def func T()->obj {dec x:obj; ret x }""", 0)>]
    [<DataRow("10a", """def func T()->obj {dec x:obj x:=x; ret x }""", 0)>]
    [<DataRow("11", """def func T(x:obj)->obj { intr }""", 0)>]
    [<DataRow("11", """def func T(x:obj)->obj {dec x:obj; ret x }""", 0)>]
    [<DataRow("11a", """def func T(x:obj)->obj {dec x:obj x:=x; ret x }""", 0)>]
    [<DataRow("12", """inf T { pre: true con: true }""", 0)>]
    [<DataRow("12", """inf T {dec x:obj; pre: true con: true }""", 0)>]
    [<DataRow("12a", """inf T {dec x:obj x:=x; pre: true con: true }""", 0)>]
    [<DataRow("13", """inf T {dec x:obj; pre: true con: true }""", 0)>]
    [<DataRow("13", """inf T {dec x:obj; pre: true con: true }""", 0)>]
    [<DataRow("13a", """inf T {dec x:obj x:=x; pre: true con: true }""", 0)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestLG004(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = LG004 "" 
            runTestHelper "TestLG004.fpl" fplCode code expected
