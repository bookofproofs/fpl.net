namespace Diagnostics.IdentifierRelated

open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open TestFplInterpreter.Helpers.Common

(* ID015
   Purpose: Report illegal uses of the special identifier `parent` from contexts that do not expose it.
   What it indicates: A `parent` reference was encountered in a declaration or scope where no parent instance is available (for example in a static/signature context or otherwise invalid location).
   Use: Emitted during symbol-table validation to locate and highlight invalid `parent` references so the author can correct their placement or intent.
   Action / Treat: Move the `parent` reference into an appropriate instance context, remove it, or refactor the declaration so a parent is available. Treat ID015 as an error that must be resolved for correct embedding and resolution. *)

[<TestClass>]
type TestID015() =

    [<DataRow("00a", """def cl A {dec x:obj x:=parent; ctor A() {}}""", 1)>]
    [<DataRow("00b", """def cl A {ctor A() {dec x:obj x:=parent;}}""", 0)>]
    [<DataRow("00c", """def cl A {intr property pred T() { is(parent,A) } }""", 0)>]
    [<DataRow("00d", """def cl A {intr property func T()->obj {dec x:obj x:=parent; return x } }""", 0)>]
    [<DataRow("00e", """def cl A {intr property pred T() { is(parent,A) } }""", 0)>]
    [<DataRow("00f", """def cl A {intr property func T()->obj {dec x:obj x:=parent; return x } }""", 0)>]
    // TODO: issue diagnostics when asserting is(parent,...), only asserting is(self,...) inside classes is allowed
    [<DataRow("01a", """def pred A() {dec assert is(parent,A); true }""", 1)>]
    [<DataRow("01b", """def pred A() {intr property pred T() { is(parent,A) } }""", 0)>]
    [<DataRow("01c", """def pred A() {intr property func T()->pred { return is(parent,A) } }""", 0)>]
    // TODO: issue diagnostics when asserting is(parent, ...), only asserting is(self,...) inside classes is allowed
    [<DataRow("02a", """def func A()->obj {dec x:obj assert is(parent,A); return x}""", 1)>]
    [<DataRow("02b", """def func A()->obj {intr property pred T() { is(parent,A) } }""", 0)>]
    [<DataRow("02c", """def func A()->obj {intr property func T()->pred { return is(parent,A) } }""", 0)>]
    [<DataRow("03", """axiom A {parent}""", 1)>]
    [<DataRow("04", """theorem A {parent}""", 1)>]
    [<DataRow("05", """lemma A {parent}""", 1)>]
    [<DataRow("06", """prop A {parent}""", 1)>]
    [<DataRow("08", """conj A {parent}""", 1)>]
    [<DataRow("09", """cor A$1 {parent}""", 1)>]
    [<DataRow("10", """prf A$1 {1: parent qed}""", 1)>]
    [<DataRow("11", """inf A {pre: true con: parent}""", 1)>]
    [<DataRow("12", """inf A {pre: parent con: true}""", 1)>]
    [<DataRow("13", """loc not(parent) := !tex: "\neg(" x ")";""", 1)>]
    // TODO: issue diagnostics when asserting is(parent, ...), only asserting is(self,...) inside classes is allowed
    [<DataRow("14a", """ext A x@/\d+/ -> obj {dec assert is(parent,A); ret x}""", 1)>]
    [<DataRow("14b", """def cl Nat def func Zero() -> Nat def func Succ(n: Nat) -> Nat def pred Equal(x,y: tpl) infix "=" 50 { del.Equal(x,y) } ext Digits x@/\d+/ -> Nat { return mcases (| (x = @0): Zero() | (x = @1): Succ(Zero()) | (x = @2): Succ(Succ(Zero())) ? Succ(parent(delegate.Decrement(x))) )}""", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestID015(no:string, fplCode:string, expected:int) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID015 ""
            runTestHelper "TestID015.fpl" fplCode code expected
