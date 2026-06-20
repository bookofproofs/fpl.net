namespace Diagnostics.IdentifierRelated

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open FplInterpreter.Main
open TestFplInterpreter.Helpers.Common
open TestSharedConfig

[<TestClass>]
type TestID016() =

    [<DataRow("00a", """def cl A {dec x:obj x:=self; ctor A() {}}""", 0)>]
    [<DataRow("00b", """def cl A {ctor A() {dec x:obj x:=self;}}""", 1)>]
    [<DataRow("00c", """def cl A {intr property pred T() { is(self,A) } }""", 0)>]
    [<DataRow("00d", """def cl A {intr property func T()->obj {dec x:obj x:=self; return x } }""", 0)>]
    [<DataRow("00e", """def cl A {intr property pred T() { is(self,A) } }""", 0)>]
    [<DataRow("00f", """def cl A {intr property func T()->obj {dec x:obj x:=self; return x } }""", 0)>]
    // TODO: issue diagnostics when asserting is(self,...) outside classes
    [<DataRow("01a", """def pred A() {dec assert is(self,A); true }""", 0)>]
    [<DataRow("01b", """def pred A() {intr property pred T() { is(self,A) } }""", 0)>]
    [<DataRow("01c", """def pred A() {intr property func T()->pred { return is(self,A) } }""", 0)>]
    // TODO: issue diagnostics when asserting is(self,...) outside classes
    [<DataRow("02a", """def func A()->obj {dec x:obj assert is(self,A); return x}""", 0)>]
    [<DataRow("02b", """def func A()->obj {intr property pred T() { is(self,A) } }""", 0)>]
    [<DataRow("02c", """def func A()->obj {intr property func T() ->pred{ return is(self,A) } }""", 0)>]
    [<DataRow("03", """axiom A {self}""", 1)>]
    [<DataRow("04", """theorem A {self}""", 1)>]
    [<DataRow("05", """lemma A {self}""", 1)>]
    [<DataRow("06", """prop A {self}""", 1)>]
    [<DataRow("08", """conj A {self}""", 1)>]
    [<DataRow("09", """cor A$1 {self}""", 1)>]
    [<DataRow("10", """prf A$1 {1: self qed}""", 1)>]
    [<DataRow("11", """inf A {pre: true con: self}""", 1)>]
    [<DataRow("12", """inf A {pre: self con: true}""", 1)>]
    [<DataRow("13", """loc not(self) := !tex: "\neg(" x ")";""", 1)>]
    // TODO: issue diagnostics when asserting is(self,...) outside classes
    [<DataRow("14a", """ext A x@/\d+/ -> obj {dec assert is(self,A); ret x}""", 0)>]
    [<DataRow("14b", """def cl Nat def func Zero() -> Nat def func Succ(n: Nat) -> Nat def pred Equal(x,y: tpl) infix "=" 50 { del.Equal(x,y) } ext Digits x@/\d+/ -> Nat { return mcases (| (x = @0): Zero() | (x = @1): Succ(Zero()) | (x = @2): Succ(Succ(Zero())) ? Succ(self(delegate.Decrement(x))) )}""", 0)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestID016(no: string, fplCode:string, expected:int) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID016 ""
            runTestHelper "TestID016.fpl" fplCode code expected
