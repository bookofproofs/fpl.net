namespace Diagnostics.Identifiers

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open FplInterpreter.Main
open TestFplInterpreter.Helpers.Common
open TestSharedConfig


[<TestClass>]
type TestID010() =

    [<DataRow("00", "def cl Test {intr}", 0)>]
    [<DataRow("01", "def cl Test:Set {intr}", 1)>]
    [<DataRow("02", "def class Set def cl Test:Set {intr}", 0)>]
    [<DataRow("03", "def cl Set {intr} def cl Test:Set {intr}", 0)>]
    [<DataRow("04", "def cl Set {intr} def cl Test:SetTypo {intr}", 1)>]
    [<DataRow("05", "def cl Set {intr} def pred Test() {dec x:Set; true}", 0)>]
    [<DataRow("06", "def cl Set {intr} def pred Test() {dec x:object; is(x,Set)}", 0)>]

    [<DataRow("07", "def cl A {intr} thm T {true} proof T$1 {1. bydef A |- trivial }", 0)>]
    [<DataRow("08", "def cl A {intr} thm T {true} proof T$1 {1. bydef B |- trivial }", 1)>]
    [<DataRow("09", "thm A {true} thm T {true} proof T$1 {1. A |- trivial }", 0)>]
    [<DataRow("10", "thm B {true} thm T {true} proof T$1 {1. A |- trivial }", 1)>]
    [<DataRow("10a", "thm A {true} cor A$1 {true} thm T {true} proof T$1 {1. bycor A$1 |- trivial }", 0)>]
    [<DataRow("10b", "ax A {true} thm T {true} proof T$1 {1. byax A |- trivial }", 0)>]
    [<DataRow("10c", "ax A {true} thm T {true} proof T$1 {1. byax B |- trivial }", 1)>]

    // the following examples should not emit ID010 because this context is covered by the SIG04 diagnostics
    [<DataRow("11","def pred Test(x:Set) {intr}", 1)>]
    [<DataRow("12","def class Set def pred IsEmpty(x: Set) {true}", 0)>]
    [<DataRow("13", "def pred Test(x:Set) {intr}", 1)>]
    [<DataRow("14", "def cl Set {intr} def pred Test(x:SetTypo) {intr}", 1)>]
    [<DataRow("15", "def cl Set {intr} axiom Test {dec x:SetTypo; true}", 1)>]
    [<DataRow("16", "def cl Set {intr} axiom Test {dec x:SetTypo; true}", 1)>]
    [<DataRow("17", "def pred Test() {dec x:Set; true}", 1)>]
    [<DataRow("18", "axiom A { all x:Nat {true} }", 1)>]
    [<DataRow("19", "def pred Test() {dec x:object; is(x,Set)}", 1)>]
    [<DataRow("20", """def pred T1() {true} def pred Test() { OtherTest() }""", 1)>]    
    [<DataRow("21", """def func Succ(n:Nat) -> obj {intr}""", 1)>]
    [<DataRow("22", """def cl A def pred T() { is (self,ATypo) }""", 1)>]
    [<DataRow("23", """def cl A {intr prty func P()->obj prty pred T() {is(P(), obj)} }""", 0)>]
    [<DataRow("23a", """def cl A {intr prty func P()->obj prty pred T() {is(P, func)} }""", 0)>]
    [<DataRow("24", """def cl A {intr prty pred P() } def pred T() {dec a:A a:=A(); a.P()}""", 0)>]
    [<DataRow("25a", """proof A$1 {1: trivial}ext U x@/\d+/ -> pred {ret A$1} """, 0)>]
    [<DataRow("25b", """cor A$1 {true} ext U x@/\d+/ -> pred {ret A$1} """, 0)>]
    [<DataRow("25c", """cor A$1 {true} def pred T() {A$1}""", 0)>]
    [<DataRow("26a", """ext Digits x@/\d+/ -> Digits { ret x }""", 0)>] // extensions allow mapping to themselves 
    [<DataRow("26b", """def func Digits() -> Digits""", 1)>] // functions do not allow mapping to themselves


    // array types 
    [<DataRow("AR1", "def cl A def pred T() {dec arr:*ind[A]; true}", 0)>]  
    [<DataRow("AR1a", "def cl A def pred T() {dec arr:*ind[ATypo]; true}", 1)>]  
    [<DataRow("AR2", "def cl A def pred T() {dec arr:*A[ind]; true}", 0)>]  
    [<DataRow("AR2a", "def cl A def pred T() {dec arr:*ATypo[ind]; true}", 1)>]  

    [<DataRow("MAP1", """def func T()->A {intr}""", 1)>]
    [<DataRow("MAP1a", """def cl A def func T()->A {intr}""", 0)>]
    [<DataRow("MAP2", """def func T()->*A[ind] {intr}""", 1)>]
    [<DataRow("MAP2a", """def cl A def func T()->*A[ind] {intr}""", 0)>]
    [<DataRow("MAP3", """def func T()->*ind[A] {intr}""", 1)>]
    [<DataRow("MAP3a", """def cl A def func T()->*ind[A] {intr}""", 0)>]


    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestID010(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID010 ""
            runTestHelper "TestID010.fpl" fplCode code expected
