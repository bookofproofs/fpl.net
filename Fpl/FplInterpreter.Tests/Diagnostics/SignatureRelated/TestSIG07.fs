namespace Diagnostics.SignatureRelated

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open FplInterpreter.Main
open TestFplInterpreter.Helpers.Common
open TestSharedConfig


[<TestClass>]
type TestSIG07() =

    [<DataRow("00a", "def pred S(x:pred) {dec T:=true; true}", 1)>] 
    [<DataRow("00a_", "def cl T def pred S(x:pred) {dec T:=true; true}", 1)>] // SIG07 won't be issued due to proceeding ID010 error (T unknown)
    [<DataRow("00b", "ax T {true} def pred S(x:pred) {dec T:=true; true}", 1)>]
    [<DataRow("00c", "thm T {true} def pred S(x:pred) {dec T:=true; true}", 1)>]
    [<DataRow("00d", "lem T {true} def pred S(x:pred) {dec T:=true; true}", 1)>]
    [<DataRow("00e", "prop T {true} def pred S(x:pred) {dec T:=true; true}", 1)>]
    [<DataRow("00f", "conj T {true} def pred S(x:pred) {dec T:=true; true}", 1)>]
    [<DataRow("00g", "ext T x@/\d+/ -> obj {ret x} def pred S(x:pred) {dec T:=true; true}", 1)>]
    [<DataRow("00h", "def cl T def pred S(x:pred) {dec T:=true; true}", 1)>]
    [<DataRow("00i", """loc T := !tex: "T"; def pred S(x:pred) {dec T:=true; true}""", 1)>]
    [<DataRow("00k", "inf T {pre:true con:true} def pred S(x:pred) {dec T:=true; true}", 1)>]
    //[<DataRow("00l", "proof T$1 {1: trivial}def pred S(x:pred) {dec T$1:=true; true}", 1)>] // syntactically incorrect
    //[<DataRow("00m", "cor T$1 {true} def pred S(x:pred) {dec T$1:=true; true}", 1)>] // syntactically incorrect

    [<DataRow("01a", "def pred T() {intr} def pred S(x:pred) {dec T():=true; true}", 1)>]
    [<DataRow("01b", "def func T()->obj {intr} def pred S(x:pred) {dec T():=true; true}", 1)>]
    [<DataRow("01c", "def pred T(a:ind) {intr} def pred S(x:pred) {dec T(a):=true; true}", 1)>]
    [<DataRow("01d", "def func T(a:ind)->obj {intr} def pred S(x:pred) {dec T(a):=true; true}", 1)>]
    [<DataRow("02a", "def pred S() {dec self:=true; true}", 1)>]
    [<DataRow("02b", "def pred S() {dec parent:=true; true}", 0)>] // SIG07 won't be issued due to proceeding errors (ID015)
    [<DataRow("02c", "def pred S() {dec self():=true; true}", 1)>]
    [<DataRow("02d", "def pred S() {dec parent():=true; true}", 1)>] 
    [<DataRow("02e", "def pred S() {dec self(a):=true; true}", 1)>]
    [<DataRow("02e_", "def pred S() {dec a:obj self(a):=true; true}", 1)>]
    [<DataRow("02f", "def pred S() {dec parent(a):=true; true}", 1)>]
    [<DataRow("03a", "def pred S() {dec A:=true; true}", 1)>]
    [<DataRow("03b", "ax A {true} def pred S() {dec A:=true; true}", 1)>]
    [<DataRow("03c", "def pred S() {dec A():=true; true}", 1)>]
    [<DataRow("03d", "ax A {true} def pred S() {dec A():=true; true}", 1)>]
    [<DataRow("03e", "def pred S() {dec A(a):=true; true}", 1)>]
    [<DataRow("03f", "ax A {true} def pred S() {dec A(a):=true; true}", 1)>]
    [<DataRow("04a", "def pred S() {dec @1:=true; true}", 1)>]
    [<DataRow("04b", "ext D x@/\d+/ -> obj { ret x } def pred S() {dec @1:=true; true}", 1)>]
    [<DataRow("04c", "def pred S() {dec @1():=true; true}", 1)>]
    [<DataRow("04d", "ext D x@/\d+/ -> obj { ret x } def pred S() {dec @1():=true; true}", 1)>]
    [<DataRow("04e", "def pred S() {dec @1(a):=true; true}", 1)>]
    [<DataRow("04f", "ext D x@/\d+/ -> obj { ret x } def pred S() {dec @1(a):=true; true}", 1)>]

    [<DataRow("10a", "def pred S(x:pred) {dec x:=true; true}", 0)>]
    [<DataRow("10b", "def pred S(x:pred) {dec x():=true; true}", 1)>]
    [<DataRow("10c", "def pred S(x:*pred[ind]) {dec x[$1]:=true; true}", 0)>]
    [<DataRow("10d", "def pred S(x:*pred[obj]) {dec x[@1]:=true; true}", 0)>]
    [<DataRow("10e", "def pred S(x:*pred[Nat]) {dec a:Nat x[a]:=true; true}", 0)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestSIG07(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SIG07 ("", "", "")
            runTestHelper "TestSIG07.fpl" fplCode code expected
