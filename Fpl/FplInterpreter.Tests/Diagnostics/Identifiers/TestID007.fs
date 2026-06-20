namespace Diagnostics.Identifiers

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open FplInterpreter.Main
open TestFplInterpreter.Helpers.Common
open TestSharedConfig


[<TestClass>]
type TestID007() =

    // inheritance class from class
    [<DataRow("00a", "def cl A def cl T:A", 0)>]
    [<DataRow("00b", "ax A {true} def cl T:A", 1)>]
    [<DataRow("00c", "thm A {true} def cl T:A", 1)>]
    [<DataRow("00d", "lem A {true} def cl T:A", 1)>]
    [<DataRow("00e", "prop A {true} def cl T:A", 1)>]
    [<DataRow("00f", "cor A$1 {true} def cl T:A", 1)>]
    [<DataRow("00g", "proof A$1 {1: true} def cl T:A", 1)>]
    [<DataRow("00h", "inf A {pre:true con:true} def cl T:A", 1)>]
    [<DataRow("00i", "ext A x@/\d+/ -> obj {ret x} def cl T:A", 1)>]
    [<DataRow("00k", "def pred A() {intr} def cl T:A", 1)>]
    [<DataRow("00l", "def func A()->obj {intr} def cl T:A", 1)>]
    [<DataRow("01", "def func T()->obj", 0)>]
    [<DataRow("01a", "def func A()->obj def func T:A()->obj ", 0)>]
    [<DataRow("01b", "def func A(x:obj)->obj def func T:A()->obj ", 1)>]
    [<DataRow("01c", "def func A()->pred def func T:A()->obj ", 1)>]
    // inheritance func from func
    [<DataRow("02a", "def cl A def func T:A()->obj", 1)>]
    [<DataRow("02b", "ax A {true} def func T:A()->obj", 1)>]
    [<DataRow("02c", "thm A {true} def func T:A()->obj", 1)>]
    [<DataRow("02d", "lem A {true} def func T:A()->obj", 1)>]
    [<DataRow("02e", "prop A {true} def func T:A()->obj", 1)>]
    [<DataRow("02f", "cor A$1 {true} def func T:A()->obj", 1)>]
    [<DataRow("02g", "proof A$1 {1: true} def func T:A()->obj", 1)>]
    [<DataRow("02h", "inf A {pre:true con:true} def func T:A()->obj", 1)>]
    [<DataRow("02i", "ext A x@/\d+/ -> obj {ret x} def func T:A()->obj", 1)>]
    [<DataRow("02k", "def pred A() {intr} def func T:A()->obj", 1)>]
    [<DataRow("02l", "def func A(x:obj)->obj {intr} def func T:A()->obj", 1)>]
    // inheritance pred from pred
    [<DataRow("02a", "def cl A def pred T:A()", 1)>]
    [<DataRow("02b", "ax A {true} def pred T:A()", 1)>]
    [<DataRow("02c", "thm A {true} def pred T:A()", 1)>]
    [<DataRow("02d", "lem A {true} def pred T:A()", 1)>]
    [<DataRow("02e", "prop A {true} def pred T:A()", 1)>]
    [<DataRow("02f", "cor A$1 {true} def pred T:A()", 1)>]
    [<DataRow("02g", "proof A$1 {1: true} def pred T:A()", 1)>]
    [<DataRow("02h", "inf A {pre:true con:true} def pred T:A()", 1)>]
    [<DataRow("02i", "ext A x@/\d+/ -> obj {ret x} def pred T:A()", 1)>]
    [<DataRow("02k", "def pred A() {intr} def pred T:A()", 0)>]
    [<DataRow("02l", "def func A(x:obj)->obj {intr} def pred T:A()", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestID007(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID007 ("", "", "", "") 
            runTestHelper "TestID007.fpl" fplCode code expected
