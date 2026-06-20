namespace Diagnostics.SignatureRelated

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open FplInterpreter.Main
open TestFplInterpreter.Helpers.Common
open TestSharedConfig


[<TestClass>]
type TestSIG06() =

    [<DataRow("00a", "def cl A { intr prty pred T() {intr} } def cl B { intr prty pred T() {intr} } def cl C:A,B ", 1)>]
    [<DataRow("00b", "def cl A { intr prty pred T() {intr} } def cl B { intr prty pred S() {intr} } def cl C:A,B ", 0)>]
    [<DataRow("00c", "def cl A { intr prty pred T() {intr} } def cl B { intr prty pred T(x:func) {intr} } def cl C:A,B ", 0)>]
    [<DataRow("01a", "def cl A { intr prty func T()->obj {intr} } def cl B { intr prty func T()->obj {intr} } def cl C:A,B ", 1)>]
    [<DataRow("01b", "def cl A { intr prty func T()->obj {intr} } def cl B { intr prty pred T() {intr} } def cl C:A,B ", 0)>]
    [<DataRow("01c", "def cl A { intr prty func T()->obj {intr} } def cl B { intr prty func T(x:ind)->obj {intr} } def cl C:A,B ", 0)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestSIG06Classes(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SIG06 ("","","","")
            runTestHelper "TestSIG06Classes.fpl" fplCode code expected

    [<DataRow("00a", "def func A()->obj { intr prty pred T() {intr} } def func B()->obj { intr prty pred T() {intr} } def func C:A,B()->obj ", 1)>]
    [<DataRow("00b", "def func A()->obj { intr prty pred T() {intr} } def func B()->obj { intr prty pred S() {intr} } def func C:A,B()->obj ", 0)>]
    [<DataRow("00c", "def func A()->obj { intr prty pred T() {intr} } def func B()->obj { intr prty pred T(x:func) {intr} } def func C:A,B()->obj ", 0)>]
    [<DataRow("01a", "def func A()->obj { intr prty func T()->obj {intr} } def func B()->obj { intr prty func T()->obj {intr} } def func C:A,B()->obj ", 1)>]
    [<DataRow("01b", "def func A()->obj { intr prty func T()->obj {intr} } def func B()->obj { intr prty pred T() {intr} } def func C:A,B()->obj ", 0)>]
    [<DataRow("01c", "def func A()->obj { intr prty func T()->obj {intr} } def func B()->obj { intr prty func T(x:ind)->obj {intr} } def func C:A,B()->obj ", 0)>]
    [<DataRow("02a", "def func A()->obj { intr prty pred T() {intr} } def func B:A()->obj { intr prty pred T() {intr} } def func C:A()->obj ", 1)>]
    [<DataRow("02b", "def func A()->obj { intr prty pred T() {intr} } def func B:A()->obj { intr prty pred T() {intr} } def func C:B()->obj { intr prty pred T() {intr} } ", 2)>]
    [<DataRow("03a", "def func A()->obj { intr prty func T()->obj {intr} } def func B()->obj { intr prty func T()->obj {intr} } def func C:A,B()->obj { intr prty func T()->obj {intr} } ", 2)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestSIG06FunctionalTerms(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SIG06 ("","","","")
            runTestHelper "TestSIG06FunctionalTerms.fpl" fplCode code expected
