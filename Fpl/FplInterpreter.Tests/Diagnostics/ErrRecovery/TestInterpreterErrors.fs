namespace FplInterpreter.Tests.Diagnostics.ErrRecovery

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open ErrDiagnostics
open FplInterpreter.Globals.Debug
open FplInterpreter.Main
open CommonTestHelpers
open TestSharedConfig

[<TestClass>]
type TestInterpreterErrors() =

 
    [<DataRow("00", "def pred T() { (1 = x) } ", 0)>] // parser does infix operator, no operand missing
    [<DataRow("01", "def pred T() { (1 = ) } ", 1)>] // parser does infix operator, missing second operand
    [<DataRow("02", "def pred T() { (1 =) } ", 1)>] // parser does infix operator, missing second operand
    [<DataRow("03", "def pred T() { (1+) } ", 0)>] // parser does postfix operator, no infix operation check
    [<DataRow("03", "def pred T() { (1=) } ", 1)>] // parser does infix operator, missing second operand
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestSY000(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SY000 ""
            runTestHelper "TestSY000.fpl" fplCode code expected

    [<DataRow("00", "def pred T() { ∃!0 x:obj{true} } ", 1)>] 
    [<DataRow("01", "def pred T() { ∃!1 x:obj{true} } ", 0)>] 
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestSY001(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SY001 
            runTestHelper "TestSY001.fpl" fplCode code expected

    [<DataRow("00", "def pred T() { ∃!1 x:obj{true} } ", 1)>] 
    [<DataRow("01", "def pred T() { ∃!2 x:obj{true} } ", 0)>] 
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestSY002(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SY002
            runTestHelper "TestSY002.fpl" fplCode code expected

    [<DataRow("pred00", """def pred T()""", 0)>]
    [<DataRow("pred01", """def pred T)""", 1)>]
    [<DataRow("func00", """def func T()->obj """, 0)>]
    [<DataRow("func01", """def func T)->obj """, 1)>]
    [<DataRow("ctor00", """def cl S def cl T {ctor T() {dec base.S(); }}""", 0)>]
    [<DataRow("ctor01", """def cl S def cl T {ctor T) {dec base.S(); }}""", 1)>]
    [<DataRow("propPred00", """def cl S def cl T {intr prty pred T() }""", 0)>]
    [<DataRow("propPred01", """def cl S def cl T {intr prty pred T) }""", 1)>]
    [<DataRow("propFunc00", """def cl S def cl T {intr prty func T()->obj }""", 0)>]
    [<DataRow("propFunc01", """def cl S def cl T {intr prty func T)->obj }""", 1)>]
    [<DataRow("del00", """def pred T() {del.T()}""", 0)>]
    [<DataRow("del01", """def pred T() {del.T)}""", 1)>]
    [<DataRow("base00", """def cl S def cl T {ctor T() {dec base.T(); }}""", 0)>]
    [<DataRow("base01", """def cl S def cl T {ctor T() {dec base.T); }}""", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestSY006(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SY006
            runTestHelper "TestSY007.fpl" fplCode code expected


    [<DataRow("pred00", """def pred T()""", 0)>]
    [<DataRow("pred01", """def pred T(""", 1)>]
    [<DataRow("func00", """def func T()->obj """, 0)>]
    [<DataRow("func01", """def func T(->obj """, 1)>]
    [<DataRow("ctor00", """def cl S def cl T {ctor T() {dec base.S(); }}""", 0)>]
    [<DataRow("ctor01", """def cl S def cl T {ctor T( {dec base.S(); }}""", 1)>]
    [<DataRow("propPred00", """def cl S def cl T {intr prty pred T() }""", 0)>]
    [<DataRow("propPred01", """def cl S def cl T {intr prty pred T( }""", 1)>]
    [<DataRow("propFunc00", """def cl S def cl T {intr prty func T()->obj }""", 0)>]
    [<DataRow("propFunc01", """def cl S def cl T {intr prty func T(->obj }""", 1)>]
    [<DataRow("del00", """def pred T() {del.T()}""", 0)>]
    [<DataRow("del01", """def pred T() {del.T(}""", 1)>]
    [<DataRow("base00", """def cl S def cl T {ctor T() {dec base.T(); }}""", 0)>]
    [<DataRow("base01", """def cl S def cl T {ctor T() {dec base.T(; }}""", 1)>]
    [<DataRow("ref00", """def pred T() {S()}""", 0)>]
    [<DataRow("ref01", """def pred T() {S(}""", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestSY007(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SY007
            runTestHelper "TestSY007.fpl" fplCode code expected

    [<DataRow("del00", """def pred T() {del.T()}""", 0)>]
    [<DataRow("del01", """def pred T() {del T()}""", 1)>]
    [<DataRow("base00", """def cl S def cl T {ctor T() {dec base.T(); }}""", 0)>]
    [<DataRow("base01", """def cl S def cl T {ctor T() {dec base T(); }}""", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestSY008(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SY008
            runTestHelper "TestSY008.fpl" fplCode code expected

    [<DataRow("endOfFile01", """""", 0)>]
    [<DataRow("loc01", """loc not (x) := !tex: "\neg(" x ")" !eng: "not " x !ger: "nicht " x """, 1)>]
    [<DataRow("loc02", """loc not (x) := !tex: "\neg(" x ")" !eng: "not " x !ger: "nicht " x ;def cl A""", 0)>]
    [<DataRow("dec01", """def pred T() {dec x:obj; true}""", 0)>]
    [<DataRow("dec02", """def pred T() {dec x:obj true}""", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestSY009(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SY009
            runTestHelper "TestSY009.fpl" fplCode code expected


    [<DataRow("arrType00", """def pred T(a:*ind[obj])""", 0)>]
    [<DataRow("arrType01", """def pred T(a:*ind obj])""", 1)>]
    [<DataRow("arrType02", """def pred T(a:*ind obj)""", 1)>]
    [<DataRow("arrType03", """def pred T() {dec a:*ind[obj]; true}""", 0)>]
    [<DataRow("arrType04", """def pred T() {dec a:*ind obj]; true}""", 1)>]
    [<DataRow("arrType05", """def pred T() {dec a:*ind obj; true}""", 1)>]
    [<TestMethod>]
    member this.TestSY010(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SY010
            runTestHelper "TestSY010.fpl" fplCode code expected

    [<DataRow("arrType00", """def pred T(a:*ind[obj])""", 0)>]
    [<DataRow("arrType01", """def pred T(a:*ind[obj)""", 1)>]
    [<DataRow("arrType02", """def pred T(a:*ind obj)""", 1)>]
    [<DataRow("arrType03", """def pred T() {dec a:*ind[obj]; true}""", 0)>]
    [<DataRow("arrType04", """def pred T() {dec a:*ind[obj; true}""", 1)>]
    [<DataRow("arrType05", """def pred T() {dec a:*ind obj; true}""", 1)>]
    [<DataRow("arrUsage00", """def pred T() {dec a:=x[b]; true}""", 0)>]
    [<DataRow("arrUsage01", """def pred T() {dec a:=x[b; true}""", 1)>]
    [<TestMethod>]
    member this.TestSY011(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SY011
            runTestHelper "TestSY011.fpl" fplCode code expected
