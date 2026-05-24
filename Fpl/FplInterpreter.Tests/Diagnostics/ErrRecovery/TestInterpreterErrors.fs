namespace FplInterpreter.Tests.Diagnostics.ErrRecovery

open Microsoft.VisualStudio.TestTools.UnitTesting
open ErrDiagnostics
open FplInterpreter.Globals.Debug
open CommonTestHelpers


[<TestClass>]
type TestInterpreterErrors() =


    [<DataRow("00", "def pred T() {true} ", 0)>] 
    [<DataRow("01", "def pred T( {true} ", 1)>] 
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestSY000(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SY000 ""
            runTestHelperWithoutSyntaxChecking "TestSY000.fpl" fplCode code expected

    [<DataRow("00", "def cl T {ctor T() {dec base.T(); }}", 0)>] 
    [<DataRow("01", "def cl T {ctor T() {dec base. (); }}", 2)>] 
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestSY002(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SY002 ("","")
            runTestHelperWithoutSyntaxChecking "TestSY002.fpl" fplCode code expected

    [<DataRow("00", "def pred T() { (1 = x) } ", 0)>] // parser does infix operator, no operand missing
    [<DataRow("01", "def pred T() { (1 = ) } ", 1)>] // parser does infix operator, missing second operand
    [<DataRow("02", "def pred T() { (1 =) } ", 1)>] // parser does infix operator, missing second operand
    [<DataRow("03", "def pred T() { (1+) } ", 0)>] // parser does postfix operator, no infix operation check
    [<DataRow("03", "def pred T() { (1=) } ", 1)>] // parser does infix operator, missing second operand
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestSY010(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SY010 ""
            runTestHelper "TestSY010.fpl" fplCode code expected

    [<DataRow("00", "def pred T() { ∃!0 x:obj{true} } ", 1)>] 
    [<DataRow("01", "def pred T() { ∃!1 x:obj{true} } ", 0)>] 
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestSY011(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SY011
            runTestHelper "TestSY011.fpl" fplCode code expected

    [<DataRow("00", "def pred T() { ∃!1 x:obj{true} } ", 1)>] 
    [<DataRow("01", "def pred T() { ∃!2 x:obj{true} } ", 0)>] 
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestSY012(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SY012
            runTestHelper "TestSY012.fpl" fplCode code expected

