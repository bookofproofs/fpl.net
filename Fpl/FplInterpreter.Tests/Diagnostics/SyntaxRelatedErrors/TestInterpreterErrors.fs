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


    [<DataRow("01", "def cl T {ctor T() {dec base. (); }}", 2, "001.1~001.2")>] 
    [<TestMethod>]
    member this.TestSY002ErrorChain(no:string, fplCode:string, expectedErrors, chain:string) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SY002 ("","")
            let result = runTestHelperWithoutSyntaxCheckingGetResult "TestSY002ErrorChain.fpl" fplCode code expectedErrors
            let firstChain = result.Head.Message.Substring(19, 5)
            let lastChain = (result |> List.rev |> List.head).Message.Substring(19, 5)
            let chainActual = $"{firstChain}~{lastChain}"
            Assert.AreEqual<string>(chain, chainActual)

    // parens top level block
    [<DataRow("00", "def pred T() { true } ", 0)>] 
    [<DataRow("00a", "def pred T() { (true) } ", 1)>] 
    [<DataRow("00a", "def pred T() { x + y } ", 0)>] 
    [<DataRow("00b", "def pred T() { (x + y) } ", 1)>] 
    [<DataRow("00c", "def pred T() { (x * y + x) } ", 1)>] 
    [<DataRow("00d", "def pred T() { (x * (y + x)) } ", 1)>] 
    [<DataRow("00e", "def pred T() { (x * (y) + x) } ", 2)>] 

    // parens inside 
    [<DataRow("01", "def pred T() { x + y } ", 0)>] 
    [<DataRow("01a", "def pred T() { (x) + y } ", 1)>] 
    [<DataRow("01b", "def pred T() { x + (y) } ", 1)>] 
    [<DataRow("01c", "def pred T() { (x) = y } ", 1)>] 
    [<DataRow("01d", "def pred T() { x = (y) } ", 1)>] 
    [<DataRow("01e", "def pred T() { x = (y + x) } ", 0)>] 
    [<DataRow("01f", "def pred T() { x * (y + x) } ", 0)>] 
    [<DataRow("01g", "def pred T() { (x) * (y) + (x) } ", 3)>] 
    [<DataRow("01h", "def pred T() { (x * y) + (x) } ", 1)>]
    // only syntax check is done, precedence is part of interpretation and does not apply here
    // so in the following test case, no SY010 diagnostics is issued even though in standard mathematics
    // * comes before + and parentheses could be omitted.
    [<DataRow("01i", "def pred T() { (x * y) + x } ", 0)>] 
    [<DataRow("01i", "def pred T() { x * (y + x) } ", 0)>] 

    // parens top level in compound predicates
    [<DataRow("02", "def pred T() { not ((x)) } ", 2)>] 
    // parens top level in compound predicates
    [<DataRow("03", "def pred T() { is(x,obj) } ", 0)>] 
    [<DataRow("03a", "def pred T() { is((x),obj) } ", 1)>] 
    [<DataRow("04", "def pred T() { not x } ", 0)>] 
    [<DataRow("04a", "def pred T() { not (x) } ", 1)>] 
    [<DataRow("05", "def pred T() { and (x,y) } ", 0)>] 
    [<DataRow("05a", "def pred T() { and ((x),y) } ", 1)>] 
    [<DataRow("06", "def pred T() { impl(x,y) } ", 0)>] 
    [<DataRow("06a", "def pred T() { impl((x),y) } ", 1)>] 
    [<DataRow("07", "def pred T() { iif(x,y) } ", 0)>] 
    [<DataRow("07a", "def pred T() { iif((x),y) } ", 1)>] 
    [<DataRow("08", "def pred T() { or(x,y) } ", 0)>] 
    [<DataRow("08a", "def pred T() { or((x),y) } ", 1)>] 
    [<DataRow("09", "def pred T() { xor(x,y) } ", 0)>] 
    [<DataRow("09a", "def pred T() { xor((x),y) } ", 1)>] 
    [<DataRow("10", "def pred T() { all x:obj {x} } ", 0)>] 
    [<DataRow("10a", "def pred T() { all x:obj {(x)}  } ", 1)>] 
    [<DataRow("11", "def pred T() { ex x:obj {x} } ", 0)>] 
    [<DataRow("11a", "def pred T() { ex x:obj {(x)}  } ", 1)>] 
    [<DataRow("12", "def pred T() { exn$2 x:obj {x} } ", 0)>] 
    [<DataRow("12a", "def pred T() { exn$2 x:obj {(x)}  } ", 1)>] 

    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestSY010(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SY010
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

