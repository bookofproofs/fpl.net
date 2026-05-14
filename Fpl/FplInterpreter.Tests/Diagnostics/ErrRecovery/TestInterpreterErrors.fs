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

 
    [<DataRow("00", "def pred T() { (1 = x) } ;", 0)>] // parser does infix operator, no operand missing
    [<DataRow("01", "def pred T() { (1 = ) } ;", 1)>] // parser does infix operator, missing second operand
    [<DataRow("02", "def pred T() { (1 =) } ;", 1)>] // parser does infix operator, missing second operand
    [<DataRow("03", "def pred T() { (1+) } ;", 0)>] // parser does postfix operator, no infix operation check
    [<DataRow("03", "def pred T() { (1=) } ;", 1)>] // parser does infix operator, missing second operand
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestSY000(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SY000 ""
            runTestHelper "TestSY000.fpl" fplCode code expected

    [<DataRow("00", "def pred T() { ∃!0 x:obj{true} } ;", 1)>] 
    [<DataRow("01", "def pred T() { ∃!1 x:obj{true} } ;", 0)>] 
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestSY001(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SY001 
            runTestHelper "TestSY001.fpl" fplCode code expected

    [<DataRow("00", "def pred T() { ∃!1 x:obj{true} } ;", 1)>] 
    [<DataRow("01", "def pred T() { ∃!2 x:obj{true} } ;", 0)>] 
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestSY002(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SY002
            runTestHelper "TestSY002.fpl" fplCode code expected

    [<DataRow("all00", "ax T { ∀ x:obj {true} } ;", 0)>]
    [<DataRow("all01", "ax T { ∀ x:obj true} } ;", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestSY003(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SY003
            runTestHelper "TestSY003.fpl" fplCode code expected

    [<DataRow("all00", "ax T { ∀ x:obj {true} } ;", 0)>]
    [<DataRow("all01", "ax T { ∀ x:obj {true } ;", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestSY004(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SY004
            runTestHelper "TestSY004.fpl" fplCode code expected
