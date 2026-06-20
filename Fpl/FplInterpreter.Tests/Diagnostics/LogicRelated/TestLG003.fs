namespace Diagnostics.LogicRelated

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open FplInterpreter.Main
open TestFplInterpreter.Helpers.Common
open TestSharedConfig


[<TestClass>]
type TestLG003() =

    [<DataRow("00", """axiom T { true }""", 0)>]
    [<DataRow("01", """axiom T { false }""", 1)>]
    [<DataRow("02", """theorem T { true }""", 0)>]
    [<DataRow("03", """theorem T { false }""", 1)>]
    [<DataRow("04", """proposition T { true }""", 0)>]
    [<DataRow("05", """proposition T { false }""", 1)>]
    [<DataRow("06", """lemma T { true }""", 0)>]
    [<DataRow("07", """lemma T { false }""", 1)>]
    [<DataRow("08", """corollary T$1 { true }""", 0)>]
    [<DataRow("09", """corollary T$1 { false }""", 1)>]
    [<DataRow("10", """conjecture T { true }""", 0)>]
    [<DataRow("11", """conjecture T { false }""", 1)>]
    [<DataRow("12", """postulate T { true }""", 0)>]
    [<DataRow("13", """postulate T { false }""", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestLG003(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = LG003 ("", "")
            runTestHelper "TestLG003.fpl" fplCode code expected
