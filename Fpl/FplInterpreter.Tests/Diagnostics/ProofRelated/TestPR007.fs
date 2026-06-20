namespace Diagnostics.ProofRelated

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open FplInterpreter.Main
open TestFplInterpreter.Helpers.Common
open TestSharedConfig


[<TestClass>]
type TestPR007() =

    [<DataRow("01", """theorem T { true } proof T$1 {1: trivial}""", 0)>]
    [<DataRow("01a", """theorem T { true }""", 1)>]
    [<DataRow("02", """proposition T { true } proof T$1 {1: trivial}""", 0)>]
    [<DataRow("02a", """proposition T { true }""", 1)>]
    [<DataRow("03", """lemma T { true } proof T$1 {1: trivial}""", 0)>]
    [<DataRow("03a", """lemma T { true }""", 1)>]
    [<DataRow("04", """corollary T$1 { true } proof T$1$1 {1: trivial}""", 0)>]
    [<DataRow("04a", """corollary T$1 { true }""", 1)>]
    [<DataRow("04b", """corollary T$1 { true } proof T$1$1 {1. bydef A |- trivial}""", 0)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestPR007(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = PR007 ("", "") 
            runTestHelper "TestPR007.fpl" fplCode code expected
