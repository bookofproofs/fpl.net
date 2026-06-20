namespace Diagnostics.ProofRelated

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open FplInterpreter.Main
open TestFplInterpreter.Helpers.Common
open TestSharedConfig


[<TestClass>]
type TestPR00() =

    [<DataRow("01", """thm T { true } proof T$1 {1: trivial}""", 0)>]
    [<DataRow("02a", """thm T { true } proof T$1 {1: true}""", 0)>]
    [<DataRow("02b", """thm T { true } proof T$1 {1. 2 |- false}""", 1)>]
    [<DataRow("02c", """thm T { true } proof T$1 {1. B |- true}""", 1)>]
    [<DataRow("02d", """thm T { true } proof T$1 {1. bydef S |- false}""", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestPR009(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = PR009 
            runTestHelper "TestPR009.fpl" fplCode code expected
