namespace Diagnostics.ProofRelated

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open FplInterpreter.Main
open TestFplInterpreter.Helpers.Common
open TestSharedConfig


[<TestClass>]
type TestPR017() =

    [<DataRow("01", """thm T { true } proof T$1 {1: trivial}""", 0)>]
    [<DataRow("02", """ax A { true } thm T {true} proof T$1 {1: trivial 2. byax A |- true}""", 1)>] // trivial is not the last one in proof
    [<DataRow("03", """ax A { true } thm T {true} proof T$1 {1. byax A |- true 2: trivial}""", 0)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestPR017(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = PR017 
            runTestHelper "TestPR017.fpl" fplCode code expected
