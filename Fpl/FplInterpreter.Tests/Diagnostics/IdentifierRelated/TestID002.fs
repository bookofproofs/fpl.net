namespace Diagnostics.IdentifierRelated

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open FplInterpreter.Main
open TestFplInterpreter.Helpers.Common
open TestSharedConfig


[<TestClass>]
type TestID002() =

    [<DataRow("axiom Test {true} proof Test$1 {1: trivial}", 1)>]
    [<DataRow("postulate Test {true} proof Test$1 {1: trivial}", 1)>]
    [<DataRow("conjecture Test {true} proof Test$1 {1: trivial}", 1)>]
    [<DataRow("def pred Test() {true} proof Test$1 {1: trivial}", 1)>]
    [<DataRow("def cl Test {intr} proof Test$1 {1: trivial}", 1)>]
    [<DataRow("def func Test()->obj {intr} proof Test$1 {1: trivial}", 1)>]
    [<DataRow("proof Test$1 {1: trivial}proof Test$1$1 {1: trivial}", 1)>]
    [<DataRow("theorem Test {true} proof Test$1 {1: trivial}", 0)>]
    [<DataRow("lemma Test {true} proof Test$1 {1: trivial}", 0)>]
    [<DataRow("proposition Test {true} proof Test$1 {1: trivial}", 0)>]
    [<DataRow("corollary Test$1 {true} proof Test$1$1 {1: trivial}", 0)>]
    [<DataRow("inf T { pre: true con: true } proof T$1 {1: trivial}", 1)>]
    [<DataRow("ext T x@/\d+/ -> obj {ret x} proof T$1 {1: trivial}", 1)>]
    [<DataRow("uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestID002(fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID002 ("","")
            runTestHelper "TestID002.fpl" fplCode code expected
