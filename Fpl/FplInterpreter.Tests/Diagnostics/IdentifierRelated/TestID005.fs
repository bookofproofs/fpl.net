namespace Diagnostics.IdentifierRelated

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open FplInterpreter.Main
open TestFplInterpreter.Helpers.Common
open TestSharedConfig


[<TestClass>]
type TestID005() =

    [<DataRow("def pred Test() {true} corollary Test$1 {true}", 1)>]
    [<DataRow("def cl Test {intr} corollary Test$1 {true}", 1)>]
    [<DataRow("def func Test()->obj {intr} corollary Test$1 {true}", 1)>]
    [<DataRow("proof Test$1 {1: trivial}corollary Test$1$1 {true}", 1)>] // corollaries of proofs are not allowed
    [<DataRow("theorem Test {true} corollary Test$1 {true}", 0)>]
    [<DataRow("lemma Test {true} corollary Test$1 {true}", 0)>]
    [<DataRow("proposition Test {true} corollary Test$1 {true}", 0)>]
    [<DataRow("axiom Test {true} corollary Test$1 {true}", 0)>] // corollaries of axioms are allowed
    [<DataRow("postulate Test {true} corollary Test$1 {true}", 0)>] // corollaries of postulates (axioms) not allowed
    [<DataRow("conjecture Test {true} corollary Test$1 {true}", 0)>] // corollaries of conjectures are not allowed
    [<DataRow("corollary Test$1 {true} corollary Test$1$1 {true}", 0)>] // corollaries of corollaries are allowed
    [<DataRow("uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestID005(fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID005 ("","")
            runTestHelper "TestID005.fpl" fplCode code expected
