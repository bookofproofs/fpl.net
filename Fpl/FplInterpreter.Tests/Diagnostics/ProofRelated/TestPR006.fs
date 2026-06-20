namespace Diagnostics.ProofRelated

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open FplInterpreter.Main
open TestFplInterpreter.Helpers.Common
open TestSharedConfig


[<TestClass>]
type TestPR006() =

    [<DataRow("00", """thm A {true} proof A$1 {1: trivial}proof T$1 { 1: trivial qed}""", 0)>]
    [<DataRow("01", """thm A {true} proof A$1 {1: trivial}proof T$1 { 1. A$1:2, A$1:3 |- trivial qed}""", 2)>]
    [<DataRow("02", """thm A {true} proof A$1 {1: trivial}proof T$1 { 1: trivial 2. A$1:1 |- trivial qed}""", 0)>]
    [<DataRow("03", """thm A {true} proof A$1 {1: trivial}proof T$1 { 1: trivial 2. A$1:1, A$1:1a |- trivial qed}""", 1)>]
    [<DataRow("04", """thm A {true} proof A$1 {1: trivial}proof T$1 { 1. A$1:2, A$1:1 |- trivial qed}""", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestPR006(no: string, fplCode:string, expected:int) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = PR006 ("", "")
            runTestHelper "TestPR005.fpl" fplCode code expected
