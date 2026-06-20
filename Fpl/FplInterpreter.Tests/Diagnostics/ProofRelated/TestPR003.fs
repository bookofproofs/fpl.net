namespace Diagnostics.ProofRelated

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open FplInterpreter.Main
open TestFplInterpreter.Helpers.Common
open TestSharedConfig


[<TestClass>]
type TestPR003() =

    [<DataRow("""proof T$1 { 100: assume and(x,y) 300: trivial 100: trivial qed}""", 1)>]
    [<DataRow("""proof T$1 { 1: trivial 1: trivial qed}""", 1)>]
    [<DataRow("""proof T$1 { 1: trivial 2: trivial qed}""", 0)>]
    [<DataRow("uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestPR003(fplCode:string, expected:int) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = PR003 ("", "")
            runTestHelper "TestPR003.fpl" fplCode code expected
