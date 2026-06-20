namespace Diagnostics.ProofRelated

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open FplInterpreter.Main
open TestFplInterpreter.Helpers.Common
open TestSharedConfig


[<TestClass>]
type TestPR016() =

    [<DataRow("00", "proof T$1 {1: assume x 2: trivial 3: revoke 2}", 0)>]
    [<DataRow("01", "proof T$1 {1: assume x 2: trivial 3: assume y 4: trivial 5: revoke 1}", 1)>]
    [<DataRow("02", "proof T$1 {1: assume x 2: trivial 3: assume y 4: trivial 5: revoke 3}", 0)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestPR016(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = PR016 ""
            runTestHelper "TestPR016.fpl" fplCode code expected
