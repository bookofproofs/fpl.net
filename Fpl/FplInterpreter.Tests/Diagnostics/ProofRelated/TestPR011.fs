namespace Diagnostics.ProofRelated

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open FplInterpreter.Main
open TestFplInterpreter.Helpers.Common
open TestSharedConfig


[<TestClass>]
type TestPR011() =

    [<DataRow("00", "proof T$1 {1. byax A |- trivial}", 0)>]
    [<DataRow("01", "proof T$1 {1. byax A$1:3 |- trivial}", 1)>]
    [<DataRow("00", "proof T$1 {1. byconj A |- trivial}", 0)>]
    [<DataRow("01", "proof T$1 {1. byconj A$1:3 |- trivial}", 1)>]
    [<DataRow("04", "proof T$1 {1. bydef A |- trivial}", 0)>]
    [<DataRow("05", "proof T$1 {1. bydef A$1:3 |- trivial}", 1)>]
    [<DataRow("02", "proof T$1 {1. byinf A |- trivial}", 0)>]
    [<DataRow("03", "proof T$1 {1. byinf A$1:3 |- trivial}", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestPR011(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = PR011 ("", "")
            runTestHelper "TestPR011.fpl" fplCode code expected
