namespace FplInterpreter.Tests.Diagnostics.SyntaxRelated

open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open FplInterpreter.Globals.Debug
open CommonTestHelpers

(*
The SY012 diagnostic indicates superfluous number 1 given in the exists quantor that can
be safely removed without changing the semantics of the FPL code. ("exists only one")

*)

[<TestClass>]
type TestSY012() =


    [<DataRow("00", "def pred T() { ∃!1 x:obj{true} } ", 1)>] 
    [<DataRow("01", "def pred T() { ∃!2 x:obj{true} } ", 0)>] 
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestSY012(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SY012
            runTestHelper "TestSY012.fpl" fplCode code expected

