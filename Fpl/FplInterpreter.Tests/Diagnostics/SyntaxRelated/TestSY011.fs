namespace FplInterpreter.Tests.Diagnostics.SyntaxRelated

open Microsoft.VisualStudio.TestTools.UnitTesting
open ErrDiagnostics
open FplInterpreter.Globals.Debug
open CommonTestHelpers

(*
The SY011 diagnostic indicates that the quantor "exists exactly 0" should be replaced by
"not exists" to ensure the correct interpretation of the FPL code.

*)

[<TestClass>]
type TestSY011() =

    [<DataRow("00", "def pred T() { ∃!0 x:obj{true} } ", 1)>] 
    [<DataRow("01", "def pred T() { ¬∃ x:obj{true} } ", 0)>] 
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestSY011(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SY011
            runTestHelper "TestSY011.fpl" fplCode code expected

