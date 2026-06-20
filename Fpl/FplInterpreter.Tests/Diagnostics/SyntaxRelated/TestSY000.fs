namespace FplInterpreter.Tests.Diagnostics.SyntaxRelated

open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open TestFplInterpreter.Helpers.Common

(*
The SY000 diagnostic indicates a syntax errors issued by FplParser, if the error does not involve
the backtracking mechanism of the FParsec library, the FplParser is based on.
For the backtracking syntax errors see SY001 and SY002.
*)

[<TestClass>]
type TestSY000() =


    [<DataRow("00", "def pred T() {true} ", 0)>] 
    [<DataRow("01", "def pred T( {true} ", 1)>] 
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestSY000(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SY000 ""
            runTestHelperWithoutSyntaxChecking "TestSY000.fpl" fplCode code expected

