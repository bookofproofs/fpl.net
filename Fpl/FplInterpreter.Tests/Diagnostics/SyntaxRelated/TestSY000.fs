namespace Diagnostics.SyntaxRelated

open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open TestFplInterpreter.Helpers.Common

(* SY000
   Purpose: Report a primary syntax error produced by the parser when the failure is not related to FParsec backtracking.
   What it indicates: The input does not conform to the grammar at the reported location and the parser could not recover with alternative branches.
   Use: Inspect the provided error message and position to correct the offending token or structure so it matches the language grammar.
   Action / Treat: Fix the syntax (token, punctuation, or structure); treat SY000 as a blocking parser error that must be resolved for successful interpretation. *)

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

