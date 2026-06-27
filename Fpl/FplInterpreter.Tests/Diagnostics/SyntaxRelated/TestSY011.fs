namespace Diagnostics.SyntaxRelated

open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open TestFplInterpreter.Helpers.Common

(* SY011
   Purpose: Warn when the "exists exactly 0" quantifier (`∃!0`) is used and suggest the clearer `¬∃` form.
   What it indicates: The quantifier `∃!0` is redundant/unnatural and the expression is better written as a negated existence (`¬∃`) for correct and idiomatic interpretation.
   Use: Help authors locate occurrences of `∃!0` so they can replace them with `¬∃` and avoid confusion about intended meaning.
   Action / Treat: Replace `∃!0` with `¬∃` to express "there does not exist"; treat SY011 as a non-fatal warning recommending a syntactic clarification. *)

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

