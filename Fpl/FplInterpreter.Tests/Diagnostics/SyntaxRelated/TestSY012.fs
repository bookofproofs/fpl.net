namespace Diagnostics.SyntaxRelated


open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open TestFplInterpreter.Helpers.Common


(* SY012 diagnostic
   Purpose: SY012 highlights a redundant numeric detail in a uniqueness quantifier that can be removed without changing semantics.
   What it indicates: the explicit numeric annotation in the quantifier is unnecessary and the formula can be expressed more simply.
   Benefit: applying the suggestion improves readability and keeps formulas idiomatic by removing superfluous notation.
   Treat SY012 as a non-fatal, readability-oriented hint to simplify quantifier syntax while preserving the original meaning. *)


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

