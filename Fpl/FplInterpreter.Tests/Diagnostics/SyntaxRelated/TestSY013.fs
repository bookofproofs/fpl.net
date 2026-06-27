namespace Diagnostics.SyntaxRelated


open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open TestFplInterpreter.Helpers.Common


(* SY013 diagnostic
   Purpose: SY013 warns when parentheses around an infix sub-expression are unnecessary because the inner operator has strictly higher precedence than the surrounding operator.
   What it indicates: the enclosed parentheses can be removed without changing the expression's meaning, so the code can be simplified.
   This diagnostic is a readability suggestion that helps reduce visual clutter and produce more idiomatic infix expressions.
   Treat SY013 as a non-fatal warning recommending removal of redundant grouping to make operator precedence explicit and code easier to read. *)


[<TestClass>]
type TestSY013() =

    
    // (tests for false positives)
    // precedence defined
    [<DataRow("00", """def pred T(x,y:obj) infix "+" 1 {true} def pred S(x,y:obj) infix "*" 2 {true} def pred T() { (x + y) * z } """, 0)>]  // * has higher precedence, parens are ok, because they cannot be safely removed
    [<DataRow("00a", """def pred T(x,y:obj) infix "+" 1 {true} def pred S(x,y:obj) infix "*" 2 {true} def pred T() { z * (x + y) } """, 0)>]  // * has higher precedence, parens are ok, because they cannot be safely removed

    // (tests for positives)
    // precedence defined
    [<DataRow("01", """def pred T(x,y:obj) infix "+" 1 {true} def pred S(x,y:obj) infix "*" 2 {true} def pred T() { x + (y * z) } """, 1)>]  // * has higher precedence, parens can be safely removed
    [<DataRow("01a", """def pred T(x,y:obj) infix "+" 1 {true} def pred S(x,y:obj) infix "*" 2 {true} def pred T() { (y * z) + x } """, 1)>]  // * has higher precedence, parens can be safely removed

    // (tests for false positives)
    // no precedence defined
    [<DataRow("02", """def pred T() { (x + y) * z } """, 0)>] 
    [<DataRow("02a", """def pred T() { z * (x + y) } """, 0)>] 
    [<DataRow("03", """def pred T() { x + (y * z) } """, 0)>] 
    [<DataRow("03a", """def pred T() { (y * z) + x } """, 0)>] 
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestSY013(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SY013 ("", 0, "", 0)
            runTestHelper "TestSY013.fpl" fplCode code expected

