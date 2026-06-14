namespace FplInterpreter.Tests.Diagnostics.SyntaxRelated

open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open CommonTestHelpers


(*
The SY013 diagnostic indicates superfluous parentheses enclosing an infix operation with some
symbol, e.g. "*", can be safely removed without changing the semantics of the FPL code,
because another infix operation in the same infix sequence uses another symbol, e.g. "+",
and the reason for the safe removal is that the precedence of the first symbol is higher than
the precedence of the second symbol.

Both symbols have to define different precedences.

For non-precedence-based parenthesis diagnostics: see SY010 diagnostic.
*)

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

