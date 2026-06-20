namespace FplInterpreter.Tests.Diagnostics.SyntaxRelated

open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open TestFplInterpreter.Helpers.Common


(*
The SY014 diagnostic indicates that two infix operations have the same precedence and the
expression might be ambiguous because either no parentheses are used or the assigned precedences are not deferent.
*)

[<TestClass>]
type TestSY014() =

    // (tests for false positives)
    // deferent precedences defined and even parentheses used 
    [<DataRow("00", """def pred T(x,y:obj) infix "+" 1 {true} def pred S(x,y:obj) infix "*" 2 {true} def pred T() { (x + y) * z } """, 0)>]  
    [<DataRow("00a", """def pred T(x,y:obj) infix "+" 1 {true} def pred S(x,y:obj) infix "*" 2 {true} def pred T() { z * (x + y) } """, 0)>]  
    [<DataRow("01", """def pred T(x,y:obj) infix "+" 1 {true} def pred S(x,y:obj) infix "*" 2 {true} def pred T() { x + (y * z) } """, 0)>]  
    [<DataRow("01a", """def pred T(x,y:obj) infix "+" 1 {true} def pred S(x,y:obj) infix "*" 2 {true} def pred T() { (y * z) + x } """, 0)>]  

    // (tests for false positives)
    // deferent precedences defined and no parentheses used 
    [<DataRow("02", """def pred T(x,y:obj) infix "+" 1 {true} def pred S(x,y:obj) infix "*" 2 {true} def pred T() { x + y * z } """, 0)>]  
    [<DataRow("02a", """def pred T(x,y:obj) infix "+" 1 {true} def pred S(x,y:obj) infix "*" 2 {true} def pred T() { z * x + y } """, 0)>]  
    [<DataRow("03", """def pred T(x,y:obj) infix "+" 1 {true} def pred S(x,y:obj) infix "*" 2 {true} def pred T() { x + y * z } """, 0)>]  
    [<DataRow("03a", """def pred T(x,y:obj) infix "+" 1 {true} def pred S(x,y:obj) infix "*" 2 {true} def pred T() { y * z + x } """, 0)>]  

    // (tests for false positives)
    // conflicting precedences defined, but parentheses used 
    [<DataRow("04", """def pred T(x,y:obj) infix "+" 1 {true} def pred S(x,y:obj) infix "*" 1 {true} def pred T() { (x + y) * z } """, 0)>]  
    [<DataRow("04a", """def pred T(x,y:obj) infix "+" 1 {true} def pred S(x,y:obj) infix "*" 1 {true} def pred T() { z * (x + y) } """, 0)>]  
    [<DataRow("05", """def pred T(x,y:obj) infix "+" 1 {true} def pred S(x,y:obj) infix "*" 1 {true} def pred T() { x + (y * z) } """, 0)>]  
    [<DataRow("05a", """def pred T(x,y:obj) infix "+" 1 {true} def pred S(x,y:obj) infix "*" 1 {true} def pred T() { (y * z) + x } """, 0)>]  

    // (tests for positives)
    // conflicting precedences defined and no parentheses used 
    [<DataRow("06", """def pred T(x,y:obj) infix "+" 1 {true} def pred S(x,y:obj) infix "*" 1 {true} def pred T() { x + y * z } """, 1)>]  
    [<DataRow("06a", """def pred T(x,y:obj) infix "+" 1 {true} def pred S(x,y:obj) infix "*" 1 {true} def pred T() { z * x + y } """, 1)>]  
    [<DataRow("07", """def pred T(x,y:obj) infix "+" 1 {true} def pred S(x,y:obj) infix "*" 1 {true} def pred T() { x + y * z } """, 1)>]  
    [<DataRow("07a", """def pred T(x,y:obj) infix "+" 1 {true} def pred S(x,y:obj) infix "*" 1 {true} def pred T() { y * z + x } """, 1)>]  

    // (tests for false positives)
    // no precedence defined but parentheses used
    [<DataRow("08", """def pred T() { (x + y) * z } """, 0)>] 
    [<DataRow("08a", """def pred T() { z * (x + y) } """, 0)>] 
    [<DataRow("09", """def pred T() { x + (y * z) } """, 0)>] 
    [<DataRow("09a", """def pred T() { (y * z) + x } """, 0)>] 

    // (tests for positives)
    // no precedence defined and no parentheses used
    [<DataRow("10", """def pred T() { x + y * z } """, 1)>] 
    [<DataRow("10a", """def pred T() { z * x + y } """, 1)>] 
    [<DataRow("11", """def pred T() { x + y * z } """, 1)>] 
    [<DataRow("11a", """def pred T() { y * z + x } """, 1)>] 

    // (tests for false positives)
    // edge cases - only one infix operation involved
    [<DataRow("12", """def pred T() { (x + y) } """, 0)>] 
    [<DataRow("12a", """def pred T() { x + y } """, 0)>] 
    [<DataRow("13", """def pred T(x,y:obj) infix "+" 1 {true} def pred T() { (x + y) } """, 0)>]  
    [<DataRow("13a", """def pred T(x,y:obj) infix "+" 1 {true} def pred T() { x + y } """, 0)>]  
    // edge cases - the same infix operation involved twice
    [<DataRow("12", """def pred T() { (x + y + z) } """, 0)>] 
    [<DataRow("12a", """def pred T() { x + y + z } """, 0)>] 
    [<DataRow("13", """def pred T(x,y:obj) infix "+" 1 {true} def pred T() { (x + y + z) } """, 0)>]  
    [<DataRow("13a", """def pred T(x,y:obj) infix "+" 1 {true} def pred T() { x + y + z } """, 0)>]  
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]

    [<TestMethod>]
    member this.TestSY014(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SY014 ("", "", 0)
            runTestHelper "TestSY014.fpl" fplCode code expected

