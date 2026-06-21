namespace Diagnostics.VariableRelated

open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open TestFplInterpreter.Helpers.Common

(* VAR01 diagnostic
   Purpose: VAR01 flags references to identifiers that have no binding in the current scope.
   What it indicates: an identifier used in an expression cannot be resolved to any declared variable, so evaluation will rely on a missing or implicit binding.
   Use: follow VAR01 to find missing declarations, incorrect imports/uses, or simple typos in variable names.
   Action: fix by declaring the identifier in the appropriate scope, adjusting the surrounding scope, or correcting the name so it resolves unambiguously.
   Treat VAR01 as a name-resolution error that prevents the code from being correctly evaluated until resolved. *)

[<TestClass>]
type TestVAR01() =

    [<DataRow("00", "def pred Test() {x}", 1)>]
    [<DataRow("01", "inf ExistsByExample {dec p: pred(c: obj); pre: p(c) con: ex x:obj {p(x)}}", 0)>]
    [<DataRow("02", "axiom A { all x:Nat {true} }", 0)>]
    [<DataRow("03", "axiom A { all x:obj {y} }", 1)>]
    [<DataRow("04", "axiom A { dec x:obj; true }", 0)>]
    [<DataRow("05", "axiom A { dec x:obj; true }", 0)>]
    [<DataRow("06", """loc and(p,q) := !tex: p "\wedge" q;""", 0)>]
    [<DataRow("07", """loc and(p,q) := !tex: x "\wedge" q;""", 1)>]
    [<DataRow("08", """loc and(p,q) := !tex: x "\wedge" y;""", 2)>]
    [<DataRow("09", """def pred Add(x,y: obj) infix "+" 2 {intr} loc (x + y) := !tex: x "+" y !eng: x "plus" y !ger: x "plus" y;""", 0)>]
    [<DataRow("10", """def pred Add(x,y: obj) infix "+" 2 {intr} axiom A {(x + y * z = 1)}""", 3)>]
    [<DataRow("11", "axiom A {dec arr: tpl; x }", 1)>]
    [<DataRow("12", "prop A {dec d:pred; true} proof A$1 {1: d qed}", 0)>]
    [<DataRow("13", "prop A {dec d:pred; true} cor A$1 { d }", 0)>]
    [<DataRow("14", "def class A {ctor A(x: obj, p:pred(u: pred)) {dec assert u;  }}", 0)>]
    [<DataRow("15", "ext D x@/\d+/ -> pred { ret (x = @1) }", 0)>]
    [<DataRow("16", "ext D x@/\d+/ -> pred { dec y:obj; ret (x = y) }", 0)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestVAR01(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = VAR01 ""
            runTestHelper "TestVAR01.fpl" fplCode code expected
