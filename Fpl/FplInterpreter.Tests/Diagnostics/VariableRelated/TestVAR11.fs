namespace Diagnostics.VariableRelated

open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open TestFplInterpreter.Helpers.Common

(* VAR11
   Purpose: Detect repeated use of the same free variable inside a localization formula.
   What it indicates: A variable is used twice between `loc` and `:=` without being bound by a quantifier.
   Use: Help find duplicate free-variable usage in localization blocks so translation formulas stay unambiguous.
   Action / Treat: Rename one of the repeated free-variable occurrences or bind it explicitly; treat VAR11 as an error that must be fixed. *)


[<TestClass>]
type TestVAR11() =

    [<DataRow("01", """loc and(p,q) := !tex: p "\wedge" q;""", 0)>]
    [<DataRow("02", """loc and(q,q) := !tex: p "\wedge" q;""", 1)>]
    [<DataRow("03", """loc ex q:pred { and(q,q) } := !tex: p "\wedge" q;""", 0)>]
    [<DataRow("04", """loc ex q:pred { q } := !tex: q;""", 0)>]
    [<DataRow("05", """loc and(x, ex x:obj {x}) := !tex: x;""", 0)>]
    [<DataRow("06", """loc or(x, ex x:obj {x}) := !tex: x;""", 0)>]
    [<DataRow("07", """loc impl(x, all x:obj {x}) := !tex: x;""", 0)>]
    [<DataRow("08", """loc iif(x, ex x:obj {x}) := !tex: x;""", 0)>]
    [<DataRow("09", """loc xor(x, ex x:obj {x}) := !tex: x;""", 0)>]
    [<DataRow("10", """loc not and(x, ex x:obj {x}) := !tex: x;""", 0)>]
    [<DataRow("11", """loc and(ex x:obj {x}, x) := !tex: x;""", 0)>]
    [<DataRow("12", """loc or(ex x:obj {x}, x) := !tex: x;""", 0)>]
    [<DataRow("13", """loc impl(ex x:obj {x}, x) := !tex: x;""", 0)>]
    [<DataRow("14", """loc iif(ex x:obj {x}, x) := !tex: x;""", 0)>]
    [<DataRow("15", """loc and(x, ex y:obj {x}) := !tex: x;""", 1)>]
    [<DataRow("16", """loc or(x, ex y:obj {and(x,y)}) := !tex: x;""", 1)>]
    [<DataRow("17", """loc impl(x, all y:obj {x}) := !tex: x;""", 1)>]
    [<DataRow("18", """loc iif(x, ex y:obj {and(y,x)}) := !tex: x;""", 1)>]
    [<DataRow("19", """loc xor(x, ex y:obj {x}) := !tex: x;""", 1)>]
    [<DataRow("20", """loc not and(x, ex y:obj {x}) := !tex: x;""", 1)>]
    [<DataRow("21", """loc and(ex y:obj {x}, x) := !tex: x;""", 1)>]
    [<DataRow("22", """loc or(ex y:obj {x}, x) := !tex: x;""", 1)>]
    [<DataRow("23", """loc impl(ex y:obj {x}, x) := !tex: x;""", 1)>]
    [<DataRow("24", """loc iif(ex y:obj {x}, x) := !tex: x;""", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestVAR11(no: string, fplCode: string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then
            ()
        else
            let code = VAR11 ("", "")
            runTestHelper "TestVAR11.fpl" fplCode code expected
