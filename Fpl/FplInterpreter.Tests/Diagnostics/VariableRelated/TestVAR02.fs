namespace Diagnostics.VariableRelated

open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open TestFplInterpreter.Helpers.Common

(* VAR02
   Purpose: Detect when a quantifier binds the same identifier more than once.
   What it indicates: A quantifier declaration contains duplicate identifiers, producing ambiguous or invalid bindings.
   Use: Help locate and correct duplicate names inside quantifier headers so each bound name is distinct.
   Action / Treat: Rename or split the bindings so the quantifier binds unique identifiers; treat VAR02 as a correctness error that must be resolved. *)

[<TestClass>]
type TestVAR02() =

    [<DataRow("00", "axiom T {all n:pred { n } }", 0)>]
    [<DataRow("00a", "axiom T {all n, n:pred { n } }", 1)>]
    [<DataRow("00b", "axiom T {all n:pred, n:pred { n } }", 1)>]
    [<DataRow("01", "axiom T {ex n:pred { n } }", 0)>]
    [<DataRow("01a", "axiom T {ex n, n:pred { n } }", 1)>]
    [<DataRow("01b", "axiom T {ex n:pred, n:pred { n } }", 1)>]
    [<DataRow("02", "axiom T {exn$1 n:pred { n } }", 0)>]
    [<DataRow("02a", "axiom T {exn$1 n, n:pred { n } }", 1)>]
    [<DataRow("02a", "axiom T {exn$1 n:pred, n:pred { n } }", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestVAR02(no: string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = VAR02 ""
            runTestHelper "TestVAR02.fpl" fplCode code expected
