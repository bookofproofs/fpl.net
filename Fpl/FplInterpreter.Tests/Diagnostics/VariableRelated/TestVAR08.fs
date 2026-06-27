namespace Diagnostics.VariableRelated

open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open TestFplInterpreter.Helpers.Common

(* VAR08
   Purpose: Warn when a variadic (array-like) variable is attempted to be bound by a quantifier.
   What it indicates: Quantifiers cannot correctly bind variadic/array variables and the binding is unsupported or ambiguous.
   Use: Point authors to incorrect quantification of variadic variables so the expression can be restructured.
   Action / Treat: Replace the variadic variable with a non-variadic form, change the quantification strategy, or make the intended binding explicit; treat VAR08 as an error requiring correction. *)

[<TestClass>]
type TestVAR08() =

    [<DataRow("00", "axiom T {all n:pred { n } }", 0)>]
    [<DataRow("00a", "axiom T {all n:*pred[ind] { n } }", 1)>]
    [<DataRow("00b", "axiom T {all n:*pred[obj] { n } }", 1)>]
    [<DataRow("01", "axiom T {ex n:pred { n } }", 0)>]
    [<DataRow("01a", "axiom T {ex n:*pred[ind] { n } }", 1)>]
    [<DataRow("01b", "axiom T {ex n:*pred[obj] { n } }", 1)>]
    [<DataRow("02", "axiom T {exn$1 n:pred { n } }", 0)>]
    [<DataRow("02a", "axiom T {exn$1 n:*pred[ind] { n } }", 1)>]
    [<DataRow("02a", "axiom T {exn$1 n:*pred[obj] { n } }", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestVAR08(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = VAR08
            runTestHelper "TestVAR08.fpl" fplCode code expected
