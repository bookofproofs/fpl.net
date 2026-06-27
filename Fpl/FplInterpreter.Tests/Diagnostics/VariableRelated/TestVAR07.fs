namespace Diagnostics.VariableRelated

open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open TestFplInterpreter.Helpers.Common

(* VAR07
   Purpose: Detect use of a uniqueness quantifier form that accepts only a single bound variable with multiple identifiers.
   What it indicates: The quantifier form was used with more than one identifier, violating its expected arity.
   Use: Identify arity mismatches so the author can choose an appropriate quantifier or split the bindings.
   Action / Treat: Use a quantifier that matches the intended number of variables or rewrite the declaration; treat VAR07 as an error that must be fixed. *)

[<TestClass>]
type TestVAR07() =

    [<DataRow("00", "def pred T() {exn$1 n:pred { n } }", 0)>]
    [<DataRow("01", "def pred T() {exn$1 n, m:pred { n } }", 1)>]
    [<DataRow("02", "def pred T() {exn$1 n:pred, m:pred { n } }", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestVAR07(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = VAR07 ""
            runTestHelper "TestVAR07.fpl" fplCode code expected
