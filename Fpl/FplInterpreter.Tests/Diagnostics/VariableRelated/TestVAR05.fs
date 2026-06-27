namespace Diagnostics.VariableRelated

open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open TestFplInterpreter.Helpers.Common

(* VAR05
   Purpose: Detect quantifier-bound variables that are never referenced in the quantifier body.
   What it indicates: A bound variable is redundant and likely represents a forgotten use or an unnecessary binding.
   Use: Help authors find and eliminate dead bindings that confuse readers or signal logic errors.
   Action / Treat: Remove the unused bound variable or update the body to use it; treat VAR05 as an error (or strong hint) to correct the quantification. *)


[<TestClass>]
type TestVAR05() =

    [<DataRow("def pred T() { all x:obj {true}}", 1)>]
    [<DataRow("def pred T() { all x:obj {x}}", 0)>]
    [<DataRow("def pred T() { ex x:obj {true}}", 1)>]
    [<DataRow("def pred T() { ex x:obj {x}}", 0)>]
    [<DataRow("def pred T() { exn$1 x:obj {true}}", 1)>]
    [<DataRow("def pred T() { exn$1 x:obj {x}}", 0)>]
    [<DataRow("uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestVAR05(fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = VAR05 ""
            runTestHelper "TestVAR05.fpl" fplCode code expected
