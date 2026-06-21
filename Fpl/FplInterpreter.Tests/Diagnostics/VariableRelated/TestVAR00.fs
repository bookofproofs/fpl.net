namespace Diagnostics.VariableRelated

open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open TestFplInterpreter.Helpers.Common

(* VAR00 diagnostic
   Purpose: VAR00 warns when multiple variadic/array-style variables are declared together, which can create ambiguous or unclear bindings.
   What it indicates: the declaration may produce indexing or scope ambiguities that make symbol-table construction and later evaluation nondeterministic or hard to reason about.
   Action suggested: separate variadic declarations or use explicit mapping/array types so each variable's intended shape and binding are unambiguous.
   Treat VAR00 as a prompt to rewrite the declaration for clarity and deterministic symbol resolution. *)

[<TestClass>]
type TestVAR00() =

    [<DataRow("def predicate Test(x,y:* pred[ind]) {true}", 1)>]
    [<DataRow("def predicate Test(x,y:* pred[obj]) {true}", 1)>]
    [<DataRow("def predicate Test(x,y: pred) {true}", 0)>]
    [<DataRow("def predicate Test(x:* pred[ind]) {true}", 0)>]
    [<DataRow("def predicate Test(x:* pred[obj]) {true}", 0)>]
    [<DataRow("uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestVAR00(fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = VAR00
            runTestHelper "TestVAR00.fpl" fplCode code expected
