namespace Diagnostics.IdentifierRelated

open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open TestFplInterpreter.Helpers.Common

(* ID027
   Purpose: Report illegal self-recursive use of an entity as the domain of a `for` statement.
   What it indicates: A `for` statement attempted to iterate over an entity that is (directly or indirectly) the same entity being defined or processed, producing an illegal recursion in the loop domain.
   Use: Emitted during semantic checks to point to `for` statements whose domain expression refers back to the entity currently being defined, which would cause a recursion or nonsensical domain.
   Action / Treat: Change the `for` domain so it does not depend on the entity being defined (use a separate collection/value or reorder definitions), or rewrite the logic to avoid self-reference. Treat ID027 as an error that must be resolved. *)

[<TestClass>]
type TestID027() =

    [<DataRow("00", "def func T(list:* Nat[ind])->pred { dec result:pred for list in list { result:=true }; return result }", 1)>]
    [<DataRow("01", "def func T(list:* Nat[ind])->pred { dec result:pred for list in a { result:=true }; return result }", 0)>]
    [<DataRow("02", "def cl Set def cl Nat:Set def pred T(list:*Set[Nat]) { dec s:Set for s in list { i:=Succ(i) }; true }", 0)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestID027(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID027 ""
            runTestHelper "TestID027.fpl" fplCode code expected
