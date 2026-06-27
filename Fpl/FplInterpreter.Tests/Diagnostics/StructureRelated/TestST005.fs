namespace Diagnostics.StructureRelated

open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open TestFplInterpreter.Helpers.Common

(* ST005
   Purpose: Indicate that the interpreter could not establish a valid enumerator for a `for ... in` domain.
   What it indicates: The domain expression (e.g., the second part of `for x in DOMAIN`) is not an array/iterable form the interpreter knows how to enumerate, so the loop cannot be executed as written.
   Use: Helps locate invalid or unsupported iteration domains (missing array, non-enumerable type, or malformed domain reference) when authoring loops.
   Action / Treat: Supply an enumerable domain (e.g., an array variable or a proper range/collection), change the loop to a supported iteration form, or implement a custom enumerator; treat ST005 as an informational/structure diagnostic that requires correcting the domain so the loop can be evaluated. *)

[<TestClass>]
type TestST005() =

    [<DataRow("01", """def cl Nat def func Sum(list:* Nat[ind])->Nat { dec a:obj result, addend: Nat result:=Zero() for addend in list { result:=Add(result,addend) }; return result }""", 0)>]
    [<DataRow("02", """def cl Nat def func Sum(from, to: Nat, arr:*Nat[Nat]) -> Nat { dec a:obj i, result: Nat result:=Zero() for i in ClosedRange(from,to) { result:=Add(result,arr[i]) }; return result }""", 1)>]
    [<DataRow("03", """def cl Nat def func Sum() -> Nat { dec addend, result: Nat result:=Zero() for addend in Nat { result:=Add(result,addend) }; return result }""", 1)>]
    [<DataRow("04", """def cl Nat def func Add(x,y:Nat)->Nat def func Sum()->Nat {dec addend, result: Nat for addend in Nat() { result:=Add(result,addend) }; ret result }""", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestST005(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ST005 ("", "")
            runTestHelper "TestST005.fpl" fplCode code expected
