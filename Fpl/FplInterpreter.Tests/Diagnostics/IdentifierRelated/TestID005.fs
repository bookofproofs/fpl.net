namespace Diagnostics.IdentifierRelated

open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open TestFplInterpreter.Helpers.Common

(* ID005
   Purpose: Report corollary declarations that cannot be associated with an appropriate parent block.
   What it indicates: A `corollary` was declared but no compatible enclosing theorem‑like statement was found (or the target found is of an incompatible kind, or the placement is not permitted, e.g. corollaries of proofs).
   Use: Emitted when embedding/associating corollaries to point authors to corollary nodes that lack a valid parent or are placed in the wrong scope.
   Action / Treat: Attach the corollary to a valid theorem‑like parent, move it into the correct scope, or change the declaration so it no longer requires association. ID005 is an error that must be resolved for correct association and further processing. *)

[<TestClass>]
type TestID005() =

    [<DataRow("def pred Test() {true} corollary Test$1 {true}", 1)>]
    [<DataRow("def cl Test {intr} corollary Test$1 {true}", 1)>]
    [<DataRow("def func Test()->obj {intr} corollary Test$1 {true}", 1)>]
    [<DataRow("proof Test$1 {1: trivial}corollary Test$1$1 {true}", 1)>] // corollaries of proofs are not allowed
    [<DataRow("theorem Test {true} corollary Test$1 {true}", 0)>]
    [<DataRow("lemma Test {true} corollary Test$1 {true}", 0)>]
    [<DataRow("proposition Test {true} corollary Test$1 {true}", 0)>]
    [<DataRow("axiom Test {true} corollary Test$1 {true}", 0)>] // corollaries of axioms are allowed
    [<DataRow("postulate Test {true} corollary Test$1 {true}", 0)>] // corollaries of postulates (axioms) not allowed
    [<DataRow("conjecture Test {true} corollary Test$1 {true}", 0)>] // corollaries of conjectures are not allowed
    [<DataRow("corollary Test$1 {true} corollary Test$1$1 {true}", 0)>] // corollaries of corollaries are allowed
    [<DataRow("uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestID005(fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID005 ("","")
            runTestHelper "TestID005.fpl" fplCode code expected
