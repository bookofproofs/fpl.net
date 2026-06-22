namespace Diagnostics.IdentifierRelated

open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open TestFplInterpreter.Helpers.Common

(* ID002
   Purpose: Report a proof block associated with an incompatible block type.
   What it indicates: A `proof` block was found but the signature it targets is not a theorem‑like statement (the associated block is of the wrong kind).
   Use: Emitted when a proof cannot be attached to an appropriate enclosing declaration so callers can locate the misplaced proof.
   Action / Treat: Attach the proof to a proper theorem‑like declaration or change the declaration so it accepts a proof; ID002 is an error that must be resolved for correct proof association. *)

[<TestClass>]
type TestID002() =

    [<DataRow("axiom Test {true} proof Test$1 {1: trivial}", 1)>]
    [<DataRow("postulate Test {true} proof Test$1 {1: trivial}", 1)>]
    [<DataRow("conjecture Test {true} proof Test$1 {1: trivial}", 1)>]
    [<DataRow("def pred Test() {true} proof Test$1 {1: trivial}", 1)>]
    [<DataRow("def cl Test {intr} proof Test$1 {1: trivial}", 1)>]
    [<DataRow("def func Test()->obj {intr} proof Test$1 {1: trivial}", 1)>]
    [<DataRow("proof Test$1 {1: trivial}proof Test$1$1 {1: trivial}", 1)>]
    [<DataRow("theorem Test {true} proof Test$1 {1: trivial}", 0)>]
    [<DataRow("lemma Test {true} proof Test$1 {1: trivial}", 0)>]
    [<DataRow("proposition Test {true} proof Test$1 {1: trivial}", 0)>]
    [<DataRow("corollary Test$1 {true} proof Test$1$1 {1: trivial}", 0)>]
    [<DataRow("inf T { pre: true con: true } proof T$1 {1: trivial}", 1)>]
    [<DataRow("ext T x@/\d+/ -> obj {ret x} proof T$1 {1: trivial}", 1)>]
    [<DataRow("uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestID002(fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID002 ("","")
            runTestHelper "TestID002.fpl" fplCode code expected
