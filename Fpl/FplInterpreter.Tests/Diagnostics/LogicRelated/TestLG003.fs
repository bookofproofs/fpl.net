namespace Diagnostics.LogicRelated

open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open TestFplInterpreter.Helpers.Common

(* LG003
   Purpose: Report that a predicate ist not satisfiable and therefore the node cannot be treated as valid for the intended role.
   What it indicates: A predicate or validation expression associated with a node produced a false result during evaluation (the node's representation was `false`), preventing the node from being accepted for its expected purpose.
   Use: Emitted when a validity check fails so callers can locate the offending node and its predicate that produced the failure.
   Action / Treat: Inspect and correct the predicate or the node's definition so the validity check succeeds; LG003 is emitted as an error and must be resolved for normal processing to continue. *)

[<TestClass>]
type TestLG003() =

    [<DataRow("00", """axiom T { true }""", 0)>]
    [<DataRow("01", """axiom T { false }""", 1)>]
    [<DataRow("02", """theorem T { true }""", 0)>]
    [<DataRow("03", """theorem T { false }""", 1)>]
    [<DataRow("04", """proposition T { true }""", 0)>]
    [<DataRow("05", """proposition T { false }""", 1)>]
    [<DataRow("06", """lemma T { true }""", 0)>]
    [<DataRow("07", """lemma T { false }""", 1)>]
    [<DataRow("08", """corollary T$1 { true }""", 0)>]
    [<DataRow("09", """corollary T$1 { false }""", 1)>]
    [<DataRow("10", """conjecture T { true }""", 0)>]
    [<DataRow("11", """conjecture T { false }""", 1)>]
    [<DataRow("12", """postulate T { true }""", 0)>]
    [<DataRow("13", """postulate T { false }""", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestLG003(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = LG003 ("", "")
            runTestHelper "TestLG003.fpl" fplCode code expected
