namespace Diagnostics.IdentifierRelated

open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open TestFplInterpreter.Helpers.Common

(* ID021
   Purpose: Report a duplicate invocation of a base constructor from a derived class.
   What it indicates: A derived class constructor forwards to the base constructor more than once, producing a redundant/illegal duplicate base constructor call during embedding.
   Use: Emitted when validating class construction chains to point to constructors that incorrectly call the same base constructor multiple times.
   Action / Treat: Remove the redundant base constructor invocation so the base is called exactly once (or refactor the construction pattern); treat ID021 as an error that must be fixed for correct class initialization. *)

[<TestClass>]
type TestID021() =

    [<DataRow("01", "def cl A {intr} def cl B:A {ctor B() {dec base.A(); } }", 0)>]
    [<DataRow("01a", "def cl A {intr} def cl B:A {ctor B() {dec base.A() base.A(); } }", 1)>]
    [<DataRow("02", "def cl A { ctor A() {dec base.Obj(); } }", 0)>]
    [<DataRow("02a", "def cl A { ctor A() {dec base.Obj() base.Obj(); } }", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestID021(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID021 ""
            runTestHelper "TestID021.fpl" fplCode code expected
