namespace Diagnostics.IdentifierRelated

open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl0Base.Errors.Diagnostics
open Fpl2Interpreter.Helpers.Debug
open TestFpl2Interpreter.Helpers.Common

(* ID028
   Purpose: Report an invocation of the base class constructor directly from the body of a derived class, not from one of its constructors.
   What it indicates: A derived class calls its base class constructor not from the body of one of its constructors but from its own body.
   Use: Emitted during class embedding/validation to ensure that all constructors call the base class constructors properly.
   Action / Treat: Move the call from the class' body to the body of one of its constructors. *)

[<TestClass>]
type TestID028() =

    [<DataRow("00", "def class Cls def class Sub : Cls {ctor Sub() {dec base.Cls();} }", 0)>]
    [<DataRow("01", "def class Cls def class Sub : Cls {dec base.Cls(); ctor Sub() { } }", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures", 0)>]
    [<TestMethod>]
    member this.TestID028(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID028 ""
            runTestHelper "TestID028.fpl" fplCode code expected
