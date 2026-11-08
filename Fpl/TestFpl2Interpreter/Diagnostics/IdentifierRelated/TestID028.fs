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

    // A valid call inside a constructor
    [<DataRow("00", "def class Cls def class Sub : Cls {ctor Sub() {dec base.Cls();} }", 0)>]
    // A misplaced call in the class body 
    [<DataRow("01", "def class Cls def class Sub : Cls {dec base.Cls(); ctor Sub() { } }", 1)>]
    // Two misplaced calls in the class body produce two diagnostics, since each call is checked separately.
    [<DataRow("02", "def class Cls def class Sub : Cls {dec base.Cls() base.Cls(); ctor Sub() { } }", 2)>]
    // A valid call in a constructor next to a misplaced call in the body yields exactly one diagnostic.
    [<DataRow("04", "def class Cls def class Sub : Cls {dec base.Cls(); ctor Sub() {dec base.Cls(); } }", 1)>]
    // Misplaced calls in two different derived classes are each reported.
    [<DataRow("05", "def class Cls def class Sub : Cls {dec base.Cls(); ctor Sub() { } } def class Sub2 : Cls {dec base.Cls(); ctor Sub2() { } }", 2)>]
    // A misplaced call in a derived class whose base has an explicit constructor is reported.
    [<DataRow("06", "def class Cls {ctor Cls() { } } def class Sub : Cls {dec base.Cls(); ctor Sub() { } }", 1)>]
    // Valid calls in the constructors of two derived classes produce no diagnostics, which guards against false positives.
    [<DataRow("07", "def class Cls def class Sub : Cls {ctor Sub() {dec base.Cls();} } def class Sub2 : Cls {ctor Sub2() {dec base.Cls();} }", 0)>]
    [<DataRow("99", "uses Fpl.Commons.Structures", 0)>]
    [<TestMethod>]
    member this.TestID028(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID028 ""
            runTestHelper "TestID028.fpl" fplCode code expected
