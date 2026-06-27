namespace Diagnostics.IdentifierRelated

open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open TestFplInterpreter.Helpers.Common

(* ID008
   Purpose: Detect and report a misspelled or incorrect constructor identifier in a constructor declaration.
   What it indicates: A constructor declaration used a name that does not match the enclosing class/constructor identifier (commonly a typo or wrong identifier), so the constructor cannot be associated with the intended class.
   Use: Emitted during embedding/creation of identifiers to point authors to constructor declarations whose identifier does not match the expected class name.
   Action / Treat: Correct the constructor name so it matches the class (or rename/refactor the class as intended) so the constructor binds to the correct class. Treat ID008 as an error preventing proper constructor association. *)

[<TestClass>]
type TestID008() =

    [<DataRow("def cl Test {ctor TestTypo(x:Nat) {}}", 1)>]
    [<DataRow("def cl Test {ctor TestTypo1() {}}", 1)>]
    [<DataRow("def cl Test {ctor Test() {}}", 0)>]
    [<DataRow("def cl Test {dec x:obj x := 0; ctor Test() {dec base.Obj(); }}", 0)>]
    [<DataRow("uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestID008(fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID008 ("", "") 
            runTestHelper "TestID008.fpl" fplCode code expected
