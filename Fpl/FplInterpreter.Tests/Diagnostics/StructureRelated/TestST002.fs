namespace Diagnostics.StructureRelated

open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open TestFplInterpreter.Helpers.Common

(* ST002
   Purpose: Signal an unexpected or empty constructor block at symbol-table creation time.
   What it indicates: A constructor node was created but no signature/argument entries were found where the implementation expects them (the constructor's ArgList is empty). This typically means the constructor's declaration or its surrounding class block is malformed or missing the expected signature/parameters.
   Use: Helps locate class/constructor definitions whose constructor body cannot be associated with a valid signature or where the constructor appears structurally empty.
   Action / Treat: Verify and correct the constructor signature and surrounding class declaration (add the missing parameter list, fix the constructor header, or remove the stray constructor block). Treat ST002 as an informational/structure diagnostic that points to a likely authoring mistake to be fixed. *)

[<TestClass>]
type TestST002() =

    [<DataRow("00a", "def cl A {ctor A(x:obj){} } ", 1)>]
    [<DataRow("00b", "def cl A:B {ctor A(){dec base.B();} } ", 0)>]
    [<DataRow("00c", "def cl A {ctor A(){} } ", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestST002(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ST002 ""
            runTestHelper "TestST002.fpl" fplCode code expected
