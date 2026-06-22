namespace Diagnostics.IdentifierRelated

open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open TestFplInterpreter.Helpers.Common

(* ID009
   Purpose: Detect and report circular dependencies in class/interface inheritance chains.
   What it indicates: The declared inheritance chain for a type contains a cycle (the type directly or indirectly inherits from itself), making the hierarchy ill-formed.
   Use: Emitted during type embedding and validation to point authors to problematic inheritance declarations that prevent correct type construction and resolution.
   Action / Treat: Break the cycle by removing or changing one of the base references, refactor the hierarchy to eliminate indirect self-dependency, or redesign the types so inheritance is acyclic. Treat ID009 as an error that must be resolved for correct embedding and type resolution. *)

[<TestClass>]
type TestID009() =

    [<DataRow("def cl Test {intr}", 0)>]
    [<DataRow("def cl Test:Test {intr}", 1)>]
    [<DataRow("def cl Test:Test1, Test2, Test3 {intr}", 0)>]
    [<DataRow("def cl Test:Test1, Test2, Test3, Test {intr}", 1)>]
    [<DataRow("uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestID009(fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID009 ""
            runTestHelper "TestID009.fpl" fplCode code expected
