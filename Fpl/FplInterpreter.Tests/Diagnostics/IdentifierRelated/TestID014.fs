namespace Diagnostics.IdentifierRelated

open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open TestFplInterpreter.Helpers.Common

(* ID014
   Purpose: Report duplicate declarations of the same language code in localization metadata.
   What it indicates: The codebase contains two declarations that use the identical language/code identifier within the same scope, producing a naming conflict for localization entries.
   Use: Helps authors find and remove or disambiguate duplicate language declarations so localization metadata remains unique and unambiguous.
   Action / Treat: Rename or remove the conflicting declaration or consolidate entries so each language code is declared exactly once; treat ID014 as an error that must be resolved. *)

[<TestClass>]
type TestID014() =

    [<DataRow("""loc iif(x, y) := !tex: x "\Leftrightarrow" y !eng: x " if and only if " y !ger: x " dann und nur dann wenn " y;""", 0)>]
    [<DataRow("""loc iif(x, y) := !tex: x "\Leftrightarrow" y !eng: x " if and only if " y !tex: x " dann und nur dann wenn " y;""", 1)>]
    [<DataRow("uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestID014(fplCode:string, expected:int) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID014 ("", "")
            runTestHelper "TestID014.fpl" fplCode code expected
