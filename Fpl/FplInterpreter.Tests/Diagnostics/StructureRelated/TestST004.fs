namespace Diagnostics.StructureRelated

open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open TestFplInterpreter.Helpers.Common

(* ST004
   Purpose: Indicate that a requested language variant is not available in a localization block.
   What it indicates: The localization does not provide an entry for the current language code, so a representation for that language cannot be produced.
   Use: Helps authors find missing translations/localizations for a language used by the environment or by callers.
   Action / Treat: Add the missing language entry to the `loc` block or supply a fallback; treat ST004 as a warning signaling incomplete localization data that should be completed for correct language-specific rendering. *)

[<TestClass>]
type TestST004() =

    [<DataRow("01", """loc Equal(x,y) := !tex: x "=" y !eng: x " equals " y !ger: x " ist gleich " y !pol: x " równa się " y;""", 0)>]
    [<DataRow("02", """loc Equal(x,y) := !eng: x " equals " y !ger: x " ist gleich " y !pol: x " równa się " y;""", 1)>] // tex implementation missing
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestST004(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ST004 ""
            runTestHelper "TestST004.fpl" fplCode code expected
