namespace Diagnostics.IdentifierRelated

open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open TestFplInterpreter.Helpers.Common

(* ID024
   Purpose: Report that an expression was already localized for the same language at the same localization context.
   What it indicates: A localization contains a language localization more than once, producing a conflict.
   Use: Helps locate duplicate localizations so authors can remove or consolidate redundant expression localizations and avoid ambiguity.
   Action / Treat: Remove or rename the redundant localization, or adjust the embedding order so only a single localization remains. Treat ID024 as an error that must be resolved to maintain unique expression localization within the scope. *)

[<TestClass>]
type TestID024() =

    [<DataRow("00a", """loc not(x) :=  !tex: "\neg(" x ")";""", 0)>]
    [<DataRow("00b", """loc not(x) :=  !tex: "\neg(" x ")"; loc not(x) :=  !tex: "\neg(" x ")"; """, 1)>]
    [<DataRow("00c", """loc not(y) :=  !tex: "\neg(" x ")"; loc not(x) :=  !tex: "\neg(" x ")"; """, 1)>]
    [<DataRow("01a", """loc Equal(x,y) := !tex: x "=" y !eng: x " equals " y !ger: x " ist gleich " y;""", 0)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestID024(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID024 ("", "")
            runTestHelper "TestID024.fpl" fplCode code expected
