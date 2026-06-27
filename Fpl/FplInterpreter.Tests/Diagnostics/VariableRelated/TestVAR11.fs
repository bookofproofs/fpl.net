namespace Diagnostics.VariableRelated

open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open TestFplInterpreter.Helpers.Common

(* VAR11
   Purpose: Detect duplicate variable identifiers declared inside a localization (`loc`) block.
   What it indicates: Two localization variables share the same name, causing ambiguity in translation/localization scope.
   Use: Help find name collisions in localization blocks so translations map to distinct variables.
   Action / Treat: Rename the conflicting localization identifier so every `loc` variable is unique; treat VAR11 as an error that must be fixed. *)

   
[<TestClass>]
type TestVAR11() =

    [<DataRow("01", """loc and(p,q) := !tex: p "\wedge" q;""", 0)>]
    [<DataRow("02", """loc and(q,q) := !tex: p "\wedge" q;""", 1)>]
    [<DataRow("03", """loc ex q:pred { and(q,q) } := !tex: p "\wedge" q;""", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestVAR11(no: string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = VAR11 ("", "")
            runTestHelper "TestVAR11.fpl" fplCode code expected
