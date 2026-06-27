namespace Diagnostics.ProofRelated

open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open TestFplInterpreter.Helpers.Common

(* PR011
   Purpose: Report misuse of a justification that references an argument of a proof instead of the expected block.
   What it indicates: A justification keyword (e.g. `byax`, `byconj`, `bydef`, `byinf`, ...) was given a reference that points to an argument inside a proof (such as `A$1:3`) rather than the enclosing theorem/axiom/conjecture/corollary that the justification form expects.
   Use: Pinpoint justification sites that target the wrong granularity so the author can reference the correct statement or use an appropriate justification form that accepts argument-level references.
   Action / Treat: Change the justification to reference the enclosing theorem-like statement (for example use `byax A` or `bycor A$1` as appropriate) or choose a justification that accepts an argument reference. Treat PR011 as an error that must be corrected for the justification to be valid. *)

[<TestClass>]
type TestPR011() =

    [<DataRow("00", "proof T$1 {1. byax A |- trivial}", 0)>]
    [<DataRow("01", "proof T$1 {1. byax A$1:3 |- trivial}", 1)>]
    [<DataRow("00", "proof T$1 {1. byconj A |- trivial}", 0)>]
    [<DataRow("01", "proof T$1 {1. byconj A$1:3 |- trivial}", 1)>]
    [<DataRow("04", "proof T$1 {1. bydef A |- trivial}", 0)>]
    [<DataRow("05", "proof T$1 {1. bydef A$1:3 |- trivial}", 1)>]
    [<DataRow("02", "proof T$1 {1. byinf A |- trivial}", 0)>]
    [<DataRow("03", "proof T$1 {1. byinf A$1:3 |- trivial}", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestPR011(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = PR011 ("", "")
            runTestHelper "TestPR011.fpl" fplCode code expected
