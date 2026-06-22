namespace Diagnostics.IdentifierRelated

open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open TestFplInterpreter.Helpers.Common

(* ID023
   Purpose: Report an ambiguous association when more than one candidate matches a justification target.
   What it indicates: The attempt to bind a justification to a single block found multiple candidates in the current scope, so the association is ambiguous.
   Use: Pinpoints ambiguous references so the author can disambiguate the intended target.
   Action / Treat: Disambiguate the reference (rename, qualify, or remove competing candidates, or change the justification) so exactly one candidate remains; treat ID023 as an error that must be resolved. *)

[<TestClass>]
type TestID023() =

    [<DataRow("00a", "def cl A {intr} thm T {true} proof T$1 {1. bydef A |- trivial }", 0)>]
    [<DataRow("00b", "def cl A {intr} def cl B:A {intr} thm T {true} proof T$1 {1. bydef A |- trivial }", 0)>]
    [<DataRow("01a", "lem A {true} thm T {true} proof T$1 {1. A |- trivial }", 0)>]
    [<DataRow("01b", "lem A {true} def pred A(x:obj) {intr} thm T {true} proof T$1 {1. bydef A |- trivial }", 1)>]
    [<DataRow("02a", "cor A$1 {true} thm T {true} proof T$1 {1. A$1 |- trivial }", 0)>]
    [<DataRow("02b", "proof A$1 {1:  trivial} cor A$11 {true} thm T {true} proof T$1 {1. A$1 |- trivial }", 1)>]
    [<DataRow("03", " def cl A1 def cl A { dec assert is(self, A1); ctor A() {} } thm T {true} prf T$1 { 1. bydef A |- true }", 0)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestID023(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID023 ""
            runTestHelper "TestID023.fpl" fplCode code expected
