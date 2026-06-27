namespace Diagnostics.SignatureRelated

open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open TestFplInterpreter.Helpers.Common

(* SIG13
   Purpose: Enforce that all branches of a map/case-style statement return values of the same type.
   What it indicates: A branch returns a value whose type differs from the type established by the first (reference) branch, or the first branch was `undef` which forces other branches to also be `undef`.
   Use: Detect type-inconsistencies across `mcases`/`mapcases` (and similar) branches so the author can make branch results uniform.
   Action / Treat: Make all branch results produce the same type (or use `undef` consistently), or change the first branch so it reflects the intended common result type; treat SIG13 as a type-consistency diagnostic that must be resolved to guarantee well-typed branch result aggregation. *)

[<TestClass>]
type TestSIG13() =

    [<DataRow("00", """def pred T() { mcases (| true: false | false: true ? undef) }""", 0)>] // undef as else case is allowed, all other cases return the same type as the first branch
    [<DataRow("01", """def pred T() {dec x:obj; mcases (| true: $1 | false: x ? undef) }""", 1)>] // second branch returns type different from first branch
    [<DataRow("02", """def pred T() {dec x:obj; mcases (| true: false | false: $42 ? undef) }""", 1)>] // second branch returns type different from first branch
    [<DataRow("03", """def pred T() {dec x:obj; mcases (| true: false | false: $42 | false: $4 ? undef) }""", 2)>] // two consecutive branches return types different from first branch
    [<DataRow("04", """def pred T() {dec x:obj; mcases (| true: undef | false: $42 | false: true ? false) }""", 3)>] // undef as first branch forces all other branches also to return undef
    [<DataRow("05", """def pred T() {dec x:obj; mcases (| true: true | false: undef | false: true ? true) }""", 0)>] // undef in the middle is allowed
    [<DataRow("06", """def cl Nat def func Succ(n: Nat) -> Nat ext Digits x@/\d+/ -> Nat {ret mcases (| true: Nat() ? Succ(self(delegate.Decrement(x))) ) }""", 0)>]  
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestSIG13(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SIG13 ("", "", "", "")
            
            runTestHelper "TestSIG13.fpl" fplCode code expected
