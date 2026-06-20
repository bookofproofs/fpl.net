namespace Diagnostics.SignatureRelated

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open FplInterpreter.Main
open TestFplInterpreter.Helpers.Common
open TestSharedConfig


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
