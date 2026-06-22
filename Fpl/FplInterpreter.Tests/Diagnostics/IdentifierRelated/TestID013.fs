namespace Diagnostics.IdentifierRelated

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open FplInterpreter.Main
open TestFplInterpreter.Helpers.Common
open TestSharedConfig

(* ID013
   Purpose: Report problems related to delegate usage during embedding and checks.
   What it indicates: Delegate invocation or resolution failed (unknown delegate, argument-type mismatch, or other delegate-specific validation error reported by the delegate handler).
   Use: Emitted when delegate lookups or delegate-driven checks cannot be satisfied so callers can locate incorrect delegate calls or mismatched argument types.
   Action / Treat: Ensure the delegate is declared and visible, and that passed arguments match the delegate's expected types/signature; fix or declare the delegate as appropriate. ID013 diagnostics carry the delegate-provided diagnostic text to explain the specific delegate failure. *)

[<TestClass>]
type TestID013() =

    [<DataRow("00", "def pred T() {del.Test()}", 1, "Unknown delegate `Test`")>]
    [<DataRow("01", "def pred T() {del.Test1(x,y)}", 1, "Unknown delegate `Test1`")>]
    [<DataRow("02", "def pred T() {del.Equal(x,y)}", 1, "Predicate `=` cannot be evaluated because the left argument is undefined.")>]
    [<DataRow("03", "def pred T(x:pred) {del.Equal(x,y)}", 1, "Predicate `=` cannot be evaluated because the right argument is undefined.")>]
    [<DataRow("04", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)}""", 0, "missing error message")>]
    [<DataRow("04a", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def pred NotEqual(x,y: tpl) infix "<>" 60 {not (x = y)} """, 0, "missing error message")>] 
    [<DataRow("04", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)}""", 0, "missing error message")>]
    [<DataRow("06", "ax T {all x,y:obj {del.Equal(x,y)}}", 0, "missing error message")>]
    [<DataRow("06a", "def cl Nat {intr} ax T {all x,y:Nat {del.Equal(x,y)}}", 0, "missing error message")>]
    [<DataRow("06b", "ax T {all x,y:Bla {del.Equal(x,y)}}", 0, "missing error message")>]
    [<DataRow("07", "ax T {exn$1 x:obj {del.Equal(x,@1)}}", 0, "missing error message")>]
    [<DataRow("07a", "def cl Nat {intr} ax T {exn$1 x:Nat {del.Equal(x,@1)}}", 0, "missing error message")>]
    [<DataRow("07b", "ax T {exn$1 x:obj {del.Equal(x,$1)}}", 0, "missing error message")>]
    [<DataRow("07b_", "def cl Nat {intr} ax T {exn$1 x:Nat {del.Equal(x,$1)}}", 0, "missing error message")>]
    [<DataRow("08", """ax T {all n:obj {exn$1 y:obj {del.Equal(y,n)}}}""", 0, "missing error message")>]
    [<DataRow("08a", """def cl Nat {intr} ax T {all n:Nat {exn$1 y:Nat {del.Equal(y,n)}}}""", 0, "missing error message")>]
    [<DataRow("09", """def func Add()->obj {intr} prop AddIsSomething {dec anotherAdd, op: Add; all n,m:obj { (add(n,m) = anotherAdd(n,m) )} }""", 0, "missing error message")>]
    [<DataRow("10", """def func Add()->obj {intr} prop AddIsSomething {dec anotherAdd: Add; all n,m:obj { (anotherAdd(n,m) = n) } }""", 0, "missing error message")>]
    [<DataRow("11", """def func Add()->obj {intr} prop AddIsSomething {dec anotherAdd: Add; all n,m:obj { (anotherAdd(n,@0) = n) } }""", 0, "missing error message")>]
    [<DataRow("12", """def pred T(x,y:tpl) infix "=" 50 {del.Equal(x,y)} ext Digits x@/\d+/ -> ind {dec n:ind cases (| (x = @0): n := $1 ? n := $2); ret n }""", 0, "missing error message")>]
    [<DataRow("13", """def pred Equal(x,y: tpl) infix "=" 50 { del.Equal(x,y)} ext Digits x@/\d+/ -> pred {ret mcases ( | (x = @0): true ? false ) }""", 0, "missing error message")>]
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0, "missing error message")>]
    [<TestMethod>]
    member this.TestID013(no:string, fplCode:string, expected, expectedErrMsg:string) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID013 ""
            let errMsg = runTestHelperWithText "TestID013.fpl" fplCode code expected
            Assert.AreEqual<string>(expectedErrMsg, errMsg)

    // the delegate Decrement should accept only "Digits" as type
    [<DataRow("00", "ext Digits x@/\d+/ -> Digits {ret x} def pred T(x:Digits) { del.Decrement(x) }", 0)>] 
    [<DataRow("01", "ext Digits x@/\d+/ -> pred {ret true} def pred T(x:Digits) { del.Decrement(x) }", 0)>] // does not issue ID013 since x is Digits, as required
    [<DataRow("01a", "ext Digits x@/\d+/ -> ind {ret $42} def pred T(x:Digits) { del.Decrement(x) }", 0)>]  // does not issue ID013 since x is Digits, as required
    [<DataRow("02", "def pred T(x:pred) { del.Decrement(x) }", 1)>] 
    [<DataRow("02a", "def pred T(x:pred()) { del.Decrement(x) }", 1)>] 
    [<DataRow("02b", "def pred T(x:pred(y:obj)) { del.Decrement(x) }", 1)>] 
    [<DataRow("03", "def pred T(x:func) { del.Decrement(x) }", 1)>] 
    [<DataRow("03a", "def pred T(x:func()->obj) { del.Decrement(x) }", 1)>] 
    [<DataRow("03b", "def pred T(x:func()->ind) { del.Decrement(x) }", 1)>] 
    [<DataRow("03c", "def pred T(x:func(y:obj)->obj) { del.Decrement(x) }", 1)>] 
    [<DataRow("03d", "def pred T(x:func(y:ind)->ind) { del.Decrement(x) }", 1)>] 
    [<DataRow("04", "def pred T(x:obj) { del.Decrement(x) }", 1)>] 
    [<DataRow("05", "def pred T(x:ind) { del.Decrement(x) }", 1)>] 
    [<DataRow("06", "def cl Nat def pred T(x:Nat) { del.Decrement(x) }", 1)>] 
    [<TestMethod>]
    member this.TestID013DecrementType(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID013 ""
            
            runTestHelper "TestID013DecrementType.fpl" fplCode code expected
