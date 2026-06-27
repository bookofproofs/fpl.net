namespace Diagnostics.LogicRelated

open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open TestFplInterpreter.Helpers.Common

(* LG001
   Purpose: Report failure when a predicate-valued check cannot be evaluated due to an argument that itself could not be treated as a predicate.
   What it indicates: A predicate evaluation failed because one of its arguments did not evaluate to a predicate‑like value (type/shape mismatch or an undetermined expression).
   Use: Emitted during predicate and validation checks to pinpoint expressions that block logical evaluation, allowing authors to identify the offending argument and its context.
   Action / Treat: Correct the argument so it evaluates to a predicate (fix its expression or type), or adjust the predicate usage. Treat LG001 as an error requiring correction before the enclosing check can succeed. *)

[<TestClass>]
type TestLG001() =

    [<DataRow("00a", """def pred T() { true }""", 0)>]
    [<DataRow("00", """def pred T() { not true }""", 0)>]
    [<DataRow("01", """def pred T() { dec x:pred; not x }""", 0)>]
    [<DataRow("02", """def pred T() { dec x:pred; not (x) }""", 0)>]
    [<DataRow("03", """def pred T() { dec x:pred; not ((x)) }""", 0)>]
    [<DataRow("04", """def pred T() { dec x:pred; not (((x))) }""", 0)>]
    [<DataRow("05", """def pred T() { dec x:ind; not x }""", 1)>]
    [<DataRow("06", """def pred T() { all x:obj {true} }""", 0)>]
    [<DataRow("07", """inf ModusTollens {dec p,q: pred; pre: and (not q, impl(p,q) ) con: not (p)}""", 0)>]
    [<DataRow("08", """inf ModusTollens {dec p,q: pred; pre: and (not q, impl(p,q) ) con: not (p)}""", 0)>]
    [<DataRow("09", """inf ModusTollens {dec p,q: pred; pre: and (not q, impl(p,q) ) con: not (p)}""", 0)>]
    [<DataRow("10", """inf ModusTollens {dec p,q: pred; pre: and (not q, impl(p,q) ) con: not p}""", 0)>]
    [<DataRow("11", """def pred T() { and(true,and(x,true)) }""", 1)>]
    [<DataRow("12", """def pred T() { dec x:pred; and(and(true,x),true) }""", 0)>]
    [<DataRow("13", """def pred T() { or(false,or(x,false)) }""", 1)>]
    [<DataRow("14", """def pred T() { dec x:pred; or(or(false,x),false) }""", 0)>]
    [<DataRow("15", """def pred T() { impl(true,x) }""", 1)>]
    [<DataRow("16", """def pred T() { impl(true,true) }""", 0)>]
    [<DataRow("17", """def pred T() { dec x:pred; iif(true,x) }""", 0)>]
    [<DataRow("18", """def pred T() { iif(true,x) }""", 1)>]
    [<DataRow("19", """def pred T() { xor(xor(true,true),true) }""", 0)>]
    [<DataRow("20", """def pred T() { dec x,y:pred; xor(y,xor(x,z)) }""", 1)>]
    [<DataRow("21", """loc and(p,q) := !tex: p "\wedge" q;""", 0)>]
    [<DataRow("22", """def class Set def pred In(x,y: Set) def pred IsEmpty(x: Set) { all y:Set { not In(y, x) } }""", 0)>]
    [<DataRow("23", """def pred T() { mcases (| true: false | false: true ? undef) }""", 0)>]
    [<DataRow("23a", """def pred T() {dec x:obj; mcases (| $1: false | x: true ? undef) }""", 2)>]
    [<DataRow("23b", """def pred T() {dec res:pred cases (| true: res:=false | false: res:=true ? res:=undef); res}""", 0)>]
    [<DataRow("23c", """def pred T() {dec res:pred x:obj cases (| $1: res:=false | x: res:=true ? res:=undef); res}""", 2)>]
    [<DataRow("23d", """def pred T() {dec res:pred x:obj cases (| true: res:=false | x: res:=true ? res:=undef); res}""", 1)>]
    [<DataRow("24", """def pred T() { true }""", 0)>]
    [<DataRow("24a", """def pred T() { $1 }""", 1)>]
    [<DataRow("24b", """def pred T() { undef }""", 1)>]
    [<DataRow("25", """ax T { true }""", 0)>]
    [<DataRow("25a", """ax T { $1 }""", 1)>]
    [<DataRow("25b", """ax T { undef }""", 1)>]
    [<DataRow("26", """thm T { true }""", 0)>]
    [<DataRow("26a", """thm T { $1 }""", 1)>]
    [<DataRow("26b", """thm T { undef }""", 1)>]
    [<DataRow("27", """lem T { true }""", 0)>]
    [<DataRow("27a", """lem T { $1 }""", 1)>]
    [<DataRow("27b", """lem T { undef }""", 1)>]
    [<DataRow("28", """prop T { true }""", 0)>]
    [<DataRow("28a", """prop T { $1 }""", 1)>]
    [<DataRow("28b", """prop T { undef }""", 1)>]
    [<DataRow("29", """conj T { true }""", 0)>]
    [<DataRow("29a", """conj T { $1 }""", 1)>]
    [<DataRow("29b", """conj T { undef }""", 1)>]
    [<DataRow("30", """cor T$1 { true }""", 0)>]
    [<DataRow("30a", """cor T$1 { $1 }""", 1)>]
    [<DataRow("30b", """cor T$1 { undef }""", 1)>]
    [<DataRow("31", """def pred T() { ex n:obj { true } }""", 0)>]
    [<DataRow("31a", """def pred T() { ex n:obj { $1 } }""", 1)>]
    [<DataRow("31b", """def pred T() { ex n:obj { undef } }""", 1)>]
    [<DataRow("32", """def pred T() { all n:obj { true } }""", 0)>]
    [<DataRow("32a", """def pred T() { all n:obj { $1 } }""", 1)>]
    [<DataRow("32b", """def pred T() { all n:obj { undef } }""", 1)>]
    [<DataRow("33", """def pred T() { exn$1 n:obj { true } }""", 0)>]
    [<DataRow("33a", """def pred T() { exn$1 n:obj { $1 } }""", 1)>]
    [<DataRow("33b", """def pred T() { exn$1 n:obj { undef } }""", 1)>]
    [<DataRow("34", """def pred T() { and ( true, false ) }""", 0)>]
    [<DataRow("34a", """def pred T() { and ( true, $1 ) }""", 1)>]
    [<DataRow("34b", """def pred T() { and ( $1, undef ) }""", 2)>]
    [<DataRow("35", """def pred T() { or ( true, false ) }""", 0)>]
    [<DataRow("35a", """def pred T() { or ( true, $1 ) }""", 1)>]
    [<DataRow("35b", """def pred T() { or ( $1, undef ) }""", 2)>]
    [<DataRow("36", """def pred T() { impl ( true, false ) }""", 0)>]
    [<DataRow("36a", """def pred T() { impl ( true, $1 ) }""", 1)>]
    [<DataRow("36b", """def pred T() { impl ( $1, undef ) }""", 2)>]     
    [<DataRow("37", """def pred T() { xor ( true, false ) }""", 0)>]
    [<DataRow("37a", """def pred T() { xor ( true, $1 ) }""", 1)>]
    [<DataRow("37b", """def pred T() { xor ( $1, undef ) }""", 2)>]  
    [<DataRow("38", """def pred T() { iif ( true, false ) }""", 0)>]
    [<DataRow("38a", """def pred T() { iif ( true, $1 ) }""", 1)>]
    [<DataRow("38b", """def pred T() { iif ( $1, undef ) }""", 2)>]  
    [<DataRow("39", """def pred Equal(x,y:tpl) infix "=" 0 { delegate.Equal(x,y) } def cl A def pred EqualA (a,b: A) { (a = b) }""", 0)>]
    [<DataRow("40", """def pred T() {mcases (| true: false | false: true ? false) }""", 0)>]
    [<DataRow("41", """proof T$1 {1: true }""", 0)>]
    [<DataRow("41a", """proof T$1 {1: undef }""", 1)>]
    [<DataRow("41b", """proof T$1 {1: undef 2. 1 |- true}""", 1)>]
    [<DataRow("41c", """proof T$1 {1: false 2. 1 |- true}""", 0)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestLG001(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = LG001 ("", "", "")
            runTestHelper "TestLG001.fpl" fplCode code expected

    [<DataRow("""axiom A {dec x,y:Nat; impl(x,y)}""", 27)>]
    [<TestMethod>]
    member this.TestLG001Position(fplCode:string, (expected:int64)) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = LG001 ("", "", "")
            let filename = "TestLG001Position.fpl"
            prepareFplCode (filename, fplCode, false) |> ignore
            checkForUnexpectedErrors filename fplCode
            let result = filterByErrorCode ad code.Code
            Assert.AreEqual<int64>(expected, result.Head.StartPos.Column)
        
    [<DataRow("00", """axiom A {dec x,y:obj; impl(x,y)}""", "Cannot evaluate `implication` because its argument `x` typed `obj` could not be evaluated as a predicate. This issue might be subsequent to other errors to be resolved first.")>]
    [<DataRow("01", """def cl T {intr} axiom A {impl(T,true)}""", "Cannot evaluate `implication` because its argument `T` could not be evaluated as a predicate. This issue might be subsequent to other errors to be resolved first.")>]
    [<DataRow("02", """axiom A {dec x,y:ind; impl(x,y)}""", "Cannot evaluate `implication` because its argument `x` typed `ind` could not be evaluated as a predicate. This issue might be subsequent to other errors to be resolved first.")>]
    [<DataRow("03", """axiom A {dec x,y:func; impl(x,y)}""", "Cannot evaluate `implication` because its argument `x` typed `func` could not be evaluated as a predicate. This issue might be subsequent to other errors to be resolved first.")>]
    [<DataRow("04", """axiom A {impl(x,y)}""", "Cannot evaluate `implication` because its argument `x` typed `undef` could not be evaluated as a predicate. This issue might be subsequent to other errors to be resolved first.")>]
    [<DataRow("05", """axiom A {impl(T(),true)}""", "Cannot evaluate `implication` because its argument `T()` typed `T` could not be evaluated as a predicate. This issue might be subsequent to other errors to be resolved first.")>]
    [<DataRow("06", """axiom A {impl(T,true)}""", "Cannot evaluate `implication` because its argument `T` could not be evaluated as a predicate. This issue might be subsequent to other errors to be resolved first.")>]
    [<TestMethod>]
    member this.TestLG001MsgSpecificity(no:string, fplCode:string, (expected:string)) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = LG001 ("", "", "")
            let filename = "TestLG001MsgSpecificity.fpl"
            prepareFplCode (filename, fplCode, false) |> ignore
            checkForUnexpectedErrors filename fplCode
            let result = filterByErrorCode ad code.Code
            Assert.AreEqual<string>(expected, result.Head.Message)
