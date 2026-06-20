namespace Diagnostics.Identifiers

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open FplInterpreter.Main
open TestFplInterpreter.Helpers.Common
open TestSharedConfig


[<TestClass>]
type TestID001() =


    [<DataRow("theorem SomeTheorem {true} theorem SomeTheorem {true} ", 1)>]
    [<DataRow("theorem SomeTheorem {true} ", 0)>]

    [<DataRow("lemma SomeLemma {true} lemma SomeLemma {true} ", 1)>]
    [<DataRow("lemma SomeLemma {true} ", 0)>]

    [<DataRow("conj TestId {true} conj TestId {true} ", 1)>]
    [<DataRow("conj TestId {true} ", 0)>]

    [<DataRow("prop TestId {true} prop TestId {true} ", 1)>]
    [<DataRow("prop TestId {true} ", 0)>]

    [<DataRow("axiom TestId {true} axiom TestId {true} ", 1)>]
    [<DataRow("axiom TestId {true} ", 0)>]

    [<DataRow("axiom TestId {true} postulate TestId {true} ", 1)>]

    [<DataRow("def pred TestId() {true} postulate TestId {true} ", 0)>]

    [<DataRow("theorem TestId {true} postulate TestId {true} ", 1)>]

    [<DataRow("theorem TestId {true} def pred TestId() {true} ", 0)>]

    [<DataRow("postulate SomePostulate {true} postulate SomePostulate {true} ", 1)>]
    [<DataRow("postulate SomePostulate {true} ", 0)>]

    [<DataRow("def func SomeFunctionalTerm(x,y:pred(u,v,w:obj)) -> obj {intrinsic} def func SomeFunctionalTerm(x,y:pred(u,v,w:obj)) -> obj {intrinsic} ", 1)>]
    [<DataRow("def func SomeFunctionalTerm(x,y:pred(u,v,w:obj)) -> obj {intrinsic} ", 0)>]

    [<DataRow("def class SomeClass def class SomeClass ", 1)>]
    [<DataRow("def class SomeClass ", 0)>]

    [<DataRow("def predicate SomePredicate() {intrinsic} def predicate SomePredicate() {intrinsic} ", 1)>]
    [<DataRow("def predicate SomePredicate() {intrinsic} ", 0)>]

    [<DataRow("inf SomeRuleOfInference {pre: true con: true} inf SomeRuleOfInference {pre: true con: true}", 1)>]
    [<DataRow("inf SomeRuleOfInference {pre: true con: true} ", 0)>]

    [<DataRow("corollary SomeCorollary$1 {true} corollary SomeCorollary$1 {true}", 1)>]
    [<DataRow("corollary SomeCorollary$1$2 {true} corollary SomeCorollary$1$2 {true}", 1)>]
    [<DataRow("corollary SomeCorollary$1$1$2 {true} corollary SomeCorollary$1$1$2 {true}", 1)>]
    [<DataRow("corollary SomeCorollary$1 {true} ", 0)>]
    [<DataRow("corollary SomeCorollary$1$2 {true} ", 0)>]
    [<DataRow("corollary SomeCorollary$1$1$2 {true} ", 0)>]

    [<DataRow("proof TestId$1 {1: trivial}proof TestId$1 {1: trivial}", 1)>]
    [<DataRow("proof TestId$1$2 {1: trivial}proof TestId$1$2 {1: trivial}", 1)>]
    [<DataRow("proof TestId$1$2$3 {1: trivial}proof TestId$1$2$3 {1: trivial}", 1)>]
    [<DataRow("proof TestId$1 {1: trivial}", 0)>]
    [<DataRow("proof TestId$1$2 {1: trivial}", 0)>]
    [<DataRow("proof TestId$1$2$3 {1: trivial}", 0)>]

    [<DataRow(@"ext Digits x@/\d+/ -> X {ret x} def pred T() {true} ext Digits x@/\d+/ -> X {ret x}", 1)>]
    [<DataRow(@"ext Digits x@/\d+/ -> X {ret x} def pred T() {true} ext Digits1 x@/\d+/ -> X {ret x} ", 0)>]
    [<DataRow(@"ext Digits x@/\d+/ -> X {ret x} def pred T() {true} ext Digits1 x@/\d+/ -> X {ret x} def pred S() {true} ", 0)>]
    [<DataRow(@"ext Digits x@/\d+/ -> X {ret x} def pred T() {true} ext Digits1 x@/\d+/ -> X {ret x} def pred T() {@1} ", 1)>]
    [<DataRow(@"ext Digits x@/\d+/ -> X {ret x} def pred T(x:Digits) {true}", 0)>]
    [<DataRow(@"ext Digits x@/\d+/ -> X {ret x} def pred T(x:Typo) {true}", 0)>]

    [<DataRow("def func Sum(list:* Nat[ind])->Nat {dec result: Nat; return result} def func Sum2(list:* Nat[ind])->Nat {dec result: Nat; return result}", 0)>]
    [<DataRow("""def cl B {intr} def cl A {dec x:obj; ctor A(y:B) {} }""", 0)>]
    [<DataRow("uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestID001(fplCode:string, expected:int) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID001 ("", "")
            runTestHelper "TestID001.fpl" fplCode code expected


    [<DataRow("""def pred Equal(x,y: tpl) infix "=" 50 {intr} """, 0)>]
    [<DataRow("uses Fpl.Commons inf ModusPonens {pre:true con:true} ", 1)>]
    [<DataRow("uses Fpl.Commons theorem ModusTollens {true} ", 1)>]
    [<DataRow("uses Fpl.Commons def pred HypotheticalSyllogism() {true} ", 0)>]
    [<DataRow("uses Fpl.Commons axiom DisjunctiveSyllogism {true} ", 1)>]
    [<DataRow("uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestID001ConflictWithOtherTheories(fplCode:string, expected:int) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID001 ("", "")
            runTestHelper "TestID001ConflictWithOtherTheories.fpl" fplCode code expected
