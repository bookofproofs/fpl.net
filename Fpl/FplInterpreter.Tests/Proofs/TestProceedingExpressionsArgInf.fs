namespace FplInterpreter.Tests.Proofs
open Microsoft.VisualStudio.TestTools.UnitTesting
open FplInterpreterBasicTypes
open Fpl.Primitives
open FplInterpreterProofs
open FplInterpreter.Globals.Heap
open FplInterpreter.Globals.HelpersComplex
open CommonTestHelpers

[<TestClass>]
type TestProceedingExpressionsArgInf() =

    [<DataRow("00", """thm T {true} proof T$1 {1: trivial }""", "true", 1)>]
    [<DataRow("01", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" thm T {all n,m:Nat { impl( ( Succ(n) = Succ(m) ), ( n = m ) ) }} thm T {true} prf T$1 { 1: trivial }""", "∀ m, n:Nat {((n') = (m')) ⇒ (n = m)}", 1)>]
    [<DataRow("02", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" thm T {all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }} thm T {true} prf T$1 { 1: trivial }""", "∀ m, n:Nat {((n') = (m')) ⇒ (n = m)}", 1)>]
    [<TestMethod>]
    member this.TestProceedingExpressionArgInfTrivial(no:string, fplCode, expectedExpr:string, expectedNumbExpr:int) =
        
        let filename = "TestProceedingExpressionArgInfTrivial"
        prepareFplCode(filename + ".fpl", fplCode, false) 
        let r = heap.Root

        let candidates = findCandidatesByName "T" false true
        let prf = candidates |> List.filter (fun fv -> fv.FplId = "T$1") |> List.map (fun fv -> fv :?> FplProof) |> List.head
        let fvJiOpt = tryFindInference (prf:FplProof) PrimArgInfTrivial 

        match fvJiOpt with
        | Some (:? FplArgInferenceTrivial as infTrivial) ->
            let result = infTrivial.ProceedingExprCandidates
            Assert.AreEqual<int>(expectedNumbExpr, result.Length)
            Assert.AreEqual<string>(expectedExpr, result.Head.Type SignatureType.Name)
        | Some ref ->
            Assert.IsInstanceOfType<FplArgInferenceTrivial>(ref)
        | None ->
            failwith $"expected FplArgInferenceTrivial, found none"

        prepareFplCode(filename, "", false) |> ignore


    [<DataRow("01", "thm T {true} proof T$1 {1: and(true,false) 2. 1, byinf AndCummutative |- and(false,true)}", "false ∧ true", 1)>]
    [<DataRow("01a", "thm T {true} proof T$1 {1: false ∧ true}", "false ∧ true", 1)>]
    [<DataRow("02", "thm T {true} proof T$1 {1: and(not (impl(true, false)), all z:obj {is(z, K)})}", "¬(true ⇒ false) ∧ ∀ z:obj {z is K}", 1)>]
    [<DataRow("02a", "thm T {true} proof T$1 {1: and(not impl(true, false), all z:obj {is(z, K)})}", "¬(true ⇒ false) ∧ ∀ z:obj {z is K}", 1)>]
    [<DataRow("02b", "thm T {true} proof T$1 {1: ¬(¬(true ⇒ false) ⇒ ¬∀ z:obj {z is K})}", "¬(¬(true ⇒ false) ⇒ ¬∀ z:obj {z is K})", 1)>]
    [<DataRow("03", "thm T {true} proof T$1 {1: not true}", "¬true", 1)>]
    [<DataRow("03c", "thm T {true} proof T$1 {1: (∀ x:obj {x is N} ∧ ¬∃ y:obj {y is M}) ∨ (¬∀ x:obj {x is N} ∧ ∃ y:obj {y is M})}}", "(∀ x:obj {x is N} ∧ ¬∃ y:obj {y is M}) ∨ (¬∀ x:obj {x is N} ∧ ∃ y:obj {y is M})", 1)>]
    [<DataRow("04", "thm T {true} proof T$1 {1. 2 |- ¬((true ⇒ false) ⩡ (true ∧ false)) }", "¬((true ⇒ false) ⩡ (true ∧ false))", 1)>]
    [<DataRow("04d", "thm T {true} proof T$1 {1: ¬((true ⇒ false) ⩡ (true ∧ false))}", "¬((true ⇒ false) ⩡ (true ∧ false))", 1)>]
    [<DataRow("05", "thm T {true} proof T$1 {1: not ex x:obj {iif(is(x,N), false)}}", "¬∃ x:obj {(x is N) ⇔ false}", 1)>]
    [<DataRow("05e", "thm T {true} proof T$1 {1: ∀ x:obj {¬((x is N) ⇔ false)}}", "∀ x:obj {¬((x is N) ⇔ false)}", 1)>]
    [<DataRow("06", "thm T {true} proof T$1 {1: all x:obj {not iif(is(x,N), true)} 2. 1, byinf AllNot2ExNot |- ¬∃ x:obj {(x is N) ⇔ true}}", "¬∃ x:obj {(x is N) ⇔ true}", 1)>]
    [<DataRow("06f", "thm T {true} proof T$1 {1: all x:obj {not iif(is(x,N), true)} }", "∀ x:obj {¬((x is N) ⇔ true)}", 1)>]
    [<TestMethod>]
    member this.TestProceedingExpressionArgInfDerived(no:string, fplCode, expectedExpr:string, expectedNumbExpr:int) =
        
        let filename = "TestProceedingExpressionArgInfDerived"
        prepareFplCode(filename + ".fpl", fplCode, false) 
        let r = heap.Root

        let candidates = findCandidatesByName "T" false true
        let prf = candidates |> List.filter (fun fv -> fv.FplId = "T$1") |> List.map (fun fv -> fv :?> FplProof) |> List.head
        let fvJiOpt = tryFindLastInference (prf:FplProof) PrimArgInfDerive 

        match fvJiOpt with
        | Some (:? FplArgInferenceDerived as argInf) ->
            let result = argInf.ProceedingExprCandidates
            Assert.AreEqual<int>(expectedNumbExpr, result.Length)
            Assert.AreEqual<string>(expectedExpr, result.Head.Type SignatureType.Name)
        | Some ref ->
            Assert.IsInstanceOfType<FplArgInferenceDerived>(ref)
        | None ->
            failwith $"expected FplArgInferenceDerived, found none"

        prepareFplCode(filename, "", false) |> ignore
