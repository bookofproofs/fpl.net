namespace FplInterpreter.Tests.Proofs
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Primitives
open Fpl.Interpreter.BasicTypes
open Fpl.Interpreter.SymbolTable.Storage.Heap
open Fpl.Interpreter.SymbolTable.Storage.Util
open Fpl.Interpreter.SymbolTable.Types4.Proofs
open TestFplInterpreter.Helpers.Common

[<TestClass>]
type TestInferredExpressionsArgInf() =

    [<DataRow("00", """thm T {true} proof T$1 {1: trivial }""", "true", 1)>]
    [<DataRow("01", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" thm T {all n,m:Nat { impl( ( Succ(n) = Succ(m) ), ( n = m ) ) }} thm T {true} prf T$1 { 1: trivial }""", "∀ m, n:Nat {((n') = (m')) ⇒ (n = m)}", 1)>]
    [<DataRow("02", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" thm T {all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }} thm T {true} prf T$1 { 1: trivial }""", "∀ m, n:Nat {((n') = (m')) ⇒ (n = m)}", 1)>]
    [<TestMethod>]
    member this.TestInferredExpressionArgInfTrivial(no:string, fplCode, expectedExpr:string, expectedNumbExpr:int) =
        
        let filename = "TestInferredExpressionArgInfTrivial"
        prepareFplCode(filename + ".fpl", fplCode, false) 
        let r = heap.Root

        let candidates = findCandidatesByName "T" false true
        let prf = candidates |> List.filter (fun fv -> fv.FplId = "T$1") |> List.map (fun fv -> fv :?> FplProof) |> List.head
        let fvJiOpt = tryFindInference (prf:FplProof) PrimArgInfTrivial 

        match fvJiOpt with
        | Some (:? FplArgInferenceTrivial as infTrivial) ->
            let result = infTrivial.InferredExprCandidates
            Assert.AreEqual<int>(expectedNumbExpr, result.Length)
            Assert.AreEqual<string>(expectedExpr, result.Head.Type SignatureType.Name)
        | Some ref ->
            Assert.IsInstanceOfType(ref, typeof<FplArgInferenceTrivial>)
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
    member this.TestInferredExpressionArgInfDerived(no:string, fplCode, expectedExpr:string, expectedNumbExpr:int) =
        
        let filename = "TestInferredExpressionArgInfDerived"
        prepareFplCode(filename + ".fpl", fplCode, false) 
        let r = heap.Root

        let candidates = findCandidatesByName "T" false true
        let prf = candidates |> List.filter (fun fv -> fv.FplId = "T$1") |> List.map (fun fv -> fv :?> FplProof) |> List.head
        let fvJiOpt = tryFindLastInference (prf:FplProof) PrimArgInfDerive 

        match fvJiOpt with
        | Some (:? FplArgInferenceDerived as argInf) ->
            let result = argInf.InferredExprCandidates
            Assert.AreEqual<int>(expectedNumbExpr, result.Length)
            Assert.AreEqual<string>(expectedExpr, result.Head.Type SignatureType.Name)
        | Some ref ->
            Assert.IsInstanceOfType(ref, typeof<FplArgInferenceDerived>)
        | None ->
            failwith $"expected FplArgInferenceDerived, found none"

        prepareFplCode(filename, "", false) |> ignore


    [<DataRow("01", "thm T {true} proof T$1 {1: assume and(true,false)}", "true ∧ false", 1)>]
    [<DataRow("02", "thm T {true} proof T$1 {1: assume ¬(¬(true ⇒ false) ⇒ ¬∀ z:obj {z is K})}", "¬(¬(true ⇒ false) ⇒ ¬∀ z:obj {z is K})", 1)>]
    [<DataRow("03", "thm T {true} proof T$1 {1: assume not true}", "¬true", 1)>]
    [<DataRow("04", "thm T {true} proof T$1 {1: assume ∀ x:obj {¬((x is N) ⇔ false)}}", "∀ x:obj {¬((x is N) ⇔ false)}", 1)>]
    [<TestMethod>]
    member this.TestInferredExpressionArgInferenceAssume(no:string, fplCode, expectedExpr:string, expectedNumbExpr:int) =
        
        let filename = "TestInferredExpressionArgInferenceAssume"
        prepareFplCode(filename + ".fpl", fplCode, false) 
        let r = heap.Root

        let candidates = findCandidatesByName "T" false true
        let prf = candidates |> List.filter (fun fv -> fv.FplId = "T$1") |> List.map (fun fv -> fv :?> FplProof) |> List.head
        let fvJiOpt = tryFindLastInference (prf:FplProof) PrimArgInfAssume 

        match fvJiOpt with
        | Some (:? FplArgInferenceAssume as argInf) ->
            let result = argInf.InferredExprCandidates
            Assert.AreEqual<int>(expectedNumbExpr, result.Length)
            Assert.AreEqual<string>(expectedExpr, result.Head.Type SignatureType.Name)
        | Some ref ->
            Assert.IsInstanceOfType(ref, typeof<FplArgInferenceAssume>)
        | None ->
            failwith $"expected FplArgInferenceAssume, found none"

        prepareFplCode(filename, "", false) |> ignore


    [<DataRow("01", "thm T {true} proof T$1 {dec x:pred x:=and(true,false) ; 1: assume x}", "true ∧ false", 1)>]
    [<DataRow("02", "thm T {true} proof T$1 {dec x:pred x:=¬(¬(true ⇒ false) ⇒ ¬∀ z:obj {z is K}) ;1: assume x}", "¬(¬(true ⇒ false) ⇒ ¬∀ z:obj {z is K})", 1)>]
    [<DataRow("03", "thm T {true} proof T$1 {dec x:pred x:=not true ;1: assume x}", "¬true", 1)>]
    [<DataRow("04", "thm T {true} proof T$1 {dec x:pred x:=∀ x:obj {¬((x is N) ⇔ false)} ;1: assume x}", "∀ x:obj {¬((x is N) ⇔ false)}", 1)>]
    [<TestMethod>]
    member this.TestInferredExpressionArgInferenceAssumeVar(no:string, fplCode, expectedExpr:string, expectedNumbExpr:int) =
        
        let filename = "TestInferredExpressionArgInferenceAssumeVar"
        prepareFplCode(filename + ".fpl", fplCode, false) 
        let r = heap.Root

        let candidates = findCandidatesByName "T" false true
        let prf = candidates |> List.filter (fun fv -> fv.FplId = "T$1") |> List.map (fun fv -> fv :?> FplProof) |> List.head
        let fvJiOpt = tryFindLastInference (prf:FplProof) PrimArgInfAssume 

        match fvJiOpt with
        | Some (:? FplArgInferenceAssume as argInf) ->
            let result = argInf.InferredExprCandidates
            Assert.AreEqual<int>(expectedNumbExpr, result.Length)
            Assert.AreEqual<string>(expectedExpr, result.Head.Type SignatureType.Name)
        | Some ref ->
            Assert.IsInstanceOfType(ref, typeof<FplArgInferenceAssume>)
        | None ->
            failwith $"expected FplArgInferenceAssume, found none"

        prepareFplCode(filename, "", false) |> ignore

    [<DataRow("01", "thm T {true} proof T$1 {1: assume and(true,false) 2: revoke 1}", "¬(true ∧ false)", 1)>]
    [<DataRow("02", "thm T {true} proof T$1 {1: assume ¬(¬(true ⇒ false) ⇒ ¬∀ z:obj {z is K}) 2: revoke 1}", "¬¬(¬(true ⇒ false) ⇒ ¬∀ z:obj {z is K})", 1)>]
    [<DataRow("03", "thm T {true} proof T$1 {1: assume not true 2: revoke 1}", "¬¬true", 1)>]
    [<DataRow("04", "thm T {true} proof T$1 {1: assume ∀ x:obj {¬((x is N) ⇔ false)} 2: revoke 1}", "¬∀ x:obj {¬((x is N) ⇔ false)}", 1)>]
    [<TestMethod>]
    member this.TestInferredExpressionArgInferenceRevoke(no:string, fplCode, expectedExpr:string, expectedNumbExpr:int) =
        
        let filename = "TestInferredExpressionArgInferenceRevoke"
        prepareFplCode(filename + ".fpl", fplCode, false) 
        let r = heap.Root

        let candidates = findCandidatesByName "T" false true
        let prf = candidates |> List.filter (fun fv -> fv.FplId = "T$1") |> List.map (fun fv -> fv :?> FplProof) |> List.head
        let fvJiOpt = tryFindLastInference (prf:FplProof) PrimArgInfRevoke 

        match fvJiOpt with
        | Some (:? FplArgInferenceRevoke as argInf) ->
            let result = argInf.InferredExprCandidates
            Assert.AreEqual<int>(expectedNumbExpr, result.Length)
            Assert.AreEqual<string>(expectedExpr, result.Head.Type SignatureType.Name)
        | Some ref ->
            Assert.IsInstanceOfType(ref, typeof<FplArgInferenceRevoke>)
        | None ->
            failwith $"expected FplArgInferenceRevoke, found none"

        prepareFplCode(filename, "", false) |> ignore
