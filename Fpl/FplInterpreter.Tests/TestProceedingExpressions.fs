namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open FplInterpreterBasicTypes
open FplPrimitives
open FplInterpreterProofs
open FplInterpreter.Globals.Heap
open FplInterpreter.Globals.HelpersComplex
open CommonTestHelpers

[<TestClass>]
type TestProceedingExpressions() =

    [<DataRow("00", """ax A {true} thm T {true} prf T$1 { 1. byax A |- true };""", "true", 1)>]
    [<DataRow("01", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" ax A {all n,m:Nat { impl( ( Succ(n) = Succ(m) ), ( n = m ) ) }} thm T {true} prf T$1 { 1. byax A |- true };""", "∀ m, n:Nat {((n') = (m')) ⇒ (n = m)}", 1)>]
    [<DataRow("02", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" ax A {all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }} thm T {true} prf T$1 { 1. byax A |- true };""", "∀ m, n:Nat {((n') = (m')) ⇒ (n = m)}", 1)>]
    [<TestMethod>]
    member this.TestProceedingExpressionJustByAx(no:string, fplCode, expectedExpr:string, expectedNumbExpr:int) =
        
        let filename = "TestProceedingExpressionJustByAx"
        prepareFplCode(filename + ".fpl", fplCode, false) 
        let r = heap.Root
        let theory = r.Scope[filename]

        let candidates = findCandidatesByName "T" false true
        let prf = candidates |> List.filter (fun fv -> fv.FplId = "T$1") |> List.map (fun fv -> fv :?> FplProof) |> List.head
        let fjByAxOpt =
            prf.OrderedArguments
            |> List.map (fun fv -> fv.Justification)
            |> List.filter (fun fv -> fv.IsSome)
            |> List.map (fun fv -> fv.Value)
            |> List.map (fun fv -> fv :?> FplJustification)
            |> List.map (fun fv -> fv.GetOrderedJustificationItems)
            |> List.concat 
            |> List.tryFind (fun fv -> fv.Name = PrimJIByAx)

        match fjByAxOpt with
        | Some (:? FplJustificationItemByAx as fjbyAx) ->
            let result = fjbyAx.ProceedingExprCandidates
            Assert.AreEqual<int>(expectedNumbExpr, result.Length)
            Assert.AreEqual<string>(expectedExpr, result.Head.Type SignatureType.Name)
        | Some ref ->
            Assert.IsInstanceOfType<FplJustificationItemByAx>(ref)
        | None ->
            failwith $"expected FplJustificationItemByAx, found none"

        prepareFplCode(filename, "", false) |> ignore


    [<DataRow("00", """thm T {true} proof T$1 {1. |- trivial };""", "true", 1)>]
    [<DataRow("01", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" thm T {all n,m:Nat { impl( ( Succ(n) = Succ(m) ), ( n = m ) ) }} thm T {true} prf T$1 { 1. |- trivial };""", "∀ m, n:Nat {((n') = (m')) ⇒ (n = m)}", 1)>]
    [<DataRow("02", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" thm T {all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }} thm T {true} prf T$1 { 1. |- trivial };""", "∀ m, n:Nat {((n') = (m')) ⇒ (n = m)}", 1)>]
    [<TestMethod>]
    member this.TestProceedingExpressionInfTrivial(no:string, fplCode, expectedExpr:string, expectedNumbExpr:int) =
        
        let filename = "TestProceedingExpressionInfTrivial"
        prepareFplCode(filename + ".fpl", fplCode, false) 
        let r = heap.Root
        let theory = r.Scope[filename]

        let candidates = findCandidatesByName "T" false true
        let prf = candidates |> List.filter (fun fv -> fv.FplId = "T$1") |> List.map (fun fv -> fv :?> FplProof) |> List.head
        let fjByAxOpt =
            prf.OrderedArguments
            |> List.map (fun fv -> fv.ArgumentInference)
            |> List.filter (fun fv -> fv.IsSome)
            |> List.map (fun fv -> fv.Value)
            |> List.tryFind (fun fv -> fv.Name = PrimArgInfTrivial)

        match fjByAxOpt with
        | Some infTrivial ->
            let result = infTrivial.ProceedingExprCandidates
            Assert.AreEqual<int>(expectedNumbExpr, result.Length)
            Assert.AreEqual<string>(expectedExpr, result.Head.Type SignatureType.Name)
        | Some ref ->
            Assert.IsInstanceOfType<FplJustificationItemByAx>(ref)
        | None ->
            failwith $"expected FplJustificationItemByAx, found none"

        prepareFplCode(filename, "", false) |> ignore
