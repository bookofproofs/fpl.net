namespace FplInterpreter.Tests.Proofs
open System
open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open FplInterpreterBasicTypes
open FplPrimitives
open FplInterpreterProofs
open FplInterpreter.Globals.Heap
open FplInterpreter.Globals.HelpersComplex
open CommonTestHelpers

[<TestClass>]
type TestProceedingExpressionsArgInf() =

    let tryFindInference (prf:FplProof) infType = 
        prf.OrderedArguments
        |> List.map (fun fv -> fv.ArgumentInference)
        |> List.filter (fun fv -> fv.IsSome)
        |> List.map (fun fv -> fv.Value)
        |> List.tryFind (fun fv -> fv.Name = infType)

    [<DataRow("00", """thm T {true} proof T$1 {1. |- trivial };""", "true", 1)>]
    [<DataRow("01", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" thm T {all n,m:Nat { impl( ( Succ(n) = Succ(m) ), ( n = m ) ) }} thm T {true} prf T$1 { 1. |- trivial };""", "∀ m, n:Nat {((n') = (m')) ⇒ (n = m)}", 1)>]
    [<DataRow("02", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" thm T {all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }} thm T {true} prf T$1 { 1. |- trivial };""", "∀ m, n:Nat {((n') = (m')) ⇒ (n = m)}", 1)>]
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


