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

    let tryFindJustification (prf:FplProof) justType = 
        prf.OrderedArguments
        |> List.map (fun fv -> fv.Justification)
        |> List.filter (fun fv -> fv.IsSome)
        |> List.map (fun fv -> fv.Value)
        |> List.map (fun fv -> fv :?> FplJustification)
        |> List.map (fun fv -> fv.GetOrderedJustificationItems)
        |> List.concat 
        |> List.tryFind (fun fv -> fv.Name = justType)


    let tryFindInference (prf:FplProof) infType = 
        prf.OrderedArguments
        |> List.map (fun fv -> fv.ArgumentInference)
        |> List.filter (fun fv -> fv.IsSome)
        |> List.map (fun fv -> fv.Value)
        |> List.tryFind (fun fv -> fv.Name = infType)

    [<DataRow("00", """ax A {true} thm T {true} prf T$1 { 1. byax A |- true };""", "true", 1)>]
    [<DataRow("01", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" ax A {all n,m:Nat { impl( ( Succ(n) = Succ(m) ), ( n = m ) ) }} thm T {true} prf T$1 { 1. byax A |- true };""", "∀ m, n:Nat {((n') = (m')) ⇒ (n = m)}", 1)>]
    [<DataRow("02", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" ax A {all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }} thm T {true} prf T$1 { 1. byax A |- true };""", "∀ m, n:Nat {((n') = (m')) ⇒ (n = m)}", 1)>]
    [<TestMethod>]
    member this.TestProceedingExpressionJustByAx(no:string, fplCode, expectedExpr:string, expectedNumbExpr:int) =
        
        let filename = "TestProceedingExpressionJustByAx"
        prepareFplCode(filename + ".fpl", fplCode, false) 
        let r = heap.Root

        let candidates = findCandidatesByName "T" false true
        let prf = candidates |> List.filter (fun fv -> fv.FplId = "T$1") |> List.map (fun fv -> fv :?> FplProof) |> List.head
        let fvJiOpt = tryFindJustification prf PrimJIByAx

        match fvJiOpt with
        | Some (:? FplJustificationItemByAx as fvJi) ->
            let result = fvJi.ProceedingExprCandidates
            Assert.AreEqual<int>(expectedNumbExpr, result.Length)
            Assert.AreEqual<string>(expectedExpr, result.Head.Type SignatureType.Name)
        | Some ref ->
            Assert.IsInstanceOfType<FplJustificationItemByAx>(ref)
        | None ->
            failwith $"expected FplJustificationItemByAx, found none"

        prepareFplCode(filename, "", false) |> ignore

    [<DataRow("00", """conj A {true} thm T {true} prf T$1 { 1. byconj A |- true };""", "true", 1)>]
    [<DataRow("01", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" conj A {all n,m:Nat { impl( ( Succ(n) = Succ(m) ), ( n = m ) ) }} thm T {true} prf T$1 { 1. byconj A |- true };""", "∀ m, n:Nat {((n') = (m')) ⇒ (n = m)}", 1)>]
    [<DataRow("02", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" conj A {all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }} thm T {true} prf T$1 { 1. byconj A |- true };""", "∀ m, n:Nat {((n') = (m')) ⇒ (n = m)}", 1)>]
    [<TestMethod>]
    member this.TestProceedingExpressionJustByConj(no:string, fplCode, expectedExpr:string, expectedNumbExpr:int) =
        
        let filename = "TestProceedingExpressionJustByConj"
        prepareFplCode(filename + ".fpl", fplCode, false) 
        let r = heap.Root

        let candidates = findCandidatesByName "T" false true
        let prf = candidates |> List.filter (fun fv -> fv.FplId = "T$1") |> List.map (fun fv -> fv :?> FplProof) |> List.head

        let fvJiOpt = tryFindJustification prf PrimJIByConj

        match fvJiOpt with
        | Some (:? FplJustificationItemByConj as fvJi) ->
            let result = fvJi.ProceedingExprCandidates
            Assert.AreEqual<int>(expectedNumbExpr, result.Length)
            Assert.AreEqual<string>(expectedExpr, result.Head.Type SignatureType.Name)
        | Some ref ->
            Assert.IsInstanceOfType<FplJustificationItemByConj>(ref)
        | None ->
            failwith $"expected FplJustificationItemByConj, found none"

        prepareFplCode(filename, "", false) |> ignore


    [<DataRow("00thm", """thm A {true} thm T {true} prf T$1 { 1. A |- true };""", "true", 1)>]
    [<DataRow("01thm", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" thm A {all n,m:Nat { impl( ( Succ(n) = Succ(m) ), ( n = m ) ) }} thm T {true} prf T$1 { 1. A |- true };""", "∀ m, n:Nat {((n') = (m')) ⇒ (n = m)}", 1)>]
    [<DataRow("02thm", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" thm A {all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }} thm T {true} prf T$1 { 1. A |- true };""", "∀ m, n:Nat {((n') = (m')) ⇒ (n = m)}", 1)>]
    [<DataRow("00lem", """lem A {true} thm T {true} prf T$1 { 1. A |- true };""", "true", 1)>]
    [<DataRow("01lem", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" thm A {all n,m:Nat { impl( ( Succ(n) = Succ(m) ), ( n = m ) ) }} lem T {true} prf T$1 { 1. A |- true };""", "∀ m, n:Nat {((n') = (m')) ⇒ (n = m)}", 1)>]
    [<DataRow("02lem", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" thm A {all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }} lem T {true} prf T$1 { 1. A |- true };""", "∀ m, n:Nat {((n') = (m')) ⇒ (n = m)}", 1)>]
    [<DataRow("00prop", """prop A {true} thm T {true} prf T$1 { 1. A |- true };""", "true", 1)>]
    [<DataRow("01prop", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" thm A {all n,m:Nat { impl( ( Succ(n) = Succ(m) ), ( n = m ) ) }} prop T {true} prf T$1 { 1. A |- true };""", "∀ m, n:Nat {((n') = (m')) ⇒ (n = m)}", 1)>]
    [<DataRow("02prop", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" thm A {all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }} prop T {true} prf T$1 { 1. A |- true };""", "∀ m, n:Nat {((n') = (m')) ⇒ (n = m)}", 1)>]
    [<TestMethod>]
    member this.TestProceedingExpressionJustByTheoremLikeStmt(no:string, fplCode, expectedExpr:string, expectedNumbExpr:int) =
        
        let filename = "TestProceedingExpressionJustByTheoremLikeStmt"
        prepareFplCode(filename + ".fpl", fplCode, false) 
        let r = heap.Root

        let candidates = findCandidatesByName "T" false true
        let prf = candidates |> List.filter (fun fv -> fv.FplId = "T$1") |> List.map (fun fv -> fv :?> FplProof) |> List.head

        let fvJiOpt = tryFindJustification prf PrimJIByTheoremLikeStmt

        match fvJiOpt with
        | Some (:? FplJustificationItemByTheoremLikeStmt as fvJi) ->
            let result = fvJi.ProceedingExprCandidates
            Assert.AreEqual<int>(expectedNumbExpr, result.Length)
            Assert.AreEqual<string>(expectedExpr, result.Head.Type SignatureType.Name)
        | Some ref ->
            Assert.IsInstanceOfType<FplJustificationItemByTheoremLikeStmt>(ref)
        | None ->
            failwith $"expected FplJustificationItemByTheoremLikeStmt, found none"

        prepareFplCode(filename, "", false) |> ignore


    [<DataRow("00", """def cl N thm T {true} prf T$1 { 1. |- ex x:obj {is(x,N)} 2. 1 |- trivial};""", "∃ x:obj {is(x, N)}", 1)>]
    [<DataRow("01", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" ax A {true} thm T {true} prf T$1 { 1. byax A |- all x,y:Set {impl ( is(x, N), ( x = y ))} 2. 1 |- true };""", "∀ x, y:Set {(is(x, N)) ⇒ (x = y)}", 1)>]
    [<TestMethod>]
    member this.TestProceedingExpressionJustByArgRef(no:string, fplCode, expectedExpr:string, expectedNumbExpr:int) =
        
        let filename = "TestProceedingExpressionJustByArgRef"
        prepareFplCode(filename + ".fpl", fplCode, false) 
        let r = heap.Root

        let candidates = findCandidatesByName "T" false true
        let prf = candidates |> List.filter (fun fv -> fv.FplId = "T$1") |> List.map (fun fv -> fv :?> FplProof) |> List.head
        let fvJiOpt = tryFindJustification prf PrimJIByRefArgument

        match fvJiOpt with
        | Some (:? FplJustificationItemByRefArgument as fvJi) ->
            let result = fvJi.ProceedingExprCandidates
            Assert.AreEqual<int>(expectedNumbExpr, result.Length)
            Assert.AreEqual<string>(expectedExpr, result.Head.Type SignatureType.Name)
        | Some ref ->
            Assert.IsInstanceOfType<FplJustificationItemByRefArgument>(ref)
        | None ->
            failwith $"expected FplJustificationItemByRefArgument, found none"

        prepareFplCode(filename, "", false) |> ignore

    [<DataRow("00", """def cl N thm T {true} prf T$1 { 1. |- ex x:obj {is(x,N)} 2. 1 |- trivial} thm T1 {true} prf T1$1 { 1. T$1:1 |- trivial };""", "∃ x:obj {is(x, N)}", 1)>]
    [<DataRow("01", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" ax A {true} thm T {true} prf T$1 { 1. byax A |- all x,y:Set {impl ( is(x, N), ( x = y ))} 2. 1 |- true } thm T1 {true} proof T1$1 {1. T$1:1 |- true};""", "∀ x, y:Set {(is(x, N)) ⇒ (x = y)}", 1)>]
    [<TestMethod>]
    member this.TestProceedingExpressionJustByProofArgument(no:string, fplCode, expectedExpr:string, expectedNumbExpr:int) =
        
        let filename = "TestProceedingExpressionJustByProofArgument"
        prepareFplCode(filename + ".fpl", fplCode, false) 
        let r = heap.Root

        let candidates = findCandidatesByName "T1" false true
        let prf = candidates |> List.filter (fun fv -> fv.FplId = "T1$1") |> List.map (fun fv -> fv :?> FplProof) |> List.head

        let fvJiOpt = tryFindJustification prf PrimJIByProofArgument

        match fvJiOpt with
        | Some (:? FplJustificationItemByProofArgument as fvJi) ->
            let result = fvJi.ProceedingExprCandidates
            Assert.AreEqual<int>(expectedNumbExpr, result.Length)
            Assert.AreEqual<string>(expectedExpr, result.Head.Type SignatureType.Name)
        | Some ref ->
            Assert.IsInstanceOfType<FplJustificationItemByProofArgument>(ref)
        | None ->
            failwith $"expected FplJustificationItemByProofArgument, found none"

        prepareFplCode(filename, "", false) |> ignore


    [<DataRow("00", """thm T {true} proof T$1 {1. |- trivial };""", "true", 1)>]
    [<DataRow("01", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" thm T {all n,m:Nat { impl( ( Succ(n) = Succ(m) ), ( n = m ) ) }} thm T {true} prf T$1 { 1. |- trivial };""", "∀ m, n:Nat {((n') = (m')) ⇒ (n = m)}", 1)>]
    [<DataRow("02", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" thm T {all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }} thm T {true} prf T$1 { 1. |- trivial };""", "∀ m, n:Nat {((n') = (m')) ⇒ (n = m)}", 1)>]
    [<TestMethod>]
    member this.TestProceedingExpressionInfTrivial(no:string, fplCode, expectedExpr:string, expectedNumbExpr:int) =
        
        let filename = "TestProceedingExpressionInfTrivial"
        prepareFplCode(filename + ".fpl", fplCode, false) 
        let r = heap.Root

        let candidates = findCandidatesByName "T" false true
        let prf = candidates |> List.filter (fun fv -> fv.FplId = "T$1") |> List.map (fun fv -> fv :?> FplProof) |> List.head
        let fvJiOpt = tryFindInference (prf:FplProof) PrimArgInfTrivial 

        match fvJiOpt with
        | Some infTrivial ->
            let result = infTrivial.ProceedingExprCandidates
            Assert.AreEqual<int>(expectedNumbExpr, result.Length)
            Assert.AreEqual<string>(expectedExpr, result.Head.Type SignatureType.Name)
        | None ->
            failwith $"expected FplJustificationItemByAx, found none"

        prepareFplCode(filename, "", false) |> ignore
