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
type TestProceedingExpressionsJust() =

    let tryFindJustification (prf:FplProof) justType = 
        prf.OrderedArguments
        |> List.map (fun fv -> fv.Justification)
        |> List.filter (fun fv -> fv.IsSome)
        |> List.map (fun fv -> fv.Value)
        |> List.map (fun fv -> fv :?> FplJustification)
        |> List.map (fun fv -> fv.GetOrderedJustificationItems)
        |> List.concat 
        |> List.tryFind (fun fv -> fv.Name = justType)


    [<DataRow("00", """ax A {true} thm T {true} prf T$1 { 1. byax A |- true }""", "true", 1)>]
    [<DataRow("01", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" ax A {all n,m:Nat { impl( ( Succ(n) = Succ(m) ), ( n = m ) ) }} thm T {true} prf T$1 { 1. byax A |- true }""", "∀ m, n:Nat {((n') = (m')) ⇒ (n = m)}", 1)>]
    [<DataRow("02", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" ax A {all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }} thm T {true} prf T$1 { 1. byax A |- true }""", "∀ m, n:Nat {((n') = (m')) ⇒ (n = m)}", 1)>]
    [<DataRow("03", """ax A {∃!1 x:obj{true}} thm T {true} prf T$1 { 1. byax A |- true }""", "∃! x:obj {true}", 1)>]
    [<DataRow("04", """ax A {∃! x:obj{true}} thm T {true} prf T$1 { 1. byax A |- true }""", "∃! x:obj {true}", 1)>]
    [<DataRow("03", """ax A {∃!3 x:obj{true}} thm T {true} prf T$1 { 1. byax A |- true }""", "∃!3 x:obj {true}", 1)>]
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

    [<DataRow("00", """conj A {true} thm T {true} prf T$1 { 1. byconj A |- true }""", "true", 1)>]
    [<DataRow("01", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" conj A {all n,m:Nat { impl( ( Succ(n) = Succ(m) ), ( n = m ) ) }} thm T {true} prf T$1 { 1. byconj A |- true }""", "∀ m, n:Nat {((n') = (m')) ⇒ (n = m)}", 1)>]
    [<DataRow("02", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" conj A {all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }} thm T {true} prf T$1 { 1. byconj A |- true }""", "∀ m, n:Nat {((n') = (m')) ⇒ (n = m)}", 1)>]
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


    [<DataRow("00thm", """thm A {true} thm T {true} prf T$1 { 1. A |- true }""", "true", 1)>]
    [<DataRow("01thm", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" thm A {all n,m:Nat { impl( ( Succ(n) = Succ(m) ), ( n = m ) ) }} thm T {true} prf T$1 { 1. A |- true }""", "∀ m, n:Nat {((n') = (m')) ⇒ (n = m)}", 1)>]
    [<DataRow("02thm", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" thm A {all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }} thm T {true} prf T$1 { 1. A |- true }""", "∀ m, n:Nat {((n') = (m')) ⇒ (n = m)}", 1)>]
    [<DataRow("00lem", """lem A {true} thm T {true} prf T$1 { 1. A |- true }""", "true", 1)>]
    [<DataRow("01lem", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" lem A {all n,m:Nat { impl( ( Succ(n) = Succ(m) ), ( n = m ) ) }} thm T {true} prf T$1 { 1. A |- true }""", "∀ m, n:Nat {((n') = (m')) ⇒ (n = m)}", 1)>]
    [<DataRow("02lem", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" lem A {all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }} thm T {true} prf T$1 { 1. A |- true }""", "∀ m, n:Nat {((n') = (m')) ⇒ (n = m)}", 1)>]
    [<DataRow("00prop", """prop A {true} thm T {true} prf T$1 { 1. A |- true }""", "true", 1)>]
    [<DataRow("01prop", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" prop A {all n,m:Nat { impl( ( Succ(n) = Succ(m) ), ( n = m ) ) }} thm T {true} prf T$1 { 1. A |- true }""", "∀ m, n:Nat {((n') = (m')) ⇒ (n = m)}", 1)>]
    [<DataRow("02prop", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" prop A {all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }} thm T {true} prf T$1 { 1. A |- true }""", "∀ m, n:Nat {((n') = (m')) ⇒ (n = m)}", 1)>]
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


    [<DataRow("00cor", """cor A$1 {true} thm T {true} prf T$1 { 1. bycor A$1 |- true }""", "true", 1)>]
    [<DataRow("01cor", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" cor A$1 {all n,m:Nat { impl( ( Succ(n) = Succ(m) ), ( n = m ) ) }} thm T {true} prf T$1 { 1. bycor A$1 |- true }""", "∀ m, n:Nat {((n') = (m')) ⇒ (n = m)}", 1)>]
    [<DataRow("02cor", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" cor A$1 {all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }} thm T {true} prf T$1 { 1. bycor A$1 |- true }""", "∀ m, n:Nat {((n') = (m')) ⇒ (n = m)}", 1)>]
    [<DataRow("00corthm", """thm A {true} cor A$1 {true} thm T {true} prf T$1 { 1. bycor A$1 |- true }""", "true", 1)>]
    [<DataRow("02corthm", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" thm A {true} cor A$1 {all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }} thm T {true} prf T$1 { 1. bycor A$1 |- true }""", "∀ m, n:Nat {((n') = (m')) ⇒ (n = m)}", 1)>]
    [<DataRow("00corlem", """lem A {true} cor A$1 {true} thm T {true} prf T$1 { 1. bycor A$1 |- true }""", "true", 1)>]
    [<DataRow("02corlem", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" lem A {true} cor A$1 {all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }} lem T {true} prf T$1 { 1. bycor A$1 |- true }""", "∀ m, n:Nat {((n') = (m')) ⇒ (n = m)}", 1)>]
    [<DataRow("00corprop", """prop A {true} cor A$1 {true} thm T {true} prf T$1 { 1. bycor A$1 |- true }""", "true", 1)>]
    [<DataRow("02corprop", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" prop A {true} cor A$1 {all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }} prop T {true} prf T$1 { 1. bycor A$1 |- true }""", "∀ m, n:Nat {((n') = (m')) ⇒ (n = m)}", 1)>]
    [<DataRow("00corax", """ax A {true} cor A$1 {true} thm T {true} prf T$1 { 1. bycor A$1 |- true }""", "true", 1)>]
    [<DataRow("02corax", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" ax A {true} cor A$1 {all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }} prop T {true} prf T$1 { 1. bycor A$1 |- true }""", "∀ m, n:Nat {((n') = (m')) ⇒ (n = m)}", 1)>]
    [<DataRow("00corconj", """conj A {true} cor A$1 {true} thm T {true} prf T$1 { 1. bycor A$1 |- true }""", "true", 1)>]
    [<DataRow("02corconj", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" conj A {true} cor A$1 {all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }} prop T {true} prf T$1 { 1. bycor A$1 |- true }""", "∀ m, n:Nat {((n') = (m')) ⇒ (n = m)}", 1)>]
    [<TestMethod>]
    member this.TestProceedingExpressionJustByCor(no:string, fplCode, expectedExpr:string, expectedNumbExpr:int) =
        
        let filename = "TestProceedingExpressionJustByCor"
        prepareFplCode(filename + ".fpl", fplCode, false) 
        let r = heap.Root

        let candidates = findCandidatesByName "T" false true
        let prf = candidates |> List.filter (fun fv -> fv.FplId = "T$1") |> List.map (fun fv -> fv :?> FplProof) |> List.head

        let fvJiOpt = tryFindJustification prf PrimJIByCor

        match fvJiOpt with
        | Some (:? FplJustificationItemByCor as fvJi) ->
            let result = fvJi.ProceedingExprCandidates
            Assert.AreEqual<int>(expectedNumbExpr, result.Length)
            Assert.AreEqual<string>(expectedExpr, result.Head.Type SignatureType.Name)
        | Some ref ->
            Assert.IsInstanceOfType<FplJustificationItemByCor>(ref)
        | None ->
            failwith $"expected FplJustificationItemByCor, found none"

        prepareFplCode(filename, "", false) |> ignore

    [<DataRow("00cl0", """def cl A thm T {true} prf T$1 { 1. bydef A |- true }""", "undet", 1)>]
    [<DataRow("00cl1", """def cl A1 def cl A { dec assert is(self, A1); ctor A() {} } thm T {true} prf T$1 { 1. bydef A |- true }""", "A is A1", 1)>] // one assertion
    [<DataRow("00cl2", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" def cl A1 def cl A { dec assert is(self, A1) assert all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }; ctor A() {} } thm T {true} prf T$1 { 1. bydef A |- true }""", "∀ m, n:Nat {((n') = (m')) ⇒ (n = m)}", 2)>] // two assertions
    [<DataRow("00cl3", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" def cl A1 def cl A { dec assert is(self, A1) assert all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }; ctor A() {} prty pred J() {ex x:obj {is(x,Nat)}} } thm T {true} prf T$1 { 1. bydef A |- true }""", "∃ x:obj {x is Nat}", 3)>] // two assertions + predicative property
    [<DataRow("00pr0", """def pred A() thm T {true} prf T$1 { 1. bydef A |- true }""", "undet", 1)>]
    [<DataRow("00pr0a", """def cl Nat def pred A() {ex x:obj {is(x,Nat)}} thm T {true} prf T$1 { 1. bydef A |- true }""", "∃ x:obj {x is Nat}", 1)>]
    [<DataRow("00pr1", """def cl A1 def pred A() { dec assert is(self, A1); true} thm T {true} prf T$1 { 1. bydef A |- true }""", "A() is A1", 2)>] // one assertion + predicate itself
    [<DataRow("00pr2", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" def cl A1 def pred A() { dec assert is(self, A1) assert all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }; true } thm T {true} prf T$1 { 1. bydef A |- true }""", "∀ m, n:Nat {((n') = (m')) ⇒ (n = m)}", 3)>] // two assertions + predicate itself
    [<DataRow("00pr3", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" def cl A1 def pred A() { dec assert is(self, A1) assert all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }; true prty pred J() {ex x:obj {is(x,Nat)}} } thm T {true} prf T$1 { 1. bydef A |- true }""", "∃ x:obj {x is Nat}", 4)>] // two assertions + predicate itself + predicative property
    [<DataRow("00fu0", """def func A()->obj thm T {true} prf T$1 { 1. bydef A |- true }""", "undet", 1)>]
    [<DataRow("00fu0a", """def cl Nat def func A()->ind {dec assert ex x:obj {is(x,Nat)}; ret $1} thm T {true} prf T$1 { 1. bydef A |- true }""", "∃ x:obj {x is Nat}", 1)>]
    [<DataRow("00fu1", """def cl A1 def func A()->ind { dec assert is(self, A1); ret $1} thm T {true} prf T$1 { 1. bydef A |- true }""", "(A() -> ind) is A1", 1)>] // one assertion 
    [<DataRow("00fu2", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" def cl A1 def func A()->obj { dec assert is(self, A1) assert all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }; ret $1 } thm T {true} prf T$1 { 1. bydef A |- true }""", "∀ m, n:Nat {((n') = (m')) ⇒ (n = m)}", 2)>] // two assertions 
    [<DataRow("00fu3", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" def cl A1 def func A()->obj { dec assert is(self, A1) assert all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }; ret $1 prty pred J() {ex x:obj {is(x,Nat)}} } thm T {true} prf T$1 { 1. bydef A |- true }""", "∃ x:obj {x is Nat}", 3)>] // two assertions + predicative property
    [<TestMethod>]
    member this.TestProceedingExpressionJustByDef(no:string, fplCode, expectedExpr:string, expectedNumbExpr:int) =
        
        let filename = "TestProceedingExpressionJustByDef"
        prepareFplCode(filename + ".fpl", fplCode, false) 
        let r = heap.Root

        let candidates = findCandidatesByName "T" false true
        let prf = candidates |> List.filter (fun fv -> fv.FplId = "T$1") |> List.map (fun fv -> fv :?> FplProof) |> List.head

        let fvJiOpt = tryFindJustification prf PrimJIByDef

        match fvJiOpt with
        | Some (:? FplJustificationItemByDef as fvJi) ->
            let result = fvJi.ProceedingExprCandidates
            Assert.AreEqual<int>(expectedNumbExpr, result.Length)
            let expr = result |> List.rev |> List.head
            Assert.AreEqual<string>(expectedExpr, expr.Type SignatureType.Name)
        | Some ref ->
            Assert.IsInstanceOfType<FplJustificationItemByDef>(ref)
        | None ->
            failwith $"expected FplJustificationItemByDef, found none"

        prepareFplCode(filename, "", false) |> ignore

    [<DataRow("00cl0", """def cl A thm T {dec v:A; true} prf T$1 { 1. bydef v |- true }""", "undet", 1)>]
    [<DataRow("00cl1", """def cl A1 def cl A { dec assert is(self, A1); ctor A() {} } thm T {dec v:A; true} prf T$1 { 1. bydef v |- true }""", "A is A1", 1)>] // one assertion
    [<DataRow("00cl2", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" def cl A1 def cl A { dec assert is(self, A1) assert all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }; ctor A() {} } thm T {dec v:A; true} prf T$1 { 1. bydef v |- true }""", "∀ m, n:Nat {((n') = (m')) ⇒ (n = m)}", 2)>] // two assertions
    [<DataRow("00cl3", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" def cl A1 def cl A { dec assert is(self, A1) assert all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }; ctor A() {} prty pred J() {ex x:obj {is(x,Nat)}} } thm T {dec v:A; true} prf T$1 { 1. bydef v |- true }""", "∃ x:obj {x is Nat}", 3)>] // two assertions + predicative property
    [<DataRow("00pr0", """def pred A() thm T {dec v:A; true} prf T$1 { 1. bydef v |- true }""", "undet", 1)>]
    [<DataRow("00pr0a", """def cl Nat def pred A() {ex x:obj {is(x,Nat)}} thm T {dec v:A; true} prf T$1 { 1. bydef v |- true }""", "∃ x:obj {x is Nat}", 1)>]
    [<DataRow("00pr1", """def cl A1 def pred A() { dec assert is(self, A1); true} thm T {dec v:A; true} prf T$1 { 1. bydef v |- true }""", "A() is A1", 2)>] // one assertion + predicate itself
    [<DataRow("00pr2", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" def cl A1 def pred A() { dec assert is(self, A1) assert all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }; true } thm T {dec v:A; true} prf T$1 { 1. bydef v |- true }""", "∀ m, n:Nat {((n') = (m')) ⇒ (n = m)}", 3)>] // two assertions + predicate itself
    [<DataRow("00pr3", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" def cl A1 def pred A() { dec assert is(self, A1) assert all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }; true prty pred J() {ex x:obj {is(x,Nat)}} } thm T {dec v:A; true} prf T$1 { 1. bydef v |- true }""", "∃ x:obj {x is Nat}", 4)>] // two assertions + predicate itself + predicative property
    [<DataRow("00fu0", """def func A()->obj thm T {dec v:A; true} prf T$1 { 1. bydef v |- true }""", "undet", 1)>]
    [<DataRow("00fu0a", """def cl Nat def func A()->ind {dec assert ex x:obj {is(x,Nat)}; ret $1} thm T {dec v:A; true} prf T$1 { 1. bydef v |- true }""", "∃ x:obj {x is Nat}", 1)>]
    [<DataRow("00fu1", """def cl A1 def func A()->ind { dec assert is(self, A1); ret $1} thm T {dec v:A; true} prf T$1 { 1. bydef v |- true }""", "(A() -> ind) is A1", 1)>] // one assertion 
    [<DataRow("00fu2", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" def cl A1 def func A()->obj { dec assert is(self, A1) assert all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }; ret $1 } thm T {dec v:A; true} prf T$1 { 1. bydef v |- true }""", "∀ m, n:Nat {((n') = (m')) ⇒ (n = m)}", 2)>] // two assertions 
    [<DataRow("00fu3", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" def cl A1 def func A()->obj { dec assert is(self, A1) assert all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }; ret $1 prty pred J() {ex x:obj {is(x,Nat)}} } thm T {dec v:A; true} prf T$1 { 1. bydef v |- true }""", "∃ x:obj {x is Nat}", 3)>] // two assertions + predicative property
    [<TestMethod>]
    member this.TestProceedingExpressionJustByDefVar(no:string, fplCode, expectedExpr:string, expectedNumbExpr:int) =
        
        let filename = "TestProceedingExpressionJustByDefVar"
        prepareFplCode(filename + ".fpl", fplCode, false) 
        let r = heap.Root

        let candidates = findCandidatesByName "T" false true
        let prf = candidates |> List.filter (fun fv -> fv.FplId = "T$1") |> List.map (fun fv -> fv :?> FplProof) |> List.head

        let fvJiOpt = tryFindJustification prf PrimJIByDefVar

        match fvJiOpt with
        | Some (:? FplJustificationItemByDefVar as fvJi) ->
            let result = fvJi.ProceedingExprCandidates
            Assert.AreEqual<int>(expectedNumbExpr, result.Length)
            let expr = result |> List.rev |> List.head
            Assert.AreEqual<string>(expectedExpr, expr.Type SignatureType.Name)
        | Some ref ->
            Assert.IsInstanceOfType<FplJustificationItemByDefVar>(ref)
        | None ->
            failwith $"expected FplJustificationItemByDefVar, found none"

        prepareFplCode(filename, "", false) |> ignore

    [<DataRow("00", """def cl N thm T {true} prf T$1 { 1: ex x:obj {is(x,N)} 2. 1 |- trivial}""", "∃ x:obj {x is N}", 1)>]
    [<DataRow("01", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" ax A {true} thm T {true} prf T$1 { 1. byax A |- all x,y:Set {impl ( is(x, N), ( x = y ))} 2. 1 |- true }""", "∀ x, y:Set {(x is N) ⇒ (x = y)}", 1)>]
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

    [<DataRow("00", """def cl N thm T {true} prf T$1 { 1: ex x:obj {is(x,N)} 2. 1 |- trivial} thm T1 {true} prf T1$1 { 1. T$1:1 |- trivial }""", "∃ x:obj {x is N}", 1)>]
    [<DataRow("01", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" ax A {true} thm T {true} prf T$1 { 1. byax A |- all x,y:Set {impl ( is(x, N), ( x = y ))} 2. 1 |- true } thm T1 {true} proof T1$1 {1. T$1:1 |- true}""", "∀ x, y:Set {(x is N) ⇒ (x = y)}", 1)>]
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


    // AndCummutative and(p,q) 
    [<DataRow("AndC_01", "inf AndCummutative{dec p,q:pred; pre:and(p,q) con:and(q,p)} thm T {true} proof T$1 {1: and(true,false) 2. 1, byinf AndCummutative |- and(false,true)}", "false ∧ true", 1)>]
    [<DataRow("AndC_03", "inf AndCummutative{dec p,q:pred; pre:and(p,q) con:and(q,p)} thm T {true} proof T$1 {1: and(all x:obj {is(x,N)}, not(all x:obj {is(x,N)})) 2. 1, byinf AndCummutative |- and(not(all x:obj {is(x,N)}), all x:obj {is(x,N)})}", "¬∀ x:obj {x is N} ∧ ∀ x:obj {x is N}", 1)>]

    // ModusPonens and (p, impl (p,q) )
    [<DataRow("MP_01", "inf M {dec p,q: pred; pre: and (p, impl (p,q) ) con: q} thm T {true} proof T$1 {1: and(true, impl(true, false)) 2. 1, byinf M |- false}", "false", 1)>]
    [<DataRow("MP_01d", "inf M {dec p,q: pred; pre: and (p, impl (p,q) ) con: q} thm T {true} proof T$1 {1: and(ex x:obj {is(x,N)}, impl(ex x:obj {is(x,N)}, false)) 2. 1, byinf M |- false}", "false", 1)>]
    [<DataRow("MP_01e", "inf M {dec p,q: pred; pre: and (p, impl (p,q) ) con: q} thm T {true} proof T$1 {1: and(all x:obj {is(x,N)}, impl(all x:obj {is(x,N)}, false)) 2. 1, byinf M |- false}", "false", 1)>]
    [<DataRow("MP_01f", "inf M {dec p,q: pred; pre: and (p, impl (p,q) ) con: q} thm T {true} proof T$1 {1: and(exn$1 x:obj {is(x,N)}, impl(exn$1 x:obj {is(x,N)}, false)) 2. 1, byinf M |- false}", "false", 1)>]
    [<DataRow("MP_01g", "inf M {dec p,q: pred; pre: and (p, impl (p,q) ) con: q} thm T {true} proof T$1 {1: and(and(exn$1 x:obj {is(x,N)}, true), impl(and(exn$1 x:obj {is(x,N)}, true), false)) 2. 1, byinf M |- false}", "false", 1)>]
    [<DataRow("MP_01h", "inf M {dec p,q: pred; pre: and (p, impl (p,q) ) con: q} thm T {true} proof T$1 {1: and(not (ex x:obj {is(x,N)}), impl(not (ex x:obj {is(x,N)}), false)) 2. 1, byinf M |- false}", "false", 1)>]
    [<DataRow("MP_01j", "inf M {dec p,q: pred; pre: and (p, impl (p,q) ) con: q} thm T {true} proof T$1 {1: and(ex x:obj {is(x,N)}, impl(ex x:obj {is(x,N)}, xor(true,false))) 2. 1, byinf M |- false}", "true ⩡ false", 1)>]
    [<DataRow("MP_01k", "inf M {dec p,q: pred; pre: and (p, impl (p,q) ) con: q} thm T {true} proof T$1 {1: and(iif(true, ex x:obj {is(x,N)}), impl(iif(true, ex x:obj {is(x,N)}), false)) 2. 1, byinf M |- false}", "false", 1)>]
    [<DataRow("MP_01m", "inf M {dec p,q: pred; pre: and (p, impl (p,q) ) con: q} thm T {true} proof T$1 {1: and(is(A,N), impl(is(A,N), false)) 2. 1, byinf M |- false}", "false", 1)>]


    // OrCummutative or(p,q) 
    [<DataRow("OrC_01", "inf OrCummutative{dec p,q:pred; pre:or(p,q) con:or(q,p)} thm T {true} proof T$1 {1: or(true,false) 2. 1, byinf OrCummutative |- or(false,true)}", "false ∨ true", 1)>]
    [<DataRow("OrC_03", "inf OrCummutative{dec p,q:pred; pre:or(p,q) con:or(q,p)} thm T {true} proof T$1 {1: or(ex x:obj {is(x,N)}, iif(true,false)) 2. 1, byinf OrCummutative |- or(iif(true,false), ex x:obj {is(x,N)})}", "(true ⇔ false) ∨ ∃ x:obj {x is N}", 1)>]

    // IifCummutative iif(p,q)
    [<DataRow("IifC_01", "inf IifCummutative{dec p,q:pred; pre:iif(p,q) con:iif(q,p)} thm T {true} proof T$1 {1: iif(true,false) 2. 1, byinf IifCummutative |- iif(false,true)}", "false ⇔ true", 1)>]
    [<DataRow("IifC_03", "inf IifCummutative{dec p,q:pred; pre:iif(p,q) con:iif(q,p)} thm T {true} proof T$1 {1: iif(iif(true,ex x:obj {is(x,N)}), xor(true,false)) 2. 1, byinf IifCummutative |- iif(xor(true,false), iif(true,ex x:obj {is(x,N)}))}", "(true ⩡ false) ⇔ (true ⇔ ∃ x:obj {x is N})", 1)>]

    // AndAssociative and(p,and(q,s)) 
    [<DataRow("AndA_01", "inf AndAssociative{dec p,q,s:pred; pre:and(p,and(q,s)) con:and(and(p,q),s)} thm T {true} proof T$1 {1: and(true, and(false, true)) 2. 1, byinf AndAssociative |- and(and(true,false), true)}", "(true ∧ false) ∧ true", 1)>]
    [<DataRow("AndA_02a", "inf AndAssociative{dec p,q,s:pred; pre:and(p,and(q,s)) con:and(and(p,q),s)} thm T {true} proof T$1 {1: and(true,and(true,false)) 2. 1, byinf AndAssociative |- and(and(true,false), true)}", "(true ∧ true) ∧ false", 1)>]
    [<DataRow("AndA_03", "inf AndAssociative{dec p,q,s:pred; pre:and(p,and(q,s)) con:and(and(p,q),s)} thm T {true} proof T$1 {1: and(all x:obj {is(x,N)}, and(ex y:obj {is(y,M)}, true)) 2. 1, byinf AndAssociative |- and(and(all x:obj {is(x,N)}, ex y:obj {is(y,M)}), true)}", "(∀ x:obj {x is N} ∧ ∃ y:obj {y is M}) ∧ true", 1)>]

    // OrAssociative or(p,or(q,s))
    [<DataRow("OrA_01", "inf OrAssociative{dec p,q,s:pred; pre:or(p,or(q,s)) con:or(or(p,q),s)} thm T {true} proof T$1 {1: or(true, or(false, true)) 2. 1, byinf OrAssociative |- or(or(true,false), true)}", "(true ∨ false) ∨ true", 1)>]
    [<DataRow("OrA_03", "inf OrAssociative{dec p,q,s:pred; pre:or(p,or(q,s)) con:or(or(p,q),s)} thm T {true} proof T$1 {1: or(ex x:obj {is(x,N)}, or(iif(true,false), true)) 2. 1, byinf OrAssociative |- or(or(ex x:obj {is(x,N)}, iif(true,false)), true)}", "(∃ x:obj {x is N} ∨ (true ⇔ false)) ∨ true", 1)>]

    // IifAssociative iif(p,iif(q,s))
    [<DataRow("IifA_01", "inf IifAssociative{dec p,q,s:pred; pre:iif(p,iif(q,s)) con:iif(iif(p,q),s)} thm T {true} proof T$1 {1: iif(true, iif(false, true)) 2. 1, byinf IifAssociative |- iif(iif(true,false), true)}", "(true ⇔ false) ⇔ true", 1)>]
    [<DataRow("IifA_03", "inf IifAssociative{dec p,q,s:pred; pre:iif(p,iif(q,s)) con:iif(iif(p,q),s)} thm T {true} proof T$1 {1: iif(ex x:obj {is(x,N)}, iif(iif(true,false), ex y:obj {is(y,M)})) 2. 1, byinf IifAssociative |- iif(iif(ex x:obj {is(x,N)}, iif(true,false)), ex y:obj {is(y,M)})}", "(∃ x:obj {x is N} ⇔ (true ⇔ false)) ⇔ ∃ y:obj {y is M}", 1)>]

    // FalseAndAbsorbing and(false,p)
    [<DataRow("FAbs_01", "inf FalseAndAbsorbing{dec p:pred; pre:and(false,p) con:false} thm T {true} proof T$1 {1: and(false,true) 2. 1, byinf FalseAndAbsorbing |- false}", "false", 1)>]
    [<DataRow("FAbs_03", "inf FalseAndAbsorbing{dec p:pred; pre:and(false,p) con:false} thm T {true} proof T$1 {1: and(false, ex x:obj {is(x,N)}) 2. 1, byinf FalseAndAbsorbing |- false}", "false", 1)>]

    // OrAndAbsorbing: pre: or(p, and(p, q))
    [<DataRow("OrAndAbsorbing_01", "inf OrAndAbsorbing{dec p,q:pred; pre:or(p,and(p,q)) con:p} thm T {true} proof T$1 {1: or(and(true, not(false)), and(and(true, not(false)), xor(false, true))) 2. 1, byinf OrAndAbsorbing |- true}", "true ∧ ¬false", 1)>]
    [<DataRow("OrAndAbsorbing_02", "inf OrAndAbsorbing{dec p,q:pred; pre:or(p,and(p,q)) con:p} thm T {true} proof T$1 {1: or(iif(true, not(false)), and(iif(true, not(false)), ex n:obj {is(n, N)})) 2. 1, byinf OrAndAbsorbing |- true}", "true ⇔ ¬false", 1)>]
    [<DataRow("OrAndAbsorbing_03", "inf OrAndAbsorbing{dec p,q:pred; pre:or(p,and(p,q)) con:p} thm T {true} proof T$1 {1: or(all x:obj {is(x, N)}, and(all x:obj {is(x, N)}, impl(true, false))) 2. 1, byinf OrAndAbsorbing |- true}", "∀ x:obj {x is N}", 1)>]

    // AndOrAbsorbing: pre: and(p, or(p, q))
    [<DataRow("AndOrAbsorbing_01", "inf AndOrAbsorbing{dec p,q:pred; pre:and(p,or(p,q)) con:p} thm T {true} proof T$1 {1: and(xor(true, false), or(xor(true, false), not(false))) 2. 1, byinf AndOrAbsorbing |- true}", "true ⩡ false", 1)>]
    [<DataRow("AndOrAbsorbing_02", "inf AndOrAbsorbing{dec p,q:pred; pre:and(p,or(p,q)) con:p} thm T {true} proof T$1 {1: and(not(and(true, false)), or(not(and(true, false)), all y:obj {is(y, N)})) 2. 1, byinf AndOrAbsorbing |- true}", "¬(true ∧ false)", 1)>]
    [<DataRow("AndOrAbsorbing_03", "inf AndOrAbsorbing{dec p,q:pred; pre:and(p,or(p,q)) con:p} thm T {true} proof T$1 {1: and(ex n:obj {is(n, N)}, or(ex n:obj {is(n, N)}, iif(true, false))) 2. 1, byinf AndOrAbsorbing |- true}", "∃ n:obj {n is N}", 1)>]

    // AndTrueNeutral: pre: and(true, p)
    [<DataRow("AndTrueNeutral_01", "inf AndTrueNeutral{dec p:pred; pre:and(true,p) con:p} thm T {true} proof T$1 {1: and(true, and(xor(true, false), not(false))) 2. 1, byinf AndTrueNeutral |- true}", "(true ⩡ false) ∧ ¬false", 1)>]
    [<DataRow("AndTrueNeutral_02", "inf AndTrueNeutral{dec p:pred; pre:and(true,p) con:p} thm T {true} proof T$1 {1: and(true, ex n:obj {is(n, N)}) 2. 1, byinf AndTrueNeutral |- true}", "∃ n:obj {n is N}", 1)>]
    [<DataRow("AndTrueNeutral_03", "inf AndTrueNeutral{dec p:pred; pre:and(true,p) con:p} thm T {true} proof T$1 {1: and(true, iif(or(true, false), not(false))) 2. 1, byinf AndTrueNeutral |- true}", "(true ∨ false) ⇔ ¬false", 1)>]

    // OrFalseNeutral: pre: or(false, p)
    [<DataRow("OrFalseNeutral_01", "inf OrFalseNeutral{dec p:pred; pre:or(false,p) con:p} thm T {true} proof T$1 {1: or(false, and(true, not(false))) 2. 1, byinf OrFalseNeutral |- true}", "true ∧ ¬false", 1)>]
    [<DataRow("OrFalseNeutral_02", "inf OrFalseNeutral{dec p:pred; pre:or(false,p) con:p} thm T {true} proof T$1 {1: or(false, ex n:obj {is(n, N)}) 2. 1, byinf OrFalseNeutral |- true}", "∃ n:obj {n is N}", 1)>]
    [<DataRow("OrFalseNeutral_03", "inf OrFalseNeutral{dec p:pred; pre:or(false,p) con:p} thm T {true} proof T$1 {1: or(false, iif(true, xor(false, true))) 2. 1, byinf OrFalseNeutral |- true}", "true ⇔ (false ⩡ true)", 1)>]

    // AndInversion: pre: and(p, not p)
    [<DataRow("AndInversion_01", "inf AndInversion{dec p:pred; pre:and(p,not p) con:false} thm T {true} proof T$1 {1: and(or(true, false), not(or(true, false))) 2. 1, byinf AndInversion |- true}", "false", 1)>]
    [<DataRow("AndInversion_02", "inf AndInversion{dec p:pred; pre:and(p,not p) con:false} thm T {true} proof T$1 {1: and(all x:obj {is(x, N)}, not(all x:obj {is(x, N)})) 2. 1, byinf AndInversion |- true}", "false", 1)>]
    [<DataRow("AndInversion_03", "inf AndInversion{dec p:pred; pre:and(p,not p) con:false} thm T {true} proof T$1 {1: and(ex n:obj {is(n, N)}, not(ex n:obj {is(n, N)})) 2. 1, byinf AndInversion |- true}", "false", 1)>]

    // OrInversion: pre: or(p, not p)
    [<DataRow("OrInversion_01", "inf OrInversion{dec p:pred; pre:or(p,not p) con:true} thm T {true} proof T$1 {1: or(impl(true, false), not(impl(true, false))) 2. 1, byinf OrInversion |- true}", "true", 1)>]
    [<DataRow("OrInversion_02", "inf OrInversion{dec p:pred; pre:or(p,not p) con:true} thm T {true} proof T$1 {1: or(xor(true, false), not(xor(true, false))) 2. 1, byinf OrInversion |- true}", "true", 1)>]
    [<DataRow("OrInversion_03", "inf OrInversion{dec p:pred; pre:or(p,not p) con:true} thm T {true} proof T$1 {1: or(all x:obj {is(x, N)}, not(all x:obj {is(x, N)})) 2. 1, byinf OrInversion |- true}", "true", 1)>]

    // AndIdempotence: pre: and(p, p)
    [<DataRow("AndIdempotence_01", "inf AndIdempotence{dec p:pred; pre:and(p,p) con:p} thm T {true} proof T$1 {1: and(or(true, false), or(true, false)) 2. 1, byinf AndIdempotence |- true}", "true ∨ false", 1)>]
    [<DataRow("AndIdempotence_02", "inf AndIdempotence{dec p:pred; pre:and(p,p) con:p} thm T {true} proof T$1 {1: and(all x:obj {is(x, N)}, all x:obj {is(x, N)}) 2. 1, byinf AndIdempotence |- true}",  "∀ x:obj {x is N}", 1)>]
    [<DataRow("AndIdempotence_03", "inf AndIdempotence{dec p:pred; pre:and(p,p) con:p} thm T {true} proof T$1 {1: and(iif(true, false), iif(true, false)) 2. 1, byinf AndIdempotence |- true}", "true ⇔ false", 1)>]

    // OrIdempotence: pre: or(p, p)
    [<DataRow("OrIdempotence_01", "inf OrIdempotence{dec p:pred; pre:or(p,p) con:p} thm T {true} proof T$1 {1: or(and(true, false), and(true, false)) 2. 1, byinf OrIdempotence |- true}", "true ∧ false", 1)>]
    [<DataRow("OrIdempotence_02", "inf OrIdempotence{dec p:pred; pre:or(p,p) con:p} thm T {true} proof T$1 {1: or(ex n:obj {is(n, N)}, ex n:obj {is(n, N)}) 2. 1, byinf OrIdempotence |- true}", "∃ n:obj {n is N}", 1)>]
    [<DataRow("OrIdempotence_03", "inf OrIdempotence{dec p:pred; pre:or(p,p) con:p} thm T {true} proof T$1 {1: or(xor(true, false), xor(true, false)) 2. 1, byinf OrIdempotence |- true}", "true ⩡ false", 1)>]

    // OrAndDistributiveUnpack: pre: or(p, and(q, s))
    [<DataRow("OrAndDistributiveUnpack_01", "inf OrAndDistributiveUnpack{dec p,q,s:pred; pre:or(p,and(q,s)) con:and(or(p,q),or(p,s))} thm T {true} proof T$1 {1: or(iif(true, false), and(and(true, not(false)), xor(false, true))) 2. 1, byinf OrAndDistributiveUnpack |- true}", "((true ⇔ false) ∨ (true ∧ ¬false)) ∧ ((true ⇔ false) ∨ (false ⩡ true))", 1)>]
    [<DataRow("OrAndDistributiveUnpack_02", "inf OrAndDistributiveUnpack{dec p,q,s:pred; pre:or(p,and(q,s)) con:and(or(p,q),or(p,s))} thm T {true} proof T$1 {1: or(all x:obj {is(x, N)}, and(ex y:obj {is(y, M)}, or(true, false))) 2. 1, byinf OrAndDistributiveUnpack |- true}", "(∀ x:obj {x is N} ∨ ∃ y:obj {y is M}) ∧ (∀ x:obj {x is N} ∨ (true ∨ false))", 1)>]
    [<DataRow("OrAndDistributiveUnpack_03", "inf OrAndDistributiveUnpack{dec p,q,s:pred; pre:or(p,and(q,s)) con:and(or(p,q),or(p,s))} thm T {true} proof T$1 {1: or(not(xor(true, false)), and(iif(true, true), all z:obj {is(z, N)})) 2. 1, byinf OrAndDistributiveUnpack |- true}", "(¬(true ⩡ false) ∨ (true ⇔ true)) ∧ (¬(true ⩡ false) ∨ ∀ z:obj {z is N})", 1)>]

    // AndOrDistributivePack: pre: and(or(p, q), or(p, s))
    [<DataRow("AndOrDistributivePack_01", "inf AndOrDistributivePack{dec p,q,s:pred; pre:and(or(p,q),or(p,s)) con:or(p,and(q,s))} thm T {true} proof T$1 {1: and(or(iif(true, false), xor(true, false)), or(iif(true, false), and(ex n:obj {is(n, N)}, true))) 2. 1, byinf AndOrDistributivePack |- true}", "(true ⇔ false) ∨ ((true ⩡ false) ∧ (∃ n:obj {n is N} ∧ true))", 1)>]
    [<DataRow("AndOrDistributivePack_02", "inf AndOrDistributivePack{dec p,q,s:pred; pre:and(or(p,q),or(p,s)) con:or(p,and(q,s))} thm T {true} proof T$1 {1: and(or(all x:obj {is(x, N)}, not(false)), or(all x:obj {is(x, N)}, xor(true, false))) 2. 1, byinf AndOrDistributivePack |- true}", "∀ x:obj {x is N} ∨ (¬false ∧ (true ⩡ false))", 1)>]
    [<DataRow("AndOrDistributivePack_03", "inf AndOrDistributivePack{dec p,q,s:pred; pre:and(or(p,q),or(p,s)) con:or(p,and(q,s))} thm T {true} proof T$1 {1: and(or(not(true), impl(true, false)), or(not(true), ex y:obj {is(y, N)})) 2. 1, byinf AndOrDistributivePack |- true}", "¬true ∨ ((true ⇒ false) ∧ ∃ y:obj {y is N})", 1)>]

    // AndOrDistributiveUnpack: pre: and(p, or(q, s))
    [<DataRow("AndOrDistributiveUnpack_01", "inf AndOrDistributiveUnpack{dec p,q,s:pred; pre:and(p,or(q,s)) con:or(and(p,q),and(p,s))} thm T {true} proof T$1 {1: and(iif(true, ex x:obj {is(x, N)}), or(and(true, not(false)), xor(false, true))) 2. 1, byinf AndOrDistributiveUnpack |- true}", "((true ⇔ ∃ x:obj {x is N}) ∧ (true ∧ ¬false)) ∨ ((true ⇔ ∃ x:obj {x is N}) ∧ (false ⩡ true))", 1)>]
    [<DataRow("AndOrDistributiveUnpack_02", "inf AndOrDistributiveUnpack{dec p,q,s:pred; pre:and(p,or(q,s)) con:or(and(p,q),and(p,s))} thm T {true} proof T$1 {1: and(all x:obj {is(x, N)}, or(ex y:obj {is(y, M)}, impl(true, false))) 2. 1, byinf AndOrDistributiveUnpack |- true}", "(∀ x:obj {x is N} ∧ ∃ y:obj {y is M}) ∨ (∀ x:obj {x is N} ∧ (true ⇒ false))", 1)>]
    [<DataRow("AndOrDistributiveUnpack_03", "inf AndOrDistributiveUnpack{dec p,q,s:pred; pre:and(p,or(q,s)) con:or(and(p,q),and(p,s))} thm T {true} proof T$1 {1: and(not(xor(true, false)), or(iif(true, false), and(exn$1 z:obj {is(z, N)}, true))) 2. 1, byinf AndOrDistributiveUnpack |- true}", "(¬(true ⩡ false) ∧ (true ⇔ false)) ∨ (¬(true ⩡ false) ∧ (∃! z:obj {z is N} ∧ true))", 1)>]

    // OrAndDistributivePack: pre: or(and(p, q), and(p, s))
    [<DataRow("OrAndDistributivePack_01", "inf OrAndDistributivePack{dec p,q,s:pred; pre:or(and(p,q),and(p,s)) con:and(p,or(q,s))} thm T {true} proof T$1 {1: or(and(all x:obj {is(x, N)}, not(false)), and(all x:obj {is(x, N)}, ex y:obj {is(y, M)})) 2. 1, byinf OrAndDistributivePack |- true}", "∀ x:obj {x is N} ∧ (¬false ∨ ∃ y:obj {y is M})", 1)>]
    [<DataRow("OrAndDistributivePack_02", "inf OrAndDistributivePack{dec p,q,s:pred; pre:or(and(p,q),and(p,s)) con:and(p,or(q,s))} thm T {true} proof T$1 {1: or(and(iif(true, false), and(true, not(false))), and(iif(true, false), impl(true, false))) 2. 1, byinf OrAndDistributivePack |- true}", "(true ⇔ false) ∧ ((true ∧ ¬false) ∨ (true ⇒ false))", 1)>]
    [<DataRow("OrAndDistributivePack_03", "inf OrAndDistributivePack{dec p,q,s:pred; pre:or(and(p,q),and(p,s)) con:and(p,or(q,s))} thm T {true} proof T$1 {1: or(and(ex x:obj {is(x, N)}, or(true, false)), and(ex x:obj {is(x, N)}, xor(true, false))) 2. 1, byinf OrAndDistributivePack |- true}", "∃ x:obj {x is N} ∧ ((true ∨ false) ∨ (true ⩡ false))", 1)>]

    // DeMorganAndUnpack: pre: not and(p, q)
    [<DataRow("DeMorganAndUnpack_01", "inf DeMorganAndUnpack{dec p,q:pred; pre:not and(p,q) con:or(not p,not q)} thm T {true} proof T$1 {1: not and(not(true), xor(true, false)) 2. 1, byinf DeMorganAndUnpack |- true}", "¬¬true ∨ ¬(true ⩡ false)", 1)>]
    [<DataRow("DeMorganAndUnpack_02", "inf DeMorganAndUnpack{dec p,q:pred; pre:not and(p,q) con:or(not p,not q)} thm T {true} proof T$1 {1: not and(all x:obj {is(x, N)}, ex y:obj {not is(y, M)}) 2. 1, byinf DeMorganAndUnpack |- true}", "¬∀ x:obj {x is N} ∨ ¬∃ y:obj {¬(y is M)}", 1)>]
    [<DataRow("DeMorganAndUnpack_03", "inf DeMorganAndUnpack{dec p,q:pred; pre:not and(p,q) con:or(not p,not q)} thm T {true} proof T$1 {1: not and(iif(true, false), impl(true, false)) 2. 1, byinf DeMorganAndUnpack |- true}", "¬(true ⇔ false) ∨ ¬(true ⇒ false)", 1)>]

    // DeMorganOrPack: pre: or(not p, not q)
    [<DataRow("DeMorganOrPack_01", "inf DeMorganOrPack{dec p,q:pred; pre:or(not p,not q) con:not and(p,q)} thm T {true} proof T$1 {1: or(not (and(true, false)), not (ex x:obj {is(x, N)})) 2. 1, byinf DeMorganOrPack |- true}", "¬((true ∧ false) ∧ ∃ x:obj {x is N})", 1)>]
    [<DataRow("DeMorganOrPack_02", "inf DeMorganOrPack{dec p,q:pred; pre:or(not p,not q) con:not and(p,q)} thm T {true} proof T$1 {1: or(not (iif(true, false)), not (all x:obj {is(x, N)})) 2. 1, byinf DeMorganOrPack |- true}", "¬((true ⇔ false) ∧ ∀ x:obj {x is N})", 1)>]
    [<DataRow("DeMorganOrPack_03", "inf DeMorganOrPack{dec p,q:pred; pre:or(not p,not q) con:not and(p,q)} thm T {true} proof T$1 {1: or(not (xor(true, false)), not (impl(true, false))) 2. 1, byinf DeMorganOrPack |- true}", "¬((true ⩡ false) ∧ (true ⇒ false))", 1)>]

    // DeMorganOrUnpack: pre: not or(p, q)
    [<DataRow("DeMorganOrUnpack_01", "inf DeMorganOrUnpack{dec p,q:pred; pre:not or(p,q) con:and(not p,not q)} thm T {true} proof T$1 {1: not or(iif(true, false), ex x:obj {is(x, N)}) 2. 1, byinf DeMorganOrUnpack |- true}", "¬(true ⇔ false) ∧ ¬∃ x:obj {x is N}", 1)>]
    [<DataRow("DeMorganOrUnpack_02", "inf DeMorganOrUnpack{dec p,q:pred; pre:not or(p,q) con:and(not p,not q)} thm T {true} proof T$1 {1: not or(all x:obj {is(x, N)}, xor(true, false)) 2. 1, byinf DeMorganOrUnpack |- true}", "¬∀ x:obj {x is N} ∧ ¬(true ⩡ false)", 1)>]
    [<DataRow("DeMorganOrUnpack_03", "inf DeMorganOrUnpack{dec p,q:pred; pre:not or(p,q) con:and(not p,not q)} thm T {true} proof T$1 {1: not or(impl(true, false), and(true, false)) 2. 1, byinf DeMorganOrUnpack |- true}", "¬(true ⇒ false) ∧ ¬(true ∧ false)", 1)>]

    // DeMorganAndPack: pre: and(not p, not q)
    [<DataRow("DeMorganAndPack_01", "inf DeMorganAndPack{dec p,q:pred; pre:and(not p,not q) con:not or(p,q)} thm T {true} proof T$1 {1: and(not (iif(true, false)), not (ex x:obj {is(x, N)})) 2. 1, byinf DeMorganAndPack |- true}", "¬((true ⇔ false) ∨ ∃ x:obj {x is N})", 1)>]
    [<DataRow("DeMorganAndPack_02", "inf DeMorganAndPack{dec p,q:pred; pre:and(not p,not q) con:not or(p,q)} thm T {true} proof T$1 {1: and(not (all x:obj {is(x, N)}), not (xor(true, false))) 2. 1, byinf DeMorganAndPack |- true}", "¬(∀ x:obj {x is N} ∨ (true ⩡ false))", 1)>]
    [<DataRow("DeMorganAndPack_03", "inf DeMorganAndPack{dec p,q:pred; pre:and(not p,not q) con:not or(p,q)} thm T {true} proof T$1 {1: and(not (impl(true, false)), not (and(true, false))) 2. 1, byinf DeMorganAndPack |- true}", "¬((true ⇒ false) ∨ (true ∧ false))", 1)>]

    // NotDouble: pre: not not p
    [<DataRow("NotDouble_01", "inf NotDouble{dec p:pred; pre:not not p con:p} thm T {true} proof T$1 {1: not not (and(true, xor(false, true))) 2. 1, byinf NotDouble |- true}", "true ∧ (false ⩡ true)", 1)>]
    [<DataRow("NotDouble_02", "inf NotDouble{dec p:pred; pre:not not p con:p} thm T {true} proof T$1 {1: not not (ex x:obj {not is(x, N)}) 2. 1, byinf NotDouble |- true}", "∃ x:obj {¬(x is N)}", 1)>]
    [<DataRow("NotDouble_03", "inf NotDouble{dec p:pred; pre:not not p con:p} thm T {true} proof T$1 {1: not not (iif(true, ex y:obj {is(y, M)})) 2. 1, byinf NotDouble |- true}", "true ⇔ ∃ y:obj {y is M}", 1)>]

    // ImplUnpack2Or: pre: impl(p, q)
    [<DataRow("ImplUnpack2Or_01", "inf ImplUnpack2Or{dec p,q:pred; pre:impl(p,q) con:or(not p,q)} thm T {true} proof T$1 {1: impl(all x:obj {is(x, N)}, ex y:obj {is(y, M)}) 2. 1, byinf ImplUnpack2Or |- true}", "¬∀ x:obj {x is N} ∨ ∃ y:obj {y is M}", 1)>]
    [<DataRow("ImplUnpack2Or_02", "inf ImplUnpack2Or{dec p,q:pred; pre:impl(p,q) con:or(not p,q)} thm T {true} proof T$1 {1: impl(not (xor(true, false)), iif(true, false)) 2. 1, byinf ImplUnpack2Or |- true}", "¬¬(true ⩡ false) ∨ (true ⇔ false)", 1)>]
    [<DataRow("ImplUnpack2Or_03", "inf ImplUnpack2Or{dec p,q:pred; pre:impl(p,q) con:or(not p,q)} thm T {true} proof T$1 {1: impl(and(true, not(false)), or(ex x:obj {is(x, N)}, true)) 2. 1, byinf ImplUnpack2Or |- true}", "¬(true ∧ ¬false) ∨ (∃ x:obj {x is N} ∨ true)", 1)>]

    // OrPack2Impl: pre: or(not p, q)
    [<DataRow("OrPack2Impl_01", "inf OrPack2Impl{dec p,q:pred; pre:or(not p,q) con:impl(p,q)} thm T {true} proof T$1 {1: or(not all x:obj {is(x, N)}, ex y:obj {is(y, M)}) 2. 1, byinf OrPack2Impl |- true}", "∀ x:obj {x is N} ⇒ ∃ y:obj {y is M}", 1)>]
    [<DataRow("OrPack2Impl_02", "inf OrPack2Impl{dec p,q:pred; pre:or(not p,q) con:impl(p,q)} thm T {true} proof T$1 {1: or(not (iif(true, false)), and(true, false)) 2. 1, byinf OrPack2Impl |- true}", "(true ⇔ false) ⇒ (true ∧ false)", 1)>]
    [<DataRow("OrPack2Impl_03", "inf OrPack2Impl{dec p,q:pred; pre:or(not p,q) con:impl(p,q)} thm T {true} proof T$1 {1: or(not (xor(true, false)), impl(true, false)) 2. 1, byinf OrPack2Impl |- true}", "(true ⩡ false) ⇒ (true ⇒ false)", 1)>]

    // IifUnpack2Or: pre: iif(p, q)
    [<DataRow("IifUnpack2Or_01", "inf IifUnpack2Or{dec p,q:pred; pre:iif(p,q) con:or(and(not p,not q),and(p,q))} thm T {true} proof T$1 {1: iif(all x:obj {is(x, N)}, ex y:obj {is(y, M)}) 2. 1, byinf IifUnpack2Or |- true}", "(¬∀ x:obj {x is N} ∧ ¬∃ y:obj {y is M}) ∨ (∀ x:obj {x is N} ∧ ∃ y:obj {y is M})", 1)>]
    [<DataRow("IifUnpack2Or_02", "inf IifUnpack2Or{dec p,q:pred; pre:iif(p,q) con:or(and(not p,not q),and(p,q))} thm T {true} proof T$1 {1: iif(not (xor(true, false)), and(true, false)) 2. 1, byinf IifUnpack2Or |- true}", "(¬¬(true ⩡ false) ∧ ¬(true ∧ false)) ∨ (¬(true ⩡ false) ∧ (true ∧ false))", 1)>]
    [<DataRow("IifUnpack2Or_03", "inf IifUnpack2Or{dec p,q:pred; pre:iif(p,q) con:or(and(not p,not q),and(p,q))} thm T {true} proof T$1 {1: iif(iif(true, false), xor(true, false)) 2. 1, byinf IifUnpack2Or |- true}", "(¬(true ⇔ false) ∧ ¬(true ⩡ false)) ∨ ((true ⇔ false) ∧ (true ⩡ false))", 1)>]

    // IifUnpack2And: pre: iif(p, q)
    [<DataRow("IifUnpack2And_01", "inf IifUnpack2And{dec p,q:pred; pre:iif(p,q) con:and(or(not p,q),or(p,not q))} thm T {true} proof T$1 {1: iif(ex x:obj {is(x, N)}, iif(true, false)) 2. 1, byinf IifUnpack2And |- true}", "(¬∃ x:obj {x is N} ∨ (true ⇔ false)) ∧ (∃ x:obj {x is N} ∨ ¬(true ⇔ false))", 1)>]
    [<DataRow("IifUnpack2And_02", "inf IifUnpack2And{dec p,q:pred; pre:iif(p,q) con:and(or(not p,q),or(p,not q))} thm T {true} proof T$1 {1: iif(all x:obj {is(x, N)}, xor(true, false)) 2. 1, byinf IifUnpack2And |- true}", "(¬∀ x:obj {x is N} ∨ (true ⩡ false)) ∧ (∀ x:obj {x is N} ∨ ¬(true ⩡ false))", 1)>]
    [<DataRow("IifUnpack2And_03", "inf IifUnpack2And{dec p,q:pred; pre:iif(p,q) con:and(or(not p,q),or(p,not q))} thm T {true} proof T$1 {1: iif(not (and(true, false)), or(true, false)) 2. 1, byinf IifUnpack2And |- true}", "(¬¬(true ∧ false) ∨ (true ∨ false)) ∧ (¬(true ∧ false) ∨ ¬(true ∨ false))", 1)>]

    // OrPack2Iif: pre: or(and(not p, not q), and(p, q))
    [<DataRow("OrPack2Iif_01", "inf OrPack2Iif{dec p,q:pred; pre:or(and(not p,not q),and(p,q)) con:iif(p,q)} thm T {true} proof T$1 {1: or(and(not all x:obj {is(x, N)}, not ex y:obj {is(y, M)}), and(all x:obj {is(x, N)}, ex y:obj {is(y, M)})) 2. 1, byinf OrPack2Iif |- true}", "∀ x:obj {x is N} ⇔ ∃ y:obj {y is M}", 1)>]
    [<DataRow("OrPack2Iif_02", "inf OrPack2Iif{dec p,q:pred; pre:or(and(not p,not q),and(p,q)) con:iif(p,q)} thm T {true} proof T$1 {1: or(and(not iif(true, false), not xor(true, false)), and(iif(true, false), xor(true, false))) 2. 1, byinf OrPack2Iif |- true}", "(true ⇔ false) ⇔ (true ⩡ false)", 1)>]
    [<DataRow("OrPack2Iif_03", "inf OrPack2Iif{dec p,q:pred; pre:or(and(not p,not q),and(p,q)) con:iif(p,q)} thm T {true} proof T$1 {1: or(and(not (impl(true, false)), not (and(true, false))), and(impl(true, false), and(true, false))) 2. 1, byinf OrPack2Iif |- true}", "(true ⇒ false) ⇔ (true ∧ false)", 1)>]

    // AndPack2Iif: pre: and(or(not p, q), or(p, not q))
    [<DataRow("AndPack2Iif_02", "inf AndPack2Iif{dec p,q:pred; pre:and(or(not p,q),or(p,not q)) con:iif(p,q)} thm T {true} proof T$1 {1: and(or(not iif(true, false), xor(true, false)), or(iif(true, false), not xor(true, false))) 2. 1, byinf AndPack2Iif |- true}", "(true ⇔ false) ⇔ (true ⩡ false)", 1)>]
    [<DataRow("AndPack2Iif_03", "inf AndPack2Iif{dec p,q:pred; pre:and(or(not p,q),or(p,not q)) con:iif(p,q)} thm T {true} proof T$1 {1: and(or(not (impl(true, false)), and(true, false)), or(impl(true, false), not and(true, false))) 2. 1, byinf AndPack2Iif |- true}", "(true ⇒ false) ⇔ (true ∧ false)", 1)>]

    // AndPack2Xor: pre: and(or(not p, not q), or(p, q))
    [<DataRow("AndPack2Xor_01", "inf AndPack2Xor{dec p,q:pred; pre:and(or(not p,not q),or(p,q)) con:xor(p,q)} thm T {true} proof T$1 {1: and(or(not all x:obj {is(x, N)}, not ex y:obj {is(y, M)}), or(all x:obj {is(x, N)}, ex y:obj {is(y, M)})) 2. 1, byinf AndPack2Xor |- true}", "∀ x:obj {x is N} ⩡ ∃ y:obj {y is M}", 1)>]
    [<DataRow("AndPack2Xor_02", "inf AndPack2Xor{dec p,q:pred; pre:and(or(not p,not q),or(p,q)) con:xor(p,q)} thm T {true} proof T$1 {1: and(or(not iif(true, false), not xor(true, false)), or(iif(true, false), xor(true, false))) 2. 1, byinf AndPack2Xor |- true}", "(true ⇔ false) ⩡ (true ⩡ false)", 1)>]
    [<DataRow("AndPack2Xor_03", "inf AndPack2Xor{dec p,q:pred; pre:and(or(not p,not q),or(p,q)) con:xor(p,q)} thm T {true} proof T$1 {1: and(or(not (impl(true, false)), not (and(true, false))), or(impl(true, false), and(true, false))) 2. 1, byinf AndPack2Xor |- true}", "(true ⇒ false) ⩡ (true ∧ false)", 1)>]

    // AndUnpack2NotOr: pre: and(p, q)
    [<DataRow("AndUnpack2NotOr_01", "inf AndUnpack2NotOr{dec p,q:pred; pre:and(p,q) con:not (or(not p,not q))} thm T {true} proof T$1 {1: and(all x:obj {is(x, N)}, ex y:obj {is(y, M)}) 2. 1, byinf AndUnpack2NotOr |- true}", "¬(¬∀ x:obj {x is N} ∨ ¬∃ y:obj {y is M})", 1)>]
    [<DataRow("AndUnpack2NotOr_02", "inf AndUnpack2NotOr{dec p,q:pred; pre:and(p,q) con:not (or(not p,not q))} thm T {true} proof T$1 {1: and(iif(true, false), xor(true, false)) 2. 1, byinf AndUnpack2NotOr |- true}", "¬(¬(true ⇔ false) ∨ ¬(true ⩡ false))", 1)>]
    [<DataRow("AndUnpack2NotOr_03", "inf AndUnpack2NotOr{dec p,q:pred; pre:and(p,q) con:not (or(not p,not q))} thm T {true} proof T$1 {1: and(not (ex x:obj {is(x, N)}), impl(true, false)) 2. 1, byinf AndUnpack2NotOr |- true}", "¬(¬¬∃ x:obj {x is N} ∨ ¬(true ⇒ false))", 1)>]

    // NotOrPack2And: pre: not (or(not p, not q))
    [<DataRow("NotOrPack2And_01", "inf NotOrPack2And{dec p,q:pred; pre:not (or(not p,not q)) con:and(p,q)} thm T {true} proof T$1 {1: not (or(not all x:obj {is(x, N)}, not ex y:obj {is(y, M)})) 2. 1, byinf NotOrPack2And |- true}", "∀ x:obj {x is N} ∧ ∃ y:obj {y is M}", 1)>]
    [<DataRow("NotOrPack2And_02", "inf NotOrPack2And{dec p,q:pred; pre:not (or(not p,not q)) con:and(p,q)} thm T {true} proof T$1 {1: not (or(not iif(true, false), not xor(true, false))) 2. 1, byinf NotOrPack2And |- true}", "(true ⇔ false) ∧ (true ⩡ false)", 1)>]
    [<DataRow("NotOrPack2And_03", "inf NotOrPack2And{dec p,q:pred; pre:not (or(not p,not q)) con:and(p,q)} thm T {true} proof T$1 {1: not (or(not impl(true, false), not and(true, false))) 2. 1, byinf NotOrPack2And |- true}", "(true ⇒ false) ∧ (true ∧ false)", 1)>]

    // AndUnpack2NotImpl: pre: and(p, q)
    [<DataRow("AndUnpack2NotImpl_01", "inf AndUnpack2NotImpl{dec p,q:pred; pre:and(p,q) con:not (impl(p,not q))} thm T {true} proof T$1 {1: and(all x:obj {is(x, N)}, iif(true, false)) 2. 1, byinf AndUnpack2NotImpl |- true}", "¬(∀ x:obj {x is N} ⇒ ¬(true ⇔ false))", 1)>]
    [<DataRow("AndUnpack2NotImpl_02", "inf AndUnpack2NotImpl{dec p,q:pred; pre:and(p,q) con:not (impl(p,not q))} thm T {true} proof T$1 {1: and(ex x:obj {is(x, M)}, not (xor(true, false))) 2. 1, byinf AndUnpack2NotImpl |- true}", "¬(∃ x:obj {x is M} ⇒ ¬¬(true ⩡ false))", 1)>]
    [<DataRow("AndUnpack2NotImpl_03", "inf AndUnpack2NotImpl{dec p,q:pred; pre:and(p,q) con:not (impl(p,not q))} thm T {true} proof T$1 {1: and(not (impl(true, false)), all z:obj {is(z, K)}) 2. 1, byinf AndUnpack2NotImpl |- true}", "¬(¬(true ⇒ false) ⇒ ¬∀ z:obj {z is K})", 1)>]

    // NotImplPack2And: pre: not (impl(p, not q))
    [<DataRow("NotImplPack2And_01", "inf NotImplPack2And{dec p,q:pred; pre:not (impl(p,not q)) con:and(p,q)} thm T {true} proof T$1 {1: not (impl(all x:obj {is(x, N)}, not ex y:obj {is(y, M)})) 2. 1, byinf NotImplPack2And |- true}", "∀ x:obj {x is N} ∧ ∃ y:obj {y is M}", 1)>]
    [<DataRow("NotImplPack2And_02", "inf NotImplPack2And{dec p,q:pred; pre:not (impl(p,not q)) con:and(p,q)} thm T {true} proof T$1 {1: not (impl(iif(true, false), not xor(true, false))) 2. 1, byinf NotImplPack2And |- true}", "(true ⇔ false) ∧ (true ⩡ false)", 1)>]
    [<DataRow("NotImplPack2And_03", "inf NotImplPack2And{dec p,q:pred; pre:not (impl(p,not q)) con:and(p,q)} thm T {true} proof T$1 {1: not (impl(and(true, false), not iif(true, false))) 2. 1, byinf NotImplPack2And |- true}", "(true ∧ false) ∧ (true ⇔ false)", 1)>]

    // NotImpl2And: pre: not (impl(p, q))
    [<DataRow("NotImpl2And_01", "inf NotImpl2And{dec p,q:pred; pre:not (impl(p,q)) con:and(p,not q)} thm T {true} proof T$1 {1: not (impl(all x:obj {is(x, N)}, ex y:obj {is(y, M)})) 2. 1, byinf NotImpl2And |- true}", "∀ x:obj {x is N} ∧ ¬∃ y:obj {y is M}", 1)>]
    [<DataRow("NotImpl2And_02", "inf NotImpl2And{dec p,q:pred; pre:not (impl(p,q)) con:and(p,not q)} thm T {true} proof T$1 {1: not (impl(iif(true, false), xor(true, false))) 2. 1, byinf NotImpl2And |- true}", "(true ⇔ false) ∧ ¬(true ⩡ false)", 1)>]
    [<DataRow("NotImpl2And_03", "inf NotImpl2And{dec p,q:pred; pre:not (impl(p,q)) con:and(p,not q)} thm T {true} proof T$1 {1: not (impl(and(true, false), not (ex x:obj {is(x, N)}))) 2. 1, byinf NotImpl2And |- true}", "(true ∧ false) ∧ ¬¬∃ x:obj {x is N}", 1)>]

    // And2NotImpl: pre: and(p, not q)
    [<DataRow("And2NotImpl_01", "inf And2NotImpl{dec p,q:pred; pre:and(p,not q) con:not (impl(p,q))} thm T {true} proof T$1 {1: and(all x:obj {is(x, N)}, not ex y:obj {is(y, M)}) 2. 1, byinf And2NotImpl |- true}", "¬(∀ x:obj {x is N} ⇒ ∃ y:obj {y is M})", 1)>]
    [<DataRow("And2NotImpl_02", "inf And2NotImpl{dec p,q:pred; pre:and(p,not q) con:not (impl(p,q))} thm T {true} proof T$1 {1: and(iif(true, false), not xor(true, false)) 2. 1, byinf And2NotImpl |- true}", "¬((true ⇔ false) ⇒ (true ⩡ false))", 1)>]
    [<DataRow("And2NotImpl_03", "inf And2NotImpl{dec p,q:pred; pre:and(p,not q) con:not (impl(p,q))} thm T {true} proof T$1 {1: and(not (impl(true, false)), not all z:obj {is(z, K)}) 2. 1, byinf And2NotImpl |- true}", "¬(¬(true ⇒ false) ⇒ ∀ z:obj {z is K})", 1)>]

    // NotIif2Or: pre: not (iif(p, q))
    [<DataRow("NotIif2Or_01", "inf NotIif2Or{dec p,q:pred; pre:not (iif(p,q)) con:or(and(p,not q),and(not p,q))} thm T {true} proof T$1 {1: not (iif(all x:obj {is(x, N)}, ex y:obj {is(y, M)})) 2. 1, byinf NotIif2Or |- true}", "(∀ x:obj {x is N} ∧ ¬∃ y:obj {y is M}) ∨ (¬∀ x:obj {x is N} ∧ ∃ y:obj {y is M})", 1)>]
    [<DataRow("NotIif2Or_02", "inf NotIif2Or{dec p,q:pred; pre:not (iif(p,q)) con:or(and(p,not q),and(not p,q))} thm T {true} proof T$1 {1: not (iif(iif(true, false), xor(true, false))) 2. 1, byinf NotIif2Or |- true}", "((true ⇔ false) ∧ ¬(true ⩡ false)) ∨ (¬(true ⇔ false) ∧ (true ⩡ false))", 1)>]
    [<DataRow("NotIif2Or_03", "inf NotIif2Or{dec p,q:pred; pre:not (iif(p,q)) con:or(and(p,not q),and(not p,q))} thm T {true} proof T$1 {1: not (iif(not (and(true, false)), impl(true, false))) 2. 1, byinf NotIif2Or |- true}", "(¬(true ∧ false) ∧ ¬(true ⇒ false)) ∨ (¬¬(true ∧ false) ∧ (true ⇒ false))", 1)>]

    // Or2NotIif: pre: or(and(p, not q), and(not p, q))
    [<DataRow("Or2NotIif_01", "inf Or2NotIif{dec p,q:pred; pre:or(and(p,not q),and(not p,q)) con:not (iif(p,q))} thm T {true} proof T$1 {1: or(and(all x:obj {is(x, N)}, not ex y:obj {is(y, M)}), and(not all x:obj {is(x, N)}, ex y:obj {is(y, M)})) 2. 1, byinf Or2NotIif |- true}", "¬(∀ x:obj {x is N} ⇔ ∃ y:obj {y is M})", 1)>]
    [<DataRow("Or2NotIif_02", "inf Or2NotIif{dec p,q:pred; pre:or(and(p,not q),and(not p,q)) con:not (iif(p,q))} thm T {true} proof T$1 {1: or(and(iif(true, false), not xor(true, false)), and(not iif(true, false), xor(true, false))) 2. 1, byinf Or2NotIif |- true}", "¬((true ⇔ false) ⇔ (true ⩡ false))", 1)>]
    [<DataRow("Or2NotIif_03", "inf Or2NotIif{dec p,q:pred; pre:or(and(p,not q),and(not p,q)) con:not (iif(p,q))} thm T {true} proof T$1 {1: or(and(impl(true, false), not all z:obj {is(z, K)}), and(not (impl(true, false)), all z:obj {is(z, K)})) 2. 1, byinf Or2NotIif |- true}", "¬((true ⇒ false) ⇔ ∀ z:obj {z is K})", 1)>]

    // NotXor2Or: pre: not (xor(p, q))
    [<DataRow("NotXor2Or_01", "inf NotXor2Or{dec p,q:pred; pre:not (xor(p,q)) con:or(and(p,q),and(not p,not q))} thm T {true} proof T$1 {1: not (xor(all x:obj {is(x, N)}, ex y:obj {is(y, M)})) 2. 1, byinf NotXor2Or |- true}", "(∀ x:obj {x is N} ∧ ∃ y:obj {y is M}) ∨ (¬∀ x:obj {x is N} ∧ ¬∃ y:obj {y is M})", 1)>]
    [<DataRow("NotXor2Or_02", "inf NotXor2Or{dec p,q:pred; pre:not (xor(p,q)) con:or(and(p,q),and(not p,not q))} thm T {true} proof T$1 {1: not (xor(iif(true, false), and(true, false))) 2. 1, byinf NotXor2Or |- true}", "((true ⇔ false) ∧ (true ∧ false)) ∨ (¬(true ⇔ false) ∧ ¬(true ∧ false))", 1)>]
    [<DataRow("NotXor2Or_03", "inf NotXor2Or{dec p,q:pred; pre:not (xor(p,q)) con:or(and(p,q),and(not p,not q))} thm T {true} proof T$1 {1: not (xor(not (impl(true, false)), xor(true, false))) 2. 1, byinf NotXor2Or |- true}", "(¬(true ⇒ false) ∧ (true ⩡ false)) ∨ (¬¬(true ⇒ false) ∧ ¬(true ⩡ false))", 1)>]

    // Or2NotXor: pre: or(and(p, q), and(not p, not q))
    [<DataRow("Or2NotXor_01", "inf Or2NotXor{dec p,q:pred; pre:or(and(p,q),and(not p,not q)) con:not (xor(p,q))} thm T {true} proof T$1 {1: or(and(all x:obj {is(x, N)}, ex y:obj {is(y, M)}), and(not all x:obj {is(x, N)}, not ex y:obj {is(y, M)})) 2. 1, byinf Or2NotXor |- true}", "¬(∀ x:obj {x is N} ⩡ ∃ y:obj {y is M})", 1)>]
    [<DataRow("Or2NotXor_02", "inf Or2NotXor{dec p,q:pred; pre:or(and(p,q),and(not p,not q)) con:not (xor(p,q))} thm T {true} proof T$1 {1: or(and(iif(true, false), xor(true, false)), and(not iif(true, false), not xor(true, false))) 2. 1, byinf Or2NotXor |- true}", "¬((true ⇔ false) ⩡ (true ⩡ false))", 1)>]
    [<DataRow("Or2NotXor_03", "inf Or2NotXor{dec p,q:pred; pre:or(and(p,q),and(not p,not q)) con:not (xor(p,q))} thm T {true} proof T$1 {1: or(and(impl(true, false), and(true, false)), and(not (impl(true, false)), not (and(true, false)))) 2. 1, byinf Or2NotXor |- true}", "¬((true ⇒ false) ⩡ (true ∧ false))", 1)>]

    // NotAll2ExNot: pre: not all x1:tpl {p(x1)}
    [<DataRow("NotAll2ExNot_01", "inf NotAll2ExNot{dec p:pred(x:tpl); pre:not all x1:tpl{p(x1)} con:ex x1:tpl{not p(x1)}} thm T {true} proof T$1 {1: not all x1:obj {is(x1, N)} 2. 1, byinf NotAll2ExNot |- true}", "∃ x1:obj {¬(x1 is N)}", 1)>]
    [<DataRow("NotAll2ExNot_02", "inf NotAll2ExNot{dec p:pred(x:tpl); pre:not all x1:tpl{p(x1)} con:ex x1:tpl{not p(x1)}} thm T {true} proof T$1 {1: not all n:Nat {impl(is(n, N), xor(true, false))} 2. 1, byinf NotAll2ExNot |- true}", "∃ n:Nat {¬((n is N) ⇒ (true ⩡ false))}", 1)>]
    [<DataRow("NotAll2ExNot_03", "inf NotAll2ExNot{dec p:pred(x:tpl); pre:not all x1:tpl{p(x1)} con:ex x1:tpl{not p(x1)}} thm T {true} proof T$1 {1: not all y:obj {not is(y, M)} 2. 1, byinf NotAll2ExNot |- true}", "∃ y:obj {¬¬(y is M)}", 1)>]

    // ExNot2NotAll: pre: ex x:tpl{not p(x)}
    [<DataRow("ExNot2NotAll_01", "inf ExNot2NotAll{dec p:pred(y:tpl); pre:ex x:tpl{not p(x)} con:not all x:tpl{p(x)}} thm T {true} proof T$1 {1: ex x:obj {not iif(is(x,N), true)} 2. 1, byinf ExNot2NotAll |- true}", "¬∀ x:obj {(x is N) ⇔ true}", 1)>]
    [<DataRow("ExNot2NotAll_02", "inf ExNot2NotAll{dec p:pred(y:tpl); pre:ex x:tpl{not p(x)} con:not all x:tpl{p(x)}} thm T {true} proof T$1 {1: ex n:Nat {not and(is(n,N), xor(true,false))} 2. 1, byinf ExNot2NotAll |- true}", "¬∀ n:Nat {(n is N) ∧ (true ⩡ false)}", 1)>]

    // NotEx2AllNot: pre: not ex x:tpl{p(x)}
    [<DataRow("NotEx2AllNot_01", "inf NotEx2AllNot{dec p:pred(y:tpl); pre:not ex x:tpl{p(x)} con:all x:tpl{not p(x)}} thm T {true} proof T$1 {1: not ex x:obj {iif(is(x,N), false)} 2. 1, byinf NotEx2AllNot |- true}", "∀ x:obj {¬((x is N) ⇔ false)}", 1)>]
    [<DataRow("NotEx2AllNot_02", "inf NotEx2AllNot{dec p:pred(y:tpl); pre:not ex x:tpl{p(x)} con:all x:tpl{not p(x)}} thm T {true} proof T$1 {1: not ex n:Nat {and(not is(n,N), xor(true,false))} 2. 1, byinf NotEx2AllNot |- true}", "∀ n:Nat {¬(¬(n is N) ∧ (true ⩡ false))}", 1)>]

    // AllNot2ExNot: pre: all x:tpl{not p(x)}
    [<DataRow("AllNot2ExNot_01", "inf AllNot2ExNot{dec p:pred(y:tpl); pre:all x:tpl{not p(x)} con:not ex x:tpl{p(x)}} thm T {true} proof T$1 {1: all x:obj {not iif(is(x,N), true)} 2. 1, byinf AllNot2ExNot |- true}", "¬∃ x:obj {(x is N) ⇔ true}", 1)>]
    [<DataRow("AllNot2ExNot_02", "inf AllNot2ExNot{dec p:pred(y:tpl); pre:all x:tpl{not p(x)} con:not ex x:tpl{p(x)}} thm T {true} proof T$1 {1: all n:Nat {not (and(is(n,N), xor(true,false)))} 2. 1, byinf AllNot2ExNot |- true}", "¬∃ n:Nat {(n is N) ∧ (true ⩡ false)}", 1)>]

    // ImplPack2Or: pre: impl(not p,q)
    [<DataRow("ImplPack2Or_01", "inf ImplPack2Or{dec p,q:pred; pre:impl(not p,q) con:or(p,q)} thm T {true} proof T$1 {1: impl(not (and(is(A,N), true)), ex x:obj {is(x,M)}) 2. 1, byinf ImplPack2Or |- true}", "((A is N) ∧ true) ∨ ∃ x:obj {x is M}", 1)>]
    [<DataRow("ImplPack2Or_02", "inf ImplPack2Or{dec p,q:pred; pre:impl(not p,q) con:or(p,q)} thm T {true} proof T$1 {1: impl(not (xor(true,false)), iif(all x:obj {is(x,N)}, false)) 2. 1, byinf ImplPack2Or |- true}", "(true ⩡ false) ∨ (∀ x:obj {x is N} ⇔ false)", 1)>]
    [<DataRow("ImplPack2Or_03", "inf ImplPack2Or{dec p,q:pred; pre:impl(not p,q) con:or(p,q)} thm T {true} proof T$1 {1: impl(not (iif(true,false)), or(ex y:obj {is(y,M)}, true)) 2. 1, byinf ImplPack2Or |- true}", "(true ⇔ false) ∨ (∃ y:obj {y is M} ∨ true)", 1)>]

    // ModusTollens: pre: not q, impl(p,q)
    [<DataRow("ModusTollens_01", "inf ModusTollens{dec p,q:pred; pre:not q,impl(p,q) con:not (p)} thm T {true} proof T$1 {1: not (iif(true,false)) 2: impl(all x:obj { is(x,N) }, iif(true,false)) 3. 1, 2, byinf ModusTollens |- true }", "¬∀ x:obj {x is N}", 1)>]
    [<DataRow("ModusTollens_02a", "inf ModusTollens{dec p,q:pred; pre:not q,impl(p,q) con:not (p)} thm T {true} proof T$1 {1: not ex x:obj { is(x,N) } 2: impl(xor(true,false), ex x:obj { is(x,N) }) 3. 1, 2, byinf ModusTollens |- true }", "¬(true ⩡ false)", 1)>]
    [<DataRow("ModusTollens_03a", "inf ModusTollens{dec p,q:pred; pre:not q,impl(p,q) con:not (p)} thm T {true} proof T$1 {1: not (and(is(A,N), false)) 2: impl(iif(true,false), and(is(A,N), false)) 3. 1, 2, byinf ModusTollens |- true }", "¬(true ⇔ false)", 1)>]

    // HypotheticalSyllogism: pre: impl(p,q), impl(q,s)
    [<DataRow("HypotheticalSyllogism_01", "inf HypotheticalSyllogism{dec p,q,s:pred; pre:impl(p,q),impl(q,s) con:impl(p,s)} thm T {true} proof T$1 {1: impl(all x:obj { is(x,N) }, ex y:obj { is(y,M) }) 2: impl(ex z:obj { is(z,M) }, xor(true,false)) 3. 1, 2, byinf HypotheticalSyllogism |- true }", "∀ x:obj {x is N} ⇒ (true ⩡ false)", 1)>]
    [<DataRow("HypotheticalSyllogism_02a", "inf HypotheticalSyllogism{dec p,q,s:pred; pre:impl(p,q),impl(q,s) con:impl(p,s)} thm T {true} proof T$1 {1: impl(iif(true,false), iif(iif(true,false), true)) 2: impl(iif(iif(true,false), true), all u:obj { is(u,K) }) 3. 1, 2, byinf HypotheticalSyllogism |- true }", "(true ⇔ false) ⇒ ∀ u:obj {u is K}", 1)>]
    [<DataRow("HypotheticalSyllogism_03a", "inf HypotheticalSyllogism{dec p,q,s:pred; pre:impl(p,q),impl(q,s) con:impl(p,s)} thm T {true} proof T$1 {1: impl(iif(true,false), all u:obj { is(u,K) }) 2: impl(all u:obj { is(u,K) }, iif(true,false)) 3. 1, 2, byinf HypotheticalSyllogism |- true }", "(true ⇔ false) ⇒ (true ⇔ false)", 1)>]

    // DisjunctiveSyllogism: pre: not p, or(p,q)
    [<DataRow("DisjunctiveSyllogism_01a", "inf DisjunctiveSyllogism{dec p,q:pred; pre:not p,or(p,q) con:q} thm T {true} proof T$1 {1: not (iif(true,false)) 2: or(iif(true,false), ex y:obj { is(y,M) }) 3. 1, 2, byinf DisjunctiveSyllogism |- true }", "∃ y:obj {y is M}", 1)>]
    [<DataRow("DisjunctiveSyllogism_02", "inf DisjunctiveSyllogism{dec p,q:pred; pre:not p,or(p,q) con:q} thm T {true} proof T$1 {1: not iif(true,false) 2: or(iif(true,false), xor(true,false)) 3. 1, 2, byinf DisjunctiveSyllogism |- true }", "true ⩡ false", 1)>]
    [<DataRow("DisjunctiveSyllogism_03a", "inf DisjunctiveSyllogism{dec p,q:pred; pre:not p,or(p,q) con:q} thm T {true} proof T$1 {1: not (and(is(A,N), false)) 2: or(and(is(A,N), false), impl(true,false)) 3. 1, 2, byinf DisjunctiveSyllogism |- true }", "true ⇒ false", 1)>]

    // ExistsByExample: pre: p(c)
    [<DataRow("ExistsByExample_01", "inf ExistsByExample{dec p:pred(x:obj); pre:p(x) con:ex x:tpl{p(x)}} thm T {dec a:obj; true} proof T$1 {1: iif(is(a,N), true) 2. 1, byinf ExistsByExample |- true}", "∃ a:obj {(a is N) ⇔ true}", 1)>]
    [<DataRow("ExistsByExample_02", "inf ExistsByExample{dec p:pred(); pre:p con:ex x:tpl{p(x)}} thm T {true} proof T$1 {1: and(ex x:obj {is(x,M)}, iif(true,false)) 2. 1, byinf ExistsByExample |- true}", "∃ x:tpl {∃ x:obj {x is M} ∧ (true ⇔ false)}", 1)>]
    [<DataRow("ExistsByExample_02a", "inf ExistsByExample{dec p:pred(c:tpl); pre:p con:ex x:tpl{p(x)}} thm T {true} proof T$1 {dec a:tpl; 1: and(is(a,M) , (a = $1)) 2. 1, byinf ExistsByExample |- true}", "∃ x:tpl {(a is M) ∧ a = $1}", 1)>]
    [<DataRow("ExistsByExample_03", "inf ExistsByExample{dec p:pred(); pre:p con:ex x:tpl{p(x)}} thm T {true} proof T$1 {1: xor(all z:obj {is(z,K)}, not (xor(true,false))) 2. 1, byinf ExistsByExample |- true}", "∃ x:tpl {∀ z:obj {z is K} ⩡ ¬(true ⩡ false)}", 1)>]

    // Contraposition: pre: impl(not p, not q)
    [<DataRow("Contraposition_01", "inf Contraposition{dec p,q:pred; pre:impl(not p,not q) con:impl(q,p)} thm T {true} proof T$1 {1: impl(not all x:obj {is(x,N)}, not (ex y:obj {is(y,M)})) 2. 1, byinf Contraposition |- true}", "∃ y:obj {y is M} ⇒ ∀ x:obj {x is N}", 1)>]
    [<DataRow("Contraposition_02", "inf Contraposition{dec p,q:pred; pre:impl(not p,not q) con:impl(q,p)} thm T {true} proof T$1 {1: impl(not (iif(true,false)), not (xor(true,false))) 2. 1, byinf Contraposition |- true}", "(true ⩡ false) ⇒ (true ⇔ false)", 1)>]
    [<DataRow("Contraposition_03", "inf Contraposition{dec p,q:pred; pre:impl(not p,not q) con:impl(q,p)} thm T {true} proof T$1 {1: impl(not (and(is(A,N), true)), not (iif(true,false))) 2. 1, byinf Contraposition |- true}", "(true ⇔ false) ⇒ ((A is N) ∧ true)", 1)>]

    // OrPack2Xor: pre: or(and(not p, q), and(p, not q))
    [<DataRow("OrPack2Xor_02", "inf OrPack2Xor{dec p,q:pred; pre:or(and(not p,q),and(p,not q)) con:xor(p,q)} thm T {true} proof T$1 {1: or(and(not iif(true, false), xor(true, false)), and(iif(true, false), not xor(true, false))) 2. 1, byinf OrPack2Xor |- true}", "(true ⇔ false) ⩡ (true ⩡ false)", 1)>]
    [<DataRow("OrPack2Xor_03", "inf OrPack2Xor{dec p,q:pred; pre:or(and(not p,q),and(p,not q)) con:xor(p,q)} thm T {true} proof T$1 {1: or(and(not (impl(true, false)), and(true, false)), and(impl(true, false), not and(true, false))) 2. 1, byinf OrPack2Xor |- true}", "(true ⇒ false) ⩡ (true ∧ false)", 1)>]

    // OrUnpack2Impl: pre: or(p,q)
    [<DataRow("OrUnpack2Impl_01", "inf OrUnpack2Impl{dec p,q:pred; pre:or(p,q) con:impl(not p,q)} thm T {true} proof T$1 {1: or(iif(is(A,N), true), ex x:obj {is(x,M)}) 2. 1, byinf OrUnpack2Impl |- true}", "¬((A is N) ⇔ true) ⇒ ∃ x:obj {x is M}", 1)>]
    [<DataRow("OrUnpack2Impl_02", "inf OrUnpack2Impl{dec p,q:pred; pre:or(p,q) con:impl(not p,q)} thm T {true} proof T$1 {1: or(all x:obj {is(x,N)}, and(true, not false)) 2. 1, byinf OrUnpack2Impl |- true}", "¬∀ x:obj {x is N} ⇒ (true ∧ ¬false)", 1)>]
    [<DataRow("OrUnpack2Impl_03", "inf OrUnpack2Impl{dec p,q:pred; pre:or(p,q) con:impl(not p,q)} thm T {true} proof T$1 {1: or(xor(true,false), impl(all z:obj {is(z,N)}, false)) 2. 1, byinf OrUnpack2Impl |- true}", "¬(true ⩡ false) ⇒ (∀ z:obj {z is N} ⇒ false)", 1)>]

    // PrenexPackAllAnd: pre: all x:tpl{and(p, q(x))}
    [<DataRow("PrenexPackAllAnd_01", "inf PrenexPackAllAnd{dec p:pred q:pred(z:tpl); pre:all x:tpl{and(p,q(x))} con:and(p,all x:tpl{q(x)})} thm T {true} proof T$1 {1: all x:obj {and(iif(true,false), iif(is(x,N), true))} 2. 1, byinf PrenexPackAllAnd |- true}", "(true ⇔ false) ∧ ∀ x:obj {(x is N) ⇔ true}", 1)>]
    [<DataRow("PrenexPackAllAnd_02", "inf PrenexPackAllAnd{dec p:pred q:pred(z:tpl); pre:all x:tpl{and(p,q(x))} con:and(p,all x:tpl{q(x)})} thm T {true} proof T$1 {1: all n:Nat {and(ex y:obj {is(y,K)}, xor(is(n,K), true))} 2. 1, byinf PrenexPackAllAnd |- true}", "∃ y:obj {y is K} ∧ ∀ n:Nat {(n is K) ⩡ true}", 1)>]
    [<DataRow("PrenexPackAllAnd_03", "inf PrenexPackAllAnd{dec p:pred q:pred(z:tpl); pre:all x:tpl{and(p,q(x))} con:and(p,all x:tpl{q(x)})} thm T {true} proof T$1 {1: all z:obj {and(not (xor(true,false)), impl(is(z,N), false))} 2. 1, byinf PrenexPackAllAnd |- true}", "¬(true ⩡ false) ∧ ∀ z:obj {(z is N) ⇒ false}", 1)>]

    // PrenexPackAllIif: pre: all x:tpl{iif(p, q(x))}
    [<DataRow("PrenexPackAllIif_01", "inf PrenexPackAllIif{dec p:pred q:pred(z:tpl); pre:all x:tpl{iif(p,q(x))} con:iif(p,all x:tpl{q(x)})} thm T {true} proof T$1 {1: all x:obj {iif(iif(true,false), iif(is(x,N), true))} 2. 1, byinf PrenexPackAllIif |- true}", "(true ⇔ false) ⇔ ∀ x:obj {(x is N) ⇔ true}", 1)>]
    [<DataRow("PrenexPackAllIif_02", "inf PrenexPackAllIif{dec p:pred q:pred(z:tpl); pre:all x:tpl{iif(p,q(x))} con:iif(p,all x:tpl{q(x)})} thm T {true} proof T$1 {1: all n:Nat {iif(all y:obj {is(y,M)}, xor(is(n,K), true))} 2. 1, byinf PrenexPackAllIif |- true}", "∀ y:obj {y is M} ⇔ ∀ n:Nat {(n is K) ⩡ true}", 1)>]
    [<DataRow("PrenexPackAllIif_03", "inf PrenexPackAllIif{dec p:pred q:pred(z:tpl); pre:all x:tpl{iif(p,q(x))} con:iif(p,all x:tpl{q(x)})} thm T {true} proof T$1 {1: all z:obj {iif(not (xor(true,false)), impl(is(z,N), false))} 2. 1, byinf PrenexPackAllIif |- true}", "¬(true ⩡ false) ⇔ ∀ z:obj {(z is N) ⇒ false}", 1)>]

    // PrenexPackAllImpl: pre: all x:tpl{impl(p, q(x))}
    [<DataRow("PrenexPackAllImpl_01", "inf PrenexPackAllImpl{dec p:pred q:pred(z:tpl); pre:all x:tpl{impl(p,q(x))} con:impl(p,all x:tpl{q(x)})} thm T {true} proof T$1 {1: all x:obj {impl(iif(true,false), iif(is(x,N), true))} 2. 1, byinf PrenexPackAllImpl |- true}", "(true ⇔ false) ⇒ ∀ x:obj {(x is N) ⇔ true}", 1)>]
    [<DataRow("PrenexPackAllImpl_02", "inf PrenexPackAllImpl{dec p:pred q:pred(z:tpl); pre:all x:tpl{impl(p,q(x))} con:impl(p,all x:tpl{q(x)})} thm T {true} proof T$1 {1: all n:Nat {impl(all y:obj {is(y,M)}, xor(is(n,K), true))} 2. 1, byinf PrenexPackAllImpl |- true}", "∀ y:obj {y is M} ⇒ ∀ n:Nat {(n is K) ⩡ true}", 1)>]
    [<DataRow("PrenexPackAllImpl_03", "inf PrenexPackAllImpl{dec p:pred q:pred(z:tpl); pre:all x:tpl{impl(p,q(x))} con:impl(p,all x:tpl{q(x)})} thm T {true} proof T$1 {1: all z:obj {impl(not (xor(true,false)), impl(is(z,N), false))} 2. 1, byinf PrenexPackAllImpl |- true}", "¬(true ⩡ false) ⇒ ∀ z:obj {(z is N) ⇒ false}", 1)>]

    // PrenexPackAllOr: pre: all x:tpl{or(p, q(x))}
    [<DataRow("PrenexPackAllOr_01", "inf PrenexPackAllOr{dec p:pred q:pred(z:tpl); pre:all x:tpl{or(p,q(x))} con:or(p,all x:tpl{q(x)})} thm T {true} proof T$1 {1: all x:obj {or(iif(true,false), iif(is(x,N), true))} 2. 1, byinf PrenexPackAllOr |- true}", "(true ⇔ false) ∨ ∀ x:obj {(x is N) ⇔ true}", 1)>]
    [<DataRow("PrenexPackAllOr_02", "inf PrenexPackAllOr{dec p:pred q:pred(z:tpl); pre:all x:tpl{or(p,q(x))} con:or(p,all x:tpl{q(x)})} thm T {true} proof T$1 {1: all n:Nat {or(ex y:obj {is(y,M)}, xor(is(n,K), true))} 2. 1, byinf PrenexPackAllOr |- true}", "∃ y:obj {y is M} ∨ ∀ n:Nat {(n is K) ⩡ true}", 1)>]
    [<DataRow("PrenexPackAllOr_03", "inf PrenexPackAllOr{dec p:pred q:pred(z:tpl); pre:all x:tpl{or(p,q(x))} con:or(p,all x:tpl{q(x)})} thm T {true} proof T$1 {1: all z:obj {or(not (xor(true,false)), impl(is(z,N), false))} 2. 1, byinf PrenexPackAllOr |- true}", "¬(true ⩡ false) ∨ ∀ z:obj {(z is N) ⇒ false}", 1)>]

    // PrenexPackExAnd: pre: ex x:tpl{and(p, q(x))}
    [<DataRow("PrenexPackExAnd_01", "inf PrenexPackExAnd{dec p:pred q:pred(z:tpl); pre:ex x:tpl{and(p,q(x))} con:and(p,ex x:tpl{q(x)})} thm T {true} proof T$1 {1: ex x:obj {and(iif(true,false), iif(is(x,N), true))} 2. 1, byinf PrenexPackExAnd |- true}", "(true ⇔ false) ∧ ∃ x:obj {(x is N) ⇔ true}", 1)>]
    [<DataRow("PrenexPackExAnd_02", "inf PrenexPackExAnd{dec p:pred q:pred(z:tpl); pre:ex x:tpl{and(p,q(x))} con:and(p,ex x:tpl{q(x)})} thm T {true} proof T$1 {1: ex n:Nat {and(all y:obj {is(y,K)}, xor(is(n,M), true))} 2. 1, byinf PrenexPackExAnd |- true}", "∀ y:obj {y is K} ∧ ∃ n:Nat {(n is M) ⩡ true}", 1)>]
    [<DataRow("PrenexPackExAnd_03", "inf PrenexPackExAnd{dec p:pred q:pred(z:tpl); pre:ex x:tpl{and(p,q(x))} con:and(p,ex x:tpl{q(x)})} thm T {true} proof T$1 {1: ex z:obj {and(not (xor(true,false)), impl(is(z,N), false))} 2. 1, byinf PrenexPackExAnd |- true}", "¬(true ⩡ false) ∧ ∃ z:obj {(z is N) ⇒ false}", 1)>]

    // PrenexPackExIif: pre: ex x:tpl{iif(p, q(x))}
    [<DataRow("PrenexPackExIif_01", "inf PrenexPackExIif{dec p:pred q:pred(z:tpl); pre:ex x:tpl{iif(p,q(x))} con:iif(p,ex x:tpl{q(x)})} thm T {true} proof T$1 {1: ex x:obj {iif(iif(true,false), iif(is(x,N), true))} 2. 1, byinf PrenexPackExIif |- true}", "(true ⇔ false) ⇔ ∃ x:obj {(x is N) ⇔ true}", 1)>]
    [<DataRow("PrenexPackExIif_02", "inf PrenexPackExIif{dec p:pred q:pred(z:tpl); pre:ex x:tpl{iif(p,q(x))} con:iif(p,ex x:tpl{q(x)})} thm T {true} proof T$1 {1: ex n:Nat {iif(all y:obj {is(y,M)}, xor(is(n,K), true))} 2. 1, byinf PrenexPackExIif |- true}", "∀ y:obj {y is M} ⇔ ∃ n:Nat {(n is K) ⩡ true}", 1)>]
    [<DataRow("PrenexPackExIif_03", "inf PrenexPackExIif{dec p:pred q:pred(z:tpl); pre:ex x:tpl{iif(p,q(x))} con:iif(p,ex x:tpl{q(x)})} thm T {true} proof T$1 {1: ex z:obj {iif(not (xor(true,false)), impl(is(z,N), false))} 2. 1, byinf PrenexPackExIif |- true}", "¬(true ⩡ false) ⇔ ∃ z:obj {(z is N) ⇒ false}", 1)>]

    // PrenexPackExImpl: pre: ex x:tpl{impl(p, q(x))}
    [<DataRow("PrenexPackExImpl_01", "inf PrenexPackExImpl{dec p:pred q:pred(z:tpl); pre:ex x:tpl{impl(p,q(x))} con:impl(p,ex x:tpl{q(x)})} thm T {true} proof T$1 {1: ex x:obj {impl(iif(true,false), iif(is(x,N), true))} 2. 1, byinf PrenexPackExImpl |- true}", "(true ⇔ false) ⇒ ∃ x:obj {(x is N) ⇔ true}", 1)>]
    [<DataRow("PrenexPackExImpl_02", "inf PrenexPackExImpl{dec p:pred q:pred(z:tpl); pre:ex x:tpl{impl(p,q(x))} con:impl(p,ex x:tpl{q(x)})} thm T {true} proof T$1 {1: ex n:Nat {impl(all y:obj {is(y,M)}, xor(is(n,K), true))} 2. 1, byinf PrenexPackExImpl |- true}", "∀ y:obj {y is M} ⇒ ∃ n:Nat {(n is K) ⩡ true}", 1)>]
    [<DataRow("PrenexPackExImpl_03", "inf PrenexPackExImpl{dec p:pred q:pred(z:tpl); pre:ex x:tpl{impl(p,q(x))} con:impl(p,ex x:tpl{q(x)})} thm T {true} proof T$1 {1: ex z:obj {impl(not (xor(true,false)), impl(is(z,N), false))} 2. 1, byinf PrenexPackExImpl |- true}", "¬(true ⩡ false) ⇒ ∃ z:obj {(z is N) ⇒ false}", 1)>]

    // PrenexPackExOr: pre: ex x:tpl{or(p, q(x))}
    [<DataRow("PrenexPackExOr_01", "inf PrenexPackExOr{dec p:pred q:pred(z:tpl); pre:ex x:tpl{or(p,q(x))} con:or(p,ex x:tpl{q(x)})} thm T {true} proof T$1 {1: ex x:obj {or(iif(true,false), iif(is(x,N), true))} 2. 1, byinf PrenexPackExOr |- true}", "(true ⇔ false) ∨ ∃ x:obj {(x is N) ⇔ true}", 1)>]
    [<DataRow("PrenexPackExOr_02", "inf PrenexPackExOr{dec p:pred q:pred(z:tpl); pre:ex x:tpl{or(p,q(x))} con:or(p,ex x:tpl{q(x)})} thm T {true} proof T$1 {1: ex n:Nat {or(all y:obj {is(y,M)}, xor(is(n,K), true))} 2. 1, byinf PrenexPackExOr |- true}", "∀ y:obj {y is M} ∨ ∃ n:Nat {(n is K) ⩡ true}", 1)>]
    [<DataRow("PrenexPackExOr_03", "inf PrenexPackExOr{dec p:pred q:pred(z:tpl); pre:ex x:tpl{or(p,q(x))} con:or(p,ex x:tpl{q(x)})} thm T {true} proof T$1 {1: ex z:obj {or(not (xor(true,false)), impl(is(z,N), false))} 2. 1, byinf PrenexPackExOr |- true}", "¬(true ⩡ false) ∨ ∃ z:obj {(z is N) ⇒ false}", 1)>]

    // PrenexPackExXor: pre: ex x:tpl{xor(p, q(x))}
    [<DataRow("PrenexPackExXor_01", "inf PrenexPackExXor{dec p:pred q:pred(z:tpl); pre:ex x:tpl{xor(p,q(x))} con:xor(p,ex x:tpl{q(x)})} thm T {true} proof T$1 {1: ex x:obj {xor(iif(true,false), iif(is(x,N), true))} 2. 1, byinf PrenexPackExXor |- true}", "(true ⇔ false) ⩡ ∃ x:obj {(x is N) ⇔ true}", 1)>]
    [<DataRow("PrenexPackExXor_02", "inf PrenexPackExXor{dec p:pred q:pred(z:tpl); pre:ex x:tpl{xor(p,q(x))} con:xor(p,ex x:tpl{q(x)})} thm T {true} proof T$1 {1: ex n:Nat {xor(all y:obj {is(y,M)}, xor(is(n,K), true))} 2. 1, byinf PrenexPackExXor |- true}", "∀ y:obj {y is M} ⩡ ∃ n:Nat {(n is K) ⩡ true}", 1)>]
    [<DataRow("PrenexPackExXor_03", "inf PrenexPackExXor{dec p:pred q:pred(z:tpl); pre:ex x:tpl{xor(p,q(x))} con:xor(p,ex x:tpl{q(x)})} thm T {true} proof T$1 {1: ex z:obj {xor(not (xor(true,false)), impl(is(z,N), false))} 2. 1, byinf PrenexPackExXor |- true}", "¬(true ⩡ false) ⩡ ∃ z:obj {(z is N) ⇒ false}", 1)>]

    // PrenexUnpackAndAll: pre: and(p, all x:tpl{q(x)})
    [<DataRow("PrenexUnpackAndAll_01", "inf PrenexUnpackAndAll{dec p:pred q:pred(z:tpl); pre:and(p,all x:tpl{q(x)}) con:all x:tpl{and(p,q(x))}} thm T {true} proof T$1 {1: and(iif(true,false), all n:Nat {iif(is(n,N), true)}) 2. 1, byinf PrenexUnpackAndAll |- true}", "∀ n:Nat {(true ⇔ false) ∧ ((n is N) ⇔ true)}", 1)>]
    [<DataRow("PrenexUnpackAndAll_02", "inf PrenexUnpackAndAll{dec p:pred q:pred(z:tpl); pre:and(p,all x:tpl{q(x)}) con:all x:tpl{and(p,q(x))}} thm T {true} proof T$1 {1: and(ex y:obj {is(y,M)}, all z:obj {xor(is(z,K), true)}) 2. 1, byinf PrenexUnpackAndAll |- true}", "∀ z:obj {∃ y:obj {y is M} ∧ ((z is K) ⩡ true)}", 1)>]
    [<DataRow("PrenexUnpackAndAll_03", "inf PrenexUnpackAndAll{dec p:pred q:pred(z:tpl); pre:and(p,all x:tpl{q(x)}) con:all x:tpl{and(p,q(x))}} thm T {true} proof T$1 {1: and(not (xor(true,false)), all x:obj {impl(is(x,N), false)}) 2. 1, byinf PrenexUnpackAndAll |- true}", "∀ x:obj {¬(true ⩡ false) ∧ ((x is N) ⇒ false)}", 1)>]

    // PrenexUnpackAndEx: pre: and(p, ex x:tpl{q(x)})
    [<DataRow("PrenexUnpackAndEx_01", "inf PrenexUnpackAndEx{dec p:pred q:pred(z:tpl); pre:and(p,ex x:tpl{q(x)}) con:ex x:tpl{and(p,q(x))}} thm T {true} proof T$1 {1: and(iif(true,false), ex n:Nat {iif(is(n,N), true)}) 2. 1, byinf PrenexUnpackAndEx |- true}", "∃ n:Nat {(true ⇔ false) ∧ ((n is N) ⇔ true)}", 1)>]
    [<DataRow("PrenexUnpackAndEx_02", "inf PrenexUnpackAndEx{dec p:pred q:pred(z:tpl); pre:and(p,ex x:tpl{q(x)}) con:ex x:tpl{and(p,q(x))}} thm T {true} proof T$1 {1: and(all z:obj {is(z,K)}, ex y:obj {xor(is(y,M), true)}) 2. 1, byinf PrenexUnpackAndEx |- true}", "∃ y:obj {∀ z:obj {z is K} ∧ ((y is M) ⩡ true)}", 1)>]
    [<DataRow("PrenexUnpackAndEx_03", "inf PrenexUnpackAndEx{dec p:pred q:pred(z:tpl); pre:and(p,ex x:tpl{q(x)}) con:ex x:tpl{and(p,q(x))}} thm T {true} proof T$1 {1: and(not (xor(true,false)), ex x:obj {impl(is(x,N), false)}) 2. 1, byinf PrenexUnpackAndEx |- true}", "∃ x:obj {¬(true ⩡ false) ∧ ((x is N) ⇒ false)}", 1)>]

    // PrenexUnpackIifAll: pre: iif(p, all x:tpl{q(x)})
    [<DataRow("PrenexUnpackIifAll_01", "inf PrenexUnpackIifAll{dec p:pred q:pred(z:tpl); pre:iif(p,all x:tpl{q(x)}) con:all x:tpl{iif(p,q(x))}} thm T {true} proof T$1 {1: iif(iif(true,false), all n:Nat {iif(is(n,N), true)}) 2. 1, byinf PrenexUnpackIifAll |- true}", "∀ n:Nat {(true ⇔ false) ⇔ ((n is N) ⇔ true)}", 1)>]
    [<DataRow("PrenexUnpackIifAll_02", "inf PrenexUnpackIifAll{dec p:pred q:pred(z:tpl); pre:iif(p,all x:tpl{q(x)}) con:all x:tpl{iif(p,q(x))}} thm T {true} proof T$1 {1: iif(all y:obj {is(y,M)}, all z:obj {xor(is(z,K), true)}) 2. 1, byinf PrenexUnpackIifAll |- true}", "∀ z:obj {∀ y:obj {y is M} ⇔ ((z is K) ⩡ true)}", 1)>]
    [<DataRow("PrenexUnpackIifAll_03", "inf PrenexUnpackIifAll{dec p:pred q:pred(z:tpl); pre:iif(p,all x:tpl{q(x)}) con:all x:tpl{iif(p,q(x))}} thm T {true} proof T$1 {1: iif(not (xor(true,false)), all x:obj {impl(is(x,N), false)}) 2. 1, byinf PrenexUnpackIifAll |- true}", "∀ x:obj {¬(true ⩡ false) ⇔ ((x is N) ⇒ false)}", 1)>]

    // PrenexUnpackIifEx: pre: iif(p, ex x:tpl{q(x)})
    [<DataRow("PrenexUnpackIifEx_01", "inf PrenexUnpackIifEx{dec p:pred q:pred(z:tpl); pre:iif(p,ex x:tpl{q(x)}) con:ex x:tpl{iif(p,q(x))}} thm T {true} proof T$1 {1: iif(iif(true,false), ex n:Nat {iif(is(n,N), true)}) 2. 1, byinf PrenexUnpackIifEx |- true}", "∃ n:Nat {(true ⇔ false) ⇔ ((n is N) ⇔ true)}", 1)>]
    [<DataRow("PrenexUnpackIifEx_02", "inf PrenexUnpackIifEx{dec p:pred q:pred(z:tpl); pre:iif(p,ex x:tpl{q(x)}) con:ex x:tpl{iif(p,q(x))}} thm T {true} proof T$1 {1: iif(all y:obj {is(y,M)}, ex z:obj {xor(is(z,K), true)}) 2. 1, byinf PrenexUnpackIifEx |- true}", "∃ z:obj {∀ y:obj {y is M} ⇔ ((z is K) ⩡ true)}", 1)>]
    [<DataRow("PrenexUnpackIifEx_03", "inf PrenexUnpackIifEx{dec p:pred q:pred(z:tpl); pre:iif(p,ex x:tpl{q(x)}) con:ex x:tpl{iif(p,q(x))}} thm T {true} proof T$1 {1: iif(not (xor(true,false)), ex x:obj {impl(is(x,N), false)}) 2. 1, byinf PrenexUnpackIifEx |- true}", "∃ x:obj {¬(true ⩡ false) ⇔ ((x is N) ⇒ false)}", 1)>]

    // PrenexUnpackImplAll: pre: impl(p, all x:tpl{q(x)})
    [<DataRow("PrenexUnpackImplAll_01", "inf PrenexUnpackImplAll{dec p:pred q:pred(z:tpl); pre:impl(p,all x:tpl{q(x)}) con:all x:tpl{impl(p,q(x))}} thm T {true} proof T$1 {1: impl(iif(true,false), all n:Nat {iif(is(n,N), true)}) 2. 1, byinf PrenexUnpackImplAll |- true}", "∀ n:Nat {(true ⇔ false) ⇒ ((n is N) ⇔ true)}", 1)>]
    [<DataRow("PrenexUnpackImplAll_02", "inf PrenexUnpackImplAll{dec p:pred q:pred(z:tpl); pre:impl(p,all x:tpl{q(x)}) con:all x:tpl{impl(p,q(x))}} thm T {true} proof T$1 {1: impl(all y:obj {is(y,M)}, all z:obj {xor(is(z,K), true)}) 2. 1, byinf PrenexUnpackImplAll |- true}", "∀ z:obj {∀ y:obj {y is M} ⇒ ((z is K) ⩡ true)}", 1)>]
    [<DataRow("PrenexUnpackImplAll_03", "inf PrenexUnpackImplAll{dec p:pred q:pred(z:tpl); pre:impl(p,all x:tpl{q(x)}) con:all x:tpl{impl(p,q(x))}} thm T {true} proof T$1 {1: impl(not (xor(true,false)), all x:obj {impl(is(x,N), false)}) 2. 1, byinf PrenexUnpackImplAll |- true}", "∀ x:obj {¬(true ⩡ false) ⇒ ((x is N) ⇒ false)}", 1)>]

    // PrenexUnpackImplEx: pre: impl(p, ex x:tpl{q(x)})
    [<DataRow("PrenexUnpackImplEx_01", "inf PrenexUnpackImplEx{dec p:pred q:pred(z:tpl); pre:impl(p,ex x:tpl{q(x)}) con:ex x:tpl{impl(p,q(x))}} thm T {true} proof T$1 {1: impl(iif(true,false), ex n:Nat {iif(is(n,N), true)}) 2. 1, byinf PrenexUnpackImplEx |- true}", "∃ n:Nat {(true ⇔ false) ⇒ ((n is N) ⇔ true)}", 1)>]
    [<DataRow("PrenexUnpackImplEx_02", "inf PrenexUnpackImplEx{dec p:pred q:pred(z:tpl); pre:impl(p,ex x:tpl{q(x)}) con:ex x:tpl{impl(p,q(x))}} thm T {true} proof T$1 {1: impl(all y:obj {is(y,M)}, ex z:obj {xor(is(z,K), true)}) 2. 1, byinf PrenexUnpackImplEx |- true}", "∃ z:obj {∀ y:obj {y is M} ⇒ ((z is K) ⩡ true)}", 1)>]
    [<DataRow("PrenexUnpackImplEx_03", "inf PrenexUnpackImplEx{dec p:pred q:pred(z:tpl); pre:impl(p,ex x:tpl{q(x)}) con:ex x:tpl{impl(p,q(x))}} thm T {true} proof T$1 {1: impl(not (xor(true,false)), ex x:obj {impl(is(x,N), false)}) 2. 1, byinf PrenexUnpackImplEx |- true}", "∃ x:obj {¬(true ⩡ false) ⇒ ((x is N) ⇒ false)}", 1)>]

    // PrenexUnpackOrAll: pre: or(p, all x:tpl{q(x)})
    [<DataRow("PrenexUnpackOrAll_01", "inf PrenexUnpackOrAll{dec p:pred q:pred(z:tpl); pre:or(p,all x:tpl{q(x)}) con:all x:tpl{or(p,q(x))}} thm T {true} proof T$1 {1: or(iif(true,false), all n:Nat {iif(is(n,N), true)}) 2. 1, byinf PrenexUnpackOrAll |- true}", "∀ n:Nat {(true ⇔ false) ∨ ((n is N) ⇔ true)}", 1)>]
    [<DataRow("PrenexUnpackOrAll_02", "inf PrenexUnpackOrAll{dec p:pred q:pred(z:tpl); pre:or(p,all x:tpl{q(x)}) con:all x:tpl{or(p,q(x))}} thm T {true} proof T$1 {1: or(ex y:obj {is(y,M)}, all z:obj {xor(is(z,K), true)}) 2. 1, byinf PrenexUnpackOrAll |- true}", "∀ z:obj {∃ y:obj {y is M} ∨ ((z is K) ⩡ true)}", 1)>]
    [<DataRow("PrenexUnpackOrAll_03", "inf PrenexUnpackOrAll{dec p:pred q:pred(z:tpl); pre:or(p,all x:tpl{q(x)}) con:all x:tpl{or(p,q(x))}} thm T {true} proof T$1 {1: or(not (xor(true,false)), all x:obj {impl(is(x,N), false)}) 2. 1, byinf PrenexUnpackOrAll |- true}", "∀ x:obj {¬(true ⩡ false) ∨ ((x is N) ⇒ false)}", 1)>]

    // PrenexUnpackOrEx: pre: or(p, ex x:tpl{q(x)})
    [<DataRow("PrenexUnpackOrEx_01", "inf PrenexUnpackOrEx{dec p:pred q:pred(z:tpl); pre:or(p,ex x:tpl{q(x)}) con:ex x:tpl{or(p,q(x))}} thm T {true} proof T$1 {1: or(iif(true,false), ex n:Nat {iif(is(n,N), true)}) 2. 1, byinf PrenexUnpackOrEx |- true}", "∃ n:Nat {(true ⇔ false) ∨ ((n is N) ⇔ true)}", 1)>]
    [<DataRow("PrenexUnpackOrEx_02", "inf PrenexUnpackOrEx{dec p:pred q:pred(z:tpl); pre:or(p,ex x:tpl{q(x)}) con:ex x:tpl{or(p,q(x))}} thm T {true} proof T$1 {1: or(all y:obj {is(y,M)}, ex z:obj {xor(is(z,K), true)}) 2. 1, byinf PrenexUnpackOrEx |- true}", "∃ z:obj {∀ y:obj {y is M} ∨ ((z is K) ⩡ true)}", 1)>]
    [<DataRow("PrenexUnpackOrEx_03", "inf PrenexUnpackOrEx{dec p:pred q:pred(z:tpl); pre:or(p,ex x:tpl{q(x)}) con:ex x:tpl{or(p,q(x))}} thm T {true} proof T$1 {1: or(not (xor(true,false)), ex x:obj {impl(is(x,N), false)}) 2. 1, byinf PrenexUnpackOrEx |- true}", "∃ x:obj {¬(true ⩡ false) ∨ ((x is N) ⇒ false)}", 1)>]

    // PrenexUnpackXorAll: pre: xor(p, all x:tpl{q(x)})
    [<DataRow("PrenexUnpackXorAll_01", "inf PrenexUnpackXorAll{dec p:pred q:pred(z:tpl); pre:xor(p,all x:tpl{q(x)}) con:all x:tpl{xor(p,q(x))}} thm T {true} proof T$1 {1: xor(iif(true,false), all n:Nat {iif(is(n,N), true)}) 2. 1, byinf PrenexUnpackXorAll |- true}", "∀ n:Nat {(true ⇔ false) ⩡ ((n is N) ⇔ true)}", 1)>]
    [<DataRow("PrenexUnpackXorAll_02", "inf PrenexUnpackXorAll{dec p:pred q:pred(z:tpl); pre:xor(p,all x:tpl{q(x)}) con:all x:tpl{xor(p,q(x))}} thm T {true} proof T$1 {1: xor(all y:obj {is(y,M)}, all z:obj {xor(is(z,K), true)}) 2. 1, byinf PrenexUnpackXorAll |- true}", "∀ z:obj {∀ y:obj {y is M} ⩡ ((z is K) ⩡ true)}", 1)>]
    [<DataRow("PrenexUnpackXorAll_03", "inf PrenexUnpackXorAll{dec p:pred q:pred(z:tpl); pre:xor(p,all x:tpl{q(x)}) con:all x:tpl{xor(p,q(x))}} thm T {true} proof T$1 {1: xor(not (xor(true,false)), all x:obj {impl(is(x,N), false)}) 2. 1, byinf PrenexUnpackXorAll |- true}", "∀ x:obj {¬(true ⩡ false) ⩡ ((x is N) ⇒ false)}", 1)>]

    // PrenexUnpackXorEx: pre: xor(p, ex x:tpl{q(x)})
    [<DataRow("PrenexUnpackXorEx_01", "inf PrenexUnpackXorEx{dec p:pred q:pred(z:tpl); pre:xor(p,ex x:tpl{q(x)}) con:ex x:tpl{xor(p,q(x))}} thm T {true} proof T$1 {1: xor(iif(true,false), ex n:Nat {iif(is(n,N), true)}) 2. 1, byinf PrenexUnpackXorEx |- true}", "∃ n:Nat {(true ⇔ false) ⩡ ((n is N) ⇔ true)}", 1)>]
    [<DataRow("PrenexUnpackXorEx_02", "inf PrenexUnpackXorEx{dec p:pred q:pred(z:tpl); pre:xor(p,ex x:tpl{q(x)}) con:ex x:tpl{xor(p,q(x))}} thm T {true} proof T$1 {1: xor(all y:obj {is(y,M)}, ex z:obj {xor(is(z,K), true)}) 2. 1, byinf PrenexUnpackXorEx |- true}", "∃ z:obj {∀ y:obj {y is M} ⩡ ((z is K) ⩡ true)}", 1)>]
    [<DataRow("PrenexUnpackXorEx_03", "inf PrenexUnpackXorEx{dec p:pred q:pred(z:tpl); pre:xor(p,ex x:tpl{q(x)}) con:ex x:tpl{xor(p,q(x))}} thm T {true} proof T$1 {1: xor(not (xor(true,false)), ex x:obj {impl(is(x,N), false)}) 2. 1, byinf PrenexUnpackXorEx |- true}", "∃ x:obj {¬(true ⩡ false) ⩡ ((x is N) ⇒ false)}", 1)>]

    // Proceeding2Results: pre: p, q
    [<DataRow("Proceeding2Results_01", "inf Proceeding2Results{dec p,q:pred; pre:p,q con:and(p,q)} thm T {true} proof T$1 {1: all x:obj {is(x, N)} 2: ex y:obj {is(y, M)} 3. 1, 2, byinf Proceeding2Results |- true}", "∀ x:obj {x is N} ∧ ∃ y:obj {y is M}", 1)>]
    [<DataRow("Proceeding2Results_02", "inf Proceeding2Results{dec p,q:pred; pre:p,q con:and(p,q)} thm T {true} proof T$1 {1: iif(true, false) 2: not (xor(true, false)) 3. 1, 2, byinf Proceeding2Results |- true}", "(true ⇔ false) ∧ ¬(true ⩡ false)", 1)>]
    [<DataRow("Proceeding2Results_03", "inf Proceeding2Results{dec p,q:pred; pre:p,q con:and(p,q)} thm T {true} proof T$1 {1: and(true, false) 2: impl(true, false) 3. 1, 2, byinf Proceeding2Results |- true}", "(true ∧ false) ∧ (true ⇒ false)", 1)>]

    // Proceeding3Results: pre: p, q, s
    [<DataRow("Proceeding3Results_01", "inf Proceeding3Results{dec p,q,s:pred; pre:p,q,s con:and(and(p,q),s)} thm T {true} proof T$1 {1: all x:obj {is(x, N)} 2: ex y:obj {is(y, M)} 3: iif(true, false) 4. 1, 2, 3, byinf Proceeding3Results |- true}", "(∀ x:obj {x is N} ∧ ∃ y:obj {y is M}) ∧ (true ⇔ false)", 1)>]
    [<DataRow("Proceeding3Results_02", "inf Proceeding3Results{dec p,q,s:pred; pre:p,q,s con:and(and(p,q),s)} thm T {true} proof T$1 {1: and(true, not(false)) 2: xor(true, false) 3: all z:obj {is(z, K)} 4. 1, 2, 3, byinf Proceeding3Results |- true}", "((true ∧ ¬false) ∧ (true ⩡ false)) ∧ ∀ z:obj {z is K}", 1)>]
    [<DataRow("Proceeding3Results_03", "inf Proceeding3Results{dec p,q,s:pred; pre:p,q,s con:and(and(p,q),s)} thm T {true} proof T$1 {1: not (xor(true, false)) 2: impl(true, false) 3: exn$1 u:obj {is(u, L)} 4. 1, 2, 3, byinf Proceeding3Results |- true}", "(¬(true ⩡ false) ∧ (true ⇒ false)) ∧ ∃! u:obj {u is L}", 1)>]

    // TrueOrAbsorbing or(true,p)
    [<DataRow("TOrAbs_01", "inf TrueOrAbsorbing{dec p:pred; pre:or(true,p) con:true} thm T {true} proof T$1 {1: or(true,false) 2. 1, byinf TrueOrAbsorbing |- true}", "true", 1)>]
    [<DataRow("TOrAbs_03", "inf TrueOrAbsorbing{dec p:pred; pre:or(true,p) con:true} thm T {true} proof T$1 {1: or(true, ex x:obj {is(x,N)}) 2. 1, byinf TrueOrAbsorbing |- true}", "true", 1)>]

    // WeakeningRule: pre: p
    [<DataRow("WeakeningRule_02", "inf WeakeningRule{dec p,q:pred; pre:p con:impl(q,p)} thm T {true} proof T$1 {1: all x:obj {is(x,N)} 2. 1, byinf WeakeningRule |- true}", "q ⇒ ∀ x:obj {x is N}", 1)>]
    [<DataRow("WeakeningRule_03", "inf WeakeningRule{dec p,q:pred; pre:p con:impl(q,p)} thm T {true} proof T$1 {1: and(not (xor(true,false)), iif(true,false)) 2. 1, byinf WeakeningRule |- true}", "q ⇒ (¬(true ⩡ false) ∧ (true ⇔ false))", 1)>]

    // XorAssociative xor(p,xor(q,s))
    [<DataRow("XorA_01", "inf XorAssociative{dec p,q,s:pred; pre:xor(p,xor(q,s)) con:xor(xor(p,q),s)} thm T {true} proof T$1 {1: xor(true, xor(false, true)) 2. 1, byinf XorAssociative |- xor(xor(true,false), true)}", "(true ⩡ false) ⩡ true", 1)>]
    [<DataRow("XorA_03", "inf XorAssociative{dec p,q,s:pred; pre:xor(p,xor(q,s)) con:xor(xor(p,q),s)} thm T {true} proof T$1 {1: xor(iif(true,ex x:obj {is(x,N)}), xor(xor(true,false), true)) 2. 1, byinf XorAssociative |- xor(xor(iif(true,ex x:obj {is(x,N)}), xor(true,false)), true)}", "((true ⇔ ∃ x:obj {x is N}) ⩡ (true ⩡ false)) ⩡ true", 1)>]

    // XorCummutative xor(p,q) 
    [<DataRow("XorC_01", "inf XorCummutative{dec p,q:pred; pre:xor(p,q) con:xor(q,p)} thm T {true} proof T$1 {1: xor(true,false) 2. 1, byinf XorCummutative |- xor(false,true)}", "false ⩡ true", 1)>]
    [<DataRow("XorC_03", "inf XorCummutative{dec p,q:pred; pre:xor(p,q) con:xor(q,p)} thm T {true} proof T$1 {1: xor(and(ex x:obj {is(x,N)}, true), impl(true,false)) 2. 1, byinf XorCummutative |- xor(impl(true,false), and(ex x:obj {is(x,N)}, true))}", "(true ⇒ false) ⩡ (∃ x:obj {x is N} ∧ true)", 1)>]

    // XorUnpack2And: pre: xor(p, q)
    [<DataRow("XorUnpack2And_01", "inf XorUnpack2And{dec p,q:pred; pre:xor(p,q) con:and(or(not p,not q),or(p,q))} thm T {true} proof T$1 {1: xor(and(ex x:obj {is(x, N)}, true), iif(true, false)) 2. 1, byinf XorUnpack2And |- true}", "(¬(∃ x:obj {x is N} ∧ true) ∨ ¬(true ⇔ false)) ∧ ((∃ x:obj {x is N} ∧ true) ∨ (true ⇔ false))", 1)>]
    [<DataRow("XorUnpack2And_02", "inf XorUnpack2And{dec p,q:pred; pre:xor(p,q) con:and(or(not p,not q),or(p,q))} thm T {true} proof T$1 {1: xor(not (all x:obj {is(x, N)}), or(true, false)) 2. 1, byinf XorUnpack2And |- true}", "(¬¬∀ x:obj {x is N} ∨ ¬(true ∨ false)) ∧ (¬∀ x:obj {x is N} ∨ (true ∨ false))", 1)>]
    [<DataRow("XorUnpack2And_03", "inf XorUnpack2And{dec p,q:pred; pre:xor(p,q) con:and(or(not p,not q),or(p,q))} thm T {true} proof T$1 {1: xor(iif(true, true), not (xor(false, true))) 2. 1, byinf XorUnpack2And |- true}", "(¬(true ⇔ true) ∨ ¬¬(false ⩡ true)) ∧ ((true ⇔ true) ∨ ¬(false ⩡ true))", 1)>]

    // XorUnpack2Or: pre: xor(p, q)
    [<DataRow("XorUnpack2Or_01", "inf XorUnpack2Or{dec p,q:pred; pre:xor(p,q) con:or(and(not p,q),and(p,not q))} thm T {true} proof T$1 {1: xor(all x:obj {is(x, N)}, ex y:obj {is(y, M)}) 2. 1, byinf XorUnpack2Or |- true}", "(¬∀ x:obj {x is N} ∧ ∃ y:obj {y is M}) ∨ (∀ x:obj {x is N} ∧ ¬∃ y:obj {y is M})", 1)>]
    [<DataRow("XorUnpack2Or_02", "inf XorUnpack2Or{dec p,q:pred; pre:xor(p,q) con:or(and(not p,q),and(p,not q))} thm T {true} proof T$1 {1: xor(not iif(true, false), and(true, false)) 2. 1, byinf XorUnpack2Or |- true}", "(¬¬(true ⇔ false) ∧ (true ∧ false)) ∨ (¬(true ⇔ false) ∧ ¬(true ∧ false))", 1)>]
    [<DataRow("XorUnpack2Or_03", "inf XorUnpack2Or{dec p,q:pred; pre:xor(p,q) con:or(and(not p,q),and(p,not q))} thm T {true} proof T$1 {1: xor(iif(true, false), xor(true, false)) 2. 1, byinf XorUnpack2Or |- true}", "(¬(true ⇔ false) ∧ (true ⩡ false)) ∨ ((true ⇔ false) ∧ ¬(true ⩡ false))", 1)>]
    [<TestMethod>]
    member this.TestProceedingExpressionJustByInf(no:string, fplCode, expectedExpr:string, expectedNumbExpr:int) =
        
        let filename = "TestProceedingExpressionJustByInf"
        prepareFplCode(filename + ".fpl", fplCode, false) 
        let r = heap.Root

        let candidates = findCandidatesByName "T" false true
        let prf = candidates |> List.filter (fun fv -> fv.FplId = "T$1") |> List.map (fun fv -> fv :?> FplProof) |> List.head

        let fvJiOpt = tryFindJustification prf PrimJIByInf
        match fvJiOpt with
        | Some (:? FplJustificationItemByInf as fvJi) ->
            let result = fvJi.ProceedingExprCandidates
            Assert.AreEqual<int>(expectedNumbExpr, result.Length)
            let actualExpr = result.Head.Type SignatureType.Name
            //if actExprWithoutParens <> expExprWithoutParens then
            //    File.AppendAllText("log.txt", $"{no}\t{fplCode}\t{expectedExpr}\t{expectedNumbExpr}{Environment.NewLine}")
            Assert.AreEqual<string>(expectedExpr, actualExpr)
        | Some ref ->
            Assert.IsInstanceOfType<FplJustificationItemByInf>(ref)
        | None ->
            failwith $"expected FplJustificationItemByInf, found none"

        prepareFplCode(filename, "", false) |> ignore



