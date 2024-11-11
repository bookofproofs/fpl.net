namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open ErrDiagnostics
open FplInterpreterTypes
open CommonTestHelpers

[<TestClass>]
type TestSignatureMatching() =

    [<DataRow("""def pred T (x,y:obj) {true} def pred Caller() {dec ~a,b:obj ~c:ind; T(a,b,c)} ;""",
        "T(obj, obj, ind) does not match T(obj, obj); no matching paramater for c:ind.")>]
    [<DataRow("""def pred T (x,y:obj) {true} def pred Caller() {dec ~a,b:obj; T(a,b)} ;""",
        "")>]
    [<DataRow("""def pred T (x,y:Nat) {true} def pred Caller() {dec ~a,b:obj; T(a,b)} ;""",
        "T(obj, obj) does not match T(Nat, Nat); a:obj does not match x:Nat.")>]
    [<DataRow("""def pred T (x,y:obj) {true} def pred Caller() {dec ~a,b:Nat; T(a,b)} ;""",
        "T(Nat, Nat) does not match T(obj, obj); a:Nat is undefined and does not match x:obj.")>]
    [<DataRow("""def cl Nat:obj {intr} def pred T (x,y:obj) {true} def pred Caller() {dec ~a,b:Nat; T(a,b)} ;""",
        "")>]
    [<DataRow("""def pred T (x,y:tpl) {true} def pred Caller() {dec ~a,b:obj; T(a,b)} ;""",
        "")>]
    [<DataRow("""def cl A:B {intr} def pred T (x,y:obj) {true} def pred Caller() {dec ~a,b:NatTypo; T(a,b)} ;""",
        "T(NatTypo, NatTypo) does not match T(obj, obj); a:NatTypo is undefined and does not match x:obj.")>]
    [<TestMethod>]
    member this.TestSignatureMatchingReferences(varVal, var:string) =
        ad.Clear()
        let fplCode = sprintf """%s""" varVal
        let filename = "TestTestSignatureMatchingReferences.fpl"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let blocks = theory.Scope.Values |> Seq.toList 
            let fvPars = blocks |> List.filter(fun fv -> fv.Type(SignatureType.Name).StartsWith("T(")) |> List.head
            let pred = blocks |> List.filter(fun fv -> fv.Type(SignatureType.Name).StartsWith("Caller(")) |> List.head
            let fvArgs = pred.ValueList[0]
            Assert.AreEqual<string>(var, matchParamsWithArguments fvArgs fvPars)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("def cl T:obj {intr};", "T:obj")>]
    [<DataRow("def cl T:Test {intr};", "1")>]
    [<DataRow("def cl T:Test1, object, Test3 {intr};", "T:obj")>]
    [<DataRow("def cl T:Test1, Test2, Test3, object {intr};", "T:obj")>]
    [<DataRow("def cl A:obj {intr} def cl B:obj {intr} def cl C:obj {intr} def cl T:A,B,C,E {ctor D() {dec base.A() base.B() base.C() base.F(); self} };", "T:C:obj")>]
    [<DataRow("def cl A:obj {intr} def cl T:A {ctor B() {dec base.A(); self} };", "T:A:obj")>]
    [<DataRow("def cl A:obj {intr} def cl T:A {ctor B() {dec base.C(); self} };", "T:A:obj")>]
    [<DataRow("def cl T:obj { ctor A() {dec base.obj(); self} };", "T:obj")>]
    [<DataRow("def cl T:obj { ctor A() {dec base.B(); self} };", "T:obj")>]
    [<DataRow("def cl T:C { ctor A() {dec base.obj(); self} };", "None")>]
    [<DataRow("uses Fpl.SetTheory def cl T:Set {ctor Test() {dec base.obj(); self} };", "T:Set:obj")>]
    [<DataRow("uses Fpl.SetTheory def cl T:Set {ctor Test() {dec base.Set(); self} };", "T:Set:obj")>]
    [<DataRow("def cl A:obj {intr} def cl B:A {intr} def cl T:B,A {intr};", "T:B:A:obj")>]
    [<DataRow("uses Fpl.SetTheory def cl T:EmptySet,Set {intr};", "T:EmptySet:Set:obj")>]
    [<DataRow("uses Fpl.SetTheory def cl T:Set, EmptySet {intr};", "T:Set:obj")>]
    [<DataRow("def cl A:obj {intr} def cl B:A {intr} def cl T:A,B {intr};", "T:A:obj")>]
    [<DataRow("def cl A:obj {intr} def cl B:A {intr} def cl T:B {intr};", "T:B:A:obj")>]
    [<DataRow("uses Fpl.SetTheory def cl T:EmptySet {intr};", "T:EmptySet:Set:obj")>]
    [<DataRow("uses Fpl.SetTheory def cl T:Set {intr};", "T:Set:obj")>]
    [<DataRow("uses Fpl.Commons uses Fpl.SetTheory def cl T:Set {intr};", "T:Set:obj")>]
    [<DataRow("def cl A:obj {intr} def cl B:A {intr} def cl T:A {intr};", "T:A:obj")>]
    [<DataRow("def cl A:obj {intr} def cl B:A {intr} def cl T:A,A {intr};", "T:A:obj")>]
    [<DataRow("def cl A:obj {intr} def cl B:A {intr} def cl T:obj,object {intr};", "T:obj")>]
    [<DataRow("def cl A:obj {intr} def cl B:A {intr} def cl T:object,D,E,obj {intr};", "T:obj")>]
    [<DataRow("uses Fpl.SetTheory def cl T:Set,obj {intr};", "T:obj")>]
    [<DataRow("uses Fpl.SetTheory def cl T:EmptySet,obj {intr};", "T:obj")>]
    [<DataRow("uses Fpl.SetTheory def cl T:obj,Set {intr};", "T:obj")>]
    [<DataRow("uses Fpl.SetTheory def cl T:obj,EmptySet {intr};", "T:obj")>]
    [<TestMethod>]
    member this.TestBaseClassPath(varVal:string, var:string) =
        ad.Clear()
        let fplCode = sprintf """%s""" varVal
        let filename = "TestBaseClassPath.fpl"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let blocks = theory.Scope.Values |> Seq.toList 
            let cl = blocks |> List.filter(fun fv -> fv.Type(SignatureType.Name).StartsWith("T")) |> List.head
            let res = findClassInheritanceChain cl "obj"
            match res with 
            | None -> Assert.AreEqual<string>("was not found", "was not found")
            | Some str -> Assert.AreEqual<string>(var, str)
        | None -> 
            Assert.IsTrue(false)