namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open ErrDiagnostics
open FplInterpreterTypes
open CommonTestHelpers

[<TestClass>]
type TestSignatureMatching() =

    [<DataRow("""def pred T (x,y:obj) {true} def pred Caller() {dec ~a,b:obj ~c:ind; T(a,b,c)} ;""",
        "no matching paramater for `c:ind` in TestSignatureMatchingReferencesPlain.T(obj, obj)")>]
    [<DataRow("""def pred T (x,y:obj) {true} def pred Caller() {dec ~a,b:obj; T(a,b)} ;""",
        "")>]
    [<DataRow("""def pred T (x,y:Nat) {true} def pred Caller() {dec ~a,b:obj; T(a,b)} ;""",
        "`a:obj` does not match `x:Nat` in TestSignatureMatchingReferencesPlain.T(Nat, Nat)")>]
    [<DataRow("""def pred T (x,y:obj) {true} def pred Caller() {dec ~a,b:Nat; T(a,b)} ;""",
        "`a:Nat` is undefined and does not match `x:obj` in TestSignatureMatchingReferencesPlain.T(obj, obj)")>]
    [<DataRow("""def pred T () {true} def pred Caller() {T()} ;""",
        "")>]
    [<TestMethod>]
    member this.TestSignatureMatchingReferencesPlain(varVal, var:string) =
        ad.Clear()
        let fplCode = sprintf """%s""" varVal
        let filename = "TestSignatureMatchingReferencesPlain"
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
            match matchArgumentsWithParameters fvArgs fvPars with
            | Some errMsg -> Assert.AreEqual<string>(var, errMsg)
            | None -> Assert.AreEqual<string>("no error","no error")
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("""def pred T (x,y:pred) {true} def pred Caller() {dec ~a,b:pred ~c:ind; T(a,b,c)} ;""",
        "no matching paramater for `c:ind` in TestSignatureMatchingReferencesPred.T(pred, pred)")>]
    [<DataRow("""def pred T (x,y:pred) {true} def pred Caller() {dec ~a,b:pred; T(a,b)} ;""",
        "")>]
    [<DataRow("""def pred T (x,y:Nat) {true} def pred Caller() {dec ~a,b:pred; T(a,b)} ;""",
        "`a:pred` does not match `x:Nat` in TestSignatureMatchingReferencesPred.T(Nat, Nat)")>]
    [<DataRow("""def pred T (x,y:pred) {true} def pred Caller() {dec ~a,b:Nat; T(a,b)} ;""",
        "`a:Nat` is undefined and does not match `x:pred` in TestSignatureMatchingReferencesPred.T(pred, pred)")>]
    [<TestMethod>]
    member this.TestSignatureMatchingReferencesPred(varVal, var:string) =
        ad.Clear()
        let fplCode = sprintf """%s""" varVal
        let filename = "TestSignatureMatchingReferencesPred"
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
            match matchArgumentsWithParameters fvArgs fvPars with
            | Some errMsg -> Assert.AreEqual<string>(var, errMsg)
            | None -> Assert.AreEqual<string>("no error","no error")
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("""def pred T (x,y:pred) {true} def pred T3() {true} def pred Caller() {T(T3(),T3())} ;""",
        "")>]
    [<DataRow("""def pred T (x,y:pred) {true} def pred T1() {true} def pred T2(x:obj) {true} def pred Caller() {dec ~a:obj; T(T1(),T2(a))} ;""",
        "")>]
    [<TestMethod>]
    member this.TestSignatureMatchingReferencesPredNested(varVal, var:string) =
        ad.Clear()
        let fplCode = sprintf """%s""" varVal
        let filename = "TestSignatureMatchingReferencesPredNested"
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
            match matchArgumentsWithParameters fvArgs fvPars with
            | Some errMsg -> Assert.AreEqual<string>(var, errMsg)
            | None -> Assert.AreEqual<string>("no error","no error")
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("""def pred T (x,y:obj) {true} def pred Caller() {dec ~a,b:obj ~c:ind; T(a,b,c)} ;""",
        "no matching paramater for `c:ind` in TestSignatureMatchingReferencesClasses.T(obj, obj)")>]
    [<DataRow("""def pred T (x,y:obj) {true} def pred Caller() {dec ~a,b:obj; T(a,b)} ;""",
        "")>]
    [<DataRow("""def pred T (x,y:Nat) {true} def pred Caller() {dec ~a,b:obj; T(a,b)} ;""",
        "`a:obj` does not match `x:Nat` in TestSignatureMatchingReferencesClasses.T(Nat, Nat)")>]
    [<DataRow("""def pred T (x,y:obj) {true} def pred Caller() {dec ~a,b:Nat; T(a,b)} ;""",
        "`a:Nat` is undefined and does not match `x:obj` in TestSignatureMatchingReferencesClasses.T(obj, obj)")>]
    [<DataRow("""def cl Nat:obj {intr} def pred T (x,y:obj) {true} def pred Caller() {dec ~a,b:Nat; T(a,b)} ;""",
        "")>]
    [<DataRow("""def pred T (x,y:tpl) {true} def pred Caller() {dec ~a,b:obj; T(a,b)} ;""",
        "")>]
    [<DataRow("""def cl A:B {intr} def pred T (x,y:obj) {true} def pred Caller() {dec ~a,b:NatTypo; T(a,b)} ;""",
        "`a:NatTypo` is undefined and does not match `x:obj` in TestSignatureMatchingReferencesClasses.T(obj, obj)")>]
    [<TestMethod>]
    member this.TestSignatureMatchingReferencesClasses(varVal, var:string) =
        ad.Clear()
        let fplCode = sprintf """%s""" varVal
        let filename = "TestSignatureMatchingReferencesClasses"
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
            match matchArgumentsWithParameters fvArgs fvPars with
            | Some errMsg -> Assert.AreEqual<string>(var, errMsg)
            | None -> Assert.AreEqual<string>("no error","no error")
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("""def pred T (x,y:Nat) {true} def cl Nat:obj {}def pred Caller() {dec ~a,b:obj ~c:ind; T(a,b,c)} ;""",
        "no matching paramater for `c:ind` in TestSignatureMatchingReferencesClasses.T(obj, obj)")>]
    [<TestMethod>]
    member this.TestSignatureMatchingReferencesConstructors(varVal, var:string) =
        ad.Clear()
        let fplCode = sprintf """%s""" varVal
        let filename = "TestSignatureMatchingReferencesClasses"
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
            match matchArgumentsWithParameters fvArgs fvPars with
            | Some errMsg -> Assert.AreEqual<string>(var, errMsg)
            | None -> Assert.AreEqual<string>("no error","no error")
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("""def pred T (x:*obj) {true} def pred Caller() {dec ~a,b:obj; T(a,b)} ;""",
        "")>]
    [<DataRow("""def pred T (x:*obj) {true} def pred Caller() {dec ~a:obj; T(a)} ;""",
        "")>]
    [<DataRow("""def pred T (x:*obj) {true} def pred Caller() {T()} ;""",
        "")>]
    [<DataRow("""def pred T (x:+obj) {true} def pred Caller() {dec ~a,b:obj; T(a,b)} ;""",
        "")>]
    [<DataRow("""def pred T (x:+obj) {true} def pred Caller() {dec ~a:obj; T(a)} ;""",
        "")>]
    [<DataRow("""def pred T (x:+obj) {true} def pred Caller() {T()} ;""",
        "() does not match `x:+obj` in TestSignatureMatchingReferencesVariadicObj.T(+obj)")>]
    [<DataRow("""def pred T (x:*obj) {true} def pred Caller() {dec ~a,b:*obj; T(a,b)} ;""",
        "no matching paramater for `b:*obj` in TestSignatureMatchingReferencesVariadicObj.T(*obj)")>]
    [<DataRow("""def pred T (x:*obj) {true} def pred Caller() {dec ~a:*obj; T(a)} ;""",
        "")>]
    [<TestMethod>]
    member this.TestSignatureMatchingReferencesVariadicObj(varVal, var:string) =
        ad.Clear()
        let fplCode = sprintf """%s""" varVal
        let filename = "TestSignatureMatchingReferencesVariadicObj"
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
            match matchArgumentsWithParameters fvArgs fvPars with
            | Some errMsg -> Assert.AreEqual<string>(var, errMsg)
            | None -> Assert.AreEqual<string>("no error","no error")
        | None -> 
            Assert.IsTrue(false)
            
    [<DataRow("""def pred T (x:*ind) {true} def pred Caller() {dec ~a,b:ind; T(a,b)} ;""",
        "")>]
    [<DataRow("""def pred T (x:*ind) {true} def pred Caller() {dec ~a:ind; T(a)} ;""",
        "")>]
    [<DataRow("""def pred T (x:*ind) {true} def pred Caller() {T()} ;""",
        "")>]
    [<DataRow("""def pred T (x:+ind) {true} def pred Caller() {dec ~a,b:ind; T(a,b)} ;""",
        "")>]
    [<DataRow("""def pred T (x:+ind) {true} def pred Caller() {dec ~a:ind; T(a)} ;""",
        "")>]
    [<DataRow("""def pred T (x:+ind) {true} def pred Caller() {T()} ;""",
        "() does not match `x:+ind` in TestSignatureMatchingReferencesVariadicInd.T(+ind)")>]
    [<DataRow("""def pred T (x:*ind) {true} def pred Caller() {dec ~a,b:*ind; T(a,b)} ;""",
        "no matching paramater for `b:*ind` in TestSignatureMatchingReferencesVariadicInd.T(*ind)")>]
    [<DataRow("""def pred T (x:*ind) {true} def pred Caller() {dec ~a:*ind; T(a)} ;""",
        "")>]
    [<TestMethod>]
    member this.TestSignatureMatchingReferencesVariadicInd(varVal, var:string) =
        ad.Clear()
        let fplCode = sprintf """%s""" varVal
        let filename = "TestSignatureMatchingReferencesVariadicInd"
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
            match matchArgumentsWithParameters fvArgs fvPars with
            | Some errMsg -> Assert.AreEqual<string>(var, errMsg)
            | None -> Assert.AreEqual<string>("no error","no error")
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("""def pred T (x:*pred) {true} def pred Caller() {dec ~a,b:pred; T(a,b)} ;""",
        "")>]
    [<DataRow("""def pred T (x:*pred) {true} def pred Caller() {dec ~a:pred; T(a)} ;""",
        "")>]
    [<DataRow("""def pred T (x:*pred) {true} def pred Caller() {T()} ;""",
        "")>]
    [<DataRow("""def pred T (x:+pred) {true} def pred Caller() {dec ~a,b:pred; T(a,b)} ;""",
        "")>]
    [<DataRow("""def pred T (x:+pred) {true} def pred Caller() {dec ~a:pred; T(a)} ;""",
        "")>]
    [<DataRow("""def pred T (x:+pred) {true} def pred Caller() {T()} ;""",
        "() does not match `x:+pred` in TestSignatureMatchingReferencesVariadicPred.T(+pred)")>]
    [<DataRow("""def pred T (x:*pred) {true} def pred Caller() {dec ~a,b:*pred; T(a,b)} ;""",
        "no matching paramater for `b:*pred` in TestSignatureMatchingReferencesVariadicPred.T(*pred)")>]
    [<DataRow("""def pred T (x:*pred) {true} def pred Caller() {dec ~a:*pred; T(a)} ;""",
        "")>]
    [<TestMethod>]
    member this.TestSignatureMatchingReferencesVariadicPred(varVal, var:string) =
        ad.Clear()
        let fplCode = sprintf """%s""" varVal
        let filename = "TestSignatureMatchingReferencesVariadicPred"
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
            match matchArgumentsWithParameters fvArgs fvPars with
            | Some errMsg -> Assert.AreEqual<string>(var, errMsg)
            | None -> Assert.AreEqual<string>("no error","no error")
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("""def pred T (x:*func) {true} def pred Caller() {dec ~a,b:func; T(a,b)} ;""",
        "")>]
    [<DataRow("""def pred T (x:*func) {true} def pred Caller() {dec ~a:func; T(a)} ;""",
        "")>]
    [<DataRow("""def pred T (x:*func) {true} def pred Caller() {T()} ;""",
        "")>]
    [<DataRow("""def pred T (x:+func) {true} def pred Caller() {dec ~a,b:func; T(a,b)} ;""",
        "")>]
    [<DataRow("""def pred T (x:+func) {true} def pred Caller() {dec ~a:func; T(a)} ;""",
        "")>]
    [<DataRow("""def pred T (x:+func) {true} def pred Caller() {T()} ;""",
        "() does not match `x:+func` in TestSignatureMatchingReferencesVariadicFunc.T(+func)")>]
    [<DataRow("""def pred T (x:*func) {true} def pred Caller() {dec ~a,b:*func; T(a,b)} ;""",
        "no matching paramater for `b:*func` in TestSignatureMatchingReferencesVariadicFunc.T(*func)")>]
    [<DataRow("""def pred T (x:*func) {true} def pred Caller() {dec ~a:*func; T(a)} ;""",
        "")>]
    [<TestMethod>]
    member this.TestSignatureMatchingReferencesVariadicFunc(varVal, var:string) =
        ad.Clear()
        let fplCode = sprintf """%s""" varVal
        let filename = "TestSignatureMatchingReferencesVariadicFunc"
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
            match matchArgumentsWithParameters fvArgs fvPars with
            | Some errMsg -> Assert.AreEqual<string>(var, errMsg)
            | None -> Assert.AreEqual<string>("no error","no error")
        | None -> 
            Assert.IsTrue(false)
            
    [<DataRow("""def pred T (x:*Nat) {true} def pred Caller() {dec ~a,b:Nat; T(a,b)} ;""",
        "")>]
    [<DataRow("""def pred T (x:*Nat) {true} def pred Caller() {dec ~a:Nat; T(a)} ;""",
        "")>]
    [<DataRow("""def pred T (x:*Nat) {true} def pred Caller() {T()} ;""",
        "")>]
    [<DataRow("""def pred T (x:+Nat) {true} def pred Caller() {dec ~a,b:Nat; T(a,b)} ;""",
        "")>]
    [<DataRow("""def pred T (x:+Nat) {true} def pred Caller() {dec ~a:Nat; T(a)} ;""",
        "")>]
    [<DataRow("""def pred T (x:+Nat) {true} def pred Caller() {T()} ;""",
        "() does not match `x:+Nat` in TestSignatureMatchingReferencesVariadicNat.T(+Nat)")>]
    [<DataRow("""def pred T (x:*Nat) {true} def pred Caller() {dec ~a,b:*Nat; T(a,b)} ;""",
        "no matching paramater for `b:*Nat` in TestSignatureMatchingReferencesVariadicNat.T(*Nat)")>]
    [<DataRow("""def pred T (x:*Nat) {true} def pred Caller() {dec ~a:*Nat; T(a)} ;""",
        "")>]
    [<TestMethod>]
    member this.TestSignatureMatchingReferencesVariadicNat(varVal, var:string) =
        ad.Clear()
        let fplCode = sprintf """%s""" varVal
        let filename = "TestSignatureMatchingReferencesVariadicNat"
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
            match matchArgumentsWithParameters fvArgs fvPars with
            | Some errMsg -> Assert.AreEqual<string>(var, errMsg)
            | None -> Assert.AreEqual<string>("no error","no error")
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
        let filename = "TestBaseClassPath"
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