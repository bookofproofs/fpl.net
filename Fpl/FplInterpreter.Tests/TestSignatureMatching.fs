namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open ErrDiagnostics
open FplPrimitives
open FplInterpreterTypes
open CommonTestHelpers

[<TestClass>]
type TestSignatureMatching() =

    [<DataRow("""def pred T (x,y:Obj) {true} def pred Caller() {dec ~a,b:Obj ~c:ind; T(a,b,c)} ;""",
        "no matching parameter for `c:ind` in TestSignatureMatchingReferencesPlain.T(obj, obj)")>]
    [<DataRow("""def pred T (x,y:Obj) {true} def pred Caller() {dec ~a,b:Obj; T(a,b)} ;""",
        "")>]
    [<DataRow("""def pred T (x,y:Nat) {true} def pred Caller() {dec ~a,b:Obj; T(a,b)} ;""",
        "`a:Obj` does not match `x:Nat` in TestSignatureMatchingReferencesPlain.T(Nat, Nat)")>]
    [<DataRow("""def pred T (x,y:Obj) {true} def pred Caller() {dec ~a,b:Nat; T(a,b)} ;""",
        "`a:Nat` is undefined and does not match `x:Obj` in TestSignatureMatchingReferencesPlain.T(obj, obj)")>]
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
            let fvPars = blocks |> List.filter(fun fv -> (fv.Type(SignatureType.Name)).StartsWith("T(")) |> List.head
            let pred = blocks |> List.filter(fun fv -> (fv.Type(SignatureType.Name)).StartsWith("Caller(")) |> List.head
            let fvArgs = pred.ArgList[0]
            match matchArgumentsWithParameters fvArgs fvPars with
            | Some errMsg -> Assert.AreEqual<string>(var, errMsg)
            | None -> Assert.AreEqual<string>("no error","no error")
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("""def pred T (x,y:pred) {true} def pred Caller() {dec ~a,b:pred ~c:ind; T(a,b,c)} ;""",
        "no matching parameter for `c:ind` in TestSignatureMatchingReferencesPred.T(pred, pred)")>]
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
            let fvPars = blocks |> List.filter(fun fv -> (fv.Type(SignatureType.Name)).StartsWith("T(")) |> List.head
            let pred = blocks |> List.filter(fun fv -> (fv.Type(SignatureType.Name)).StartsWith("Caller(")) |> List.head
            let fvArgs = pred.ArgList[0]
            match matchArgumentsWithParameters fvArgs fvPars with
            | Some errMsg -> Assert.AreEqual<string>(var, errMsg)
            | None -> Assert.AreEqual<string>("no error","no error")
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("00", """def pred T(f:func()->obj) {intr} def pred Caller() {dec ~x:func()->obj; T(x)} ;""",
        "")>]
    [<DataRow("01", """def pred T(f:func()->Nat) {intr} def pred Caller() {dec ~x:func()->obj; T(x)} ;""",
        "`obj:Obj` does not match `f() -> Nat:func() -> Nat` in TestSignatureMatchingReferencesFunc.T(func() -> Nat)")>]
    [<TestMethod>]
    member this.TestSignatureMatchingReferencesFunc(no:string, varVal, var:string) =
        ad.Clear()
        let fplCode = sprintf """%s""" varVal
        let filename = "TestSignatureMatchingReferencesFunc"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let blocks = theory.Scope.Values |> Seq.toList 
            let fvPars = blocks |> List.filter(fun fv -> (fv.Type(SignatureType.Name)).StartsWith("T(")) |> List.head
            let pred = blocks |> List.filter(fun fv -> (fv.Type(SignatureType.Name)).StartsWith("Caller(")) |> List.head
            let fvArgs = pred.ArgList[0]
            match matchArgumentsWithParameters fvArgs fvPars with
            | Some errMsg -> Assert.AreEqual<string>(var, errMsg)
            | None -> Assert.AreEqual<string>("no error","no error")
        | None -> 
            Assert.IsTrue(false)


    [<DataRow("00", """def func T(n,m:Obj)->obj {return self(n,m)};""",
        "")>]
    [<TestMethod>]
    member this.TestSignatureMatchingReferencesFuncReturnSelf(no:string, varVal, var:string) =
        ad.Clear()
        let fplCode = sprintf """%s""" varVal
        let filename = "TestSignatureMatchingReferencesFuncReturnSelf"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let blocks = theory.Scope.Values |> Seq.toList 
            let pred = blocks |> List.head
            let retStmt = pred.ArgList[pred.ArgList.Count - 1]
            let fvArgs = retStmt.ArgList[0]
            match matchArgumentsWithParameters fvArgs pred with
            | Some errMsg -> Assert.AreEqual<string>(var, errMsg)
            | None -> Assert.AreEqual<string>("no error","no error")
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("00", """def func T(y:Obj)->obj { intr } def func Caller()->obj {dec ~x:Obj; return T(x)} ;""",
        "")>]
    [<DataRow("01", """def func T(y:Obj)->obj { return y } def func Caller()->obj {dec ~x:Obj; return T(x)} ;""",
        "")>]
    [<DataRow("02", """def func T(y:Obj)->obj { intr } def func Caller()->obj {return T(x)} ;""",
        "`x:undef` does not match `y:Obj` in TestSignatureMatchingReferencesFuncReturn.T(obj) -> obj")>]
    [<DataRow("03", """def func T(y:Obj)->obj { return y } def func Caller()->obj {return T(x)} ;""",
        "`x:undef` does not match `y:Obj` in TestSignatureMatchingReferencesFuncReturn.T(obj) -> obj")>]
    [<DataRow("04", """def func T(y:Obj)->obj { return y } def func Caller(x:Obj)->obj {return T(x)};""",
        "")>]
    [<TestMethod>]
    member this.TestSignatureMatchingReferencesFuncReturn(no:string, varVal, var:string) =
        ad.Clear()
        let fplCode = sprintf """%s""" varVal
        let filename = "TestSignatureMatchingReferencesFuncReturn"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let blocks = theory.Scope.Values |> Seq.toList 
            let fvPars = blocks |> List.filter(fun fv -> (fv.Type(SignatureType.Name)).StartsWith("T(")) |> List.head
            let pred = blocks |> List.filter(fun fv -> (fv.Type(SignatureType.Name)).StartsWith("Caller(")) |> List.head
            let retStmt = pred.ArgList[pred.ArgList.Count - 1]
            let fvArgs = retStmt.ArgList[0]
            match matchArgumentsWithParameters fvArgs fvPars with
            | Some errMsg -> Assert.AreEqual<string>(var, errMsg)
            | None -> Assert.AreEqual<string>("no error","no error")
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("""def pred T (x,y:pred) {true} def pred T3() {true} def pred Caller() {T(T3(),T3())} ;""",
        "")>]
    [<DataRow("""def pred T (x,y:pred) {true} def pred T1() {true} def pred T2(x:Obj) {true} def pred Caller() {dec ~a:Obj; T(T1(),T2(a))} ;""",
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
            let fvPars = blocks |> List.filter(fun fv -> (fv.Type(SignatureType.Name)).StartsWith("T(")) |> List.head
            let pred = blocks |> List.filter(fun fv -> (fv.Type(SignatureType.Name)).StartsWith("Caller(")) |> List.head
            let fvArgs = pred.ArgList[0]
            match matchArgumentsWithParameters fvArgs fvPars with
            | Some errMsg -> Assert.AreEqual<string>(var, errMsg)
            | None -> Assert.AreEqual<string>("no error","no error")
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("""def pred T (x,y:Obj) {true} def pred Caller() {dec ~a,b:Obj ~c:ind; T(a,b,c)} ;""",
        "no matching parameter for `c:ind` in TestSignatureMatchingReferencesClasses.T(obj, obj)")>]
    [<DataRow("""def pred T (x,y:Obj) {true} def pred Caller() {dec ~a,b:Obj; T(a,b)} ;""",
        "")>]
    [<DataRow("""def pred T (x,y:Nat) {true} def pred Caller() {dec ~a,b:Obj; T(a,b)} ;""",
        "`a:Obj` does not match `x:Nat` in TestSignatureMatchingReferencesClasses.T(Nat, Nat)")>]
    [<DataRow("""def pred T (x,y:Obj) {true} def pred Caller() {dec ~a,b:Nat; T(a,b)} ;""",
        "`a:Nat` is undefined and does not match `x:Obj` in TestSignatureMatchingReferencesClasses.T(obj, obj)")>]
    [<DataRow("""def obj Nat:Obj {intr} def pred T (x,y:Obj) {true} def pred Caller() {dec ~a,b:Nat; T(a,b)} ;""",
        "")>]
    [<DataRow("""def pred T (x,y:tpl) {true} def pred Caller() {dec ~a,b:Obj; T(a,b)} ;""",
        "")>]
    [<DataRow("""def obj A:B {intr} def pred T (x,y:Obj) {true} def pred Caller() {dec ~a,b:NatTypo; T(a,b)} ;""",
        "`a:NatTypo` is undefined and does not match `x:Obj` in TestSignatureMatchingReferencesClasses.T(obj, obj)")>]
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
            let fvPars = blocks |> List.filter(fun fv -> (fv.Type(SignatureType.Name)).StartsWith("T(")) |> List.head
            let pred = blocks |> List.filter(fun fv -> (fv.Type(SignatureType.Name)).StartsWith("Caller(")) |> List.head
            let fvArgs = pred.ArgList[0]
            match matchArgumentsWithParameters fvArgs fvPars with
            | Some errMsg -> Assert.AreEqual<string>(var, errMsg)
            | None -> Assert.AreEqual<string>("no error","no error")
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("""def obj A:Obj {ctor A(){dec base.obj(); }} 
                 def obj B:A {ctor B(x:Obj){dec base.A(); }} 
                 def obj C:B {ctor C(){dec base.B(); }};;""",
        "`()` does not match `x:Obj` in TestSignatureMatchingReferencesConstructors.B.B(obj)")>]
    [<DataRow("""def obj A:Obj {ctor A(){dec base.obj(); }} 
                 def obj B:A {ctor B(x:Obj){dec base.A(); }} 
                 def obj C:B {ctor C(){dec ~x:ind base.B(x); }};;""",
        "`x:ind` does not match `x:Obj` in TestSignatureMatchingReferencesConstructors.B.B(obj)")>]
    [<DataRow("""def obj A:Obj {ctor A(){dec base.obj(); }} 
                 def obj B:A {ctor B(x:Obj){dec base.A(); }} 
                 def obj C:B {ctor C(){dec ~x:Obj base.B(x); }};;""",
        "")>]
    [<DataRow("""def obj A:Obj {ctor A(){dec base.obj(); }} 
                 def obj B:A {ctor B(x:A){dec base.A(); }} 
                 def obj C:B {ctor C(){dec ~x:Obj base.B(x); }};;""",
        "`x:Obj` does not match `x:A` in TestSignatureMatchingReferencesConstructors.B.B(A)")>]
    [<DataRow("""def obj A:Obj {ctor A(){dec base.obj(); }} 
                 def obj B:A {ctor B(x:A){dec base.A(); }} 
                 def obj C:B {ctor C(){dec ~x:A base.B(x); }};;""",
        "")>]
    [<TestMethod>]
    member this.TestSignatureMatchingReferencesConstructors(varVal, var:string) =
        ad.Clear()
        let fplCode = sprintf """%s""" varVal
        let filename = "TestSignatureMatchingReferencesConstructors"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let blocks = theory.Scope.Values |> Seq.toList 
            let testClass = blocks |> List.filter(fun fv -> (fv.Type(SignatureType.Name)).StartsWith("C")) |> List.head
            let parentClass = testClass.ArgList[0]
            let constructorParentClass = parentClass.Scope.Values |> Seq.toList |> List.head
            let constructor = testClass.Scope.Values |> Seq.toList |> List.head
            let baseConstructorCall = constructor.ArgList |> Seq.filter (fun fv -> fv.FplId = "bas") |> Seq.toList |> List.head
            let fvArgs = baseConstructorCall.ArgList[0]
            match matchArgumentsWithParameters fvArgs constructorParentClass with
            | Some errMsg -> Assert.AreEqual<string>(var, errMsg)
            | None -> Assert.AreEqual<string>("no error","no error")
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("""def pred T (x:*Obj) {true} def pred Caller() {dec ~a,b:Obj; T(a,b)} ;""",
        "")>]
    [<DataRow("""def pred T (x:*Obj) {true} def pred Caller() {dec ~a:Obj; T(a)} ;""",
        "")>]
    [<DataRow("""def pred T (x:*Obj) {true} def pred Caller() {T()} ;""",
        "")>]
    [<DataRow("""def pred T (x:+Obj) {true} def pred Caller() {dec ~a,b:Obj; T(a,b)} ;""",
        "")>]
    [<DataRow("""def pred T (x:+Obj) {true} def pred Caller() {dec ~a:Obj; T(a)} ;""",
        "")>]
    [<DataRow("""def pred T (x:+Obj) {true} def pred Caller() {T()} ;""",
        "() does not match `x:+Obj` in TestSignatureMatchingReferencesVariadicObj.T(+Obj)")>]
    [<DataRow("""def pred T (x:*Obj) {true} def pred Caller() {dec ~a,b:*Obj; T(a,b)} ;""",
        "no matching parameter for `b:*Obj` in TestSignatureMatchingReferencesVariadicObj.T(*Obj)")>]
    [<DataRow("""def pred T (x:*Obj) {true} def pred Caller() {dec ~a:*Obj; T(a)} ;""",
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
            let fvPars = blocks |> List.filter(fun fv -> (fv.Type(SignatureType.Name)).StartsWith("T(")) |> List.head
            let pred = blocks |> List.filter(fun fv -> (fv.Type(SignatureType.Name)).StartsWith("Caller(")) |> List.head
            let fvArgs = pred.ArgList[0]
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
        "no matching parameter for `b:*ind` in TestSignatureMatchingReferencesVariadicInd.T(*ind)")>]
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
            let fvPars = blocks |> List.filter(fun fv -> (fv.Type(SignatureType.Name)).StartsWith("T(")) |> List.head
            let pred = blocks |> List.filter(fun fv -> (fv.Type(SignatureType.Name)).StartsWith("Caller(")) |> List.head
            let fvArgs = pred.ArgList[0]
            match matchArgumentsWithParameters fvArgs fvPars with
            | Some errMsg -> Assert.AreEqual<string>(var, errMsg)
            | None -> Assert.AreEqual<string>("no error","no error")
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("""def pred T (x:*Obj) {dec ~i:ind; x[i]} ;""",
        "")>]
    [<TestMethod>]
    member this.TestSignatureMatchingReferencesVariadicCoord(varVal, var:string) =
        ad.Clear()
        let fplCode = sprintf """%s""" varVal
        let filename = "TestSignatureMatchingReferencesVariadicCoord"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let blocks = theory.Scope.Values |> Seq.toList 
            let block = blocks |> List.filter(fun fv -> (fv.Type(SignatureType.Name)).StartsWith("T(")) |> List.head
            let fvPars = block.Scope |> Seq.map (fun kvp -> kvp.Value) |> Seq.toList |> List.head
            let fvArgs = block.ArgList[0] 
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
        "no matching parameter for `b:*pred` in TestSignatureMatchingReferencesVariadicPred.T(*pred)")>]
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
            let fvPars = blocks |> List.filter(fun fv -> (fv.Type(SignatureType.Name)).StartsWith("T(")) |> List.head
            let pred = blocks |> List.filter(fun fv -> (fv.Type(SignatureType.Name)).StartsWith("Caller(")) |> List.head
            let fvArgs = pred.ArgList[0]
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
        "no matching parameter for `b:*func` in TestSignatureMatchingReferencesVariadicFunc.T(*func)")>]
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
            let fvPars = blocks |> List.filter(fun fv -> (fv.Type(SignatureType.Name)).StartsWith("T(")) |> List.head
            let pred = blocks |> List.filter(fun fv -> (fv.Type(SignatureType.Name)).StartsWith("Caller(")) |> List.head
            let fvArgs = pred.ArgList[0]
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
        "no matching parameter for `b:*Nat` in TestSignatureMatchingReferencesVariadicNat.T(*Nat)")>]
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
            let fvPars = blocks |> List.filter(fun fv -> (fv.Type(SignatureType.Name)).StartsWith("T(")) |> List.head
            let pred = blocks |> List.filter(fun fv -> (fv.Type(SignatureType.Name)).StartsWith("Caller(")) |> List.head
            let fvArgs = pred.ArgList[0]
            match matchArgumentsWithParameters fvArgs fvPars with
            | Some errMsg -> Assert.AreEqual<string>(var, errMsg)
            | None -> Assert.AreEqual<string>("no error","no error")
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("00", "def obj T:Obj {intr};", "T:Obj")>]
    [<DataRow("01", "def obj T:Test {intr};", "1")>]
    [<DataRow("02", "def obj T:Test1, object, Test3 {intr};", "T:Obj")>]
    [<DataRow("03", "def obj T:Test1, Test2, Test3, object {intr};", "T:Obj")>]
    [<DataRow("04", "def obj A:Obj {intr} def obj B:Obj {intr} def obj C:Obj {intr} def obj T:A,B,C,E {ctor D() {dec base.A() base.B() base.C() base.F(); } };", "T:C:Obj")>]
    [<DataRow("05", "def obj A:Obj {intr} def obj T:A {ctor B() {dec base.A(); } };", "T:A:Obj")>]
    [<DataRow("06", "def obj A:Obj {intr} def obj T:A {ctor B() {dec base.C(); } };", "T:A:Obj")>]
    [<DataRow("07", "def obj T:Obj { ctor A() {dec base.obj(); } };", "T:Obj")>]
    [<DataRow("08", "def obj T:Obj { ctor A() {dec base.B(); } };", "T:Obj")>]
    [<DataRow("09", "def obj T:C { ctor A() {dec base.obj(); } };", "None")>]
    [<DataRow("10", "uses Fpl.SetTheory def obj T:Set {ctor Test() {dec base.obj(); } };", "T:Set:Obj")>]
    [<DataRow("11", "uses Fpl.SetTheory def obj T:Set {ctor Test() {dec base.Set(); } };", "T:Set:Obj")>]
    [<DataRow("12", "def obj A:Obj {intr} def obj B:A {intr} def obj T:B,A {intr};", "T:B:A:Obj")>]
    [<DataRow("13", "uses Fpl.SetTheory def obj T:EmptySet,Set {intr};", "T:EmptySet:Set:Obj")>]
    [<DataRow("14", "uses Fpl.SetTheory def obj T:Set, EmptySet {intr};", "T:Set:Obj")>]
    [<DataRow("15", "def obj A:Obj {intr} def obj B:A {intr} def obj T:A,B {intr};", "T:A:Obj")>]
    [<DataRow("16", "def obj A:Obj {intr} def obj B:A {intr} def obj T:B {intr};", "T:B:A:Obj")>]
    [<DataRow("17", "uses Fpl.SetTheory def obj T:EmptySet {intr};", "T:EmptySet:Set:Obj")>]
    [<DataRow("18", "uses Fpl.SetTheory def obj T:Set {intr};", "T:Set:Obj")>]
    [<DataRow("19", "uses Fpl.Commons uses Fpl.SetTheory def obj T:Set {intr};", "T:Set:Obj")>]
    [<DataRow("20", "def obj A:Obj {intr} def obj B:A {intr} def obj T:A {intr};", "T:A:Obj")>]
    [<DataRow("21", "def obj A:Obj {intr} def obj B:A {intr} def obj T:A,A {intr};", "T:A:Obj")>]
    [<DataRow("22", "def obj A:Obj {intr} def obj B:A {intr} def obj T:Obj,object {intr};", "T:Obj")>]
    [<DataRow("23", "def obj A:Obj {intr} def obj B:A {intr} def obj T:Object,D,E,obj {intr};", "T:Obj")>]
    [<DataRow("24", "uses Fpl.SetTheory def obj T:Set,obj {intr};", "T:Set:Obj")>]
    [<DataRow("25", "uses Fpl.SetTheory def obj T:EmptySet,obj {intr};", "T:EmptySet:Set:Obj")>]
    [<DataRow("26", "uses Fpl.SetTheory def obj T:Obj,Set {intr};", "T:Obj")>]
    [<DataRow("27", "uses Fpl.SetTheory def obj T:Obj,EmptySet {intr};", "T:Obj")>]
    [<TestMethod>]
    member this.TestBaseClassPath(no:string, varVal:string, var:string) =
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
            let cl = blocks |> List.filter(fun fv -> (fv.Type(SignatureType.Name)).StartsWith("T")) |> List.head
            if inheritsFrom cl LiteralObj then
                let str = findInheritanceChains cl LiteralObj |> Seq.map (fun kvp -> kvp.Key) |> Seq.head
                Assert.AreEqual<string>(var, str)
            else 
                Assert.AreEqual<string>("was not found", "was not found")
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("inh", """def obj A:Obj { intr } def pred T() {dec ~n:A n:=A(); true};""", "")>]
    [<DataRow("inh_a", """def obj A:Obj { intr } def pred T() {dec ~n:Obj n:=A(); true};""", "")>]
    [<DataRow("inh_b", """def obj A:Obj {intr} def obj B:A { intr } def pred T() {dec ~n:A n:=B(); true};""", "")>]
    [<DataRow("inh_c", """def obj A:Obj {intr} def obj B:A { intr } def pred T() {dec ~n:B n:=A(); true};""", "x")>]
    [<DataRow("inh_d", """def obj A:Obj {intr} def obj B:A { intr } def pred T() {dec ~n:Obj n:=B(); true};""", "")>]
    [<DataRow("inh_e", """def obj A:Obj {intr} def obj B:A { intr } def pred T() {dec ~n:Obj n:=A(); true};""", "")>]
    [<DataRow("inh_f", """def obj A:Obj {intr} def obj B:Obj { intr } def pred T() {dec ~n:B n:=A(); true};""", "x")>]
    [<DataRow("inh_g", """def obj A:Obj {intr} def obj B:Obj { intr } def pred T() {dec ~n:A n:=B(); true};""", "x")>]
    [<DataRow("inh_type_a", """def obj A:Obj { intr } def pred T() {dec ~n:ind n:=A(); true};""", "x")>]
    [<DataRow("inh_type_b", """def obj A:Obj { intr } def pred T() {dec ~n:pred n:=A(); true};""", "x")>]
    [<DataRow("inh_type_c", """def obj A:Obj { intr } def pred T() {dec ~n:func n:=A(); true};""", "x")>]
    [<DataRow("constr_a", """def obj A:Obj { ctor A(x:Obj) {dec base.obj(); } } def pred T() {dec ~n:A n:=A(); true};""", "x")>]
    [<DataRow("constr_b", """def obj A:Obj { ctor A(x:Obj) {dec base.obj(); } } def pred T() {dec ~n:A ~x:ind n:=A(x); true};""", "x")>]
    [<DataRow("constr_c", """def obj A:Obj { ctor A(x:Obj) {dec base.obj(); } } def pred T() {dec ~n:A ~y:pred n:=A(y); true};""", "x")>]
    [<DataRow("constr_d", """def obj A:Obj { ctor A(x:Obj) {dec base.obj(); } } def pred T() {dec ~n:A ~z:tpl n:=A(z); true};""", "x")>]
    [<DataRow("constr_e", """def obj A:Obj { ctor A(x:Obj) {dec base.obj(); } } def pred T() {dec ~n:Obj ~z:Obj n:=A(z); true};""", "")>]
    [<DataRow("constr_inh_a", """def obj A:Obj { ctor A(x:Obj) {dec base.obj(); } } def obj B:A { ctor B(x:pred) {dec base.A(); } } def pred T() {dec ~n:A n:=B(); true};""", "x")>]
    [<DataRow("constr_inh_b", """def obj A:Obj { ctor A(x:Obj) {dec base.obj(); } } def obj B:A { ctor B(x:pred) {dec base.A(); } } def pred T() {dec ~n:Obj n:=B(); true};""", "y")>]
    [<DataRow("constr_inh_c", """def obj A:Obj { ctor A(x:Obj) {dec base.obj(); } } def obj B:A { ctor B(x:pred) {dec base.A(); } } def pred T() {dec ~n:Obj n:=A(); true};""", "y")>]
    [<DataRow("constr_inh_d", """def obj A:Obj { ctor A(x:Obj) {dec base.obj(); } } def obj B:A { ctor B(x:pred) {dec base.A(); } } def pred T() {dec ~n:B ~x:Obj n:=A(x); true};""", "x")>]
    [<DataRow("constr_inh_e", """def obj A:Obj { ctor A(x:Obj) {dec base.obj(); } } def obj B:Obj { ctor B(x:pred) {dec base.obj(); } } def pred T() {dec ~n:B ~x:Obj n:=A(x); true};""", "x")>]
    [<DataRow("constr_inh_f", """def obj A:Obj { ctor A(x:Obj) {dec base.obj(); } } def obj B:Obj { ctor B(x:pred) {dec base.obj(); } } def pred T() {dec ~n:A ~x:pred n:=B(x); true};""", "")>]
    [<TestMethod>]
    member this.TestAssignmentsOfConstructors(no:string, varVal:string, var:string) =
        ad.Clear()
        let fplCode = sprintf """%s""" varVal
        let filename = "TestAssignmentsOfConstructors"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let blocks = theory.Scope.Values |> Seq.toList 
            let pred = blocks |> List.filter(fun fv -> (fv.Type(SignatureType.Name)).StartsWith("T(")) |> List.head
            let stmtAssign = pred.ArgList[0]
            let fvParsPre = stmtAssign.ArgList[0]
            let fvPars = (getArgument fvParsPre).Value
            let fvArgs = stmtAssign.ArgList[1]
            match matchArgumentsWithParameters fvArgs fvPars with
            | Some errMsg -> Assert.AreEqual<string>(var, errMsg)
            | None -> Assert.AreEqual<string>("no error","no error")
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("00", """def obj A:Obj { intr } def pred T() {dec ~n:A n:=A; true};""", "A")>]
    [<DataRow("01", """def obj A:Obj { ctor A(x:Obj) {dec base.obj(); } } def pred T() {dec ~n:A ~x:Obj n:=A(x); true};""", "A(obj)")>]
    [<DataRow("02", """def obj A:Obj { ctor A(x:pred) {dec base.obj(); } } def pred T() {dec ~n:A ~x:pred n:=A(x); true};""", "A(pred)")>]
    [<DataRow("03a", """def obj A:Obj { ctor A(x:Obj) {dec base.obj(); } ctor A(x:pred) {dec base.obj(); } ctor A(x:ind) {dec base.obj(); } } def pred T() {dec ~n:A ~x:Obj n:=A(x); true};""", "A(obj)")>]
    [<DataRow("03b", """def obj A:Obj { ctor A(x:Obj) {dec base.obj(); } ctor A(x:pred) {dec base.obj(); } ctor A(x:ind) {dec base.obj(); } } def pred T() {dec ~n:A ~x:pred n:=A(x); true};""", "A(pred)")>]
    [<DataRow("03c", """def obj A:Obj { ctor A(x:Obj) {dec base.obj(); } ctor A(x:pred) {dec base.obj(); } ctor A(x:ind) {dec base.obj(); } } def pred T() {dec ~n:A ~x:ind n:=A(x); true};""", "A(ind)")>]
    [<TestMethod>]
    /// Test if a reference of the assigned value gets the correct candidate 
    /// depending on its signature and the available constructor candidates in the referenced class
    /// regardless of the order in which the constructors in the referenced class are declared.
    member this.TestAssignmentsOfConstructorsCorrectCandidates(no:string, varVal:string, expectedCandidateSignature:string) =
        ad.Clear()
        let fplCode = sprintf """%s""" varVal
        let filename = "TestAssignmentsOfConstructorsCorrectCandidates"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let blocks = theory.Scope.Values |> Seq.toList 
            let pred = blocks |> List.filter(fun fv -> (fv.Type(SignatureType.Name)).StartsWith("T(")) |> List.head
            let stmtAssign = pred.ArgList[0]
            let assignedReferenceValue = stmtAssign.ArgList[1]
            let candidate = assignedReferenceValue.Scope[assignedReferenceValue.FplId]
            Assert.AreEqual<string>(expectedCandidateSignature, candidate.Type(SignatureType.Type))
        | None -> 
            Assert.IsTrue(false)
