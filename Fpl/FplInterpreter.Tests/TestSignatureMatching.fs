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

    [<DataRow("00", """def pred T(f:func()->obj) {intr} def pred Caller() {dec ~x:func()->obj; T(x)} ;""",
        "")>]
    [<DataRow("01", """def pred T(f:func()->Nat) {intr} def pred Caller() {dec ~x:func()->obj; T(x)} ;""",
        "`obj:obj` does not match `f() -> Nat:func() -> Nat` in TestSignatureMatchingReferencesFunc.T(func() -> Nat)")>]
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
            let fvPars = blocks |> List.filter(fun fv -> fv.Type(SignatureType.Name).StartsWith("T(")) |> List.head
            let pred = blocks |> List.filter(fun fv -> fv.Type(SignatureType.Name).StartsWith("Caller(")) |> List.head
            let fvArgs = pred.ValueList[0]
            match matchArgumentsWithParameters fvArgs fvPars with
            | Some errMsg -> Assert.AreEqual<string>(var, errMsg)
            | None -> Assert.AreEqual<string>("no error","no error")
        | None -> 
            Assert.IsTrue(false)


    [<DataRow("00", """def func T(n,m:obj)->obj {return self(n,m)};""",
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
            let retStmt = pred.ValueList[pred.ValueList.Count - 1]
            let fvArgs = retStmt.ValueList[0]
            match matchArgumentsWithParameters fvArgs pred with
            | Some errMsg -> Assert.AreEqual<string>(var, errMsg)
            | None -> Assert.AreEqual<string>("no error","no error")
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("00", """def func T(y:obj)->obj { intr } def func Caller()->obj {dec ~x:obj; return T(x)} ;""",
        "")>]
    [<DataRow("01", """def func T(y:obj)->obj { return y } def func Caller()->obj {dec ~x:obj; return T(x)} ;""",
        "")>]
    [<DataRow("02", """def func T(y:obj)->obj { intr } def func Caller()->obj {return T(x)} ;""",
        "`x:undef` does not match `y:obj` in TestSignatureMatchingReferencesFuncReturn.T(obj) -> obj")>]
    [<DataRow("03", """def func T(y:obj)->obj { return y } def func Caller()->obj {return T(x)} ;""",
        "`x:undef` does not match `y:obj` in TestSignatureMatchingReferencesFuncReturn.T(obj) -> obj")>]
    [<DataRow("04", """def func T(y:obj)->obj { return y } def func Caller(x:obj)->obj {return T(x)};""",
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
            let fvPars = blocks |> List.filter(fun fv -> fv.Type(SignatureType.Name).StartsWith("T(")) |> List.head
            let pred = blocks |> List.filter(fun fv -> fv.Type(SignatureType.Name).StartsWith("Caller(")) |> List.head
            let retStmt = pred.ValueList[pred.ValueList.Count - 1]
            let fvArgs = retStmt.ValueList[0]
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

    [<DataRow("""def cl A:obj {ctor A(){dec base.obj(); self}} 
                 def cl B:A {ctor B(x:obj){dec base.A(); self}} 
                 def cl C:B {ctor C(){dec base.B(); self}};;""",
        "`()` does not match `x:obj` in TestSignatureMatchingReferencesConstructors.B.B(obj)")>]
    [<DataRow("""def cl A:obj {ctor A(){dec base.obj(); self}} 
                 def cl B:A {ctor B(x:obj){dec base.A(); self}} 
                 def cl C:B {ctor C(){dec ~x:ind base.B(x); self}};;""",
        "`x:ind` does not match `x:obj` in TestSignatureMatchingReferencesConstructors.B.B(obj)")>]
    [<DataRow("""def cl A:obj {ctor A(){dec base.obj(); self}} 
                 def cl B:A {ctor B(x:obj){dec base.A(); self}} 
                 def cl C:B {ctor C(){dec ~x:obj base.B(x); self}};;""",
        "")>]
    [<DataRow("""def cl A:obj {ctor A(){dec base.obj(); self}} 
                 def cl B:A {ctor B(x:A){dec base.A(); self}} 
                 def cl C:B {ctor C(){dec ~x:obj base.B(x); self}};;""",
        "`x:obj` does not match `x:A` in TestSignatureMatchingReferencesConstructors.B.B(A)")>]
    [<DataRow("""def cl A:obj {ctor A(){dec base.obj(); self}} 
                 def cl B:A {ctor B(x:A){dec base.A(); self}} 
                 def cl C:B {ctor C(){dec ~x:A base.B(x); self}};;""",
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
            let testClass = blocks |> List.filter(fun fv -> fv.Type(SignatureType.Name).StartsWith("C")) |> List.head
            let parentClass = testClass.ValueList[0]
            let constructorParentClass = parentClass.Scope.Values |> Seq.toList |> List.head
            let constructor = testClass.Scope.Values |> Seq.toList |> List.head
            let baseConstructorCall = constructor.ValueList |> Seq.filter (fun fv -> fv.BlockType = FplValueType.Stmt && fv.FplId = "bas") |> Seq.toList |> List.head
            let fvArgs = baseConstructorCall.ValueList[0]
            match matchArgumentsWithParameters fvArgs constructorParentClass with
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

    [<DataRow("""def pred T (x:*obj) {dec ~i:ind; x[i]} ;""",
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
            let block = blocks |> List.filter(fun fv -> fv.Type(SignatureType.Name).StartsWith("T(")) |> List.head
            let fvPars = block.Scope |> Seq.map (fun kvp -> kvp.Value) |> Seq.toList |> List.head
            let fvArgs = block.ValueList[0] 
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

    [<DataRow("00", "def cl T:obj {intr};", "T:obj")>]
    [<DataRow("01", "def cl T:Test {intr};", "1")>]
    [<DataRow("02", "def cl T:Test1, object, Test3 {intr};", "T:obj")>]
    [<DataRow("03", "def cl T:Test1, Test2, Test3, object {intr};", "T:obj")>]
    [<DataRow("04", "def cl A:obj {intr} def cl B:obj {intr} def cl C:obj {intr} def cl T:A,B,C,E {ctor D() {dec base.A() base.B() base.C() base.F(); self} };", "T:C:obj")>]
    [<DataRow("05", "def cl A:obj {intr} def cl T:A {ctor B() {dec base.A(); self} };", "T:A:obj")>]
    [<DataRow("06", "def cl A:obj {intr} def cl T:A {ctor B() {dec base.C(); self} };", "T:A:obj")>]
    [<DataRow("07", "def cl T:obj { ctor A() {dec base.obj(); self} };", "T:obj")>]
    [<DataRow("08", "def cl T:obj { ctor A() {dec base.B(); self} };", "T:obj")>]
    [<DataRow("09", "def cl T:C { ctor A() {dec base.obj(); self} };", "None")>]
    [<DataRow("10", "uses Fpl.SetTheory def cl T:Set {ctor Test() {dec base.obj(); self} };", "T:Set:obj")>]
    [<DataRow("11", "uses Fpl.SetTheory def cl T:Set {ctor Test() {dec base.Set(); self} };", "T:Set:obj")>]
    [<DataRow("12", "def cl A:obj {intr} def cl B:A {intr} def cl T:B,A {intr};", "T:B:A:obj")>]
    [<DataRow("13", "uses Fpl.SetTheory def cl T:EmptySet,Set {intr};", "T:EmptySet:Set:obj")>]
    [<DataRow("14", "uses Fpl.SetTheory def cl T:Set, EmptySet {intr};", "T:Set:obj")>]
    [<DataRow("15", "def cl A:obj {intr} def cl B:A {intr} def cl T:A,B {intr};", "T:A:obj")>]
    [<DataRow("16", "def cl A:obj {intr} def cl B:A {intr} def cl T:B {intr};", "T:B:A:obj")>]
    [<DataRow("17", "uses Fpl.SetTheory def cl T:EmptySet {intr};", "T:EmptySet:Set:obj")>]
    [<DataRow("18", "uses Fpl.SetTheory def cl T:Set {intr};", "T:Set:obj")>]
    [<DataRow("19", "uses Fpl.Commons uses Fpl.SetTheory def cl T:Set {intr};", "T:Set:obj")>]
    [<DataRow("20", "def cl A:obj {intr} def cl B:A {intr} def cl T:A {intr};", "T:A:obj")>]
    [<DataRow("21", "def cl A:obj {intr} def cl B:A {intr} def cl T:A,A {intr};", "T:A:obj")>]
    [<DataRow("22", "def cl A:obj {intr} def cl B:A {intr} def cl T:obj,object {intr};", "T:obj")>]
    [<DataRow("23", "def cl A:obj {intr} def cl B:A {intr} def cl T:object,D,E,obj {intr};", "T:obj")>]
    [<DataRow("24", "uses Fpl.SetTheory def cl T:Set,obj {intr};", "T:Set:obj")>]
    [<DataRow("25", "uses Fpl.SetTheory def cl T:EmptySet,obj {intr};", "T:EmptySet:Set:obj")>]
    [<DataRow("26", "uses Fpl.SetTheory def cl T:obj,Set {intr};", "T:obj")>]
    [<DataRow("27", "uses Fpl.SetTheory def cl T:obj,EmptySet {intr};", "T:obj")>]
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
            let cl = blocks |> List.filter(fun fv -> fv.Type(SignatureType.Name).StartsWith("T")) |> List.head
            let res = findClassInheritanceChain cl "obj"
            match res with 
            | None -> Assert.AreEqual<string>("was not found", "was not found")
            | Some str -> Assert.AreEqual<string>(var, str)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("inh", """def cl A:obj { intr } def pred T() {dec ~n:A n:=A(); true};""", "")>]
    [<DataRow("inh_a", """def cl A:obj { intr } def pred T() {dec ~n:obj n:=A(); true};""", "")>]
    [<DataRow("inh_b", """def cl A:obj {intr} def cl B:A { intr } def pred T() {dec ~n:A n:=B(); true};""", "")>]
    [<DataRow("inh_c", """def cl A:obj {intr} def cl B:A { intr } def pred T() {dec ~n:B n:=A(); true};""", "x")>]
    [<DataRow("inh_d", """def cl A:obj {intr} def cl B:A { intr } def pred T() {dec ~n:obj n:=B(); true};""", "")>]
    [<DataRow("inh_e", """def cl A:obj {intr} def cl B:A { intr } def pred T() {dec ~n:obj n:=A(); true};""", "")>]
    [<DataRow("inh_f", """def cl A:obj {intr} def cl B:obj { intr } def pred T() {dec ~n:B n:=A(); true};""", "x")>]
    [<DataRow("inh_g", """def cl A:obj {intr} def cl B:obj { intr } def pred T() {dec ~n:A n:=B(); true};""", "x")>]
    [<DataRow("inh_type_a", """def cl A:obj { intr } def pred T() {dec ~n:ind n:=A(); true};""", "x")>]
    [<DataRow("inh_type_b", """def cl A:obj { intr } def pred T() {dec ~n:pred n:=A(); true};""", "x")>]
    [<DataRow("inh_type_c", """def cl A:obj { intr } def pred T() {dec ~n:func n:=A(); true};""", "x")>]
    [<DataRow("constr_a", """def cl A:obj { ctor A(x:obj) {dec base.obj(); self} } def pred T() {dec ~n:A n:=A(); true};""", "x")>]
    [<DataRow("constr_b", """def cl A:obj { ctor A(x:obj) {dec base.obj(); self} } def pred T() {dec ~n:A ~x:ind n:=A(x); true};""", "x")>]
    [<DataRow("constr_c", """def cl A:obj { ctor A(x:obj) {dec base.obj(); self} } def pred T() {dec ~n:A ~y:pred n:=A(y); true};""", "x")>]
    [<DataRow("constr_d", """def cl A:obj { ctor A(x:obj) {dec base.obj(); self} } def pred T() {dec ~n:A ~z:tpl n:=A(z); true};""", "x")>]
    [<DataRow("constr_e", """def cl A:obj { ctor A(x:obj) {dec base.obj(); self} } def pred T() {dec ~n:obj ~z:obj n:=A(z); true};""", "")>]
    [<DataRow("constr_inh_a", """def cl A:obj { ctor A(x:obj) {dec base.obj(); self} } def cl B:A { ctor B(x:pred) {dec base.A(); self} } def pred T() {dec ~n:A n:=B(); true};""", "x")>]
    [<DataRow("constr_inh_b", """def cl A:obj { ctor A(x:obj) {dec base.obj(); self} } def cl B:A { ctor B(x:pred) {dec base.A(); self} } def pred T() {dec ~n:obj n:=B(); true};""", "y")>]
    [<DataRow("constr_inh_c", """def cl A:obj { ctor A(x:obj) {dec base.obj(); self} } def cl B:A { ctor B(x:pred) {dec base.A(); self} } def pred T() {dec ~n:obj n:=A(); true};""", "y")>]
    [<DataRow("constr_inh_d", """def cl A:obj { ctor A(x:obj) {dec base.obj(); self} } def cl B:A { ctor B(x:pred) {dec base.A(); self} } def pred T() {dec ~n:B ~x:obj n:=A(x); true};""", "x")>]
    [<DataRow("constr_inh_e", """def cl A:obj { ctor A(x:obj) {dec base.obj(); self} } def cl B:obj { ctor B(x:pred) {dec base.obj(); self} } def pred T() {dec ~n:B ~x:obj n:=A(x); true};""", "x")>]
    [<DataRow("constr_inh_f", """def cl A:obj { ctor A(x:obj) {dec base.obj(); self} } def cl B:obj { ctor B(x:pred) {dec base.obj(); self} } def pred T() {dec ~n:A ~x:pred n:=B(x); true};""", "")>]
    [<TestMethod>]
    member this.TestAssignmentsOfConstructors(no:string, varVal:string, var:string) =
        ad.Clear()
        let fplCode = sprintf """%s""" varVal
        let filename = "TestAssignments"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let blocks = theory.Scope.Values |> Seq.toList 
            let pred = blocks |> List.filter(fun fv -> fv.Type(SignatureType.Name).StartsWith("T(")) |> List.head
            let stmtAssign = pred.ValueList[0]
            let fvParsPre = stmtAssign.ValueList[0]
            let fvPars = fvParsPre.GetValue.Value
            let fvArgs = stmtAssign.ValueList[1]
            match matchArgumentsWithParameters fvArgs fvPars with
            | Some errMsg -> Assert.AreEqual<string>(var, errMsg)
            | None -> Assert.AreEqual<string>("no error","no error")
        | None -> 
            Assert.IsTrue(false)
