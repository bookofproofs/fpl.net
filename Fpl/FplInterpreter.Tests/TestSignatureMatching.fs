namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open ErrDiagnostics
open FplPrimitives
open FplInterpreterTypes
open CommonTestHelpers

[<TestClass>]
type TestSignatureMatching() =

    [<DataRow("""def pred T (x,y:obj) {true} def pred Caller() {dec ~a,b:obj ~c:ind; T(a,b,c)} ;""",
        "no matching parameter for `c:ind` in TestSignatureMatchingReferencesPlain.T(obj, obj)")>]
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
            let fvPars = blocks |> List.filter(fun fv -> (fv.Type(SignatureType.Name)).StartsWith("T(")) |> List.head
            let pred = blocks |> List.filter(fun fv -> (fv.Type(SignatureType.Name)).StartsWith("Caller(")) |> List.head
            let fvArgs = pred.ArgList[0]
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
            let retStmt = pred.ArgList[pred.ArgList.Count - 1]
            let fvArgs = retStmt.ArgList[0]
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
            let fvPars = blocks |> List.filter(fun fv -> (fv.Type(SignatureType.Name)).StartsWith("T(")) |> List.head
            let pred = blocks |> List.filter(fun fv -> (fv.Type(SignatureType.Name)).StartsWith("Caller(")) |> List.head
            let fvArgs = pred.ArgList[0]
            match matchArgumentsWithParameters fvArgs fvPars with
            | Some errMsg -> Assert.AreEqual<string>(var, errMsg)
            | None -> Assert.AreEqual<string>("no error","no error")
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("""def pred T (x,y:obj) {true} def pred Caller() {dec ~a,b:obj ~c:ind; T(a,b,c)} ;""",
        "no matching parameter for `c:ind` in TestSignatureMatchingReferencesClasses.T(obj, obj)")>]
    [<DataRow("""def pred T (x,y:obj) {true} def pred Caller() {dec ~a,b:obj; T(a,b)} ;""",
        "")>]
    [<DataRow("""def pred T (x,y:Nat) {true} def pred Caller() {dec ~a,b:obj; T(a,b)} ;""",
        "`a:obj` does not match `x:Nat` in TestSignatureMatchingReferencesClasses.T(Nat, Nat)")>]
    [<DataRow("""def pred T (x,y:obj) {true} def pred Caller() {dec ~a,b:Nat; T(a,b)} ;""",
        "`a:Nat` is undefined and does not match `x:obj` in TestSignatureMatchingReferencesClasses.T(obj, obj)")>]
    [<DataRow("""def cl Nat {intr} def pred T (x,y:obj) {true} def pred Caller() {dec ~a,b:Nat; T(a,b)} ;""",
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
            let fvPars = blocks |> List.filter(fun fv -> (fv.Type(SignatureType.Name)).StartsWith("T(")) |> List.head
            let pred = blocks |> List.filter(fun fv -> (fv.Type(SignatureType.Name)).StartsWith("Caller(")) |> List.head
            let fvArgs = pred.ArgList[0]
            match matchArgumentsWithParameters fvArgs fvPars with
            | Some errMsg -> Assert.AreEqual<string>(var, errMsg)
            | None -> Assert.AreEqual<string>("no error","no error")
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("""def cl A {ctor A(){dec base.Obj(); }} 
                 def cl B:A {ctor B(x:obj){dec base.A(); }} 
                 def cl C:B {ctor C(){dec base.B(); }};;""",
        "`()` does not match `x:obj` in TestSignatureMatchingReferencesConstructors.B.B(obj)")>]
    [<DataRow("""def cl A {ctor A(){dec base.Obj(); }} 
                 def cl B:A {ctor B(x:obj){dec base.A(); }} 
                 def cl C:B {ctor C(){dec ~x:ind base.B(x); }};;""",
        "`x:ind` does not match `x:obj` in TestSignatureMatchingReferencesConstructors.B.B(obj)")>]
    [<DataRow("""def cl A {ctor A(){dec base.Obj(); }} 
                 def cl B:A {ctor B(x:obj){dec base.A(); }} 
                 def cl C:B {ctor C(){dec ~x:obj base.B(x); }};;""",
        "")>]
    [<DataRow("""def cl A {ctor A(){dec base.Obj(); }} 
                 def cl B:A {ctor B(x:A){dec base.A(); }} 
                 def cl C:B {ctor C(){dec ~x:obj base.B(x); }};;""",
        "`x:obj` does not match `x:A` in TestSignatureMatchingReferencesConstructors.B.B(A)")>]
    [<DataRow("""def cl A {ctor A(){dec base.Obj(); }} 
                 def cl B:A {ctor B(x:A){dec base.A(); }} 
                 def cl C:B {ctor C(){dec ~x:A base.B(x); }};;""",
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

    [<DataRow("00", """def pred T (x:*obj) {true} def pred Caller() {dec ~a,b:obj; T(a,b)} ;""",
        "")>]
    [<DataRow("01", """def pred T (x:*obj) {true} def pred Caller() {dec ~a:obj; T(a)} ;""",
        "")>]
    [<DataRow("02", """def pred T (x:*obj) {true} def pred Caller() {T()} ;""",
        "")>]
    [<DataRow("03", """def pred T (x:+obj) {true} def pred Caller() {dec ~a,b:obj; T(a,b)} ;""",
        "")>]
    [<DataRow("04", """def pred T (x:+obj) {true} def pred Caller() {dec ~a:obj; T(a)} ;""",
        "")>]
    [<DataRow("05", """def pred T (x:+obj) {true} def pred Caller() {T()} ;""",
        "missing argument for `x:+obj` in TestSignatureMatchingReferencesVariadicObj.T(+obj)")>]
    [<DataRow("06", """def pred T (x:*obj) {true} def pred Caller() {dec ~a,b:*obj; T(a,b)} ;""",
        "no matching parameter for `b:*obj` in TestSignatureMatchingReferencesVariadicObj.T(*obj)")>]
    [<DataRow("07", """def pred T (x:*obj) {true} def pred Caller() {dec ~a:*obj; T(a)} ;""",
        "")>]
    [<TestMethod>]
    member this.TestSignatureMatchingReferencesVariadicObj(no:string, varVal, var:string) =
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
            
    [<DataRow("00", """def pred T (x:*ind) {true} def pred Caller() {dec ~a,b:ind; T(a,b)} ;""",
        "")>]
    [<DataRow("01", """def pred T (x:*ind) {true} def pred Caller() {dec ~a:ind; T(a)} ;""",
        "")>]
    [<DataRow("02", """def pred T (x:*ind) {true} def pred Caller() {T()} ;""",
        "")>]
    [<DataRow("03", """def pred T (x:+ind) {true} def pred Caller() {dec ~a,b:ind; T(a,b)} ;""",
        "")>]
    [<DataRow("04", """def pred T (x:+ind) {true} def pred Caller() {dec ~a:ind; T(a)} ;""",
        "")>]
    [<DataRow("05", """def pred T (x:+ind) {true} def pred Caller() {T()} ;""",
        "missing argument for `x:+ind` in TestSignatureMatchingReferencesVariadicInd.T(+ind)")>]
    [<DataRow("06", """def pred T (x:*ind) {true} def pred Caller() {dec ~a,b:*ind; T(a,b)} ;""",
        "no matching parameter for `b:*ind` in TestSignatureMatchingReferencesVariadicInd.T(*ind)")>]
    [<DataRow("07", """def pred T (x:*ind) {true} def pred Caller() {dec ~a:*ind; T(a)} ;""",
        "")>]
    [<TestMethod>]
    member this.TestSignatureMatchingReferencesVariadicInd(no:string, varVal, var:string) =
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
            let block = blocks |> List.filter(fun fv -> (fv.Type(SignatureType.Name)).StartsWith("T(")) |> List.head
            let fvPars = block.Scope |> Seq.map (fun kvp -> kvp.Value) |> Seq.toList |> List.head
            let fvArgs = block.ArgList[0] 
            match matchArgumentsWithParameters fvArgs fvPars with
            | Some errMsg -> Assert.AreEqual<string>(var, errMsg)
            | None -> Assert.AreEqual<string>("no error","no error")
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("00", """def pred T (x:*pred) {true} def pred Caller() {dec ~a,b:pred; T(a,b)} ;""",
        "")>]
    [<DataRow("01", """def pred T (x:*pred) {true} def pred Caller() {dec ~a:pred; T(a)} ;""",
        "")>]
    [<DataRow("02", """def pred T (x:*pred) {true} def pred Caller() {T()} ;""",
        "")>]
    [<DataRow("03", """def pred T (x:+pred) {true} def pred Caller() {dec ~a,b:pred; T(a,b)} ;""",
        "")>]
    [<DataRow("04", """def pred T (x:+pred) {true} def pred Caller() {dec ~a:pred; T(a)} ;""",
        "")>]
    [<DataRow("05", """def pred T (x:+pred) {true} def pred Caller() {T()} ;""",
        "missing argument for `x:+pred` in TestSignatureMatchingReferencesVariadicPred.T(+pred)")>]
    [<DataRow("06", """def pred T (x:*pred) {true} def pred Caller() {dec ~a,b:*pred; T(a,b)} ;""",
        "no matching parameter for `b:*pred` in TestSignatureMatchingReferencesVariadicPred.T(*pred)")>]
    [<DataRow("07", """def pred T (x:*pred) {true} def pred Caller() {dec ~a:*pred; T(a)} ;""",
        "")>]
    [<TestMethod>]
    member this.TestSignatureMatchingReferencesVariadicPred(no:string, varVal, var:string) =
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

    [<DataRow("00", """def pred T (x:*func) {true} def pred Caller() {dec ~a,b:func; T(a,b)} ;""",
        "")>]
    [<DataRow("01", """def pred T (x:*func) {true} def pred Caller() {dec ~a:func; T(a)} ;""",
        "")>]
    [<DataRow("02", """def pred T (x:*func) {true} def pred Caller() {T()} ;""",
        "")>]
    [<DataRow("03", """def pred T (x:+func) {true} def pred Caller() {dec ~a,b:func; T(a,b)} ;""",
        "")>]
    [<DataRow("04", """def pred T (x:+func) {true} def pred Caller() {dec ~a:func; T(a)} ;""",
        "")>]
    [<DataRow("05", """def pred T (x:+func) {true} def pred Caller() {T()} ;""",
        "missing argument for `x:+func` in TestSignatureMatchingReferencesVariadicFunc.T(+func)")>]
    [<DataRow("06", """def pred T (x:*func) {true} def pred Caller() {dec ~a,b:*func; T(a,b)} ;""",
        "no matching parameter for `b:*func` in TestSignatureMatchingReferencesVariadicFunc.T(*func)")>]
    [<DataRow("07", """def pred T (x:*func) {true} def pred Caller() {dec ~a:*func; T(a)} ;""",
        "")>]
    [<TestMethod>]
    member this.TestSignatureMatchingReferencesVariadicFunc(no:string, varVal, var:string) =
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
            
    [<DataRow("00", """def pred T (x:*Nat) {true} def pred Caller() {dec ~a,b:Nat; T(a,b)} ;""",
        "")>]
    [<DataRow("01", """def pred T (x:*Nat) {true} def pred Caller() {dec ~a:Nat; T(a)} ;""",
        "")>]
    [<DataRow("02", """def pred T (x:*Nat) {true} def pred Caller() {T()} ;""",
        "")>]
    [<DataRow("03", """def pred T (x:+Nat) {true} def pred Caller() {dec ~a,b:Nat; T(a,b)} ;""",
        "")>]
    [<DataRow("04", """def pred T (x:+Nat) {true} def pred Caller() {dec ~a:Nat; T(a)} ;""",
        "")>]
    [<DataRow("05", """def pred T (x:+Nat) {true} def pred Caller() {T()} ;""",
        "missing argument for `x:+Nat` in TestSignatureMatchingReferencesVariadicNat.T(+Nat)")>]
    [<DataRow("06", """def pred T (x:*Nat) {true} def pred Caller() {dec ~a,b:*Nat; T(a,b)} ;""",
        "no matching parameter for `b:*Nat` in TestSignatureMatchingReferencesVariadicNat.T(*Nat)")>]
    [<DataRow("07", """def pred T (x:*Nat) {true} def pred Caller() {dec ~a:*Nat; T(a)} ;""",
        "")>]
    [<TestMethod>]
    member this.TestSignatureMatchingReferencesVariadicNat(no:string, varVal, var:string) =
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

    [<DataRow("00", "def cl T {intr};", "T", "ok")>]
    [<DataRow("01", "def cl T:Test {intr};", "T:Test", "ok")>]
    [<DataRow("02", "def cl T:Test1, Test3 {intr};", "T:Test1, T:Test3", "ok|ok")>]
    [<DataRow("03", "def cl T:Test1, Test2, Test3 {intr};", "T:Test1, T:Test2, T:Test3", "ok|ok|ok")>]
    [<DataRow("04", "def cl A {intr} def cl B {intr} def cl C {intr} def cl T:A,B,C,E {ctor D() {dec base.A() base.B() base.C() base.F(); } };", "T:A, T:B, T:C, T:E", "ok|ok|ok|ok")>]
    [<DataRow("05", "def cl A {intr} def cl T:A {ctor B() {dec base.A(); } };", "T:A", "ok")>]
    [<DataRow("06", "def cl A {intr} def cl T:A {ctor B() {dec base.C(); } };", "T:A", "ok")>]
    [<DataRow("07", "def cl T { ctor A() {dec base.Obj(); } };", "T", "ok")>]
    [<DataRow("08", "def cl T { ctor A() {dec base.B(); } };", "T", "ok")>]
    [<DataRow("09", "def cl T:C { ctor A() {dec base.Obj(); } };", "T:C", "ok")>]
    [<DataRow("10", "uses Fpl.SetTheory def cl T:Set {ctor Test() {dec base.Obj(); } };", "T:Set", "ok")>]
    [<DataRow("11", "uses Fpl.SetTheory def cl T:Set {ctor Test() {dec base.Set(); } };", "T:Set", "ok")>]
    [<DataRow("12", "def cl A {intr} def cl B:A {intr} def cl T:B,A {intr};", "T:B:A, T:A", "ok|cross-inheritance not supported, `A` is base for `B` and `T`.")>]
    [<DataRow("13", "uses Fpl.SetTheory def cl T:EmptySet,Set {intr};", "T:EmptySet:Set, T:Set", "ok|cross-inheritance not supported, `Set` is base for `EmptySet` and `T`.")>]
    [<DataRow("13a", "uses Fpl.SetTheory def cl T:EmptySet;", "T:EmptySet:Set", "ok")>]
    [<DataRow("13b", "def cl Set def cl EmptySet:Set def cl T:EmptySet;", "T:EmptySet:Set", "ok")>]
    [<DataRow("14", "uses Fpl.SetTheory def cl T:Set, EmptySet {intr};", "T:Set, T:EmptySet:Set", "ok|cross-inheritance not supported, `Set` is base for `T` and `EmptySet`.")>]
    [<DataRow("15", "def cl A {intr} def cl B:A {intr} def cl T:A,B {intr};", "T:A, T:B:A", "ok|cross-inheritance not supported, `A` is base for `T` and `B`.")>]
    [<DataRow("16", "def cl A {intr} def cl B:A {intr} def cl T:B {intr};", "T:B:A", "ok")>]
    [<DataRow("17", "uses Fpl.SetTheory def cl T:EmptySet {intr};", "T:EmptySet:Set", "ok")>]
    [<DataRow("18", "uses Fpl.SetTheory def cl T:Set {intr};", "T:Set", "ok")>]
    [<DataRow("19", "uses Fpl.Commons uses Fpl.SetTheory def cl T:Set {intr};", "T:Set", "ok")>]
    [<DataRow("20", "def cl A {intr} def cl B:A {intr} def cl T:A {intr};", "T:A", "ok")>]
    [<DataRow("21", "def cl A {intr} def cl B:A {intr} def cl T:A,A {intr};", "T:A", "duplicate inheritance from `A` detected.")>]
    [<DataRow("21a", "def cl A {intr} def cl B:A {intr} def cl T:A,C,A {intr};", "T:A, T:C", "duplicate inheritance from `A` detected.|ok")>]
    [<DataRow("22", "def cl A {intr} def cl B:A {intr} def cl T {intr};", "T", "ok")>]
    [<DataRow("23", "def cl A {intr} def cl B:A {intr} def cl T:D,E {intr};", "T:D, T:E", "ok|ok")>]
    [<DataRow("24", "uses Fpl.SetTheory def cl T:Set {intr};", "T:Set", "ok")>]
    [<DataRow("25", "uses Fpl.SetTheory def cl T:EmptySet {intr};", "T:EmptySet:Set", "ok")>]
    [<DataRow("26", "uses Fpl.SetTheory def cl T:Set {intr};", "T:Set", "ok")>]
    [<DataRow("27", "uses Fpl.SetTheory def cl T:EmptySet {intr};", "T:EmptySet:Set", "ok")>]
    [<DataRow("29a", "def cl A:B def cl B:A def cl T:A;", "T:A:B", "ok")>]
    [<DataRow("29b", "def cl A:B def cl B:A def cl T:B;", "T:B:A:B", "cross-inheritance not supported, `B` is base for `T` and `A`.")>]
    [<DataRow("29c", "def cl A:B def cl B:A def cl T:A,B;", "T:A:B, T:B", "ok|cross-inheritance not supported, `B` is base for `A` and `T`.")>]
    [<DataRow("29d", "def cl A:B def cl B:A def cl T:B,A;", "T:B:A:B, T:A", "cross-inheritance not supported, `B` is base for `T` and `A`.|cross-inheritance not supported, `A` is base for `B` and `T`.")>]
    [<TestMethod>]
    member this.TestBaseClassPath(no:string, varVal:string, expectedPaths:string, expectedMessages:string) =
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
            let cl = blocks |> List.filter(fun fv -> fv.FplId = "T") |> List.head
            let paths = 
                findInheritanceChains cl 
                |> Seq.map (fun kvp -> kvp.Key) 
                |> String.concat ", "
            Assert.AreEqual<string>(expectedPaths, paths)
            let messages = 
                findInheritanceChains cl 
                |> Seq.map (fun kvp -> kvp.Value) 
                |> String.concat "|"
            Assert.AreEqual<string>(expectedMessages, messages)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("00", "def func T()->obj {intr};", "T", "ok")>]
    [<DataRow("01", "def func T:Test()->obj {intr};", "T:Test", "ok")>]
    [<DataRow("02", "def func T:Test1, Test3()->obj {intr};", "T:Test1, T:Test3", "ok|ok")>]
    [<DataRow("03", "def func T:Test1, Test2, Test3()->obj {intr};", "T:Test1, T:Test2, T:Test3", "ok|ok|ok")>]
    [<DataRow("04", "def func A()->obj {intr} def func B()->obj {intr} def func C()->obj {intr} def func T:A,B,C,E()->obj ;", "T:A, T:B, T:C, T:E", "ok|ok|ok|ok")>]
    [<DataRow("05", "def func A()->obj {intr} def func T:A()->obj ;", "T:A", "ok")>]
    [<DataRow("06", "def func A()->obj {intr} def func T:B()->obj ;", "T:B", "ok")>]
    [<DataRow("07", "def func T(a:pred)->obj ;", "T", "ok")>]
    [<DataRow("08", "def func T(x,y:ind)->obj ;", "T", "ok")>]
    [<DataRow("09", "def func T:C(a,b,c:func)->pred(x,y:obj);", "T:C", "ok")>]
    [<DataRow("10", "def func A()->obj {intr} def func B:A ()->obj {intr} def func T:B,A()->obj {intr};", "T:B:A, T:A", "ok|cross-inheritance not supported, `A` is base for `B` and `T`.")>]
    [<DataRow("11", "def func Set()->obj def func EmptySet:Set()->obj def func T:EmptySet()->obj;", "T:EmptySet:Set", "ok")>]
    [<DataRow("12", "def func A()->obj {intr} def func B:A()->obj {intr} def func T:A,B()->obj {intr};", "T:A, T:B:A", "ok|cross-inheritance not supported, `A` is base for `T` and `B`.")>]
    [<DataRow("13", "def func A()->obj {intr} def func B:A()->obj {intr} def func T:B()->obj {intr};", "T:B:A", "ok")>]
    [<DataRow("14", "def func A()->obj {intr} def func B:A()->obj {intr} def func T:A()->obj {intr};", "T:A", "ok")>]
    [<DataRow("15", "def func A()->obj {intr} def func B:A()->obj {intr} def func T:A,A()->obj {intr};", "T:A", "duplicate inheritance from `A` detected.")>]
    [<DataRow("16", "def func A()->obj {intr} def func B:A()->obj {intr} def func T:A,C,A()->obj {intr};", "T:A, T:C", "duplicate inheritance from `A` detected.|ok")>]
    [<DataRow("17", "def func A()->obj {intr} def func B:A()->obj {intr} def func T()->obj {intr};", "T", "ok")>]
    [<DataRow("18", "def func A()->obj {intr} def func B:A()->obj {intr} def func T:D,E()->obj {intr};", "T:D, T:E", "ok|ok")>]
    [<DataRow("19", "def func A:B()->obj def func B:A()->obj def func T:A()->obj;", "T:A:B", "ok")>]
    [<DataRow("20a", "def func A:B()->obj def func B:A()->obj def func T:B()->obj;", "T:B:A:B", "cross-inheritance not supported, `B` is base for `T` and `A`.")>]
    [<DataRow("20b", "def func A:B()->obj def func B:A()->obj def func T:A,B()->obj;", "T:A:B, T:B", "ok|cross-inheritance not supported, `B` is base for `A` and `T`.")>]
    [<DataRow("20c", "def func A:B()->obj def func B:A()->obj def func T:B,A()->obj;", "T:B:A:B, T:A", "cross-inheritance not supported, `B` is base for `T` and `A`.|cross-inheritance not supported, `A` is base for `B` and `T`.")>]
    [<TestMethod>]
    member this.TestBaseFunctionalTermPath(no:string, varVal:string, expectedPaths:string, expectedMessages:string) =
        ad.Clear()
        let fplCode = sprintf """%s""" varVal
        let filename = "TestBaseFunctionalTermPath"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let blocks = theory.Scope.Values |> Seq.toList 
            let cl = blocks |> List.filter(fun fv -> fv.FplId = "T") |> List.head
            let paths = 
                findInheritanceChains cl 
                |> Seq.map (fun kvp -> kvp.Key) 
                |> String.concat ", "
            Assert.AreEqual<string>(expectedPaths, paths)
            let messages = 
                findInheritanceChains cl 
                |> Seq.map (fun kvp -> kvp.Value) 
                |> String.concat "|"
            Assert.AreEqual<string>(expectedMessages, messages)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("inh", """def cl A { intr } def pred T() {dec ~n:A n:=A(); true};""", "")>]
    [<DataRow("inh_a", """def cl A { intr } def pred T() {dec ~n:obj n:=A(); true};""", "")>]
    [<DataRow("inh_b", """def cl A {intr} def cl B:A { intr } def pred T() {dec ~n:A n:=B(); true};""", "")>]
    [<DataRow("inh_c", """def cl A {intr} def cl B:A { intr } def pred T() {dec ~n:B n:=A(); true};""", "x")>]
    [<DataRow("inh_d", """def cl A {intr} def cl B:A { intr } def pred T() {dec ~n:obj n:=B(); true};""", "")>]
    [<DataRow("inh_e", """def cl A {intr} def cl B:A { intr } def pred T() {dec ~n:obj n:=A(); true};""", "")>]
    [<DataRow("inh_f", """def cl A {intr} def cl B { intr } def pred T() {dec ~n:B n:=A(); true};""", "x")>]
    [<DataRow("inh_g", """def cl A {intr} def cl B { intr } def pred T() {dec ~n:A n:=B(); true};""", "x")>]
    [<DataRow("inh_type_a", """def cl A { intr } def pred T() {dec ~n:ind n:=A(); true};""", "x")>]
    [<DataRow("inh_type_b", """def cl A { intr } def pred T() {dec ~n:pred n:=A(); true};""", "x")>]
    [<DataRow("inh_type_c", """def cl A { intr } def pred T() {dec ~n:func n:=A(); true};""", "x")>]
    [<DataRow("constr_a", """def cl A { ctor A(x:obj) {dec base.Obj(); } } def pred T() {dec ~n:A n:=A(); true};""", "x")>]
    [<DataRow("constr_b", """def cl A { ctor A(x:obj) {dec base.Obj(); } } def pred T() {dec ~n:A ~x:ind n:=A(x); true};""", "x")>]
    [<DataRow("constr_c", """def cl A { ctor A(x:obj) {dec base.Obj(); } } def pred T() {dec ~n:A ~y:pred n:=A(y); true};""", "x")>]
    [<DataRow("constr_d", """def cl A { ctor A(x:obj) {dec base.Obj(); } } def pred T() {dec ~n:A ~z:tpl n:=A(z); true};""", "x")>]
    [<DataRow("constr_e", """def cl A { ctor A(x:obj) {dec base.Obj(); } } def pred T() {dec ~n:obj ~z:obj n:=A(z); true};""", "")>]
    [<DataRow("constr_inh_a", """def cl A { ctor A(x:obj) {dec base.Obj(); } } def cl B:A { ctor B(x:pred) {dec base.A(); } } def pred T() {dec ~n:A n:=B(); true};""", "x")>]
    [<DataRow("constr_inh_b", """def cl A { ctor A(x:obj) {dec base.Obj(); } } def cl B:A { ctor B(x:pred) {dec base.A(); } } def pred T() {dec ~n:obj n:=B(); true};""", "y")>]
    [<DataRow("constr_inh_c", """def cl A { ctor A(x:obj) {dec base.Obj(); } } def cl B:A { ctor B(x:pred) {dec base.A(); } } def pred T() {dec ~n:obj n:=A(); true};""", "y")>]
    [<DataRow("constr_inh_d", """def cl A { ctor A(x:obj) {dec base.Obj(); } } def cl B:A { ctor B(x:pred) {dec base.A(); } } def pred T() {dec ~n:B ~x:obj n:=A(x); true};""", "x")>]
    [<DataRow("constr_inh_e", """def cl A { ctor A(x:obj) {dec base.Obj(); } } def cl B { ctor B(x:pred) {dec base.Obj(); } } def pred T() {dec ~n:B ~x:obj n:=A(x); true};""", "x")>]
    [<DataRow("constr_inh_f", """def cl A { ctor A(x:obj) {dec base.Obj(); } } def cl B { ctor B(x:pred) {dec base.Obj(); } } def pred T() {dec ~n:A ~x:pred n:=B(x); true};""", "")>]
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
            let fvPars = stmtAssign.ArgList[0]
            let fvArgs = stmtAssign.ArgList[1]
            match matchArgumentsWithParameters fvArgs fvPars with
            | Some errMsg -> Assert.AreEqual<string>(var, errMsg)
            | None -> Assert.AreEqual<string>("no error","no error")
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("00", """def cl A { intr } def pred T() {dec ~n:A n:=A; true};""", "A")>]
    [<DataRow("01", """def cl A { ctor A(x:obj) {dec base.Obj(); } } def pred T() {dec ~n:A ~x:obj n:=A(x); true};""", "A(obj)")>]
    [<DataRow("02", """def cl A { ctor A(x:pred) {dec base.Obj(); } } def pred T() {dec ~n:A ~x:pred n:=A(x); true};""", "A(pred)")>]
    [<DataRow("03a", """def cl A { ctor A(x:obj) {dec base.Obj(); } ctor A(x:pred) {dec base.Obj(); } ctor A(x:ind) {dec base.Obj(); } } def pred T() {dec ~n:A ~x:obj n:=A(x); true};""", "A(obj)")>]
    [<DataRow("03b", """def cl A { ctor A(x:obj) {dec base.Obj(); } ctor A(x:pred) {dec base.Obj(); } ctor A(x:ind) {dec base.Obj(); } } def pred T() {dec ~n:A ~x:pred n:=A(x); true};""", "A(pred)")>]
    [<DataRow("03c", """def cl A { ctor A(x:obj) {dec base.Obj(); } ctor A(x:pred) {dec base.Obj(); } ctor A(x:ind) {dec base.Obj(); } } def pred T() {dec ~n:A ~x:ind n:=A(x); true};""", "A(ind)")>]
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
