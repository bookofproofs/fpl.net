namespace FplInterpreter.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open ErrDiagnostics
open FplInterpreterTypes
open CommonTestHelpers
open System.Text

[<TestClass>]
type TestExpressionEvaluation() =
    let evalTreeFplId (fplValue: FplValue) = fplValue.Type(SignatureType.Name)
    let evalTreeFplRepresentation (fplValue: FplValue) = fplValue.Type(SignatureType.Repr)

    [<DataRow("def pred T() { true };", "pred{true}")>]
    [<DataRow("def pred T() { false };", "pred{false}")>]
    [<DataRow("def pred T() { undef };", "undef")>]
    [<DataRow("def pred T() { intr };", "pred{undetermined}")>]
    [<TestMethod>]
    member this.TestExpressionEvaluationConstants(fplCode, expected: string) =
        ad.Clear()
        let filename = "TestExpressionEvaluationConstants"
        let stOption = prepareFplCode (filename + ".fpl", fplCode, false)
        prepareFplCode (filename, "", false) |> ignore

        match stOption with
        | Some st ->
            let r = st.Root
            let theory = r.Scope[filename]

            let pr1 = theory.Scope["T()"]
            let expr = 
                if pr1.ValueList.Count > 0 then
                    pr1.ValueList[0]
                else 
                    pr1
            let actual = evalTreeFplRepresentation(expr)
            printfn "expected: %s" expected 
            printfn "actual  : %s" actual
            printfn "%s" (expr.Type(SignatureType.Mixed))
            printfn "%s" (evalTreeFplId(expr))
            Assert.AreEqual<string>(expected, actual)
        | None -> Assert.IsTrue(false)

    [<DataRow("def pred T() { and(and(true,true),true) };", "pred{true}")>]
    [<DataRow("def pred T() { and(and(true,false),true) };", "pred{false}")>]
    [<DataRow("def pred T() { and(and(true,true),false) };", "pred{false}")>]
    [<DataRow("def pred T() { and(and(false,true),true) };", "pred{false}")>]
    [<DataRow("def pred T() { and(true,and(x,true)) };", "pred{undetermined}")>]
    [<DataRow("def pred T() { and(true,and(true,x)) };", "pred{undetermined}")>]
    [<DataRow("def pred T() { and(x,and(true,true)) };", "pred{undetermined}")>]
    [<TestMethod>]
    member this.TestExpressionEvaluationConjunction(fplCode, expected: string) =
        ad.Clear()
        let filename = "TestExpressionEvaluationConjunction"
        let stOption = prepareFplCode (filename + ".fpl", fplCode, false)
        prepareFplCode (filename, "", false) |> ignore

        match stOption with
        | Some st ->
            let r = st.Root
            let theory = r.Scope[filename]

            let pr1 = theory.Scope["T()"]
            let expr = 
                if pr1.ValueList.Count > 0 then
                    pr1.ValueList[0]
                else 
                    pr1
            let actual = evalTreeFplRepresentation(expr)
            printfn "expected: %s" expected 
            printfn "actual  : %s" actual
            printfn "%s" (expr.Type(SignatureType.Mixed))
            printfn "%s" (evalTreeFplId(expr))
            Assert.AreEqual<string>(expected, actual)
        | None -> Assert.IsTrue(false)

    [<DataRow("def pred T() { or(false,or(false,false)) };", "pred{false}")>]
    [<DataRow("def pred T() { or(false,or(true,false)) };", "pred{true}")>]
    [<DataRow("def pred T() { or(or(false,false),true) };", "pred{true}")>]
    [<DataRow("def pred T() { or(or(true,false),false) };", "pred{true}")>]
    [<DataRow("def pred T() { or(false,or(x,false)) };", "pred{undetermined}")>]
    [<DataRow("def pred T() { or(or(false,false),x) };", "pred{undetermined}")>]
    [<DataRow("def pred T() { or(x,or(false,false)) };", "pred{undetermined}")>]
    [<TestMethod>]
    member this.TestExpressionEvaluationDisjunction(fplCode, expected: string) =
        ad.Clear()
        let filename = "TestExpressionEvaluationDisjunction"
        let stOption = prepareFplCode (filename + ".fpl", fplCode, false)
        prepareFplCode (filename, "", false) |> ignore

        match stOption with
        | Some st ->
            let r = st.Root
            let theory = r.Scope[filename]

            let pr1 = theory.Scope["T()"]
            let expr = 
                if pr1.ValueList.Count > 0 then
                    pr1.ValueList[0]
                else 
                    pr1
            let actual = evalTreeFplRepresentation(expr)
            printfn "expected: %s" expected 
            printfn "actual  : %s" actual
            printfn "%s" (expr.Type(SignatureType.Mixed))
            printfn "%s" (evalTreeFplId(expr))
            Assert.AreEqual<string>(expected, actual)
        | None -> Assert.IsTrue(false)

    [<DataRow("def pred T() { xor(false,xor(false,false)) };", "pred{false}")>]
    [<DataRow("def pred T() { xor(xor(false,false),true) };", "pred{true}")>]
    [<DataRow("def pred T() { xor(xor(false,true),false) };", "pred{true}")>]
    [<DataRow("def pred T() { xor(xor(true,false),false) };", "pred{true}")>]
    [<DataRow("def pred T() { xor(false,xor(true,true)) };", "pred{false}")>]
    [<DataRow("def pred T() { xor(xor(true,false),true) };", "pred{false}")>]
    [<DataRow("def pred T() { xor(true,xor(true,false)) };", "pred{false}")>]
    [<DataRow("def pred T() { xor(true,xor(true,true)) };", "pred{true}")>]
    [<DataRow("def pred T() { xor(xor(false,x),false) };", "pred{undetermined}")>]
    [<DataRow("def pred T() { xor(false,xor(false,x)) };", "pred{undetermined}")>]
    [<DataRow("def pred T() { xor(x,xor(false,false)) };", "pred{undetermined}")>]
    [<TestMethod>]
    member this.TestExpressionEvaluationExclusiveOr(fplCode, expected: string) =
        ad.Clear()
        let filename = "TestExpressionEvaluationExclusiveOr"
        let stOption = prepareFplCode (filename + ".fpl", fplCode, false)
        prepareFplCode (filename, "", false) |> ignore

        match stOption with
        | Some st ->
            let r = st.Root
            let theory = r.Scope[filename]

            let pr1 = theory.Scope["T()"]
            let expr = 
                if pr1.ValueList.Count > 0 then
                    pr1.ValueList[0]
                else 
                    pr1
            let actual = evalTreeFplRepresentation(expr)
            printfn "expected: %s" expected 
            printfn "actual  : %s" actual
            printfn "%s" (expr.Type(SignatureType.Mixed))
            printfn "%s" (evalTreeFplId(expr))
            Assert.AreEqual<string>(expected, actual)
        | None -> Assert.IsTrue(false)

    [<DataRow("def pred T() { impl(true,true) };", "pred{true}")>]
    [<DataRow("def pred T() { impl(true,false) };", "pred{false}")>]
    [<DataRow("def pred T() { impl(false,true) };", "pred{true}")>]
    [<DataRow("def pred T() { impl(false,false) };", "pred{true}")>]
    [<DataRow("def pred T() { impl(x,false) };", "pred{undetermined}")>]
    [<DataRow("def pred T() { impl(false,x) };", "pred{undetermined}")>]
    [<DataRow("def pred T() { impl(x,true) };", "pred{undetermined}")>]
    [<DataRow("def pred T() { impl(true,x) };", "pred{undetermined}")>]
    [<TestMethod>]
    member this.TestExpressionEvaluationImplication(fplCode, expected: string) =
        ad.Clear()
        let filename = "TestExpressionEvaluationImplication"
        let stOption = prepareFplCode (filename + ".fpl", fplCode, false)
        prepareFplCode (filename, "", false) |> ignore

        match stOption with
        | Some st ->
            let r = st.Root
            let theory = r.Scope[filename]

            let pr1 = theory.Scope["T()"]
            let expr = 
                if pr1.ValueList.Count > 0 then
                    pr1.ValueList[0]
                else 
                    pr1
            let actual = evalTreeFplRepresentation(expr)
            printfn "expected: %s" expected 
            printfn "actual  : %s" actual
            printfn "%s" (expr.Type(SignatureType.Mixed))
            printfn "%s" (evalTreeFplId(expr))
            Assert.AreEqual<string>(expected, actual)
        | None -> Assert.IsTrue(false)

    [<DataRow("def pred T() { iif(true,true) };", "pred{true}")>]
    [<DataRow("def pred T() { iif(true,false) };", "pred{false}")>]
    [<DataRow("def pred T() { iif(false,true) };", "pred{false}")>]
    [<DataRow("def pred T() { iif(false,false) };", "pred{true}")>]
    [<DataRow("def pred T() { iif(x,false) };", "pred{undetermined}")>]
    [<DataRow("def pred T() { iif(false,x) };", "pred{undetermined}")>]
    [<DataRow("def pred T() { iif(x,true) };", "pred{undetermined}")>]
    [<DataRow("def pred T() { iif(true,x) };", "pred{undetermined}")>]
    [<TestMethod>]
    member this.TestExpressionEvaluationEquivalence(fplCode, expected: string) =
        ad.Clear()
        let filename = "TestExpressionEvaluationEquivalence"
        let stOption = prepareFplCode (filename + ".fpl", fplCode, false)
        prepareFplCode (filename, "", false) |> ignore

        match stOption with
        | Some st ->
            let r = st.Root
            let theory = r.Scope[filename]

            let pr1 = theory.Scope["T()"]
            let expr = 
                if pr1.ValueList.Count > 0 then
                    pr1.ValueList[0]
                else 
                    pr1
            let actual = evalTreeFplRepresentation(expr)
            printfn "expected: %s" expected 
            printfn "actual  : %s" actual
            printfn "%s" (expr.Type(SignatureType.Mixed))
            printfn "%s" (evalTreeFplId(expr))
            Assert.AreEqual<string>(expected, actual)
        | None -> Assert.IsTrue(false)

    [<DataRow("def pred T() { dec ~x:obj; is(x,obj) };", "pred{true}")>]
    [<DataRow("def pred T() { dec ~x:Nat; is(x,Nat) };", "pred{true}")>]
    [<DataRow("def pred T() { dec ~x:Nat; is(x,obj) };", "pred{false}")>]
    [<DataRow("def cl Nat:obj {intr} def pred T() { dec ~x:Nat; is(x,obj) };", "pred{true}")>]
    [<DataRow("def cl A:obj {intr} def cl B:A {intr} def pred T() { dec ~x:B; is(x,A) };", "pred{true}")>]
    [<DataRow("def cl A:obj {intr} def cl B:A {intr} def pred T() { dec ~x:B; is(x,obj) };", "pred{true}")>]
    [<DataRow("def cl A:obj {intr} def cl B:A {intr} def pred T() { dec ~x:A; is(x,obj) };", "pred{true}")>]
    [<DataRow("def cl A:obj {intr} def cl B:A {intr} def pred T() { dec ~x:A; is(x,B) };", "pred{false}")>]
    [<DataRow("def pred T() { dec ~x:pred(y:obj); is(x,pred(z:obj)) };", "pred{true}")>]
    [<DataRow("def pred T() { dec ~x:pred(y:obj); is(x,pred) };", "pred{true}")>]
    [<DataRow("def pred T() { dec ~x:func(y:obj)->obj; is(x,func) };", "pred{true}")>]
    [<DataRow("def pred T() { dec ~x:func(y:obj)->ind; is(x,func(y:obj)->ind) };", "pred{true}")>]
    [<DataRow("def pred T() { dec ~x:pred(y:obj); is(self,pred) };", "pred{true}")>]
    [<DataRow("def pred T() { dec ~x:pred(y:func()->obj); is(x,pred) };", "pred{true}")>]
    [<DataRow("def pred T() { dec ~x:pred(y:func()->obj); is(x,pred(y:func()->obj)) };", "pred{true}")>]
    [<DataRow("def pred T() { dec ~x:pred(y:func()->obj); is(x,pred(y:func()->ind)) };", "pred{false}")>]
    [<DataRow("def pred T() { dec ~x:pred(y:func()->obj); is(x,pred(y:func(z:obj)->obj)) };", "pred{false}")>]
    [<DataRow("def pred T() { dec ~x:pred(y:func(z:obj)->obj); is(x,pred(y:func(z:obj)->Nat)) };", "pred{false}")>]
    [<DataRow("def pred T() { dec ~x:pred(y:func(z:obj)->Nat); is(x,pred(y:func(z:obj)->Nat)) };", "pred{true}")>]
    [<DataRow("def pred T() { dec ~x:pred(y:func(z:obj)->Nat); is(x,pred(a:func(b:obj)->Nat)) };", "pred{true}")>]
    [<TestMethod>]
    member this.TestExpressionEvaluationIsOperand(fplCode, expected: string) =
        ad.Clear()
        let filename = "TestExpressionEvaluationIsOperand"
        let stOption = prepareFplCode (filename + ".fpl", fplCode, false)
        prepareFplCode (filename, "", false) |> ignore

        match stOption with
        | Some st ->
            let r = st.Root
            let theory = r.Scope[filename]
            let pr1 = theory.Scope["T()"]
            Assert.AreEqual<string>(expected, pr1.ReprId)
        | None -> Assert.IsTrue(false)

    [<DataRow("def pred T() { dec ~x:pred(y:obj); is(self,pred) };", "pred")>]
    [<DataRow("def pred T() { dec ~x:pred(y:func(z:obj)->obj); is(x,pred(y:func(z:obj)->Nat)) };", "pred(func(obj) -> Nat)")>]
    [<DataRow("def pred T() { dec ~x:pred(y:func()->obj); is(x,pred(y:func(z:obj)->obj)) };", "pred(func(obj) -> obj)")>]
    [<DataRow("def pred T() { dec ~x:pred(y:func()->obj); is(x,pred(y:func()->ind)) };", "pred(func() -> ind)")>]
    [<TestMethod>]
    member this.TestExpressionEvaluationIsOperandRepr(fplCode, expected: string) =
        ad.Clear()
        let filename = "TestExpressionEvaluationIsOperand"
        let stOption = prepareFplCode (filename + ".fpl", fplCode, false)
        prepareFplCode (filename, "", false) |> ignore

        match stOption with
        | Some st ->
            let r = st.Root
            let theory = r.Scope[filename]
            let pr1 = theory.Scope["T()"]
            let isOperand = pr1.ValueList[0]
            let mapping = isOperand.ValueList[1]
            Assert.AreEqual<string>(expected, mapping.Type(SignatureType.Type))
        | None -> Assert.IsTrue(false)