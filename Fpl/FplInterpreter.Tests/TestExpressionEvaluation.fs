namespace FplInterpreter.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open ErrDiagnostics
open FplInterpreterTypes
open CommonTestHelpers

[<TestClass>]
type TestExpressionEvaluation() =
    let evalTreeFplId (fplValue: FplValue) = getType SignatureType.Name fplValue
    let evalTreeFplRepresentation (fplValue: FplValue) = getRepresentation fplValue

    [<DataRow("def pred T() { true };", "true")>]
    [<DataRow("def pred T() { false };", "false")>]
    [<DataRow("def pred T() { undef };", "undef")>]
    [<DataRow("def pred T() { intr };", "undetermined")>]
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
            let actual = evalTreeFplRepresentation(pr1)
            printfn "expected: %s" expected 
            printfn "actual  : %s" actual
            printfn "%s" (getType SignatureType.Mixed pr1)
            printfn "%s" (evalTreeFplId(pr1))
            Assert.AreEqual<string>(expected, actual)
        | None -> Assert.IsTrue(false)

    [<DataRow("def pred T() { and(and(true,true),true) };", "true")>]
    [<DataRow("def pred T() { and(and(true,false),true) };", "false")>]
    [<DataRow("def pred T() { and(and(true,true),false) };", "false")>]
    [<DataRow("def pred T() { and(and(false,true),true) };", "false")>]
    [<DataRow("def pred T() { and(true,and(x,true)) };", "undetermined")>]
    [<DataRow("def pred T() { and(true,and(true,x)) };", "undetermined")>]
    [<DataRow("def pred T() { and(x,and(true,true)) };", "undetermined")>]
    [<DataRow("def pred T() { and(true,true) };", "true")>]
    [<DataRow("def pred T() { and(true,false) };", "false")>]
    [<DataRow("def pred T() { and(false,true) };", "false")>]
    [<DataRow("def pred T() { and(false,false) };", "false")>]
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
            let actual = evalTreeFplRepresentation(pr1)
            printfn "expected: %s" expected 
            printfn "actual  : %s" actual
            printfn "%s" (getType SignatureType.Mixed pr1)
            printfn "%s" (evalTreeFplId(pr1))
            Assert.AreEqual<string>(expected, actual)
        | None -> Assert.IsTrue(false)

    [<DataRow("def pred T() { not true };", "false")>]
    [<DataRow("def pred T() { not (true) };", "false")>]
    [<DataRow("def pred T() { not ((true)) };", "false")>]
    [<DataRow("def pred T() { not false };", "true")>]
    [<DataRow("def pred T() { not (false) };", "true")>]
    [<DataRow("def pred T() { not ((false)) };", "true")>]
    [<TestMethod>]
    member this.TestExpressionEvaluationNegation(fplCode, expected: string) =
        ad.Clear()
        let filename = "TestExpressionEvaluationNegation"
        let stOption = prepareFplCode (filename + ".fpl", fplCode, false)
        prepareFplCode (filename, "", false) |> ignore

        match stOption with
        | Some st ->
            let r = st.Root
            let theory = r.Scope[filename]

            let pr1 = theory.Scope["T()"]
            let actual = evalTreeFplRepresentation(pr1)
            printfn "expected: %s" expected 
            printfn "actual  : %s" actual
            printfn "%s" (getType SignatureType.Mixed pr1)
            printfn "%s" (evalTreeFplId(pr1))
            Assert.AreEqual<string>(expected, actual)
        | None -> Assert.IsTrue(false)

    [<DataRow("def pred T() { or(false,or(false,false)) };", "false")>]
    [<DataRow("def pred T() { or(false,or(true,false)) };", "true")>]
    [<DataRow("def pred T() { or(or(false,false),true) };", "true")>]
    [<DataRow("def pred T() { or(or(true,false),false) };", "true")>]
    [<DataRow("def pred T() { or(false,or(x,false)) };", "undetermined")>]
    [<DataRow("def pred T() { or(or(false,false),x) };", "undetermined")>]
    [<DataRow("def pred T() { or(x,or(false,false)) };", "undetermined")>]
    [<DataRow("def pred T() { or(true,true) };", "true")>]
    [<DataRow("def pred T() { or(true,false) };", "true")>]
    [<DataRow("def pred T() { or(false,true) };", "true")>]
    [<DataRow("def pred T() { or(false,false) };", "false")>]
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
            let actual = evalTreeFplRepresentation(pr1)
            printfn "expected: %s" expected 
            printfn "actual  : %s" actual
            printfn "%s" (getType SignatureType.Mixed pr1)
            printfn "%s" (evalTreeFplId(pr1))
            Assert.AreEqual<string>(expected, actual)
        | None -> Assert.IsTrue(false)

    [<DataRow("def pred T() { xor(false,xor(false,false)) };", "false")>]
    [<DataRow("def pred T() { xor(xor(false,false),true) };", "true")>]
    [<DataRow("def pred T() { xor(xor(false,true),false) };", "true")>]
    [<DataRow("def pred T() { xor(xor(true,false),false) };", "true")>]
    [<DataRow("def pred T() { xor(false,xor(true,true)) };", "false")>]
    [<DataRow("def pred T() { xor(xor(true,false),true) };", "false")>]
    [<DataRow("def pred T() { xor(true,xor(true,false)) };", "false")>]
    [<DataRow("def pred T() { xor(true,xor(true,true)) };", "true")>]
    [<DataRow("def pred T() { xor(xor(false,x),false) };", "undetermined")>]
    [<DataRow("def pred T() { xor(false,xor(false,x)) };", "undetermined")>]
    [<DataRow("def pred T() { xor(x,xor(false,false)) };", "undetermined")>]
    [<DataRow("def pred T() { xor(true,true) };", "false")>]
    [<DataRow("def pred T() { xor(true,false) };", "true")>]
    [<DataRow("def pred T() { xor(false,true) };", "true")>]
    [<DataRow("def pred T() { xor(false,false) };", "false")>]
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
            let actual = evalTreeFplRepresentation(pr1)
            printfn "expected: %s" expected 
            printfn "actual  : %s" actual
            printfn "%s" (getType SignatureType.Mixed pr1)
            printfn "%s" (evalTreeFplId(pr1))
            Assert.AreEqual<string>(expected, actual)
        | None -> Assert.IsTrue(false)

    [<DataRow("def pred T() { impl(true,true) };", "true")>]
    [<DataRow("def pred T() { impl(true,false) };", "false")>]
    [<DataRow("def pred T() { impl(false,true) };", "true")>]
    [<DataRow("def pred T() { impl(false,false) };", "true")>]
    [<DataRow("def pred T() { impl(x,false) };", "undetermined")>]
    [<DataRow("def pred T() { impl(false,x) };", "undetermined")>]
    [<DataRow("def pred T() { impl(x,true) };", "undetermined")>]
    [<DataRow("def pred T() { impl(true,x) };", "undetermined")>]
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
            let actual = evalTreeFplRepresentation(pr1)
            printfn "expected: %s" expected 
            printfn "actual  : %s" actual
            printfn "%s" (getType SignatureType.Mixed pr1)
            printfn "%s" (evalTreeFplId(pr1))
            Assert.AreEqual<string>(expected, actual)
        | None -> Assert.IsTrue(false)

    [<DataRow("def pred T() { iif(true,true) };", "true")>]
    [<DataRow("def pred T() { iif(true,false) };", "false")>]
    [<DataRow("def pred T() { iif(false,true) };", "false")>]
    [<DataRow("def pred T() { iif(false,false) };", "true")>]
    [<DataRow("def pred T() { iif(x,false) };", "undetermined")>]
    [<DataRow("def pred T() { iif(false,x) };", "undetermined")>]
    [<DataRow("def pred T() { iif(x,true) };", "undetermined")>]
    [<DataRow("def pred T() { iif(true,x) };", "undetermined")>]
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
            let actual = evalTreeFplRepresentation(pr1)
            printfn "expected: %s" expected 
            printfn "actual  : %s" actual
            printfn "%s" (getType SignatureType.Mixed pr1)
            printfn "%s" (evalTreeFplId(pr1))
            Assert.AreEqual<string>(expected, actual)
        | None -> Assert.IsTrue(false)

    [<DataRow("def pred T() { dec ~x:obj; is(x,obj) };", "true")>]
    [<DataRow("def pred T() { dec ~x:Nat; is(x,Nat) };", "true")>]
    [<DataRow("def pred T() { dec ~x:Nat; is(x,obj) };", "false")>]
    [<DataRow("def cl Nat:obj {intr} def pred T() { dec ~x:Nat; is(x,obj) };", "true")>]
    [<DataRow("def cl A:obj {intr} def cl B:A {intr} def pred T() { dec ~x:B; is(x,A) };", "true")>]
    [<DataRow("def cl A:obj {intr} def cl B:A {intr} def pred T() { dec ~x:B; is(x,obj) };", "true")>]
    [<DataRow("def cl A:obj {intr} def cl B:A {intr} def pred T() { dec ~x:A; is(x,obj) };", "true")>]
    [<DataRow("def cl A:obj {intr} def cl B:A {intr} def pred T() { dec ~x:A; is(x,B) };", "false")>]
    [<DataRow("def pred T() { dec ~x:pred(y:obj); is(x,pred(z:obj)) };", "true")>]
    [<DataRow("def pred T() { dec ~x:pred(y:obj); is(x,pred) };", "true")>]
    [<DataRow("def pred T() { dec ~x:func(y:obj)->obj; is(x,func) };", "true")>]
    [<DataRow("def pred T() { dec ~x:func(y:obj)->ind; is(x,func(y:obj)->ind) };", "true")>]
    [<DataRow("def pred T() { dec ~x:pred(y:obj); is(self,pred) };", "true")>]
    [<DataRow("def pred T() { dec ~x:pred(y:func()->obj); is(x,pred) };", "true")>]
    [<DataRow("def pred T() { dec ~x:pred(y:func()->obj); is(x,pred(y:func()->obj)) };", "true")>]
    [<DataRow("def pred T() { dec ~x:pred(y:func()->obj); is(x,pred(y:func()->ind)) };", "false")>]
    [<DataRow("def pred T() { dec ~x:pred(y:func()->obj); is(x,pred(y:func(z:obj)->obj)) };", "false")>]
    [<DataRow("def pred T() { dec ~x:pred(y:func(z:obj)->obj); is(x,pred(y:func(z:obj)->Nat)) };", "false")>]
    [<DataRow("def pred T() { dec ~x:pred(y:func(z:obj)->Nat); is(x,pred(y:func(z:obj)->Nat)) };", "true")>]
    [<DataRow("def pred T() { dec ~x:pred(y:func(z:obj)->Nat); is(x,pred(a:func(b:obj)->Nat)) };", "true")>]
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
            let actual = evalTreeFplRepresentation(pr1)
            Assert.AreEqual<string>(expected, actual)
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
            let isOperand = pr1.ArgList[0]
            let mapping = isOperand.ArgList[1]
            Assert.AreEqual<string>(expected, getType SignatureType.Type mapping)
        | None -> Assert.IsTrue(false)