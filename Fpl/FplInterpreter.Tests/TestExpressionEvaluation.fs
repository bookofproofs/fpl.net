namespace FplInterpreter.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open ErrDiagnostics
open FplGrammarCommons
open FplInterpreterTypes
open CommonTestHelpers

[<TestClass>]
type TestExpressionEvaluation() =
    let evalTreeFplId (fplValue: FplValue) = getType SignatureType.Name fplValue
    let evalTreeFplRepresentation (fplValue: FplValue) = getRepresentation fplValue

    [<DataRow("def pred T() { true };", literalTrue)>]
    [<DataRow("def pred T() { false };", literalFalse)>]
    [<DataRow("def pred T() { undef };", literalUndef)>]
    [<DataRow("def pred T() { intr };", literalUndetermined)>]
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

    [<DataRow("def pred T() { and(and(true,true),true) };", literalTrue)>]
    [<DataRow("def pred T() { and(and(true,false),true) };", literalFalse)>]
    [<DataRow("def pred T() { and(and(true,true),false) };", literalFalse)>]
    [<DataRow("def pred T() { and(and(false,true),true) };", literalFalse)>]
    [<DataRow("def pred T() { and(true,and(x,true)) };", literalUndetermined)>]
    [<DataRow("def pred T() { and(true,and(true,x)) };", literalUndetermined)>]
    [<DataRow("def pred T() { and(x,and(true,true)) };", literalUndetermined)>]
    [<DataRow("def pred T() { and(true,true) };", literalTrue)>]
    [<DataRow("def pred T() { and(true,false) };", literalFalse)>]
    [<DataRow("def pred T() { and(false,true) };", literalFalse)>]
    [<DataRow("def pred T() { and(false,false) };", literalFalse)>]
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

    [<DataRow("def pred T() { not true };", literalFalse)>]
    [<DataRow("def pred T() { not (true) };", literalFalse)>]
    [<DataRow("def pred T() { not ((true)) };", literalFalse)>]
    [<DataRow("def pred T() { not false };", literalTrue)>]
    [<DataRow("def pred T() { not (false) };", literalTrue)>]
    [<DataRow("def pred T() { not ((false)) };", literalTrue)>]
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

    [<DataRow("def pred T() { or(false,or(false,false)) };", literalFalse)>]
    [<DataRow("def pred T() { or(false,or(true,false)) };", literalTrue)>]
    [<DataRow("def pred T() { or(or(false,false),true) };", literalTrue)>]
    [<DataRow("def pred T() { or(or(true,false),false) };", literalTrue)>]
    [<DataRow("def pred T() { or(false,or(x,false)) };", literalUndetermined)>]
    [<DataRow("def pred T() { or(or(false,false),x) };", literalUndetermined)>]
    [<DataRow("def pred T() { or(x,or(false,false)) };", literalUndetermined)>]
    [<DataRow("def pred T() { or(true,true) };", literalTrue)>]
    [<DataRow("def pred T() { or(true,false) };", literalTrue)>]
    [<DataRow("def pred T() { or(false,true) };", literalTrue)>]
    [<DataRow("def pred T() { or(false,false) };", literalFalse)>]
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

    [<DataRow("def pred T() { xor(false,xor(false,false)) };", literalFalse)>]
    [<DataRow("def pred T() { xor(xor(false,false),true) };", literalTrue)>]
    [<DataRow("def pred T() { xor(xor(false,true),false) };", literalTrue)>]
    [<DataRow("def pred T() { xor(xor(true,false),false) };", literalTrue)>]
    [<DataRow("def pred T() { xor(false,xor(true,true)) };", literalFalse)>]
    [<DataRow("def pred T() { xor(xor(true,false),true) };", literalFalse)>]
    [<DataRow("def pred T() { xor(true,xor(true,false)) };", literalFalse)>]
    [<DataRow("def pred T() { xor(true,xor(true,true)) };", literalTrue)>]
    [<DataRow("def pred T() { xor(xor(false,x),false) };", literalUndetermined)>]
    [<DataRow("def pred T() { xor(false,xor(false,x)) };", literalUndetermined)>]
    [<DataRow("def pred T() { xor(x,xor(false,false)) };", literalUndetermined)>]
    [<DataRow("def pred T() { xor(true,true) };", literalFalse)>]
    [<DataRow("def pred T() { xor(true,false) };", literalTrue)>]
    [<DataRow("def pred T() { xor(false,true) };", literalTrue)>]
    [<DataRow("def pred T() { xor(false,false) };", literalFalse)>]
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

    [<DataRow("def pred T() { impl(true,true) };", literalTrue)>]
    [<DataRow("def pred T() { impl(true,false) };", literalFalse)>]
    [<DataRow("def pred T() { impl(false,true) };", literalTrue)>]
    [<DataRow("def pred T() { impl(false,false) };", literalTrue)>]
    [<DataRow("def pred T() { impl(x,false) };", literalUndetermined)>]
    [<DataRow("def pred T() { impl(false,x) };", literalUndetermined)>]
    [<DataRow("def pred T() { impl(x,true) };", literalUndetermined)>]
    [<DataRow("def pred T() { impl(true,x) };", literalUndetermined)>]
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

    [<DataRow("def pred T() { iif(true,true) };", literalTrue)>]
    [<DataRow("def pred T() { iif(true,false) };", literalFalse)>]
    [<DataRow("def pred T() { iif(false,true) };", literalFalse)>]
    [<DataRow("def pred T() { iif(false,false) };", literalTrue)>]
    [<DataRow("def pred T() { iif(x,false) };", literalUndetermined)>]
    [<DataRow("def pred T() { iif(false,x) };", literalUndetermined)>]
    [<DataRow("def pred T() { iif(x,true) };", literalUndetermined)>]
    [<DataRow("def pred T() { iif(true,x) };", literalUndetermined)>]
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

    [<DataRow("def pred T() { dec ~x:obj; is(x,obj) };", literalTrue)>]
    [<DataRow("def pred T() { dec ~x:Nat; is(x,Nat) };", literalTrue)>]
    [<DataRow("def pred T() { dec ~x:Nat; is(x,obj) };", literalFalse)>]
    [<DataRow("def cl Nat:obj {intr} def pred T() { dec ~x:Nat; is(x,obj) };", literalTrue)>]
    [<DataRow("def cl A:obj {intr} def cl B:A {intr} def pred T() { dec ~x:B; is(x,A) };", literalTrue)>]
    [<DataRow("def cl A:obj {intr} def cl B:A {intr} def pred T() { dec ~x:B; is(x,obj) };", literalTrue)>]
    [<DataRow("def cl A:obj {intr} def cl B:A {intr} def pred T() { dec ~x:A; is(x,obj) };", literalTrue)>]
    [<DataRow("def cl A:obj {intr} def cl B:A {intr} def pred T() { dec ~x:A; is(x,B) };", literalFalse)>]
    [<DataRow("def pred T() { dec ~x:pred(y:obj); is(x,pred(z:obj)) };", literalTrue)>]
    [<DataRow("def pred T() { dec ~x:pred(y:obj); is(x,pred) };", literalTrue)>]
    [<DataRow("def pred T() { dec ~x:func(y:obj)->obj; is(x,func) };", literalTrue)>]
    [<DataRow("def pred T() { dec ~x:func(y:obj)->ind; is(x,func(y:obj)->ind) };", literalTrue)>]
    [<DataRow("def pred T() { dec ~x:pred(y:obj); is(self,pred) };", literalTrue)>]
    [<DataRow("def pred T() { dec ~x:pred(y:func()->obj); is(x,pred) };", literalTrue)>]
    [<DataRow("def pred T() { dec ~x:pred(y:func()->obj); is(x,pred(y:func()->obj)) };", literalTrue)>]
    [<DataRow("def pred T() { dec ~x:pred(y:func()->obj); is(x,pred(y:func()->ind)) };", literalFalse)>]
    [<DataRow("def pred T() { dec ~x:pred(y:func()->obj); is(x,pred(y:func(z:obj)->obj)) };", literalFalse)>]
    [<DataRow("def pred T() { dec ~x:pred(y:func(z:obj)->obj); is(x,pred(y:func(z:obj)->Nat)) };", literalFalse)>]
    [<DataRow("def pred T() { dec ~x:pred(y:func(z:obj)->Nat); is(x,pred(y:func(z:obj)->Nat)) };", literalTrue)>]
    [<DataRow("def pred T() { dec ~x:pred(y:func(z:obj)->Nat); is(x,pred(a:func(b:obj)->Nat)) };", literalTrue)>]
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

    [<DataRow("def pred T() { dec ~x:pred(y:obj); is(self,pred) };", literalPred)>]
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