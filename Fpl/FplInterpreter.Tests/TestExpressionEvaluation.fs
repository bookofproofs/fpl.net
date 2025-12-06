namespace FplInterpreter.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open ErrDiagnostics
open FplPrimitives
open FplInterpreterTypes
open CommonTestHelpers

[<TestClass>]
type TestExpressionEvaluation() =
    let evalTreeFplId (fplValue: FplValue) = fplValue.Type(SignatureType.Name)
    let evalTreeFplRepresentation (fplValue: FplValue) = fplValue.Represent()

    [<DataRow("def pred T() { true };", LiteralTrue)>]
    [<DataRow("def pred T() { false };", LiteralFalse)>]
    [<DataRow("def pred T() { undef };", LiteralUndef)>]
    [<DataRow("def pred T() { intr };", PrimUndetermined)>]
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
            let variableStack = new FplVariableStack()
            pr1.Run variableStack
            let actual = evalTreeFplRepresentation(pr1)
            printfn "expected: %s" expected 
            printfn "actual  : %s" actual
            printfn "%s" (pr1.Type(SignatureType.Mixed))
            printfn "%s" (evalTreeFplId(pr1))
            Assert.AreEqual<string>(expected, actual)
        | None -> Assert.IsTrue(false)

    [<DataRow("def pred T() { and(and(true,true),true) };", LiteralTrue)>]
    [<DataRow("def pred T() { and(and(true,false),true) };", LiteralFalse)>]
    [<DataRow("def pred T() { and(and(true,true),false) };", LiteralFalse)>]
    [<DataRow("def pred T() { and(and(false,true),true) };", LiteralFalse)>]
    [<DataRow("def pred T() { and(true,and(x,true)) };", PrimUndetermined)>]
    [<DataRow("def pred T() { and(true,and(true,x)) };", PrimUndetermined)>]
    [<DataRow("def pred T() { and(x,and(true,true)) };", PrimUndetermined)>]
    [<DataRow("def pred T() { and(true,true) };", LiteralTrue)>]
    [<DataRow("def pred T() { and(true,false) };", LiteralFalse)>]
    [<DataRow("def pred T() { and(false,true) };", LiteralFalse)>]
    [<DataRow("def pred T() { and(false,false) };", LiteralFalse)>]
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
            let variableStack = new FplVariableStack()
            pr1.Run variableStack
            let actual = evalTreeFplRepresentation(pr1)
            printfn "expected: %s" expected 
            printfn "actual  : %s" actual
            printfn "%s" (pr1.Type(SignatureType.Mixed))
            printfn "%s" (evalTreeFplId(pr1))
            Assert.AreEqual<string>(expected, actual)
        | None -> Assert.IsTrue(false)

    [<DataRow("def pred T() { not true };", LiteralFalse)>]
    [<DataRow("def pred T() { not (true) };", LiteralFalse)>]
    [<DataRow("def pred T() { not ((true)) };", LiteralFalse)>]
    [<DataRow("def pred T() { not false };", LiteralTrue)>]
    [<DataRow("def pred T() { not (false) };", LiteralTrue)>]
    [<DataRow("def pred T() { not ((false)) };", LiteralTrue)>]
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
            let variableStack = new FplVariableStack()
            pr1.Run variableStack

            let actual = evalTreeFplRepresentation(pr1)
            printfn "expected: %s" expected 
            printfn "actual  : %s" actual
            printfn "%s" (pr1.Type(SignatureType.Mixed))
            printfn "%s" (evalTreeFplId(pr1))
            Assert.AreEqual<string>(expected, actual)
        | None -> Assert.IsTrue(false)

    [<DataRow("def pred T() { or(false,or(false,false)) };", LiteralFalse)>]
    [<DataRow("def pred T() { or(false,or(true,false)) };", LiteralTrue)>]
    [<DataRow("def pred T() { or(or(false,false),true) };", LiteralTrue)>]
    [<DataRow("def pred T() { or(or(true,false),false) };", LiteralTrue)>]
    [<DataRow("def pred T() { or(false,or(x,false)) };", PrimUndetermined)>]
    [<DataRow("def pred T() { or(or(false,false),x) };", PrimUndetermined)>]
    [<DataRow("def pred T() { or(x,or(false,false)) };", PrimUndetermined)>]
    [<DataRow("def pred T() { or(true,true) };", LiteralTrue)>]
    [<DataRow("def pred T() { or(true,false) };", LiteralTrue)>]
    [<DataRow("def pred T() { or(false,true) };", LiteralTrue)>]
    [<DataRow("def pred T() { or(false,false) };", LiteralFalse)>]
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
            let variableStack = new FplVariableStack()
            pr1.Run variableStack
            let actual = evalTreeFplRepresentation(pr1)
            printfn "expected: %s" expected 
            printfn "actual  : %s" actual
            printfn "%s" (pr1.Type(SignatureType.Mixed))
            printfn "%s" (evalTreeFplId(pr1))
            Assert.AreEqual<string>(expected, actual)
        | None -> Assert.IsTrue(false)

    [<DataRow("def pred T() { xor(false,xor(false,false)) };", LiteralFalse)>]
    [<DataRow("def pred T() { xor(xor(false,false),true) };", LiteralTrue)>]
    [<DataRow("def pred T() { xor(xor(false,true),false) };", LiteralTrue)>]
    [<DataRow("def pred T() { xor(xor(true,false),false) };", LiteralTrue)>]
    [<DataRow("def pred T() { xor(false,xor(true,true)) };", LiteralFalse)>]
    [<DataRow("def pred T() { xor(xor(true,false),true) };", LiteralFalse)>]
    [<DataRow("def pred T() { xor(true,xor(true,false)) };", LiteralFalse)>]
    [<DataRow("def pred T() { xor(true,xor(true,true)) };", LiteralTrue)>]
    [<DataRow("def pred T() { xor(xor(false,x),false) };", PrimUndetermined)>]
    [<DataRow("def pred T() { xor(false,xor(false,x)) };", PrimUndetermined)>]
    [<DataRow("def pred T() { xor(x,xor(false,false)) };", PrimUndetermined)>]
    [<DataRow("def pred T() { xor(true,true) };", LiteralFalse)>]
    [<DataRow("def pred T() { xor(true,false) };", LiteralTrue)>]
    [<DataRow("def pred T() { xor(false,true) };", LiteralTrue)>]
    [<DataRow("def pred T() { xor(false,false) };", LiteralFalse)>]
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
            let variableStack = new FplVariableStack()
            pr1.Run variableStack

            let actual = evalTreeFplRepresentation(pr1)
            printfn "expected: %s" expected 
            printfn "actual  : %s" actual
            printfn "%s" (pr1.Type(SignatureType.Mixed))
            printfn "%s" (evalTreeFplId(pr1))
            Assert.AreEqual<string>(expected, actual)
        | None -> Assert.IsTrue(false)

    [<DataRow("def pred T() { impl(true,true) };", LiteralTrue)>]
    [<DataRow("def pred T() { impl(true,false) };", LiteralFalse)>]
    [<DataRow("def pred T() { impl(false,true) };", LiteralTrue)>]
    [<DataRow("def pred T() { impl(false,false) };", LiteralTrue)>]
    [<DataRow("def pred T() { impl(x,false) };", PrimUndetermined)>]
    [<DataRow("def pred T() { impl(false,x) };", PrimUndetermined)>]
    [<DataRow("def pred T() { impl(x,true) };", PrimUndetermined)>]
    [<DataRow("def pred T() { impl(true,x) };", PrimUndetermined)>]
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
            let variableStack = new FplVariableStack()
            pr1.Run variableStack

            let actual = evalTreeFplRepresentation(pr1)
            printfn "expected: %s" expected 
            printfn "actual  : %s" actual
            printfn "%s" (pr1.Type(SignatureType.Mixed))
            printfn "%s" (evalTreeFplId(pr1))
            Assert.AreEqual<string>(expected, actual)
        | None -> Assert.IsTrue(false)

    [<DataRow("def pred T() { iif(true,true) };", LiteralTrue)>]
    [<DataRow("def pred T() { iif(true,false) };", LiteralFalse)>]
    [<DataRow("def pred T() { iif(false,true) };", LiteralFalse)>]
    [<DataRow("def pred T() { iif(false,false) };", LiteralTrue)>]
    [<DataRow("def pred T() { iif(x,false) };", PrimUndetermined)>]
    [<DataRow("def pred T() { iif(false,x) };", PrimUndetermined)>]
    [<DataRow("def pred T() { iif(x,true) };", PrimUndetermined)>]
    [<DataRow("def pred T() { iif(true,x) };", PrimUndetermined)>]
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
            let variableStack = new FplVariableStack()
            pr1.Run variableStack

            let actual = evalTreeFplRepresentation(pr1)
            printfn "expected: %s" expected 
            printfn "actual  : %s" actual
            printfn "%s" (pr1.Type(SignatureType.Mixed))
            printfn "%s" (evalTreeFplId(pr1))
            Assert.AreEqual<string>(expected, actual)
        | None -> Assert.IsTrue(false)

    [<DataRow("00", "def pred T() { dec ~x:obj; is(x,obj) };", LiteralTrue)>]
    [<DataRow("01", "def pred T() { dec ~x:Nat; is(x,Nat) };", LiteralTrue)>]
    [<DataRow("02", "def pred T() { dec ~x:Nat; is(x,obj) };", LiteralFalse)>]
    [<DataRow("03", "def cl Nat {intr} def pred T() { dec ~x:Nat; is(x,obj) };", LiteralTrue)>]
    [<DataRow("04", "def cl A {intr} def cl B:A {intr} def pred T() { dec ~x:B; is(x,A) };", LiteralTrue)>]
    [<DataRow("05", "def cl A {intr} def cl B:A {intr} def pred T() { dec ~x:B; is(x,obj) };", LiteralTrue)>]
    [<DataRow("06", "def cl A {intr} def cl B:A {intr} def pred T() { dec ~x:A; is(x,obj) };", LiteralTrue)>]
    [<DataRow("07", "def cl A {intr} def cl B:A {intr} def pred T() { dec ~x:A; is(x,B) };", LiteralFalse)>]
    [<DataRow("08", "def pred T() { dec ~x:pred(y:obj); is(x,pred(z:obj)) };", LiteralTrue)>]
    [<DataRow("09", "def pred T() { dec ~x:pred(y:obj); is(x,pred) };", LiteralTrue)>]
    [<DataRow("10", "def pred T() { dec ~x:func(y:obj)->obj; is(x,func) };", LiteralTrue)>]
    [<DataRow("11", "def pred T() { dec ~x:func(y:obj)->ind; is(x,func(y:obj)->ind) };", LiteralTrue)>]
    [<DataRow("12", "def pred T() { is(self,pred) };", LiteralTrue)>]
    [<DataRow("12a", "def pred T() { intr prty pred T1() {is(parent,pred)} };", LiteralTrue)>]
    [<DataRow("12b", "def pred T() { intr prty pred T1() {is(parent,ind)} };", LiteralFalse)>]
    [<DataRow("13", "def pred T() { dec ~x:pred(y:func()->obj); is(x,pred) };", LiteralTrue)>]
    [<DataRow("14", "def pred T() { dec ~x:pred(y:func()->obj); is(x,pred(y:func()->obj)) };", LiteralTrue)>]
    [<DataRow("15", "def pred T() { dec ~x:pred(y:func()->obj); is(x,pred(y:func()->ind)) };", LiteralFalse)>]
    [<DataRow("16", "def pred T() { dec ~x:pred(y:func()->obj); is(x,pred(y:func(z:obj)->obj)) };", LiteralFalse)>]
    [<DataRow("17", "def pred T() { dec ~x:pred(y:func(z:obj)->obj); is(x,pred(y:func(z:obj)->Nat)) };", LiteralFalse)>]
    [<DataRow("18", "def pred T() { dec ~x:pred(y:func(z:obj)->Nat); is(x,pred(y:func(z:obj)->Nat)) };", LiteralTrue)>]
    [<DataRow("19", "def pred T() { dec ~x:pred(y:func(z:obj)->Nat); is(x,pred(a:func(b:obj)->Nat)) };", LiteralTrue)>]
    [<TestMethod>]
    member this.TestExpressionEvaluationIsOperand(no:string, fplCode, expected: string) =
        ad.Clear()
        let filename = "TestExpressionEvaluationIsOperand"
        let stOption = prepareFplCode (filename + ".fpl", fplCode, false)
        prepareFplCode (filename, "", false) |> ignore

        match stOption with
        | Some st ->
            let r = st.Root
            let theory = r.Scope[filename]
            let pr1 = 
                if fplCode.Contains("prty ") then 
                    theory.Scope["T()"].Scope["T1()"]
                else
                    theory.Scope["T()"]         

            let actual = evalTreeFplRepresentation(pr1)
            Assert.AreEqual<string>(expected, actual)
        | None -> Assert.IsTrue(false)

    [<DataRow("def pred T() { dec ~x:pred(y:obj); is(self,pred) };", LiteralPred)>]
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
            Assert.AreEqual<string>(expected, mapping.Type(SignatureType.Type))
        | None -> Assert.IsTrue(false)