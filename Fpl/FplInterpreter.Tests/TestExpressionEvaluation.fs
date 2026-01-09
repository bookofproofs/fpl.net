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
    [<DataRow("01", "def pred T() { dec ~x:A; is(x,A) };", LiteralFalse)>] // Type A is undefined
    [<DataRow("01a", "def cl A def pred T() { dec ~x:A; is(x,A) };", LiteralTrue)>] // Type A is defined
    [<DataRow("01b", "def func A()->ind def pred T() { dec ~x:func()->ind; is(x,A) };", LiteralTrue)>] 
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

    // match with simple types
    [<DataRow("ST0", "def pred Test() {dec ~x:obj; is(x, obj)};", LiteralTrue)>]
    [<DataRow("ST1", "def pred Test() {dec ~x:ind; is(x, ind)};", LiteralTrue)>]
    [<DataRow("ST2", "def pred Test() {dec ~x:func; is(x, func)};", LiteralTrue)>]
    [<DataRow("ST2a", "def pred Test() {dec ~x:func()->ind; is(x, func)};", LiteralTrue)>]
    [<DataRow("ST2b", "def pred Test() {dec ~x:func(y:obj)->ind; is(x, func)};", LiteralTrue)>]
    [<DataRow("ST2c", "def pred Test() {dec ~x:func(y:obj)->func; is(x, func)};", LiteralTrue)>]
    [<DataRow("ST2d", "def pred Test() {dec ~x:func(y:obj)->func(z:pred)->pred; is(x, func)};", LiteralTrue)>]
    [<DataRow("ST3", "def pred Test() {dec ~x:pred; is(x, pred)};", LiteralTrue)>]
    [<DataRow("ST3a", "def pred Test() {dec ~x:pred(); is(x, pred)};", LiteralTrue)>]
    [<DataRow("ST3b", "def pred Test() {dec ~x:pred; is(x, pred)};", LiteralTrue)>]
    [<DataRow("ST3c", "def pred Test() {dec ~x:pred(y:obj); is(x, pred)};", LiteralTrue)>]

    // mismatch with simple type obj
    [<DataRow("ST0_obj", "def pred Test() {dec ~x:obj; is(x, obj)};", LiteralTrue)>]
    [<DataRow("ST1_obj", "def pred Test() {dec ~x:ind; is(x, obj)};", LiteralFalse)>]
    [<DataRow("ST2_obj", "def pred Test() {dec ~x:func; is(x, obj)};", LiteralFalse)>]
    [<DataRow("ST2a_obj", "def pred Test() {dec ~x:func()->ind; is(x, obj)};", LiteralFalse)>]
    [<DataRow("ST2b_obj", "def pred Test() {dec ~x:func(y:obj)->ind; is(x, obj)};", LiteralFalse)>]
    [<DataRow("ST2c_obj", "def pred Test() {dec ~x:func(y:obj)->func; is(x, obj)};", LiteralFalse)>]
    [<DataRow("ST2d_obj", "def pred Test() {dec ~x:func(y:obj)->func(z:pred)->pred; is(x, obj)};", LiteralFalse)>]
    [<DataRow("ST3_obj", "def pred Test() {dec ~x:pred; is(x, obj)};", LiteralFalse)>]
    [<DataRow("ST3a_obj", "def pred Test() {dec ~x:pred(); is(x, obj)};", LiteralFalse)>]
    [<DataRow("ST3b_obj", "def pred Test() {dec ~x:pred; is(x, obj)};", LiteralFalse)>]
    [<DataRow("ST3c_obj", "def pred Test() {dec ~x:pred(y:obj); is(x, obj)};", LiteralFalse)>]

    // mismatch with simple type ind
    [<DataRow("ST0_ind", "def pred Test() {dec ~x:obj; is(x, ind)};", LiteralFalse)>]
    [<DataRow("ST1_ind", "def pred Test() {dec ~x:ind; is(x, ind)};", LiteralTrue)>]
    [<DataRow("ST2_ind", "def pred Test() {dec ~x:func; is(x, ind)};", LiteralFalse)>]
    [<DataRow("ST2a_ind", "def pred Test() {dec ~x:func()->ind; is(x, ind)};", LiteralFalse)>]
    [<DataRow("ST2b_ind", "def pred Test() {dec ~x:func(y:obj)->ind; is(x, ind)};", LiteralFalse)>]
    [<DataRow("ST2c_ind", "def pred Test() {dec ~x:func(y:obj)->func; is(x, ind)};", LiteralFalse)>]
    [<DataRow("ST2d_ind", "def pred Test() {dec ~x:func(y:obj)->func(z:pred)->pred; is(x, ind)};", LiteralFalse)>]
    [<DataRow("ST3_ind", "def pred Test() {dec ~x:pred; is(x, ind)};", LiteralFalse)>]
    [<DataRow("ST3a_ind", "def pred Test() {dec ~x:pred(); is(x, ind)};", LiteralFalse)>]
    [<DataRow("ST3b_ind", "def pred Test() {dec ~x:pred; is(x, ind)};", LiteralFalse)>]
    [<DataRow("ST3c_ind", "def pred Test() {dec ~x:pred(y:obj); is(x, ind)};", LiteralFalse)>]

    // mismatch with simple type pred
    [<DataRow("ST0_pred", "def pred Test() {dec ~x:obj; is(x, pred)};", LiteralFalse)>]
    [<DataRow("ST1_pred", "def pred Test() {dec ~x:ind; is(x, pred)};", LiteralFalse)>]
    [<DataRow("ST2_pred", "def pred Test() {dec ~x:func; is(x, pred)};", LiteralFalse)>]
    [<DataRow("ST2a_pred", "def pred Test() {dec ~x:func()->ind; is(x, pred)};", LiteralFalse)>]
    [<DataRow("ST2b_pred", "def pred Test() {dec ~x:func(y:obj)->pred; is(x, pred)};", LiteralFalse)>]
    [<DataRow("ST2c_pred", "def pred Test() {dec ~x:func(y:obj)->func; is(x, pred)};", LiteralFalse)>]
    [<DataRow("ST2d_pred", "def pred Test() {dec ~x:func(y:obj)->func(z:pred)->pred; is(x, pred)};", LiteralFalse)>]
    [<DataRow("ST3_pred", "def pred Test() {dec ~x:pred; is(x, pred)};", LiteralTrue)>]
    [<DataRow("ST3a_pred", "def pred Test() {dec ~x:pred(); is(x, pred)};", LiteralTrue)>]
    [<DataRow("ST3b_pred", "def pred Test() {dec ~x:pred; is(x, pred)};", LiteralTrue)>]
    [<DataRow("ST3c_pred", "def pred Test() {dec ~x:pred(y:obj); is(x, pred)};", LiteralTrue)>]

    // mismatch with simple type func
    [<DataRow("ST0_func", "def pred Test() {dec ~x:obj; is(x, func)};", LiteralFalse)>]
    [<DataRow("ST1_func", "def pred Test() {dec ~x:ind; is(x, func)};", LiteralFalse)>]
    [<DataRow("ST2_func", "def pred Test() {dec ~x:func; is(x, func)};", LiteralTrue)>]
    [<DataRow("ST2a_func", "def pred Test() {dec ~x:func()->ind; is(x, func)};", LiteralTrue)>]
    [<DataRow("ST2b_func", "def pred Test() {dec ~x:func(y:obj)->pred; is(x, func)};", LiteralTrue)>]
    [<DataRow("ST2c_func", "def pred Test() {dec ~x:func(y:obj)->func; is(x, func)};", LiteralTrue)>]
    [<DataRow("ST2d_func", "def pred Test() {dec ~x:func(y:obj)->func(z:pred)->pred; is(x, func)};", LiteralTrue)>]
    [<DataRow("ST3_func", "def pred Test() {dec ~x:pred; is(x, func)};", LiteralFalse)>]
    [<DataRow("ST3a_func", "def pred Test() {dec ~x:pred(); is(x, func)};", LiteralFalse)>]
    [<DataRow("ST3b_func", "def pred Test() {dec ~x:pred; is(x, func)};", LiteralFalse)>]
    [<DataRow("ST3c_func", "def pred Test() {dec ~x:pred(y:obj); is(x, func)};", LiteralFalse)>]

    // (mis)match with pred() types
    [<DataRow("NP0", "def pred Test() {dec ~x:obj; is(x, pred())};", LiteralFalse)>]
    [<DataRow("NP1", "def pred Test() {dec ~x:ind; is(x, pred())};", LiteralFalse)>]
    [<DataRow("NP2", "def pred Test() {dec ~x:func; is(x, pred())};", LiteralFalse)>]
    [<DataRow("NP2a", "def pred Test() {dec ~x:func()->ind; is(x, pred())};", LiteralFalse)>]
    [<DataRow("NP2b", "def pred Test() {dec ~x:func(y:obj)->ind; is(x, pred())};", LiteralFalse)>]
    [<DataRow("NP2c", "def pred Test() {dec ~x:func(y:obj)->func; is(x, pred())};", LiteralFalse)>]
    [<DataRow("NP2d", "def pred Test() {dec ~x:func(y:obj)->func(z:pred)->pred; is(x, pred())};", LiteralFalse)>]
    [<DataRow("NP3", "def pred Test() {dec ~x:pred; is(x, pred())};", LiteralFalse)>]
    [<DataRow("NP3a", "def pred Test() {dec ~x:pred(); is(x, pred())};", LiteralTrue)>]
    [<DataRow("NP3b", "def pred Test() {dec ~x:pred; is(x, pred())};", LiteralFalse)>]
    [<DataRow("NP3c", "def pred Test() {dec ~x:pred(y:obj); is(x, pred())};", LiteralFalse)>]
    // (mis)match with pred(...) types
    [<DataRow("NP_0", "def pred Test() {dec ~x:obj; is(x, pred(a:obj))};", LiteralFalse)>]
    [<DataRow("NP_1", "def pred Test() {dec ~x:ind; is(x, pred(a:obj))};", LiteralFalse)>]
    [<DataRow("NP_2", "def pred Test() {dec ~x:func; is(x, pred(a:obj))};", LiteralFalse)>]
    [<DataRow("NP_2a", "def pred Test() {dec ~x:func()->ind; is(x, pred(a:obj))};", LiteralFalse)>]
    [<DataRow("NP_2b", "def pred Test() {dec ~x:func(y:obj)->ind; is(x, pred(a:obj))};", LiteralFalse)>]
    [<DataRow("NP_2c", "def pred Test() {dec ~x:func(y:obj)->func; is(x, pred(a:obj))};", LiteralFalse)>]
    [<DataRow("NP_2d", "def pred Test() {dec ~x:func(y:obj)->func(z:pred)->pred; is(x, pred(a:obj))};", LiteralFalse)>]
    [<DataRow("NP_3", "def pred Test() {dec ~x:pred; is(x, pred(a:obj))};", LiteralFalse)>]
    [<DataRow("NP_3a", "def pred Test() {dec ~x:pred(); is(x, pred(a:obj))};", LiteralFalse)>]
    [<DataRow("NP_3b", "def pred Test() {dec ~x:pred; is(x, pred(a:obj))};", LiteralFalse)>]
    [<DataRow("NP_3c", "def pred Test() {dec ~x:pred(y:obj); is(x, pred(a:obj))};", LiteralTrue)>]
    [<DataRow("NP_3c", "def pred Test() {dec ~x:pred(y:ind); is(x, pred(a:obj))};", LiteralFalse)>]

    // (mis)match with func() types
    [<DataRow("NF0", "def pred Test() {dec ~x:obj; is(x, func()->ind)};", LiteralFalse)>]
    [<DataRow("NF1", "def pred Test() {dec ~x:ind; is(x, func()->ind)};", LiteralFalse)>]
    [<DataRow("NF2", "def pred Test() {dec ~x:func; is(x, func()->ind)};", LiteralFalse)>]
    [<DataRow("NF2a", "def pred Test() {dec ~x:func()->ind; is(x, func()->ind)};", LiteralTrue)>]
    [<DataRow("NF2b", "def pred Test() {dec ~x:func(y:obj)->ind; is(x, func()->ind)};", LiteralFalse)>]
    [<DataRow("NF2c", "def pred Test() {dec ~x:func(y:obj)->obj; is(x, func()->ind)};", LiteralFalse)>]
    [<DataRow("NF2d", "def pred Test() {dec ~x:func(y:ind)->ind; is(x, func()->ind)};", LiteralFalse)>]
    [<DataRow("NF2e", "def pred Test() {dec ~x:func(y:obj)->func; is(x, func()->ind)};", LiteralFalse)>]
    [<DataRow("NF2f", "def pred Test() {dec ~x:func(y:obj)->func(z:pred)->pred; is(x, func()->ind)};", LiteralFalse)>]
    [<DataRow("NF3", "def pred Test() {dec ~x:pred; is(x, func()->ind)};", LiteralFalse)>]
    [<DataRow("NF3a", "def pred Test() {dec ~x:pred(); is(x, func()->ind)};", LiteralFalse)>]
    [<DataRow("NF3b", "def pred Test() {dec ~x:pred; is(x, func()->ind)};", LiteralFalse)>]
    [<DataRow("NF3c", "def pred Test() {dec ~x:pred(y:obj); is(x, func()->ind)};", LiteralFalse)>]
    // (mis)match with func(...) types
    [<DataRow("NF_0", "def pred Test() {dec ~x:obj; is(x, func(a:obj)->ind)};", LiteralFalse)>]
    [<DataRow("NF_1", "def pred Test() {dec ~x:ind; is(x, func(a:obj)->ind)};", LiteralFalse)>]
    [<DataRow("NF_2", "def pred Test() {dec ~x:func; is(x, func(a:obj)->ind)};", LiteralFalse)>]
    [<DataRow("NF_2a", "def pred Test() {dec ~x:func()->ind; is(x, func(a:obj)->ind)};", LiteralFalse)>]
    [<DataRow("NF_2b", "def pred Test() {dec ~x:func(y:obj)->ind; is(x, func(a:obj)->ind)};", LiteralTrue)>]
    [<DataRow("NF_2c", "def pred Test() {dec ~x:func(y:obj)->obj; is(x, func(a:obj)->ind)};", LiteralFalse)>]
    [<DataRow("NF_2d", "def pred Test() {dec ~x:func(y:ind)->ind; is(x, func(a:obj)->ind)};", LiteralFalse)>]
    [<DataRow("NF_2e", "def pred Test() {dec ~x:func(y:obj)->func; is(x, func(a:obj)->ind)};", LiteralFalse)>]
    [<DataRow("NF_2f", "def pred Test() {dec ~x:func(y:obj)->func(z:pred)->pred; is(x, func(a:obj)->ind)};", LiteralFalse)>]
    [<DataRow("NF_3", "def pred Test() {dec ~x:pred; is(x, func(a:obj)->ind)};", LiteralFalse)>]
    [<DataRow("NF_3a", "def pred Test() {dec ~x:pred(); is(x, func(a:obj)->ind)};", LiteralFalse)>]
    [<DataRow("NF_3b", "def pred Test() {dec ~x:pred; is(x, func(a:obj)->ind)};", LiteralFalse)>]
    [<DataRow("NF_3c", "def pred Test() {dec ~x:pred(y:obj); is(x, func(a:obj)->ind)};", LiteralFalse)>]
    [<DataRow("NF_3d", "def pred Test() {dec ~x:pred(y:ind); is(x, func(a:obj)->ind)};", LiteralFalse)>]

    // match with class type
    [<DataRow("CT1", "def cl A {intr} def pred Test() {dec ~x:A; is(x, obj)};", LiteralTrue)>] // A is obj, no error
    [<DataRow("CT2", "def cl A {intr} def pred Test() {dec ~x:A; is(x, A)};", LiteralTrue)>] // A is A, no error
    [<DataRow("CT3", "def cl A {intr} def cl B:A {intr} def pred Test() {dec ~x:B; is(x, A)};", LiteralTrue)>] // x is also B:A, no error
    [<DataRow("CT4", "def cl A {intr} def cl B:A {intr} def pred Test() {dec ~x:B; is(x, B)};", LiteralTrue)>] // x is B, no error
    [<DataRow("CT5", "def cl A {intr} def cl B:A {intr} def pred Test() {dec ~x:B; is(x, obj)};", LiteralTrue)>] // x is also B:A:obj, no error
    // ... with instances
    [<DataRow("CI1", "def cl A {intr} def pred Test() {dec ~x:A x:=A(); is(x, obj)};", LiteralTrue)>] // A is obj, no error
    [<DataRow("CI2", "def cl A {intr} def pred Test() {dec ~x:A x:=A(); is(x, A)};", LiteralTrue)>] // A is A, no error
    [<DataRow("CI3", "def cl A {intr} def cl B:A {intr} def pred Test() {dec ~x:B x:=B(); is(x, A)};", LiteralTrue)>] // x is also B:A, no error
    [<DataRow("CI4", "def cl A {intr} def cl B:A {intr} def pred Test() {dec ~x:B x:=B(); is(x, B)};", LiteralTrue)>] // x is B, no error
    [<DataRow("CI5", "def cl A {intr} def cl B:A {intr} def pred Test() {dec ~x:B x:=B(); is(x, obj)};", LiteralTrue)>] // x is also B:A:obj, no error

    // mismatch with class type
    [<DataRow("CT1_", "def pred Test() {dec ~x:A; is(x, obj)};", LiteralFalse)>] // A is undefined, error
    [<DataRow("CT2_", "def cl A def pred Test() {dec ~x:obj; is(x, A)};", LiteralFalse)>] // obj is not A, error
    [<DataRow("CT3_", "def cl A def cl B:A def pred Test() {dec ~a:A; is(a, B)};", LiteralFalse)>] // A is not B, error
    // mismatch with class references
    [<DataRow("CI1_", "def pred Test() {dec ~x:A x:=A; is(x, obj)};", LiteralFalse)>] // A is undefined, error
    [<DataRow("CI2_", "def cl A def cl B:A def pred Test() {dec ~a:A a:=A; is(a, B)};", LiteralFalse)>] // A is not B, error
    [<DataRow("CI3_", "def cl A def cl B:A def pred Test() {dec ~a:B a:=B; is(a, B)};", LiteralFalse)>] // B is B, but a class reference, error
    [<DataRow("CI4_", "def cl A def pred Test() {dec ~x:A x:=A; is(x, obj)};", LiteralFalse)>] // A is obj, but x is class reference, error
    [<DataRow("CI5_", "def cl A def cl B:A def pred Test() {dec ~x:B x:=B; is(x, B)};", LiteralFalse)>] // B is B, but x is class referene, error
    [<DataRow("CI6_", "def cl A def pred Test() {dec ~x:A x:=A; is(x, A)};", LiteralFalse)>] // A is A, but x is class reference, error
    [<DataRow("CI7_", "def cl A def cl B:A def pred Test() {dec ~x:B x:=B; is(x, A)};", LiteralFalse)>] // B is A but x is a class reference, error 
    [<DataRow("CI8_", "def cl A def cl B:A def pred Test() {dec ~x:B x:=B; is(x, B)};", LiteralFalse)>] // x is B, but class reference, error
    [<DataRow("CI9_", "def cl A def cl B:A def pred Test() {dec ~x:B x:=B; is(x, obj)};", LiteralFalse)>] // B is obj but x is class reference, error

    // match with the type pred(...) 
    [<DataRow("MS1", "def pred A(z:obj) def pred Test() {is(A, pred(y:obj))};", LiteralTrue)>] // true: pred(y:obj) matches signature A(obj)
    [<DataRow("MS1a", "def pred A(z:obj) def pred Test() {dec ~x:obj; is(A(x), pred(y:obj))};", LiteralFalse)>] // false: pred(y:obj) does not match value A(obj) 
    [<DataRow("MS1b", "def pred A(z:obj) def pred Test() {dec ~x:ind; is(A(x), pred(y:obj))};", LiteralFalse)>] // false: pred(y:obj) does not match value A(ind) not matching A(obj)
    [<DataRow("MS1c", "def pred A(z:ind) def pred Test() {dec ~x:ind; is(A(x), pred(y:obj))};", LiteralFalse)>] // false: pred(y:obj) does not match value A(ind) 
    [<DataRow("MS1d", "def pred A(z:ind) def pred Test() {is(A, pred(y:obj))};", LiteralFalse)>] // false: pred(y:obj) does not match signature A(ind)
    [<DataRow("MS1e", "ax A {true} def pred Test() {is(A, pred(y:obj))};", LiteralFalse)>] // false: pred(y:obj) does not match signature A (axiom)
    [<DataRow("MS1f", "thm A {true} def pred Test() {is(A, pred(y:obj))};", LiteralFalse)>] // false: pred(y:obj) does not match signature A (theorem)
    [<DataRow("MS1g", "lem A {true} def pred Test() {is(A, pred(y:obj))};", LiteralFalse)>] // false: pred(y:obj) does not match signature A (lemma)
    [<DataRow("MS1h", "prop A {true} def pred Test() {is(A, pred(y:obj))};", LiteralFalse)>] // false: pred(y:obj) does not match signature A (proposition)
    [<DataRow("MS1i", "conj A {true} def pred Test() {is(A, pred(y:obj))};", LiteralFalse)>] // false: pred(y:obj) does not match signature A (conjecture)
    [<DataRow("MS1j", "cor A$1 {true} def pred Test() {is(A$1, pred(y:obj))};", LiteralFalse)>] // false: pred(y:obj) does not match signature A (corollary)
    [<DataRow("MS1k", "proof A$1 {1. |- trivial} def pred Test() {is(A$1, pred(y:obj))};", LiteralFalse)>] // false: pred(y:obj) does not match signature A (proof)
    [<DataRow("MS1l", "inf A {pre:true con:true} def pred Test() {is(A, pred(y:obj))};", LiteralFalse)>] // false: pred(y:obj) does not match signature A (rule of inference)
    [<DataRow("MS1m", "def func A()->obj def pred Test() {is(A, pred(y:obj))};", LiteralFalse)>] // false: pred(y:obj) does not match signature A (functional term)
    [<DataRow("MS1n", "ext A x@/\d+/ -> obj {dec ~y:obj; return y} def pred Test() {is(A, pred(y:obj))};", LiteralFalse)>] // false: pred(y:obj) does not match signature A (extension)
    [<DataRow("MS1o", "def cl A def pred Test() {is(A, pred(y:obj))};", LiteralFalse)>] // false: pred(y:obj) does not match signature A (class)
    // ... using properties of classes
    [<DataRow("MS1p1", "def cl A {intr prty pred X(x:obj) } def pred Test() {is(A.X, pred(y:obj))};", LiteralTrue)>] // true: pred(y:obj) matches signature A.X(obj) 
    [<DataRow("MS1p2", "def cl A {intr prty pred X(x:ind) } def pred Test() {is(A.X, pred(y:obj))};", LiteralFalse)>] // false: pred(y:obj) does not match signature A.X(ind)
    [<DataRow("MS1p3", "def cl A {intr prty pred X() } def pred Test() {is(A.X, pred(y:obj))};", LiteralFalse)>] // false: pred(y:obj) does not match signature A.X()
    [<DataRow("MS1p4", "def cl A {intr prty func X()->obj } def pred Test() {is(A.X, pred(y:obj))};", LiteralFalse)>] // false: pred(y:obj) does not match signature A.X()->obj
    [<DataRow("MS1p5", "def cl A {intr prty func X(x:obj)->obj } def pred Test() {is(A.X, pred(y:obj))};", LiteralFalse)>] // false: pred(y:obj) does not match signature A.X(obj)->obj
    [<DataRow("MS1p6", "def cl A {intr prty func X(x:ind)->obj } def pred Test() {is(A.X, pred(y:obj))};", LiteralFalse)>] // false: pred(y:obj) does not match signature A.X(ind)->obj
    [<DataRow("MS1p7", "def cl A {intr prty pred X(x:obj) } def pred Test() {dec ~a:obj; is(A.X(a), pred(y:obj))};", LiteralFalse)>] // false: pred(y:obj) does not match by value A.X(obj) 
    // ... using properties of instances
    [<DataRow("MS1q1", "def cl A {intr prty pred X(x:obj) } def pred Test() {dec ~o:A o:=A(); is(o.X, pred(y:obj))};", LiteralTrue)>] // true: pred(y:obj) matches signature A.X(obj) 
    [<DataRow("MS1q2", "def cl A {intr prty pred X(x:ind) } def pred Test() {dec ~o:A o:=A(); is(o.X, pred(y:obj))};", LiteralFalse)>] // false: pred(y:obj) does not match signature A.X(ind)
    [<DataRow("MS1q3", "def cl A {intr prty pred X() } def pred Test() {dec ~o:A o:=A(); is(o.X, pred(y:obj))};", LiteralFalse)>] // false: pred(y:obj) does not match signature A.X()
    [<DataRow("MS1q4", "def cl A {intr prty func X()->obj } def pred Test() {dec ~o:A o:=A(); is(o.X, pred(y:obj))};", LiteralFalse)>] // false: pred(y:obj) does not match signature A.X()
    [<DataRow("MS1q5", "def cl A {intr prty func X(x:obj)->obj } def pred Test() {dec ~o:A o:=A(); is(o.X, pred(y:obj))};", LiteralFalse)>] // false: pred(y:obj) does not match signature A.X(obj)->obj
    [<DataRow("MS1q6", "def cl A {intr prty func X(x:ind)->obj } def pred Test() {dec ~o:A o:=A(); is(o.X, pred(y:obj))};", LiteralFalse)>] // false: pred(y:obj) does not match signature A.X(ind)->obj
    [<DataRow("MS1q7", "def cl A {intr prty pred X(x:obj) } def pred Test() {dec ~a:obj ~o:A o:=A(); is(o.X(a), pred(y:obj))};", LiteralFalse)>] // false: pred(y:obj) does not match by value A.X(obj) 
    // ... using properties of predicates
    [<DataRow("MS1r1", "def pred A() {intr prty pred X(x:obj) } def pred Test() {is(A.X, pred(y:obj))};", LiteralTrue)>] // true: pred(y:obj) matches signature A.X(obj) 
    [<DataRow("MS1r2", "def pred A() {intr prty pred X(x:ind) } def pred Test() {is(A.X, pred(y:obj))};", LiteralFalse)>] // false: pred(y:obj) does not match signature A.X(ind)
    [<DataRow("MS1r3", "def pred A() {intr prty pred X() } def pred Test() {is(A.X, pred(y:obj))};", LiteralFalse)>] // false: pred(y:obj) does not match signature A.X()
    [<DataRow("MS1r4", "def pred A() {intr prty func X()->obj } def pred Test() {is(A.X, pred(y:obj))};", LiteralFalse)>] // false: pred(y:obj) does not match signature A.X()
    [<DataRow("MS1r5", "def pred A() {intr prty func X(x:obj)->obj } def pred Test() {is(A.X, pred(y:obj))};", LiteralFalse)>] // false: pred(y:obj) does not match signature A.X(obj)->obj
    [<DataRow("MS1r6", "def pred A() {intr prty func X(x:ind)->obj } def pred Test() {is(A.X, pred(y:obj))};", LiteralFalse)>] // false: pred(y:obj) does not match signature A.X(ind)->obj
    [<DataRow("MS1r7", "def pred A() {intr prty pred X(x:obj) } def pred Test() {dec ~a:obj; is(A.X(a), pred(y:obj))};", LiteralFalse)>] // false: pred(y:obj) does not match by value A.X(obj) 
    // ... using properties of functional terms
    [<DataRow("MS1s1", "def func A()->obj {intr prty pred X(x:obj) } def pred Test() {is(A.X, pred(y:obj))};", LiteralTrue)>] // true: pred(y:obj) matches signature A.X(obj) 
    [<DataRow("MS1s2", "def func A()->obj {intr prty pred X(x:ind) } def pred Test() {is(A.X, pred(y:obj))};", LiteralFalse)>] // false: pred(y:obj) does not match signature A.X(ind)
    [<DataRow("MS1s3", "def func A()->obj {intr prty pred X() } def pred Test() {is(A.X, pred(y:obj))};", LiteralFalse)>] // false: pred(y:obj) does not match signature A.X()
    [<DataRow("MS1s4", "def func A()->obj {intr prty func X()->obj } def pred Test() {is(A.X, pred(y:obj))};", LiteralFalse)>] // false: pred(y:obj) does not match signature A.X()
    [<DataRow("MS1s5", "def func A()->obj {intr prty func X(x:obj)->obj } def pred Test() {is(A.X, pred(y:obj))};", LiteralFalse)>] // false: pred(y:obj) does not match signature A.X(obj)->obj
    [<DataRow("MS1s6", "def func A()->obj {intr prty func X(x:ind)->obj } def pred Test() {is(A.X, pred(y:obj))};", LiteralFalse)>] // false: pred(y:obj) does not match signature A.X(ind)->obj
    [<DataRow("MS1s7", "def func A()->obj {intr prty pred X(x:obj) } def pred Test() {dec ~a:obj; is(A.X(a), pred(y:obj))};", LiteralFalse)>] // false: pred(y:obj) does not match by value A.X(obj) 
    
    // match with the type pred() 
    [<DataRow("MS1_", "def pred A() def pred Test() {is(A, pred())};", LiteralTrue)>] // true: pred() matches signature A()
    [<DataRow("MS1a_", "def pred A() def pred Test() {dec ~x:obj; is(A(x), pred())};", LiteralFalse)>] // false: pred() does not match value A(obj) 
    [<DataRow("MS1b_", "def pred A() def pred Test() {dec ~x:ind; is(A(x), pred())};", LiteralFalse)>] // false: pred() does not match value A(ind) not matching A(obj)
    [<DataRow("MS1c_", "def pred A() def pred Test() {dec ~x:ind; is(A(x), pred())};", LiteralFalse)>] // false: pred() does not match value A(ind) 
    [<DataRow("MS1d_", "def pred A(x:ind) def pred Test() {is(A, pred())};", LiteralFalse)>] // false: pred() does not match signature A(ind)
    [<DataRow("MS1e_", "ax A {true} def pred Test() {is(A, pred())};", LiteralFalse)>] // false: pred() does not match signature A (axiom)
    [<DataRow("MS1f_", "thm A {true} def pred Test() {is(A, pred())};", LiteralFalse)>] // false: pred() does not match signature A (theorem)
    [<DataRow("MS1g_", "lem A {true} def pred Test() {is(A, pred())};", LiteralFalse)>] // false: pred() does not match signature A (lemma)
    [<DataRow("MS1h_", "prop A {true} def pred Test() {is(A, pred())};", LiteralFalse)>] // false: pred() does not match signature A (proposition)
    [<DataRow("MS1i_", "conj A {true} def pred Test() {is(A, pred())};", LiteralFalse)>] // false: pred() does not match signature A (conjecture)
    [<DataRow("MS1j_", "cor A$1 {true} def pred Test() {is(A$1, pred())};", LiteralFalse)>] // false: pred() does not match signature A (corollary)
    [<DataRow("MS1k_", "proof A$1 {1. |- trivial} def pred Test() {is(A$1, pred())};", LiteralFalse)>] // false: pred() does not match signature A (proof)
    [<DataRow("MS1l_", "inf A {pre:true con:true} def pred Test() {is(A, pred())};", LiteralFalse)>] // false: pred() does not match signature A (rule of inference)
    [<DataRow("MS1m_", "def func A()->obj def pred Test() {is(A, pred())};", LiteralFalse)>] // false: pred() does not match signature A (functional term)
    [<DataRow("MS1n_", "ext A x@/\d+/ -> obj {dec ~y:obj; return y} def pred Test() {is(A, pred())};", LiteralFalse)>] // false: pred() does not match signature A (extension)
    [<DataRow("MS1o_", "def cl A def pred Test() {is(A, pred())};", LiteralFalse)>] // false: pred() does not match signature A (class)
    // ... using properties of classes
    [<DataRow("MS1p_1", "def cl A {intr prty pred X(x:obj) } def pred Test() {is(A.X, pred())};", LiteralFalse)>] // false: pred() does not match signature A.X(obj) 
    [<DataRow("MS1p_2", "def cl A {intr prty pred X(x:ind) } def pred Test() {is(A.X, pred())};", LiteralFalse)>] // false: pred() does not match signature A.X(ind)
    [<DataRow("MS1p_3", "def cl A {intr prty pred X() } def pred Test() {is(A.X, pred())};", LiteralTrue)>] // true: pred() match signature A.X()
    [<DataRow("MS1p_4", "def cl A {intr prty func X()->obj } def pred Test() {is(A.X, pred())};", LiteralFalse)>] // false: pred() does not match signature A.X()->obj
    [<DataRow("MS1p_5", "def cl A {intr prty func X(x:obj)->obj } def pred Test() {is(A.X, pred())};", LiteralFalse)>] // false: pred() does not match signature A.X(obj)->obj
    [<DataRow("MS1p_6", "def cl A {intr prty func X(x:ind)->obj } def pred Test() {is(A.X, pred())};", LiteralFalse)>] // false: pred() does not match signature A.X(ind)->obj
    [<DataRow("MS1p_7", "def cl A {intr prty pred X(x:obj) } def pred Test() {dec ~a:obj; is(A.X(a), pred())};", LiteralFalse)>] // false: pred() does not match by value A.X(obj) 
    // ... using properties of instances
    [<DataRow("MS1q_1", "def cl A {intr prty pred X(x:obj) } def pred Test() {dec ~o:A o:=A(); is(o.X, pred())};", LiteralFalse)>] // false: pred() does not match signature A.X(obj) 
    [<DataRow("MS1q_2", "def cl A {intr prty pred X(x:ind) } def pred Test() {dec ~o:A o:=A(); is(o.X, pred())};", LiteralFalse)>] // false: pred() does not match signature A.X(ind)
    [<DataRow("MS1q_3", "def cl A {intr prty pred X() } def pred Test() {dec ~o:A o:=A(); is(o.X, pred())};", LiteralTrue)>] // true: pred() match signature A.X()
    [<DataRow("MS1q_4", "def cl A {intr prty func X()->obj } def pred Test() {dec ~o:A o:=A(); is(o.X, pred())};", LiteralFalse)>] // false: pred() does not match signature A.X()->obj
    [<DataRow("MS1q_5", "def cl A {intr prty func X(x:obj)->obj } def pred Test() {dec ~o:A o:=A(); is(o.X, pred())};", LiteralFalse)>] // false: pred() does not match signature A.X(obj)->obj
    [<DataRow("MS1q_6", "def cl A {intr prty func X(x:ind)->obj } def pred Test() {dec ~o:A o:=A(); is(o.X, pred())};", LiteralFalse)>] // false: pred() does not match signature A.X(ind)->obj
    [<DataRow("MS1q_7", "def cl A {intr prty pred X(x:obj) } def pred Test() {dec ~a:obj ~o:A o:=A(); is(o.X(a), pred())};", LiteralFalse)>] // false: pred() does not match by value A.X(obj) 
    // ... using properties of predicates
    [<DataRow("MS1r_1", "def pred A() {intr prty pred X(x:obj) } def pred Test() {is(A.X, pred())};", LiteralFalse)>] // false: pred() does not match signature A.X(obj) 
    [<DataRow("MS1r_2", "def pred A() {intr prty pred X(x:ind) } def pred Test() {is(A.X, pred())};", LiteralFalse)>] // false: pred() does not match signature A.X(ind)
    [<DataRow("MS1r_3", "def pred A() {intr prty pred X() } def pred Test() {is(A.X, pred())};", LiteralTrue)>] // true: pred() match signature A.X()
    [<DataRow("MS1r_4", "def pred A() {intr prty func X()->obj } def pred Test() {is(A.X, pred())};", LiteralFalse)>] // false: pred() does not match signature A.X()
    [<DataRow("MS1r_5", "def pred A() {intr prty func X(x:obj)->obj } def pred Test() {is(A.X, pred())};", LiteralFalse)>] // false: pred() does not match signature A.X(obj)->obj
    [<DataRow("MS1r_6", "def pred A() {intr prty func X(x:ind)->obj } def pred Test() {is(A.X, pred())};", LiteralFalse)>] // false: pred() does not match signature A.X(ind)->obj
    [<DataRow("MS1r_7", "def pred A() {intr prty pred X(x:obj) } def pred Test() {dec ~a:obj; is(A.X(a), pred())};", LiteralFalse)>] // false: pred() does not match by value A.X(obj) 
    // ... using properties of functional terms
    [<DataRow("MS1s_1", "def func A()->obj {intr prty pred X(x:obj) } def pred Test() {is(A.X, pred())};", LiteralFalse)>] // false: pred() does not match signature A.X(obj) 
    [<DataRow("MS1s_2", "def func A()->obj {intr prty pred X(x:ind) } def pred Test() {is(A.X, pred())};", LiteralFalse)>] // false: pred() does not match signature A.X(ind)
    [<DataRow("MS1s_3", "def func A()->obj {intr prty pred X() } def pred Test() {is(A.X, pred())};", LiteralTrue)>] // true: pred() match signature A.X()
    [<DataRow("MS1s_4", "def func A()->obj {intr prty func X()->obj } def pred Test() {is(A.X, pred())};", LiteralFalse)>] // false: pred() does not match signature A.X()->obj
    [<DataRow("MS1s_5", "def func A()->obj {intr prty func X(x:obj)->obj } def pred Test() {is(A.X, pred())};", LiteralFalse)>] // false: pred() does not match signature A.X(obj)->obj
    [<DataRow("MS1s_6", "def func A()->obj {intr prty func X(x:ind)->obj } def pred Test() {is(A.X, pred())};", LiteralFalse)>] // false: pred() does not match signature A.X(ind)->obj
    [<DataRow("MS1s_7", "def func A()->obj {intr prty pred X(x:obj) } def pred Test() {dec ~a:obj; is(A.X(a), pred())};", LiteralFalse)>] // false: pred() does not match by value A.X(obj) 
     
    // match with the type pred 
    [<DataRow("MS2", "def pred A(z:obj) def pred Test() {is(A, pred)};", LiteralTrue)>] // true: pred matches signature A(obj)
    [<DataRow("MS2a", "def pred A(z:obj) def pred Test() {dec ~x:obj; is(A(x), pred)};", LiteralTrue)>] // true: pred matches value A(obj) 
    [<DataRow("MS2b", "def pred A(z:obj) def pred Test() {dec ~x:ind; is(A(x), pred)};", LiteralFalse)>] // false: pred does not match value A(ind) since it does not match A(obj)
    [<DataRow("MS2c", "def pred A(z:ind) def pred Test() {dec ~x:ind; is(A(x), pred)};", LiteralTrue)>] // true: pred matches value A(ind) 
    [<DataRow("MS2d", "def pred A(z:ind) def pred Test() {is(A, pred)};", LiteralTrue)>] // true: pred matches signature A(ind)
    [<DataRow("MS2e", "ax A {true} def pred Test() {is(A, pred)};", LiteralTrue)>] // true: pred matches signature A (axiom)
    [<DataRow("MS2f", "thm A {true} def pred Test() {is(A, pred)};", LiteralTrue)>] // true: pred matches signature A (theorem)
    [<DataRow("MS2g", "lem A {true} def pred Test() {is(A, pred)};", LiteralTrue)>] // true: pred matches signature A (lemma)
    [<DataRow("MS2h", "prop A {true} def pred Test() {is(A, pred)};", LiteralTrue)>] // true: pred matches signature A (proposition)
    [<DataRow("MS2i", "conj A {true} def pred Test() {is(A, pred)};", LiteralTrue)>] // true: pred does matches signature A (conjecture)
    [<DataRow("MS2j", "cor A$1 {true} def pred Test() {is(A$1, pred)};", LiteralTrue)>] // true: pred matches signature A$1 (corollary)
    [<DataRow("MS2k", "proof A$1 {1. |- trivial} def pred Test() {is(A$1, pred)};", LiteralFalse)>] // false: pred does not match signature A$1 (proof)
    [<DataRow("MS2l", "inf A {pre:true con:true} def pred Test() {is(A, pred)};", LiteralFalse)>] // false: pred does not match signature A (rule of inference)
    [<DataRow("MS2m", "def func A()->obj def pred Test() {is(A, pred)};", LiteralFalse)>] // false: pred does not match signature A (functional term)
    [<DataRow("MS2n", "ext A x@/\d+/ -> obj {dec ~y:obj; return y} def pred Test() {is(A, pred)};", LiteralFalse)>] // false: pred does not match signature A (extension)
    [<DataRow("MS2o", "def cl A def pred Test() {is(A, pred)};", LiteralFalse)>] // false: pred does not match signature A (class)
    // ... using properties of classes
    [<DataRow("MS2p1", "def cl A {intr prty pred X(x:obj) } def pred Test() {is(A.X, pred)};", LiteralTrue)>] // true: pred matches signature A.A(obj) 
    [<DataRow("MS2p2", "def cl A {intr prty pred X(x:ind) } def pred Test() {is(A.X, pred)};", LiteralTrue)>] // true: pred does not match signature A.X(ind)
    [<DataRow("MS2p3", "def cl A {intr prty pred X() } def pred Test() {is(A.X, pred)};", LiteralTrue)>] // true: pred match signature A.X()
    [<DataRow("MS2p4", "def cl A {intr prty func X()->obj } def pred Test() {is(A.X, pred)};", LiteralFalse)>] // false: pred does not match signature A.X()->obj
    [<DataRow("MS2p5", "def cl A {intr prty func X(x:obj)->obj } def pred Test() {is(A.X, pred)};", LiteralFalse)>] // false: pred does not match signature A.X(obj)->obj
    [<DataRow("MS2p6", "def cl A {intr prty func X(x:ind)->obj } def pred Test() {is(A.X, pred)};", LiteralFalse)>] // false: pred does not match signature A.X(ind)->obj
    [<DataRow("MS2p7", "def cl A {intr prty pred X(x:obj) } def pred Test() {dec ~a:obj; is(A.X(a), pred)};", LiteralFalse)>] // false: pred does not match by value A.X(obj) 
    // ... using properties of instances
    [<DataRow("MS2q1", "def cl A {intr prty pred X(x:obj) } def pred Test() {dec ~o:A o:=A(); is(o.X, pred)};", LiteralTrue)>] // true: pred matches signature A.X(obj) 
    [<DataRow("MS2q2", "def cl A {intr prty pred X(x:ind) } def pred Test() {dec ~o:A o:=A(); is(o.X, pred)};", LiteralTrue)>] // true: pred matches signature A.X(ind)
    [<DataRow("MS2q3", "def cl A {intr prty pred X() } def pred Test() {dec ~o:A o:=A(); is(o.X, pred)};", LiteralTrue)>] // true: pred match signature A.X()
    [<DataRow("MS2q4", "def cl A {intr prty func X()->obj } def pred Test() {dec ~o:A o:=A(); is(o.X, pred)};", LiteralFalse)>] // false: pred does not match signature A.X()->obj
    [<DataRow("MS2q5", "def cl A {intr prty func X(x:obj)->obj } def pred Test() {dec ~o:A o:=A(); is(o.X, pred)};", LiteralFalse)>] // false: pred does not match signature A.X(obj)->obj
    [<DataRow("MS2q6", "def cl A {intr prty func X(x:ind)->obj } def pred Test() {dec ~o:A o:=A(); is(o.X, pred)};", LiteralFalse)>] // false: pred does not match signature A.X(ind)->obj
    [<DataRow("MS2q7", "def cl A {intr prty pred X(x:obj) } def pred Test() {dec ~a:obj ~o:A o:=A(); is(o.X(a), pred)};", LiteralFalse)>] // false: pred does not match by value A.X(obj) 
    // ... using properties of predicates
    [<DataRow("MS2r1", "def pred A() {intr prty pred X(x:obj) } def pred Test() {is(A.X, pred)};", LiteralTrue)>] // true: pred matches signature A.X(obj) 
    [<DataRow("MS2r2", "def pred A() {intr prty pred X(x:ind) } def pred Test() {is(A.X, pred)};", LiteralTrue)>] // true: pred matches signature A.X(ind)
    [<DataRow("MS2r3", "def pred A() {intr prty pred X() } def pred Test() {is(A.X, pred)};", LiteralTrue)>] // true: pred match signature A.X()
    [<DataRow("MS2r4", "def pred A() {intr prty func X()->obj } def pred Test() {is(A.X, pred)};", LiteralFalse)>] // false: pred does not match signature A.X()->obj
    [<DataRow("MS2r5", "def pred A() {intr prty func X(x:obj)->obj } def pred Test() {is(A.X, pred)};", LiteralFalse)>] // false: pred does not match signature A.X(obj)->obj
    [<DataRow("MS2r6", "def pred A() {intr prty func X(x:ind)->obj } def pred Test() {is(A.X, pred)};", LiteralFalse)>] // false: pred does not match signature A.X(ind)->obj
    [<DataRow("MS2r7", "def pred A() {intr prty pred X(x:obj) } def pred Test() {dec ~a:obj; is(A.X(a), pred)};", LiteralFalse)>] // false: pred does not match by value A.X(obj) 
    // ... using properties of functional terms
    [<DataRow("MS2s1", "def func A()->obj {intr prty pred X(x:obj) } def pred Test() {is(A.X, pred)};", LiteralTrue)>] // true: pred matches signature A.X(obj) 
    [<DataRow("MS2s2", "def func A()->obj {intr prty pred X(x:ind) } def pred Test() {is(A.X, pred)};", LiteralTrue)>] // true: pred matches signature A.X(ind)
    [<DataRow("MS2s3", "def func A()->obj {intr prty pred X() } def pred Test() {is(A.X, pred)};", LiteralTrue)>] // true: pred match signature A.X()
    [<DataRow("MS2s4", "def func A()->obj {intr prty func X()->obj } def pred Test() {is(A.X, pred)};", LiteralFalse)>] // false: pred does not match signature A.X()->obj
    [<DataRow("MS2s5", "def func A()->obj {intr prty func X(x:obj)->obj } def pred Test() {is(A.X, pred)};", LiteralFalse)>] // false: pred does not match signature A.X(obj)->obj
    [<DataRow("MS2s6", "def func A()->obj {intr prty func X(x:ind)->obj } def pred Test() {is(A.X, pred)};", LiteralFalse)>] // false: pred does not match signature A.X(ind)->obj
    [<DataRow("MS2s7", "def func A()->obj {intr prty pred X(x:obj) } def pred Test() {dec ~a:obj; is(A.X(a), pred)};", LiteralFalse)>] // false: pred does not match by value A.X(obj) 

    // match with the type func(...)->...
    [<DataRow("MS3", "def func A(z:obj)->ind def pred Test() {is(A, func(y:obj)->ind)};", LiteralTrue)>] // true: func(y:obj)->ind matches signature A(obj)->ind
    [<DataRow("MS3a", "def func A(z:obj)->ind def pred Test() {dec ~x:obj; is(A(x), func(y:obj)->ind)};", LiteralFalse)>] // false: func(y:obj)->ind does not match value A(obj) 
    [<DataRow("MS3b", "def func A(z:obj)->ind def pred Test() {dec ~x:ind; is(A(x), func(y:obj)->ind)};", LiteralFalse)>] // false: func(y:obj)->ind does not match value A(ind) not matching A(obj)
    [<DataRow("MS3c", "def func A(z:ind)->ind def pred Test() {dec ~x:ind; is(A(x), func(y:obj)->ind)};", LiteralFalse)>] // false: func(y:obj)->ind does not match value A(ind) 
    [<DataRow("MS3d", "def func A(z:ind)->ind def pred Test() {is(A, func(y:obj)->ind)};", LiteralFalse)>] // false: func(y:obj)->ind does not match signature A(ind)->ind
    [<DataRow("MS3e", "ax A {true} def pred Test() {is(A, func(y:obj)->ind)};", LiteralFalse)>] // false: func(y:obj)->ind does not match signature A (axiom)
    [<DataRow("MS3f", "thm A {true} def pred Test() {is(A, func(y:obj)->ind)};", LiteralFalse)>] // false: func(y:obj)->ind does not match signature A (theorem)
    [<DataRow("MS3g", "lem A {true} def pred Test() {is(A, func(y:obj)->ind)};", LiteralFalse)>] // false: func(y:obj)->ind does not match signature A (lemma)
    [<DataRow("MS3h", "prop A {true} def pred Test() {is(A, func(y:obj)->ind)};", LiteralFalse)>] // false: func(y:obj)->ind does not match signature A (proposition)
    [<DataRow("MS3i", "conj A {true} def pred Test() {is(A, func(y:obj)->ind)};", LiteralFalse)>] // false: func(y:obj)->ind does not match signature A (conjecture)
    [<DataRow("MS3j", "cor A$1 {true} def pred Test() {is(A$1, func(y:obj)->ind)};", LiteralFalse)>] // false: func(y:obj)->ind does not match signature A$1 (corollary)
    [<DataRow("MS3k", "proof A$1 {1. |- trivial} def pred Test() {is(A$1, func(y:obj)->ind)};", LiteralFalse)>] // false: func(y:obj)->ind does not match signature A$1 (proof)
    [<DataRow("MS3l", "inf A {pre:true con:true} def pred Test() {is(A, func(y:obj)->ind)};", LiteralFalse)>] // false: func(y:obj)->ind does not match signature A (rule of inference)
    [<DataRow("MS3m", "def func A(z:obj)->func()->obj def pred Test() {is(A, func(y:obj)->ind)};", LiteralFalse)>] // false: func(y:obj)->ind does not match signature A(z:obj)->func()->obj (functional term)
    [<DataRow("MS3n", "ext A x@/\d+/ -> obj {dec ~y:obj; return y} def pred Test() {is(A, func(y:obj)->ind)};", LiteralFalse)>] // false: func(y:obj)->ind does not match signature A (extension)
    [<DataRow("MS3o", "def cl A def pred Test() {is(A, func(y:obj)->ind)};", LiteralFalse)>] // false: func(y:obj)->ind does not match signature A (class)
    // ... using properties of classes
    [<DataRow("MS3p1", "def cl A {intr prty pred X(x:obj) } def pred Test() {is(A.X, func(y:obj)->ind)};", LiteralFalse)>] // false: func(y:obj)->ind matches signature A.A(obj) 
    [<DataRow("MS3p2", "def cl A {intr prty pred X(x:ind) } def pred Test() {is(A.X, func(y:obj)->ind)};", LiteralFalse)>] // false: func(y:obj)->ind does not match signature A.X(ind)
    [<DataRow("MS3p3", "def cl A {intr prty pred X() } def pred Test() {is(A.X, func(y:obj)->ind)};", LiteralFalse)>] // false: func(y:obj)->ind match signature A.X()
    [<DataRow("MS3p4", "def cl A {intr prty func X()->obj } def pred Test() {is(A.X, func(y:obj)->ind)};", LiteralFalse)>] // false: func(y:obj)->ind does not match signature A.X()->obj
    [<DataRow("MS3p5", "def cl A {intr prty func X(x:obj)->obj } def pred Test() {is(A.X, func(y:obj)->obj)};", LiteralTrue)>] // true: func(y:obj)->ind matches signature A.X(obj)->obj
    [<DataRow("MS3p6", "def cl A {intr prty func X(x:ind)->obj } def pred Test() {is(A.X, func(y:obj)->ind)};", LiteralFalse)>] // false: func(y:obj)->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3p7", "def cl A {intr prty func X(x:obj)->ind } def pred Test() {dec ~a:obj; is(A.X(a), func(y:obj)->ind)};", LiteralFalse)>] // false: func(y:obj)->ind does not match by value A.X(obj) 
    // ... using properties of instances
    [<DataRow("MS3q1", "def cl A {intr prty pred X(x:obj) } def pred Test() {dec ~o:A o:=A(); is(o.X, func(y:obj)->ind)};", LiteralFalse)>] // false: func(y:obj)->ind matches signature A.A(obj) 
    [<DataRow("MS3q2", "def cl A {intr prty pred X(x:ind) } def pred Test() {dec ~o:A o:=A(); is(o.X, func(y:obj)->ind)};", LiteralFalse)>] // false: func(y:obj)->ind does not match signature A.X(ind)
    [<DataRow("MS3q3", "def cl A {intr prty pred X() } def pred Test() {dec ~o:A o:=A(); is(o.X, func(y:obj)->ind)};", LiteralFalse)>] // false: func(y:obj)->ind match signature A.X()
    [<DataRow("MS3q4", "def cl A {intr prty func X()->obj } def pred Test() {dec ~o:A o:=A(); is(o.X, func(y:obj)->ind)};", LiteralFalse)>] // false: func(y:obj)->ind does not match signature A.X()->obj
    [<DataRow("MS3q5", "def cl A {intr prty func X(x:obj)->obj } def pred Test() {dec ~o:A o:=A(); is(o.X, func(y:obj)->obj)};", LiteralTrue)>] // true: func(y:obj)->ind matches signature A.X(obj)->obj
    [<DataRow("MS3q6", "def cl A {intr prty func X(x:ind)->obj } def pred Test() {dec ~o:A o:=A(); is(o.X, func(y:obj)->ind)};", LiteralFalse)>] // false: func(y:obj)->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3q7", "def cl A {intr prty func X(x:obj)->ind } def pred Test() {dec ~a:obj ~o:A o:=A(); is(o.X(a), func(y:obj)->ind)};", LiteralFalse)>] // false: func(y:obj)->ind does not match by value A.X(obj) 
    // ... using properties of predicates
    [<DataRow("MS3r1", "def pred A() {intr prty pred X(x:obj) } def pred Test() {is(A.X, func(y:obj)->ind)};", LiteralFalse)>] // false: func(y:obj)->ind matches signature A.A(obj) 
    [<DataRow("MS3r2", "def pred A() {intr prty pred X(x:ind) } def pred Test() {is(A.X, func(y:obj)->ind)};", LiteralFalse)>] // false: func(y:obj)->ind does not match signature A.X(ind)
    [<DataRow("MS3r3", "def pred A() {intr prty pred X() } def pred Test() {is(A.X, func(y:obj)->ind)};", LiteralFalse)>] // false: func(y:obj)->ind match signature A.X()
    [<DataRow("MS3r4", "def pred A() {intr prty func X()->obj } def pred Test() {is(A.X, func(y:obj)->ind)};", LiteralFalse)>] // false: func(y:obj)->ind does not match signature A.X()->obj
    [<DataRow("MS3r5", "def pred A() {intr prty func X(x:obj)->obj } def pred Test() {is(A.X, func(y:obj)->obj)};", LiteralTrue)>] // true: func(y:obj)->ind matches signature A.X(obj)->obj
    [<DataRow("MS3r6", "def pred A() {intr prty func X(x:ind)->obj } def pred Test() {is(A.X, func(y:obj)->ind)};", LiteralFalse)>] // false: func(y:obj)->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3r7", "def pred A() {intr prty func X(x:obj)->ind } def pred Test() {dec ~a:obj; is(A.X(a), func(y:obj)->ind)};", LiteralFalse)>] // false: func(y:obj)->ind does not match by value A.X(obj) 
    // ... using properties of functional terms
    [<DataRow("MS3s1", "def func A()->obj {intr prty pred X(x:obj) } def pred Test() {is(A.X, func(y:obj)->ind)};", LiteralFalse)>] // false: func(y:obj)->ind matches signature A.A(obj) 
    [<DataRow("MS3s2", "def func A()->obj {intr prty pred X(x:ind) } def pred Test() {is(A.X, func(y:obj)->ind)};", LiteralFalse)>] // false: func(y:obj)->ind does not match signature A.X(ind)
    [<DataRow("MS3s3", "def func A()->obj {intr prty pred X() } def pred Test() {is(A.X, func(y:obj)->ind)};", LiteralFalse)>] // false: func(y:obj)->ind match signature A.X()
    [<DataRow("MS3s4", "def func A()->obj {intr prty func X()->obj } def pred Test() {is(A.X, func(y:obj)->ind)};", LiteralFalse)>] // false: func(y:obj)->ind does not match signature A.X()->obj
    [<DataRow("MS3s5", "def func A()->obj {intr prty func X(x:obj)->obj } def pred Test() {is(A.X, func(y:obj)->obj)};", LiteralTrue)>] // true: func(y:obj)->ind matches signature A.X(obj)->obj
    [<DataRow("MS3s6", "def func A()->obj {intr prty func X(x:ind)->obj } def pred Test() {is(A.X, func(y:obj)->ind)};", LiteralFalse)>] // false: func(y:obj)->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3s7", "def func A()->obj {intr prty func X(x:obj)->ind } def pred Test() {dec ~a:obj; is(A.X(a), func(y:obj)->ind)};", LiteralFalse)>] // false: func(y:obj)->ind does not match by value A.X(obj) 
    
    // match with the type func()->...
    [<DataRow("MS3_", "def func A()->ind def pred Test() {is(A, func()->ind)};", LiteralTrue)>] // true: func()->ind matches signature A()->ind
    [<DataRow("MS3a_", "def func A()->ind def pred Test() {dec ~x:obj; is(A(x), func()->ind)};", LiteralFalse)>] // false: func()->ind does not match value A(obj) 
    [<DataRow("MS3b_", "def func A()->ind def pred Test() {dec ~x:ind; is(A(x), func()->ind)};", LiteralFalse)>] // false: func()->ind does not match value A(ind) not matching A(obj)
    [<DataRow("MS3c_", "def func A()->ind def pred Test() {dec ~x:ind; is(A(x), func()->ind)};", LiteralFalse)>] // false: func()->ind does not match value A(ind) 
    [<DataRow("MS3d_", "def func A(z:ind)->ind def pred Test() {is(A, func()->ind)};", LiteralFalse)>] // false: func()->ind does not match signature A(ind)->ind
    [<DataRow("MS3e_", "ax A {true} def pred Test() {is(A, func()->ind)};", LiteralFalse)>] // false: func()->ind does not match signature A (axiom)
    [<DataRow("MS3f_", "thm A {true} def pred Test() {is(A, func()->ind)};", LiteralFalse)>] // false: func()->ind does not match signature A (theorem)
    [<DataRow("MS3g_", "lem A {true} def pred Test() {is(A, func()->ind)};", LiteralFalse)>] // false: func()->ind does not match signature A (lemma)
    [<DataRow("MS3h_", "prop A {true} def pred Test() {is(A, func()->ind)};", LiteralFalse)>] // false: func()->ind does not match signature A (proposition)
    [<DataRow("MS3i_", "conj A {true} def pred Test() {is(A, func()->ind)};", LiteralFalse)>] // false: func()->ind does not match signature A (conjecture)
    [<DataRow("MS3j_", "cor A$1 {true} def pred Test() {is(A$1, func()->ind)};", LiteralFalse)>] // false: func()->ind does not match signature A$1 (corollary)
    [<DataRow("MS3k_", "proof A$1 {1. |- trivial} def pred Test() {is(A$1, func()->ind)};", LiteralFalse)>] // false: func()->ind does not match signature A$1 (proof)
    [<DataRow("MS3l_", "inf A {pre:true con:true} def pred Test() {is(A, func()->ind)};", LiteralFalse)>] // false: func()->ind does not match signature A (rule of inference)
    [<DataRow("MS3m_", "def func A()->func()->obj def pred Test() {is(A, func()->ind)};", LiteralFalse)>] // false: func()->ind does not match signature A(z:obj)->func()->obj (functional term)
    [<DataRow("MS3n_", "ext A x@/\d+/ -> obj {dec ~y:obj; return y} def pred Test() {is(A, func(y:obj)->ind)};", LiteralFalse)>] // false: func(y:obj)->ind does not match signature A (extension)
    [<DataRow("MS3o_", "def cl A def pred Test() {is(A, func()->ind)};", LiteralFalse)>] // false: func()->ind does not match signature A (class)
    // ... using properties of classes
    [<DataRow("MS3p_1", "def cl A {intr prty pred X(x:obj) } def pred Test() {is(A.X, func()->ind)};", LiteralFalse)>] // false: func()->ind matches signature A.A(obj) 
    [<DataRow("MS3p_2", "def cl A {intr prty pred X(x:ind) } def pred Test() {is(A.X, func()->ind)};", LiteralFalse)>] // false: func()->ind does not match signature A.X(ind)
    [<DataRow("MS3p_3", "def cl A {intr prty pred X() } def pred Test() {is(A.X, func()->ind)};", LiteralFalse)>] // false: func()->ind match signature A.X()
    [<DataRow("MS3p_4", "def cl A {intr prty func X()->obj } def pred Test() {is(A.X, func()->ind)};", LiteralFalse)>] // false: func()->ind does not match signature A.X()->obj
    [<DataRow("MS3p_5", "def cl A {intr prty func X()->obj } def pred Test() {is(A.X, func()->obj)};", LiteralTrue)>] // true: func()->ind matches signature A.X(obj)->obj
    [<DataRow("MS3p_6", "def cl A {intr prty func X(x:ind)->obj } def pred Test() {is(A.X, func()->ind)};", LiteralFalse)>] // false: func()->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3p_7", "def cl A {intr prty func X(x:obj)->ind } def pred Test() {dec ~a:obj; is(A.X(a), func()->ind)};", LiteralFalse)>] // false: func()->ind does not match by value A.X(obj) 
    // ... using properties of instances
    [<DataRow("MS3q_1", "def cl A {intr prty pred X(x:obj) } def pred Test() {dec ~o:A o:=A(); is(o.X, func()->ind)};", LiteralFalse)>] // false: func()->ind matches signature A.A(obj) 
    [<DataRow("MS3q_2", "def cl A {intr prty pred X(x:ind) } def pred Test() {dec ~o:A o:=A(); is(o.X, func()->ind)};", LiteralFalse)>] // false: func()->ind does not match signature A.X(ind)
    [<DataRow("MS3q_3", "def cl A {intr prty pred X() } def pred Test() {dec ~o:A o:=A(); is(o.X, func()->ind)};", LiteralFalse)>] // false: func()->ind match signature A.X()
    [<DataRow("MS3q_4", "def cl A {intr prty func X()->obj } def pred Test() {dec ~o:A o:=A(); is(o.X, func()->ind)};", LiteralFalse)>] // false: func()->ind does not match signature A.X()->obj
    [<DataRow("MS3q_5", "def cl A {intr prty func X()->obj } def pred Test() {dec ~o:A o:=A(); is(o.X, func()->obj)};", LiteralTrue)>] // true: func()->ind matches signature A.X(obj)->obj
    [<DataRow("MS3q_6", "def cl A {intr prty func X(x:ind)->obj } def pred Test() {dec ~o:A o:=A(); is(o.X, func()->ind)};", LiteralFalse)>] // false: func()->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3q_7", "def cl A {intr prty func X(x:obj)->ind } def pred Test() {dec ~a:obj ~o:A o:=A(); is(o.X(a), func()->ind)};", LiteralFalse)>] // false: func()->ind does not match by value A.X(obj) 
    // ... using properties of predicates
    [<DataRow("MS3r_1", "def pred A() {intr prty pred X(x:obj) } def pred Test() {is(A.X, func()->ind)};", LiteralFalse)>] // false: func()->ind matches signature A.A(obj) 
    [<DataRow("MS3r_2", "def pred A() {intr prty pred X(x:ind) } def pred Test() {is(A.X, func()->ind)};", LiteralFalse)>] // false: func()->ind does not match signature A.X(ind)
    [<DataRow("MS3r_3", "def pred A() {intr prty pred X() } def pred Test() {is(A.X, func()->ind)};", LiteralFalse)>] // false: func()->ind match signature A.X()
    [<DataRow("MS3r_4", "def pred A() {intr prty func X()->obj } def pred Test() {is(A.X, func()->ind)};", LiteralFalse)>] // false: func()->ind does not match signature A.X()->obj
    [<DataRow("MS3r_5", "def pred A() {intr prty func X()->obj } def pred Test() {is(A.X, func()->obj)};", LiteralTrue)>] // true: func()->ind matches signature A.X(obj)->obj
    [<DataRow("MS3r_6", "def pred A() {intr prty func X(x:ind)->obj } def pred Test() {is(A.X, func()->ind)};", LiteralFalse)>] // false: func()->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3r_7", "def pred A() {intr prty func X(x:obj)->ind } def pred Test() {dec ~a:obj; is(A.X(a), func()->ind)};", LiteralFalse)>] // false: func()->ind does not match by value A.X(obj) 
    // ... using properties of functional terms
    [<DataRow("MS3s_1", "def func A()->obj {intr prty pred X(x:obj) } def pred Test() {is(A.X, func()->ind)};", LiteralFalse)>] // false: func()->ind matches signature A.A(obj) 
    [<DataRow("MS3s_2", "def func A()->obj {intr prty pred X(x:ind) } def pred Test() {is(A.X, func()->ind)};", LiteralFalse)>] // false: func()->ind does not match signature A.X(ind)
    [<DataRow("MS3s_3", "def func A()->obj {intr prty pred X() } def pred Test() {is(A.X, func()->ind)};", LiteralFalse)>] // false: func()->ind match signature A.X()
    [<DataRow("MS3s_4", "def func A()->obj {intr prty func X()->obj } def pred Test() {is(A.X, func()->ind)};", LiteralFalse)>] // false: func()->ind does not match signature A.X()->obj
    [<DataRow("MS3s_5", "def func A()->obj {intr prty func X()->obj } def pred Test() {is(A.X, func()->obj)};", LiteralTrue)>] // true: func()->ind matches signature A.X(obj)->obj
    [<DataRow("MS3s_6", "def func A()->obj {intr prty func X(x:ind)->obj } def pred Test() {is(A.X, func()->ind)};", LiteralFalse)>] // false: func()->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3s_7", "def func A()->obj {intr prty func X(x:obj)->ind } def pred Test() {dec ~a:obj; is(A.X(a), func()->ind)};", LiteralFalse)>] // false: func()->ind does not match by value A.X(obj) 

    // match with the type func
    [<DataRow("MS4", "def func A(z:obj)->ind def pred Test() {is(A, func)};", LiteralTrue)>] // true: func matches signature A(obj)->ind
    [<DataRow("MS4a", "def func A(z:obj)->ind def pred Test() {dec ~x:obj; is(A(x), func)};", LiteralFalse)>] // false: func does not match value A(obj) 
    [<DataRow("MS4b", "def func A(z:obj)->ind def pred Test() {dec ~x:ind; is(A(x), func)};", LiteralFalse)>] // false: func does not match value A(ind) not matching A(obj)
    [<DataRow("MS4c", "def func A(z:ind)->ind def pred Test() {dec ~x:ind; is(A(x), func)};", LiteralFalse)>] // false: func does not match value A(ind) 
    [<DataRow("MS4d", "def func A(z:ind)->func(a:obj)->ind def pred Test() {is(A, func)};", LiteralTrue)>] // true: func matches signature A(ind)->func(obj)->ind
    [<DataRow("MS4e", "ax A {true} def pred Test() {is(A, func)};", LiteralFalse)>] // false: func does not matche signature A (axiom)
    [<DataRow("MS4f", "thm A {true} def pred Test() {is(A, func)};", LiteralFalse)>] // false: func does not matche signature A (theorem)
    [<DataRow("MS4g", "lem A {true} def pred Test() {is(A, func)};", LiteralFalse)>] // false: func does not matche signature A (lemma)
    [<DataRow("MS4h", "prop A {true} def pred Test() {is(A, func)};", LiteralFalse)>] // false: func does not matche signature A (proposition)
    [<DataRow("MS4i", "conj A {true} def pred Test() {is(A, func)};", LiteralFalse)>] // false: func does not matche signature A (conjecture)
    [<DataRow("MS4j", "cor A$1 {true} def pred Test() {is(A$1, func)};", LiteralFalse)>] // false: func does not matche signature A$1 (corollary)
    [<DataRow("MS4k", "proof A$1 {1. |- trivial} def pred Test() {is(A$1, func)};", LiteralFalse)>] // false: func does not matche signature A$1 (proof)
    [<DataRow("MS4l", "inf A {pre:true con:true} def pred Test() {is(A, func)};", LiteralFalse)>] // false: func does not matche signature A (rule of inference)
    [<DataRow("MS4m", "def func A(z:obj)->func()->obj def pred Test() {is(A, func)};", LiteralTrue)>] // true: func matches signature A (functional term)
    [<DataRow("MS4n", "ext A x@/\d+/ -> obj {dec ~y:obj; return y} def pred Test() {is(A, func)};", LiteralFalse)>] // false: func does not match signature A (extension)
    [<DataRow("MS4o", "def cl A def pred Test() {is(A, func)};", LiteralFalse)>] // false: func does not match signature A (class)
    // ... using properties of classes
    [<DataRow("MS4p1", "def cl A {intr prty pred X(x:obj) } def pred Test() {is(A.X, func)};", LiteralFalse)>] // false: func matches signature A.A(obj) 
    [<DataRow("MS4p2", "def cl A {intr prty pred X(x:ind) } def pred Test() {is(A.X, func)};", LiteralFalse)>] // false: func does not match signature A.X(ind)
    [<DataRow("MS4p3", "def cl A {intr prty pred X() } def pred Test() {is(A.X, func)};", LiteralFalse)>] // false: func match signature A.X()
    [<DataRow("MS4p4", "def cl A {intr prty func X()->obj } def pred Test() {is(A.X, func)};", LiteralTrue)>] // true: func matches signature A.X()->obj
    [<DataRow("MS4p5", "def cl A {intr prty func X()->obj } def pred Test() {is(A.X, func()->obj)};", LiteralTrue)>] // true: func matches signature A.X(obj)->obj
    [<DataRow("MS4p6", "def cl A {intr prty func X(x:ind)->obj } def pred Test() {is(A.X, func)};", LiteralTrue)>] // true: func matches signature A.X(ind)->obj
    [<DataRow("MS4p7", "def cl A {intr prty func X(x:obj)->ind } def pred Test() {dec ~a:obj; is(A.X(a), func)};", LiteralFalse)>] // false: func does not match by value A.X(obj) 
    // ... using properties of instances
    [<DataRow("MS4q1", "def cl A {intr prty pred X(x:obj) } def pred Test() {dec ~o:A o:=A(); is(o.X, func)};", LiteralFalse)>] // false: func matches signature A.A(obj) 
    [<DataRow("MS4q2", "def cl A {intr prty pred X(x:ind) } def pred Test() {dec ~o:A o:=A(); is(o.X, func)};", LiteralFalse)>] // false: func does not match signature A.X(ind)
    [<DataRow("MS4q3", "def cl A {intr prty pred X() } def pred Test() {dec ~o:A o:=A(); is(o.X, func)};", LiteralFalse)>] // false: func match signature A.X()
    [<DataRow("MS4q4", "def cl A {intr prty func X()->obj } def pred Test() {dec ~o:A o:=A(); is(o.X, func)};", LiteralTrue)>] // true: func matches signature A.X()->obj
    [<DataRow("MS4q5", "def cl A {intr prty func X()->obj } def pred Test() {dec ~o:A o:=A(); is(o.X, func()->obj)};", LiteralTrue)>] // true: func matches signature A.X(obj)->obj
    [<DataRow("MS4q6", "def cl A {intr prty func X(x:ind)->obj } def pred Test() {dec ~o:A o:=A(); is(o.X, func)};", LiteralTrue)>] // true: func matches signature A.X(ind)->obj
    [<DataRow("MS4q7", "def cl A {intr prty func X(x:obj)->ind } def pred Test() {dec ~a:obj ~o:A o:=A(); is(o.X(a), func)};", LiteralFalse)>] // false: func does not match by value A.X(obj) 
    // ... using properties of predicates
    [<DataRow("MS4r1", "def pred A() {intr prty pred X(x:obj) } def pred Test() {is(A.X, func)};", LiteralFalse)>] // false: func matches signature A.A(obj) 
    [<DataRow("MS4r2", "def pred A() {intr prty pred X(x:ind) } def pred Test() {is(A.X, func)};", LiteralFalse)>] // false: func does not match signature A.X(ind)
    [<DataRow("MS4r3", "def pred A() {intr prty pred X() } def pred Test() {is(A.X, func)};", LiteralFalse)>] // false: func match signature A.X()
    [<DataRow("MS4r4", "def pred A() {intr prty func X()->obj } def pred Test() {is(A.X, func)};", LiteralTrue)>] // true: func matches signature A.X()->obj
    [<DataRow("MS4r5", "def pred A() {intr prty func X()->obj } def pred Test() {is(A.X, func()->obj)};", LiteralTrue)>] // true: func matches signature A.X(obj)->obj
    [<DataRow("MS4r6", "def pred A() {intr prty func X(x:ind)->obj } def pred Test() {is(A.X, func)};", LiteralTrue)>] // true: func matches signature A.X(ind)->obj
    [<DataRow("MS4r7", "def pred A() {intr prty func X(x:obj)->ind } def pred Test() {dec ~a:obj; is(A.X(a), func)};", LiteralFalse)>] // false: func does not match by value A.X(obj) 
    // ... using properties of functional terms
    [<DataRow("MS4s1", "def func A()->obj {intr prty pred X(x:obj) } def pred Test() {is(A.X, func)};", LiteralFalse)>] // false: func matches signature A.A(obj) 
    [<DataRow("MS4s2", "def func A()->obj {intr prty pred X(x:ind) } def pred Test() {is(A.X, func)};", LiteralFalse)>] // false: func does not match signature A.X(ind)
    [<DataRow("MS4s3", "def func A()->obj {intr prty pred X() } def pred Test() {is(A.X, func)};", LiteralFalse)>] // false: func match signature A.X()
    [<DataRow("MS4s4", "def func A()->obj {intr prty func X()->obj } def pred Test() {is(A.X, func)};", LiteralTrue)>] // true: func matches signature A.X()->obj
    [<DataRow("MS4s5", "def func A()->obj {intr prty func X()->obj } def pred Test() {is(A.X, func()->obj)};", LiteralTrue)>] // true: func matches signature A.X(obj)->obj
    [<DataRow("MS4s6", "def func A()->obj {intr prty func X(x:ind)->obj } def pred Test() {is(A.X, func)};", LiteralTrue)>] // true: func matches signature A.X(ind)->obj
    [<DataRow("MS4s7", "def func A()->obj {intr prty func X(x:obj)->ind } def pred Test() {dec ~a:obj; is(A.X(a), func)};", LiteralFalse)>] // false: func does not match by value A.X(obj) 

    [<TestMethod>]
    member this.TestExpressionEvaluationIsOperandEvenMore(no:string, fplCode, expected: string) =
        ad.Clear()
        let filename = "TestExpressionEvaluationIsOperand"
        let stOption = prepareFplCode (filename + ".fpl", fplCode, false)
        prepareFplCode (filename, "", false) |> ignore

        match stOption with
        | Some st ->
            let r = st.Root
            let theory = r.Scope[filename]
            let pr1 = theory.Scope.Values |> Seq.filter (fun fv -> fv.FplId = "Test") |> Seq.head         

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