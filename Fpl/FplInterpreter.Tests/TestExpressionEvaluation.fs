namespace FplInterpreter.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open ErrDiagnostics
open FplInterpreterTypes
open CommonTestHelpers
open System.Text

[<TestClass>]
type TestExpressionEvaluation() =
    let evalTree (fplValue: FplValue) t =
        let rec evalTreeRec (fv: FplValue) (sb: StringBuilder) =
            if t then 
                sb.Append($"{fv.FplId}") |> ignore
            else
                sb.Append($"{fv.ReprId}") |> ignore
            if fv.ValueList.Count = 1 then
                sb.Append("{") |> ignore
                evalTreeRec fv.ValueList[0] sb
                sb.Append("} ") |> ignore
            if fv.ValueList.Count > 0 then
                sb.Append("{") |> ignore
                let mutable i = 0
                fv.ValueList |> Seq.iter (fun child -> 
                    i <- i + 1
                    evalTreeRec child sb
                    if i < fv.ValueList.Count then 
                        sb.Append(", ") |> ignore 
                    )
                sb.Append("}") |> ignore

        let sbuilder = StringBuilder()
        evalTreeRec fplValue sbuilder
        sbuilder.ToString()

    let evalTreeFplId (fplValue: FplValue) = evalTree fplValue true
    let evalTreeFplRepresentation (fplValue: FplValue) = evalTree fplValue false

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

    [<DataRow("def pred T() { and(true,true,true) };", "true{true, true, true}")>]
    [<DataRow("def pred T() { and(true,false,true) };", "false{true, false, true}")>]
    [<DataRow("def pred T() { and(true,true,false) };", "false{true, true, false}")>]
    [<DataRow("def pred T() { and(false,true,true) };", "false{false, true, true}")>]
    [<DataRow("def pred T() { and(true,x,true) };", "undetermined{true, undef, true}")>]
    [<DataRow("def pred T() { and(true,true,x) };", "undetermined{true, true, undef}")>]
    [<DataRow("def pred T() { and(x,true,true) };", "undetermined{undef, true, true}")>]
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

    [<DataRow("def pred T() { or(false,false,false) };", "false{false, false, false}")>]
    [<DataRow("def pred T() { or(false,true,false) };", "true{false, true, false}")>]
    [<DataRow("def pred T() { or(false,false,true) };", "true{false, false, true}")>]
    [<DataRow("def pred T() { or(true,false,false) };", "true{true, false, false}")>]
    [<DataRow("def pred T() { or(false,x,false) };", "undetermined{false, undef, false}")>]
    [<DataRow("def pred T() { or(false,false,x) };", "undetermined{false, false, undef}")>]
    [<DataRow("def pred T() { or(x,false,false) };", "undetermined{undef, false, false}")>]
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

    [<DataRow("def pred T() { xor(false,false,false) };", "false{false, false, false}")>]
    [<DataRow("def pred T() { xor(false,false,true) };", "true{false, false, true}")>]
    [<DataRow("def pred T() { xor(false,true,false) };", "true{false, true, false}")>]
    [<DataRow("def pred T() { xor(true,false,false) };", "true{true, false, false}")>]
    [<DataRow("def pred T() { xor(false,true,true) };", "false{false, true, true}")>]
    [<DataRow("def pred T() { xor(true,false,true) };", "false{true, false, true}")>]
    [<DataRow("def pred T() { xor(true,true,false) };", "false{true, true, false}")>]
    [<DataRow("def pred T() { xor(true,true,true) };", "true{true, true, true}")>]
    [<DataRow("def pred T() { xor(false,x,false) };", "undetermined{false, undef, false}")>]
    [<DataRow("def pred T() { xor(false,false,x) };", "undetermined{false, false, undef}")>]
    [<DataRow("def pred T() { xor(x,false,false) };", "undetermined{undef, false, false}")>]
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

    [<DataRow("def pred T() { impl(true,true) };", "true{true, true}")>]
    [<DataRow("def pred T() { impl(true,false) };", "false{true, false}")>]
    [<DataRow("def pred T() { impl(false,true) };", "true{false, true}")>]
    [<DataRow("def pred T() { impl(false,false) };", "true{false, false}")>]
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

    [<DataRow("def pred T() { iif(true,true) };", "true{true, true}")>]
    [<DataRow("def pred T() { iif(true,false) };", "false{true, false}")>]
    [<DataRow("def pred T() { iif(false,true) };", "false{false, true}")>]
    [<DataRow("def pred T() { iif(false,false) };", "true{false, false}")>]
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