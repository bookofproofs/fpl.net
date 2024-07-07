namespace FplInterpreter.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
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
                sb.Append($"{fv.FplRepresentation}") |> ignore
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

    [<DataRow("def pred T() { true };", "PredRepr True")>]
    [<DataRow("def pred T() { false };", "PredRepr False")>]
    [<DataRow("def pred T() { undef };", "Undef")>]
    [<DataRow("def pred T() { intr };", "PredRepr Undetermined")>]
    [<TestMethod>]
    member this.TestExpressionEvaluationConstants(fplCode, expected: string) =
        FplParser.parserDiagnostics.Clear()
        let filename = "TestExpressionEvaluationConstants.fpl"
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
            printfn "%s" expr.Name
            printfn "%s" (evalTreeFplId(expr))
            Assert.AreEqual<string>(expected, actual)
        | None -> Assert.IsTrue(false)

    [<DataRow("def pred T() { and(true,true,true) };", "PredRepr True{PredRepr True, PredRepr True, PredRepr True}")>]
    [<DataRow("def pred T() { and(true,false,true) };", "PredRepr False{PredRepr True, PredRepr False, PredRepr True}")>]
    [<DataRow("def pred T() { and(true,true,false) };", "PredRepr False{PredRepr True, PredRepr True, PredRepr False}")>]
    [<DataRow("def pred T() { and(false,true,true) };", "PredRepr False{PredRepr False, PredRepr True, PredRepr True}")>]
    [<DataRow("def pred T() { and(true,x,true) };", "PredRepr Undetermined{PredRepr True, Undef, PredRepr True}")>]
    [<DataRow("def pred T() { and(true,true,x) };", "PredRepr Undetermined{PredRepr True, PredRepr True, Undef}")>]
    [<DataRow("def pred T() { and(x,true,true) };", "PredRepr Undetermined{Undef, PredRepr True, PredRepr True}")>]
    [<TestMethod>]
    member this.TestExpressionEvaluationConjunction(fplCode, expected: string) =
        FplParser.parserDiagnostics.Clear()
        let filename = "TestExpressionEvaluationConjunction.fpl"
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
            printfn "%s" expr.Name
            printfn "%s" (evalTreeFplId(expr))
            Assert.AreEqual<string>(expected, actual)
        | None -> Assert.IsTrue(false)

    [<DataRow("def pred T() { impl(true,true) };", "PredRepr True{PredRepr True, PredRepr True}")>]
    [<DataRow("def pred T() { impl(true,false) };", "PredRepr False{PredRepr True, PredRepr False}")>]
    [<DataRow("def pred T() { impl(false,true) };", "PredRepr True{PredRepr False, PredRepr True}")>]
    [<DataRow("def pred T() { impl(false,false) };", "PredRepr True{PredRepr False, PredRepr False}")>]
    [<TestMethod>]
    member this.TestExpressionEvaluationImplication(fplCode, expected: string) =
        FplParser.parserDiagnostics.Clear()
        let filename = "TestExpressionEvaluationImplication.fpl"
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
            printfn "%s" expr.Name
            printfn "%s" (evalTreeFplId(expr))
            Assert.AreEqual<string>(expected, actual)
        | None -> Assert.IsTrue(false)

    [<DataRow("def pred T() { iif(true,true) };", "PredRepr True{PredRepr True, PredRepr True}")>]
    [<DataRow("def pred T() { iif(true,false) };", "PredRepr False{PredRepr True, PredRepr False}")>]
    [<DataRow("def pred T() { iif(false,true) };", "PredRepr False{PredRepr False, PredRepr True}")>]
    [<DataRow("def pred T() { iif(false,false) };", "PredRepr True{PredRepr False, PredRepr False}")>]
    [<TestMethod>]
    member this.TestExpressionEvaluationEquivalence(fplCode, expected: string) =
        FplParser.parserDiagnostics.Clear()
        let filename = "TestExpressionEvaluationEquivalence.fpl"
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
            printfn "%s" expr.Name
            printfn "%s" (evalTreeFplId(expr))
            Assert.AreEqual<string>(expected, actual)
        | None -> Assert.IsTrue(false)