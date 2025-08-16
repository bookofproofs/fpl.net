namespace FplInterpreter.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open ErrDiagnostics
open FplGrammarCommons
open FplInterpreterTypes
open CommonTestHelpers

[<TestClass>]
type TestExpressionEvaluationVariables() =
    let evalTreeFplId (fplValue: FplValue) = fplValue.Type(SignatureType.Name)
    let evalTreeFplRepresentation (fplValue: FplValue) = fplValue.Represent()

    [<DataRow("def pred T() { dec ~x:pred x:=true; x };", literalTrue)>]
    [<DataRow("def pred T() { dec ~x:pred x:=false; x };", literalFalse)>]
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
            let actual = pr1.Represent()
            printfn "expected: %s" expected 
            printfn "actual  : %s" actual
            printfn "%s" (pr1.Type(SignatureType.Mixed))
            printfn "%s" (evalTreeFplId(pr1))
            Assert.AreEqual<string>(expected, actual)
        | None -> Assert.IsTrue(false)

    [<DataRow("def pred T() { dec ~x,y,z:pred x:=true y:=true z:=true; and(and(x,y),z) };", literalTrue)>]
    [<DataRow("def pred T() { dec ~x,y,z:pred x:=true y:=false z:=true; and(and(x,y),z) };", literalFalse)>]
    [<DataRow("def pred T() { dec ~x,y,z:pred x:=true y:=true z:=false; and(and(x,y),z) };", literalFalse)>]
    [<DataRow("def pred T() { dec ~x,y,z:pred x:=false y:=true z:=true; and(and(x,y),z) };", literalFalse)>]
    [<DataRow("def pred T() { dec ~x,y,z:pred x:=true z:=true; and(x,and(y,z)) };", literalUndetermined)>]
    [<DataRow("def pred T() { dec ~x,y,z:pred x:=true y:=true; and(x,and(y,z)) };", literalUndetermined)>]
    [<DataRow("def pred T() { dec ~x,y,z:pred y:=true z:=true; and(x,and(y,z)) };", literalUndetermined)>]
    [<DataRow("def pred T() { dec ~x,y,z:pred x:=false z:=true; and(x,and(y,z)) };", literalFalse)>]
    [<DataRow("def pred T() { dec ~x,y,z:pred x:=false y:=true; and(x,and(y,z)) };", literalFalse)>]
    [<DataRow("def pred T() { dec ~x,y,z:pred y:=false z:=true; and(x,and(y,z)) };", literalFalse)>]
    [<DataRow("def pred T() { dec ~x,y,z:pred y:=true z:=false; and(x,and(y,z)) };", literalFalse)>]
    [<DataRow("def pred T() { dec ~x,y:pred x:=true y:=true; and(x,y) };", literalTrue)>]
    [<DataRow("def pred T() { dec ~x,y:pred x:=true y:=false; and(x,y) };", literalFalse)>]
    [<DataRow("def pred T() { dec ~x,y:pred x:=false y:=true; and(x,y) };", literalFalse)>]
    [<DataRow("def pred T() { dec ~x,y:pred x:=false y:=false; and(x,y) };", literalFalse)>]
    [<TestMethod>]
    member this.TestExpressionEvaluationConjunction(fplCode, expected: string) =
        ad.Clear()
        let filename = "TestExpressionEvaluationConjunction1"
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
            printfn "%s" (pr1.Type(SignatureType.Mixed))
            printfn "%s" (evalTreeFplId(pr1))
            Assert.AreEqual<string>(expected, actual)
        | None -> Assert.IsTrue(false)

    [<DataRow("def pred T() { dec ~x:pred x:=true; not x };", literalFalse)>]
    [<DataRow("def pred T() { dec ~x:pred x:=true; not (x) };", literalFalse)>]
    [<DataRow("def pred T() { dec ~x:pred x:=true; not ((x)) };", literalFalse)>]
    [<DataRow("def pred T() { dec ~x:pred x:=false; not x };", literalTrue)>]
    [<DataRow("def pred T() { dec ~x:pred x:=false; not (x) };", literalTrue)>]
    [<DataRow("def pred T() { dec ~x:pred x:=false; not ((x)) };", literalTrue)>]
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
            printfn "%s" (pr1.Type(SignatureType.Mixed))
            printfn "%s" (evalTreeFplId(pr1))
            Assert.AreEqual<string>(expected, actual)
        | None -> Assert.IsTrue(false)

    [<DataRow("def pred T() { dec ~x,y,z:pred x:=false y:=false z:=false; or(x,or(y,z)) };", literalFalse)>]
    [<DataRow("def pred T() { dec ~x,y,z:pred x:=false y:=true z:=false; or(x,or(y,z)) };", literalTrue)>]
    [<DataRow("def pred T() { dec ~x,y,z:pred x:=false y:=false z:=true; or(or(x,y),z) };", literalTrue)>]
    [<DataRow("def pred T() { dec ~x,y,z:pred x:=true y:=false z:=false; or(or(x,y),z) };", literalTrue)>]
    [<DataRow("def pred T() { dec ~x,y,z:pred x:=false z:=false; or(x,or(y,z)) };", literalUndetermined)>]
    [<DataRow("def pred T() { dec ~x,y,z:pred x:=false y:=false; or(or(x,y),z) };", literalUndetermined)>]
    [<DataRow("def pred T() { dec ~x,y,z:pred y:=false z:=false; or(x,or(y,z)) };", literalUndetermined)>]
    [<DataRow("def pred T() { dec ~x,y,z:pred x:=false z:=true; or(x,or(y,z)) };", literalTrue)>]
    [<DataRow("def pred T() { dec ~x,y,z:pred x:=false y:=true; or(x,or(y,z)) };", literalTrue)>]
    [<DataRow("def pred T() { dec ~x,y,z:pred y:=false z:=true; or(x,or(y,z)) };", literalTrue)>]
    [<DataRow("def pred T() { dec ~x,y,z:pred y:=true z:=false; or(x,or(y,z)) };", literalTrue)>]
    [<DataRow("def pred T() { dec ~x,y:pred x:=true y:=true; or(x,y) };", literalTrue)>]
    [<DataRow("def pred T() { dec ~x,y:pred x:=true y:=false; or(x,y) };", literalTrue)>]
    [<DataRow("def pred T() { dec ~x,y:pred x:=false y:=true; or(x,y) };", literalTrue)>]
    [<DataRow("def pred T() { dec ~x,y:pred x:=false y:=false; or(x,y) };", literalFalse)>]
    [<TestMethod>]
    member this.TestExpressionEvaluationDisjunction(fplCode, expected: string) =
        ad.Clear()
        let filename = $"TestExpressionEvaluationDisjunction"
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
            printfn "%s" (pr1.Type(SignatureType.Mixed))
            printfn "%s" (evalTreeFplId(pr1))
            Assert.AreEqual<string>(expected, actual)
        | None -> Assert.IsTrue(false)

    [<DataRow("def pred T() { dec ~x,y,z:pred x:=false y:=false z:=false; xor(x,xor(y,z)) };", literalFalse)>]
    [<DataRow("def pred T() { dec ~x,y,z:pred x:=false y:=false z:=true; xor(xor(x,y),z) };", literalTrue)>]
    [<DataRow("def pred T() { dec ~x,y,z:pred x:=false y:=true z:=false; xor(xor(x,y),z) };", literalTrue)>]
    [<DataRow("def pred T() { dec ~x,y,z:pred x:=true y:=false z:=false; xor(xor(x,y),z) };", literalTrue)>]
    [<DataRow("def pred T() { dec ~x,y,z:pred x:=false y:=true z:=true; xor(x,xor(y,z)) };", literalFalse)>]
    [<DataRow("def pred T() { dec ~x,y,z:pred x:=true y:=false z:=true; xor(xor(x,y),z) };", literalFalse)>]
    [<DataRow("def pred T() { dec ~x,y,z:pred x:=true y:=true z:=false; xor(x,xor(y,z)) };", literalFalse)>]
    [<DataRow("def pred T() { dec ~x,y,z:pred x:=true y:=true z:=true; xor(x,xor(y,z)) };", literalTrue)>]
    [<DataRow("def pred T() { dec ~x,y,z:pred x:=false y:=true; xor(xor(x,y),z) };", literalUndetermined)>]
    [<DataRow("def pred T() { dec ~x,y,z:pred y:=false z:=true; xor(x,xor(y,z)) };", literalUndetermined)>]
    [<DataRow("def pred T() { dec ~x,y,z:pred x:=true z:=false; xor(x,xor(y,z)) };", literalUndetermined)>]
    [<DataRow("def pred T() { dec ~x,y:pred x:=true y:=true; xor(x,y) };", literalFalse)>]
    [<DataRow("def pred T() { dec ~x,y:pred x:=true y:=false; xor(x,y) };", literalTrue)>]
    [<DataRow("def pred T() { dec ~x,y:pred x:=false y:=true; xor(x,y) };", literalTrue)>]
    [<DataRow("def pred T() { dec ~x,y:pred x:=false y:=false; xor(x,y) };", literalFalse)>]
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
            printfn "%s" (pr1.Type(SignatureType.Mixed))
            printfn "%s" (evalTreeFplId(pr1))
            Assert.AreEqual<string>(expected, actual)
        | None -> Assert.IsTrue(false)

    [<DataRow("def pred T() { dec ~x,y:pred x:=true y:=true; impl(x,y) };", literalTrue)>]
    [<DataRow("def pred T() { dec ~x,y:pred x:=true y:=false; impl(x,y) };", literalFalse)>]
    [<DataRow("def pred T() { dec ~x,y:pred x:=false y:=true; impl(x,y) };", literalTrue)>]
    [<DataRow("def pred T() { dec ~x,y:pred x:=false y:=false; impl(x,y) };", literalTrue)>]
    [<DataRow("def pred T() { dec ~x,y:pred y:=false; impl(x,y) };", literalUndetermined)>]
    [<DataRow("def pred T() { dec ~x,y:pred x:=false; impl(x,y) };", literalUndetermined)>]
    [<DataRow("def pred T() { dec ~x,y:pred y:=true; impl(x,y) };", literalUndetermined)>]
    [<DataRow("def pred T() { dec ~x,y:pred x:=true; impl(x,y) };", literalUndetermined)>]
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
            printfn "%s" (pr1.Type(SignatureType.Mixed))
            printfn "%s" (evalTreeFplId(pr1))
            Assert.AreEqual<string>(expected, actual)
        | None -> Assert.IsTrue(false)

    [<DataRow("def pred T() { dec ~x,y:pred x:=true y:=true; iif(x,y) };", literalTrue)>]
    [<DataRow("def pred T() { dec ~x,y:pred x:=true y:=false; iif(x,y) };", literalFalse)>]
    [<DataRow("def pred T() { dec ~x,y:pred x:=false y:=true; iif(x,y) };", literalFalse)>]
    [<DataRow("def pred T() { dec ~x,y:pred x:=false y:=false; iif(x,y) };", literalTrue)>]
    [<DataRow("def pred T() { dec ~x,y:pred y:=false; iif(x,y) };", literalUndetermined)>]
    [<DataRow("def pred T() { dec ~x,y:pred x:=false; iif(x,y) };", literalUndetermined)>]
    [<DataRow("def pred T() { dec ~x,y:pred y:=true; iif(x,y) };", literalUndetermined)>]
    [<DataRow("def pred T() { dec ~x,y:pred x:=true; iif(x,y) };", literalUndetermined)>]
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
            printfn "%s" (pr1.Type(SignatureType.Mixed))
            printfn "%s" (evalTreeFplId(pr1))
            Assert.AreEqual<string>(expected, actual)
        | None -> Assert.IsTrue(false)

    