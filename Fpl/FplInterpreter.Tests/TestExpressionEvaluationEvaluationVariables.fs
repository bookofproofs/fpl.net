namespace FplInterpreter.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open ErrDiagnostics
open FplInterpreterTypes
open CommonTestHelpers

[<TestClass>]
type TestExpressionEvaluationVariables() =
    let evalTreeFplId (fplValue: FplValue) = getType SignatureType.Name fplValue
    let evalTreeFplRepresentation (fplValue: FplValue) = getRepresentation fplValue

    [<DataRow("def pred T() { dec ~x:pred x:=true; x };", "true")>]
    [<DataRow("def pred T() { dec ~x:pred x:=false; x };", "false")>]
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

    [<DataRow("def pred T() { dec ~x,y,z:pred x:=true y:=true z:=true; and(and(x,y),z) };", "true")>]
    [<DataRow("def pred T() { dec ~x,y,z:pred x:=true y:=false z:=true; and(and(x,y),z) };", "false")>]
    [<DataRow("def pred T() { dec ~x,y,z:pred x:=true y:=true z:=false; and(and(x,y),z) };", "false")>]
    [<DataRow("def pred T() { dec ~x,y,z:pred x:=false y:=true z:=true; and(and(x,y),z) };", "false")>]
    [<DataRow("def pred T() { dec ~x,y,z:pred x:=true z:=true; and(x,and(y,z)) };", "undetermined")>]
    [<DataRow("def pred T() { dec ~x,y,z:pred x:=true y:=true; and(x,and(y,z)) };", "undetermined")>]
    [<DataRow("def pred T() { dec ~x,y,z:pred y:=true z:=true; and(x,and(y,z)) };", "undetermined")>]
    [<DataRow("def pred T() { dec ~x,y:pred x:=true y:=true; and(x,y) };", "true")>]
    [<DataRow("def pred T() { dec ~x,y:pred x:=true y:=false; and(x,y) };", "false")>]
    [<DataRow("def pred T() { dec ~x,y:pred x:=false y:=true; and(x,y) };", "false")>]
    [<DataRow("def pred T() { dec ~x,y:pred x:=false y:=false; and(x,y) };", "false")>]
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

    [<DataRow("def pred T() { dec ~x:pred x:=true; not x };", "false")>]
    [<DataRow("def pred T() { dec ~x:pred x:=true; not (x) };", "false")>]
    [<DataRow("def pred T() { dec ~x:pred x:=true; not ((x)) };", "false")>]
    [<DataRow("def pred T() { dec ~x:pred x:=false; not x };", "true")>]
    [<DataRow("def pred T() { dec ~x:pred x:=false; not (x) };", "true")>]
    [<DataRow("def pred T() { dec ~x:pred x:=false; not ((x)) };", "true")>]
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

    [<DataRow("def pred T() { dec ~x,y,z:pred x:=false y:=false z:=false; or(x,or(y,z)) };", "false")>]
    [<DataRow("def pred T() { dec ~x,y,z:pred x:=false y:=true z:=false; or(x,or(y,z)) };", "true")>]
    [<DataRow("def pred T() { dec ~x,y,z:pred x:=false y:=false z:=true; or(or(x,y),z) };", "true")>]
    [<DataRow("def pred T() { dec ~x,y,z:pred x:=true y:=false z:=false; or(or(x,y),z) };", "true")>]
    [<DataRow("def pred T() { dec ~x,y,z:pred x:=false z:=false; or(x,or(y,z)) };", "undetermined")>]
    [<DataRow("def pred T() { dec ~x,y,z:pred x:=false y:=false; or(or(x,y),z) };", "undetermined")>]
    [<DataRow("def pred T() { dec ~x,y,z:pred y:=false z:=false; or(x,or(y,z)) };", "undetermined")>]
    [<DataRow("def pred T() { dec ~x,y:pred x:=true y:=true; or(x,y) };", "true")>]
    [<DataRow("def pred T() { dec ~x,y:pred x:=true y:=false; or(x,y) };", "true")>]
    [<DataRow("def pred T() { dec ~x,y:pred x:=false y:=true; or(x,y) };", "true")>]
    [<DataRow("def pred T() { dec ~x,y:pred x:=false y:=false; or(x,y) };", "false")>]
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

    [<DataRow("def pred T() { dec ~x,y,z:pred x:=false y:=false z:=false; xor(x,xor(y,z)) };", "false")>]
    [<DataRow("def pred T() { dec ~x,y,z:pred x:=false y:=false z:=true; xor(xor(x,y),z) };", "true")>]
    [<DataRow("def pred T() { dec ~x,y,z:pred x:=false y:=true z:=false; xor(xor(x,y),z) };", "true")>]
    [<DataRow("def pred T() { dec ~x,y,z:pred x:=true y:=false z:=false; xor(xor(x,y),z) };", "true")>]
    [<DataRow("def pred T() { dec ~x,y,z:pred x:=false y:=true z:=true; xor(x,xor(y,z)) };", "false")>]
    [<DataRow("def pred T() { dec ~x,y,z:pred x:=true y:=false z:=true; xor(xor(x,y),z) };", "false")>]
    [<DataRow("def pred T() { dec ~x,y,z:pred x:=true y:=true z:=false; xor(x,xor(y,z)) };", "false")>]
    [<DataRow("def pred T() { dec ~x,y,z:pred x:=true y:=true z:=true; xor(x,xor(y,z)) };", "true")>]
    [<DataRow("def pred T() { dec ~x,y,z:pred x:=false y:=true; xor(xor(x,y),z) };", "undetermined")>]
    [<DataRow("def pred T() { dec ~x,y,z:pred y:=false z:=true; xor(x,xor(y,z)) };", "undetermined")>]
    [<DataRow("def pred T() { dec ~x,y,z:pred x:=true z:=false; xor(x,xor(y,z)) };", "undetermined")>]
    [<DataRow("def pred T() { dec ~x,y:pred x:=true y:=true; xor(x,y) };", "false")>]
    [<DataRow("def pred T() { dec ~x,y:pred x:=true y:=false; xor(x,y) };", "true")>]
    [<DataRow("def pred T() { dec ~x,y:pred x:=false y:=true; xor(x,y) };", "true")>]
    [<DataRow("def pred T() { dec ~x,y:pred x:=false y:=false; xor(x,y) };", "false")>]
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

    [<DataRow("def pred T() { dec ~x,y:pred x:=true y:=true; impl(x,y) };", "true")>]
    [<DataRow("def pred T() { dec ~x,y:pred x:=true y:=false; impl(x,y) };", "false")>]
    [<DataRow("def pred T() { dec ~x,y:pred x:=false y:=true; impl(x,y) };", "true")>]
    [<DataRow("def pred T() { dec ~x,y:pred x:=false y:=false; impl(x,y) };", "true")>]
    [<DataRow("def pred T() { dec ~x,y:pred y:=false; impl(x,y) };", "undetermined")>]
    [<DataRow("def pred T() { dec ~x,y:pred x:=false; impl(x,y) };", "undetermined")>]
    [<DataRow("def pred T() { dec ~x,y:pred y:=true; impl(x,y) };", "undetermined")>]
    [<DataRow("def pred T() { dec ~x,y:pred x:=true; impl(x,y) };", "undetermined")>]
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

    [<DataRow("def pred T() { dec ~x,y:pred x:=true y:=true; iif(x,y) };", "true")>]
    [<DataRow("def pred T() { dec ~x,y:pred x:=true y:=false; iif(x,y) };", "false")>]
    [<DataRow("def pred T() { dec ~x,y:pred x:=false y:=true; iif(x,y) };", "false")>]
    [<DataRow("def pred T() { dec ~x,y:pred x:=false y:=false; iif(x,y) };", "true")>]
    [<DataRow("def pred T() { dec ~x,y:pred y:=false; iif(x,y) };", "undetermined")>]
    [<DataRow("def pred T() { dec ~x,y:pred x:=false; iif(x,y) };", "undetermined")>]
    [<DataRow("def pred T() { dec ~x,y:pred y:=true; iif(x,y) };", "undetermined")>]
    [<DataRow("def pred T() { dec ~x,y:pred x:=true; iif(x,y) };", "undetermined")>]
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

    