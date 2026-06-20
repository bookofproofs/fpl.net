namespace TestFplInterpreter.Scope

open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type Completeness() =

    [<TestMethod>]
    member this.TestBlocks() =
        TestCases.ScopeBlocks("") |> ignore

    [<TestMethod>]
    member this.TestConstructors() =
        TestCases.ScopeConstructors("") |> ignore

    [<TestMethod>]
    member this.TestProofsAndCorollaries() =
        TestCases.ScopeProofsAndCorollaries("") |> ignore

    [<TestMethod>]
    member this.TestProperties() =
        TestCases.ScopeProperties("") |> ignore

    [<TestMethod>]
    member this.TestVariablesInBlock() =
        TestCases.ScopeVariablesInBlock("") |> ignore

    [<TestMethod>]
    member this.TestVariablesInBlockWithVariadic() =
        TestCases.ScopeVariablesInBlockVariadic("") |> ignore

    [<TestMethod>]
    member this.TestVariablesInSignature() =
        TestCases.ScopeVariablesInSignature("") |> ignore

    [<TestMethod>]
    member this.TestVariablesInSignatureWithVariadic() =
        TestCases.ScopeVariablesInSignatureVariadic("") |> ignore



