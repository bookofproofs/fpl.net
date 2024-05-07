namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open FplInterpreterTypes

[<TestClass>]
type TestFplValueScopeCompleteness() =

    [<TestMethod>]
    member this.TestBlocks() =
        CommonFplValueTestCases.ScopeBlocks() |> ignore

    [<TestMethod>]
    member this.TestConstructors() =
        CommonFplValueTestCases.ScopeConstructors() |> ignore

    [<TestMethod>]
    member this.TestProofsAndCorollaries() =
        CommonFplValueTestCases.ScopeProofsAndCorollaries() |> ignore

    [<TestMethod>]
    member this.TestProperties() =
        CommonFplValueTestCases.ScopeProperties() |> ignore

    [<TestMethod>]
    member this.TestVariablesInBlock() =
        CommonFplValueTestCases.ScopeVariablesInBlock() |> ignore

    [<TestMethod>]
    member this.TestVariablesInBlockWithVariadic() =
        CommonFplValueTestCases.ScopeVariablesInBlockVariadic() |> ignore

    [<TestMethod>]
    member this.TestVariablesInSignature() =
        CommonFplValueTestCases.ScopeVariablesInSignature() |> ignore

    [<TestMethod>]
    member this.TestVariablesInSignatureWithVariadic() =
        CommonFplValueTestCases.ScopeVariablesInSignatureVariadic() |> ignore

    [<TestMethod>]
    member this.TestCallConstructorParentClass() =
        CommonFplValueTestCases.ScopeCallConstructorParentClass() |> ignore

    [<TestMethod>]
    member this.TestDelegate() =
        CommonFplValueTestCases.ScopeDelegate() |> ignore

    [<TestMethod>]
    member this.TestPredicateWithArguments() =
        CommonFplValueTestCases.ScopePredicateWithArguments() |> ignore
