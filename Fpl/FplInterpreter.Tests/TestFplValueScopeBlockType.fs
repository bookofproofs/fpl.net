namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open FplInterpreterTypes

[<TestClass>]
type TestFplValueScopeBlockType() =

    [<TestMethod>]
    member this.TestBlocks() =
        CommonFplValueTestCases.ScopeBlocks() |> ignore
        Assert.IsTrue(false)

    [<TestMethod>]
    member this.TestConstructors() =
        CommonFplValueTestCases.ScopeConstructors() |> ignore
        Assert.IsTrue(false)

    [<TestMethod>]
    member this.TestProofsAndCorollaries() =
        CommonFplValueTestCases.ScopeVariablesInSignatureWithVariadic() |> ignore
        Assert.IsTrue(false)

    [<TestMethod>]
    member this.TestProperties() =
        CommonFplValueTestCases.ScopeProperties() |> ignore
        Assert.IsTrue(false)

    [<TestMethod>]
    member this.TestVariablesInBlock() =
        Assert.IsTrue(false)

    [<TestMethod>]
    member this.TestVariablesInBlockWithVariadic() =
        Assert.IsTrue(false)

    [<TestMethod>]
    member this.TestVariablesInSignature() =
        Assert.IsTrue(false)

    [<TestMethod>]
    member this.TestVariablesInSignatureWithVariadic() =
        Assert.IsTrue(false)