namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open FplInterpreterTypes

[<TestClass>]
type TestFplValueScopeCompleteness() =

    [<TestMethod>]
    member this.TestBlocks() =
        try
            CommonFplValueTestCases.ScopeBlocks() |> ignore
            Assert.IsTrue(true)
        with
        | ex -> 
            Assert.IsTrue(false)

    [<TestMethod>]
    member this.TestConstructors() =
        try
            CommonFplValueTestCases.ScopeConstructors() |> ignore
            Assert.IsTrue(true)
        with
        | ex -> 
            Assert.IsTrue(false)

    [<TestMethod>]
    member this.TestProofsAndCorollaries() =
        try
            CommonFplValueTestCases.ScopeProofsAndCorollaries() |> ignore
            Assert.IsTrue(true)
        with
        | ex -> 
            Assert.IsTrue(false)

    [<TestMethod>]
    member this.TestProperties() =
        try
            CommonFplValueTestCases.ScopeProperties() |> ignore
            Assert.IsTrue(true)
        with
        | ex -> 
            Assert.IsTrue(false)

    [<TestMethod>]
    member this.TestVariablesInBlock() =
        try
            CommonFplValueTestCases.ScopeVariablesInBlock() |> ignore
            Assert.IsTrue(true)
        with
        | ex -> 
            Assert.IsTrue(false)

    [<TestMethod>]
    member this.TestVariablesInBlockWithVariadic() =
        try
            CommonFplValueTestCases.ScopeVariablesInBlockVariadic() |> ignore
            Assert.IsTrue(true)
        with
        | ex -> 
            Assert.IsTrue(false)

    [<TestMethod>]
    member this.TestVariablesInSignature() =
        try
            CommonFplValueTestCases.ScopeVariablesInSignature() |> ignore
            Assert.IsTrue(true)
        with
        | ex -> 
            Assert.IsTrue(false)

    [<TestMethod>]
    member this.TestVariablesInSignatureWithVariadic() =
        try
            CommonFplValueTestCases.ScopeVariablesInSignatureWithVariadic() |> ignore
            Assert.IsTrue(true)
        with
        | ex -> 
            Assert.IsTrue(false)
