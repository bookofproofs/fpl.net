namespace FplParser.Tests

open FParsec
open FplParser
open Microsoft.VisualStudio.TestTools.UnitTesting


[<TestClass>]
type TestCoordInTypePossibilities () =
    (* Tests the specification in DocuFplGrammarPocShiftFromPythonToFSharp.md #### 23 *)

    [<TestMethod>]
    member this.TestExtensionDigit () =
        let result = run coordInType """1"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestDollarDigit () =
        let result = run coordInType """$1"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestVariable () =
        let result = run coordInType """xyz"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestVariable2 () =
        let result = run coordInType """x.y().z()"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestSelf () =
        let result = run coordInType """self"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestSelf1 () =
        let result = run coordInType """@self"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestPascalCaseId () =
        let result = run coordInType """PascalCaseId"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPascalCaseId1 () =
        let result = run coordInType """PascalCaseId()"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestPascalCaseId2 () =
        let result = run coordInType """PascalCaseId.PascalCaseId()"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestPascalCaseId3 () =
        let result = run coordInType """PascalCaseId.PascalCaseId().PascalCaseId()"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestPascalCaseId4 () =
        let result = run coordInType """PascalCaseId.PascalCaseId.PascalCaseId"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))


