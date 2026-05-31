namespace FplParser.Tests

open FParsec
open FplParsing.Combinators
open FplPrimitives
open Microsoft.VisualStudio.TestTools.UnitTesting


[<TestClass>]
type TestCoordPossibilities () =
    (* Tests the specification in DocuFplGrammarPocShiftFromPythonToFSharp.md #### 23 *)

    [<TestMethod>]
    member this.TestExtensionDigit () =
        let result = run (predicate .>> eof) """@1"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestDollarDigit () =
        let result = run (predicate .>> eof) """$1"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestVariable () =
        let result = run (predicate .>> eof) """xyz"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestVariable2 () =
        let result = run (predicate .>> eof) """x.y().z()"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestSelf () =
        let result = run (predicate .>> eof) LiteralSelf
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestSelf1 () =
        let result = run (predicate .>> eof) LiteralParent
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPascalCaseId () =
        let result = run (predicate .>> eof) PrimPascalCaseId
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPascalCaseId1 () =
        let result = run (predicate .>> eof) """PascalCaseId()"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPascalCaseId2 () =
        let result = run (predicate .>> eof) """PascalCaseId.PascalCaseId()"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPascalCaseId3 () =
        let result = run (predicate .>> eof) """PascalCaseId.PascalCaseId().PascalCaseId()"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))


