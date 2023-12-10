namespace FplParser.Tests

open FParsec
open FplParser
open Microsoft.VisualStudio.TestTools.UnitTesting


[<TestClass>]
type TestQualifiersPredicateIdentifier () =
    (* Tests the specification in DocuFplGrammarPocShiftFromPythonToFSharp.md #### 23 *)

    [<TestMethod>]
    member this.TestDottedBoth () =
        let result = run (predicate .>> eof) """PascalId.PascalId"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestDottedBoth1 () =
        let result = run (predicate .>> eof) """PascalId().PascalId()"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestDottedBothA () =
        let result = run (predicate .>> eof) """PascalId'.PascalId"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestDottedBothAa () =
        let result = run (predicate .>> eof) """Xx'.Yy'.Zz"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestDottedBothAb () =
        let result = run (predicate .>> eof) """Xx'.Yy().Zz"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestDottedBothB () =
        let result = run (predicate .>> eof) """PascalId. PascalId"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestDottedBothC () =
        let result = run (predicate .>> eof) """PascalId . PascalId"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestArgumentsBoth () =
        let result = run (predicate .>> eof) """PascalId(PascalId)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestArgumentsBothA () =
        let result = run (predicate .>> eof) """PascalId'(PascalId)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestArgumentsBothB () =
        let result = run (predicate .>> eof) """PascalId( PascalId )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestArgumentsBothC () =
        let result = run (predicate .>> eof) """PascalId'( PascalId )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestCoordinatesBoth () =
        let result = run (predicate .>> eof) """PascalId[PascalId]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestCoordinatesBothA () =
        let result = run (predicate .>> eof) """PascalId'[PascalId]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestCoordinatesBothB () =
        let result = run (predicate .>> eof) """PascalId[ PascalId ]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestCoordinatesBothC () =
        let result = run (predicate .>> eof) """PascalId'[ PascalId ]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestRangesBoth () =
        let result = run (predicate .>> eof) """PascalId[[PascalId,]]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestRangesBothA () =
        let result = run (predicate .>> eof) """PascalId'[[PascalId,]]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestRangesBothB () =
        let result = run (predicate .>> eof) """PascalId[[ PascalId ,]]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestRangesBothC () =
        let result = run (predicate .>> eof) """PascalId'[[ PascalId ,]]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

