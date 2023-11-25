namespace FplParser.Tests

open FParsec
open FplParser
open Microsoft.VisualStudio.TestTools.UnitTesting


[<TestClass>]
type TestQualifiersExtensionDigits () =
    (* Tests the specification in DocuFplGrammarPocShiftFromPythonToFSharp.md #### 23 *)

    [<TestMethod>]
    member this.TestDottedBoth () =
        let result = run (predicate .>> eof) """1.1"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestDottedBothA () =
        let result = run (predicate .>> eof) """1 .1"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestDottedBothB () =
        let result = run (predicate .>> eof) """1. 1"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestDottedBothC () =
        let result = run (predicate .>> eof) """1 . 1"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestArgumentsBoth () =
        let result = run (predicate .>> eof) """4(2)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestArgumentsBothA () =
        let result = run (predicate .>> eof) """1 (1)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestArgumentsBothB () =
        let result = run (predicate .>> eof) """1( 1 )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestArgumentsBothC () =
        let result = run (predicate .>> eof) """1 ( 1 )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestCoordinatesBoth () =
        let result = run (predicate .>> eof) """1<1>"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestCoordinatesBothA () =
        let result = run (predicate .>> eof) """1 <1>"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestCoordinatesBothB () =
        let result = run (predicate .>> eof) """1< 1 >"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestCoordinatesBothC () =
        let result = run (predicate .>> eof) """1 < 1 >"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestRangesBoth () =
        let result = run (predicate .>> eof) """1[1,]>"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestRangesBothA () =
        let result = run (predicate .>> eof) """1 [1,]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestRangesBothB () =
        let result = run (predicate .>> eof) """1[ 1 ,]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestRangesBothC () =
        let result = run (predicate .>> eof) """1 [ 1 ,]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestSubscriptsBoth () =
        let result = run (predicate .>> eof) """1!1"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestSubscriptsBothA () =
        let result = run (predicate .>> eof) """1 !1"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestSubscriptsBothB () =
        let result = run (predicate .>> eof) """1! 1"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestSubscriptsBothC () =
        let result = run (predicate .>> eof) """1 ! 1"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    (* extension digits can only be used "as" in some cases *) 

    [<TestMethod>]
    member this.TestDottedAs1 () =
        let result = run (predicate .>> eof) """x.1"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestDottedAsA1 () =
        let result = run (predicate .>> eof) """x .1"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestDottedAsB1 () =
        let result = run (predicate .>> eof) """x. 1"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestDottedAsC1 () =
        let result = run (predicate .>> eof) """x . 1"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestArgumentsAs1 () =
        let result = run (predicate .>> eof) """x(1)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestArgumentsAsA1 () =
        let result = run (predicate .>> eof) """x (1)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestArgumentsAsB1 () =
        let result = run (predicate .>> eof) """x( 1 )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestArgumentsAsC1 () =
        let result = run (predicate .>> eof) """s ( 1 )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestCoordinatesAs1 () =
        let result = run (predicate .>> eof) """x<1>"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestCoordinatesAsA1 () =
        let result = run (predicate .>> eof) """x <1>"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestCoordinatesAsB1 () =
        let result = run (predicate .>> eof) """x< 1 >"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestCoordinatesAsC1 () =
        let result = run (predicate .>> eof) """x < 1 >"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestRangesAs1 () =
        let result = run (predicate .>> eof) """x[1,]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestRangesAsA1 () =
        let result = run (predicate .>> eof) """x [1,]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestRangesAsB1 () =
        let result = run (predicate .>> eof) """x[ 1 ,]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestRangesAsC1 () =
        let result = run (predicate .>> eof) """x [ 1 ,]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestSubscriptsAs1 () =
        let result = run (predicate .>> eof) """x!1"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestSubscriptsAsA1 () =
        let result = run (predicate .>> eof) """x !1"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestSubscriptsAsB1 () =
        let result = run (predicate .>> eof) """x! 1"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestSubscriptsAsC1 () =
        let result = run (predicate .>> eof) """x ! 1"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))


