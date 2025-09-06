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
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestDottedBothA () =
        let result = run (predicate .>> eof) """1 .1"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

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
        let result = run (predicate .>> eof) """@4(@2)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestArgumentsBothA () =
        let result = run (predicate .>> eof) """@1 (@1)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestArgumentsBothB () =
        let result = run (predicate .>> eof) """1( 1 )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestArgumentsBothC () =
        let result = run (predicate .>> eof) """@1 ( @1 )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestCoordinatesBoth () =
        let result = run (predicate .>> eof) """@1[@1]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestCoordinatesBothA () =
        let result = run (predicate .>> eof) """@1 [@1]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestCoordinatesBothB () =
        let result = run (predicate .>> eof) """@1[ @1 ]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestCoordinatesBothC () =
        let result = run (predicate .>> eof) """@1 [ @1 ]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestCoordsBoth () =
        let result = run (predicate .>> eof) """@1[@1,]>"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestCoordsBothA () =
        let result = run (predicate .>> eof) """@1 [@1,]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestCoordsBothB () =
        let result = run (predicate .>> eof) """@1[ @1 ,]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestCoordsBothC () =
        let result = run (predicate .>> eof) """@1 [ @1 ,]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))



    (* extension digits can only be used "as" in some cases *) 

    [<TestMethod>]
    member this.TestDottedAs1 () =
        let result = run (predicate .>> eof) """x.@1"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestDottedAsA1 () =
        let result = run (predicate .>> eof) """x'.@1"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestDottedAsB1 () =
        let result = run (predicate .>> eof) """x. @1"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestDottedAsC1 () =
        let result = run (predicate .>> eof) """x . @1"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestArgumentsAs1 () =
        let result = run (predicate .>> eof) """x(@1)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestArgumentsAsA1 () =
        let result = run (predicate .>> eof) """x'(@1)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestArgumentsAsB1 () =
        let result = run (predicate .>> eof) """x( @1 )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestArgumentsAsC1 () =
        let result = run (predicate .>> eof) """s'( @1 )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestCoordinatesAs1 () =
        let result = run (predicate .>> eof) """x[@1]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestCoordinatesAsA1 () =
        let result = run (predicate .>> eof) """x'[@1]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestCoordinatesAsB1 () =
        let result = run (predicate .>> eof) """x[ @1 ]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestCoordinatesAsC1 () =
        let result = run (predicate .>> eof) """x'[ @1 ]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))






