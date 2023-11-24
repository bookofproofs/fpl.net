namespace FplParser.Tests

open FParsec
open FplParser
open Microsoft.VisualStudio.TestTools.UnitTesting


[<TestClass>]
type TestQualifiersDollarDigits () =
    (* Tests the specification in DocuFplGrammarPocShiftFromPythonToFSharp.md #### 23 *)

    [<TestMethod>]
    member this.TestDottedBoth () =
        let result = run (predicate .>> eof) """$1.$1"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestDottedBothA () =
        let result = run (predicate .>> eof) """$1 .$1"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestDottedBothB () =
        let result = run (predicate .>> eof) """$1. $1"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestDottedBothC () =
        let result = run (predicate .>> eof) """$1 . $1"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestArgumentsBoth () =
        let result = run (predicate .>> eof) """$1($1)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestArgumentsBothA () =
        let result = run (predicate .>> eof) """$1 ($1)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestArgumentsBothB () =
        let result = run (predicate .>> eof) """$1( $1 )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestArgumentsBothC () =
        let result = run (predicate .>> eof) """$1 ( $1 )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestCoordinatesBoth () =
        let result = run (predicate .>> eof) """$1<$1>"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestCoordinatesBothA () =
        let result = run (predicate .>> eof) """$1 <$1>"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestCoordinatesBothB () =
        let result = run (predicate .>> eof) """$1< $1 >"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestCoordinatesBothC () =
        let result = run (predicate .>> eof) """$1 < $1 >"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestRangesBoth () =
        let result = run (predicate .>> eof) """$1[$1,]>"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestRangesBothA () =
        let result = run (predicate .>> eof) """$1 [$1,]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestRangesBothB () =
        let result = run (predicate .>> eof) """$1[ $1 ,]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestRangesBothC () =
        let result = run (predicate .>> eof) """$1 [ $1 ,]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestSubscriptsBoth () =
        let result = run (predicate .>> eof) """$1!$1"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestSubscriptsBothA () =
        let result = run (predicate .>> eof) """$1 !$1"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestSubscriptsBothB () =
        let result = run (predicate .>> eof) """$1! $1"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestSubscriptsBothC () =
        let result = run (predicate .>> eof) """$1 ! $1"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    (* dollar digits can only be used "as" in some cases *) 

    [<TestMethod>]
    member this.TestDottedBoth1 () =
        let result = run (predicate .>> eof) """x.$1"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestDottedBothA1 () =
        let result = run (predicate .>> eof) """x .$1"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestDottedBothB1 () =
        let result = run (predicate .>> eof) """x. $1"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestDottedBothC1 () =
        let result = run (predicate .>> eof) """x . $1"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestArgumentsBoth1 () =
        let result = run (predicate .>> eof) """x($1)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestArgumentsBothA1 () =
        let result = run (predicate .>> eof) """x ($1)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestArgumentsBothB1 () =
        let result = run (predicate .>> eof) """x( $1 )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestArgumentsBothC1 () =
        let result = run (predicate .>> eof) """s ( $1 )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestCoordinatesBoth1 () =
        let result = run (predicate .>> eof) """x<$1>"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestCoordinatesBothA1 () =
        let result = run (predicate .>> eof) """x <$1>"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestCoordinatesBothB1 () =
        let result = run (predicate .>> eof) """x< $1 >"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestCoordinatesBothC1 () =
        let result = run (predicate .>> eof) """x < $1 >"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestRangesBoth1 () =
        let result = run (predicate .>> eof) """x[$1,]>"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestRangesBothA1 () =
        let result = run (predicate .>> eof) """x [$1,]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestRangesBothB1 () =
        let result = run (predicate .>> eof) """x[ $1 ,]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestRangesBothC1 () =
        let result = run (predicate .>> eof) """x [ $1 ,]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestSubscriptsBoth1 () =
        let result = run (predicate .>> eof) """x!$1"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestSubscriptsBothA1 () =
        let result = run (predicate .>> eof) """x !$1"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestSubscriptsBothB1 () =
        let result = run (predicate .>> eof) """x! $1"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestSubscriptsBothC1 () =
        let result = run (predicate .>> eof) """x ! $1"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))


