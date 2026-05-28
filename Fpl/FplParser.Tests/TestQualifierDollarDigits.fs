namespace FplParser.Tests

open FParsec
open FplParsing.Combinators
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
    member this.TestSubscriptsBoth () =
        let result = run (predicate .>> eof) """$1!.$1"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))






    [<TestMethod>]
    member this.TestSubscriptsBothC () =
        let result = run (predicate .>> eof) """$1'! $1"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    (* dollar digits can only be used "as" in some cases *) 

    [<TestMethod>]
    member this.TestDottedAs () =
        let result = run (predicate .>> eof) """x.$1"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestDottedAs1 () =
        let result = run (predicate .>> eof) """x'.$1"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestDottedAs2 () =
        let result = run (predicate .>> eof) """x.-$1"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestDottedAs3 () =
        let result = run (predicate .>> eof) """x . $1"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestArgumentsAs1 () =
        let result = run (predicate .>> eof) """x($1)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestArgumentsAsA1 () =
        let result = run (predicate .>> eof) """x($1)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestArgumentsAsB1 () =
        let result = run (predicate .>> eof) """x( $1 )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestArgumentsAsC1 () =
        let result = run (predicate .>> eof) """s ( $1 )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestCoordinatesAs1 () =
        let result = run (predicate .>> eof) """x[$1]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestCoordinatesAsA1 () =
        let result = run (predicate .>> eof) """x [$1]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestCoordinatesAsB1 () =
        let result = run (predicate .>> eof) """x[ $1 ]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestCoordinatesAsC1 () =
        let result = run (predicate .>> eof) """x[ $1 ]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestCoordsAs1a () =
        let result = run (predicate .>> eof) """ClosedOpenRange($1)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestCoordsAsA1 () =
        let result = run (predicate .>> eof) """x[$1,$1]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestCoordsAsB1a () =
        let result = run (predicate .>> eof) """x[ $1 ]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestCoordsAsC1a () =
        let result = run (predicate .>> eof) """x [ $1 ]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestCoordsAs1 () =
        let result = run (predicate .>> eof) """x[$1]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))


    [<TestMethod>]
    member this.TestCoordsAsB1 () =
        let result = run (predicate .>> eof) """x[ $1]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("00", """x [ $1]""")>]
    [<DataRow("01", """$1!.Test(1)""")>]
    [<DataRow("02", """$1($1)""")>]
    [<DataRow("02a", """$1( $1 )""")>]
    [<DataRow("03", """$1'($1)""")>]
    [<DataRow("04", """$1'( $1 )""")>]
    [<DataRow("05", """$1[$1]""")>]
    [<DataRow("06", """$1'[$1]""")>]
    [<DataRow("07", """$1[ $1 ]""")>]
    [<DataRow("08", """$1'[ $1 ]""")>]
    [<DataRow("09", """$1[$1]""")>]
    [<DataRow("10", """$1'[$1]""")>]
    [<DataRow("11", """$1[$1 ]""")>]
    [<DataRow("12", """$1'[ $1 ]""")>]
    [<DataRow("13", """x'[$1]""")>]
    [<DataRow("14", """$1'!.Test(1)""")>]
    [<DataRow("15", """$1!'.Test(1)""")>]
    [<TestMethod>]
    member this.TestFailure (no:string, fplCode:string) =
        let result = run (predicate .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<DataRow("00", """x[ $1]""")>]
    [<DataRow("01", """$1!""")>]
    [<DataRow("02", """$1""")>]
    [<DataRow("03", """$1'""")>]
    [<DataRow("05", """-$1""")>]
    [<TestMethod>]
    member this.TestSuccess (no:string, fplCode:string) =
        let result = run (predicate .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

