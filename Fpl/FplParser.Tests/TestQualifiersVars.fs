namespace FplParser.Tests

open FParsec
open Fpl.Parser.Grammar
open Microsoft.VisualStudio.TestTools.UnitTesting


[<TestClass>]
type TestQualifiersVars () =
    (* Tests the specification in DocuFplGrammarPocShiftFromPythonToFSharp.md #### 23 *)


    [<DataRow("00", """x'(y)""")>]
    [<DataRow("01", """x'( y )""")>]
    [<DataRow("02", """x'[y]""")>]
    [<DataRow("03", """x'[ y ]""")>]
    [<DataRow("04", """x'.y""")>]
    [<DataRow("04", """x. y""")>]
    [<TestMethod>]
    member this.TestArgumentsFailure (no:string, fplCode:string) =
        let result = run (predicate .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<DataRow("00", """x( y )""")>]
    [<DataRow("01", """x(y)""")>]
    [<DataRow("02", """x[y]""")>]
    [<DataRow("03", """x[y]'""")>]
    [<DataRow("04", """x.y'""")>]
    [<DataRow("05", """x.y""")>]
    
    [<TestMethod>]
    member this.TestArgumentsSuccess (no:string, fplCode:string) =
        let result = run (predicate .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))




    [<TestMethod>]
    member this.TestCoordinatesBothB () =
        let result = run (predicate .>> eof) """x[ y ]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))



    [<TestMethod>]
    member this.TestCoordsBoth () =
        let result = run (predicate .>> eof) """z[x[y]]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestCoordsBothA () =
        let result = run (predicate .>> eof) """z[x, y[i]]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestCoordsBothB () =
        let result = run (predicate .>> eof) """x[y]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestCoordsBothC () =
        let result = run (predicate .>> eof) """x [y]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

