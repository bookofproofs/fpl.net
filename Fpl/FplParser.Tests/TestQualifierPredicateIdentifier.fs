namespace FplParser.Tests

open FParsec
open Fpl.Parser.Grammar
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
    member this.TestDottedBothAa () =
        let result = run (predicate .>> eof) """Xx'.Yy'.Zz"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))


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

    [<DataRow("00", """PascalId'(PascalId)""")>]
    [<DataRow("01", """PascalId'( PascalId )""")>]
    [<DataRow("02", """PascalId'[PascalId]""")>]
    [<DataRow("03", """PascalId'[ PascalId ]""")>]
    [<DataRow("04", """Xx'.Yy().Zz""")>]
    [<DataRow("05", """PascalId'[PascalId,Nat]""")>]
    [<DataRow("06", """PascalId'[PascalId,Nat]""")>]
    [<DataRow("07", """PascalId'.PascalId""")>]
    [<TestMethod>]
    member this.TestFail (no:string, fplCode:string) =
        let result = run (predicate .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<DataRow("00", """PascalId(PascalId)'""")>]
    [<DataRow("01", """PascalId( PascalId )'""")>]
    [<DataRow("02", """PascalId[PascalId]'""")>]
    [<DataRow("03", """PascalId[ PascalId ]'""")>]
    [<DataRow("04", """Xx'""")>]
    [<DataRow("04a", """Xx.Yy().Zz'""")>]
    [<DataRow("04b", """(Xx.Yy().Zz)'""")>]
    [<DataRow("05", """PascalId[PascalId,Nat]'""")>]
    [<DataRow("06", """(PascalId[PascalId,Nat])'""")>]
    [<DataRow("07", """PascalId.PascalId'""")>]
    [<DataRow("07a", """(PascalId.PascalId)'""")>]
    [<TestMethod>]
    member this.TestSuccess (no:string, fplCode:string) =
        let result = run (predicate .>> eof) fplCode
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
    member this.TestCoordinatesBoth () =
        let result = run (predicate .>> eof) """PascalId[PascalId]"""
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
    member this.TestCoordsBoth () =
        let result = run (predicate .>> eof) """PascalId[PascalId,Nat]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestCoordsBothB () =
        let result = run (predicate .>> eof) """PascalId[PascalId,Nat]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))



