namespace FplParser.Tests

open FParsec
open FplParser
open Microsoft.VisualStudio.TestTools.UnitTesting


[<TestClass>]
type TestInDomain () =

    [<TestMethod>]
    member this.TestInDomain06 () =
        let result = run (inEntity .>> eof) """in TestClass"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestInDomain10 () =
        let result = run (inEntity .>> eof) """in someVar"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestInDomain11 () =
        let result = run (inEntity .>> eof) """in self"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestInDomain12 () =
        let result = run (inEntity .>> eof) """in ClosedRange(from,to)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestInDomain13 () =
        let result = run (inEntity .>> eof) """in T[x]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))


