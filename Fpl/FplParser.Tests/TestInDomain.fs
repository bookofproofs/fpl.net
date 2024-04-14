namespace FplParser.Tests

open FParsec
open FplParser
open Microsoft.VisualStudio.TestTools.UnitTesting


[<TestClass>]
type TestInDomain () =

    [<TestMethod>]
    member this.TestInDomain01 () =
        let result = run (inDomain .>> eof) """is obj"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestInDomain02 () =
        let result = run (inDomain .>> eof) """is obj[x:TestClass]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestInDomain03 () =
        let result = run (inDomain .>> eof) """is func"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestInDomain04 () =
        let result = run (inDomain .>> eof) """is ind"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestInDomain05 () =
        let result = run (inDomain .>> eof) """is pred"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestInDomain06 () =
        let result = run (inDomain .>> eof) """in TestClass"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestInDomain07 () =
        let result = run (inDomain .>> eof) """is template"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestInDomain08 () =
        let result = run (inDomain .>> eof) """is @Nat"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestInDomain09 () =
        let result = run (inDomain .>> eof) """is func()->obj"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestInDomain10 () =
        let result = run (inDomain .>> eof) """in someVar"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestInDomain11 () =
        let result = run (inDomain .>> eof) """in self"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestInDomain12 () =
        let result = run (inDomain .>> eof) """in ClosedRange(from,to)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestInDomain13 () =
        let result = run (inDomain .>> eof) """in T[x]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestInDomain14 () =
        let result = run (inDomain .>> eof) """is T[x:func]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))
