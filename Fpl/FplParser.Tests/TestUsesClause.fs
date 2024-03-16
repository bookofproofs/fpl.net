namespace FplParser.Tests

open FParsec
open FplParser
open Microsoft.VisualStudio.TestTools.UnitTesting


[<TestClass>]
type TestUsesClause () =
    let replaceWhiteSpace (input: string) =
        let whiteSpaceChars = [|' '; '\t'; '\n'; '\r'|]
        input.Split(whiteSpaceChars)
            |> String.concat ""

    [<TestMethod>]
    member this.TestUsesClause01 () =
        let result = run (usesClause .>> eof) """uses TestNamespace"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestUsesClause01a () =
        let result = run (usesClause .>> eof) """uses Fpl.Commons"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestUsesClause02 () =
        let result = run (usesClause .>> eof) """uses TestNamespace1.TestNamespace2"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestUsesClause01c () =
        let result = run (usesClause .>> eof) """uses TestNamespace *"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestUsesClause02a () =
        let result = run (usesClause .>> eof) """uses TestNamespace1.TestNamespace2 *"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestUsesClause01b () =
        let result = run (usesClause .>> eof) """uses TestNamespace alias T1"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestUsesClause02b () =
        let result = run (usesClause .>> eof) """uses TestNamespace1.TestNamespace2 alias T2"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))
