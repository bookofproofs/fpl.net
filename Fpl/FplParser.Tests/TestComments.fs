namespace FplParser.Tests

open FParsec
open ErrorHandling
open FplParser
open Microsoft.VisualStudio.TestTools.UnitTesting


[<TestClass>]
type TestComments () =
    let replaceWhiteSpace (input: string) =
        let whiteSpaceChars = [|' '; '\t'; '\n'; '\r'|]
        input.Split(whiteSpaceChars)
            |> String.concat ""

    [<TestMethod>]
    member this.TestCW () =
        let result = tryParse CW "streamtest" """ """ 
        let actual = sprintf "%O" result
        let expected = """(CW (Some ()), [])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestCWErr () =
        let result = tryParse CW "streamtest" """s """
        let actual = sprintf "%O" result
        let expected = """(CW None, [Diagnostic (("streamtest", Ln: 1, Col: 1), "PE", "expecting whitespace, inline comment, or block comment")])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestCW1 () =
        let result = tryParse CW "streamtest" """// this is a comment""" 
        let actual = sprintf "%O" result
        let expected = """(CW (Some ()), [])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestCW1Err () =
        let result = tryParse CW "streamtest" """s// this is a comment"""
        let actual = sprintf "%O" result
        let expected = """(CW None, [Diagnostic (("streamtest", Ln: 1, Col: 1), "PE", "expecting whitespace, inline comment, or block comment")])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestCW2 () =
        let result = tryParse CW "streamtest" """/* this is a 
        comment */""" 
        let actual = sprintf "%O" result
        let expected = """(CW (Some ()), [])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestCW2Err () =
        let result = tryParse CW "streamtest" """s/* this is 
        a comment*/"""
        let actual = sprintf "%O" result
        let expected = """(CW None, [Diagnostic (("streamtest", Ln: 1, Col: 1), "PE", "expecting whitespace, inline comment, or block comment")])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);
