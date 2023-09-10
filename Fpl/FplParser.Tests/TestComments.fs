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
    member this.TestSignificantWS () =
        let result = tryParse SW "streamtest" """ """ 
        let actual = sprintf "%O" result
        let expected = """(SignificantWS (Some ()), [])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestSignificantWSErr () =
        let result = tryParse SW "streamtest" """s """
        let actual = sprintf "%O" result
        let expected = """(SignificantWS None, [Diagnostic (("streamtest", Ln: 1, Col: 1), "PE", "whitespace")])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestInlineComment () =
        let result = tryParse inlineComment "streamtest" """// inline comment""" 
        let actual = sprintf "%O" result
        let expected = """(InlineComment (Some ()), [])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestInlineCommentErr () =
        let result = tryParse inlineComment "streamtest" """s// inline comment """
        let actual = sprintf "%O" result
        let expected = """(InlineComment (Some ()), [Diagnostic (("streamtest", Ln: 1, Col: 1), "PE", "expecting '//'")])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestBlockComment () =
        let result = tryParse blockComment "streamtest" """/* block 
        comment */""" 
        let actual = sprintf "%O" result
        let expected = """(BlockComment (Some ()), [])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestBlockCommentErr () =
        let result = tryParse blockComment "streamtest" """s/* block 
        comment */""" 
        let actual = sprintf "%O" result
        let expected = """(BlockComment (Some ()), [Diagnostic (("streamtest", Ln: 1, Col: 1), "PE", "expecting '/*'")])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestBlockCommentErr1 () =
        let result = tryParse blockComment "streamtest" """/* block 
        comment """ 
        let actual = sprintf "%O" result
        let expected = """(BlockComment None, [Diagnostic (("streamtest", Ln: 1, Col: 1), "PE", "missing closing '*/'")])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestCW () =
        let result = tryParse CW "streamtest" """ """ 
        let actual = sprintf "%O" result
        let expected = """(CW (Some (SignificantWS (Some ()))), [])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestCWErr () =
        let result = tryParse CW "streamtest" """s """
        let actual = sprintf "%O" result
        let expected = """We-would-expect-a-diagnostic-for-CW,not-for-whitespace,since-CW-is-a-choice"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestCW1 () =
        let result = tryParse CW "streamtest" """// this is a comment""" 
        let actual = sprintf "%O" result
        let expected = """We-would-expect-an-ast-for-inlineComment,not-a-diagnostic-for-failed-whitespace,since-CW-is-a-choice"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestCW1Err () =
        let result = tryParse CW "streamtest" """s// this is a comment"""
        let actual = sprintf "%O" result
        let expected = """We-would-expect-a-diagnostic-for-CW,not-for-whitespace,since-CW-is-a-choice"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestCW2 () =
        let result = tryParse CW "streamtest" """/* this is a 
        comment */""" 
        let actual = sprintf "%O" result
        let expected = """We-would-expect-an-ast-for-blockComment,not-a-diagnostic-for-failed-whitespace,since-CW-is-a-choice"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestCW2Err () =
        let result = tryParse CW "streamtest" """s/* this is 
        a comment*/"""
        let actual = sprintf "%O" result
        let expected = """We-would-expect-a-diagnostic-for-CW,not-for-whitespace,since-CW-is-a-choice"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);
