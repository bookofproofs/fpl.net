namespace FplParser.Tests

open FParsec
open ErrorHandling
open FplParser
open Microsoft.VisualStudio.TestTools.UnitTesting


[<TestClass>]
type TestLitterals () =
    let replaceWhiteSpace (input: string) =
        let whiteSpaceChars = [|' '; '\t'; '\n'; '\r'|]
        input.Split(whiteSpaceChars)
            |> String.concat ""

    [<TestMethod>]
    member this.TestVDash () =
        let result = tryParse vDash "streamtest" """|-""" 
        let actual = sprintf "%O" result
        let expected = """(VDash(Some()),[])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestVDashErr () =
        let result = tryParse vDash "streamtest" """s|-"""
        let actual = sprintf "%O" result
        let expected = """(VDash None, [Diagnostic (("streamtest", Ln: 1, Col: 1), "PE", "expecting '|-'")])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestLeftBrace () =
        let result = tryParse leftBrace "streamtest" """{""" 
        let actual = sprintf "%O" result
        let expected = """(LeftBrace (Some ()), [])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestLeftBraceErr () =
        let result = tryParse leftBrace "streamtest" """s{"""
        let actual = sprintf "%O" result
        let expected = """(LeftBrace None, [Diagnostic (("streamtest", Ln: 1, Col: 1), "PE", "missing opening '{'")])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestRightBrace () =
        let result = tryParse rightBrace "streamtest" """}""" 
        let actual = sprintf "%O" result
        let expected = """(RightBrace (Some ()), [])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestRightBraceErr () =
        let result = tryParse rightBrace "streamtest" """s}"""
        let actual = sprintf "%O" result
        let expected = """(RightBrace None, [Diagnostic (("streamtest", Ln: 1, Col: 1), "PE", "missing closing '}'")])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestLeftParen () =
        let result = tryParse leftParen "streamtest" """(""" 
        let actual = sprintf "%O" result
        let expected = """(LeftParen (Some ()), [])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestLeftParenErr () =
        let result = tryParse leftParen "streamtest" """s("""
        let actual = sprintf "%O" result
        let expected = """(LeftParen None, [Diagnostic (("streamtest", Ln: 1, Col: 1), "PE", "missing opening '('")])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestRightParen () =
        let result = tryParse rightParen "streamtest" """)""" 
        let actual = sprintf "%O" result
        let expected = """(RightParen (Some ()), [])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestRightParenErr () =
        let result = tryParse rightParen "streamtest" """s)"""
        let actual = sprintf "%O" result
        let expected = """(RightParen None, [Diagnostic (("streamtest", Ln: 1, Col: 1), "PE", "missing closing ')'")])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestLeftBracket () =
        let result = tryParse leftBracket "streamtest" """[""" 
        let actual = sprintf "%O" result
        let expected = """(LeftBracket (Some ()), [])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestLeftBracketErr () =
        let result = tryParse leftBracket "streamtest" """s["""
        let actual = sprintf "%O" result
        let expected = """(LeftBracket None, [Diagnostic (("streamtest", Ln: 1, Col: 1), "PE", "missing opening '['")])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestRightBracket () =
        let result = tryParse rightBracket "streamtest" """]""" 
        let actual = sprintf "%O" result
        let expected = """(RightBracket (Some ()), [])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestRightBracketErr () =
        let result = tryParse rightBracket "streamtest" """s]"""
        let actual = sprintf "%O" result
        let expected = """(RightBracket None, [Diagnostic (("streamtest", Ln: 1, Col: 1), "PE", "missing closing ']'")])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestComma () =
        let result = tryParse comma "streamtest" """,""" 
        let actual = sprintf "%O" result
        let expected = """(Comma (Some ()), [])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestCommaErr () =
        let result = tryParse comma "streamtest" """s,"""
        let actual = sprintf "%O" result
        let expected = """(Comma None, [Diagnostic (("streamtest", Ln: 1, Col: 1), "PE", "expecting ','")])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestStar () =
        let result = tryParse star "streamtest" """*""" 
        let actual = sprintf "%O" result
        let expected = """(Star (Some ()), [])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestStarErr () =
        let result = tryParse star "streamtest" """s*"""
        let actual = sprintf "%O" result
        let expected = """(Star None, [Diagnostic (("streamtest", Ln: 1, Col: 1), "PE", "zero or more qualifier '*'")])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestPlus () =
        let result = tryParse plus "streamtest" """+""" 
        let actual = sprintf "%O" result
        let expected = """(Plus (Some ()), [])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestPlusErr () =
        let result = tryParse plus "streamtest" """s+"""
        let actual = sprintf "%O" result
        let expected = """(Plus None, [Diagnostic (("streamtest", Ln: 1, Col: 1), "PE", "one or more qualifier '+'")])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestDot () =
        let result = tryParse dot "streamtest" """.""" 
        let actual = sprintf "%O" result
        let expected = """(Dot (Some ()), [])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestDotErr () =
        let result = tryParse dot "streamtest" """s."""
        let actual = sprintf "%O" result
        let expected = """(Dot None, [Diagnostic (("streamtest", Ln: 1, Col: 1), "PE", "expecting point '.'")])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestColon () =
        let result = tryParse colon "streamtest" """:""" 
        let actual = sprintf "%O" result
        let expected = """(Colon (Some ()), [])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestColonErr () =
        let result = tryParse colon "streamtest" """s:"""
        let actual = sprintf "%O" result
        let expected = """(Colon None, [Diagnostic (("streamtest", Ln: 1, Col: 1), "PE", "expecting colon ':'")])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestColonEqual () =
        let result = tryParse colonEqual "streamtest" """:=""" 
        let actual = sprintf "%O" result
        let expected = """(ColonEqual (Some ()), [])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestColonEqualErr () =
        let result = tryParse colonEqual "streamtest" """s:="""
        let actual = sprintf "%O" result
        let expected = """(ColonEqual None, [Diagnostic (("streamtest", Ln: 1, Col: 1), "PE", "expecting assignment ':='")])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestAt () =
        let result = tryParse at "streamtest" """@""" 
        let actual = sprintf "%O" result
        let expected = """(At (Some ()), [])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestAtErr () =
        let result = tryParse at "streamtest" """s@"""
        let actual = sprintf "%O" result
        let expected = """(At None, [Diagnostic (("streamtest", Ln: 1, Col: 1), "PE", "expecting '@'")])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestExclamationmark () =
        let result = tryParse exclamationMark "streamtest" """!""" 
        let actual = sprintf "%O" result
        let expected = """(ExclamationMark (Some ()), [])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestExclamationmarkErr () =
        let result = tryParse exclamationMark "streamtest" """s!"""
        let actual = sprintf "%O" result
        let expected = """(ExclamationMark None, [Diagnostic (("streamtest", Ln: 1, Col: 1), "PE", "expecting '!'")])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestCase () =
        let result = tryParse case "streamtest" """|""" 
        let actual = sprintf "%O" result
        let expected = """(Case (Some ()), [])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestCaseErr () =
        let result = tryParse case "streamtest" """s|"""
        let actual = sprintf "%O" result
        let expected = """(Case None, [Diagnostic (("streamtest", Ln: 1, Col: 1), "PE", "expecting '|'")])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestTilde () =
        let result = tryParse tilde "streamtest" """~""" 
        let actual = sprintf "%O" result
        let expected = """(Tilde (Some ()), [])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestTildeErr () =
        let result = tryParse tilde "streamtest" """s~"""
        let actual = sprintf "%O" result
        let expected = """(Tilde None, [Diagnostic (("streamtest", Ln: 1, Col: 1), "PE", "expecting '~'")])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestSemicolon () =
        let result = tryParse semiColon "streamtest" """;""" 
        let actual = sprintf "%O" result
        let expected = """(Semicolon (Some ()), [])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestSemicolonErr () =
        let result = tryParse semiColon "streamtest" """s;"""
        let actual = sprintf "%O" result
        let expected = """(Semicolon None, [Diagnostic (("streamtest", Ln: 1, Col: 1), "PE", "expecting ';'")])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestDollar () =
        let result = tryParse dollar "streamtest" """$""" 
        let actual = sprintf "%O" result
        let expected = """(Dollar (Some ()), [])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestDollarErr () =
        let result = tryParse dollar "streamtest" """s$"""
        let actual = sprintf "%O" result
        let expected = """(Dollar None, [Diagnostic (("streamtest", Ln: 1, Col: 1), "PE", "expecting '$'")])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestMap () =
        let result = tryParse map "streamtest" """->""" 
        let actual = sprintf "%O" result
        let expected = """(Map (Some ()), [])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestMapErr () =
        let result = tryParse map "streamtest" """s->"""
        let actual = sprintf "%O" result
        let expected = """(Map None, [Diagnostic (("streamtest", Ln: 1, Col: 1), "PE", "expecting map '->'")])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);