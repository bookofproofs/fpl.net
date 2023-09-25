namespace FplParser.Tests

open FParsec
open FplGrammarTypes
open ErrRecovery
open FplGrammar
open Microsoft.VisualStudio.TestTools.UnitTesting


[<TestClass>]
type TestErrRecovery () =
    let replaceWhiteSpace (input: string) =
        let whiteSpaceChars = [|' '; '\t'; '\n'; '\r'|]
        input.Split(whiteSpaceChars)
            |> String.concat ""

    [<TestMethod>]
    member this.Test_NamespaceIdentifier01 () =
        let input = """Fpl.Common"""
        let result = tryParse namespaceIdentifier "recovery failed;" ad input
        let actual = sprintf "%O" result
        let expected = """NamespaceIdentifier [PascalCaseId "Fpl"; PascalCaseId "Common"]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.Test_NamespaceIdentifier01Diag () =
        ad.Clear()
        let input = """Fpl.Common"""
        let result = tryParse namespaceIdentifier "recovery failed;" ad input
        let actualDiags = ad.DiagnosticsToString
        let expectedDiags = """"""
        Assert.AreEqual(replaceWhiteSpace expectedDiags, replaceWhiteSpace actualDiags)

    [<TestMethod>]
    member this.Test_NamespaceIdentifier02 () =
        let input = """Fpl.x.Common"""
        let result = tryParse namespaceIdentifier "recovery failed;" ad input
        let actual = sprintf "%O" result
        let expected = """NamespaceIdentifier [PascalCaseId "Fpl"; Escape; PascalCaseId "Common"]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.Test_NamespaceIdentifier02Diag () =
        ad.Clear()
        let input = """Fpl.x.Common"""
        let result = tryParse namespaceIdentifier "recovery failed;" ad input
        let actualDiags = ad.DiagnosticsToString
        let expectedDiags = """Diagnostic
  (FplParser, Error, (Ln: 1, Col: 5), DiagnosticMessage "expected PascalCaseId")"""
        Assert.AreEqual(replaceWhiteSpace expectedDiags, replaceWhiteSpace actualDiags)

    [<TestMethod>]
    member this.Test_NamespaceIdentifier03 () =
        let input = """Fpl.Common."""
        let result = tryParse namespaceIdentifier "recovery failed;" ad input
        let actual = sprintf "%O" result
        let expected = """NamespaceIdentifier [PascalCaseId "Fpl"; PascalCaseId "Common"; Escape]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.Test_NamespaceIdentifier03Diag () =
        ad.Clear()
        let input = """Fpl.Common."""
        let result = tryParse namespaceIdentifier "recovery failed;" ad input
        let actualDiags = ad.DiagnosticsToString
        let expectedDiags = """Diagnostic
  (FplParser, Error, (Ln: 1, Col: 12), DiagnosticMessage "expected PascalCaseId")"""
        Assert.AreEqual(replaceWhiteSpace expectedDiags, replaceWhiteSpace actualDiags)

    [<TestMethod>]
    member this.Test_NamespaceIdentifier04 () =
        let input = """ """
        let result = tryParse namespaceIdentifier "recovery failed;" ad input
        let actual = sprintf "%O" result
        let expected = """NamespaceIdentifier [Empty]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.Test_NamespaceIdentifier04Diag () =
        ad.Clear()
        let input = """ """
        let result = tryParse namespaceIdentifier "recovery failed;" ad input
        let actualDiags = ad.DiagnosticsToString
        let expectedDiags = """"""
        Assert.AreEqual(replaceWhiteSpace expectedDiags, replaceWhiteSpace actualDiags)

    [<TestMethod>]
    member this.Test_NamespaceIdentifier05 () =
        let input = """"""
        let result = tryParse namespaceIdentifier "recovery failed;" ad input
        let actual = sprintf "%O" result
        let expected = """NamespaceIdentifier [Empty]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.Test_NamespaceIdentifier05Diag () =
        ad.Clear()
        let input = """"""
        let result = tryParse namespaceIdentifier "recovery failed;" ad input
        let actualDiags = ad.DiagnosticsToString
        let expectedDiags = """"""
        Assert.AreEqual(replaceWhiteSpace expectedDiags, replaceWhiteSpace actualDiags)