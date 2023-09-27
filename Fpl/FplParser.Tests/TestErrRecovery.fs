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
        let expected = """NamespaceIdentifier
  [PascalCaseId "Fpl"; PascalCaseId "x"; PascalCaseId "Common"]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.Test_NamespaceIdentifier02Diag () =
        ad.Clear()
        let input = """Fpl.x.Common"""
        let result = tryParse namespaceIdentifier "recovery failed;" ad input
        let actualDiags = ad.DiagnosticsToString
        let expectedDiags = """Diagnostic
  (FplParser, Error, (Ln: 1, Col: 6), DiagnosticMessage "expecting PascalCaseId")"""
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

    [<TestMethod>]
    [<DataRow ("Adi")>]
    [<DataRow ("aAdi")>]
    [<DataRow ("zAdi")>]
    [<DataRow ("_Adi")>]
    [<DataRow ("@Adi")>]
    [<DataRow ("-Adi")>]
    [<DataRow ("0Adi")>]
    [<DataRow ("9Adi")>]
    member this.Test_PascalCaseIdAlternative (param:string) =
        let input = param
        let result = tryParse pascalCaseId "recovery failed;" ad input
        let actual = sprintf "%O" result
        let expected = sprintf """PascalCaseId "%s" """ param
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    [<DataRow ("Adi")>]
    member this.Test_PascalCaseIdAlternativeDiagOK (param:string) =
        ad.Clear()
        let input = param
        let result = tryParse pascalCaseId "recovery failed;" ad input
        let actualDiags = ad.DiagnosticsToString
        let expectedDiags = """"""
        Assert.AreEqual(replaceWhiteSpace expectedDiags, replaceWhiteSpace actualDiags)

    [<TestMethod>]
    [<DataRow ("aAdi")>]
    [<DataRow ("zAdi")>]
    [<DataRow ("_Adi")>]
    [<DataRow ("@Adi")>]
    [<DataRow ("-Adi")>]
    [<DataRow ("0Adi")>]
    [<DataRow ("9Adi")>]
    member this.Test_PascalCaseIdAlternativeDiag (param:string) =
        ad.Clear()
        let input = param
        let result = tryParse pascalCaseId "recovery failed;" ad input
        let actualDiags = ad.DiagnosticsToString
        let expectedDiags = """Diagnostic
  (FplParser, Error, (Ln: 1, Col: 5), DiagnosticMessage "expecting PascalCaseId")"""
        Assert.AreEqual(replaceWhiteSpace expectedDiags, replaceWhiteSpace actualDiags)


    [<TestMethod>]
    [<DataRow ("foo")>]
    [<DataRow ("bar")>]
    [<DataRow ("baz")>]
    member this.Test_KeywordAlternative (typo:string) =
        let keyword = keywordAlternative "alias" |>> Ast.SomeString
        let result = tryParse keyword "recovery failed;" ad typo
        let actual = sprintf "%O" result
        let expected = sprintf """SomeString "%s" """ typo
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    [<DataRow ("foo")>]
    [<DataRow ("bar")>]
    [<DataRow ("baz")>]
    member this.Test_KeywordAlternativeDiag (typo:string) =
        ad.Clear()
        let keyword = keywordAlternative "alias" |>> Ast.SomeString
        let result = tryParse keyword "recovery failed;" ad typo
        let actualDiags = ad.DiagnosticsToString
        let expectedDiags = """Diagnostic
  (FplParser, Error, (Ln: 1, Col: 4),
   DiagnosticMessage "expecting 'alias' keyword")"""
        Assert.AreEqual(replaceWhiteSpace expectedDiags, replaceWhiteSpace actualDiags)

    [<TestMethod>]
    member this.Test_AliasAlternativeOK () =
        let input = "alias Adi"
        let result = tryParse alias "recovery failed;" ad input
        let actual = sprintf "%O" result
        let expected = """Alias "Adi" """
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    [<DataRow ("alias", "")>]
    [<DataRow ("alias ", "")>]
    member this.Test_AliasAlternativeEmpty (param:string, param1:string) =
        let input = param + param1
        let result = tryParse alias "recovery failed;" ad input
        let actual = sprintf "%O" result
        let expected =  """Alias "alias" """ 
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    [<DataRow ("alias", 6)>]
    [<DataRow ("alias ", 7)>]
    member this.Test_AliasAlternativeEmptyDiag (param:string, pos:int) =
        ad.Clear()
        let input = param
        let result = tryParse alias "recovery failed;" ad input
        let actualDiags = ad.DiagnosticsToString
        let expectedDiags = sprintf """Diagnostic
  (FplParser, Error, (Ln: 1, Col: %i), DiagnosticMessage "expecting PascalCaseId")""" pos
        Assert.AreEqual(replaceWhiteSpace expectedDiags, replaceWhiteSpace actualDiags)

    [<TestMethod>]
    [<DataRow ("bla ","Adi")>]
    [<DataRow ("alias ","Adi")>]
    [<DataRow ("alias ", "aAdi")>]
    [<DataRow ("alias ", "zAdi")>]
    [<DataRow ("alias ", "_Adi")>]
    [<DataRow ("alias ", "@Adi")>]
    [<DataRow ("alias ", "-Adi")>]
    [<DataRow ("alias ", "0Adi")>]
    [<DataRow ("alias ", "9Adi")>]
    member this.Test_AliasAlternative (param:string, param1:string) =
        let input = param + param1
        let result = tryParse alias "recovery failed;" ad input
        let actual = sprintf "%O" result
        let expected = sprintf """Alias "%s" """ param1
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    [<DataRow ("alias Adi")>]
    member this.Test_AliasAlternativeDiagOK (param:string) =
        ad.Clear()
        let input = param
        let result = tryParse alias "recovery failed;" ad input
        let actualDiags = ad.DiagnosticsToString
        let expectedDiags = """"""
        Assert.AreEqual(replaceWhiteSpace expectedDiags, replaceWhiteSpace actualDiags)

    [<TestMethod>]
    [<DataRow ("alias aAdi")>]
    [<DataRow ("alias zAdi")>]
    [<DataRow ("alias _Adi")>]
    [<DataRow ("alias @Adi")>]
    [<DataRow ("alias -Adi")>]
    [<DataRow ("alias 0Adi")>]
    [<DataRow ("alias 9Adi")>]
    member this.Test_AliasAlternativeDiag (param:string) =
        ad.Clear()
        let input = param
        let result = tryParse alias "recovery failed;" ad input
        let actualDiags = ad.DiagnosticsToString
        let expectedDiags = """Diagnostic
  (FplParser, Error, (Ln: 1, Col: 11), DiagnosticMessage "expecting PascalCaseId")"""
        Assert.AreEqual(replaceWhiteSpace expectedDiags, replaceWhiteSpace actualDiags)