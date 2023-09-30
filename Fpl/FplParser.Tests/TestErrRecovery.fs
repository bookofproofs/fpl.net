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
        let expectedDiags = """Diagnostic
  (FplParser, Error, (Ln: 1, Col: 1), DiagnosticMessage "no input consumed")"""
        Assert.AreEqual(replaceWhiteSpace expectedDiags, replaceWhiteSpace actualDiags)

    [<TestMethod>]
    member this.Test_AliasedNamespaceIdentifier01 () =
        let input = """A alias B"""
        let result = tryParse aliasedNamespaceIdentifier "recovery failed;" ad input
        let actual = sprintf "%O" result
        let expected = """AliasedNamespaceIdentifier (NamespaceIdentifier [PascalCaseId "A"], Alias "B")"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.Test_AliasedNamespaceIdentifier01Diag () =
        ad.Clear()
        let input = """A alias B"""
        let result = tryParse aliasedNamespaceIdentifier "recovery failed;" ad input
        let actualDiags = ad.DiagnosticsToString
        let expectedDiags = """"""
        Assert.AreEqual(replaceWhiteSpace expectedDiags, replaceWhiteSpace actualDiags)

    [<TestMethod>]
    member this.Test_AliasedNamespaceIdentifier02 () =
        let input = """A alias b"""
        let result = tryParse aliasedNamespaceIdentifier "recovery failed;" ad input
        let actual = sprintf "%O" result
        let expected = """AliasedNamespaceIdentifier (NamespaceIdentifier [PascalCaseId "A"], Alias "b")"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.Test_AliasedNamespaceIdentifier02Diag () =
        ad.Clear()
        let input = """A alias b"""
        let result = tryParse aliasedNamespaceIdentifier "recovery failed;" ad input
        let actualDiags = ad.DiagnosticsToString
        let expectedDiags = """Diagnostic
  (FplParser, Error, (Ln: 1, Col: 10),
   DiagnosticMessage "expecting PascalCaseId")"""
        Assert.AreEqual(replaceWhiteSpace expectedDiags, replaceWhiteSpace actualDiags)

    [<TestMethod>]
    member this.Test_AliasedNamespaceIdentifier03 () =
        let input = """a alias B"""
        let result = tryParse aliasedNamespaceIdentifier "recovery failed;" ad input
        let actual = sprintf "%O" result
        let expected = """AliasedNamespaceIdentifier (NamespaceIdentifier [PascalCaseId "a"], Alias "B")"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.Test_AliasedNamespaceIdentifier03Diag () =
        ad.Clear()
        let input = """a alias B"""
        let result = tryParse aliasedNamespaceIdentifier "recovery failed;" ad input
        let actualDiags = ad.DiagnosticsToString
        let expectedDiags = """Diagnostic
  (FplParser, Error, (Ln: 1, Col: 2), DiagnosticMessage "expecting PascalCaseId")"""
        Assert.AreEqual(replaceWhiteSpace expectedDiags, replaceWhiteSpace actualDiags)

    [<TestMethod>]
    member this.Test_AliasedNamespaceIdentifier04 () =
        let input = """a alias b"""
        let result = tryParse aliasedNamespaceIdentifier "recovery failed;" ad input
        let actual = sprintf "%O" result
        let expected = """AliasedNamespaceIdentifier (NamespaceIdentifier [PascalCaseId "a"], Alias "b")"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.Test_AliasedNamespaceIdentifier04Diag () =
        ad.Clear()
        let input = """a alias b"""
        let result = tryParse aliasedNamespaceIdentifier "recovery failed;" ad input
        let actualDiags = ad.DiagnosticsToString
        let expectedDiags = """Diagnostic
  (FplParser, Error, (Ln: 1, Col: 2), DiagnosticMessage "expecting PascalCaseId")
Diagnostic
  (FplParser, Error, (Ln: 1, Col: 10),
   DiagnosticMessage "expecting PascalCaseId")"""
        Assert.AreEqual(replaceWhiteSpace expectedDiags, replaceWhiteSpace actualDiags)

    [<TestMethod>]
    member this.Test_AliasedNamespaceIdentifier05 () =
        let input = """A alias """
        let result = tryParse aliasedNamespaceIdentifier "recovery failed;" ad input
        let actual = sprintf "%O" result
        let expected = """AliasedNamespaceIdentifier
  (NamespaceIdentifier [PascalCaseId "A"], Alias "alias")"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.Test_AliasedNamespaceIdentifier05Diag () =
        ad.Clear()
        let input = """A alias """
        let result = tryParse aliasedNamespaceIdentifier "recovery failed;" ad input
        let actualDiags = ad.DiagnosticsToString
        let expectedDiags = """Diagnostic
  (FplParser, Error, (Ln: 1, Col: 9), DiagnosticMessage "expecting PascalCaseId")"""
        Assert.AreEqual(replaceWhiteSpace expectedDiags, replaceWhiteSpace actualDiags)

    [<TestMethod>]
    member this.Test_AliasedNamespaceIdentifier06 () =
        let input = """A alias"""
        let result = tryParse aliasedNamespaceIdentifier "recovery failed;" ad input
        let actual = sprintf "%O" result
        let expected = """AliasedNamespaceIdentifier
  (NamespaceIdentifier [PascalCaseId "A"], Alias "alias")"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.Test_AliasedNamespaceIdentifier06Diag () =
        ad.Clear()
        let input = """A alias"""
        let result = tryParse aliasedNamespaceIdentifier "recovery failed;" ad input
        let actualDiags = ad.DiagnosticsToString
        let expectedDiags = """Diagnostic
  (FplParser, Error, (Ln: 1, Col: 8), DiagnosticMessage "expecting PascalCaseId")"""
        Assert.AreEqual(replaceWhiteSpace expectedDiags, replaceWhiteSpace actualDiags)

    [<TestMethod>]
    member this.Test_AliasedNamespaceIdentifier08 () =
        let input = """a alias"""
        let result = tryParse aliasedNamespaceIdentifier "recovery failed;" ad input
        let actual = sprintf "%O" result
        let expected = """AliasedNamespaceIdentifier
  (NamespaceIdentifier [PascalCaseId "a"], Alias "alias")"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.Test_AliasedNamespaceIdentifier08Diag () =
        ad.Clear()
        let input = """a alias"""
        let result = tryParse aliasedNamespaceIdentifier "recovery failed;" ad input
        let actualDiags = ad.DiagnosticsToString
        let expectedDiags = """Diagnostic
  (FplParser, Error, (Ln: 1, Col: 2), DiagnosticMessage "expecting PascalCaseId")
Diagnostic
  (FplParser, Error, (Ln: 1, Col: 8), DiagnosticMessage "expecting PascalCaseId")"""
        Assert.AreEqual(replaceWhiteSpace expectedDiags, replaceWhiteSpace actualDiags)

    [<TestMethod>]
    member this.Test_AliasedNamespaceIdentifier09 () =
        let input = """a alias """
        let result = tryParse aliasedNamespaceIdentifier "recovery failed;" ad input
        let actual = sprintf "%O" result
        let expected = """AliasedNamespaceIdentifier
  (NamespaceIdentifier [PascalCaseId "a"], Alias "alias")"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.Test_AliasedNamespaceIdentifier09Diag () =
        ad.Clear()
        let input = """a alias """
        let result = tryParse aliasedNamespaceIdentifier "recovery failed;" ad input
        let actualDiags = ad.DiagnosticsToString
        let expectedDiags = """Diagnostic
  (FplParser, Error, (Ln: 1, Col: 2), DiagnosticMessage "expecting PascalCaseId")
Diagnostic
  (FplParser, Error, (Ln: 1, Col: 9), DiagnosticMessage "expecting PascalCaseId")"""
        Assert.AreEqual(replaceWhiteSpace expectedDiags, replaceWhiteSpace actualDiags)

    [<TestMethod>]
    member this.Test_AliasedNamespaceIdentifier12 () =
        let input = """ alias b"""
        let result = tryParse aliasedNamespaceIdentifier "recovery failed;" ad input
        let actual = sprintf "%O" result
        let expected = """AliasedNamespaceIdentifier (NamespaceIdentifier [Empty], Alias "b")"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.Test_AliasedNamespaceIdentifier12Diag () =
        ad.Clear()
        let input = """ alias b"""
        let result = tryParse aliasedNamespaceIdentifier "recovery failed;" ad input
        let actualDiags = ad.DiagnosticsToString
        let expectedDiags = """Diagnostic
  (FplParser, Error, (Ln: 1, Col: 9), DiagnosticMessage "expecting PascalCaseId")"""
        Assert.AreEqual(replaceWhiteSpace expectedDiags, replaceWhiteSpace actualDiags)

    [<TestMethod>]
    member this.Test_AliasedNamespaceIdentifier14 () =
        let input = """ alias """
        let result = tryParse aliasedNamespaceIdentifier "recovery failed;" ad input
        let actual = sprintf "%O" result
        let expected = """AliasedNamespaceIdentifier (NamespaceIdentifier [Empty], Alias "alias")"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.Test_AliasedNamespaceIdentifier14Diag () =
        ad.Clear()
        let input = """ alias """
        let result = tryParse aliasedNamespaceIdentifier "recovery failed;" ad input
        let actualDiags = ad.DiagnosticsToString
        let expectedDiags = """Diagnostic
  (FplParser, Error, (Ln: 1, Col: 8), DiagnosticMessage "expecting PascalCaseId")"""
        Assert.AreEqual(replaceWhiteSpace expectedDiags, replaceWhiteSpace actualDiags)

    [<TestMethod>]
    member this.Test_AliasedNamespaceIdentifier15 () =
        let input = """ alias"""
        let result = tryParse aliasedNamespaceIdentifier "recovery failed;" ad input
        let actual = sprintf "%O" result
        let expected = """AliasedNamespaceIdentifier (NamespaceIdentifier [Empty], Alias "alias")"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.Test_AliasedNamespaceIdentifier15Diag () =
        ad.Clear()
        let input = """ alias"""
        let result = tryParse aliasedNamespaceIdentifier "recovery failed;" ad input
        let actualDiags = ad.DiagnosticsToString
        let expectedDiags = """Diagnostic
  (FplParser, Error, (Ln: 1, Col: 7), DiagnosticMessage "expecting PascalCaseId")"""
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
        let keyword = keywordAlternative "alias" ""|>> Ast.SomeString
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
        let keyword = keywordAlternative "alias" ""|>> Ast.SomeString
        let result = tryParse keyword "recovery failed;" ad typo
        let actualDiags = ad.DiagnosticsToString
        let expectedDiags = """Diagnostic
  (FplParser, Error, (Ln: 1, Col: 4),
   DiagnosticMessage "expecting 'alias' keyword")"""
        Assert.AreEqual(replaceWhiteSpace expectedDiags, replaceWhiteSpace actualDiags)

    [<TestMethod>]
    member this.Test_UsesClause01 () =
        let input = "uses {A, B alias Adi}"
        let result = tryParse usesClause "recovery failed;" ad input
        let actual = sprintf "%O" result
        let expected = """UsesClause
  [NamespaceIdentifier [PascalCaseId "A"];
   AliasedNamespaceIdentifier
     (NamespaceIdentifier [PascalCaseId "B"], Alias "Adi")]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.Test_UsesClause01Diag () =
        ad.Clear()
        let input = "uses {A, B alias Adi,}"
        let result = tryParse usesClause "recovery failed;" ad input
        let actual = sprintf "%O" result
        let actualDiags = ad.DiagnosticsToString
        let expectedDiags = """"""
        Assert.AreEqual(replaceWhiteSpace expectedDiags, replaceWhiteSpace actualDiags)

    [<TestMethod>]
    member this.Test_TheoryNamespace01 () =
        let input = ""
        let result = tryParse theoryNamespace "recovery failed;" ad input
        let actual = sprintf "%O" result
        let expected = """NamespaceIdentifier[Empty]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.Test_TheoryNamespace01Diag () =
        ad.Clear()
        let input = ""
        let result = tryParse theoryNamespace "recovery failed;" ad input
        let actual = sprintf "%O" result
        let actualDiags = ad.DiagnosticsToString
        let expectedDiags = """xxx"""
        Assert.AreEqual(replaceWhiteSpace expectedDiags, actualDiags)

    [<TestMethod>]
    member this.Test_UsesClause02 () =
        let input = "uses {A, B alias Adi,}"
        let result = tryParse usesClause "recovery failed;" ad input
        let actual = sprintf "%O" result
        let expected = """UsesClause
  [NamespaceIdentifier [PascalCaseId "A"];
   AliasedNamespaceIdentifier
     (NamespaceIdentifier [PascalCaseId "B"], Alias "Adi");
   NamespaceIdentifier [Empty]]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.Test_UsesClause02Diag () =
        ad.Clear()
        let input = "uses {A, B alias Adi,}"
        let result = tryParse usesClause "recovery failed;" ad input
        let actual = sprintf "%O" result
        let actualDiags = ad.DiagnosticsToString
        let expectedDiags = """xxx"""
        Assert.AreEqual(replaceWhiteSpace expectedDiags, actualDiags)

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
    [<DataRow ("bla ","Adi", 8)>]
    [<DataRow ("alias ","Adi", 10)>]
    [<DataRow ("alias ", "aAdi", 11)>]
    [<DataRow ("alias ", "zAdi", 11)>]
    [<DataRow ("alias ", "_Adi", 11)>]
    [<DataRow ("alias ", "@Adi", 11)>]
    [<DataRow ("alias ", "-Adi", 11)>]
    [<DataRow ("alias ", "0Adi", 11)>]
    [<DataRow ("alias ", "9Adi", 11)>]
    member this.Test_AliasAlternative (param:string, param1:string, ind:int) =
        let input = param + param1
        let result = tryParse alias "recovery failed;" ad input
        let actual = sprintf "%O" result
        let expected = sprintf """Alias (((Ln: 1, Col: 1), (Ln: 1, Col: %i)), "%s")""" ind param1
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