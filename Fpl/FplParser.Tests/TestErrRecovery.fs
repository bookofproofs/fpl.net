namespace FplParser.Tests

open FParsec
open FplGrammarCommons
open ErrRecovery
open FplGrammar
open Microsoft.VisualStudio.TestTools.UnitTesting


[<TestClass>]
type TestErrRecovery() =
    let replaceWhiteSpace (input: string) =
        let whiteSpaceChars = [| ' '; '\t'; '\n'; '\r' |]
        input.Split(whiteSpaceChars) |> String.concat ""

    let input000 = """TestNamescpace {
    x
    theory {
    }
}"""

    let input001 = """TestNamescpace {
    x
    theory {
        y
    }
}"""

    [<TestMethod>]
    [<DataRow("and")>]
    [<DataRow("and ")>]
    [<DataRow("and\n")>]
    [<DataRow("and\t")>]
    [<DataRow("and(")>]
    [<DataRow("theory{")>]
    member this.TestStartWithFplKeyword(test: string) =
        let result = startsWithFplKeyword test
        Assert.IsTrue(result)

    [<TestMethod>]
    [<DataRow("xxx")>]
    [<DataRow("xxx ")>]
    [<DataRow("xxx\n")>]
    [<DataRow("xxx\t")>]
    [<DataRow("xxx(")>]
    [<DataRow("xxx{")>]
    member this.TestNotStartWithFplKeyword(test: string) =
        let result = startsWithFplKeyword test
        Assert.IsFalse(result)

    [<TestMethod>]
    member this.TestTryParse000Diag () =
        let result = fplParser input000
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 2, Col: 5),
   DiagnosticMessage
     "'x'
Expecting: <whitespace, block or inline comment>, extension block, 'inf',
'inference', 'th', 'theory' or 'uses'
")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag);

    [<TestMethod>]
    member this.TestTryParse001Diag () =
        let result = fplParser input001
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
      (FplParser, Error, (Ln: 2, Col: 5),
       DiagnosticMessage
         "'x'
    Expecting: <whitespace, block or inline comment>, extension block, 'inf',
    'inference', 'th', 'theory' or 'uses'
    ")
    Diagnostic
      (FplParser, Error, (Ln: 4, Col: 9),
       DiagnosticMessage
         "'y'
    Expecting: <whitespace, block or inline comment>, 'ax', 'axiom', 'cl', 'class',
    'conj', 'conjecture', 'cor', 'corollary', 'func', 'function', 'lem', 'lemma',
    'post', 'postulate', 'pred', 'predicate', 'prf', 'proof', 'prop', 'proposition'
    , 'theorem', 'thm' or '}'
    ")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag);