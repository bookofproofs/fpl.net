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
        ad.Clear()
        let result = fplParser """TestNamespace {
    x
    theory {
    }
}"""
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
      (FplParser, Error, (Ln: 2, Col: 5),
       DiagnosticMessage
         "'x'
    Expecting: ' ', <block or inline comment>, ':ext', 'inf', 'inference', 'th',
    'theory' or 'uses'
    ")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag);

    [<TestMethod>]
    member this.TestTryParseExtension000Diag () =
        ad.Clear()
        let result = fplParser """TestNamespace {
    :
    theory {
    }
}"""
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 2, Col: 5),
   DiagnosticMessage
     "':'
Expecting: ' ', <block or inline comment>, ':ext', 'inf', 'inference', 'th',
'theory' or 'uses'
")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag);

    [<TestMethod>]
    member this.TestTryParseExtension001Diag () =
        ad.Clear()
        let result = fplParser """TestNamespace {
    :ext
    theory {
    }
}"""
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
      (FplParser, Error, (Ln: 3, Col: 5),
       DiagnosticMessage
         "Cannot recover from Error in Ln: 3 Col: 5
        theory {
        ^
    Expecting: 'ext'
    ")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag);

    [<TestMethod>]
    member this.TestTryParseExtension002Diag () =
        ad.Clear()
        let result = fplParser """TestNamespace {
    :ext ext
    theory {
    }
}"""
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 2, Col: 13),
   DiagnosticMessage
     "Cannot recover from Error in Ln: 2 Col: 13
    :ext ext
            ^
Note: The error occurred at the end of the line.
Expecting: <PascalCaseId>
")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag);

    [<TestMethod>]
    member this.TestTryParseExtension004Diag () =
        ad.Clear()
        let result = fplParser """TestNamespace {
    :ext extTest:
    theory {
    }
}"""
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
      (FplParser, Error, (Ln: 3, Col: 5),
       DiagnosticMessage
         "Cannot recover from Error in Ln: 3 Col: 5
        theory {
        ^
    Expecting: string matching the regex '\\/(?!:end).*'
    ")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag);


    [<TestMethod>]
    member this.TestTryParseUses001Diag () =
        ad.Clear()
        let input = """TestNamescpace {
    x
    theory {
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 2, Col: 5),
   DiagnosticMessage
     "'x'
Expecting: ' ', <block or inline comment>, ':ext', 'inf', 'inference', 'th',
'theory' or 'uses'
")
Diagnostic
  (FplParser, Error, (Ln: 4, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: ' ', <block or inline comment>, 'ax', 'axiom', 'cl', 'class', 'conj'
, 'conjecture', 'cor', 'corollary', 'func', 'function', 'lem', 'lemma', 'post',
'postulate', 'pred', 'predicate', 'prf', 'proof', 'prop', 'proposition',
'theorem', 'thm' or '}'
")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag);

    [<TestMethod>]
    member this.TestTryParseUses002Diag () =
        ad.Clear()
        let input = """TestNamescpace {
    uses {  
    theory {
        y 
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 3, Col: 5),
   DiagnosticMessage
     "'theory'
Expecting: ' ' or <namespace or aliased namespace, e.g. 'RefNs'>
")
Diagnostic
  (FplParser, Error, (Ln: 4, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: ' ', <block or inline comment>, 'ax', 'axiom', 'cl', 'class', 'conj'
, 'conjecture', 'cor', 'corollary', 'func', 'function', 'lem', 'lemma', 'post',
'postulate', 'pred', 'predicate', 'prf', 'proof', 'prop', 'proposition',
'theorem', 'thm' or '}'
")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag);

    [<TestMethod>]
    member this.TestTryParseUses003Diag () =
        ad.Clear()
        let input = """TestNamescpace {
    uses {  R
    theory {
        y 
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
      (FplParser, Error, (Ln: 3, Col: 5),
       DiagnosticMessage "'theory'
    Expecting: ' ', ',', 'alias' or '}'
    ")
    Diagnostic
      (FplParser, Error, (Ln: 4, Col: 9),
       DiagnosticMessage
         "'y'
    Expecting: ' ', <block or inline comment>, 'ax', 'axiom', 'cl', 'class', 'conj'
    , 'conjecture', 'cor', 'corollary', 'func', 'function', 'lem', 'lemma', 'post',
    'postulate', 'pred', 'predicate', 'prf', 'proof', 'prop', 'proposition',
    'theorem', 'thm' or '}'
    ")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag);

    [<TestMethod>]
    member this.TestTryParseUses004Diag () =
        ad.Clear()
        let input = """TestNamescpace {
    uses {  R a
    theory {
        y 
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
      (FplParser, Error, (Ln: 2, Col: 15),
       DiagnosticMessage "'a'
    Expecting: ' ', ',', 'alias' or '}'
    ")
    Diagnostic
      (FplParser, Error, (Ln: 4, Col: 9),
       DiagnosticMessage
         "'y'
    Expecting: ' ', <block or inline comment>, 'ax', 'axiom', 'cl', 'class', 'conj'
    , 'conjecture', 'cor', 'corollary', 'func', 'function', 'lem', 'lemma', 'post',
    'postulate', 'pred', 'predicate', 'prf', 'proof', 'prop', 'proposition',
    'theorem', 'thm' or '}'
    ")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag);

    [<TestMethod>]
    member this.TestTryParseUses005Diag () =
        ad.Clear()
        let input = """TestNamescpace {
    uses {  R alias
    theory {
        y 
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 3, Col: 5),
   DiagnosticMessage "'theory'
Expecting: <PascalCaseId, e.g. 'SomeId'>
")
Diagnostic
  (FplParser, Error, (Ln: 4, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: ' ', <block or inline comment>, 'ax', 'axiom', 'cl', 'class', 'conj'
, 'conjecture', 'cor', 'corollary', 'func', 'function', 'lem', 'lemma', 'post',
'postulate', 'pred', 'predicate', 'prf', 'proof', 'prop', 'proposition',
'theorem', 'thm' or '}'
")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag);

    [<TestMethod>]
    member this.TestTryParseUses006Diag () =
        ad.Clear()
        let input = """TestNamescpace {
    uses {  R alias s
    theory {
        y 
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
      (FplParser, Error, (Ln: 2, Col: 21),
       DiagnosticMessage "'s'
    Expecting: <PascalCaseId, e.g. 'SomeId'>
    ")
    Diagnostic
      (FplParser, Error, (Ln: 4, Col: 9),
       DiagnosticMessage
         "'y'
    Expecting: ' ', <block or inline comment>, 'ax', 'axiom', 'cl', 'class', 'conj'
    , 'conjecture', 'cor', 'corollary', 'func', 'function', 'lem', 'lemma', 'post',
    'postulate', 'pred', 'predicate', 'prf', 'proof', 'prop', 'proposition',
    'theorem', 'thm' or '}'
    ")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag);

    [<TestMethod>]
    member this.TestTryParseUses007Diag () =
        ad.Clear()
        let input = """TestNamescpace {
    uses {  R alias I
    theory {
        y 
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
      (FplParser, Error, (Ln: 3, Col: 5),
       DiagnosticMessage "'theory'
    Expecting: ',' or '}'
    ")
    Diagnostic
      (FplParser, Error, (Ln: 4, Col: 9),
       DiagnosticMessage
         "'y'
    Expecting: ' ', <block or inline comment>, 'ax', 'axiom', 'cl', 'class', 'conj'
    , 'conjecture', 'cor', 'corollary', 'func', 'function', 'lem', 'lemma', 'post',
    'postulate', 'pred', 'predicate', 'prf', 'proof', 'prop', 'proposition',
    'theorem', 'thm' or '}'
    ")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag);

    [<TestMethod>]
    member this.TestTryParseUses009Diag () =
        ad.Clear()
        let input = """TestNamescpace {
    uses {  a alias I }
    theory {
        y 
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 2, Col: 13),
   DiagnosticMessage
     "'a'
Expecting: ' ' or <namespace or aliased namespace, e.g. 'RefNs'>
")
Diagnostic
  (FplParser, Error, (Ln: 4, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: ' ', <block or inline comment>, 'ax', 'axiom', 'cl', 'class', 'conj'
, 'conjecture', 'cor', 'corollary', 'func', 'function', 'lem', 'lemma', 'post',
'postulate', 'pred', 'predicate', 'prf', 'proof', 'prop', 'proposition',
'theorem', 'thm' or '}'
")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag);

    [<TestMethod>]
    member this.TestTryParseInference000Diag () =
        ad.Clear()
        let input = """TestNamescpace {
    i
    theory {   
        pred I()
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """ """
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, actualDiag);

    [<TestMethod>]
    member this.TestTryParseInference001Diag () =
        ad.Clear()
        let input = """TestNamescpace {
    inf
    theory {   
        pred I()
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """ """
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, actualDiag);

    [<TestMethod>]
    member this.TestTryParseInference002Diag () =
        ad.Clear()
        let input = """TestNamescpace {
    inf {
    theory {   
        pred I()
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """ """
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, actualDiag);

    [<TestMethod>]
    member this.TestTryParseInference003Diag () =
        ad.Clear()
        let input = """TestNamescpace {
    inf {
        x
    theory {   
        pred I()
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """ """
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, actualDiag);

    [<TestMethod>]
    member this.TestTryParseInference004Diag () =
        ad.Clear()
        let input = """TestNamescpace {
    inf {
        D
    theory {   
        pred I()
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """ """
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, actualDiag);

    [<TestMethod>]
    member this.TestTryParseInference005Diag () =
        ad.Clear()
        let input = """TestNamescpace {
    inf {
        D(
    theory {   
        pred I()
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """ """
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, actualDiag);

    [<TestMethod>]
    member this.TestTryParseInference006Diag () =
        ad.Clear()
        let input = """TestNamescpace {
    inf {
        D()
    theory {   
        pred I()
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """ """
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, actualDiag);

    [<TestMethod>]
    member this.TestTryParseInference007Diag () =
        ad.Clear()
        let input = """TestNamescpace {
    inf {
        D() {
    theory {   
        pred I()
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """ """
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, actualDiag);

    [<TestMethod>]
    member this.TestTryParseInference008Diag () =
        ad.Clear()
        let input = """TestNamescpace {
    inf {
        D() {
        }
    theory {   
        pred I()
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """ """
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, actualDiag);

    [<TestMethod>]
    member this.TestTryParseInference009Diag () =
        ad.Clear()
        let input = """TestNamescpace {
    inf {
        D() {
        }
    }
    theory {   
        pred I()
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """ """
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, actualDiag);
