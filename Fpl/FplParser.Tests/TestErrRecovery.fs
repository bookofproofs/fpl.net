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
        let result = lengthOfStartingFplKeyword test
        Assert.AreNotEqual(0, result)

    [<TestMethod>]
    [<DataRow("xxx")>]
    [<DataRow("xxx ")>]
    [<DataRow("xxx\n")>]
    [<DataRow("xxx\t")>]
    [<DataRow("xxx(")>]
    [<DataRow("xxx{")>]
    member this.TestNotStartWithFplKeyword(test: string) =
        let result = lengthOfStartingFplKeyword test
        Assert.AreEqual(0, result)

    [<TestMethod>]
    member this.TestTryParseExtension000Diag () =
        ad.Clear()
        let result = fplParser """TestNamespace {
    x
    theory {
        y 
    }
}"""
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
      (FplParser, Error, (Ln: 2, Col: 5),
       DiagnosticMessage
         "'x'
    Expecting: ':ext', 'inf', 'inference', 'th', 'theory', 'uses', <block comment>, <inline comment>, 
    <significant whitespace>
    ")
    Diagnostic
      (FplParser, Error, (Ln: 4, Col: 9),
       DiagnosticMessage
         "'y'
    Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
    'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
    'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
    <inline comment>, <significant whitespace>
    ")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag)

    [<TestMethod>]
    member this.TestTryParseExtension001Diag () =
        ad.Clear()
        let result = fplParser """TestNamespace {
    :
    theory {
        y
    }
}"""
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
      (FplParser, Error, (Ln: 2, Col: 5),
       DiagnosticMessage
         "':'
    Expecting: ':ext', 'inf', 'inference', 'th', 'theory', 'uses', <block comment>, <inline comment>, 
    <significant whitespace>
    ")
    Diagnostic
      (FplParser, Error, (Ln: 4, Col: 9),
       DiagnosticMessage
         "'y'
    Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
    'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
    'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
    <inline comment>, <significant whitespace>
    ")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag)

    [<TestMethod>]
    member this.TestTryParseExtension002Diag () =
        ad.Clear()
        let result = fplParser """TestNamespace {
    :ext
    theory {
        y
    }
}"""
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 3, Col: 5),
   DiagnosticMessage "'theory'
Expecting: <PascalCaseId>
")
Diagnostic
  (FplParser, Error, (Ln: 4, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag)

    [<TestMethod>]
    member this.TestTryParseExtension003Diag () =
        ad.Clear()
        let result = fplParser """TestNamespace {
    :ext T
    theory {
        y
    }
}"""
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 3, Col: 5),
   DiagnosticMessage "'theory'
Expecting: ':'
")
Diagnostic
  (FplParser, Error, (Ln: 4, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag)

    [<TestMethod>]
    member this.TestTryParseExtension004Diag () =
        ad.Clear()
        let result = fplParser """TestNamespace {
    :ext T:
    theory {
        y
    }
}"""
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
      (FplParser, Error, (Ln: 3, Col: 5),
       DiagnosticMessage "'theory'
    Expecting: <extension regex>
    ")
    Diagnostic
      (FplParser, Error, (Ln: 4, Col: 9),
       DiagnosticMessage
         "'y'
    Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
    'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
    'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
    <inline comment>, <significant whitespace>
    ")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag)

    [<TestMethod>]
    member this.TestTryParseExtension005Diag () =
        ad.Clear()
        let result = fplParser """TestNamespace {
    :ext T: /d/
    theory {
        y
    }
}"""
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
      (FplParser, Error, (Ln: 3, Col: 5),
       DiagnosticMessage "'theory'
    Expecting: ':end'
    ")
    Diagnostic
      (FplParser, Error, (Ln: 4, Col: 9),
       DiagnosticMessage
         "'y'
    Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
    'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
    'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
    <inline comment>, <significant whitespace>
    ")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag)


    [<TestMethod>]
    member this.TestTryParseExtension006Diag () =
        ad.Clear()
        let result = fplParser """TestNamespace {
    :ext T: /d/ :end
    theory {
        y
    }
}"""
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
      (FplParser, Error, (Ln: 4, Col: 9),
       DiagnosticMessage
         "'y'
    Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
    'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
    'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
    <inline comment>, <significant whitespace>
    ")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag)


    [<TestMethod>]
    member this.TestTryParseUses001Diag () =
        ad.Clear()
        let input = """TestNamespace {
    uses
    theory {
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 3, Col: 5),
   DiagnosticMessage "'theory'
Expecting: '{'
")
Diagnostic
  (FplParser, Error, (Ln: 4, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag)

    [<TestMethod>]
    member this.TestTryParseUses002Diag () =
        ad.Clear()
        let input = """TestNamespace {
    uses {  
    theory {
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 3, Col: 5),
   DiagnosticMessage "'theory'
Expecting: <PascalCaseId>, <whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 4, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag)

    [<TestMethod>]
    member this.TestTryParseUses003Diag () =
        ad.Clear()
        let input = """TestNamespace {
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
    Expecting: ',', 'alias', '}', <whitespace>
    ")
    Diagnostic
      (FplParser, Error, (Ln: 4, Col: 9),
       DiagnosticMessage
         "'y'
    Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
    'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
    'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
    <inline comment>, <significant whitespace>
    ")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag)

    [<TestMethod>]
    member this.TestTryParseUses004Diag () =
        ad.Clear()
        let input = """TestNamespace {
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
Expecting: ',', 'alias', '}', <whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 4, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag)

    [<TestMethod>]
    member this.TestTryParseUses005Diag () =
        ad.Clear()
        let input = """TestNamespace {
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
    Expecting: <PascalCaseId>
    ")
    Diagnostic
      (FplParser, Error, (Ln: 4, Col: 9),
       DiagnosticMessage
         "'y'
    Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
    'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
    'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
    <inline comment>, <significant whitespace>
    ")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag)

    [<TestMethod>]
    member this.TestTryParseUses006Diag () =
        ad.Clear()
        let input = """TestNamespace {
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
    Expecting: <PascalCaseId>
    ")
    Diagnostic
      (FplParser, Error, (Ln: 4, Col: 9),
       DiagnosticMessage
         "'y'
    Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
    'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
    'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
    <inline comment>, <significant whitespace>
    ")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag)

    [<TestMethod>]
    member this.TestTryParseUses007Diag () =
        ad.Clear()
        let input = """TestNamespace {
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
Expecting: ',', '}'
")
Diagnostic
  (FplParser, Error, (Ln: 4, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag)

    [<TestMethod>]
    member this.TestTryParseUses009Diag () =
        ad.Clear()
        let input = """TestNamespace {
    uses {  a alias I }
    theory {
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
      (FplParser, Error, (Ln: 2, Col: 13),
       DiagnosticMessage "'a'
    Expecting: <PascalCaseId>, <whitespace>
    ")
    Diagnostic
      (FplParser, Error, (Ln: 4, Col: 9),
       DiagnosticMessage
         "'y'
    Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
    'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
    'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
    <inline comment>, <significant whitespace>
    ")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag)

    [<TestMethod>]
    member this.TestTryParseInference000Diag () =
        ad.Clear()
        let input = """TestNamespace {
    i
    theory {   
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 2, Col: 5),
   DiagnosticMessage
     "'i'
Expecting: ':ext', 'inf', 'inference', 'th', 'theory', 'uses', <block comment>, <inline comment>, 
<significant whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 4, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag)

    [<TestMethod>]
    member this.TestTryParseInference001Diag () =
        ad.Clear()
        let input = """TestNamespace {
    inf
    theory {   
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 3, Col: 5),
   DiagnosticMessage "'theory'
Expecting: '{'
")
Diagnostic
  (FplParser, Error, (Ln: 4, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag)

    [<TestMethod>]
    member this.TestTryParseInference002Diag () =
        ad.Clear()
        let input = """TestNamespace {
    inf {
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
Expecting: <PascalCaseId>, <block comment>, <inline comment>, <significant whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 4, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag)

    [<TestMethod>]
    member this.TestTryParseInference003Diag () =
        ad.Clear()
        let input = """TestNamespace {
    inf {
        x
    theory {   
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 3, Col: 9),
   DiagnosticMessage
     "'x'
Expecting: <PascalCaseId>, <block comment>, <inline comment>, <significant whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 5, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag)

    [<TestMethod>]
    member this.TestTryParseInference004Diag () =
        ad.Clear()
        let input = """TestNamespace {
    inf {
        D
    theory {   
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 4, Col: 5),
   DiagnosticMessage "'theory'
Expecting: '(', <whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 5, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag)

    [<TestMethod>]
    member this.TestTryParseInference005Diag () =
        ad.Clear()
        let input = """TestNamespace {
    inf {
        D(
    theory {   
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 4, Col: 5),
   DiagnosticMessage "'theory'
Expecting: <variable (got keyword)>
")
Diagnostic
  (FplParser, Error, (Ln: 5, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag)

    [<TestMethod>]
    member this.TestTryParseInference006Diag () =
        ad.Clear()
        let input = """TestNamespace {
    inf {
        D()
    theory {   
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
      (FplParser, Error, (Ln: 4, Col: 5),
       DiagnosticMessage "'theory'
    Expecting: '{'
    ")
    Diagnostic
      (FplParser, Error, (Ln: 5, Col: 9),
       DiagnosticMessage
         "'y'
    Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
    'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
    'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
    <inline comment>, <significant whitespace>
    ")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag)

    [<TestMethod>]
    member this.TestTryParseInference007Diag () =
        ad.Clear()
        let input = """TestNamespace {
    inf {
        D() {
    theory {   
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 4, Col: 5),
   DiagnosticMessage "'theory'
Expecting: <variable (got keyword)>
")
Diagnostic
  (FplParser, Error, (Ln: 5, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag)

    [<TestMethod>]
    member this.TestTryParseInference008Diag () =
        ad.Clear()
        let input = """TestNamespace {
    inf {
        D() {
        }
    theory {   
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 4, Col: 9),
   DiagnosticMessage
     "'}'
Expecting: '@', 'assert', 'cases', 'del', 'delegate', 'loop', 'pre', 'premise', 
'range', 'ret', 'return', 'self', <PascalCaseId>, <block comment>, <digits>, <indexed variable>, 
<inline comment>, <significant whitespace>, <variable>
")
Diagnostic
  (FplParser, Error, (Ln: 5, Col: 5),
   DiagnosticMessage
     "'theory'
Expecting: '}', <PascalCaseId>, <block comment>, <inline comment>, <significant whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 6, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag)

    [<TestMethod>]
    member this.TestTryParseInference009Diag () =
        ad.Clear()
        let input = """TestNamespace {
    inf {
        D() { pre: true con: true }
    theory {   
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 4, Col: 5),
   DiagnosticMessage
     "'theory'
Expecting: '}', <PascalCaseId>, <block comment>, <inline comment>, <significant whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 5, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag)


    [<TestMethod>]
    member this.TestTryParseInference010Diag () =
        ad.Clear()
        let input = """TestNamespace {
    inf {
        D() { pre: true con: true } }
    theory {   
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 5, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag)

    [<TestMethod>]
    member this.TestTryParseNamespace000Diag () =
        ad.Clear()
        let input = """xTestNamespace {
    theory {   
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 1, Col: 1),
   DiagnosticMessage
     "'xTestNamespace'
Expecting: <PascalCaseId>, <whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 3, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag)

    [<TestMethod>]
    member this.TestTryParseNamespace001Diag () =
        ad.Clear()
        let input = """TestNamespace.x.D {
    theory {   
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 1, Col: 15),
   DiagnosticMessage "'x.D'
Expecting: <PascalCaseId>
")
Diagnostic
  (FplParser, Error, (Ln: 3, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag)

    [<TestMethod>]
    member this.TestTryParseNamespace002Diag () =
        ad.Clear()
        let input = """TestNamespace.x.D. {
    theory {   
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 1, Col: 15),
   DiagnosticMessage "'x.D.'
Expecting: <PascalCaseId>
")
Diagnostic
  (FplParser, Error, (Ln: 3, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag)

    [<TestMethod>]
    member this.TestTryParseNamespace003Diag () =
        ad.Clear()
        let input = """TestNamespace.x.D     
        theory {   
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 1, Col: 15),
   DiagnosticMessage "'x.D'
Expecting: <PascalCaseId>
")
Diagnostic
  (FplParser, Error, (Ln: 2, Col: 9),
   DiagnosticMessage
     "'theory'
Expecting: '{', <block comment>, <inline comment>, <significant whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 3, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 4, Col: 5),
   DiagnosticMessage "'}'
Expecting: <PascalCaseId>
")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag)

    [<TestMethod>]
    member this.TestTryParseNamespace004Diag () =
        ad.Clear()
        let input = """TestNamespace.D   D  {
        theory {   
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 1, Col: 19),
   DiagnosticMessage
     "'D'
Expecting: '{', <block comment>, <inline comment>, <significant whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 3, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 4, Col: 5),
   DiagnosticMessage "'}'
Expecting: <PascalCaseId>
")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag)

    [<TestMethod>]
    member this.TestTryParseNamespace005Diag () =
        ad.Clear()
        let input = """TestNamespace.D   ,  {
        theory {   
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 1, Col: 19),
   DiagnosticMessage
     "','
Expecting: '{', <block comment>, <inline comment>, <significant whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 3, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 4, Col: 5),
   DiagnosticMessage "'}'
Expecting: <PascalCaseId>
")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag)

    [<TestMethod>]
    member this.TestTryParseNamespace006Diag () =
        ad.Clear()
        let input = """TestNamespace.S.D   {
        theory {   
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 3, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag)

    [<TestMethod>]
    member this.TestTryParseAxiom000Diag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        a
    }
    y
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 3, Col: 9),
   DiagnosticMessage
     "'a'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 5, Col: 5),
   DiagnosticMessage
     "'y'
Expecting: 'loc', 'localization', '}', <block comment>, <inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag)

    [<TestMethod>]
    member this.TestTryParseAxiom001Diag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        ax
    }
    y
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 4, Col: 5),
   DiagnosticMessage "'}'
Expecting: <PascalCaseId>
")
Diagnostic
  (FplParser, Error, (Ln: 5, Col: 5),
   DiagnosticMessage
     "'y'
Expecting: 'loc', 'localization', '}', <block comment>, <inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag)

    [<TestMethod>]
    member this.TestTryParseAxiom002Diag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        ax T
    }
    y    
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 4, Col: 5),
   DiagnosticMessage "'}'
Expecting: '(', <whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 5, Col: 5),
   DiagnosticMessage
     "'y'
Expecting: 'loc', 'localization', '}', <block comment>, <inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag)

    [<TestMethod>]
    member this.TestTryParseAxiom003Diag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        ax T (
    }
    y
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 4, Col: 5),
   DiagnosticMessage "'}'
Expecting: ')', <variable>, <whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 5, Col: 5),
   DiagnosticMessage
     "'y'
Expecting: 'loc', 'localization', '}', <block comment>, <inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag)

    [<TestMethod>]
    member this.TestTryParseAxiom004Diag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        ax T ()
    }
    y
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 4, Col: 5), DiagnosticMessage "'}'
Expecting: '{'
")
Diagnostic
  (FplParser, Error, (Ln: 5, Col: 5),
   DiagnosticMessage
     "'y'
Expecting: 'loc', 'localization', '}', <block comment>, <inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag)

    [<TestMethod>]
    member this.TestTryParseAxiom005Diag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        ax T () 
        {
        }
    }
    y
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 5, Col: 9),
   DiagnosticMessage
     "'}'
Expecting: '@', 'all', 'and', 'assert', 'cases', 'del', 'delegate', 'ex', 
'false', 'iif', 'impl', 'is', 'loop', 'not', 'or', 'range', 
'ret', 'return', 'self', 'true', 'undef', 'undefined', 'xor', <PascalCaseId>, 
<argument identifier>, <block comment>, <digits>, <indexed variable>, <inline comment>, <significant whitespace>, <variable>
")
Diagnostic
  (FplParser, Error, (Ln: 7, Col: 5),
   DiagnosticMessage
     "'y'
Expecting: 'loc', 'localization', '}', <block comment>, <inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag)

    [<TestMethod>]
    member this.TestTryParseAxiom006Diag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        ax T () 
        {
            true
        }
    }
    y
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 8, Col: 5),
   DiagnosticMessage
     "'y'
Expecting: 'loc', 'localization', '}', <block comment>, <inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag)

    [<TestMethod>]
    member this.TestTryParseAxiom007Diag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        ax T () 
        {
            true
        }
    }
    y
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 8, Col: 5),
   DiagnosticMessage
     "'y'
Expecting: 'loc', 'localization', '}', <block comment>, <inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag)

    [<TestMethod>]
    member this.TestTryParseFunction000Diag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        func
    }
    y
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 4, Col: 5),
   DiagnosticMessage "'}'
Expecting: <PascalCaseId>
")
Diagnostic
  (FplParser, Error, (Ln: 5, Col: 5),
   DiagnosticMessage
     "'y'
Expecting: 'loc', 'localization', '}', <block comment>, <inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag)

    [<TestMethod>]
    member this.TestTryParseFunction001Diag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        func A
    }
    y
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 4, Col: 5),
   DiagnosticMessage "'}'
Expecting: '(', <whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 5, Col: 5),
   DiagnosticMessage
     "'y'
Expecting: 'loc', 'localization', '}', <block comment>, <inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag)

    [<TestMethod>]
    member this.TestTryParseFunction002Diag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        func A (
    }
    y
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 4, Col: 5),
   DiagnosticMessage "'}'
Expecting: ')', <variable>, <whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 5, Col: 5),
   DiagnosticMessage
     "'y'
Expecting: 'loc', 'localization', '}', <block comment>, <inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag)

    [<TestMethod>]
    member this.TestTryParseFunction003Diag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        func A ()
    }
    y
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 4, Col: 5), DiagnosticMessage "'}'
Expecting: '->'
")
Diagnostic
  (FplParser, Error, (Ln: 5, Col: 5),
   DiagnosticMessage
     "'y'
Expecting: 'loc', 'localization', '}', <block comment>, <inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag)

    [<TestMethod>]
    member this.TestTryParseFunction004Diag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        func A () ->
    }
    y
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 4, Col: 5),
   DiagnosticMessage
     "'}'
Expecting: '*', '+', '@', 'func', 'function', 'ind', 'index', 'obj', 
'object', 'pred', 'predicate', 'template', 'tpl', <PascalCaseId>
")
Diagnostic
  (FplParser, Error, (Ln: 5, Col: 5),
   DiagnosticMessage
     "'y'
Expecting: 'loc', 'localization', '}', <block comment>, <inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag)

    [<TestMethod>]
    member this.TestTryParseFunction005Diag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        func A () -> T
    }
    y
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 4, Col: 5),
   DiagnosticMessage
     "'}'
Expecting: '(', '<', '{', <(closed) left bound '['>, <(open) left bound '[!'>, <whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 5, Col: 5),
   DiagnosticMessage
     "'y'
Expecting: 'loc', 'localization', '}', <block comment>, <inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag)

    [<TestMethod>]
    member this.TestTryParseFunction006Diag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        func A () -> T 
        {
    }
    y
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 6, Col: 5),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag)

    [<TestMethod>]
    member this.TestTryParseClasses000Diag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        cl 
        {
        }
    }
    y
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 4, Col: 9),
   DiagnosticMessage "'{'
Expecting: <PascalCaseId>
")
Diagnostic
  (FplParser, Error, (Ln: 7, Col: 5),
   DiagnosticMessage
     "'y'
Expecting: 'loc', 'localization', '}', <block comment>, <inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag)

    [<TestMethod>]
    member this.TestTryParseClasses001Diag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        cl T
        {
        }
    }
    y
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 4, Col: 9),
   DiagnosticMessage "'{'
Expecting: ':', <whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 7, Col: 5),
   DiagnosticMessage
     "'y'
Expecting: 'loc', 'localization', '}', <block comment>, <inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag)

    [<TestMethod>]
    member this.TestTryParseClasses002Diag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        cl T:
        {
        }
    }
    y
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 4, Col: 9),
   DiagnosticMessage
     "'{'
Expecting: '@', 'obj', 'object', 'template', 'tpl', <PascalCaseId>, <whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 7, Col: 5),
   DiagnosticMessage
     "'y'
Expecting: 'loc', 'localization', '}', <block comment>, <inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag)

    [<TestMethod>]
    member this.TestTryParseClasses003Diag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        cl T: obj
        {
        }
    }
    y
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 7, Col: 5),
   DiagnosticMessage
     "'y'
Expecting: 'loc', 'localization', '}', <block comment>, <inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag)

    [<TestMethod>]
    member this.TestTryParseVarDecl000Diag () =
        ad.Clear()
        let input = """TestNamespace {
    inf {
        D(x)
        {
            pre:true
            con:true
        }
    }
    theory {   
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 3, Col: 12),
   DiagnosticMessage "')'
Expecting: ',', ':', <whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 10, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag)

    [<TestMethod>]
    member this.TestTryParseVarDecl001Diag () =
        ad.Clear()
        let input = """TestNamespace {
    inf {
        D(x, )
        {
            pre:true
            con:true
        }
    }
    theory {   
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 3, Col: 14),
   DiagnosticMessage "')'
Expecting: <variable>
")
Diagnostic
  (FplParser, Error, (Ln: 10, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag)

    [<TestMethod>]
    member this.TestTryParseVarDecl002Diag () =
        ad.Clear()
        let input = """TestNamespace {
    inf {
        D(x: )
        {
            pre:true
            con:true
        }
    }
    theory {   
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 3, Col: 14),
   DiagnosticMessage
     "')'
Expecting: '*', '+', '@', 'func', 'function', 'ind', 'index', 'obj', 
'object', 'pred', 'predicate', 'template', 'tpl', <PascalCaseId>, <whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 10, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag)

    [<TestMethod>]
    member this.TestTryParseVarDecl003aDiag () =
        ad.Clear()
        let input = """TestNamespace {
    inf {
        D(x:* )
        {
            pre:true
            con:true
        }
    }
    theory {   
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 3, Col: 15),
   DiagnosticMessage
     "')'
Expecting: '@', 'func', 'function', 'ind', 'index', 'obj', 'object', 'pred', 
'predicate', 'template', 'tpl', <PascalCaseId>
")
Diagnostic
  (FplParser, Error, (Ln: 10, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag)

    [<TestMethod>]
    member this.TestTryParseVarDecl003_Diag () =
        ad.Clear()
        let input = """TestNamespace {
    inf {
        D(x:, )
        {
            pre:true
            con:true
        }
    }
    theory {   
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 3, Col: 13),
   DiagnosticMessage
     "','
Expecting: '*', '+', '@', 'func', 'function', 'ind', 'index', 'obj', 
'object', 'pred', 'predicate', 'template', 'tpl', <PascalCaseId>, <whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 10, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag)

    [<TestMethod>]
    member this.TestTryParseVarDecl003bDiag () =
        ad.Clear()
        let input = """TestNamespace {
    inf {
        D(x:+ )
        {
            pre:true
            con:true
        }
    }
    theory {   
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
      (FplParser, Error, (Ln: 3, Col: 15),
       DiagnosticMessage
         "')'
    Expecting: '@', 'func', 'function', 'ind', 'index', 'obj', 'object', 'pred', 
    'predicate', 'template', 'tpl', <PascalCaseId>
    ")
    Diagnostic
      (FplParser, Error, (Ln: 10, Col: 9),
       DiagnosticMessage
         "'y'
    Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
    'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
    'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
    <inline comment>, <significant whitespace>
    ")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag)

    [<TestMethod>]
    member this.TestTryParseVarDecl003cDiag () =
        ad.Clear()
        let input = """TestNamespace {
    inf {
        D(x:@ )
        {
            pre:true
            con:true
        }
    }
    theory {   
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 3, Col: 14),
   DiagnosticMessage "')'
Expecting: <PascalCaseId>
")
Diagnostic
  (FplParser, Error, (Ln: 10, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag)

    [<TestMethod>]
    member this.TestTryParseVarDecl003dDiag () =
        ad.Clear()
        let input = """TestNamespace {
    inf {
        D(x:func )
        {
            pre:true
            con:true
        }
    }
    theory {   
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 10, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag)

    [<TestMethod>]
    member this.TestTryParseVarDecl003eDiag () =
        ad.Clear()
        let input = """TestNamespace {
    inf {
        D(x:ind )
        {
            pre:true
            con:true
        }
    }
    theory {   
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 10, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag)

    [<TestMethod>]
    member this.TestTryParseVarDecl003fDiag () =
        ad.Clear()
        let input = """TestNamespace {
    inf {
        D(x:obj )
        {
            pre:true
            con:true
        }
    }
    theory {   
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 10, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag)

    [<TestMethod>]
    member this.TestTryParseVarDecl003gDiag () =
        ad.Clear()
        let input = """TestNamespace {
    inf {
        D(x:pred )
        {
            pre:true
            con:true
        }
    }
    theory {   
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 10, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag)

    [<TestMethod>]
    member this.TestTryParseVarDecl003hDiag () =
        ad.Clear()
        let input = """TestNamespace {
    inf {
        D(x:tpl )
        {
            pre:true
            con:true
        }
    }
    theory {   
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 10, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag)

    [<TestMethod>]
    member this.TestTryParseVarDecl003iDiag () =
        ad.Clear()
        let input = """TestNamespace {
    inf {
        D(x:T )
        {
            pre:true
            con:true
        }
    }
    theory {   
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 10, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag)

    [<TestMethod>]
    member this.TestTryParseVarDecl003i000Diag () =
        ad.Clear()
        let input = """TestNamespace {
    inf {
        D(x:tpl. )
        {
            pre:true
            con:true
        }
    }
    theory {   
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 3, Col: 16),
   DiagnosticMessage
     "'.'
Expecting: '(', ')', ',', '<', <(closed) left bound '['>, <(open) left bound '[!'>, <PascalCaseId>, <digits>, 
<whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 10, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag)

    [<TestMethod>]
    member this.TestTryParseVarDecl003i001Diag () =
        ad.Clear()
        let input = """TestNamespace {
    inf {
        D(x:tpl[ )
        {
            pre:true
            con:true
        }
    }
    theory {   
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 3, Col: 18),
   DiagnosticMessage
     "')'
Expecting: '@', 'del', 'delegate', 'self', '~', <PascalCaseId>, <digits>, <indexed variable>, 
<variable>
")
Diagnostic
  (FplParser, Error, (Ln: 10, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag)

    [<TestMethod>]
    member this.TestTryParseVarDecl003i002Diag () =
        ad.Clear()
        let input = """TestNamespace {
    inf {
        D(x:tpl[! )
        {
            pre:true
            con:true
        }
    }
    theory {   
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 3, Col: 19),
   DiagnosticMessage
     "')'
Expecting: '@', 'del', 'delegate', 'self', '~', <PascalCaseId>, <digits>, <indexed variable>, 
<variable>
")
Diagnostic
  (FplParser, Error, (Ln: 10, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag)

    [<TestMethod>]
    member this.TestTryParseVarDecl003i003Diag () =
        ad.Clear()
        let input = """TestNamespace {
    inf {
        D(x:tpl< )
        {
            pre:true
            con:true
        }
    }
    theory {   
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
      (FplParser, Error, (Ln: 3, Col: 18),
       DiagnosticMessage
         "')'
    Expecting: '@', 'del', 'delegate', 'self', <PascalCaseId>, <digits>, <indexed variable>, <variable>
    ")
    Diagnostic
      (FplParser, Error, (Ln: 10, Col: 9),
       DiagnosticMessage
         "'y'
    Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
    'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
    'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
    <inline comment>, <significant whitespace>
    ")"""
        let actualDiag = ad.DiagnosticsToString
        Assert.AreEqual(replaceWhiteSpace expectedDiag, replaceWhiteSpace actualDiag)



