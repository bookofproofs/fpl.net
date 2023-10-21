namespace FplParser.Tests

open FParsec
open FplGrammarCommons
open ErrRecovery
open FplParser
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
  (FplParser, Error, (Ln: 3, Col: 5),
   DiagnosticMessage "'theory'
Expecting: <PascalCaseId>
")
Diagnostic
  (FplParser, Error, (Ln: 4, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: <PascalCaseId>, <block comment>, <inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(3, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:<PascalCaseId>,<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


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
  (FplParser, Error, (Ln: 3, Col: 5),
   DiagnosticMessage "'theory'
Expecting: <PascalCaseId>
")
Diagnostic
  (FplParser, Error, (Ln: 4, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: <PascalCaseId>, <block comment>, <inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(3, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:<PascalCaseId>,<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


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
Expecting: <PascalCaseId>, <block comment>, <inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:<PascalCaseId>,<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


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
    Expecting: <PascalCaseId>, <block comment>, <inline comment>, <significant whitespace>
    ")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:<PascalCaseId>,<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


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
    Expecting: <PascalCaseId>, <block comment>, <inline comment>, <significant whitespace>
    ")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:<PascalCaseId>,<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


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
    Expecting: <PascalCaseId>, <block comment>, <inline comment>, <significant whitespace>
    ")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:<PascalCaseId>,<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))



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
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(1, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))



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
Expecting: <PascalCaseId>
")
Diagnostic
  (FplParser, Error, (Ln: 4, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: <PascalCaseId>, <block comment>, <inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:<PascalCaseId>,<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


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
  (FplParser, Error, (Ln: 2, Col: 10),
   DiagnosticMessage "'{'
Expecting: <PascalCaseId>
")
Diagnostic
  (FplParser, Error, (Ln: 3, Col: 5),
   DiagnosticMessage
     "'theory'
Expecting: <PascalCaseId>, <block comment>, <inline comment>, <significant whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 4, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'dec', 'declaration', 'pre', 'premise', 'spec', 'specification', <block comment>, <inline comment>, 
<significant whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 6, Col: 57),
   DiagnosticMessage
     "Expecting: '}', <block comment>, <inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(4, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


    [<TestMethod>]
    member this.TestTryParseUses003Diag () =
        ad.Clear()
        let input = """TestNamespace {
    uses   R
    theory {
        y
    }
}"""
        let result = fplParser input
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
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(1, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


    [<TestMethod>]
    member this.TestTryParseUses004Diag () =
        ad.Clear()
        let input = """TestNamespace {
    uses   R a
    theory {
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
      (FplParser, Error, (Ln: 2, Col: 14),
       DiagnosticMessage
         "'a'
    Expecting: ',', 'alias', 'inf', 'inference', 'th', 'theory', <block comment>, <inline comment>, 
    <significant whitespace>, <whitespace>
    ")
    Diagnostic
      (FplParser, Error, (Ln: 4, Col: 9),
       DiagnosticMessage
         "'y'
    Expecting: 'dec', 'declaration', 'pre', 'premise', 'spec', 'specification', <block comment>, <inline comment>, 
    <significant whitespace>
    ")
    Diagnostic
      (FplParser, Error, (Ln: 6, Col: 57),
       DiagnosticMessage
         "Expecting: '}', <block comment>, <inline comment>, <significant whitespace>
    ")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(3, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


    [<TestMethod>]
    member this.TestTryParseUses005Diag () =
        ad.Clear()
        let input = """TestNamespace {
    uses   R alias
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
    Expecting: <PascalCaseId>, <block comment>, <inline comment>, <significant whitespace>
    ")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:<PascalCaseId>,<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


    [<TestMethod>]
    member this.TestTryParseUses006Diag () =
        ad.Clear()
        let input = """TestNamespace {
    uses   R alias s
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
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


    [<TestMethod>]
    member this.TestTryParseUses007Diag () =
        ad.Clear()
        let input = """TestNamespace {
    uses   R alias I
    theory {
        y
    }
}"""
        let result = fplParser input
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
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(1, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


    [<TestMethod>]
    member this.TestTryParseUses009Diag () =
        ad.Clear()
        let input = """TestNamespace {
    uses   a alias I 
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
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


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
  (FplParser, Error, (Ln: 3, Col: 5),
   DiagnosticMessage "'theory'
Expecting: <PascalCaseId>
")
Diagnostic
  (FplParser, Error, (Ln: 4, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: <PascalCaseId>, <block comment>, <inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(3, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:<PascalCaseId>,<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


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
Expecting: '{', <whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 4, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'dec', 'declaration', 'pre', 'premise', 'spec', 'specification', <block comment>, <inline comment>, 
<significant whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 6, Col: 57),
   DiagnosticMessage
     "Expecting: '}', <block comment>, <inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(3, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


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
    Expecting: 'dec', 'declaration', 'pre', 'premise', 'spec', 'specification', <block comment>, <inline comment>, 
    <significant whitespace>
    ")
    Diagnostic
      (FplParser, Error, (Ln: 6, Col: 57),
       DiagnosticMessage
         "Expecting: '}', <block comment>, <inline comment>, <significant whitespace>
    ")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(3, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


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
      (FplParser, Error, (Ln: 4, Col: 5),
       DiagnosticMessage "'theory'
    Expecting: '(', <whitespace>
    ")
    Diagnostic
      (FplParser, Error, (Ln: 5, Col: 9),
       DiagnosticMessage
         "'y'
    Expecting: 'dec', 'declaration', 'pre', 'premise', 'spec', 'specification', <block comment>, <inline comment>, 
    <significant whitespace>
    ")
    Diagnostic
      (FplParser, Error, (Ln: 7, Col: 57),
       DiagnosticMessage
         "Expecting: '}', <block comment>, <inline comment>, <significant whitespace>
    ")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(4, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


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
    Expecting: 'dec', 'declaration', 'pre', 'premise', 'spec', 'specification', <block comment>, <inline comment>, 
    <significant whitespace>
    ")
    Diagnostic
      (FplParser, Error, (Ln: 7, Col: 57),
       DiagnosticMessage
         "Expecting: '}', <block comment>, <inline comment>, <significant whitespace>
    ")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(3, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


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
       DiagnosticMessage
         "'theory'
    Expecting: ')', <variable (got keyword)>, <whitespace>
    ")
    Diagnostic
      (FplParser, Error, (Ln: 5, Col: 9),
       DiagnosticMessage
         "'y'
    Expecting: 'dec', 'declaration', 'pre', 'premise', 'spec', 'specification', <block comment>, <inline comment>, 
    <significant whitespace>
    ")
    Diagnostic
      (FplParser, Error, (Ln: 7, Col: 57),
       DiagnosticMessage
         "Expecting: '}', <block comment>, <inline comment>, <significant whitespace>
    ")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(3, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


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
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


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
       DiagnosticMessage
         "'theory'
    Expecting: 'dec', 'declaration', 'pre', 'premise', 'spec', 'specification', <block comment>, <inline comment>, 
    <significant whitespace>
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
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


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
    Expecting: 'dec', 'declaration', 'pre', 'premise', 'spec', 'specification', <block comment>, <inline comment>, 
    <significant whitespace>
    ")
    Diagnostic
      (FplParser, Error, (Ln: 5, Col: 5),
       DiagnosticMessage
         "'theory'
    Expecting: '}', <PascalCaseId>, <block comment>, <inline comment>, <significant whitespace>, <whitespace>
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
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(3, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


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
    Expecting: '}', <PascalCaseId>, <block comment>, <inline comment>, <significant whitespace>, <whitespace>
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
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))



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
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(1, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


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
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


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
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


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
  (FplParser, Error, (Ln: 1, Col: 19),
   DiagnosticMessage "'{'
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
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(3, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


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
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'{',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


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
  (FplParser, Error, (Ln: 2, Col: 9),
   DiagnosticMessage
     "'theory'
Expecting: <PascalCaseId>, <block comment>, <inline comment>, <significant whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 3, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'dec', 'declaration', 'pre', 'premise', 'spec', 'specification', <block comment>, <inline comment>, 
<significant whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 5, Col: 57),
   DiagnosticMessage
     "Expecting: '}', <block comment>, <inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(4, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


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
  (FplParser, Error, (Ln: 2, Col: 9),
   DiagnosticMessage
     "'theory'
Expecting: <PascalCaseId>, <block comment>, <inline comment>, <significant whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 3, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'dec', 'declaration', 'pre', 'premise', 'spec', 'specification', <block comment>, <inline comment>, 
<significant whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 5, Col: 57),
   DiagnosticMessage
     "Expecting: '}', <block comment>, <inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(4, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


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
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(1, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


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
  (FplParser, Error, (Ln: 4, Col: 5),
   DiagnosticMessage
     "'y'
Expecting: 'loc', 'localization', '}', <block comment>, <inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'loc','localization','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))

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
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'loc','localization','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


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
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'loc','localization','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


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
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'loc','localization','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


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
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'loc','localization','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


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
Expecting: '<', '@', 'all', 'and', 'dec', 'declaration', 'del', 'delegate', 
'ex', 'false', 'iif', 'impl', 'is', 'not', 'or', 'self', 
'spec', 'specification', 'true', 'undef', 'undefined', 'xor', <PascalCaseId>, <argument identifier>, 
<block comment>, <digits>, <inline comment>, <significant whitespace>, <variable>
")
Diagnostic
  (FplParser, Error, (Ln: 7, Col: 5),
   DiagnosticMessage
     "'y'
Expecting: 'loc', 'localization', '}', <block comment>, <inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'loc','localization','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


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
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(1, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'loc','localization','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


    [<TestMethod>]
    member this.TestTryParseAxiom007Diag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        ax T () 
        {
            intrinsic
        }
    }
    y
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 5, Col: 13),
   DiagnosticMessage
     "'intrinsic'
Expecting: '<', '@', 'all', 'and', 'dec', 'declaration', 'del', 'delegate', 
'ex', 'false', 'iif', 'impl', 'is', 'not', 'or', 'self', 
'spec', 'specification', 'true', 'undef', 'undefined', 'xor', <PascalCaseId>, <argument identifier>, 
<block comment>, <digits>, <inline comment>, <significant whitespace>, <variable (got keyword)>
")
Diagnostic
  (FplParser, Error, (Ln: 8, Col: 5),
   DiagnosticMessage
     "'y'
Expecting: 'loc', 'localization', '}', <block comment>, <inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'loc','localization','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


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
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'loc','localization','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


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
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'loc','localization','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


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
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'loc','localization','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


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
  (FplParser, Error, (Ln: 4, Col: 5),
   DiagnosticMessage "'}'
Expecting: '->', <whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 5, Col: 5),
   DiagnosticMessage
     "'y'
Expecting: 'loc', 'localization', '}', <block comment>, <inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'loc','localization','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


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
Expecting: '@', 'func', 'function', 'ind', 'index', 'obj', 'object', 'pred', 
'predicate', 'template', 'tpl', <PascalCaseId>
")
Diagnostic
  (FplParser, Error, (Ln: 5, Col: 5),
   DiagnosticMessage
     "'y'
Expecting: 'loc', 'localization', '}', <block comment>, <inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'loc','localization','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


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
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'loc','localization','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


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
  (FplParser, Error, (Ln: 5, Col: 5),
   DiagnosticMessage
     "'}'
Expecting: 'dec', 'declaration', 'intr', 'intrinsic', 'ret', 'return', 'spec', 'specification', 
<block comment>, <inline comment>, <significant whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 6, Col: 5),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>, <whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 6, Col: 54),
   DiagnosticMessage
     "Expecting: '}', <block comment>, <inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(3, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


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
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'loc','localization','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


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
Expecting: ':', ':*', ':+', <whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 7, Col: 5),
   DiagnosticMessage
     "'y'
Expecting: 'loc', 'localization', '}', <block comment>, <inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'loc','localization','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


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
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'loc','localization','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


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
  (FplParser, Error, (Ln: 5, Col: 9),
   DiagnosticMessage
     "'}'
Expecting: 'dec', 'declaration', 'intr', 'intrinsic', 'spec', 'specification', <block comment>, <inline comment>, 
<significant whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 7, Col: 5),
   DiagnosticMessage
     "'y'
Expecting: 'loc', 'localization', '}', <block comment>, <inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'loc','localization','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


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
Expecting: ',', ':', ':*', ':+', <whitespace>
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
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))

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
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


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
Expecting: '@', 'func', 'function', 'ind', 'index', 'obj', 'object', 'pred', 
'predicate', 'template', 'tpl', <PascalCaseId>, <whitespace>
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
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


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
'predicate', 'template', 'tpl', <PascalCaseId>, <whitespace>
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
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


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
Expecting: '@', 'func', 'function', 'ind', 'index', 'obj', 'object', 'pred', 
'predicate', 'template', 'tpl', <PascalCaseId>, <whitespace>
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
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


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
'predicate', 'template', 'tpl', <PascalCaseId>, <whitespace>
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
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


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
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


    [<TestMethod>]
    member this.TestTryParseVarDecl003dDiag () =
        ad.Clear()
        let input = """TestNamespace {
    inf {
        D(x:func: )
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
  (FplParser, Error, (Ln: 3, Col: 17),
   DiagnosticMessage "':'
Expecting: '(', ')', ',', <whitespace>
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
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


    [<TestMethod>]
    member this.TestTryParseVarDecl003eDiag () =
        ad.Clear()
        let input = """TestNamespace {
    inf {
        D(x:ind: )
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
   DiagnosticMessage "':'
Expecting: '(', ')', ',', <whitespace>
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
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


    [<TestMethod>]
    member this.TestTryParseVarDecl003fDiag () =
        ad.Clear()
        let input = """TestNamespace {
    inf {
        D(x:obj: )
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
     "':'
Expecting: '(', ')', ',', '<', <(closed) left bound '['>, <(open) left bound '[!'>, <whitespace>
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
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


    [<TestMethod>]
    member this.TestTryParseVarDecl003gDiag () =
        ad.Clear()
        let input = """TestNamespace {
    inf {
        D(x:pred: )
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
  (FplParser, Error, (Ln: 3, Col: 17),
   DiagnosticMessage "':'
Expecting: '(', ')', ',', <whitespace>
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
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


    [<TestMethod>]
    member this.TestTryParseVarDecl003hDiag () =
        ad.Clear()
        let input = """TestNamespace {
    inf {
        D(x:tpl: )
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
     "':'
Expecting: '(', ')', ',', '<', <(closed) left bound '['>, <(open) left bound '[!'>, <PascalCaseId>, <whitespace>
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
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


    [<TestMethod>]
    member this.TestTryParseVarDecl003iDiag () =
        ad.Clear()
        let input = """TestNamespace {
    inf {
        D(x:T: )
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
     "':'
Expecting: '(', ')', ',', '.', '<', <(closed) left bound '['>, <(open) left bound '[!'>, <whitespace>
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
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


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
Expecting: '(', ')', ',', '<', <(closed) left bound '['>, <(open) left bound '[!'>, <PascalCaseId>, <whitespace>
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
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


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
Expecting: '@', 'self', '~', <PascalCaseId>, <digits>, <variable>
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
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


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
Expecting: '@', 'self', '~', <PascalCaseId>, <digits>, <variable>
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
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


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
Expecting: '@', 'self', <PascalCaseId>, <digits>, <variable>
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
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))



    [<TestMethod>]
    member this.TestTryParseVarDeclInScopel000Diag () =
        ad.Clear()
        let input = """TestNamespace {
    inf {
        D()
        {
            dec: x ;
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
  (FplParser, Error, (Ln: 5, Col: 20),
   DiagnosticMessage "';'
Expecting: ',', ':', ':*', ':+', <whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 6, Col: 13),
   DiagnosticMessage
     "'pre:true'
Expecting: '@', 'func', 'function', 'ind', 'index', 'obj', 'object', 'pred', 
'predicate', 'template', 'tpl', <PascalCaseId>, <whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 7, Col: 13),
   DiagnosticMessage
     "'con:true'
Expecting: '(', ';', '<', <(closed) left bound '['>, <(open) left bound '[('>, <block comment>, <inline comment>, <significant whitespace>, 
<variable (got keyword)>, <whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 11, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(4, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


    [<TestMethod>]
    member this.TestTryParseVarDeclInScope001Diag () =
        ad.Clear()
        let input = """TestNamespace {
    inf {
        D()
        {
            x, 
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
  (FplParser, Error, (Ln: 5, Col: 13),
   DiagnosticMessage
     "'x,'
Expecting: 'dec', 'declaration', 'pre', 'premise', 'spec', 'specification', <block comment>, <inline comment>, 
<significant whitespace>
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
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


    [<TestMethod>]
    member this.TestTryParseVarDeclInScope002Diag () =
        ad.Clear()
        let input = """TestNamespace {
    inf {
        D()
        {
            x: 
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
  (FplParser, Error, (Ln: 5, Col: 13),
   DiagnosticMessage
     "'x:'
Expecting: 'dec', 'declaration', 'pre', 'premise', 'spec', 'specification', <block comment>, <inline comment>, 
<significant whitespace>
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
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


    [<TestMethod>]
    member this.TestTryParseVarDeclInScope003aDiag () =
        ad.Clear()
        let input = """TestNamespace {
    inf {
        D()
        {
            x:* 
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
  (FplParser, Error, (Ln: 5, Col: 13),
   DiagnosticMessage
     "'x:*'
Expecting: 'dec', 'declaration', 'pre', 'premise', 'spec', 'specification', <block comment>, <inline comment>, 
<significant whitespace>
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
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


    [<TestMethod>]
    member this.TestTryParseVarDeclInScope003_Diag () =
        ad.Clear()
        let input = """TestNamespace {
    inf {
        D()
        {
            x:, 
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
  (FplParser, Error, (Ln: 5, Col: 13),
   DiagnosticMessage
     "'x:,'
Expecting: 'dec', 'declaration', 'pre', 'premise', 'spec', 'specification', <block comment>, <inline comment>, 
<significant whitespace>
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
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


    [<TestMethod>]
    member this.TestTryParseVarDeclInScope003bDiag () =
        ad.Clear()
        let input = """TestNamespace {
    inf {
        D()
        {
            x:+ 
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
  (FplParser, Error, (Ln: 5, Col: 13),
   DiagnosticMessage
     "'x:+'
Expecting: 'dec', 'declaration', 'pre', 'premise', 'spec', 'specification', <block comment>, <inline comment>, 
<significant whitespace>
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
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


    [<TestMethod>]
    member this.TestTryParseVarDeclInScope003cDiag () =
        ad.Clear()
        let input = """TestNamespace {
    inf {
        D()
        {
            x:@ 
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
  (FplParser, Error, (Ln: 5, Col: 13),
   DiagnosticMessage
     "'x:@'
Expecting: 'dec', 'declaration', 'pre', 'premise', 'spec', 'specification', <block comment>, <inline comment>, 
<significant whitespace>
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
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


    [<TestMethod>]
    member this.TestTryParseVarDeclInScope003dDiag () =
        ad.Clear()
        let input = """TestNamespace {
    inf {
        D()
        {
            x:func: 
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
  (FplParser, Error, (Ln: 5, Col: 13),
   DiagnosticMessage
     "'x:func:'
Expecting: 'dec', 'declaration', 'pre', 'premise', 'spec', 'specification', <block comment>, <inline comment>, 
<significant whitespace>
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
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


    [<TestMethod>]
    member this.TestTryParseVarDeclInScope003eDiag () =
        ad.Clear()
        let input = """TestNamespace {
    inf {
        D()
        {
            x:ind: 
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
  (FplParser, Error, (Ln: 5, Col: 13),
   DiagnosticMessage
     "'x:ind:'
Expecting: 'dec', 'declaration', 'pre', 'premise', 'spec', 'specification', <block comment>, <inline comment>, 
<significant whitespace>
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
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


    [<TestMethod>]
    member this.TestTryParseVarDeclInScope003fDiag () =
        ad.Clear()
        let input = """TestNamespace {
    inf {
        D()
        {
            x:obj: 
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
  (FplParser, Error, (Ln: 5, Col: 13),
   DiagnosticMessage
     "'x:obj:'
Expecting: 'dec', 'declaration', 'pre', 'premise', 'spec', 'specification', <block comment>, <inline comment>, 
<significant whitespace>
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
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


    [<TestMethod>]
    member this.TestTryParseVarDeclInScope003gDiag () =
        ad.Clear()
        let input = """TestNamespace {
    inf {
        D()
        {
            x:pred: 
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
  (FplParser, Error, (Ln: 5, Col: 13),
   DiagnosticMessage
     "'x:pred:'
Expecting: 'dec', 'declaration', 'pre', 'premise', 'spec', 'specification', <block comment>, <inline comment>, 
<significant whitespace>
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
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


    [<TestMethod>]
    member this.TestTryParseVarDeclInScope003hDiag () =
        ad.Clear()
        let input = """TestNamespace {
    inf {
        D()
        {
            x:tpl: 
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
  (FplParser, Error, (Ln: 5, Col: 13),
   DiagnosticMessage
     "'x:tpl:'
Expecting: 'dec', 'declaration', 'pre', 'premise', 'spec', 'specification', <block comment>, <inline comment>, 
<significant whitespace>
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
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


    [<TestMethod>]
    member this.TestTryParseVarDeclInScope003iDiag () =
        ad.Clear()
        let input = """TestNamespace {
    inf {
        D()
        {
            x:T: 
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
  (FplParser, Error, (Ln: 5, Col: 13),
   DiagnosticMessage
     "'x:T:'
Expecting: 'dec', 'declaration', 'pre', 'premise', 'spec', 'specification', <block comment>, <inline comment>, 
<significant whitespace>
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
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


    [<TestMethod>]
    member this.TestTryParseVarDeclInScope003i000Diag () =
        ad.Clear()
        let input = """TestNamespace {
    inf {
        D()
        {
            x:tpl. 
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
  (FplParser, Error, (Ln: 5, Col: 13),
   DiagnosticMessage
     "'x:tpl.'
Expecting: 'dec', 'declaration', 'pre', 'premise', 'spec', 'specification', <block comment>, <inline comment>, 
<significant whitespace>
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
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


    [<TestMethod>]
    member this.TestTryParseVarDeclInScope003i001Diag () =
        ad.Clear()
        let input = """TestNamespace {
    inf {
        D()
        
            x:tpl[ 
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
  (FplParser, Error, (Ln: 5, Col: 13),
   DiagnosticMessage "'x:tpl['
Expecting: '{'
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
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


    [<TestMethod>]
    member this.TestTryParseVarDeclInScope003i002Diag () =
        ad.Clear()
        let input = """TestNamespace {
    inf {
        D()
        {
            x:tpl[! 
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
  (FplParser, Error, (Ln: 5, Col: 13),
   DiagnosticMessage
     "'x:tpl[!'
Expecting: 'dec', 'declaration', 'pre', 'premise', 'spec', 'specification', <block comment>, <inline comment>, 
<significant whitespace>
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
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


    [<TestMethod>]
    member this.TestTryParseVarDeclInScope003i003Diag () =
        ad.Clear()
        let input = """TestNamespace {
    inf {
        D()
        {
            x:tpl< 
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
  (FplParser, Error, (Ln: 5, Col: 13),
   DiagnosticMessage
     "'x:tpl<'
Expecting: 'dec', 'declaration', 'pre', 'premise', 'spec', 'specification', <block comment>, <inline comment>, 
<significant whitespace>
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
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


    [<TestMethod>]
    member this.TestTryParseVarDeclInScopePred001Diag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        pred T()
        {
            dec: x, ; 
            true
        }
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 5, Col: 21),
   DiagnosticMessage "';'
Expecting: <variable>
")
Diagnostic
  (FplParser, Error, (Ln: 5, Col: 24),
   DiagnosticMessage "';'
Expecting: ',', ':', ':*', ':+', <whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 6, Col: 13),
   DiagnosticMessage
     "'true'
Expecting: '@', 'func', 'function', 'ind', 'index', 'obj', 'object', 'pred', 
'predicate', 'template', 'tpl', <PascalCaseId>, <whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 8, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>, <whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(4, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>,<whitespace>")"""))


    [<TestMethod>]
    member this.TestTryParseVarDeclInScopePred002Diag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        pred T()
        {
            dec: x: ;
            true
        }
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 5, Col: 14),
   DiagnosticMessage
     "':'
Expecting: '$', '(', '.', '<', 'mand', 'mandatory', 'opt', 'optional', 
'}', <(closed) left bound '['>, <(open) left bound '[!'>, <block comment>, <inline comment>, <significant whitespace>, <whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 8, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>, <whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(3, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>,<whitespace>")"""))


    [<TestMethod>]
    member this.TestTryParseVarDeclInScopePred003aDiag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        pred T()
        {
            dec: x:*
            ; 
            true
        }
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 6, Col: 13),
   DiagnosticMessage
     "';'
Expecting: '@', 'func', 'function', 'ind', 'index', 'obj', 'object', 'pred', 
'predicate', 'template', 'tpl', <PascalCaseId>, <whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 9, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>, <whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(3, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>,<whitespace>")"""))


    [<TestMethod>]
    member this.TestTryParseVarDeclInScopePred003_Diag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        pred T()
        {
            dec: x:, ; 
            true
        }
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 5, Col: 20),
   DiagnosticMessage
     "','
Expecting: '@', 'func', 'function', 'ind', 'index', 'obj', 'object', 'pred', 
'predicate', 'template', 'tpl', <PascalCaseId>, <whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 8, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>, <whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>,<whitespace>")"""))


    [<TestMethod>]
    member this.TestTryParseVarDeclInScopePred003bDiag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        pred T()
        {
            dec:x:+ ;
            true
        }
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 5, Col: 21),
   DiagnosticMessage
     "';'
Expecting: '@', 'func', 'function', 'ind', 'index', 'obj', 'object', 'pred', 
'predicate', 'template', 'tpl', <PascalCaseId>, <whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 6, Col: 13),
   DiagnosticMessage
     "'true'
Expecting: '(', ';', '<', <(closed) left bound '['>, <(open) left bound '[('>, <block comment>, <inline comment>, <significant whitespace>, 
<variable (got keyword)>, <whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 8, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>, <whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(3, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>,<whitespace>")"""))



    [<TestMethod>]
    member this.TestTryParseVarDeclInScopePred003dDiag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        pred T()
        {
            dec:x:func: 
            ;
            true
        }
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 5, Col: 23),
   DiagnosticMessage
     "':'
Expecting: '(', ';', <block comment>, <inline comment>, <significant whitespace>, <variable>, <whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 8, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>, <whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>,<whitespace>")"""))


    [<TestMethod>]
    member this.TestTryParseVarDeclInScopePred003eDiag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        pred T()
        {
            dec:x:ind: ;
            true
        }
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 5, Col: 22),
   DiagnosticMessage
     "':'
Expecting: '(', ';', <block comment>, <inline comment>, <significant whitespace>, <variable>, <whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 8, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>, <whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>,<whitespace>")"""))


    [<TestMethod>]
    member this.TestTryParseVarDeclInScopePred003fDiag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        pred T()
        {
            dec: x:obj: ;
            true
        }
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 5, Col: 23),
   DiagnosticMessage
     "':'
Expecting: '(', ';', '<', <(closed) left bound '['>, <(open) left bound '[!'>, <block comment>, <inline comment>, <significant whitespace>, 
<variable>, <whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 8, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>, <whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>,<whitespace>")"""))


    [<TestMethod>]
    member this.TestTryParseVarDeclInScopePred003gDiag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        pred T()
        {
            dec: x:pred: ;
            true
        }
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 5, Col: 24),
   DiagnosticMessage
     "':'
Expecting: '(', ';', <block comment>, <inline comment>, <significant whitespace>, <variable>, <whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 8, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>, <whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>,<whitespace>")"""))


    [<TestMethod>]
    member this.TestTryParseVarDeclInScopePred003hDiag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        pred T()
        {
            dec: x:tpl: ;
            true
        }
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
      (FplParser, Error, (Ln: 5, Col: 23),
       DiagnosticMessage
         "':'
    Expecting: '(', ';', '<', <(closed) left bound '['>, <(open) left bound '[('>, <PascalCaseId>, <block comment>, <inline comment>, 
    <significant whitespace>, <variable>, <whitespace>
    ")
    Diagnostic
      (FplParser, Error, (Ln: 8, Col: 9),
       DiagnosticMessage
         "'y'
    Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
    'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
    'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
    <inline comment>, <significant whitespace>, <whitespace>
    ")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>,<whitespace>")"""))


    [<TestMethod>]
    member this.TestTryParseVarDeclInScopePred003iDiag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        pred T()
        {
            dec: x:T: ;
            true
        }
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 5, Col: 21),
   DiagnosticMessage
     "':'
Expecting: '(', '.', ';', '<', <(closed) left bound '['>, <(open) left bound '[('>, <block comment>, <inline comment>, 
<significant whitespace>, <variable>, <whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 8, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>, <whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>,<whitespace>")"""))


    [<TestMethod>]
    member this.TestTryParseVarDeclInScopePred003i000Diag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        pred T()
        {
            dec: x:tpl. ;
            true
        }
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 5, Col: 23),
   DiagnosticMessage
     "'.'
Expecting: '(', ';', '<', <(closed) left bound '['>, <(open) left bound '[!'>, <PascalCaseId>, <block comment>, <inline comment>, 
<significant whitespace>, <variable>, <whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 8, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>, <whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>,<whitespace>")"""))


    [<TestMethod>]
    member this.TestTryParseVarDeclInScopePred003i001Diag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        pred T()
        {
            dec: x:tpl[ ;
            true
        }
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
      (FplParser, Error, (Ln: 5, Col: 25),
       DiagnosticMessage
         "';'
    Expecting: '@', 'self', '~', <PascalCaseId>, <digits>, <variable>
    ")
    Diagnostic
      (FplParser, Error, (Ln: 6, Col: 13),
       DiagnosticMessage
         "'true'
    Expecting: '@', 'self', <(closed) right bound ']'>, <(open) right bound ')]'>, <PascalCaseId>, <digits>, <variable (got keyword)>
    ")
    Diagnostic
      (FplParser, Error, (Ln: 8, Col: 9),
       DiagnosticMessage
         "'y'
    Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
    'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
    'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
    <inline comment>, <significant whitespace>, <whitespace>
    ")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(3, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>,<whitespace>")"""))


    [<TestMethod>]
    member this.TestTryParseVarDeclInScopePred003i002Diag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        pred T()
        {
            dec: x:tpl[! ;
            true
        }
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
      (FplParser, Error, (Ln: 5, Col: 24),
       DiagnosticMessage
         "'!'
    Expecting: '@', 'self', '~', <PascalCaseId>, <digits>, <variable>
    ")
    Diagnostic
      (FplParser, Error, (Ln: 5, Col: 27),
       DiagnosticMessage
         "';'
    Expecting: '@', 'self', <(closed) right bound ']'>, <(open) right bound ')]'>, <PascalCaseId>, <digits>, <variable>
    ")
    Diagnostic
      (FplParser, Error, (Ln: 6, Col: 13),
       DiagnosticMessage
         "'true'
    Expecting: <(closed) right bound ']'>, <(open) right bound ')]'>, <whitespace>
    ")
    Diagnostic
      (FplParser, Error, (Ln: 8, Col: 9),
       DiagnosticMessage
         "'y'
    Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
    'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
    'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
    <inline comment>, <significant whitespace>, <whitespace>
    ")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(4, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>,<whitespace>")"""))


    [<TestMethod>]
    member this.TestTryParseVarDeclInScopePred003i003Diag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        pred T()
        {
            dec: x:tpl< ;
            true
        }
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 5, Col: 25),
   DiagnosticMessage
     "';'
Expecting: '@', 'self', <PascalCaseId>, <digits>, <variable>
")
Diagnostic
  (FplParser, Error, (Ln: 6, Col: 13),
   DiagnosticMessage "'true'
Expecting: ',', '>', <whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 8, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>, <whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(3, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>,<whitespace>")"""))


    [<TestMethod>]
    member this.TestTryParseVarDeclInScopeClass001Diag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        cl T:obj
        {
            x, 
        }
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
      (FplParser, Error, (Ln: 5, Col: 13),
       DiagnosticMessage
         "'x,'
    Expecting: 'dec', 'declaration', 'intr', 'intrinsic', 'spec', 'specification', <PascalCaseId>, <block comment>, 
    <inline comment>, <significant whitespace>
    ")
    Diagnostic
      (FplParser, Error, (Ln: 7, Col: 9),
       DiagnosticMessage
         "'y'
    Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
    'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
    'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
    <inline comment>, <significant whitespace>, <whitespace>
    ")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>,<whitespace>")"""))


    [<TestMethod>]
    member this.TestTryParseVarDeclInScopeClass002Diag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        cl T:obj
        {
            x: 
        }
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 5, Col: 13),
   DiagnosticMessage
     "'x:'
Expecting: 'dec', 'declaration', 'intr', 'intrinsic', 'spec', 'specification', <block comment>, <inline comment>, 
<significant whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 7, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>, <whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>,<whitespace>")"""))


    [<TestMethod>]
    member this.TestTryParseVarDeclInScopeClass003aDiag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        cl T:obj
        {
            x:* 
        }
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 5, Col: 13),
   DiagnosticMessage
     "'x:*'
Expecting: 'dec', 'declaration', 'intr', 'intrinsic', 'spec', 'specification', <block comment>, <inline comment>, 
<significant whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 7, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>, <whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>,<whitespace>")"""))


    [<TestMethod>]
    member this.TestTryParseVarDeclInScopeClass003_Diag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        cl T:obj
        {
            x::
        }
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 5, Col: 13),
   DiagnosticMessage
     "'x::'
Expecting: 'dec', 'declaration', 'intr', 'intrinsic', 'spec', 'specification', <block comment>, <inline comment>, 
<significant whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 7, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>, <whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>,<whitespace>")"""))


    [<TestMethod>]
    member this.TestTryParseVarDeclInScopeClass003bDiag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        cl T:obj
        {
            x:+ 
        }
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 5, Col: 13),
   DiagnosticMessage
     "'x:+'
Expecting: 'dec', 'declaration', 'intr', 'intrinsic', 'spec', 'specification', <block comment>, <inline comment>, 
<significant whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 7, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>, <whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>,<whitespace>")"""))


    [<TestMethod>]
    member this.TestTryParseVarDeclInScopeClass003cDiag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        cl T:obj
        {
            x:@ 
        }
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 5, Col: 13),
   DiagnosticMessage
     "'x:@'
Expecting: 'dec', 'declaration', 'intr', 'intrinsic', 'spec', 'specification', <block comment>, <inline comment>, 
<significant whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 7, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>, <whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>,<whitespace>")"""))


    [<TestMethod>]
    member this.TestTryParseVarDeclInScopeClass003dDiag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        cl T:obj
        {
            x:func: 
        }
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 5, Col: 13),
   DiagnosticMessage
     "'x:func:'
Expecting: 'dec', 'declaration', 'intr', 'intrinsic', 'spec', 'specification', <block comment>, <inline comment>, 
<significant whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 7, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>, <whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>,<whitespace>")"""))


    [<TestMethod>]
    member this.TestTryParseVarDeclInScopeClass003eDiag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        cl T:obj
        {
            x:ind: 
        }
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 5, Col: 13),
   DiagnosticMessage
     "'x:ind:'
Expecting: 'dec', 'declaration', 'intr', 'intrinsic', 'spec', 'specification', <block comment>, <inline comment>, 
<significant whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 7, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>, <whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>,<whitespace>")"""))


    [<TestMethod>]
    member this.TestTryParseVarDeclInScopeClass003fDiag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        cl T:obj
        {
            x:obj: 
        }
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 5, Col: 13),
   DiagnosticMessage
     "'x:obj:'
Expecting: 'dec', 'declaration', 'intr', 'intrinsic', 'spec', 'specification', <block comment>, <inline comment>, 
<significant whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 7, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>, <whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>,<whitespace>")"""))


    [<TestMethod>]
    member this.TestTryParseVarDeclInScopeClass003gDiag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        cl T:obj
        {
            x:pred: 
        }
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 5, Col: 13),
   DiagnosticMessage
     "'x:pred:'
Expecting: 'dec', 'declaration', 'intr', 'intrinsic', 'spec', 'specification', <block comment>, <inline comment>, 
<significant whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 7, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>, <whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>,<whitespace>")"""))


    [<TestMethod>]
    member this.TestTryParseVarDeclInScopeClass003hDiag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        cl T:obj
        {
            x:tpl: 
        }
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 5, Col: 13),
   DiagnosticMessage
     "'x:tpl:'
Expecting: 'dec', 'declaration', 'intr', 'intrinsic', 'spec', 'specification', <block comment>, <inline comment>, 
<significant whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 7, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>, <whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>,<whitespace>")"""))


    [<TestMethod>]
    member this.TestTryParseVarDeclInScopeClass003iDiag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        cl T:obj
        {
            x:T: 
        }
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 5, Col: 13),
   DiagnosticMessage
     "'x:T:'
Expecting: 'dec', 'declaration', 'intr', 'intrinsic', 'spec', 'specification', <block comment>, <inline comment>, 
<significant whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 7, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>, <whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>,<whitespace>")"""))


    [<TestMethod>]
    member this.TestTryParseVarDeclInScopeClass003i000Diag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        cl T:obj
        {
            x:tpl. 
        }
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 5, Col: 13),
   DiagnosticMessage
     "'x:tpl.'
Expecting: 'dec', 'declaration', 'intr', 'intrinsic', 'spec', 'specification', <block comment>, <inline comment>, 
<significant whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 7, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>, <whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>,<whitespace>")"""))


    [<TestMethod>]
    member this.TestTryParseVarDeclInScopeClass003i001Diag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        cl T:obj
        {
            x:tpl[ 
        }
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 5, Col: 13),
   DiagnosticMessage
     "'x:tpl['
Expecting: 'dec', 'declaration', 'intr', 'intrinsic', 'spec', 'specification', <block comment>, <inline comment>, 
<significant whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 7, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>, <whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>,<whitespace>")"""))


    [<TestMethod>]
    member this.TestTryParseVarDeclInScopeClass003i002Diag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        cl T:obj
        {
            x:tpl[! 
        }
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 5, Col: 13),
   DiagnosticMessage
     "'x:tpl[!'
Expecting: 'dec', 'declaration', 'intr', 'intrinsic', 'spec', 'specification', <block comment>, <inline comment>, 
<significant whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 7, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>, <whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>,<whitespace>")"""))


    [<TestMethod>]
    member this.TestTryParseVarDeclInScopeClass003i003Diag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        cl T:obj
        {
            x:tpl< 
        }
        y
    }
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 5, Col: 13),
   DiagnosticMessage
     "'x:tpl<'
Expecting: 'dec', 'declaration', 'intr', 'intrinsic', 'spec', 'specification', <block comment>, <inline comment>, 
<significant whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 7, Col: 9),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>, <whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'ax','axiom','cl','class','conj','conjecture','cor','corollary','func','function','lem','lemma','post','postulate','pred','predicate','prf','proof','prop','proposition','theorem','thm','}',<blockcomment>,<inlinecomment>,<significantwhitespace>,<whitespace>")"""))


    [<TestMethod>]
    member this.TestTryParseClass000Diag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        cl
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
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'loc','localization','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


    [<TestMethod>]
    member this.TestTryParseClass001Diag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        cl x
    }
    y
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 3, Col: 12),
   DiagnosticMessage "'x'
Expecting: <PascalCaseId>
")
Diagnostic
  (FplParser, Error, (Ln: 5, Col: 5),
   DiagnosticMessage
     "'y'
Expecting: 'loc', 'localization', '}', <block comment>, <inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'loc','localization','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))

    [<TestMethod>]
    member this.TestTryParseClass001aDiag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        cl .
    }
    y
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 3, Col: 12),
   DiagnosticMessage "'.'
Expecting: <PascalCaseId>
")
Diagnostic
  (FplParser, Error, (Ln: 5, Col: 5),
   DiagnosticMessage
     "'y'
Expecting: 'loc', 'localization', '}', <block comment>, <inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'loc','localization','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


    [<TestMethod>]
    member this.TestTryParseClass001bDiag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        cl ,
    }
    y
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 3, Col: 12),
   DiagnosticMessage "','
Expecting: <PascalCaseId>
")
Diagnostic
  (FplParser, Error, (Ln: 5, Col: 5),
   DiagnosticMessage
     "'y'
Expecting: 'loc', 'localization', '}', <block comment>, <inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'loc','localization','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))

    [<TestMethod>]
    member this.TestTryParseClass002Diag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        cl A
    }
    y
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 4, Col: 5),
   DiagnosticMessage "'}'
Expecting: ':', ':*', ':+', <whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 5, Col: 5),
   DiagnosticMessage
     "'y'
Expecting: 'loc', 'localization', '}', <block comment>, <inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'loc','localization','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))

    [<TestMethod>]
    member this.TestTryParseClass003Diag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        cl A #
    }
    y
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 3, Col: 14),
   DiagnosticMessage "'#'
Expecting: ':', ':*', ':+', <whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 5, Col: 5),
   DiagnosticMessage
     "'y'
Expecting: 'loc', 'localization', '}', <block comment>, <inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'loc','localization','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))

    [<TestMethod>]
    member this.TestTryParseClass003aDiag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        cl A -
    }
    y
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 3, Col: 14),
   DiagnosticMessage "'-'
Expecting: ':', ':*', ':+', <whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 5, Col: 5),
   DiagnosticMessage
     "'y'
Expecting: 'loc', 'localization', '}', <block comment>, <inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'loc','localization','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))

    [<TestMethod>]
    member this.TestTryParseClass004Diag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        cl A:
    }
    y
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 4, Col: 5),
   DiagnosticMessage
     "'}'
Expecting: '@', 'obj', 'object', 'template', 'tpl', <PascalCaseId>, <whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 5, Col: 5),
   DiagnosticMessage
     "'y'
Expecting: 'loc', 'localization', '}', <block comment>, <inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'loc','localization','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))

    [<TestMethod>]
    member this.TestTryParseClass004aDiag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        cl A:@
    }
    y
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 3, Col: 15),
   DiagnosticMessage "'cl A:@'
Expecting: <PascalCaseId>
")
Diagnostic
  (FplParser, Error, (Ln: 4, Col: 5),
   DiagnosticMessage
     "'y'
Expecting: 'loc', 'localization', '}', <block comment>, <inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'loc','localization','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))

    [<TestMethod>]
    member this.TestTryParseClass005Diag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        cl A:obj
    }
    y
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 4, Col: 5),
   DiagnosticMessage
     "'}'
Expecting: ',', '<', '{', <(closed) left bound '['>, <(open) left bound '[('>, <whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 5, Col: 5),
   DiagnosticMessage
     "'y'
Expecting: 'loc', 'localization', '}', <block comment>, <inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'loc','localization','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


    [<TestMethod>]
    member this.TestTryParseClass006Diag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        cl A:obj 
        {
    }
    y
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 5, Col: 5),
   DiagnosticMessage
     "'}'
Expecting: 'dec', 'declaration', 'intr', 'intrinsic', 'spec', 'specification', <PascalCaseId>, <block comment>, 
<inline comment>, <significant whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 6, Col: 5),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>, <whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 6, Col: 54),
   DiagnosticMessage
     "Expecting: '}', <block comment>, <inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(3, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))

    [<TestMethod>]
    member this.TestTryParseClass007Diag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        cl A:obj 
        {
            dec:;
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
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'loc','localization','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))

    [<TestMethod>]
    member this.TestTryParseClass008Diag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        cl A:obj 
        {
            #
        }
    }
    y
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 5, Col: 13),
   DiagnosticMessage
     "'#'
Expecting: 'dec', 'declaration', 'intr', 'intrinsic', 'spec', 'specification', <block comment>, <inline comment>, 
<significant whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 8, Col: 5),
   DiagnosticMessage
     "'y'
Expecting: 'loc', 'localization', '}', <block comment>, <inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'loc','localization','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))

    [<TestMethod>]
    member this.TestTryParsePredicate000Diag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        pred
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
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'loc','localization','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


    [<TestMethod>]
    member this.TestTryParsePredicate001Diag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        pred x
    }
    y
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 3, Col: 14),
   DiagnosticMessage "'x'
Expecting: <PascalCaseId>
")
Diagnostic
  (FplParser, Error, (Ln: 5, Col: 5),
   DiagnosticMessage
     "'y'
Expecting: 'loc', 'localization', '}', <block comment>, <inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'loc','localization','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


    [<TestMethod>]
    member this.TestTryParsePredicate001aDiag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        pred .
    }
    y
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
      (FplParser, Error, (Ln: 3, Col: 14),
       DiagnosticMessage "'.'
    Expecting: <PascalCaseId>
    ")
    Diagnostic
      (FplParser, Error, (Ln: 5, Col: 5),
       DiagnosticMessage
         "'y'
    Expecting: 'loc', 'localization', '}', <block comment>, <inline comment>, <significant whitespace>
    ")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'loc','localization','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


    [<TestMethod>]
    member this.TestTryParsePredicate001bDiag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        pred ,
    }
    y
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 3, Col: 14),
   DiagnosticMessage "','
Expecting: <PascalCaseId>
")
Diagnostic
  (FplParser, Error, (Ln: 5, Col: 5),
   DiagnosticMessage
     "'y'
Expecting: 'loc', 'localization', '}', <block comment>, <inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'loc','localization','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


    [<TestMethod>]
    member this.TestTryParsePredicate002Diag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        pred A
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
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'loc','localization','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


    [<TestMethod>]
    member this.TestTryParsePredicate003Diag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        pred A #
    }
    y
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 3, Col: 16),
   DiagnosticMessage "'#'
Expecting: '(', <whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 5, Col: 5),
   DiagnosticMessage
     "'y'
Expecting: 'loc', 'localization', '}', <block comment>, <inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'loc','localization','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


    [<TestMethod>]
    member this.TestTryParsePredicate003aDiag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        pred A -
    }
    y
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 3, Col: 16),
   DiagnosticMessage "'-'
Expecting: '(', <whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 5, Col: 5),
   DiagnosticMessage
     "'y'
Expecting: 'loc', 'localization', '}', <block comment>, <inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'loc','localization','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


    [<TestMethod>]
    member this.TestTryParsePredicate004Diag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        pred A:
    }
    y
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 3, Col: 15),
   DiagnosticMessage "':'
Expecting: '(', '.', <whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 5, Col: 5),
   DiagnosticMessage
     "'y'
Expecting: 'loc', 'localization', '}', <block comment>, <inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'loc','localization','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


    [<TestMethod>]
    member this.TestTryParsePredicate004aDiag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        pred A(
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
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'loc','localization','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


    [<TestMethod>]
    member this.TestTryParsePredicate005Diag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        pred A(#
    }
    y
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 3, Col: 16),
   DiagnosticMessage "'#'
Expecting: ')', <variable>, <whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 5, Col: 5),
   DiagnosticMessage
     "'y'
Expecting: 'loc', 'localization', '}', <block comment>, <inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'loc','localization','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


    [<TestMethod>]
    member this.TestTryParsePredicate005aDiag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        pred A()
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
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'loc','localization','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))

    [<TestMethod>]
    member this.TestTryParsePredicate006Diag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        pred A() 
        {
    }
    y
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 5, Col: 5),
   DiagnosticMessage
     "'}'
Expecting: '@', 'all', 'and', 'dec', 'declaration', 'ex', 'false', 'iif', 
'impl', 'intr', 'intrinsic', 'is', 'not', 'or', 'self', 'spec', 
'specification', 'true', 'undef', 'undefined', 'xor', <PascalCaseId>, <argument identifier>, <block comment>, 
<digits>, <inline comment>, <significant whitespace>, <variable>
")
Diagnostic
  (FplParser, Error, (Ln: 6, Col: 5),
   DiagnosticMessage
     "'y'
Expecting: 'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 
'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 
'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, 
<inline comment>, <significant whitespace>, <whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 7, Col: 50),
   DiagnosticMessage
     "Expecting: '}', <block comment>, <inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(3, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))

    [<TestMethod>]
    member this.TestTryParsePredicate006aDiag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        pred A() 
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
Expecting: '@', 'all', 'and', 'dec', 'declaration', 'ex', 'false', 'iif', 
'impl', 'intr', 'intrinsic', 'is', 'not', 'or', 'self', 'spec', 
'specification', 'true', 'undef', 'undefined', 'xor', <PascalCaseId>, <argument identifier>, <block comment>, 
<digits>, <inline comment>, <significant whitespace>, <variable>
")
Diagnostic
  (FplParser, Error, (Ln: 7, Col: 5),
   DiagnosticMessage
     "'y'
Expecting: 'loc', 'localization', '}', <block comment>, <inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'loc','localization','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))

    [<TestMethod>]
    member this.TestTryParsePredicate007Diag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        pred A() 
        {
            dec:;
        }
    }
    y
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 6, Col: 9),
   DiagnosticMessage
     "'}'
Expecting: '@', 'all', 'and', 'dec', 'declaration', 'ex', 'false', 'iif', 
'impl', 'is', 'not', 'or', 'self', 'spec', 'specification', 'true', 
'undef', 'undefined', 'xor', <PascalCaseId>, <argument identifier>, <block comment>, <digits>, <inline comment>, 
<significant whitespace>, <variable>, <whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 8, Col: 5),
   DiagnosticMessage
     "'y'
Expecting: 'loc', 'localization', '}', <block comment>, <inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'loc','localization','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))


    [<TestMethod>]
    member this.TestTryParsePredicate008Diag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        pred A() 
        {
            #
        }
    }
    y
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 5, Col: 13),
   DiagnosticMessage
     "'#'
Expecting: '@', 'all', 'and', 'dec', 'declaration', 'ex', 'false', 'iif', 
'impl', 'intr', 'intrinsic', 'is', 'not', 'or', 'self', 'spec', 
'specification', 'true', 'undef', 'undefined', 'xor', <PascalCaseId>, <argument identifier>, <block comment>, 
<digits>, <inline comment>, <significant whitespace>, <variable>
")
Diagnostic
  (FplParser, Error, (Ln: 8, Col: 5),
   DiagnosticMessage
     "'y'
Expecting: 'loc', 'localization', '}', <block comment>, <inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'loc','localization','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))

    [<TestMethod>]
    member this.TestTryParsePredicate009Diag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        pred A() 
        {
            spec:;
        }
    }
    y
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 6, Col: 9),
   DiagnosticMessage
     "'}'
Expecting: '@', 'all', 'and', 'dec', 'declaration', 'ex', 'false', 'iif', 
'impl', 'is', 'not', 'or', 'self', 'spec', 'specification', 'true', 
'undef', 'undefined', 'xor', <PascalCaseId>, <argument identifier>, <block comment>, <digits>, <inline comment>, 
<significant whitespace>, <variable>, <whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 8, Col: 5),
   DiagnosticMessage
     "'y'
Expecting: 'loc', 'localization', '}', <block comment>, <inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'loc','localization','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))

    [<TestMethod>]
    member this.TestTryParsePredicate010Diag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        pred A() 
        {
            dec:;
            spec:;
        }
    }
    y
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 7, Col: 9),
   DiagnosticMessage
     "'}'
Expecting: '@', 'all', 'and', 'dec', 'declaration', 'ex', 'false', 'iif', 
'impl', 'is', 'not', 'or', 'self', 'spec', 'specification', 'true', 
'undef', 'undefined', 'xor', <PascalCaseId>, <argument identifier>, <block comment>, <digits>, <inline comment>, 
<significant whitespace>, <variable>, <whitespace>
")
Diagnostic
  (FplParser, Error, (Ln: 9, Col: 5),
   DiagnosticMessage
     "'y'
Expecting: 'loc', 'localization', '}', <block comment>, <inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'loc','localization','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))

    [<TestMethod>]
    member this.TestTryParsePredicate011Diag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        pred A() 
        {
            dec:;
            spec:;
            true
        }
    }
    y
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 10, Col: 5),
   DiagnosticMessage
     "'y'
Expecting: 'loc', 'localization', '}', <block comment>, <inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(1, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'loc','localization','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))

    [<TestMethod>]
    member this.TestTryParsePredicate012Diag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        pred A() 
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
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(1, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'loc','localization','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))

    [<TestMethod>]
    member this.TestTryParseStatement000Diag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        pred A() 
        {
            spec:
                x := 
            ;
            true
        }
    }
    y
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 7, Col: 13),
   DiagnosticMessage
     "';'
Expecting: '<', '@', 'all', 'and', 'del', 'delegate', 'ex', 'false', 
'iif', 'impl', 'is', 'not', 'or', 'self', 'true', 'undef', 
'undefined', 'xor', <PascalCaseId>, <argument identifier>, <digits>, <variable>
")
Diagnostic
  (FplParser, Error, (Ln: 11, Col: 5),
   DiagnosticMessage
     "'y'
Expecting: 'loc', 'localization', '}', <block comment>, <inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'loc','localization','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))

    [<TestMethod>]
    member this.TestTryParseStatement001Diag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        pred A() 
        {
            spec:
                x := false
            ;
            true
        }
    }
    y
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 11, Col: 5),
   DiagnosticMessage
     "'y'
Expecting: 'loc', 'localization', '}', <block comment>, <inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(1, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'loc','localization','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))

    [<TestMethod>]
    member this.TestTryParseStatement002Diag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        pred A() 
        {
            spec:
                x := theorem
            ;
            true
        }
    }
    y
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 6, Col: 22),
   DiagnosticMessage
     "'theorem'
Expecting: '<', '@', 'all', 'and', 'del', 'delegate', 'ex', 'false', 
'iif', 'impl', 'is', 'not', 'or', 'self', 'true', 'undef', 
'undefined', 'xor', <PascalCaseId>, <argument identifier>, <digits>, <variable (got keyword)>
")
Diagnostic
  (FplParser, Error, (Ln: 11, Col: 5),
   DiagnosticMessage
     "'y'
Expecting: 'loc', 'localization', '}', <block comment>, <inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'loc','localization','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))

    [<TestMethod>]
    member this.TestTryParseStatement003Diag () =
        ad.Clear()
        let input = """TestNamespace {
    theory {   
        pred A() 
        {
            spec:
                x := #
            ;
            true
        }
    }
    y
}"""
        let result = fplParser input
        let actual = sprintf "%O" result
        let expectedDiag = """Diagnostic
  (FplParser, Error, (Ln: 6, Col: 22),
   DiagnosticMessage
     "'#'
Expecting: '<', '@', 'all', 'and', 'del', 'delegate', 'ex', 'false', 
'iif', 'impl', 'is', 'not', 'or', 'self', 'true', 'undef', 
'undefined', 'xor', <PascalCaseId>, <argument identifier>, <digits>, <variable>
")
Diagnostic
  (FplParser, Error, (Ln: 11, Col: 5),
   DiagnosticMessage
     "'y'
Expecting: 'loc', 'localization', '}', <block comment>, <inline comment>, <significant whitespace>
")"""
        let actualDiag = ad.DiagnosticsToString
        printf "\n>>%s<<" actualDiag
        printf "\n>>>%s<<<" (replaceWhiteSpace actualDiag)
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue((replaceWhiteSpace actualDiag).EndsWith("""Expecting:'loc','localization','}',<blockcomment>,<inlinecomment>,<significantwhitespace>")"""))
