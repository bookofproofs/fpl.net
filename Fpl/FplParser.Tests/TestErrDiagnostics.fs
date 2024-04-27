namespace FplParser.Tests

open FParsec
open FplGrammarCommons
open ErrDiagnostics
open FplParser
open Microsoft.VisualStudio.TestTools.UnitTesting


[<TestClass>]
type TestErrDiagnostics() =
    let replaceWhiteSpace (input: string) =
        let whiteSpaceChars = [| ' '; '\t'; '\n'; '\r' |]
        input.Split(whiteSpaceChars) |> String.concat ""

    [<TestMethod>]
    member this.TestTryParseExtension000Diag () =
        ad.Clear()
        let result = fplParser (System.Uri("file://x")) """
        x
        y 
;"""
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseExtension001Diag () =
        ad.Clear()
        let result = fplParser (System.Uri("file://x")) """
    :
        y
;"""
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseExtension002Diag () =
        ad.Clear()
        let result = fplParser (System.Uri("file://x")) """
    :ext
        y
;"""
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseExtension003Diag () =
        ad.Clear()
        let result = fplParser (System.Uri("file://x")) """
    :ext T
        y
;"""
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(1, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseExtension004Diag () =
        ad.Clear()
        let result = fplParser (System.Uri("file://x")) """
    :ext T:
        y
;"""
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseExtension005Diag () =
        ad.Clear()
        let result = fplParser (System.Uri("file://x")) """
    :ext T: /d/
        y
;"""
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseExtension006Diag () =
        ad.Clear()
        let result = fplParser (System.Uri("file://x")) """
    :ext T: /d/ :end
        y
;"""
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(1, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))
        
    [<TestMethod>]
    member this.TestTryParseUses001Diag () =
        ad.Clear()
        let input = """
    uses
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseUses002Diag () =
        ad.Clear()
        let input = """
    uses T  
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(1, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseUses003Diag () =
        ad.Clear()
        let input = """
    uses   R a
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseUses004Diag () =
        ad.Clear()
        let input = """
    uses   R alias
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseUses005Diag () =
        ad.Clear()
        let input = """
    uses   R alias %
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("USE000"))

    [<TestMethod>]
    member this.TestTryParseUses006Diag () =
        ad.Clear()
        let input = """
    uses   R alias s
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(3, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseUses007Diag () =
        ad.Clear()
        let input = """
    uses   R alias I
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(1, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseUses009Diag () =
        ad.Clear()
        let input = """
    uses   a alias I 
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(1, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseInference000Diag () =
        ad.Clear()
        let input = """
    i
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseInference001Diag () =
        ad.Clear()
        let input = """
    inf
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseInference002Diag () =
        ad.Clear()
        let input = """
    inf T
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseInference003Diag () =
        ad.Clear()
        let input = """
    inf T (
        x
        y
    }
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("INF000"))

    [<TestMethod>]
    member this.TestTryParseInference004Diag () =
        ad.Clear()
        let input = """
    inf 
        D
        y
    }
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("INF000"))

    [<TestMethod>]
    member this.TestTryParseInference005Diag () =
        ad.Clear()
        let input = """
    inf 
        D(
        y
    }
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseInference006Diag () =
        ad.Clear()
        let input = """
    inf 
        D()
    {   
        y
    }
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("INF000"))

    [<TestMethod>]
    member this.TestTryParseInference007Diag () =
        ad.Clear()
        let input = """
    inf 
        D() 
    {   
        y
    }
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("INF000"))

    [<TestMethod>]
    member this.TestTryParseInference008Diag () =
        ad.Clear()
        let input = """
    inf D() {
        }
        y
    }
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("INF000"))

    [<TestMethod>]
    member this.TestTryParseInference009Diag () =
        ad.Clear()
        let input = """
    inf 
        D() { pre: true con: true }
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(1, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseInference010Diag () =
        ad.Clear()
        let input = """
    inf 
        D() { pre: true con: true } }
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseNamespace000Diag () =
        ad.Clear()
        let input = """
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(1, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseNamespace001Diag () =
        ad.Clear()
        let input = """
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(1, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseNamespace002Diag () =
        ad.Clear()
        let input = """
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(1, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseNamespace003Diag () =
        ad.Clear()
        let input = """    
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(1, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseNamespace004Diag () =
        ad.Clear()
        let input = """
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(1, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseNamespace005Diag () =
        ad.Clear()
        let input = """
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(1, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseNamespace006Diag () =
        ad.Clear()
        let input = """
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(1, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseAxiom000Diag () =
        ad.Clear()
        let input = """
       
        a
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseAxiom001Diag () =
        ad.Clear()
        let input = """
        ax
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseAxiom002Diag () =
        ad.Clear()
        let input = """
  
        ax T
        y    
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseAxiom003Diag () =
        ad.Clear()
        let input = """
        ax T (
    y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("AXI000"))

    [<TestMethod>]
    member this.TestTryParseAxiom004Diag () =
        ad.Clear()
        let input = """
 
        ax T ()
    y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseAxiom005Diag () =
        ad.Clear()
        let input = """
 
        ax T () 
        {
        }
    y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(3, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseAxiom006Diag () =
        ad.Clear()
        let input = """
        ax T () 
        {
            true
        }
    y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(1, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseAxiom007Diag () =
        ad.Clear()
        let input = """
        ax T () 
        {
            intrinsic
        }
    y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(4, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseFunction000Diag () =
        ad.Clear()
        let input = """
        def func
    y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("DEF000"))

    [<TestMethod>]
    member this.TestTryParseFunction001Diag () =
        ad.Clear()
        let input = """
        def func A
    y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("DEF000"))

    [<TestMethod>]
    member this.TestTryParseFunction002Diag () =
        ad.Clear()
        let input = """
        def func A (
    y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("DEF000"))

    [<TestMethod>]
    member this.TestTryParseFunction003Diag () =
        ad.Clear()
        let input = """
        def func A ()
    y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("DEF000"))

    [<TestMethod>]
    member this.TestTryParseFunction004Diag () =
        ad.Clear()
        let input = """
        def func A () ->
    y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("DEF000"))

    [<TestMethod>]
    member this.TestTryParseFunction005Diag () =
        ad.Clear()
        let input = """
        def func A () -> T
    y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("DEF000"))

    [<TestMethod>]
    member this.TestTryParseFunction006Diag () =
        ad.Clear()
        let input = """
        def func A () -> T 
        {
    y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("DEF000"))

    [<TestMethod>]
    member this.TestTryParseClasses000Diag () =
        ad.Clear()
        let input = """
        def cl 
        {
        }
    y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("DEF000"))

    [<TestMethod>]
    member this.TestTryParseClasses001Diag () =
        ad.Clear()
        let input = """
        def cl T
        {
        }
    y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("DEF000"))

    [<TestMethod>]
    member this.TestTryParseClasses002Diag () =
        ad.Clear()
        let input = """
        def cl T:
        {
        }
    y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("DEF000"))

    [<TestMethod>]
    member this.TestTryParseClasses003Diag () =
        ad.Clear()
        let input = """
        def cl T: obj
        {
        }
    y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("DEF000"))

    [<TestMethod>]
    member this.TestTryParseVarDecl000Diag () =
        ad.Clear()
        let input = """
    inf D(x)
        {
            pre:true
            con:true
        }
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(5, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("INF000"))

    [<TestMethod>]
    member this.TestTryParseVarDecl001Diag () =
        ad.Clear()
        let input = """
    inf 
        D(x, )
        {
            pre:true
            con:true
        }
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(4, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseVarDecl002Diag () =
        ad.Clear()
        let input = """
    inf 
        D(x: )
        {
            pre:true
            con:true
        }
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(5, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseVarDecl003aDiag () =
        ad.Clear()
        let input = """
    inf D(x:* )
        {
            pre:true
            con:true
        }
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(5, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseVarDecl003_Diag () =
        ad.Clear()
        let input = """
    inf D(x:, )
        {
            pre:true
            con:true
        }
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseVarDecl003bDiag () =
        ad.Clear()
        let input = """
    inf D(x:+ )
        {
            pre:true
            con:true
        }
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(5, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseVarDecl003cDiag () =
        ad.Clear()
        let input = """
    inf D(x:@ )
        {
            pre:true
            con:true
        }
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseVarDecl003dDiag () =
        ad.Clear()
        let input = """
    inf D(x:func: )
        {
            pre:true
            con:true
        }
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(3, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseVarDecl003eDiag () =
        ad.Clear()
        let input = """
    inf D(x:ind: )
        {
            pre:true
            con:true
        }
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(3, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseVarDecl003fDiag () =
        ad.Clear()
        let input = """
    inf D(x:obj: )
        {
            pre:true
            con:true
        }
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(3, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseVarDecl003gDiag () =
        ad.Clear()
        let input = """
    inf D(x:pred: )
        {
            pre:true
            con:true
        }
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(3, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseVarDecl003hDiag () =
        ad.Clear()
        let input = """
    inf D(x:tpl: )
        {
            pre:true
            con:true
        }
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(3, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseVarDecl003iDiag () =
        ad.Clear()
        let input = """
    inf D(x:T: )
        {
            pre:true
            con:true
        }
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(3, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseVarDecl003i000Diag () =
        ad.Clear()
        let input = """
    inf D(x:tpl. )
        {
            pre:true
            con:true
        }
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(3, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseVarDecl003i001Diag () =
        ad.Clear()
        let input = """
    inf D(x:tpl[ )
        {
            pre:true
            con:true
        }
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(4, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseVarDecl003i002Diag () =
        ad.Clear()
        let input = """
    inf D(x:tpl[! )
        {
            pre:true
            con:true
        }
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN001"))

    [<TestMethod>]
    member this.TestTryParseVarDecl003i003Diag () =
        ad.Clear()
        let input = """
    inf D(x:tpl[ )
        {
            pre:true
            con:true
        }
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(4, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))


    [<TestMethod>]
    member this.TestTryParseVarDeclInScopel000Diag () =
        ad.Clear()
        let input = """
    inf D()
        {
            dec ~x ;
            pre:true
            con:true
        }
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(4, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseVarDeclInScope001Diag () =
        ad.Clear()
        let input = """
    inf D()
        {
            x, 
            pre:true
            con:true
        }
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(3, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseVarDeclInScope002Diag () =
        ad.Clear()
        let input = """
    inf D()
        {
            x: 
            pre:true
            con:true
        }
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(3, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseVarDeclInScope003aDiag () =
        ad.Clear()
        let input = """
    inf D()
        {
            x:* 
            pre:true
            con:true
        }
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(3, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseVarDeclInScope003_Diag () =
        ad.Clear()
        let input = """
    inf D()
        {
            x:, 
            pre:true
            con:true
        }
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(3, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseVarDeclInScope003bDiag () =
        ad.Clear()
        let input = """
    inf D()
        {
            x:+ 
            pre:true
            con:true
        }
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(3, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseVarDeclInScope003cDiag () =
        ad.Clear()
        let input = """
    inf D()
        {
            x:@ 
            pre:true
            con:true
        }
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(3, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseVarDeclInScope003dDiag () =
        ad.Clear()
        let input = """
    inf D()
        {
            x:func: 
            pre:true
            con:true
        }
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(3, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseVarDeclInScope003eDiag () =
        ad.Clear()
        let input = """
    inf D()
        {
            x:ind: 
            pre:true
            con:true
        }
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(3, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseVarDeclInScope003fDiag () =
        ad.Clear()
        let input = """
    inf D()
        {
            x:obj: 
            pre:true
            con:true
        }
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(3, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseVarDeclInScope003gDiag () =
        ad.Clear()
        let input = """
    inf D()
        {
            x:pred: 
            pre:true
            con:true
        }
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(3, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseVarDeclInScope003hDiag () =
        ad.Clear()
        let input = """
    inf D()
        {
            x:tpl: 
            pre:true
            con:true
        }
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(3, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseVarDeclInScope003iDiag () =
        ad.Clear()
        let input = """
    inf D()
        {
            x:T: 
            pre:true
            con:true
        }
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(3, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseVarDeclInScope003i000Diag () =
        ad.Clear()
        let input = """
    inf D()
        {
            x:tpl. 
            pre:true
            con:true
        }
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(3, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseVarDeclInScope003i001Diag () =
        ad.Clear()
        let input = """
    inf D()
        
            x:tpl[ 
            pre:true
            con:true
        }
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(4, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseVarDeclInScope003i002Diag () =
        ad.Clear()
        let input = """
    inf 
        D()
        {
            x:tpl[! 
            pre:true
            con:true
        }
        y
    }
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(4, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseVarDeclInScope003i003Diag () =
        ad.Clear()
        let input = """
    inf 
        D()
        {
            x:tpl< 
            pre:true
            con:true
        }
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(3, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseVarDeclInScopePred001Diag () =
        ad.Clear()
        let input = """
        def pred T()
        {
            dec ~x, ; 
            true
        }
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(4, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseVarDeclInScopePred002Diag () =
        ad.Clear()
        let input = """
        def pred T()
        {
            dec ~x: ;
            true
        }
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(5, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseVarDeclInScopePred003aDiag () =
        ad.Clear()
        let input = """
        def pred T()
        {
            dec ~x:*
            ; 
            true
        }
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(5, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseVarDeclInScopePred003_Diag () =
        ad.Clear()
        let input = """
        def pred T()
        {
            dec ~x:, ; 
            true
        }
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(1, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseVarDeclInScopePred003bDiag () =
        ad.Clear()
        let input = """
        def pred T()
        {
            dec ~x:+ ;
            true
        }
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(5, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))


    [<TestMethod>]
    member this.TestTryParseVarDeclInScopePred003dDiag () =
        ad.Clear()
        let input = """
        def pred T()
        {
            dec ~x:func: 
            ;
            true
        }
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseVarDeclInScopePred003eDiag () =
        ad.Clear()
        let input = """
        def pred T()
        {
            dec~x:ind: ;
            true
        }
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseVarDeclInScopePred003fDiag () =
        ad.Clear()
        let input = """
        def pred T()
        {
            dec~x:obj: ;
            true
        }
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseVarDeclInScopePred003gDiag () =
        ad.Clear()
        let input = """
        def pred T()
        {
            dec~ x:pred: ;
            true
        }
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(7, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseVarDeclInScopePred003hDiag () =
        ad.Clear()
        let input = """
        def pred T()
        {
            dec ~x:tpl: ;
            true
        }
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseVarDeclInScopePred003iDiag () =
        ad.Clear()
        let input = """
        def pred T()
        {
            dec ~x:T: ;
            true
        }
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseVarDeclInScopePred003i000Diag () =
        ad.Clear()
        let input = """
        def pred T()
        {
            dec ~x:tpl. ;
            true
        }
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseVarDeclInScopePred003i001Diag () =
        ad.Clear()
        let input = """
        def pred T()
        {
            dec ~x:tpl[ ;
            true
        }
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(4, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseVarDeclInScopePred003i002Diag () =
        ad.Clear()
        let input = """
        def pred T()
        {
            dec ~x:tpl[! ;
            true
        }
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(1, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseVarDeclInScopePred003i003Diag () =
        ad.Clear()
        let input = """
        def pred T()
        {
            dec ~x:tpl[ ;
            true
        }
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(4, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseVarDeclInScopeClass001Diag () =
        ad.Clear()
        let input = """
        def cl T:obj
        {
            x, 
        }
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(4, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseVarDeclInScopeClass002Diag () =
        ad.Clear()
        let input = """
        def cl T:obj
        {
            x: 
        }
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(4, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseVarDeclInScopeClass003aDiag () =
        ad.Clear()
        let input = """
        def cl T:obj
        {
            x:* 
        }
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(4, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseVarDeclInScopeClass003_Diag () =
        ad.Clear()
        let input = """
        def cl T:obj
        {
            x::
        }
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(4, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseVarDeclInScopeClass003bDiag () =
        ad.Clear()
        let input = """
        def cl T:obj
        {
            x:+ 
        }
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(4, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseVarDeclInScopeClass003cDiag () =
        ad.Clear()
        let input = """
        def cl T:obj
        {
            x:@ 
        }
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(4, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseVarDeclInScopeClass003dDiag () =
        ad.Clear()
        let input = """
        def cl T:obj
        {
            x:func: 
        }
    }
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(4, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseVarDeclInScopeClass003eDiag () =
        ad.Clear()
        let input = """
        def cl T:obj
        {
            x:ind: 
        }
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(4, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseVarDeclInScopeClass003fDiag () =
        ad.Clear()
        let input = """
        def cl T:obj
        {
            x:obj: 
        }
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(4, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseVarDeclInScopeClass003gDiag () =
        ad.Clear()
        let input = """
        def cl T:obj
        {
            x:pred: 
        }
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(4, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseVarDeclInScopeClass003hDiag () =
        ad.Clear()
        let input = """
        def cl T:obj
        {
            x:tpl: 
        }
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(4, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseVarDeclInScopeClass003iDiag () =
        ad.Clear()
        let input = """
        def cl T:obj
        {
            x:T: 
        }
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(4, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseVarDeclInScopeClass003i000Diag () =
        ad.Clear()
        let input = """
        def cl T:obj
        {
            x:tpl. 
        }
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(4, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseVarDeclInScopeClass003i001Diag () =
        ad.Clear()
        let input = """
        def cl T:obj
        {
            x:tpl[ 
        }
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(4, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseVarDeclInScopeClass003i002Diag () =
        ad.Clear()
        let input = """
        def cl T:obj
        {
            x:tpl[! 
        }
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(4, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseVarDeclInScopeClass003i003Diag () =
        ad.Clear()
        let input = """
        def cl T:obj
        {
            x:tpl< 
        }
        y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("DEF000"))

    [<TestMethod>]
    member this.TestTryParseClass000Diag () =
        ad.Clear()
        let input = """
        def cl
    y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseClass001Diag () =
        ad.Clear()
        let input = """
        def cl x
    y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseClass001aDiag () =
        ad.Clear()
        let input = """
        def cl .
    y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseClass001bDiag () =
        ad.Clear()
        let input = """
        def cl ,
    y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("DEF000"))

    [<TestMethod>]
    member this.TestTryParseClass002Diag () =
        ad.Clear()
        let input = """
        def cl A
    y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("DEF000"))

    [<TestMethod>]
    member this.TestTryParseClass003Diag () =
        ad.Clear()
        let input = """
        def cl A #
    y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("DEF000"))

    [<TestMethod>]
    member this.TestTryParseClass003aDiag () =
        ad.Clear()
        let input = """
        def cl A -
    y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("DEF000"))

    [<TestMethod>]
    member this.TestTryParseClass004Diag () =
        ad.Clear()
        let input = """
        def cl A:
    y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("DEF000"))

    [<TestMethod>]
    member this.TestTryParseClass004aDiag () =
        ad.Clear()
        let input = """
        def cl A:@
    y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("DEF000"))

    [<TestMethod>]
    member this.TestTryParseClass005Diag () =
        ad.Clear()
        let input = """
        def cl A:obj
    y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("DEF000"))

    [<TestMethod>]
    member this.TestTryParseClass006Diag () =
        ad.Clear()
        let input = """
        def cl A:obj 
        {
    y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("DEF000"))

    [<TestMethod>]
    member this.TestTryParseClass007Diag () =
        ad.Clear()
        let input = """
        def cl A:obj 
        {
            dec ~a:obj ;
        }
    y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(3, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

    [<TestMethod>]
    member this.TestTryParseClass008Diag () =
        ad.Clear()
        let input = """
        def cl A:obj 
        {
            #
        }
    y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("DEF000"))


    [<TestMethod>]
    member this.TestTryParsePredicate000Diag () =
        ad.Clear()
        let input = """
        def pred
    y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))


    [<TestMethod>]
    member this.TestTryParsePredicate001Diag () =
        ad.Clear()
        let input = """
        def pred x
    y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("DEF000"))


    [<TestMethod>]
    member this.TestTryParsePredicate001aDiag () =
        ad.Clear()
        let input = """
        def pred .
    y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("DEF000"))


    [<TestMethod>]
    member this.TestTryParsePredicate001bDiag () =
        ad.Clear()
        let input = """
        def pred ,
    y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("DEF000"))


    [<TestMethod>]
    member this.TestTryParsePredicate002Diag () =
        ad.Clear()
        let input = """
        def pred A
    y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))


    [<TestMethod>]
    member this.TestTryParsePredicate003Diag () =
        ad.Clear()
        let input = """
        def pred A #
    y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("DEF000"))


    [<TestMethod>]
    member this.TestTryParsePredicate003aDiag () =
        ad.Clear()
        let input = """
        def pred A -
    y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("DEF000"))


    [<TestMethod>]
    member this.TestTryParsePredicate004Diag () =
        ad.Clear()
        let input = """
        def pred A:
    y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(3, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))


    [<TestMethod>]
    member this.TestTryParsePredicate004aDiag () =
        ad.Clear()
        let input = """
        def pred A(
    y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("DEF000"))


    [<TestMethod>]
    member this.TestTryParsePredicate005Diag () =
        ad.Clear()
        let input = """
        def pred A(#
    y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(3, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("DEF000"))


    [<TestMethod>]
    member this.TestTryParsePredicate005aDiag () =
        ad.Clear()
        let input = """
        def pred A()
    y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))


    [<TestMethod>]
    member this.TestTryParsePredicate006Diag () =
        ad.Clear()
        let input = """
        def pred A() 
        {
    y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("DEF000"))


    [<TestMethod>]
    member this.TestTryParsePredicate006aDiag () =
        ad.Clear()
        let input = """
        def pred A() 
        {
        }
    y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(3, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("DEF000"))


    [<TestMethod>]
    member this.TestTryParsePredicate007Diag () =
        ad.Clear()
        let input = """
        def pred A() 
        {
            dec ~a:obj ;
        }
    y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))


    [<TestMethod>]
    member this.TestTryParsePredicate008Diag () =
        ad.Clear()
        let input = """
        def pred A() 
        {
            #
        }
    y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("DEF000"))


    [<TestMethod>]
    member this.TestTryParsePredicate009Diag () =
        ad.Clear()
        let input = """
        def pred A() 
        {
            dec ~a:obj ;
        }
    y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))


    [<TestMethod>]
    member this.TestTryParsePredicate010Diag () =
        ad.Clear()
        let input = """
        def pred A() 
        {
            dec ~a:obj ;
        }
    y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))


    [<TestMethod>]
    member this.TestTryParsePredicate011Diag () =
        ad.Clear()
        let input = """
        def pred A() 
        {
            dec ~a: obj ;
            true
        }
    y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(1, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))


    [<TestMethod>]
    member this.TestTryParsePredicate012Diag () =
        ad.Clear()
        let input = """
        def pred A() 
        {
            true
        }
    y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(1, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))


    [<TestMethod>]
    member this.TestTryParseStatement000Diag () =
        ad.Clear()
        let input = """
        def pred A() 
        {
            dec
                x := 
            ;
            true
        }
    y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(4, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("VAR000"))


    [<TestMethod>]
    member this.TestTryParseStatement001Diag () =
        ad.Clear()
        let input = """
        def pred A() 
        {
            dec
                x := false
            ;
            true
        }
    y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(1, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))


    [<TestMethod>]
    member this.TestTryParseStatement002Diag () =
        ad.Clear()
        let input = """
        def pred A() 
        {
            dec 
                x := theorem
            ;
            true
        }
    y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(2, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("THM000"))


    [<TestMethod>]
    member this.TestTryParseStatement003Diag () =
        ad.Clear()
        let input = """
        def pred A() 
        {
            dec
                x := #
            ;
            true
        }
    y
;"""
        let result = fplParser (System.Uri("file://x")) input
        let actual = sprintf "%O" result
        let actualDiag = ad.DiagnosticsToString
        printf "%s" actualDiag
        Assert.AreEqual(3, ad.CountDiagnostics)
        Assert.IsTrue(actualDiag.Contains("SYN000"))

