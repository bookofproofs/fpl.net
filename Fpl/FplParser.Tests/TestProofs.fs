namespace FplParser.Tests

open FParsec
open FplPrimitives
open FplParsing.Combinators
open Microsoft.VisualStudio.TestTools.UnitTesting


[<TestClass>]
type TestProofs () =

    let replaceWhiteSpace (input: string) =
        let whiteSpaceChars = [|' '; '\t'; '\n'; '\r'|]
        input.Split(whiteSpaceChars)
            |> String.concat ""

    [<DataRow("""1. GreaterAB |-""")>]
    [<DataRow("""1. ProceedingResults, 1 |-""")>]    
    [<DataRow("""1. 3, GreaterTransitive  |-""")>]
    [<DataRow("""1. 4, byinf ModusPonens |- """)>]
    [<DataRow("""1. 1,2,  3 |- """)>]
     
    [<TestMethod>]
    member this.TestJustificationSuccess (fplCode:string) =
        let result = run (justification .>> eof) fplCode 
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("""3, GreaterTransitive  |-""")>]
    [<DataRow("""4, byinf ModusPonens |- """)>]
    [<DataRow("""1,2,  3 |- """)>]
    [<DataRow("""ProceedingResults, 1 |-""")>]    
    [<DataRow("""GreaterAB""")>]
    [<DataRow("""|-""")>]
    [<DataRow(""" |- """)>]
    [<TestMethod>]
    member this.TestJustificationFailure (fplCode:string) =
        let result = run (justification .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestPremiseOrOtherPredicate01 () =
        let result = run (predicate .>> eof) LiteralPre
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestPremiseOrOtherPredicate02 () =
        let result = run (predicate .>> eof) """premise """
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestPremiseOrOtherPredicate03 () =
        let result = run (predicate .>> eof) """and(a,b)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestDerivedArgument01 () =
        let result = run (derivedArgument .>> eof) LiteralQed
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestDerivedArgument02 () =
        let result = run (derivedArgument .>> eof) LiteralTrivial
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestDerivedArgument03 () =
        let result = run (derivedArgument .>> eof) LiteralCon
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestDerivedArgument04 () =
        let result = run (derivedArgument .>> eof) LiteralConL
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestDerivedArgument05 () =
        let result = run (derivedArgument .>> eof) """and(a,b)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))


    [<TestMethod>]
    member this.TestArgumentInference01 () =
        let result = run (argumentInference .>> eof) """qed"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestArgumentInference02 () =
        let result = run (argumentInference .>> eof) """trivial"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestArgumentInference03 () =
        let result = run (argumentInference .>> eof) """con"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestArgumentInference04 () =
        let result = run (argumentInference .>> eof) """conclusion"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestArgumentInference05 () =
        let result = run (argumentInference .>> eof) """and(a,b)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestArgumentInference06 () =
        let result = run (argumentInference .>> eof) """|- revoke 2"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<DataRow("1: and(a,b)")>]
    [<DataRow("1: revoke 2")>]
    [<DataRow("1. bydef A, 1, C |- revoke 2")>]
    [<DataRow("1. T$1:1 |- revoke 2")>]
    [<DataRow("1. T$1:1, T$1:2, T$1: 3 |- revoke 2")>]
    [<DataRow("1: assume and(a,b)")>]
    [<DataRow("1: assume true")>]
    [<DataRow("1: revoke 2")>]
    [<TestMethod>]
    member this.TestArgumentSuccess (test:string) =
        let result = run (justifiedArgument .>> eof) test
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))


    [<DataRow("1: B, C |- revoke 2")>]
    [<DataRow("1. |- revoke 2")>]
    [<DataRow("1: qed")>]
    [<DataRow("1: |- trivial")>]
    [<DataRow("1. |- trivial")>]
    [<DataRow("2, 3 |- trivial")>]
    [<TestMethod>]
    member this.TestArgumentFailure (test:string) =
        let result = run (justifiedArgument .>> eof) test
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<DataRow("""proof Example4$1 {1. GreaterAB |- Greater(a,b) qed}""")>]
    [<DataRow("""prf AddIsUnique$1 {1: assume and(x,b) 2: trivial qed}""")>]
    [<TestMethod>]
    member this.TestProofSuccess (test:string) =
        let result = run (proof .>> eof) test
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))


    [<DataRow("""prf AddIsUnique$1 {1: assume pre 2: trivial}""")>]
    [<TestMethod>]
    member this.TestProofFailure (test:string) =
        let result = run (proof .>> eof) test
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestProof04 () =
        let result = run (proof .>> eof) """proof Example4$1
        {
            1. SomeCorollary$1 |- (a > b)
            qed
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))
        
    [<DataRow(LiteralByCor, "$1", ":1")>]
    [<DataRow(LiteralByDef, "$1", ":1")>]
    [<DataRow(LiteralByAx, "$1", ":1")>]
    [<DataRow(LiteralByInf, "$1", ":1")>]
    [<DataRow(LiteralByCor, "$1", "")>]
    [<DataRow(LiteralByDef, "$1", "")>]
    [<DataRow(LiteralByAx, "$1", "")>]
    [<DataRow(LiteralByInf, "$1", "")>]
    [<DataRow(LiteralByCor, "", ":1")>]
    [<DataRow(LiteralByDef, "", ":1")>]
    [<DataRow(LiteralByAx, "", ":1")>]
    [<DataRow(LiteralByInf, "", "")>]
    [<DataRow(LiteralByCor, "", "")>]
    [<DataRow(LiteralByDef, "", "")>]
    [<DataRow(LiteralByAx, "", "")>]
    [<DataRow(LiteralByInf, "", "")>]
    [<TestMethod>]
    member this.TestJustificationIdentifier (keyword:string, corRef:string, argRef:string) =
        let result = run (justificationItem .>> eof) $"{keyword} A{corRef}{argRef}"
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestJustificationIdentifierByDef () =
        let result = run (justificationItem .>> eof) $"bydef x"
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))
