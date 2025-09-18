namespace FplParser.Tests

open FParsec
open FplPrimitives
open FplParser
open Microsoft.VisualStudio.TestTools.UnitTesting


[<TestClass>]
type TestProofs () =

    let replaceWhiteSpace (input: string) =
        let whiteSpaceChars = [|' '; '\t'; '\n'; '\r'|]
        input.Split(whiteSpaceChars)
            |> String.concat ""

    [<TestMethod>]
    member this.TestJustification01 () =
        let result = run (justification .>> eof) """GreaterAB"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestJustification02 () =
        let result = run (justification .>> eof) """GreaterAB"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestJustification03 () =
        let result = run (justification .>> eof) """ProceedingResults, 1"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestJustification04 () =
        let result = run (justification .>> eof) """3, GreaterTransitive """
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestJustification05 () =
        let result = run (justification .>> eof) """4, ModusPonens """
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestJustification06 () =
        let result = run (justification .>> eof) """4, ModusPonens, 1  """
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"));

    [<TestMethod>]
    member this.TestJustification07 () =
        let result = run (justification .>> eof) """6, ExistsByExample  """
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestJustification08 () =
        let result = run (justification .>> eof) """ """
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestJustification09 () =
        let result = run (justification .>> eof) """1,2,  3  """
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestJustification10 () =
        let result = run (justification .>> eof) """BB, 2  """
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestJustification12 () =
        let result = run (justification .>> eof) """C, 2  """
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPremiseOrOtherPredicate01 () =
        let result = run (predicate .>> eof) literalPre
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
        let result = run (derivedArgument .>> eof) literalQed
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestDerivedArgument02 () =
        let result = run (derivedArgument .>> eof) literalTrivial
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestDerivedArgument03 () =
        let result = run (derivedArgument .>> eof) literalCon
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestDerivedArgument04 () =
        let result = run (derivedArgument .>> eof) literalConL
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
        let result = run (argumentInference .>> eof) """|- qed"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestArgumentInference02 () =
        let result = run (argumentInference .>> eof) """|- trivial"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestArgumentInference03 () =
        let result = run (argumentInference .>> eof) """|- con"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestArgumentInference04 () =
        let result = run (argumentInference .>> eof) """|- conclusion"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestArgumentInference05 () =
        let result = run (argumentInference .>> eof) """|- and(a,b)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestArgumentInference06 () =
        let result = run (argumentInference .>> eof) """|- revoke 2"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("|-and(a,b)")>]
    [<DataRow("|- revoke 2")>]
    [<DataRow("bydef A, 1, C |- revoke 2")>]
    [<DataRow("T$1:1 |- revoke 2")>]
    [<DataRow("T$1:1, T$1:2, T$1: 3 |- revoke 2")>]
    [<DataRow("|- assume and(a,b)")>]
    [<TestMethod>]
    member this.TestArgument (test:string) =
        let result = run (justifiedArgument .>> eof) test
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))


    [<TestMethod>]
    member this.TestArgument02 () =
        let result = run (justifiedArgument .>> eof) """B, C |- revoke 2"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestArgument03 () =
        let result = run (justifiedArgument .>> eof) """|- revoke 2"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestArgument04 () =
        let result = run (justifiedArgument .>> eof) """|- qed"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestArgument05 () =
        let result = run (justifiedArgument .>> eof) """|- trivial"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestArgument06 () =
        let result = run (justifiedArgument .>> eof) """2, 3 |- trivial"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestProof01 () =
        let result = run (proof .>> eof) """proof Example4$1
        {
            1. GreaterAB |- Greater(a,b)
            qed
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestProof02 () =
        let result = run (proof .>> eof) """prf AddIsUnique$1
        {
            1. |- assume pre
            2. |- trivial
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestProof03 () =
        let result = run (proof .>> eof) """prf AddIsUnique$1
        {
            1. |- assume and(x,b)
            2. |- trivial
            qed
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

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
        
    [<DataRow(FplPrimitives.literalByCor, "$1", ":1")>]
    [<DataRow(FplPrimitives.literalByDef, "$1", ":1")>]
    [<DataRow(FplPrimitives.literalByAx, "$1", ":1")>]
    [<DataRow(FplPrimitives.literalByInf, "$1", ":1")>]
    [<DataRow(FplPrimitives.literalByCor, "$1", "")>]
    [<DataRow(FplPrimitives.literalByDef, "$1", "")>]
    [<DataRow(FplPrimitives.literalByAx, "$1", "")>]
    [<DataRow(FplPrimitives.literalByInf, "$1", "")>]
    [<DataRow(FplPrimitives.literalByCor, "", ":1")>]
    [<DataRow(FplPrimitives.literalByDef, "", ":1")>]
    [<DataRow(FplPrimitives.literalByAx, "", ":1")>]
    [<DataRow(FplPrimitives.literalByInf, "", "")>]
    [<DataRow(FplPrimitives.literalByCor, "", "")>]
    [<DataRow(FplPrimitives.literalByDef, "", "")>]
    [<DataRow(FplPrimitives.literalByAx, "", "")>]
    [<DataRow(FplPrimitives.literalByInf, "", "")>]
    [<TestMethod>]
    member this.TestJustificationIdentifier (keyword:string, corRef:string, argRef:string) =
        let result = run (justificationReference .>> eof) $"{keyword} A{corRef}{argRef}"
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestJustificationIdentifierByDef () =
        let result = run (justificationReference .>> eof) $"bydef x"
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))
