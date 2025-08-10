namespace FplParser.Tests

open FParsec
open FplGrammarCommons
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
        let result = run (justification .>> eof) """ProceedingResults(1.,2.)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestJustification04 () =
        let result = run (justification .>> eof) """3., GreaterTransitive """
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestJustification05 () =
        let result = run (justification .>> eof) """4., ModusPonens """
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestJustification06 () =
        let result = run (justification .>> eof) """4., ModusPonens, 1.  """
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"));

    [<TestMethod>]
    member this.TestJustification07 () =
        let result = run (justification .>> eof) """6., ExistsByExample(and(Greater(a,c), Greater(a,b)))  """
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
        let result = run (justification .>> eof) """and(a,b)  """
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestJustification10 () =
        let result = run (justification .>> eof) """and(a,b), 2.  """
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestJustification12 () =
        let result = run (justification .>> eof) """or(1.,2.), 2.  """
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
        let result = run (argumentInference .>> eof) """|- revoke 2."""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestJustifiedArgument01 () =
        let result = run (justifiedArgument .>> eof) """|-and(a,b)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestJustifiedArgument02 () =
        let result = run (justifiedArgument .>> eof) """|- revoke 2."""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestJustifiedArgument03 () =
        let result = run (justifiedArgument .>> eof) """and(a,b) |- revoke 2."""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestArgument01 () =
        let result = run (justifiedArgument .>> eof) """|- assume and(a,b)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestArgument02 () =
        let result = run (justifiedArgument .>> eof) """and(a,b) |- revoke 2."""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestArgument03 () =
        let result = run (justifiedArgument .>> eof) """|- revoke 2."""
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
        let result = run (justifiedArgument .>> eof) """2., 3. |- trivial"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestProof01 () =
        let result = run (proof .>> eof) """proof Example4$1
        {
            1. GreaterAB() |- Greater(a,b)
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
            1. |- assume true
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
            1. SomeCorollary$1() |- (a > b)
            qed
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))
        