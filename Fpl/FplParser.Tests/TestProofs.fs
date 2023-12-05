namespace FplParser.Tests

open FParsec
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
        let result = run (predicate .>> eof) """pre"""
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
        let result = run (derivedArgument .>> eof) """qed"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestDerivedArgument02 () =
        let result = run (derivedArgument .>> eof) """trivial"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestDerivedArgument03 () =
        let result = run (derivedArgument .>> eof) """con"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestDerivedArgument04 () =
        let result = run (derivedArgument .>> eof) """conclusion"""
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
        Assert.IsTrue(actual.StartsWith("Success:"))

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
        let result = run (argument .>> eof) """|-and(a,b)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestJustifiedArgument02 () =
        let result = run (argument .>> eof) """|- revoke 2."""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestJustifiedArgument03 () =
        let result = run (argument .>> eof) """and(a,b) |- revoke 2."""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestArgument01 () =
        let result = run (argument .>> eof) """|- assume and(a,b)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestArgument02 () =
        let result = run (argument .>> eof) """and(a,b) |- revoke 2."""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestArgument03 () =
        let result = run (argument .>> eof) """|- revoke 2."""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestArgument04 () =
        let result = run (argument .>> eof) """|- qed"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestArgument05 () =
        let result = run (argument .>> eof) """|- trivial"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestArgument06 () =
        let result = run (argument .>> eof) """2., 3. |- trivial"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestProof01 () =
        let result = run (proof .>> eof) """proof Example4$1
        {
            1. GreaterAB() |- Greater(a,b)
            2. |- qed
        }"""
        let actual = replaceWhiteSpace (sprintf "%O" result)
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestProof02 () =
        let result = run (proof .>> eof) """prf AddIsUnique$1
        {
            1. |- assume pre
            2. |- trivial
        }"""
        let actual = replaceWhiteSpace (sprintf "%O" result)
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestProof03 () =
        let result = run (proof .>> eof) """prf AddIsUnique $1
        {
            1. |- assume true
            2. |- trivial
        }"""
        let actual = replaceWhiteSpace (sprintf "%O" result)
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestProof04 () =
        let result = run (proof .>> eof) """proof Example4  $1
        {
            1. SomeCorollary $1() |- Greater(a,b)
            2. |- qed
        }"""
        let actual = (sprintf "%O" result)
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))
        