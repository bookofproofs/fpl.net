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
        let result = run justification """GreaterAB|-"""
        let actual = sprintf "%O" result
        let expected = """Success: Justification [PredicateWithoutArgs (AliasedId ["GreaterAB"])]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestJustification02 () =
        let result = run justification """GreaterAB |-"""
        let actual = sprintf "%O" result
        let expected = """Success: Justification [PredicateWithoutArgs (AliasedId ["GreaterAB"])]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestJustification03 () =
        let result = run justification """ProceedingResults(1.,2.) |-"""
        let actual = sprintf "%O" result
        let expected = """Success: Justification
  [PredicateWithArgs
     (AliasedId ["ProceedingResults"],
      [ArgumentIdentifier "1."; ArgumentIdentifier "2."])]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestJustification04 () =
        let result = run justification """3., GreaterTransitive |-"""
        let actual = sprintf "%O" result
        let expected = """Success: Justification
  [ArgumentIdentifier "3.";
   PredicateWithoutArgs (AliasedId ["GreaterTransitive"])]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestJustification05 () =
        let result = run justification """4., ModusPonens|-"""
        let actual = sprintf "%O" result
        let expected = """Success: Justification
  [ArgumentIdentifier "4."; PredicateWithoutArgs (AliasedId ["ModusPonens"])]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestJustification06 () =
        let result = run justification """4., ModusPonens, 1. |-"""
        let actual = sprintf "%O" result
        let expected = """Success: Justification
  [ArgumentIdentifier "4."; PredicateWithoutArgs (AliasedId ["ModusPonens"]);
   ArgumentIdentifier "1."]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestJustification07 () =
        let result = run justification """6., ExistsByExample(and(Greater(a,c), Greater(a,b))) |-"""
        let actual = sprintf "%O" result
        let expected = """Success: Justification
  [ArgumentIdentifier "6.";
   PredicateWithArgs
     (AliasedId ["ExistsByExample"],
      [And
         [PredicateWithArgs
            (AliasedId ["Greater"],
             [PredicateWithoutArgs (Var "a"); PredicateWithoutArgs (Var "c")]);
          PredicateWithArgs
            (AliasedId ["Greater"],
             [PredicateWithoutArgs (Var "a"); PredicateWithoutArgs (Var "b")])]])]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestJustification08 () =
        let result = run justification """|-"""
        let actual = sprintf "%O" result
        let expected = """Success: Justification []"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestJustification09 () =
        let result = run justification """and(a,b) |-"""
        let actual = sprintf "%O" result
        let expected = """Success: Justification
  [And [PredicateWithoutArgs (Var "a"); PredicateWithoutArgs (Var "b")]]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestJustification10 () =
        let result = run justification """and(a,b), 2. |-"""
        let actual = sprintf "%O" result
        let expected = """Success: Justification
  [And [PredicateWithoutArgs (Var "a"); PredicateWithoutArgs (Var "b")];
   ArgumentIdentifier "2."]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestJustification12 () =
        let result = run justification """or(1.,2.), 2. |-"""
        let actual = sprintf "%O" result
        let expected = """Success: Justification
  [Or [ArgumentIdentifier "1."; ArgumentIdentifier "2."];
   ArgumentIdentifier "2."]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestPremiseOrOtherPredicate01 () =
        let result = run premiseOrOtherPredicate """pre"""
        let actual = sprintf "%O" result
        let expected = """Success: PremiseReference"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestPremiseOrOtherPredicate02 () =
        let result = run premiseOrOtherPredicate """premise """
        let actual = sprintf "%O" result
        let expected = """Success: PremiseReference"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestPremiseOrOtherPredicate03 () =
        let result = run premiseOrOtherPredicate """and(a,b)"""
        let actual = sprintf "%O" result
        let expected = """Success: And [PredicateWithoutArgs (Var "a"); PredicateWithoutArgs (Var "b")]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestDerivedArgument01 () =
        let result = run derivedArgument """qed"""
        let actual = sprintf "%O" result
        let expected = """Success: Qed"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestDerivedArgument02 () =
        let result = run derivedArgument """trivial"""
        let actual = sprintf "%O" result
        let expected = """Success: Trivial"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestDerivedArgument03 () =
        let result = run derivedArgument """con"""
        let actual = sprintf "%O" result
        let expected = """Success: ConclusionReference"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestDerivedArgument04 () =
        let result = run derivedArgument """conclusion"""
        let actual = sprintf "%O" result
        let expected = """Success: ConclusionReference"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestDerivedArgument05 () =
        let result = run derivedArgument """and(a,b)"""
        let actual = sprintf "%O" result
        let expected = """Success: DerivedPredicate
  (And [PredicateWithoutArgs (Var "a"); PredicateWithoutArgs (Var "b")])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestArgumentInference01 () =
        let result = run argumentInference """qed"""
        let actual = sprintf "%O" result
        let expected = """Success: Qed"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestArgumentInference02 () =
        let result = run argumentInference """trivial"""
        let actual = sprintf "%O" result
        let expected = """Success: Trivial"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestArgumentInference03 () =
        let result = run argumentInference """con"""
        let actual = sprintf "%O" result
        let expected = """Success: ConclusionReference"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestArgumentInference04 () =
        let result = run argumentInference """conclusion"""
        let actual = sprintf "%O" result
        let expected = """Success: ConclusionReference"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestArgumentInference05 () =
        let result = run argumentInference """and(a,b)"""
        let actual = sprintf "%O" result
        let expected = """Success: DerivedPredicate
  (And [PredicateWithoutArgs (Var "a"); PredicateWithoutArgs (Var "b")])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestArgumentInference06 () =
        let result = run argumentInference """revoke 2."""
        let actual = sprintf "%O" result
        let expected = """Success: RevokeArgument (ArgumentIdentifier "2.")"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestJustifiedArgument01 () =
        let result = run justifiedArgument """|-and(a,b)"""
        let actual = sprintf "%O" result
        let expected = """Success: JustifiedArgument
  (Justification [],
   DerivedPredicate
     (And [PredicateWithoutArgs (Var "a"); PredicateWithoutArgs (Var "b")]))"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestJustifiedArgument02 () =
        let result = run justifiedArgument """|- revoke 2."""
        let actual = sprintf "%O" result
        let expected = """Success: JustifiedArgument (Justification [], RevokeArgument (ArgumentIdentifier "2."))"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestJustifiedArgument03 () =
        let result = run justifiedArgument """and(a,b) |- revoke 2."""
        let actual = sprintf "%O" result
        let expected = """Success: JustifiedArgument
  (Justification
     [And [PredicateWithoutArgs (Var "a"); PredicateWithoutArgs (Var "b")]],
   RevokeArgument (ArgumentIdentifier "2."))"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestArgument01 () =
        let result = run argument """assume and(a,b)"""
        let actual = sprintf "%O" result
        let expected = """Success: AssumeArgument
  (And [PredicateWithoutArgs (Var "a"); PredicateWithoutArgs (Var "b")])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestArgument02 () =
        let result = run argument """and(a,b) |- revoke 2."""
        let actual = sprintf "%O" result
        let expected = """Success: JustifiedArgument
  (Justification
     [And [PredicateWithoutArgs (Var "a"); PredicateWithoutArgs (Var "b")]],
   RevokeArgument (ArgumentIdentifier "2."))"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestArgument03 () =
        let result = run argument """|- revoke 2."""
        let actual = sprintf "%O" result
        let expected = """Success: JustifiedArgument (Justification [], RevokeArgument (ArgumentIdentifier "2."))"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestArgument04 () =
        let result = run argument """|- qed"""
        let actual = sprintf "%O" result
        let expected = """Success: JustifiedArgument (Justification [], Qed)"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestArgument05 () =
        let result = run argument """|- trivial"""
        let actual = sprintf "%O" result
        let expected = """Success: JustifiedArgument (Justification [], Trivial)"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestArgument06 () =
        let result = run argument """2., 3. |- trivial"""
        let actual = sprintf "%O" result
        let expected = """Success: JustifiedArgument
  (Justification [ArgumentIdentifier "2."; ArgumentIdentifier "3."], Trivial)"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestProof01 () =
        let result = run proof """proof Example4$1
        {
            1. GreaterAB() |- Greater(a,b)
            2. |- qed
        }"""
        let actual = replaceWhiteSpace (sprintf "%O" result)
        let expected = replaceWhiteSpace """Success: Proof
      ((AliasedId ["Example4"], [DollarDigits "1"]),
       ([],
        [Argument
           (ArgumentIdentifier "1.",
            JustifiedArgument
              (Justification [PredicateWithArgs (AliasedId ["GreaterAB"], [])],
               DerivedPredicate
                 (PredicateWithArgs
                    (AliasedId ["Greater"],
                     [PredicateWithoutArgs (Var "a"); PredicateWithoutArgs (Var "b")]))));
         Argument
           (ArgumentIdentifier "2.", JustifiedArgument (Justification [], Qed))]))"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestProof02 () =
        let result = run proof """prf AddIsUnique$1
        {
            1. assume pre
            2. |- trivial
        }"""
        let actual = replaceWhiteSpace (sprintf "%O" result)
        let expected = replaceWhiteSpace """Success: Proof
      ((AliasedId ["AddIsUnique"], [DollarDigits "1"]),
       ([],
        [Argument (ArgumentIdentifier "1.", AssumeArgument PremiseReference);
         Argument
           (ArgumentIdentifier "2.", JustifiedArgument (Justification [], Trivial))]))"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

        