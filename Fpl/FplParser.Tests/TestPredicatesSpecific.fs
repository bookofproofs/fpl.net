namespace FplParser.Tests

open FParsec
open FplGrammar
open Microsoft.VisualStudio.TestTools.UnitTesting



[<TestClass>]
type TestPredicatesSpecific () =

    let replaceWhiteSpace (input: string) =
        let whiteSpaceChars = [|' '; '\t'; '\n'; '\r'|]
        input.Split(whiteSpaceChars)
            |> String.concat ""


    [<TestMethod>]
    member this.TestPrimePredicate1 () =
        let result = run primePredicate """true"""
        let actual = sprintf "%O" result
        let expected = """Success: True"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestPrimePredicate2 () =
        let result = run primePredicate """false"""
        let actual = sprintf "%O" result
        let expected = """Success: False"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestPrimePredicate3 () =
        let result = run primePredicate """undef"""
        let actual = sprintf "%O" result
        let expected = """Success: Undefined"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestPrimePredicate4 () =
        let result = run primePredicate """undefined"""
        let actual = sprintf "%O" result
        let expected = """Success: Undefined"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestPrimePredicate5 () =
        let result = run primePredicate """list$i"""
        let actual = sprintf "%O" result
        let expected = """Success: PredicateWithoutArgs (IndexVariable ("list", "i"))"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestPrimePredicate6 () =
        let result = run primePredicate """arr[i]"""
        let actual = sprintf "%O" result
        let expected = """Success: PredicateWithoutArgs (EntityWithCoord (Var "arr", BrackedCoordList [Var "i"]))"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestPrimePredicate7 () =
        let result = run primePredicate """@@self"""
        let actual = sprintf "%O" result
        let expected = """Success: PredicateWithoutArgs (Self ['@'; '@'])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestPrimePredicate8 () =
        let result = run primePredicate """del.add(n,m)"""
        let actual = sprintf "%O" result
        let expected = """Success: PredicateWithArgs
  (DelegateId "add",
   [PredicateWithoutArgs (Var "n"); PredicateWithoutArgs (Var "m")])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestConjunction1 () =
        let result = run conjunction """and(true,false)"""
        let actual = sprintf "%O" result
        let expected = """Success: And [True; False]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestConjunction2 () =
        let result = run conjunction """and ( true, true )"""
        let actual = sprintf "%O" result
        let expected = """Success: And [True; True]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestConjunction3 () =
        let result = run conjunction """and ( true, and( true, false))"""
        let actual = sprintf "%O" result
        let expected = """Success: And [True; And [True; False]]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestConjunction4 () =
        let result = run conjunction """and ( and ( true, and( true, false)), true )"""
        let actual = sprintf "%O" result
        let expected = """Success: And [And [True; And [True; False]]; True]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);
        
    [<TestMethod>]
    member this.TestDisjunctrion1 () =
        let result = run disjunction """or(true,false)"""
        let actual = sprintf "%O" result
        let expected = """Success: Or [True; False]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestDisjunctrion2 () =
        let result = run disjunction """or ( true, true )"""
        let actual = sprintf "%O" result
        let expected = """Success: Or [True; True]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestDisjunctrion3 () =
        let result = run disjunction """or ( true, or( true, false))"""
        let actual = sprintf "%O" result
        let expected = """Success: Or [True; Or [True; False]]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestDisjunctrion4 () =
        let result = run disjunction """or ( or ( true, or( true, false)), true )"""
        let actual = sprintf "%O" result
        let expected = """Success: Or [Or [True; Or [True; False]]; True]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestDisjunctrion5 () =
        let result = run disjunction """or(1.,2.)"""
        let actual = sprintf "%O" result
        let expected = """Success: Or [ArgumentIdentifier "1."; ArgumentIdentifier "2."]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestImplication1 () =
        let result = run implication """impl(true,false)"""
        let actual = sprintf "%O" result
        let expected = """Success: Impl (True, False)"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestImplication2 () =
        let result = run implication """impl ( true, true )"""
        let actual = sprintf "%O" result
        let expected = """Success: Impl (True, True)"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestImplication3 () =
        let result = run implication """impl ( true, impl( true, false))"""
        let actual = sprintf "%O" result
        let expected = """Success: Impl (True, Impl (True, False))"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestImplication4 () =
        let result = run implication """impl ( impl ( true, impl( true, false)), true )"""
        let actual = sprintf "%O" result
        let expected = """Success: Impl (Impl (True, Impl (True, False)), True)"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestEquivalence1 () =
        let result = run equivalence """iif(true,false)"""
        let actual = sprintf "%O" result
        let expected = """Success: Iif (True, False)"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestEquivalence2 () =
        let result = run equivalence """iif ( true, true )"""
        let actual = sprintf "%O" result
        let expected = """Success: Iif (True, True)"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestEquivalence3 () =
        let result = run equivalence """iif ( true, iif( true, false))"""
        let actual = sprintf "%O" result
        let expected = """Success: Iif (True, Iif (True, False))"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestEquivalence4 () =
        let result = run equivalence """iif ( iif ( true, iif( true, false)), true )"""
        let actual = sprintf "%O" result
        let expected = """Success: Iif (Iif (True, Iif (True, False)), True)"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestXor1 () =
        let result = run exclusiveOr """xor(true,false)"""
        let actual = sprintf "%O" result
        let expected = """Success: Xor (True, False)"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestXor2 () =
        let result = run exclusiveOr """xor ( true, true )"""
        let actual = sprintf "%O" result
        let expected = """Success: Xor (True, True)"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestXor3 () =
        let result = run exclusiveOr """xor ( true, xor( true, false))"""
        let actual = sprintf "%O" result
        let expected = """Success: Xor (True, Xor (True, False))"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestXor4 () =
        let result = run exclusiveOr """xor ( xor ( true, xor( true, false)), true )"""
        let actual = sprintf "%O" result
        let expected = """Success: Xor (Xor (True, Xor (True, False)), True)"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestPredicateWithArgs1 () =
        let result = run predicateWithQualification """Zero()"""
        let actual = sprintf "%O" result
        let expected = """Success: PredicateWithArgs (AliasedId ["Zero"], [])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestPredicateWithArgs2 () =
        let result = run predicateWithQualification """self(i)"""
        let actual = sprintf "%O" result
        let expected = """Success: PredicateWithArgs (Self [], [PredicateWithoutArgs (Var "i")])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestPredicateWithArgs3 () =
        let result = run predicateWithQualification """ProceedingResults(1.,2.)"""
        let actual = sprintf "%O" result
        let expected = """Success: PredicateWithArgs
  (AliasedId ["ProceedingResults"],
   [ArgumentIdentifier "1."; ArgumentIdentifier "2."])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestPredicateWithArgs4 () =
        let result = run predicateWithQualification """Add(result,list$i)"""
        let actual = sprintf "%O" result
        let expected = """Success: PredicateWithArgs
  (AliasedId ["Add"],
   [PredicateWithoutArgs (Var "result");
    PredicateWithoutArgs (IndexVariable ("list", "i"))])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestPredicateWithArgs5 () =
        let result = run predicateWithQualification """Add(result,arr[i])"""
        let actual = sprintf "%O" result
        let expected = """Success: PredicateWithArgs
  (AliasedId ["Add"],
   [PredicateWithoutArgs (Var "result");
    PredicateWithoutArgs
      (EntityWithCoord (Var "arr", BrackedCoordList [Var "i"]))])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestQualifiedIdentifier1 () =
        let result = run predicateWithQualification """myOp.NeutralElement()"""
        let actual = sprintf "%O" result
        let expected = """Success: QualifiedIdentifier
  (Var "myOp", [PredicateWithArgs (AliasedId ["NeutralElement"], [])])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestQualifiedIdentifier2 () =
        let result = run predicateWithQualification """myOp.NeutralElement().SomeProperty()"""
        let actual = sprintf "%O" result
        let expected = """Success: QualifiedIdentifier
  (Var "myOp",
   [PredicateWithArgs (AliasedId ["NeutralElement"], []);
    PredicateWithArgs (AliasedId ["SomeProperty"], [])])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestNot1 () =
        let result = run negation """not(true)"""
        let actual = sprintf "%O" result
        let expected = """Success: Not True"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestNot2 () =
        let result = run negation """not (iif ( true, not(false)))"""
        let actual = sprintf "%O" result
        let expected = """Success: Not (Iif (True, Not False))"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);


    [<TestMethod>]
    member this.TestNot3 () =
        let result = run negation """not (iif ( iif( true, false), true))"""
        let actual = sprintf "%O" result
        let expected = """Success: Not (Iif (Iif (True, False), True))"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestNot4 () =
        let result = run negation """not(iif ( iif ( true, iif( true, false)), not(true) ))"""
        let actual = sprintf "%O" result
        let expected = """Success: Not (Iif (Iif (True, Iif (True, False)), Not True))"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestIsOperator1 () =
        let result = run isOperator """is(x, Nat)"""
        let actual = sprintf "%O" result
        let expected = """Success: IsOperator (Var "x", VariableTypeWithModifier (None, ClassHeaderType ["Nat"]))"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestAll1 () =
        let result = run all """all x,y,z(true)"""
        let actual = sprintf "%O" result
        let expected = """Success: All ([Var "x"; Var "y"; Var "z"], True)"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestAll2 () =
        let result = run all """all x,y,z (not (iif ( true, not(false))))"""
        let actual = sprintf "%O" result
        let expected = """Success: All ([Var "x"; Var "y"; Var "z"], Not (Iif (True, Not False)))"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestAll3 () =
        let result = run all """all x,y,z (not (iif ( iif( true, false), true)))"""
        let actual = sprintf "%O" result
        let expected = """Success: All ([Var "x"; Var "y"; Var "z"], Not (Iif (Iif (True, False), True)))"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestAll4 () =
        let result = run all """all x (not(iif ( iif ( true, iif( true, false)), not(true) )))"""
        let actual = sprintf "%O" result
        let expected = """Success: All ([Var "x"], Not (Iif (Iif (True, Iif (True, False)), Not True)))"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestEx1 () =
        let result = run exists """ex x,y,z(true)"""
        let actual = sprintf "%O" result
        let expected = """Success: Exists ([Var "x"; Var "y"; Var "z"], True)"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestEx2 () =
        let result = run exists """ex x,y,z (not (iif ( true, not(false))))"""
        let actual = sprintf "%O" result
        let expected = """Success: Exists ([Var "x"; Var "y"; Var "z"], Not (Iif (True, Not False)))"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestEx3 () =
        let result = run exists """ex x,y,z (not (iif ( iif( true, false), true)))"""
        let actual = sprintf "%O" result
        let expected = """Success: Exists ([Var "x"; Var "y"; Var "z"], Not (Iif (Iif (True, False), True)))"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestEx4 () =
        let result = run exists """ex x (not(iif ( iif ( true, iif( true, false)), not(true) )))"""
        let actual = sprintf "%O" result
        let expected = """Success: Exists ([Var "x"], Not (Iif (Iif (True, Iif (True, False)), Not True)))"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestExN1 () =
        let result = run existsTimesN """ex$0 x,y,z(true)"""
        let actual = replaceWhiteSpace (sprintf "%O" result)
        let expected = replaceWhiteSpace """Success: ExistsN ((DollarDigits "0", [Var "x"; Var "y"; Var "z"]), True)"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestExN2 () =
        let result = run existsTimesN """ex$1 x,y,z (not (iif ( true, not(false))))"""
        let actual = replaceWhiteSpace (sprintf "%O" result)
        let expected = replaceWhiteSpace """Success: ExistsN ((DollarDigits "1", [Var "x"; Var "y"; Var "z"]), Not (Iif (True, Not False)))"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestExN3 () =
        let result = run existsTimesN """ex$2 x,y,z (not (iif ( iif( true, false), true)))"""
        let actual = replaceWhiteSpace (sprintf "%O" result)
        let expected = replaceWhiteSpace """Success: ExistsN
  ((DollarDigits "2", [Var "x"; Var "y"; Var "z"]), Not (Iif (Iif (True, False), True)))"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestExN4 () =
        let result = run existsTimesN """ex$3 x (not(iif ( iif ( true, iif( true, false)), not(true) )))"""
        let actual = sprintf "%O" result
        let expected = """Success: ExistsN ((DollarDigits "3", [Var "x"]), Not (Iif (Iif (True, Iif (True, False)), Not True)))"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

 