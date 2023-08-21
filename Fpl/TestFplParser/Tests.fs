namespace TestFplParser

open FParsec
open FplGrammar
open Microsoft.VisualStudio.TestTools.UnitTesting
open Newtonsoft.Json


[<TestClass>]
type TestClass () =


    [<TestMethod>]
    member this.TestWildcardTheoryNamespace () =
        let result = run wildcardTheoryNamespace "Fpl.Test alias MyAlias"
        let actual = sprintf "%O" result
        let expected = """Success: AliasedNamespaceIdentifier (["Fpl"; "Test"], "MyAlias")"""
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestWildcardTheoryNamespace1 () =
        let result = run wildcardTheoryNamespace "Fpl.Test.*"
        let actual = sprintf "%O" result
        let expected = """Success: WildcaredNamespaceIdentifier ["Fpl"; "Test"]"""
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestWildcardTheoryNamespace2 () =
        let result = run wildcardTheoryNamespace "Fpl.Test"
        let actual = sprintf "%O" result
        let expected = """Success: NamespaceIdentifier ["Fpl"; "Test"]"""
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestUsesClause () =
        let result = run usesClause "uses Fpl.Test alias MyAlias ,Fpl.Test , Fpl.Test.*"
        let actual = sprintf "%O" result
        let expected = """Success: UsesClause
  [AliasedNamespaceIdentifier (["Fpl"; "Test"], "MyAlias");
   NamespaceIdentifier ["Fpl"; "Test"];
   WildcaredNamespaceIdentifier ["Fpl"; "Test"]]""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestPredicateIdentifier1 () =
        let result = run predicateIdentifier "ThisIsMyIdentifier"
        let actual = sprintf "%O" result
        let expected = """Success: AliasedId ["ThisIsMyIdentifier"]""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestPredicateIdentifier2 () =
        let result = run predicateIdentifier "This.Is.My.Identifier"
        let actual = sprintf "%O" result
        let expected = """Success: AliasedId ["This"; "Is"; "My"; "Identifier"]""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestIndexVariable1 () =
        let result = run indexValue "x$123"
        let actual = sprintf "%O" result
        let expected = """Success: IndexVariable ("x", "123")""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestIndexVariable2 () =
        let result = run indexValue "x$y"
        let actual = sprintf "%O" result
        let expected = """Success: IndexVariable ("x", "y")""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestAtList0 () =
        let result = run atList ""
        let actual = sprintf "%O" result
        let expected = """Success: []""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestAtList1 () =
        let result = run atList "@"
        let actual = sprintf "%O" result
        let expected = """Success: ['@']""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestAtList2 () =
        let result = run atList "@@"
        let actual = sprintf "%O" result
        let expected = """Success: ['@'; '@']""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestSelf0 () =
        let result = run self "self"
        let actual = sprintf "%O" result
        let expected = """Success: Self []""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestSelf1 () =
        let result = run self "@self"
        let actual = sprintf "%O" result
        let expected = """Success: Self ['@']""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestSelf2 () =
        let result = run self "@@self"
        let actual = sprintf "%O" result
        let expected = """Success: Self ['@'; '@']""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestEntitySelf () =
        let result = run entity "self"
        let actual = sprintf "%O" result
        let expected = """Success: Self []""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestEntityAtSelf () =
        let result = run entity "@self"
        let actual = sprintf "%O" result
        let expected = """Success: Self ['@']""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestEntityVariable () =
        let result = run entity "xyz"
        let actual = sprintf "%O" result
        let expected = """Success: Variable "xyz" """.Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestExtensionName () =
        let result = run extensionName "extDigits"
        let actual = sprintf "%O" result
        let expected = """Success: Extensionname "Digits" """.Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestExtensionDigits () =
        let result = run extensionBlock """
    :ext
        extDigits : /\d+/
    :end """
        let actual = sprintf "%O" result
        let expected = "Success: ExtensionBlock (Extensionname \"Digits\", ExtensionRegex \"/\d+/\n\")".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestBoundLeftClosed () =
        let result = run leftBound """["""
        let actual = sprintf "%O" result
        let expected = """Success: LeftClosed""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestBoundLeftClosed1 () =
        let result = run leftBound """[ """
        let actual = sprintf "%O" result
        let expected = """Success: LeftClosed""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestBoundLeftOpen () =
        let result = run leftBound """[!"""
        let actual = sprintf "%O" result
        let expected = """Success: LeftOpen""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestBoundLeftOpen1 () =
        let result = run leftBound """[ !"""
        let actual = sprintf "%O" result
        let expected = """Success: LeftOpen""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestBoundRightOpen () =
        let result = run rightBound """!]"""
        let actual = sprintf "%O" result
        let expected = """Success: RightOpen""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestBoundRightOpen1 () =
        let result = run rightBound """! ]"""
        let actual = sprintf "%O" result
        let expected = """Success: RightOpen""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestBoundRightClosed () =
        let result = run rightBound """]"""
        let actual = sprintf "%O" result
        let expected = """Success: RightClosed""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestBoundRightClosed1 () =
        let result = run rightBound """ ]"""
        let actual = sprintf "%O" result
        let expected = """Success: RightClosed""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestPrimePredicate1 () =
        let result = run primePredicate """true"""
        let actual = sprintf "%O" result
        let expected = """Success: True""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestPrimePredicate2 () =
        let result = run primePredicate """false"""
        let actual = sprintf "%O" result
        let expected = """Success: False""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestPrimePredicate3 () =
        let result = run primePredicate """undef"""
        let actual = sprintf "%O" result
        let expected = """Success: Undefined""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestPrimePredicate4 () =
        let result = run primePredicate """undefined"""
        let actual = sprintf "%O" result
        let expected = """Success: Undefined""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestConjunction1 () =
        let result = run conjunction """and(true,false)"""
        let actual = sprintf "%O" result
        let expected = """Success: And [True; False]""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestConjunction2 () =
        let result = run conjunction """and ( true, true )"""
        let actual = sprintf "%O" result
        let expected = """Success: And [True; True]""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestConjunction3 () =
        let result = run conjunction """and ( true, and( true, false))"""
        let actual = sprintf "%O" result
        let expected = """Success: And [True; And [True; False]]""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestConjunction4 () =
        let result = run conjunction """and ( and ( true, and( true, false)), true )"""
        let actual = sprintf "%O" result
        let expected = """Success: And [And [True; And [True; False]]; True]""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);
        
    [<TestMethod>]
    member this.TestDisjunctrion1 () =
        let result = run disjunction """or(true,false)"""
        let actual = sprintf "%O" result
        let expected = """Success: Or [True; False]""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestDisjunctrion2 () =
        let result = run disjunction """or ( true, true )"""
        let actual = sprintf "%O" result
        let expected = """Success: Or [True; True]""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestDisjunctrion3 () =
        let result = run disjunction """or ( true, or( true, false))"""
        let actual = sprintf "%O" result
        let expected = """Success: Or [True; Or [True; False]]""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestDisjunctrion4 () =
        let result = run disjunction """or ( or ( true, or( true, false)), true )"""
        let actual = sprintf "%O" result
        let expected = """Success: Or [Or [True; Or [True; False]]; True]""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestImplication1 () =
        let result = run implication """impl(true,false)"""
        let actual = sprintf "%O" result
        let expected = """Success: Impl (True, False)""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestImplication2 () =
        let result = run implication """impl ( true, true )"""
        let actual = sprintf "%O" result
        let expected = """Success: Impl (True, True)""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestImplication3 () =
        let result = run implication """impl ( true, impl( true, false))"""
        let actual = sprintf "%O" result
        let expected = """Success: Impl (True, Impl (True, False))""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestImplication4 () =
        let result = run implication """impl ( impl ( true, impl( true, false)), true )"""
        let actual = sprintf "%O" result
        let expected = """Success: Impl (Impl (True, Impl (True, False)), True)""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestEquivalence1 () =
        let result = run equivalence """iif(true,false)"""
        let actual = sprintf "%O" result
        let expected = """Success: Iif (True, False)""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestEquivalence2 () =
        let result = run equivalence """iif ( true, true )"""
        let actual = sprintf "%O" result
        let expected = """Success: Iif (True, True)""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestEquivalence3 () =
        let result = run equivalence """iif ( true, iif( true, false))"""
        let actual = sprintf "%O" result
        let expected = """Success: Iif (True, Iif (True, False))""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestEquivalence4 () =
        let result = run equivalence """iif ( iif ( true, iif( true, false)), true )"""
        let actual = sprintf "%O" result
        let expected = """Success: Iif (Iif (True, Iif (True, False)), True)""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestXor1 () =
        let result = run exclusiveOr """xor(true,false)"""
        let actual = sprintf "%O" result
        let expected = """Success: Xor (True, False)""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestXor2 () =
        let result = run exclusiveOr """xor ( true, true )"""
        let actual = sprintf "%O" result
        let expected = """Success: Xor (True, True)""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestXor3 () =
        let result = run exclusiveOr """xor ( true, xor( true, false))"""
        let actual = sprintf "%O" result
        let expected = """Success: Xor (True, Xor (True, False))""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestXor4 () =
        let result = run exclusiveOr """xor ( xor ( true, xor( true, false)), true )"""
        let actual = sprintf "%O" result
        let expected = """Success: Xor (Xor (True, Xor (True, False)), True)""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestNot1 () =
        let result = run negation """not(true)"""
        let actual = sprintf "%O" result
        let expected = """Success: Not True""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestNot2 () =
        let result = run negation """not (iif ( true, not(false)))"""
        let actual = sprintf "%O" result
        let expected = """Success: Not (Iif (True, Not False))""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);


    [<TestMethod>]
    member this.TestNot3 () =
        let result = run negation """not (iif ( iif( true, false), true))"""
        let actual = sprintf "%O" result
        let expected = """Success: Not (Iif (Iif (True, False), True))""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestNot4 () =
        let result = run negation """not(iif ( iif ( true, iif( true, false)), not(true) ))"""
        let actual = sprintf "%O" result
        let expected = """Success: Not (Iif (Iif (True, Iif (True, False)), Not True))""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);
