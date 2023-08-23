namespace TestFplParser

open FParsec
open FplGrammar
open Microsoft.VisualStudio.TestTools.UnitTesting
open Newtonsoft.Json


[<TestClass>]
type TestIdentifiers () =

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
        let result = run indexVariable "x$123"
        let actual = sprintf "%O" result
        let expected = """Success: IndexVariable ("x", "123")""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestIndexVariable2 () =
        let result = run indexVariable "x$y"
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
        let expected = """Success: Var "xyz" """.Trim().Replace("\r","")
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
    member this.TestEntityWithCoord1 () =
        let result = run entityWithCoord """myField[1 ~ n]"""
        let actual = sprintf "%O" result
        let expected = """Success: EntityWithCoord
  (Var "myField",
   ClosedOrOpenRange
     ((LeftClosed, (Some (ExtDigits "1"), Some (Var "n"))), RightClosed))""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestEntityWithCoord2 () =
        let result = run entityWithCoord """theorem[from ~ to]"""
        let actual = sprintf "%O" result
        let actual2 = actual.Replace("\r","")
        let expected = "Failure:\nError in Ln: 1 Col: 8\ntheorem[from ~ to]\n       ^\nreserved FPL keyword\n"
        Assert.AreEqual(expected, actual2);

    [<TestMethod>]
    member this.TestEntityWithCoord3 () =
        let result = run entityWithCoord """self[from ~ to]"""
        let actual = sprintf "%O" result
        let expected = """Success: EntityWithCoord
  (Self [],
   ClosedOrOpenRange
     ((LeftClosed, (Some (Var "from"), Some (Var "to"))), RightClosed))""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestEntityWithCoord4 () =
        let result = run entityWithCoord """tplSetElem[from ~ to]"""
        let actual = sprintf "%O" result
        let actual2 = actual.Replace("\r","")
        let expected = "Failure:\nError in Ln: 1 Col: 11\ntplSetElem[from ~ to]\n          ^\nuse of FPL templates is not allowed in this context\n"
        Assert.AreEqual(expected, actual2);


[<TestClass>]
type TestPredicates () =


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

    [<TestMethod>]
    member this.TestIsOperator1 () =
        let result = run isOperator """is(x, Nat)"""
        let actual = sprintf "%O" result
        let expected = """Success: IsOperator (Var "x", VariableTypeWithModifier (None, ClassHeaderType ["Nat"]))""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestAll1 () =
        let result = run all """all x,y,z(true)"""
        let actual = sprintf "%O" result
        let expected = """Success: All ([Var "x"; Var "y"; Var "z"], True)""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestAll2 () =
        let result = run all """all x,y,z (not (iif ( true, not(false))))"""
        let actual = sprintf "%O" result
        let expected = """Success: All ([Var "x"; Var "y"; Var "z"], Not (Iif (True, Not False)))""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestAll3 () =
        let result = run all """all x,y,z (not (iif ( iif( true, false), true)))"""
        let actual = sprintf "%O" result
        let expected = """Success: All ([Var "x"; Var "y"; Var "z"], Not (Iif (Iif (True, False), True)))""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestAll4 () =
        let result = run all """all x (not(iif ( iif ( true, iif( true, false)), not(true) )))"""
        let actual = sprintf "%O" result
        let expected = """Success: All ([Var "x"], Not (Iif (Iif (True, Iif (True, False)), Not True)))""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestEx1 () =
        let result = run exists """ex x,y,z(true)"""
        let actual = sprintf "%O" result
        let expected = """Success: Exists ([Var "x"; Var "y"; Var "z"], True)""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestEx2 () =
        let result = run exists """ex x,y,z (not (iif ( true, not(false))))"""
        let actual = sprintf "%O" result
        let expected = """Success: Exists ([Var "x"; Var "y"; Var "z"], Not (Iif (True, Not False)))""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestEx3 () =
        let result = run exists """ex x,y,z (not (iif ( iif( true, false), true)))"""
        let actual = sprintf "%O" result
        let expected = """Success: Exists ([Var "x"; Var "y"; Var "z"], Not (Iif (Iif (True, False), True)))""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestEx4 () =
        let result = run exists """ex x (not(iif ( iif ( true, iif( true, false)), not(true) )))"""
        let actual = sprintf "%O" result
        let expected = """Success: Exists ([Var "x"], Not (Iif (Iif (True, Iif (True, False)), Not True)))""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestExN1 () =
        let result = run existsTimesN """ex$0 x,y,z(true)"""
        let actual = sprintf "%O" result
        let expected = """Success: ExistsN (("0", [Var "x"; Var "y"; Var "z"]), True)""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestExN2 () =
        let result = run existsTimesN """ex$1 x,y,z (not (iif ( true, not(false))))"""
        let actual = sprintf "%O" result
        let expected = """Success: ExistsN (("1", [Var "x"; Var "y"; Var "z"]), Not (Iif (True, Not False)))""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestExN3 () =
        let result = run existsTimesN """ex$2 x,y,z (not (iif ( iif( true, false), true)))"""
        let actual = sprintf "%O" result
        let expected = """Success: ExistsN
  (("2", [Var "x"; Var "y"; Var "z"]), Not (Iif (Iif (True, False), True)))""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestExN4 () =
        let result = run existsTimesN """ex$3 x (not(iif ( iif ( true, iif( true, false)), not(true) )))"""
        let actual = sprintf "%O" result
        let expected = """Success: ExistsN (("3", [Var "x"]), Not (Iif (Iif (True, Iif (True, False)), Not True)))""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

[<TestClass>]
type TestClassInheritanceTypes () =


    [<TestMethod>]
    member this.TestSpecificType3 () =
        let result = run specificClassType """object"""
        let actual = sprintf "%O" result
        let expected = """Success: ObjectType""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestSpecificType4 () =
        let result = run specificClassType """tpl"""
        let actual = sprintf "%O" result
        let expected = """Success: TemplateType "tpl" """.Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestSpecificType5 () =
        let result = run specificClassType """tplSetElem"""
        let actual = sprintf "%O" result
        let expected = """Success: TemplateType "tplSetElem" """.Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);


    member this.TestSpecificType7 () =
        let result = run specificClassType """@extNat"""
        let actual = sprintf "%O" result
        let expected = """Success: ExtensionType (Extensionname "Nat")""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestSpecificType8 () =
        let result = run specificClassType """SomeClass"""
        let actual = sprintf "%O" result
        let expected = """Success: ClassHeaderType ["SomeClass"]""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestSpecificType9 () =
        let result = run specificClassType """bla"""
        let actual = sprintf "%O" result
        let actual2 = actual.Replace("\r","")
        let expected = "Failure:\nError in Ln: 1 Col: 1\nbla\n^\nExpecting: <PascalCaseId>, @ext<PascalCaseId>, 'obj', 'object', 'template' or\n'tpl'\n"
        Assert.AreEqual(expected, actual2);

    [<TestMethod>]
    member this.TestClassType3 () =
        let result = run classType """object[self]"""
        let actual = sprintf "%O" result
        let expected = """Success: FplTypeWithCoords (ObjectType, [Self []])""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestClassType3a () =
        let result = run classType """object[SomeObject1, SomeObject2,SomeObject3]"""
        let actual = sprintf "%O" result
        let expected = """Success: FplTypeWithCoords
  (ObjectType,
   [AliasedId ["SomeObject1"]; AliasedId ["SomeObject2"];
    AliasedId ["SomeObject3"]])""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestClassType4 () =
        let result = run classType """tpl[from~]"""
        let actual = sprintf "%O" result
        let expected = """Success: FplTypeWithRange
  ((TemplateType "tpl", LeftClosed),
   (RangeInType (Some (Var "from"), None), RightClosed))""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestClassType4a () =
        let result = run classType """tpl[~ to]"""
        let actual = sprintf "%O" result
        let expected = """Success: FplTypeWithRange
  ((TemplateType "tpl", LeftClosed),
   (RangeInType (None, Some (Var "to")), RightClosed))""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestClassType4b () =
        let result = run classType """tpl[~]"""
        let actual = sprintf "%O" result
        let expected = """Success: FplTypeWithRange
  ((TemplateType "tpl", LeftClosed), (RangeInType (None, None), RightClosed))""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestClassType5 () =
        let result = run classType """Set[from ~ to]"""
        let actual = sprintf "%O" result
        let expected = """Success: FplTypeWithRange
  ((ClassHeaderType ["Set"], LeftClosed),
   (RangeInType (Some (Var "from"), Some (Var "to")), RightClosed))""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestClassType5a () =
        let result = run classType """Set[!from ~ to]"""
        let actual = sprintf "%O" result
        let expected = """Success: FplTypeWithRange
  ((ClassHeaderType ["Set"], LeftOpen),
   (RangeInType (Some (Var "from"), Some (Var "to")), RightClosed))""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestClassType5b () =
        let result = run classType """Set[from ~ to!]"""
        let actual = sprintf "%O" result
        let expected = """Success: FplTypeWithRange
  ((ClassHeaderType ["Set"], LeftClosed),
   (RangeInType (Some (Var "from"), Some (Var "to")), RightOpen))""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);


    [<TestMethod>]
    member this.TestClassType7 () =
        let result = run classType """@extNat"""
        let actual = sprintf "%O" result
        let expected = """Success: ExtensionType (Extensionname "Nat")""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestClassType8 () =
        let result = run classType """SomeClass"""
        let actual = sprintf "%O" result
        let expected = """Success: ClassHeaderType ["SomeClass"]""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestClassType9 () =
        let result = run classType """bla"""
        let actual = sprintf "%O" result
        let actual2 = actual.Replace("\r","")
        let expected = "Failure:\nError in Ln: 1 Col: 1\nbla\n^\nExpecting: <PascalCaseId>, @ext<PascalCaseId>, 'obj', 'object', 'template' or\n'tpl'\n"
        Assert.AreEqual(expected, actual2);

[<TestClass>]
type TestVariableTypes () =

    [<TestMethod>]
    member this.TestVariableType001 () =
        let result = run variableType """object"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (None, ObjectType)""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType002 () =
        let result = run variableType """obj"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (None, ObjectType)""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType003 () =
        let result = run variableType """function"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (None, FunctionalTermType)""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType004 () =
        let result = run variableType """func"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (None, FunctionalTermType)""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType005 () =
        let result = run variableType """predicate"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (None, PredicateType)""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType006 () =
        let result = run variableType """pred"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (None, PredicateType)""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType007 () =
        let result = run variableType """index"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (None, IndexType)""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType008 () =
        let result = run variableType """ind"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (None, IndexType)""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType009 () =
        let result = run variableType """SomeClass"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (None, ClassHeaderType ["SomeClass"])""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType010 () =
        let result = run variableType """@extNat"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (None, ExtensionType (Extensionname "Nat"))""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType011 () =
        let result = run variableType """template"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (None, TemplateType "template")""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType012 () =
        let result = run variableType """tpl"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (None, TemplateType "tpl")""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType013 () =
        let result = run variableType """templateTest"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (None, TemplateType "templateTest")""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType014 () =
        let result = run variableType """tplTest"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (None, TemplateType "tplTest")""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType015 () =
        let result = run variableType """+object"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (Some Many1, ObjectType)""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType016 () =
        let result = run variableType """+obj"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (Some Many1, ObjectType)""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType017 () =
        let result = run variableType """+function"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (Some Many1, FunctionalTermType)""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType018 () =
        let result = run variableType """+func"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (Some Many1, FunctionalTermType)""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType019 () =
        let result = run variableType """+predicate"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (Some Many1, PredicateType)""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType020 () =
        let result = run variableType """+pred"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (Some Many1, PredicateType)""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType021 () =
        let result = run variableType """+index"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (Some Many1, IndexType)""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType022 () =
        let result = run variableType """+ind"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (Some Many1, IndexType)""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType023 () =
        let result = run variableType """+SomeClass"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (Some Many1, ClassHeaderType ["SomeClass"])""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType024 () =
        let result = run variableType """+@extNat"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (Some Many1, ExtensionType (Extensionname "Nat"))""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType025 () =
        let result = run variableType """+template"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (Some Many1, TemplateType "template")""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType026 () =
        let result = run variableType """+tpl"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (Some Many1, TemplateType "tpl")""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType027 () =
        let result = run variableType """+templateTest"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (Some Many1, TemplateType "templateTest")""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType028 () =
        let result = run variableType """+tplTest"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (Some Many1, TemplateType "tplTest")""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType029 () =
        let result = run variableType """*object"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (Some Many, ObjectType)""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType030 () =
        let result = run variableType """*obj"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (Some Many, ObjectType)""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType031 () =
        let result = run variableType """*function"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (Some Many, FunctionalTermType)""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType032 () =
        let result = run variableType """*func"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (Some Many, FunctionalTermType)""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType033 () =
        let result = run variableType """*predicate"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (Some Many, PredicateType)""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType034 () =
        let result = run variableType """*pred"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (Some Many, PredicateType)""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType035 () =
        let result = run variableType """*index"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (Some Many, IndexType)""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType036 () =
        let result = run variableType """*ind"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (Some Many, IndexType)""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType037 () =
        let result = run variableType """*SomeClass"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (Some Many, ClassHeaderType ["SomeClass"])""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType038 () =
        let result = run variableType """*@extNat"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (Some Many, ExtensionType (Extensionname "Nat"))""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType039 () =
        let result = run variableType """*template"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (Some Many, TemplateType "template")""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType040 () =
        let result = run variableType """*tpl"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (Some Many, TemplateType "tpl")""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType041 () =
        let result = run variableType """*templateTest"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (Some Many, TemplateType "templateTest")""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType042 () =
        let result = run variableType """*tplTest"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (Some Many, TemplateType "tplTest")""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType043 () =
        let result = run variableType """*object[x,y,z]"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier
  (Some Many, FplTypeWithCoords (ObjectType, [Var "x"; Var "y"; Var "z"]))""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType044 () =
        let result = run variableType """*obj[x~y]"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier
  (Some Many,
   FplTypeWithRange
     ((ObjectType, LeftClosed),
      (RangeInType (Some (Var "x"), Some (Var "y")), RightClosed)))""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType045 () =
        let result = run variableType """*function[~]"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (Some Many, FunctionalTermType)""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType046 () =
        let result = run variableType """*func[1~]"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (Some Many, FunctionalTermType)""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType047 () =
        let result = run variableType """*predicate[~2]"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (Some Many, PredicateType)""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType048 () =
        let result = run variableType """*pred[x,y,z]"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (Some Many, PredicateType)""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType049 () =
        let result = run variableType """*index[~]"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (Some Many, IndexType)""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType050 () =
        let result = run variableType """*ind[SomeClass]"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier (Some Many, IndexType)""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType051 () =
        let result = run variableType """*SomeClass[SomeOtherClass]"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier
  (Some Many,
   FplTypeWithCoords
     (ClassHeaderType ["SomeClass"], [AliasedId ["SomeOtherClass"]]))""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType052 () =
        let result = run variableType """*@extNat[1~2]"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier
  (Some Many,
   FplTypeWithRange
     ((ExtensionType (Extensionname "Nat"), LeftClosed),
      (RangeInType (Some (ExtDigits "1"), Some (ExtDigits "2")), RightClosed)))""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType053 () =
        let result = run variableType """*template[1~n]"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier
  (Some Many,
   FplTypeWithRange
     ((TemplateType "template", LeftClosed),
      (RangeInType (Some (ExtDigits "1"), Some (Var "n")), RightClosed)))""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType054 () =
        let result = run variableType """*tpl[33]"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier
  (Some Many, FplTypeWithCoords (TemplateType "tpl", [ExtDigits "33"]))""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType055 () =
        let result = run variableType """*templateTest[0~]"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier
  (Some Many,
   FplTypeWithRange
     ((TemplateType "templateTest", LeftClosed),
      (RangeInType (Some (ExtDigits "0"), None), RightClosed)))""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestVariableType056 () =
        let result = run variableType """*tplTest[~10]"""
        let actual = sprintf "%O" result
        let expected = """Success: VariableTypeWithModifier
  (Some Many,
   FplTypeWithRange
     ((TemplateType "tplTest", LeftClosed),
      (RangeInType (None, Some (ExtDigits "10")), RightClosed)))""".Trim().Replace("\r","")
        Assert.AreEqual(expected, actual);
