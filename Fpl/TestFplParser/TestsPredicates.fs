namespace TestFplParser

open FParsec
open FplGrammar
open Microsoft.VisualStudio.TestTools.UnitTesting
open Newtonsoft.Json


[<TestClass>]
type TestPredicates () =


    [<TestMethod>]
    member this.TestPredicate01 () =
        let result = run predicate """true"""
        let actual = sprintf "%O" result
        let expected = """Success: True""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestPredicate02 () =
        let result = run predicate """false"""
        let actual = sprintf "%O" result
        let expected = """Success: False""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestPredicate03 () =
        let result = run predicate """undef"""
        let actual = sprintf "%O" result
        let expected = """Success: Undefined""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestPredicate04 () =
        let result = run predicate """undefined"""
        let actual = sprintf "%O" result
        let expected = """Success: Undefined""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestPredicate05 () =
        let result = run predicate """and(true,false)"""
        let actual = sprintf "%O" result
        let expected = """Success: And [True; False]""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestPredicate06 () =
        let result = run predicate """and ( true, true )"""
        let actual = sprintf "%O" result
        let expected = """Success: And [True; True]""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestPredicate07 () =
        let result = run predicate """and ( true, and( true, false))"""
        let actual = sprintf "%O" result
        let expected = """Success: And [True; And [True; False]]""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestPredicate08 () =
        let result = run predicate """and ( and ( true, and( true, false)), true )"""
        let actual = sprintf "%O" result
        let expected = """Success: And [And [True; And [True; False]]; True]""".Replace("\r","")
        Assert.AreEqual(expected, actual);
        
    [<TestMethod>]
    member this.TestPredicate09 () =
        let result = run predicate """or(true,false)"""
        let actual = sprintf "%O" result
        let expected = """Success: Or [True; False]""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestPredicate10 () =
        let result = run predicate """or ( true, true )"""
        let actual = sprintf "%O" result
        let expected = """Success: Or [True; True]""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestPredicate11 () =
        let result = run predicate """or ( true, or( true, false))"""
        let actual = sprintf "%O" result
        let expected = """Success: Or [True; Or [True; False]]""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestPredicate12 () =
        let result = run predicate """or ( or ( true, or( true, false)), true )"""
        let actual = sprintf "%O" result
        let expected = """Success: Or [Or [True; Or [True; False]]; True]""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestPredicate13 () =
        let result = run predicate """or(1.,2.)"""
        let actual = sprintf "%O" result
        let expected = """Success: Or [ArgumentIdentifier "1."; ArgumentIdentifier "2."]""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestPredicate14 () =
        let result = run predicate """impl(true,false)"""
        let actual = sprintf "%O" result
        let expected = """Success: Impl (True, False)""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestPredicate15 () =
        let result = run predicate """impl ( true, true )"""
        let actual = sprintf "%O" result
        let expected = """Success: Impl (True, True)""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestPredicate16 () =
        let result = run predicate """impl ( true, impl( true, false))"""
        let actual = sprintf "%O" result
        let expected = """Success: Impl (True, Impl (True, False))""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestPredicate17 () =
        let result = run predicate """impl ( impl ( true, impl( true, false)), true )"""
        let actual = sprintf "%O" result
        let expected = """Success: Impl (Impl (True, Impl (True, False)), True)""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestPredicate18 () =
        let result = run predicate """iif(true,false)"""
        let actual = sprintf "%O" result
        let expected = """Success: Iif (True, False)""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestPredicate19 () =
        let result = run predicate """iif ( true, true )"""
        let actual = sprintf "%O" result
        let expected = """Success: Iif (True, True)""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestPredicate20 () =
        let result = run predicate """iif ( true, iif( true, false))"""
        let actual = sprintf "%O" result
        let expected = """Success: Iif (True, Iif (True, False))""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestPredicate21 () =
        let result = run predicate """iif ( iif ( true, iif( true, false)), true )"""
        let actual = sprintf "%O" result
        let expected = """Success: Iif (Iif (True, Iif (True, False)), True)""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestPredicate22 () =
        let result = run predicate """xor(true,false)"""
        let actual = sprintf "%O" result
        let expected = """Success: Xor (True, False)""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestPredicate23 () =
        let result = run predicate """xor ( true, true )"""
        let actual = sprintf "%O" result
        let expected = """Success: Xor (True, True)""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestPredicate23a () =
        let result = run predicate """xor ( true, xor( true, false))"""
        let actual = sprintf "%O" result
        let expected = """Success: Xor (True, Xor (True, False))""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestPredicate24 () =
        let result = run predicate """xor ( xor ( true, xor( true, false)), true )"""
        let actual = sprintf "%O" result
        let expected = """Success: Xor (Xor (True, Xor (True, False)), True)""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestPredicate25 () =
        let result = run predicate """Zero()"""
        let actual = sprintf "%O" result
        let expected = """Success: PredicateWithArgs (AliasedId ["Zero"], [])""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestPredicate26 () =
        let result = run predicate """self(i)"""
        let actual = sprintf "%O" result
        let expected = """Success: PredicateWithArgs (Self [], [PredicateWithoutArgs (Var "i")])""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestPredicate27 () =
        let result = run predicate """ProceedingResults(1.,2.)"""
        let actual = sprintf "%O" result
        let expected = """Success: PredicateWithArgs
  (AliasedId ["ProceedingResults"],
   [ArgumentIdentifier "1."; ArgumentIdentifier "2."])""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestPredicate28 () =
        let result = run predicate """myOp.NeutralElement()"""
        let actual = sprintf "%O" result
        let expected = """Success: QualifiedIdentifier
  (Var "myOp", [PredicateWithArgs (AliasedId ["NeutralElement"], [])])""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestPredicate29 () =
        let result = run predicate """myOp.NeutralElement().SomeProperty()"""
        let actual = sprintf "%O" result
        let expected = """Success: QualifiedIdentifier
  (Var "myOp",
   [PredicateWithArgs (AliasedId ["NeutralElement"], []);
    PredicateWithArgs (AliasedId ["SomeProperty"], [])])""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestPredicate30 () =
        let result = run predicate """not(true)"""
        let actual = sprintf "%O" result
        let expected = """Success: Not True""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestPredicate31 () =
        let result = run predicate """not (iif ( true, not(false)))"""
        let actual = sprintf "%O" result
        let expected = """Success: Not (Iif (True, Not False))""".Replace("\r","")
        Assert.AreEqual(expected, actual);


    [<TestMethod>]
    member this.TestPredicate32 () =
        let result = run predicate """not (iif ( iif( true, false), true))"""
        let actual = sprintf "%O" result
        let expected = """Success: Not (Iif (Iif (True, False), True))""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestPredicate33 () =
        let result = run predicate """not(iif ( iif ( true, iif( true, false)), not(true) ))"""
        let actual = sprintf "%O" result
        let expected = """Success: Not (Iif (Iif (True, Iif (True, False)), Not True))""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestPredicate34 () =
        let result = run predicate """is(x, Nat)"""
        let actual = sprintf "%O" result
        let expected = """Success: IsOperator (Var "x", VariableTypeWithModifier (None, ClassHeaderType ["Nat"]))""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestPredicate35 () =
        let result = run predicate """all x,y,z(true)"""
        let actual = sprintf "%O" result
        let expected = """Success: All ([Var "x"; Var "y"; Var "z"], True)""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestPredicate36 () =
        let result = run predicate """all x,y,z (not (iif ( true, not(false))))"""
        let actual = sprintf "%O" result
        let expected = """Success: All ([Var "x"; Var "y"; Var "z"], Not (Iif (True, Not False)))""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestPredicate37 () =
        let result = run predicate """all x,y,z (not (iif ( iif( true, false), true)))"""
        let actual = sprintf "%O" result
        let expected = """Success: All ([Var "x"; Var "y"; Var "z"], Not (Iif (Iif (True, False), True)))""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestPredicate38 () =
        let result = run predicate """all x (not(iif ( iif ( true, iif( true, false)), not(true) )))"""
        let actual = sprintf "%O" result
        let expected = """Success: All ([Var "x"], Not (Iif (Iif (True, Iif (True, False)), Not True)))""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestPredicate39 () =
        let result = run predicate """ex x,y,z(true)"""
        let actual = sprintf "%O" result
        let expected = """Success: Exists ([Var "x"; Var "y"; Var "z"], True)""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestPredicate40 () =
        let result = run predicate """ex x,y,z (not (iif ( true, not(false))))"""
        let actual = sprintf "%O" result
        let expected = """Success: Exists ([Var "x"; Var "y"; Var "z"], Not (Iif (True, Not False)))""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestPredicate41 () =
        let result = run predicate """ex x,y,z (not (iif ( iif( true, false), true)))"""
        let actual = sprintf "%O" result
        let expected = """Success: Exists ([Var "x"; Var "y"; Var "z"], Not (Iif (Iif (True, False), True)))""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestPredicate42 () =
        let result = run predicate """ex x (not(iif ( iif ( true, iif( true, false)), not(true) )))"""
        let actual = sprintf "%O" result
        let expected = """Success: Exists ([Var "x"], Not (Iif (Iif (True, Iif (True, False)), Not True)))""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestPredicate43 () =
        let result = run predicate """ex$0 x,y,z(true)"""
        let actual = sprintf "%O" result
        let expected = """Success: ExistsN (("0", [Var "x"; Var "y"; Var "z"]), True)""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestPredicate44 () =
        let result = run predicate """ex$1 x,y,z (not (iif ( true, not(false))))"""
        let actual = sprintf "%O" result
        let expected = """Success: ExistsN (("1", [Var "x"; Var "y"; Var "z"]), Not (Iif (True, Not False)))""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestPredicate45 () =
        let result = run predicate """ex$2 x,y,z (not (iif ( iif( true, false), true)))"""
        let actual = sprintf "%O" result
        let expected = """Success: ExistsN
  (("2", [Var "x"; Var "y"; Var "z"]), Not (Iif (Iif (True, False), True)))""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestPredicate46 () =
        let result = run predicate """ex$3 x (not(iif ( iif ( true, iif( true, false)), not(true) )))"""
        let actual = sprintf "%O" result
        let expected = """Success: ExistsN (("3", [Var "x"]), Not (Iif (Iif (True, Iif (True, False)), Not True)))""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestPredicate47 () =
        let result = run predicate """or(1.,2.)"""
        let actual = sprintf "%O" result
        let expected = """Success: Or [ArgumentIdentifier "1."; ArgumentIdentifier "2."]""".Replace("\r","")
        Assert.AreEqual(expected, actual);

