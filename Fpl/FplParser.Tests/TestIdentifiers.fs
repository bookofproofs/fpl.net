namespace FplParser.Tests

open FParsec
open FplGrammar
open Microsoft.VisualStudio.TestTools.UnitTesting



[<TestClass>]
type TestIdentifiers () =

    [<TestMethod>]
    member this.TestTheoryNamespace () =
        let result = run theoryNamespace "Fpl.Test alias MyAlias"
        let actual = sprintf "%O" result
        let expected = """Success: AliasedNamespaceIdentifier (["Fpl"; "Test"], "MyAlias")"""
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestTheoryNamespace2 () =
        let result = run theoryNamespace "Fpl.Test"
        let actual = sprintf "%O" result
        let expected = """Success: NamespaceIdentifier ["Fpl"; "Test"]"""
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestUsesClause () =
        let result = run usesClause "uses Fpl.Test alias MyAlias ,Fpl.Test , Fpl.Test.Test1"
        let actual = sprintf "%O" result
        let expected = """Success: UsesClause
  [AliasedNamespaceIdentifier (["Fpl"; "Test"], "MyAlias");
   NamespaceIdentifier ["Fpl"; "Test"];
   NamespaceIdentifier ["Fpl"; "Test"; "Test1"]]""".Replace("\r","")
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
    member this.TestExtensionBlock1 () =
        let result = run extensionBlock """:ext
        extDigits : /\d+/
    :end """
        let actual = sprintf "%O" result
        let expected = "Success: ExtensionBlock (Extensionname \"Digits\", ExtensionRegex \"/\d+/\n\")".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestBoundLeftClosed () =
        let result = run leftBound """["""
        let actual = sprintf "%O" result
        let expected = """Success: LeftClosed""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestBoundLeftClosed1 () =
        let result = run leftBound """[ """
        let actual = sprintf "%O" result
        let expected = """Success: LeftClosed""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestBoundLeftOpen () =
        let result = run leftBound """[!"""
        let actual = sprintf "%O" result
        let expected = """Success: LeftOpen""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestBoundLeftOpen1 () =
        let result = run leftBound """[ !"""
        let actual = sprintf "%O" result
        let expected = """Success: LeftOpen""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestBoundRightOpen () =
        let result = run rightBound """!]"""
        let actual = sprintf "%O" result
        let expected = """Success: RightOpen""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestBoundRightOpen1 () =
        let result = run rightBound """! ]"""
        let actual = sprintf "%O" result
        let expected = """Success: RightOpen""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestBoundRightClosed () =
        let result = run rightBound """]"""
        let actual = sprintf "%O" result
        let expected = """Success: RightClosed""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestBoundRightClosed1 () =
        let result = run rightBound """ ]"""
        let actual = sprintf "%O" result
        let expected = """Success: RightClosed""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestEntityWithCoord1 () =
        let result = run entityWithCoord """myField[1 ~ n]"""
        let actual = sprintf "%O" result
        let expected = """Success: EntityWithCoord
  (Var "myField",
   ClosedOrOpenRange
     ((LeftClosed, (Some (ExtDigits "1"), Some (Var "n"))), RightClosed))""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestEntityWithCoord2 () =
        let result = run entityWithCoord """theorem[from ~ to]"""
        let actual = sprintf "%O" result
        let actual2 = actual.Replace("\r","")
        let expected = "Failure:\nError in Ln: 1 Col: 8\ntheorem[from ~ to]\n       ^\nCannot use keyword 'theorem' as a variable\n"
        Assert.AreEqual(expected, actual2);

    [<TestMethod>]
    member this.TestEntityWithCoord3 () =
        let result = run entityWithCoord """self[from ~ to]"""
        let actual = sprintf "%O" result
        let expected = """Success: EntityWithCoord
  (Self [],
   ClosedOrOpenRange
     ((LeftClosed, (Some (Var "from"), Some (Var "to"))), RightClosed))""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestEntityWithCoord4 () =
        let result = run entityWithCoord """tplSetElem[from ~ to]"""
        let actual = sprintf "%O" result
        let actual2 = actual.Replace("\r","")
        let expected = "Failure:\nError in Ln: 1 Col: 11\ntplSetElem[from ~ to]\n          ^\nCannot use template 'tplSetElem' as a variable\n"
        Assert.AreEqual(expected, actual2);

    [<TestMethod>]
    member this.TestEntityWithCoord5 () =
        let result = run entityWithCoord """tpls[from ~ to]"""
        let actual = sprintf "%O" result
        let actual2 = actual.Replace("\r","")
        let expected = """Success: EntityWithCoord
  (Var "tpls",
   ClosedOrOpenRange
     ((LeftClosed, (Some (Var "from"), Some (Var "to"))), RightClosed))""".Replace("\r","")
        Assert.AreEqual(expected, actual2);
