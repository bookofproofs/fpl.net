namespace FplParser.Tests

open FParsec
open FplGrammar
open Microsoft.VisualStudio.TestTools.UnitTesting



[<TestClass>]
type TestIdentifiers () =
    let replaceWhiteSpace (input: string) =
        let whiteSpaceChars = [|' '; '\t'; '\n'; '\r'|]
        input.Split(whiteSpaceChars)
            |> String.concat ""

    [<TestMethod>]
    member this.TestTheoryNamespace () =
        let result = run theoryNamespace "Fpl.Test alias MyAlias"
        let actual = sprintf "%O" result
        let expected = """Success: AliasedNamespaceIdentifier (["Fpl"; "Test"], "MyAlias")"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestTheoryNamespace2 () =
        let result = run theoryNamespace "Fpl.Test"
        let actual = sprintf "%O" result
        let expected = """Success: NamespaceIdentifier ["Fpl"; "Test"]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestUsesClause () =
        let result = run usesClause "uses { Fpl.Test alias MyAlias ,Fpl.Test , Fpl.Test.Test1 }"
        let actual = sprintf "%O" result
        let expected = """Success: UsesClause
  [AliasedNamespaceIdentifier (["Fpl"; "Test"], "MyAlias");
   NamespaceIdentifier ["Fpl"; "Test"];
   NamespaceIdentifier ["Fpl"; "Test"; "Test1"]]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestPredicateIdentifier1 () =
        let result = run predicateIdentifier "ThisIsMyIdentifier"
        let actual = sprintf "%O" result
        let expected = """Success: AliasedId ["ThisIsMyIdentifier"]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestPredicateIdentifier2 () =
        let result = run predicateIdentifier "This.Is.My.Identifier"
        let actual = sprintf "%O" result
        let expected = """Success: AliasedId ["This"; "Is"; "My"; "Identifier"]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestIndexVariable1 () =
        let result = run indexVariable "x$123"
        let actual = sprintf "%O" result
        let expected = """Success: IndexVariable ("x", "123")"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestIndexVariable2 () =
        let result = run indexVariable "x$y"
        let actual = sprintf "%O" result
        let expected = """Success: IndexVariable ("x", "y")"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestAtList0 () =
        let result = run atList ""
        let actual = sprintf "%O" result
        let expected = """Success: []"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestAtList1 () =
        let result = run atList "@"
        let actual = sprintf "%O" result
        let expected = """Success: ['@']"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestAtList2 () =
        let result = run atList "@@"
        let actual = sprintf "%O" result
        let expected = """Success: ['@'; '@']"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestSelf0 () =
        let result = run self "self"
        let actual = sprintf "%O" result
        let expected = """Success: Self []"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestSelf1 () =
        let result = run self "@self"
        let actual = sprintf "%O" result
        let expected = """Success: Self ['@']"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestSelf2 () =
        let result = run self "@@self"
        let actual = sprintf "%O" result
        let expected = """Success: Self ['@'; '@']"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestEntitySelf () =
        let result = run entity "self"
        let actual = sprintf "%O" result
        let expected = """Success: Self []"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestEntityAtSelf () =
        let result = run entity "@self"
        let actual = sprintf "%O" result
        let expected = """<Success:Self(((Ln:1,Col:1),(Ln:1,Col:6)),['@'])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestEntityVariable () =
        let result = run entity "xyz"
        let actual = sprintf "%O" result
        let expected = """Success: Var "xyz" """.Trim()
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestExtensionName () =
        let result = run extensionName "extDigits"
        let actual = sprintf "%O" result
        let expected = """Success: Extensionname "Digits" """.Trim()
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestExtensionBlock1 () =
        let result = run extensionBlock """:ext
        extDigits : /\d+/
    :end """
        let actual = sprintf "%O" result
        let expected = "Success: ExtensionBlock (Extensionname \"Digits\", ExtensionRegex \"/\d+/\n\")"
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestBoundLeftClosed () =
        let result = run leftBound """["""
        let actual = sprintf "%O" result
        let expected = """Success: LeftClosed"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestBoundLeftClosed1 () =
        let result = run leftBound """[ """
        let actual = sprintf "%O" result
        let expected = """Success: LeftClosed"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestBoundLeftOpen () =
        let result = run leftBound """[!"""
        let actual = sprintf "%O" result
        let expected = """Success: LeftOpen"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestBoundLeftOpen1 () =
        let result = run leftBound """[ !"""
        let actual = sprintf "%O" result
        let expected = """Success: LeftOpen"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestBoundRightOpen () =
        let result = run rightBound """!]"""
        let actual = sprintf "%O" result
        let expected = """Success: RightOpen"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestBoundRightOpen1 () =
        let result = run rightBound """! ]"""
        let actual = sprintf "%O" result
        let expected = """Success: RightOpen"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestBoundRightClosed () =
        let result = run rightBound """]"""
        let actual = sprintf "%O" result
        let expected = """Success: RightClosed"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestBoundRightClosed1 () =
        let result = run rightBound """ ]"""
        let actual = sprintf "%O" result
        let expected = """Success: RightClosed"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestEntityWithCoord1 () =
        let result = run predicateWithQualification """myField[1 ~ n]"""
        let actual = sprintf "%O" result
        let expected = """Success: EntityWithCoord
  (Var "myField",
   ClosedOrOpenRange
     ((LeftClosed, (Some (ExtDigits "1"), Some (Var "n"))), RightClosed))"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestEntityWithCoord2 () =
        let result = run predicateWithQualification """theorem[from ~ to]"""
        let actual = sprintf "%O" result
        let actual2 = actual
        let expected = "Failure:
Error in Ln: 1 Col: 8
theorem[from ~ to]
       ^
Cannot use keyword 'theorem' as a variable"
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestEntityWithCoord3 () =
        let result = run predicateWithQualification """self[from ~ to]"""
        let actual = sprintf "%O" result
        let expected = """Success: EntityWithCoord
  (Self [],
   ClosedOrOpenRange
     ((LeftClosed, (Some (Var "from"), Some (Var "to"))), RightClosed))"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestEntityWithCoord4 () =
        let result = run predicateWithQualification """tplSetElem[from ~ to]"""
        let actual = sprintf "%O" result
        let expected = "Failure:
    Error in Ln: 1 Col: 11
    tplSetElem[from ~ to]
              ^
    Cannot use template 'tplSetElem' as a variable"
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestEntityWithCoord5 () =
        let result = run predicateWithQualification """tpls[from ~ to]"""
        let actual = sprintf "%O" result
        let expected = """Success: EntityWithCoord
  (Var "tpls",
   ClosedOrOpenRange
     ((LeftClosed, (Some (Var "from"), Some (Var "to"))), RightClosed))"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);
