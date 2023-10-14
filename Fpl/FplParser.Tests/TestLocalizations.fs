namespace FplParser.Tests

open FParsec
open FplParser
open Microsoft.VisualStudio.TestTools.UnitTesting


[<TestClass>]
type TestLocalizations () =
    let replaceWhiteSpace (input: string) =
        let whiteSpaceChars = [|' '; '\t'; '\n'; '\r'|]
        input.Split(whiteSpaceChars)
            |> String.concat ""

    [<TestMethod>]
    member this.TestEbnfTerm01 () =
        let result = run ebnfTerm """x "\Leftrightarrow" y """
        let actual = sprintf "%O" result
        let expected = """Success: LocalizationTerm [Var "x"; LocalizationString ""\Leftrightarrow""; Var "y"]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestEbnfTerm02 () =
        let result = run ebnfTerm """"\neg(" x ")" """
        let actual = sprintf "%O" result
        let expected = """Success: LocalizationTerm
  [LocalizationString ""\neg(""; Var "x"; LocalizationString "")""]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestEbnfTransl01 () =
        let result = run ebnfTransl """x "\Leftrightarrow" y """
        let actual = sprintf "%O" result
        let expected = """Success: LocalizationTermList
  [LocalizationTerm [Var "x"; LocalizationString ""\Leftrightarrow""; Var "y"]]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestEbnfTransl02 () =
        let result = run ebnfTransl """"\neg(" x ")" """
        let actual = sprintf "%O" result
        let expected = """Success: LocalizationTermList
  [LocalizationTerm
     [LocalizationString ""\neg(""; Var "x"; LocalizationString "")""]]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);
        
    [<TestMethod>]
    member this.TestEbnfTransl03 () =
        let result = run ebnfTransl """x "\Leftrightarrow" y | x "\Rightarrow" y """
        let actual = sprintf "%O" result
        let expected = """Success: LocalizationTermList
  [LocalizationTerm [Var "x"; LocalizationString ""\Leftrightarrow""; Var "y"];
   LocalizationTerm [Var "x"; LocalizationString ""\Rightarrow""; Var "y"]]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestEbnfTransl04 () =
        let result = run ebnfTransl """"\neg(" x ")" | x "\Rightarrow" y """
        let actual = sprintf "%O" result
        let expected = """Success: LocalizationTermList
  [LocalizationTerm
     [LocalizationString ""\neg(""; Var "x"; LocalizationString "")""];
   LocalizationTerm [Var "x"; LocalizationString ""\Rightarrow""; Var "y"]]"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestTranslation01 () =
        let result = run translation """~tex: x "\Leftrightarrow" y """
        let actual = sprintf "%O" result
        let expected = """Success: ("tex",
 LocalizationTermList
   [LocalizationTerm [Var "x"; LocalizationString ""\Leftrightarrow""; Var "y"]])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestTranslation02 () =
        let result = run translation """~tex: x "\Leftrightarrow" y | x "\Rightarrow" y """
        let actual = sprintf "%O" result
        let expected = """Success: ("tex",
 LocalizationTermList
   [LocalizationTerm [Var "x"; LocalizationString ""\Leftrightarrow""; Var "y"];
    LocalizationTerm [Var "x"; LocalizationString ""\Rightarrow""; Var "y"]])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestLocalization01 () =
        let result = run localization """not(x) :=
            ~tex: "\neg(" x ")"
            ~eng: "not " x
            ~ger: "nicht " x
            ; """
        let actual = sprintf "%O" result
        let expected = """Success: (Not (PredicateWithoutArgs (Var "x")),
 [("tex",
   LocalizationTermList
     [LocalizationTerm
        [LocalizationString ""\neg(""; Var "x"; LocalizationString "")""]]);
  ("eng",
   LocalizationTermList
     [LocalizationTerm [LocalizationString ""not ""; Var "x"]]);
  ("ger",
   LocalizationTermList
     [LocalizationTerm [LocalizationString ""nicht ""; Var "x"]])])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestLocalization02 () =
        let result = run localization """iif(x,y) :=
            // comment
            ~tex: x "\Leftrightarrow" y
            // comment
            ~eng: x " if and only if " y
            // comment
            ~ger: x " dann und nur dann wenn " y
            ;"""
        let actual = sprintf "%O" result
        let expected = """Success: (Iif (PredicateWithoutArgs (Var "x"), PredicateWithoutArgs (Var "y")),
 [("tex",
   LocalizationTermList
     [LocalizationTerm
        [Var "x"; LocalizationString ""\Leftrightarrow""; Var "y"]]);
  ("eng",
   LocalizationTermList
     [LocalizationTerm
        [Var "x"; LocalizationString "" if and only if ""; Var "y"]]);
  ("ger",
   LocalizationTermList
     [LocalizationTerm
        [Var "x"; LocalizationString "" dann und nur dann wenn ""; Var "y"]])])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestLocalization03 () =
        let result = run localization """NotEqual(x,y) :=
            ~tex: x "\neq" y
            ~eng: x "is unequal" y
            ~ger: x "ist ungleich" y
            ~pol: x ( "nie równa się" | "nie równe" ) y
            ;"""
        let actual = sprintf "%O" result
        let expected = """Success: (PredicateWithArgs
   (AliasedId ["NotEqual"],
    [PredicateWithoutArgs (Var "x"); PredicateWithoutArgs (Var "y")]),
 [("tex",
   LocalizationTermList
     [LocalizationTerm [Var "x"; LocalizationString ""\neq""; Var "y"]]);
  ("eng",
   LocalizationTermList
     [LocalizationTerm [Var "x"; LocalizationString ""is unequal""; Var "y"]]);
  ("ger",
   LocalizationTermList
     [LocalizationTerm [Var "x"; LocalizationString ""ist ungleich""; Var "y"]]);
  ("pol",
   LocalizationTermList
     [LocalizationTerm
        [Var "x";
         LocalizationTermList
           [LocalizationTerm [LocalizationString ""nie równa się""];
            LocalizationTerm [LocalizationString ""nie równe""]]; Var "y"]])])"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);
