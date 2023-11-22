namespace FplParser.Tests

open FParsec
open FplParser
open Microsoft.VisualStudio.TestTools.UnitTesting


[<TestClass>]
type TestPascalCaseIdDotted () =
    let replaceWhiteSpace (input: string) =
        let whiteSpaceChars = [|' '; '\t'; '\n'; '\r'|]
        input.Split(whiteSpaceChars)
            |> String.concat ""

    [<TestMethod>]
    member this.TestPascalCaseId () =
        let expected = """Success: PredicateWithQualification
      (((Ln: 1, Col: 1), (Ln: 1, Col: 3)),
       [(PredicateIdentifier
           (((Ln: 1, Col: 1), (Ln: 1, Col: 3)), [PascalCaseId "Xx"]), None)])"""
        let result = run predicateWithQualification """Xx"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual)

    [<TestMethod>]
    member this.TestPascalCaseId1 () =
        let expected = """Success: PredicateWithQualification
      (((Ln: 1, Col: 1), (Ln: 1, Col: 6)),
       [(PredicateIdentifier
           (((Ln: 1, Col: 1), (Ln: 1, Col: 6)),
            [PascalCaseId "Xx"; PascalCaseId "Xx"]), None)])"""
        let result = run predicateWithQualification """Xx.Xx"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual)

    [<TestMethod>]
    member this.TestPascalCaseId2 () =
        let expected = """Success: PredicateWithQualification
  (((Ln: 1, Col: 1), (Ln: 1, Col: 9)),
   [(PredicateIdentifier
       (((Ln: 1, Col: 1), (Ln: 1, Col: 9)),
        [PascalCaseId "Xx"; PascalCaseId "Xx"; PascalCaseId "Xx"]), None)])"""
        let result = run predicateWithQualification """Xx.Xx.Xx"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual)


    [<TestMethod>]
    member this.TestPascalCaseIdA () =
        let expected = """Success: PredicateWithQualification
  (((Ln: 1, Col: 1), (Ln: 1, Col: 5)),
   [(PredicateIdentifier
       (((Ln: 1, Col: 1), (Ln: 1, Col: 3)), [PascalCaseId "Xx"]),
     Some (ArgumentTuple (((Ln: 1, Col: 3), (Ln: 1, Col: 5)), [])))])"""
        let result = run predicateWithQualification """Xx()"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual)

    [<TestMethod>]
    member this.TestPascalCaseId1A () =
        let expected = """Success: PredicateWithQualification
  (((Ln: 1, Col: 1), (Ln: 1, Col: 8)),
   [(PredicateIdentifier
       (((Ln: 1, Col: 1), (Ln: 1, Col: 6)),
        [PascalCaseId "Xx"; PascalCaseId "Xx"]),
     Some (ArgumentTuple (((Ln: 1, Col: 6), (Ln: 1, Col: 8)), [])))])"""
        let result = run predicateWithQualification """Xx.Xx()"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual)

    [<TestMethod>]
    member this.TestPascalCaseId2A () =
        let expected = """Success: PredicateWithQualification
  (((Ln: 1, Col: 1), (Ln: 1, Col: 11)),
   [(PredicateIdentifier
       (((Ln: 1, Col: 1), (Ln: 1, Col: 9)),
        [PascalCaseId "Xx"; PascalCaseId "Xx"; PascalCaseId "Xx"]),
     Some (ArgumentTuple (((Ln: 1, Col: 9), (Ln: 1, Col: 11)), [])))])"""
        let result = run predicateWithQualification """Xx.Xx.Xx()"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual)

    [<TestMethod>]
    member this.TestPascalCaseIdB () =
        let expected = """Success: PredicateWithQualification
      (((Ln: 1, Col: 1), (Ln: 1, Col: 8)),
       [(PredicateIdentifier
           (((Ln: 1, Col: 1), (Ln: 1, Col: 3)), [PascalCaseId "Xx"]),
         Some (ArgumentTuple (((Ln: 1, Col: 3), (Ln: 1, Col: 5)), [])));
        (PredicateIdentifier
           (((Ln: 1, Col: 6), (Ln: 1, Col: 8)), [PascalCaseId "Yy"]), None)])"""
        let result = run predicateWithQualification """Xx().Yy"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual)

    [<TestMethod>]
    member this.TestPascalCaseId1B () =
        let expected = """Success: PredicateWithQualification
      (((Ln: 1, Col: 1), (Ln: 1, Col: 11)),
       [(PredicateIdentifier
           (((Ln: 1, Col: 1), (Ln: 1, Col: 6)),
            [PascalCaseId "Xx"; PascalCaseId "Xx"]),
         Some (ArgumentTuple (((Ln: 1, Col: 6), (Ln: 1, Col: 8)), [])));
        (PredicateIdentifier
           (((Ln: 1, Col: 9), (Ln: 1, Col: 11)), [PascalCaseId "Yy"]), None)])"""
        let result = run predicateWithQualification """Xx.Xx().Yy"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual)

    [<TestMethod>]
    member this.TestPascalCaseId2B () =
        let expected = """Success: PredicateWithQualification
  (((Ln: 1, Col: 1), (Ln: 1, Col: 14)),
   [(PredicateIdentifier
       (((Ln: 1, Col: 1), (Ln: 1, Col: 9)),
        [PascalCaseId "Xx"; PascalCaseId "Xx"; PascalCaseId "Xx"]),
     Some (ArgumentTuple (((Ln: 1, Col: 9), (Ln: 1, Col: 11)), [])));
    (PredicateIdentifier
       (((Ln: 1, Col: 12), (Ln: 1, Col: 14)), [PascalCaseId "Yy"]), None)])"""
        let result = run predicateWithQualification """Xx.Xx.Xx().Yy"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual)

    [<TestMethod>]
    member this.TestPascalCaseIdC () =
        let expected = """Success: PredicateWithQualification
  (((Ln: 1, Col: 1), (Ln: 1, Col: 11)),
   [(PredicateIdentifier
       (((Ln: 1, Col: 1), (Ln: 1, Col: 3)), [PascalCaseId "Xx"]),
     Some (ArgumentTuple (((Ln: 1, Col: 3), (Ln: 1, Col: 5)), [])));
    (PredicateIdentifier
       (((Ln: 1, Col: 6), (Ln: 1, Col: 11)),
        [PascalCaseId "Yy"; PascalCaseId "Zz"]), None)])"""
        let result = run predicateWithQualification """Xx().Yy.Zz"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual)

    [<TestMethod>]
    member this.TestPascalCaseId1C () =
        let expected = """Success: PredicateWithQualification
      (((Ln: 1, Col: 1), (Ln: 1, Col: 14)),
       [(PredicateIdentifier
           (((Ln: 1, Col: 1), (Ln: 1, Col: 6)),
            [PascalCaseId "Xx"; PascalCaseId "Xx"]),
         Some (ArgumentTuple (((Ln: 1, Col: 6), (Ln: 1, Col: 8)), [])));
        (PredicateIdentifier
           (((Ln: 1, Col: 9), (Ln: 1, Col: 14)),
            [PascalCaseId "Yy"; PascalCaseId "Zz"]), None)])"""
        let result = run predicateWithQualification """Xx.Xx().Yy.Zz"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual)

    [<TestMethod>]
    member this.TestPascalCaseId2C () =
        let expected = """Success: PredicateWithQualification
  (((Ln: 1, Col: 1), (Ln: 1, Col: 17)),
   [(PredicateIdentifier
       (((Ln: 1, Col: 1), (Ln: 1, Col: 9)),
        [PascalCaseId "Xx"; PascalCaseId "Xx"; PascalCaseId "Xx"]),
     Some (ArgumentTuple (((Ln: 1, Col: 9), (Ln: 1, Col: 11)), [])));
    (PredicateIdentifier
       (((Ln: 1, Col: 12), (Ln: 1, Col: 17)),
        [PascalCaseId "Yy"; PascalCaseId "Zz"]), None)])"""
        let result = run predicateWithQualification """Xx.Xx.Xx().Yy.Zz"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual)

