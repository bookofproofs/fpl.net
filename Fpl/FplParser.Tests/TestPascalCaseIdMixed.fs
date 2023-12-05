namespace FplParser.Tests

open FParsec
open FplParser
open Microsoft.VisualStudio.TestTools.UnitTesting


[<TestClass>]
type TestPascalCaseIdMixed () =
    let replaceWhiteSpace (input: string) =
        let whiteSpaceChars = [|' '; '\t'; '\n'; '\r'|]
        input.Split(whiteSpaceChars)
            |> String.concat ""


    [<TestMethod>]
    member this.TestPascalCaseId2 () =
        let expected = """Success: PredicateWithQualification
  (PredicateWithOptSpecification
     (((Ln: 1, Col: 1), (Ln: 1, Col: 10)),
      (PredicateIdentifier
         (((Ln: 1, Col: 1), (Ln: 1, Col: 3)), [PascalCaseId "Xx"]),
       Some
         (BrackedCoordList
            (((Ln: 1, Col: 3), (Ln: 1, Col: 10)),
             [PredicateWithQualification
                (PredicateWithOptSpecification
                   (((Ln: 1, Col: 4), (Ln: 1, Col: 9)),
                    (PredicateIdentifier
                       (((Ln: 1, Col: 4), (Ln: 1, Col: 9)),
                        [PascalCaseId "Xx"; PascalCaseId "Xx"]), None)),
                 QualificationList (((Ln: 1, Col: 9), (Ln: 1, Col: 9)), []))])))),
   QualificationList (((Ln: 1, Col: 10), (Ln: 1, Col: 10)), []))"""
        let result = run (predicateWithQualification .>> eof) """Xx[Xx.Xx]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual)

    [<TestMethod>]
    member this.TestPascalCaseId2A () =
        let expected = """Success: PredicateWithQualification
  (PredicateWithOptSpecification
     (((Ln: 1, Col: 1), (Ln: 1, Col: 12)),
      (PredicateIdentifier
         (((Ln: 1, Col: 1), (Ln: 1, Col: 3)), [PascalCaseId "Xx"]),
       Some
         (BrackedCoordList
            (((Ln: 1, Col: 3), (Ln: 1, Col: 12)),
             [PredicateWithQualification
                (PredicateWithOptSpecification
                   (((Ln: 1, Col: 4), (Ln: 1, Col: 11)),
                    (PredicateIdentifier
                       (((Ln: 1, Col: 4), (Ln: 1, Col: 9)),
                        [PascalCaseId "Xx"; PascalCaseId "Xx"]),
                     Some
                       (ArgumentTuple (((Ln: 1, Col: 9), (Ln: 1, Col: 11)), [])))),
                 QualificationList (((Ln: 1, Col: 11), (Ln: 1, Col: 11)), []))])))),
   QualificationList (((Ln: 1, Col: 12), (Ln: 1, Col: 12)), []))"""
        let result = run (predicateWithQualification .>> eof) """Xx[Xx.Xx()]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual)

    [<TestMethod>]
    member this.TestPascalCaseId1B () =
        let expected = """Success: PredicateWithQualification
  (PredicateWithOptSpecification
     (((Ln: 1, Col: 1), (Ln: 1, Col: 9)),
      (PredicateIdentifier
         (((Ln: 1, Col: 1), (Ln: 1, Col: 3)), [PascalCaseId "Xx"]),
       Some
         (BrackedCoordList
            (((Ln: 1, Col: 3), (Ln: 1, Col: 9)),
             [PredicateWithQualification
                (PredicateWithOptSpecification
                   (((Ln: 1, Col: 4), (Ln: 1, Col: 8)),
                    (PredicateIdentifier
                       (((Ln: 1, Col: 4), (Ln: 1, Col: 6)), [PascalCaseId "Xx"]),
                     Some
                       (ArgumentTuple (((Ln: 1, Col: 6), (Ln: 1, Col: 8)), [])))),
                 QualificationList (((Ln: 1, Col: 8), (Ln: 1, Col: 8)), []))])))),
   QualificationList
     (((Ln: 1, Col: 9), (Ln: 1, Col: 12)),
      [DottedPredicate
         (((Ln: 1, Col: 9), (Ln: 1, Col: 12)),
          PredicateWithOptSpecification
            (((Ln: 1, Col: 10), (Ln: 1, Col: 12)),
             (PredicateIdentifier
                (((Ln: 1, Col: 10), (Ln: 1, Col: 12)), [PascalCaseId "Yy"]),
              None)))]))"""
        let result = run (predicateWithQualification .>> eof) """Xx[Xx()].Yy"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual)


    [<TestMethod>]
    member this.TestPascalCaseId1Ba () =
        let expected = """Success: PredicateWithQualification
  (PredicateWithOptSpecification
     (((Ln: 1, Col: 1), (Ln: 1, Col: 10)),
      (PredicateIdentifier
         (((Ln: 1, Col: 1), (Ln: 1, Col: 6)),
          [PascalCaseId "Xx"; PascalCaseId "Xx"]),
       Some
         (BrackedCoordList
            (((Ln: 1, Col: 6), (Ln: 1, Col: 10)),
             [PredicateWithQualification
                (PredicateWithOptSpecification
                   (((Ln: 1, Col: 7), (Ln: 1, Col: 9)),
                    (PredicateIdentifier
                       (((Ln: 1, Col: 7), (Ln: 1, Col: 9)), [PascalCaseId "Yy"]),
                     None)),
                 QualificationList (((Ln: 1, Col: 9), (Ln: 1, Col: 9)), []))])))),
   QualificationList (((Ln: 1, Col: 10), (Ln: 1, Col: 10)), []))
"""
        let result = run (predicateWithQualification .>> eof) """Xx.Xx[Yy]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual)

    [<TestMethod>]
    member this.TestPascalCaseId2B () =
        let expected = """Success: PredicateWithQualification
  (PredicateWithOptSpecification
     (((Ln: 1, Col: 1), (Ln: 1, Col: 16)),
      (PredicateIdentifier
         (((Ln: 1, Col: 1), (Ln: 1, Col: 3)), [PascalCaseId "Xx"]),
       Some
         (BrackedCoordList
            (((Ln: 1, Col: 3), (Ln: 1, Col: 16)),
             [PredicateWithQualification
                (PredicateWithOptSpecification
                   (((Ln: 1, Col: 4), (Ln: 1, Col: 15)),
                    (PredicateIdentifier
                       (((Ln: 1, Col: 4), (Ln: 1, Col: 6)), [PascalCaseId "Xx"]),
                     Some
                       (BrackedCoordList
                          (((Ln: 1, Col: 6), (Ln: 1, Col: 15)),
                           [PredicateWithQualification
                              (PredicateWithOptSpecification
                                 (((Ln: 1, Col: 7), (Ln: 1, Col: 11)),
                                  (PredicateIdentifier
                                     (((Ln: 1, Col: 7), (Ln: 1, Col: 9)),
                                      [PascalCaseId "Xx"]),
                                   Some
                                     (ArgumentTuple
                                        (((Ln: 1, Col: 9), (Ln: 1, Col: 11)), [])))),
                               QualificationList
                                 (((Ln: 1, Col: 11), (Ln: 1, Col: 14)),
                                  [DottedPredicate
                                     (((Ln: 1, Col: 11), (Ln: 1, Col: 14)),
                                      PredicateWithOptSpecification
                                        (((Ln: 1, Col: 12), (Ln: 1, Col: 14)),
                                         (PredicateIdentifier
                                            (((Ln: 1, Col: 12), (Ln: 1, Col: 14)),
                                             [PascalCaseId "Yy"]), None)))]))])))),
                 QualificationList (((Ln: 1, Col: 15), (Ln: 1, Col: 15)), []))])))),
   QualificationList (((Ln: 1, Col: 16), (Ln: 1, Col: 16)), []))
"""
        let result = run (predicateWithQualification .>> eof) """Xx[Xx[Xx().Yy]]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual)

    [<TestMethod>]
    member this.TestPascalCaseIdC () =
        let expected = """Success: PredicateWithQualification
  (PredicateWithOptSpecification
     (((Ln: 1, Col: 1), (Ln: 1, Col: 12)),
      (PredicateIdentifier
         (((Ln: 1, Col: 1), (Ln: 1, Col: 3)), [PascalCaseId "Xx"]),
       Some
         (BrackedCoordList
            (((Ln: 1, Col: 3), (Ln: 1, Col: 12)),
             [PredicateWithQualification
                (PredicateWithOptSpecification
                   (((Ln: 1, Col: 4), (Ln: 1, Col: 8)),
                    (PredicateIdentifier
                       (((Ln: 1, Col: 4), (Ln: 1, Col: 6)), [PascalCaseId "Yy"]),
                     Some
                       (ArgumentTuple (((Ln: 1, Col: 6), (Ln: 1, Col: 8)), [])))),
                 QualificationList
                   (((Ln: 1, Col: 8), (Ln: 1, Col: 11)),
                    [DottedPredicate
                       (((Ln: 1, Col: 8), (Ln: 1, Col: 11)),
                        PredicateWithOptSpecification
                          (((Ln: 1, Col: 9), (Ln: 1, Col: 11)),
                           (PredicateIdentifier
                              (((Ln: 1, Col: 9), (Ln: 1, Col: 11)),
                               [PascalCaseId "Zz"]), None)))]))])))),
   QualificationList (((Ln: 1, Col: 12), (Ln: 1, Col: 12)), []))"""
        let result = run (predicateWithQualification .>> eof) """Xx[Yy().Zz]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual)

    [<TestMethod>]
    member this.TestPascalCaseId1C () =
        let expected = """Success: PredicateWithQualification
  (PredicateWithOptSpecification
     (((Ln: 1, Col: 1), (Ln: 1, Col: 16)),
      (PredicateIdentifier
         (((Ln: 1, Col: 1), (Ln: 1, Col: 3)), [PascalCaseId "Xx"]),
       Some
         (BrackedCoordList
            (((Ln: 1, Col: 3), (Ln: 1, Col: 16)),
             [PredicateWithQualification
                (PredicateWithOptSpecification
                   (((Ln: 1, Col: 4), (Ln: 1, Col: 15)),
                    (PredicateIdentifier
                       (((Ln: 1, Col: 4), (Ln: 1, Col: 6)), [PascalCaseId "Xx"]),
                     Some
                       (BrackedCoordList
                          (((Ln: 1, Col: 6), (Ln: 1, Col: 15)),
                           [PredicateWithQualification
                              (PredicateWithOptSpecification
                                 (((Ln: 1, Col: 7), (Ln: 1, Col: 11)),
                                  (PredicateIdentifier
                                     (((Ln: 1, Col: 7), (Ln: 1, Col: 9)),
                                      [PascalCaseId "Yy"]),
                                   Some
                                     (ArgumentTuple
                                        (((Ln: 1, Col: 9), (Ln: 1, Col: 11)), [])))),
                               QualificationList
                                 (((Ln: 1, Col: 11), (Ln: 1, Col: 14)),
                                  [DottedPredicate
                                     (((Ln: 1, Col: 11), (Ln: 1, Col: 14)),
                                      PredicateWithOptSpecification
                                        (((Ln: 1, Col: 12), (Ln: 1, Col: 14)),
                                         (PredicateIdentifier
                                            (((Ln: 1, Col: 12), (Ln: 1, Col: 14)),
                                             [PascalCaseId "Zz"]), None)))]))])))),
                 QualificationList (((Ln: 1, Col: 15), (Ln: 1, Col: 15)), []))])))),
   QualificationList (((Ln: 1, Col: 16), (Ln: 1, Col: 16)), []))"""
        let result = run (predicateWithQualification .>> eof) """Xx[Xx[Yy().Zz]]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual)

    [<TestMethod>]
    member this.TestPascalCaseId2C () =
        let expected = """Success: PredicateWithQualification
  (PredicateWithOptSpecification
     (((Ln: 1, Col: 1), (Ln: 1, Col: 20)),
      (PredicateIdentifier
         (((Ln: 1, Col: 1), (Ln: 1, Col: 3)), [PascalCaseId "Xx"]),
       Some
         (BrackedCoordList
            (((Ln: 1, Col: 3), (Ln: 1, Col: 20)),
             [PredicateWithQualification
                (PredicateWithOptSpecification
                   (((Ln: 1, Col: 4), (Ln: 1, Col: 12)),
                    (PredicateIdentifier
                       (((Ln: 1, Col: 4), (Ln: 1, Col: 6)), [PascalCaseId "Xx"]),
                     Some
                       (BrackedCoordList
                          (((Ln: 1, Col: 6), (Ln: 1, Col: 12)),
                           [PredicateWithQualification
                              (PredicateWithOptSpecification
                                 (((Ln: 1, Col: 7), (Ln: 1, Col: 11)),
                                  (PredicateIdentifier
                                     (((Ln: 1, Col: 7), (Ln: 1, Col: 9)),
                                      [PascalCaseId "Xx"]),
                                   Some
                                     (ArgumentTuple
                                        (((Ln: 1, Col: 9), (Ln: 1, Col: 11)), [])))),
                               QualificationList
                                 (((Ln: 1, Col: 11), (Ln: 1, Col: 11)), []))])))),
                 QualificationList
                   (((Ln: 1, Col: 12), (Ln: 1, Col: 19)),
                    [DottedPredicate
                       (((Ln: 1, Col: 12), (Ln: 1, Col: 19)),
                        PredicateWithOptSpecification
                          (((Ln: 1, Col: 13), (Ln: 1, Col: 19)),
                           (PredicateIdentifier
                              (((Ln: 1, Col: 13), (Ln: 1, Col: 15)),
                               [PascalCaseId "Yy"]),
                            Some
                              (BrackedCoordList
                                 (((Ln: 1, Col: 15), (Ln: 1, Col: 19)),
                                  [PredicateWithQualification
                                     (PredicateWithOptSpecification
                                        (((Ln: 1, Col: 16), (Ln: 1, Col: 18)),
                                         (PredicateIdentifier
                                            (((Ln: 1, Col: 16), (Ln: 1, Col: 18)),
                                             [PascalCaseId "Zz"]), None)),
                                      QualificationList
                                        (((Ln: 1, Col: 18), (Ln: 1, Col: 18)),
                                         []))])))))]))])))),
   QualificationList (((Ln: 1, Col: 20), (Ln: 1, Col: 20)), []))"""
        let result = run (predicateWithQualification .>> eof) """Xx[Xx[Xx()].Yy[Zz]]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual)

    [<TestMethod>]
    member this.TestPascalCaseIdD () =
        let expected = """Success: PredicateWithQualification
  (PredicateWithOptSpecification
     (((Ln: 1, Col: 1), (Ln: 1, Col: 9)),
      (PredicateIdentifier
         (((Ln: 1, Col: 1), (Ln: 1, Col: 3)), [PascalCaseId "Xx"]),
       Some
         (BrackedCoordList
            (((Ln: 1, Col: 3), (Ln: 1, Col: 9)),
             [PredicateWithQualification
                (PredicateWithOptSpecification
                   (((Ln: 1, Col: 4), (Ln: 1, Col: 8)),
                    (PredicateIdentifier
                       (((Ln: 1, Col: 4), (Ln: 1, Col: 6)), [PascalCaseId "Yy"]),
                     Some
                       (ArgumentTuple (((Ln: 1, Col: 6), (Ln: 1, Col: 8)), [])))),
                 QualificationList (((Ln: 1, Col: 8), (Ln: 1, Col: 8)), []))])))),
   QualificationList
     (((Ln: 1, Col: 9), (Ln: 1, Col: 14)),
      [DottedPredicate
         (((Ln: 1, Col: 9), (Ln: 1, Col: 14)),
          PredicateWithOptSpecification
            (((Ln: 1, Col: 10), (Ln: 1, Col: 14)),
             (PredicateIdentifier
                (((Ln: 1, Col: 10), (Ln: 1, Col: 12)), [PascalCaseId "Zz"]),
              Some (ArgumentTuple (((Ln: 1, Col: 12), (Ln: 1, Col: 14)), [])))))]))"""
        let result = run (predicateWithQualification .>> eof) """Xx[Yy()].Zz()"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual)

