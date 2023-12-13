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
        let expected = """"""
        let result = run (predicateWithQualification .>> eof) """Xx[Xx.Xx]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPascalCaseId2A () =
        let expected = """"""
        let result = run (predicateWithQualification .>> eof) """Xx[Xx.Xx()]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPascalCaseId1B () =
        let expected = """"""
        let result = run (predicateWithQualification .>> eof) """Xx[Xx()].Yy"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))


    [<TestMethod>]
    member this.TestPascalCaseId1Ba () =
        let expected = """"""
        let result = run (predicateWithQualification .>> eof) """Xx.Xx[Yy]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPascalCaseId2B () =
        let expected = """"""
        let result = run (predicateWithQualification .>> eof) """Xx[Xx[Xx().Yy]]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPascalCaseIdC () =
        let expected = """"""
        let result = run (predicateWithQualification .>> eof) """Xx[Yy().Zz]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPascalCaseId1C () =
        let expected = """"""
        let result = run (predicateWithQualification .>> eof) """Xx[Xx[Yy().Zz]]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPascalCaseId2C () =
        let expected = """"""
        let result = run (predicateWithQualification .>> eof) """Xx[Xx[Xx()].Yy[Zz]]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPascalCaseIdD () =
        let expected = """"""
        let result = run (predicateWithQualification .>> eof) """Xx[Yy()].Zz()"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))
