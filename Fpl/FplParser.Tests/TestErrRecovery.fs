namespace FplParser.Tests

open FParsec
open FplGrammarCommons
open ErrRecovery
open Microsoft.VisualStudio.TestTools.UnitTesting


[<TestClass>]
type TestErrRecovery() =
    let replaceWhiteSpace (input: string) =
        let whiteSpaceChars = [| ' '; '\t'; '\n'; '\r' |]
        input.Split(whiteSpaceChars) |> String.concat ""

    [<TestMethod>]
    [<DataRow("and")>]
    [<DataRow("and ")>]
    [<DataRow("and\n")>]
    [<DataRow("and\t")>]
    [<DataRow("and(")>]
    [<DataRow("theory{")>]
    member this.TestStartWithFplKeyword(test: string) =
        let result = startsWithFplKeyword test
        Assert.IsTrue(result)

    [<TestMethod>]
    [<DataRow("xxx")>]
    [<DataRow("xxx ")>]
    [<DataRow("xxx\n")>]
    [<DataRow("xxx\t")>]
    [<DataRow("xxx(")>]
    [<DataRow("xxx{")>]
    member this.TestNotStartWithFplKeyword(test: string) =
        let result = startsWithFplKeyword test
        Assert.IsFalse(result)

