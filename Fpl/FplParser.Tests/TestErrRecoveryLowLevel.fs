namespace FplParser.Tests

open FParsec
open FplGrammarCommons
open ErrRecovery
open FplGrammar
open Microsoft.VisualStudio.TestTools.UnitTesting


[<TestClass>]
type TestErrRecoveryLowLevel() =

    member this.TestSplitStringByTextAtPosition_Pre(expPre: string, actPre: string) =
        Assert.AreEqual(expPre, actPre)

    member this.TestSplitStringByTextAtPosition_OptTrailingWs(expOptTrailingWs: string, actExptOptTrailingWs: string) =
        Assert.AreEqual(expOptTrailingWs, actExptOptTrailingWs)

    member this.TestSplitStringByTextAtPosition_Post(expPost: string, actPost: string) =
        Assert.AreEqual(expPost, actPost)

    [<TestMethod>]
    [<DataRow("T { : theory { } }", "uses", 4, "T {", " ", ": theory { } }")>]
    [<DataRow("T {  \t: theory { } }", "uses", 6, "T {", "  \t", ": theory { } }")>]
    [<DataRow("T {  \t \t : theory { } }", "uses", 9, "T {", "  \t \t ", ": theory { } }")>]
    member this.TestSplitStringByTextAtPosition
        (
            input: string,
            text: string,
            ind: int64,
            expPre: string,
            expOptTrailingWs: string,
            expPost: string
        ) =
        let pos = Position("", ind, 0, 0)
        let (actPre, actExptOptTrailingWs, actPost) = splitStringByTextAtPosition input text pos
        this.TestSplitStringByTextAtPosition_Pre(expPre, actPre)
        this.TestSplitStringByTextAtPosition_OptTrailingWs(expOptTrailingWs, actExptOptTrailingWs)
        this.TestSplitStringByTextAtPosition_Post(expPost, actPost)
