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
    [<DataRow("T { : theory { } }", "inf", 4, "T {", " ", ": theory { } }")>]
    [<DataRow("T {  \t: theory { } }", "inf", 6, "T {", "  \t", ": theory { } }")>]
    [<DataRow("T {  \t \t : theory { } }", "inf", 9, "T {", "  \t \t ", ": theory { } }")>]
    [<DataRow("T { inf { theory { pred I() } }", "ExampleId", 10, "T { inf {", " ", "theory { pred I() } }")>]
    [<DataRow("T { inf { ExampleId  theory { pred I() } }", "§", 21, "T { inf { ExampleId", "  ", "theory { pred I() } }")>]

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

    [<TestMethod>]
    [<DataRow("", "", 0, " ", 1)>]
    [<DataRow("T { : theory { } }", "inf", 4, "T { inf  theory { } }", 3)>]
    [<DataRow("T { inf { T () { theory { } }", "§", 17, "T { inf { T () { §  theory { } }", 2)>]
    member this.TestManipulateString
        (
            input: string,
            text: string,
            ind: int64,
            expNewInput,
            expNewOffset
        ) =
        let pos = Position("", ind, 0, 0)
        let (actNewInput, newRecoveryText, newIndexOffset) = manipulateString input text pos "" (int64 0)
        Assert.AreEqual(expNewInput, actNewInput)
        Assert.AreEqual(expNewOffset, newIndexOffset)

    [<TestMethod>]
    [<DataRow("T { inf { T ( § theory { } }", ")", 14, "{ T ( § ", "{ T ( ) ")>]
    member this.TestManipulateStringRecoveryString
        (
            input: string,
            text: string,
            ind: int64,
            lastRecoveryText,
            expRecoveryText
        ) =
        let pos = Position("", ind, 0, 0)
        let (actNewInput, newRecoveryText, newIndexOffset) = manipulateString input text pos lastRecoveryText (int64 0)
        Assert.AreEqual(expRecoveryText, newRecoveryText)
