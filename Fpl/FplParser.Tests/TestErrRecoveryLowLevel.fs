﻿namespace FplParser.Tests

open FParsec
open FplGrammarCommons
open ErrRecovery
open FplGrammar
open Microsoft.VisualStudio.TestTools.UnitTesting


[<TestClass>]
type TestErrRecoveryLowLevel() =
    let replaceWhiteSpace (input: string) =
        let whiteSpaceChars = [|' '; '\t'; '\n'; '\r'|]
        input.Split(whiteSpaceChars)
            |> String.concat ""

    member this.TestSplitStringByTextAtPosition_Pre(expPre: string, actPre: string) =
        Assert.AreEqual(expPre, actPre)

    member this.TestSplitStringByTextAtPosition_OptTrailingWs(expOptTrailingWs: string, actExptOptTrailingWs: string) =
        Assert.AreEqual(expOptTrailingWs, actExptOptTrailingWs)

    member this.TestSplitStringByTextAtPosition_Post(expPost: string, actPost: string) =
        Assert.AreEqual(expPost, actPost)

    [<TestMethod>]
    [<DataRow("T { inf { D() { pre : true con : true      }  theory { y} }", "}", 46, "T { inf { D() { pre : true con : true      }", "  ", "theory { y} }")>]
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
    [<DataRow("T { inf { T () { theory { } }", "§", 17, "T { inf { T () { §  theory { } }", 3)>]
    member this.TestManipulateString
        (
            input: string,
            text: string,
            ind: int64,
            expNewInput,
            expNewOffset
        ) =
        let pos = Position("", ind, 0, 0)
        let (actNewInput, newRecoveryText, newIndexOffset, keywordLength) = manipulateString input text pos "" 
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
        let (actNewInput, newRecoveryText, newIndexOffset, keywordLength) = manipulateString input text pos lastRecoveryText 
        Assert.AreEqual(expRecoveryText, newRecoveryText)


    [<TestMethod>]
    [<DataRow("""""", "")>]
    [<DataRow("""Error, (Ln: 5, Col: 5),
   DiagnosticMessage
     "recovery failed; Error in Ln: 5 Col: 5
    }
    ^
Expecting: <PascalCaseId>, <argument identifier>, <block comment>, <digits>,
<indexed variable>, <inline comment>, <significant whitespace>, <variable>, '@'
, 'all', 'and', 'assert', 'cases', 'del', 'delegate', 'ex', 'false', 'iif',
'impl', 'is', 'loop', 'not', 'or', 'range', 'ret', 'return', 'self', 'true',
'undef', 'undefined' or 'xor'""", "'@', 'all', 'and', 'assert', 'cases', 'del', 'delegate', 'ex', 'false', 'iif', 'impl', 'is', 'loop', 'not', 'or', 'range', 'ret', 'return', 'self', 'true', 'undef', 'undefined', 'xor', <PascalCaseId>, <argument identifier>, <block comment>, <digits>, <indexed variable>, <inline comment>, <significant whitespace>, <variable>")>]
    [<DataRow("""Error in Ln: 3 Col: 5
    theory {   
    ^
Expecting: <PascalCaseId>, <block comment>, <inline comment> or <significant
whitespace>
""", "<PascalCaseId>, <block comment>, <inline comment>, <significant whitespace>")>]
    [<DataRow("""Error in Ln: 4 Col: 5
    theory {
    ^
Expecting: <block comment>, <inline comment>, <significant whitespace>, 'pre'
or 'premise'
or 'ddd',
ddd


The parser backtracked after:
  Error in Ln: 4 Col: 11
      theory {
            ^
  Expecting: <variable (got keyword 'theory')>""", "'ddd', 'pre', 'premise', <block comment>, <inline comment>, <significant whitespace>, <variable (got keyword 'theory')>")>]
    [<DataRow(""" Error in Ln: 4 Col: 9
        y
        ^
Expecting: <block comment>, <inline comment>, <significant whitespace>, 'ax',
'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 'func',
'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 'prf',
'proof', 'prop', 'proposition', 'theorem', 'thm' or '}'""", "'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, <inline comment>, <significant whitespace>")>]
    [<DataRow("""Error in Ln: 4 Col: 5
    theory {
    ^
Expecting: <block comment>, <inline comment>, <significant whitespace>, 'pre'
or 'premise'

The parser backtracked after:
  Error in Ln: 4 Col: 11
      theory {
            ^
  Expecting: <variable (got keyword)>
""", "'pre', 'premise', <block comment>, <inline comment>, <significant whitespace>, <variable (got keyword)>")>]
    [<DataRow(""" Error in Ln: 2 Col: 5
    x
    ^
Expecting: <block comment>, <inline comment>, <significant whitespace>, ':ext',
'inf', 'inference', 'th', 'theory' or 'uses'""", "':ext', 'inf', 'inference', 'th', 'theory', 'uses', <block comment>, <inline comment>, <significant whitespace>")>]
    member this.TestRetrieveExpectedParserChoices(fParsecErrMsg:string, expected:string) = 
        let actual = retrieveExpectedParserChoices fParsecErrMsg
        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    [<DataRow("""Error in Ln: 7 Col: 2
    }
     ^
    Note: The error occurred at the end of the input stream.
    Expecting: <block comment>, <inline comment>, <significant whitespace>, 'loc',
    'localization' or '}'""", "<xxx>", "'}'
    Expecting: <xxx>")>]
    member this.TestReplaceFParsecErrMsgForFplParser
        (
            input: string,
            inputChoices: string,
            expected: string
        ) =
        let actual = replaceFParsecErrMsgForFplParser input inputChoices
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual)
