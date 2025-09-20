namespace FplParser.Tests

open FParsec
open FplPrimitives
open ErrDiagnostics
open FplParser
open Microsoft.VisualStudio.TestTools.UnitTesting


[<TestClass>]
type TestErrRecoveryLowLevel() =
    let replaceWhiteSpace (input: string) =
        let whiteSpaceChars = [|' '; '\t'; '\n'; '\r'|]
        input.Split(whiteSpaceChars)
            |> String.concat ""

    member this.TestSplitStringByTextAtPosition_Pre(expPre: string, actPre: string) =
        Assert.AreEqual<string>(expPre, actPre)

    member this.TestSplitStringByTextAtPosition_OptTrailingWs(expOptTrailingWs: string, actExptOptTrailingWs: string) =
        Assert.AreEqual<string>(expOptTrailingWs, actExptOptTrailingWs)

    member this.TestSplitStringByTextAtPosition_Post(expPost: string, actPost: string) =
        Assert.AreEqual<string>(expPost, actPost)


    [<TestMethod>]
    [<DataRow("""""", "")>]
    [<DataRow("""Error in Ln: 3 Col: 56
    D(x:tpl) { pre : true con : true } }           )
    ^
    Expecting: <block comment>, <inline comment>, <significant whitespace>, 'th' or
    'theory'
    """, "'th', 'theory', <block comment>, <inline comment>, <significant whitespace>")>]
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
    [<DataRow("""Failure:
Error in Ln: 1 Col: 6
dec: tpl: Nat;
     ^
Expecting: <block comment>, <inline comment>, <significant whitespace>,
<whitespace> or ';'
Other error messages:
  Expecting: <variable (got template)>""", "';', <block comment>, <inline comment>, <significant whitespace>, <variable (got template)>, <whitespace>")>]    
    [<DataRow("""Error in Ln: 6 Col: 22
                x := theorem
                     ^
Expecting: <PascalCaseId>, <argument identifier>, <digits>, '<', '@', 'all', 'and', 'del', 'delegate', 'ex', 'false', 'iif', 'impl', 'is', 'not', 'or', 'self', 'true', 'undef', 'undefined' or 'xor' Other error messages:   Expecting: <variable (got keyword)>""", "'<', '@', 'all', 'and', 'del', 'delegate', 'ex', 'false', 'iif', 'impl', 'is', 'not', 'or', 'self', 'true', 'undef', 'undefined', 'xor', <PascalCaseId>, <argument identifier>, <digits>, <variable (got keyword)>")>]    
    member this.TestRetrieveExpectedParserChoices(fParsecErrMsg:string, expected:string) = 
        let actual = retrieveExpectedParserChoices fParsecErrMsg
        let actualS = String.concat ", " actual
        Assert.AreEqual<string>(expected, actualS)

    [<TestMethod>]
    [<DataRow("""Error in Ln: 3 Col: 14
        D(x: +  )
             ^
Expecting: <PascalCaseId>, <whitespace>, '@', 'obj', 'object', 'template' or
'tpl'

The parser backtracked after:
  Error in Ln: 3 Col: 15
          D(x: +  )
                ^
  Expecting: <PascalCaseId>, '@', 'obj', 'object', 'template' or 'tpl'""", "<xxx>", "'+'
  Expecting:<xxx>")>]
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
        let pos = Position("",0,0,0)
        let actual = replaceFParsecErrMsgForFplParser input inputChoices pos
        Assert.AreEqual<string>(replaceWhiteSpace expected, replaceWhiteSpace actual)

    [<TestMethod>]
    [<DataRow("""// definition of a functional term denoting the successor of a natural number
        function T() -> Real { intr }

        // definition of a new mathematical object (natural number)
        function T() -> Real { intr }

        pred IsGreaterOrEqual(n,m: Nat)
        {
            ex k in Nat ( <n = Add(m,k)> )
        }

        // besides the class "Nat", we can formulate definition of the set of all natural numbers
        class SetNat: Set
        {
            SetNat()
            {
                dec 
                    ~n: Nat
                    // Assert that elements of class "Nat" can be collected to a bigger object of class "SetNat"
                    // This requires that we can apply the "In" predicate defined in Fpl.Set.ZermeloFraenkel
                    // to object of the class "Nat". This becomes possible when we assert that every variable of the class
                    // "Nat" is a also Set.
                    // This is comparable to implementing an interface (or comparable to multiple inheritance).
                    base.Set()
                ;
                
            }
        }

        // Addition of natural numbers
        function T() -> Real { intr }
        /* This is 
        a 

        test
        */

        /* This is 
        another

        test
        */
        function T() -> Real { intr }

        // Example of defining a constant for the natural number 100 using the
        class N100:Set{N100(){dec ~n:Nat  base.Set() base.SetBuilder(SetNat(),IsGreater(n,100)); }}""", 1595, 46)>]
    [<DataRow("",0,1)>]
    member this.TestReplaceFplComments
        (
            input: string,
            expectedLength: int,
            expectedNumbLines: int
        ) =
        let r = removeFplComments input 
        // printfn "%i, %i, %i, %i" input.Length r.Length (input.Split('\n').Length) (r.Split('\n').Length)
        Assert.AreEqual<int>(expectedLength, r.Length)
        Assert.AreEqual<int>(expectedNumbLines, r.Split('\n').Length)
