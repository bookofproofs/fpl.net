namespace FplParser.Tests

open FParsec
open FplParser
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestFplBlockComponentes () =
    let replaceWhiteSpace (input: string) =
        let whiteSpaceChars = [|' '; '\t'; '\n'; '\r'|]
        input.Split(whiteSpaceChars)
            |> String.concat ""

    [<TestMethod>]
    member this.TestSignature01 () =
        let result = run (predicateSignature .>> eof) """pred AreRelated(u,v: Set, r: BinaryRelation)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestSignature02 () =
        let result = run (ruleOfInferenceSignature .>> eof) """inf ExistsByExample"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestSignature03 () =
        let result = run (constructorSignature .>> eof) """ctor Zero()"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestSignature04 () =
        let result = run (predicateInstanceSignature .>> eof) """pred Test(a,b: tpl)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestSignature05 () =
        let result = run (functionalTermSignature .>> eof) """func TestPredicate(a,b:Obj)->obj"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestSignature06 () =
        let result = run (functionalTermInstanceSignature .>> eof) """func BinOp(x,y: tplSetElem) -> tplSetElem"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestSignature07 () =
        let result = run (predicateSignature .>> eof) """pred IsSubset(subset,superset: Set) infix "in" 2"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestSignature08 () =
        let result = run (constructorSignature .>> eof) """constructor SetRoster(listOfSets:* Set)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestSignature09 () =
        let result = run (functionalTermSignature .>> eof) """func VecAdd(v,w: tplFieldElem) -> obj prefix "+" """
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestSignature10 () =
        let result = run (classSignature .>> eof) """cl ZeroVectorN"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestSignature11 () =
        let result = run (ruleOfInferenceSignature .>> eof) """inference ProceedingResults"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestSignature12 () =
        let result = run (constructorSignature .>> eof) """ctor Nat(x: @Decimal)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestSignature13 () =
        let result = run (functionalTermInstanceSignature .>> eof) """func Add(n,m: Nat) -> pred"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestSignature14 () =
        let result = run (constructorSignature .>> eof) """ctor AlgebraicStructure(x: tplSet, ops:+ func(args:* tplSetElem)->tplSetElem)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

