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
        let result = run (signature .>> eof) """AreRelated(u,v: Set, r: BinaryRelation)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestSignature02 () =
        let result = run (signature .>> eof) """ExistsByExample(p: pred(c: obj))"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestSignature03 () =
        let result = run (signature .>> eof) """Zero()"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestSignature04 () =
        let result = run (signature .>> eof) """Test(a,b: tpl)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestSignature05 () =
        let result = run (signature .>> eof) """TestPredicate(a,b:obj)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestSignature06 () =
        let result = run (signature .>> eof) """BinOp(x,y: tplSetElem)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestSignature07 () =
        let result = run (signature .>> eof) """IsSubset(subset,superset: Set)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestSignature08 () =
        let result = run (signature .>> eof) """SetRoster(listOfSets:* Set)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestSignature09 () =
        let result = run (signature .>> eof) """VecAdd(from,to: Nat, v,w: tplFieldElem[Nat , Nat])"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestSignature10 () =
        let result = run (signature .>> eof) """ZeroVectorN(n: Nat, field: Field)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestSignature11 () =
        let result = run (signature .>> eof) """ProceedingResults(p:+ pred)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestSignature12 () =
        let result = run (signature .>> eof) """Nat(x: @Decimal)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestSignature13 () =
        let result = run (signature .>> eof) """Add(n,m: Nat)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestSignature14 () =
        let result = run (signature .>> eof) """AlgebraicStructure(x: tplSet, ops:+ Composition(args:* tplSetElem))"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

