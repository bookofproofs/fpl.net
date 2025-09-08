namespace FplParser.Tests

open FParsec
open FplParser
open FplGrammarCommons
open Microsoft.VisualStudio.TestTools.UnitTesting



[<TestClass>]
type TestIdentifiers () =
    let replaceWhiteSpace (input: string) =
        let whiteSpaceChars = [|' '; '\t'; '\n'; '\r'|]
        input.Split(whiteSpaceChars)
            |> String.concat ""

    [<TestMethod>]
    member this.TestTheoryNamespace () =
        let result = run (theoryNamespace .>> eof) "Fpl.Test alias MyAlias"
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestTheoryNamespace2 () =
        let result = run (theoryNamespace .>> eof) "Fpl.Test"
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestUsesClause () =
        let result = run (buildingBlockList .>> eof) "uses  Fpl.Test alias MyAlias uses Fpl.Test uses Fpl.Test.Test1 "
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestUsesClause01 () =
        let result = run (buildingBlockList .>> eof) "uses Fpl.Commons uses Fpl.SetTheory.ZermeloFraenkel"
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestUsesClause02 () =
        let result = run (buildingBlockList .>> eof) "uses Fpl.Commons uses Fpl.SetTheory.ZermeloFraenkel alias ZF uses  Fpl.Arithmetics.Peano alias A"
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestUsesClause03 () =
        let result = run (buildingBlockList .>> eof) "uses Fpl.Commons *"
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicateIdentifier1 () =
        let result = run (predicateIdentifier .>> eof) "ThisIsMyIdentifier"
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicateIdentifier2 () =
        let result = run (predicateIdentifier .>> eof) "This.Is.My.Identifier"
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestIndexVariable1 () =
        let result = run (predicateWithQualification .>> eof) "x[@123]"
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestIndexVariable2 () =
        let result = run (predicateWithQualification .>> eof) "x[y]"
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestSelf0 () =
        let result = run (selfOrParent .>> eof) literalSelf
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestSelf1 () =
        let result = run (selfOrParent .>> eof) "@self"
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestSelf2 () =
        let result = run (selfOrParent .>> eof) literalParent
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestEntitySelf () =
        let result = run (entity .>> eof) literalSelf
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestEntityAtSelf () =
        let result = run (entity .>> eof) literalSelf
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestEntityVariable () =
        let result = run (entity .>> eof) "xyz"
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestExtensionName () =
        let result = run (extensionName .>> eof) "Digits"
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestExtensionBlock1 () =
        let result = run (extensionBlock .>> eof) """ext Digits x @/\d+/ ->Nat {return x}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestExtensionBlock1a () =
        let result = run (extensionBlock .>> eof) """extension Digits x@ /\d+/ -> Nat { ret x}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestExtensionBlock1b () =
        let result = run (extensionBlock .>> eof) """extension Digits x @ /\d+/ -> Nat { return x}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestExtensionBlock1c () =
        let result = run (extensionBlock .>> eof) """ext Digits x@/\d+/->Nat{return x}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestEntityWithCoord1 () =
        let result = run (predicateWithQualification .>> eof) """myField[@1 , n]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestEntityWithCoord2 () =
        let result = run (predicateWithQualification .>> eof) """theorem[from , to]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestEntityWithCoord3 () =
        let result = run (predicateWithQualification .>> eof) """self[from , to]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestEntityWithCoord4 () =
        let result = run (predicateWithQualification .>> eof) """tplSetElem[from , to]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestEntityWithCoord5 () =
        let result = run (predicateWithQualification .>> eof) """tpls[from , to]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))
