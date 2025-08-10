namespace FplParser.Tests

open FParsec
open FplGrammarCommons
open FplParser
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestClassInheritanceTypes () =
    let replaceWhiteSpace (input: string) =
        let whiteSpaceChars = [|' '; '\t'; '\n'; '\r'|]
        input.Split(whiteSpaceChars)
            |> String.concat ""

    [<TestMethod>]
    member this.TestSpecificType3 () =
        let result = run (specificClassType .>> eof) """object """
        let actual = sprintf "%O" result
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestSpecificType4 () =
        let result = run (specificClassType .>> eof) literalTpl
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestSpecificType5 () =
        let result = run (specificClassType .>> eof) """tplSetElem"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))


    member this.TestSpecificType7 () =
        let result = run (specificClassType .>> eof) """@extNat"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestSpecificType8 () =
        let result = run (specificClassType .>> eof) """SomeClass"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestSpecificType9 () =
        let result = run (specificClassType .>> eof) """bla"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestClassType3 () =
        let result = run (classType .>> eof) """object[self,]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestClassType3a () =
        let result = run (classType .>> eof) """object[x:SomeObject1, y:SomeObject2, a,b,c:SomeObject3]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestClassType4 () =
        let result = run (classType .>> eof) """tpl[a:Nat,b:func]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestClassType4a () =
        let result = run (classType .>> eof) """tpl[a:pred , b:index]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestClassType4b () =
        let result = run (classType .>> eof) """tpl[a:tpl ,b:tplA]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestClassType5 () =
        let result = run (classType .>> eof) """Set[x:ind , a,b:Nat]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestClassType5a () =
        let result = run (classType .>> eof) """Set[x:index , y:func]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestClassType5a1 () =
        let result = run (classType .>> eof) """Set[x:index , y:func()->obj]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestClassType5b () =
        let result = run (classType .>> eof) """Set[a:func()]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))


    [<TestMethod>]
    member this.TestClassType7 () =
        let result = run (classType .>> eof) """@Nat"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestClassType8 () =
        let result = run (classType .>> eof) """SomeClass"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestClassType9 () =
        let result = run (classType .>> eof) """bla"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestClassType10 () =
        let result = run (classType .>> eof) """object[x:ind,y:index]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestClassType10a () =
        let result = run (classType .>> eof) """object[x:obj,y:Nat]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))