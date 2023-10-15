namespace FplParser.Tests

open FParsec
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
        let result = run specificClassType """object"""
        let actual = sprintf "%O" result
        let expected = """Success: ObjectType"""
        Assert.AreEqual(replaceWhiteSpace expected, replaceWhiteSpace actual);

    [<TestMethod>]
    member this.TestSpecificType4 () =
        let result = run specificClassType """tpl"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestSpecificType5 () =
        let result = run specificClassType """tplSetElem"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))


    member this.TestSpecificType7 () =
        let result = run specificClassType """@extNat"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestSpecificType8 () =
        let result = run specificClassType """SomeClass"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestSpecificType9 () =
        let result = run specificClassType """bla"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestClassType3 () =
        let result = run classType """object[self~]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestClassType3a () =
        let result = run classType """object<SomeObject1, SomeObject2,SomeObject3>"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestClassType4 () =
        let result = run classType """tpl[from~]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestClassType4a () =
        let result = run classType """tpl[~ to]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestClassType4b () =
        let result = run classType """tpl[~]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestClassType5 () =
        let result = run classType """Set[from ~ to]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestClassType5a () =
        let result = run classType """Set[!from ~ to]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestClassType5b () =
        let result = run classType """Set[from ~ to!]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))


    [<TestMethod>]
    member this.TestClassType7 () =
        let result = run classType """@Nat"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestClassType8 () =
        let result = run classType """SomeClass"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestClassType9 () =
        let result = run classType """bla"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))