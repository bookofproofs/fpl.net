namespace FplParser.Tests

open FParsec
open FplParser
open Microsoft.VisualStudio.TestTools.UnitTesting


[<TestClass>]
type TestLocalizations () =
    let replaceWhiteSpace (input: string) =
        let whiteSpaceChars = [|' '; '\t'; '\n'; '\r'|]
        input.Split(whiteSpaceChars)
            |> String.concat ""

    [<TestMethod>]
    member this.TestEbnfTerm01 () =
        let result = run ebnfTerm """x "\Leftrightarrow" y """
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestEbnfTerm02 () =
        let result = run ebnfTerm """"\neg(" x ")" """
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestEbnfTransl01 () =
        let result = run ebnfTransl """x "\Leftrightarrow" y """
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestEbnfTransl02 () =
        let result = run ebnfTransl """"\neg(" x ")" """
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))
        
    [<TestMethod>]
    member this.TestEbnfTransl03 () =
        let result = run ebnfTransl """x "\Leftrightarrow" y | x "\Rightarrow" y """
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestEbnfTransl04 () =
        let result = run ebnfTransl """"\neg(" x ")" | x "\Rightarrow" y """
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestTranslation01 () =
        let result = run translation """~tex: x "\Leftrightarrow" y """
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestTranslation02 () =
        let result = run translation """~tex: x "\Leftrightarrow" y | x "\Rightarrow" y """
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestLocalization01 () =
        let result = run localization """loc not(x) :=
            ~tex: "\neg(" x ")"
            ~eng: "not " x
            ~ger: "nicht " x
            ; """
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestLocalization02 () =
        // comment
        let result = run localization """loc iif(x,y) :=
            ~tex: x "\Leftrightarrow" y
            ~eng: x " if and only if " y
            ~ger: x " dann und nur dann wenn " y
            ;"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestLocalization03 () =
        let result = run localization """loc NotEqual(x,y) :=
            ~tex: x "\neq" y
            ~eng: x "is unequal" y
            ~ger: x "ist ungleich" y
            ~pol: x ( "nie równa się" , "nie równe" ) y
            ;"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))
