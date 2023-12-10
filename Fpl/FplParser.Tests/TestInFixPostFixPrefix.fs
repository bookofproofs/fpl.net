namespace FplParser.Tests

open FParsec
open FplParser
open Microsoft.VisualStudio.TestTools.UnitTesting


[<TestClass>]
type TestInfixPostfixPrefix () =
    let replaceWhiteSpace (input: string) =
        let whiteSpaceChars = [|' '; '\t'; '\n'; '\r'|]
        input.Split(whiteSpaceChars)
            |> String.concat ""

    [<TestMethod>]
    member this.TestPostfix () =
        let expected = """"""
        let result = run (definition .>> eof) """def pred Successor postfix "'" (x: Nat) { intr }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPostfixUsage01 () =
        let expected = """"""
        let result = run (predicate .>> eof) """x'"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPostfixUsage01a () =
        let expected = """"""
        let result = run (predicate .>> eof) """x''"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPostfixUsage02 () =
        let expected = """"""
        let result = run (predicate .>> eof) """x(i)'"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPostfixUsage02a () =
        let expected = """"""
        let result = run (predicate .>> eof) """x'(i)'"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestPostfixUsage03 () =
        let expected = """"""
        let result = run (predicate .>> eof) """x[i]'"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPostfixUsage04 () =
        let expected = """"""
        let result = run (predicate .>> eof) """x.SomeProperty()'"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPostfixUsage05 () =
        let expected = """"""
        let result = run (predicate .>> eof) """1'"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestInfix () =
        let expected = """"""
        let result = run (definition .>> eof) """def func Add infix "+" (x,y: Nat) -> Nat { intr }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestInfixUsage01 () =
        let expected = """"""
        let result = run (predicate .>> eof) """(x + y)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestInfixUsage01a () =
        let expected = """"""
        let result = run (predicate .>> eof) """(x + y + z)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestCombinedUsage01 () =
        let expected = """"""
        let result = run (predicate .>> eof) """(x' + y'' < z)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestCombinedUsage01a () =
        let expected = """"""
        let result = run (predicate .>> eof) """(-x' + -y'' < -z)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestCombinedUsage02 () =
        let expected = """"""
        let result = run (predicate .>> eof) """(x' + (y'' < z))"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestCombinedUsage03 () =
        let expected = """"""
        let result = run (predicate .>> eof) """((x' + y'') < z)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestCombinedUsage04 () =
        let expected = """"""
        let result = run (predicate .>> eof) """(-x' + -y'' + z)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestCombinedUsage05 () =
        let expected = """"""
        let result = run (predicate .>> eof) """-(x + -y)'"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPrefix () =
        let expected = """"""
        let result = run (definition .>> eof) """def func Minus prefix "-" (x: Nat) -> Nat { intr }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPrefixUsage01 () =
        let expected = """"""
        let result = run (predicate .>> eof) """'x"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPrefixUsage01a () =
        let expected = """"""
        let result = run (predicate .>> eof) """''x"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPrefixUsage02 () =
        let expected = """"""
        let result = run (predicate .>> eof) """'x(i)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPrefixUsage03 () =
        let expected = """"""
        let result = run (predicate .>> eof) """'x[i]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPrefixUsage04 () =
        let expected = """"""
        let result = run (predicate .>> eof) """'x.SomeProperty()"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPrefixUsage05 () =
        let expected = """"""
        let result = run (predicate .>> eof) """'1"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPrefixUsage06 () =
        let expected = """"""
        let result = run (predicate .>> eof) """-(x)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPrefixUsage06a () =
        let expected = """"""
        let result = run (predicate .>> eof) """-Test(x)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPostfixUsage06a () =
        let expected = """"""
        let result = run (predicate .>> eof) """-f(x)'"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPostfixUsage06b () =
        let expected = """"""
        let result = run (predicate .>> eof) """-f((x + 1))!"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPostfixUsage06c () =
        let expected = """"""
        let result = run (predicate .>> eof) """f(x)!"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPostfixUsage06d () =
        let expected = """"""
        let result = run (predicate .>> eof) """-x!"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPostfixUsage07 () =
        let expected = """"""
        let result = run (predicate .>> eof) """f'(x)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))
