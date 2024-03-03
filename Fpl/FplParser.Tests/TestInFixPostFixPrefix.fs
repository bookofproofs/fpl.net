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
        let result = run (definition .>> eof) """def pred Successor postfix "'" (x: Nat) { intr }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPostfixUsage01 () =
        let result = run (predicate .>> eof) """x'"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPostfixUsage01a () =
        let result = run (predicate .>> eof) """x''"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPostfixUsage02 () =
        let result = run (predicate .>> eof) """x(i)'"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPostfixUsage02a () =
        
        let result = run (predicate .>> eof) """x'(i)'"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestPostfixUsage03 () =
        let result = run (predicate .>> eof) """x[i]'"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPostfixUsage04 () =
        
        let result = run (predicate .>> eof) """x.SomeProperty()'"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPostfixUsage05 () =
        let result = run (predicate .>> eof) """1'"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPostfixUsage06a () =
        let result = run (predicate .>> eof) """-f(x)'"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPostfixUsage06b () =
        let result = run (predicate .>> eof) """-f((x + 1))!"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPostfixUsage06c () =
        let result = run (predicate .>> eof) """f(x)!"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPostfixUsage06d () =
        let result = run (predicate .>> eof) """-x!"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPostfixUsage07 () =
        let result = run (predicate .>> eof) """f'(x)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPostfixUsage08 () =
        let result = run (predicate .>> eof) """x'"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPostfixUsage08a () =
        let result = run (predicate .>> eof) """(x')'"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPostfixUsage08b () =
        let result = run (predicate .>> eof) """x''"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPostfixUsage09 () =
        let result = run (predicate .>> eof) """x '"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestInfix () =
        let result = run (definition .>> eof) """def func Add infix "+" (x,y: Nat) -> Nat { intr }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestInfixUsage01 () =
        let result = run (predicate .>> eof) """(x + y)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestInfixUsage01a () =
        let result = run (predicate .>> eof) """(x + y + z)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestInfixUsage02 () =
        
        let result = run (predicate .>> eof) """((x + y) + z)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestInfixUsage03 () =
        let result = run (predicate .>> eof) """(x + (y + z))"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestInfixUsage04 () =
        let result = run (predicate .>> eof) """(x ∈ z)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))
        
    [<TestMethod>]
    member this.TestInfixUsage04a () =
        let result = run (predicate .>> eof) """(x and z)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestInfixUsage04b () =
        let result = run (predicate .>> eof) """(x in z)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestCombinedUsage01 () =
        let result = run (predicate .>> eof) """(x' + y'' < z)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestCombinedUsage01a () =
        
        let result = run (predicate .>> eof) """(-x' + -y'' < -z)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestCombinedUsage02 () =
        let result = run (predicate .>> eof) """(x' + (y'' < z))"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestCombinedUsage03 () =
        let result = run (predicate .>> eof) """((x' + y'') < z)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestCombinedUsage04 () =
        let result = run (predicate .>> eof) """(-x' + -y'' + z)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestCombinedUsage05 () =
        let result = run (predicate .>> eof) """-(x + -y)'"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestCombinedUsage06 () =
        let result = run (predicate .>> eof) """-(x + y)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestCombinedUsage07 () =
        let result = run (predicate .>> eof) """-(x + y).Test()"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestCombinedUsage08 () =
        let result = run (predicate .>> eof) """(f -∘ g)(x)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestCombinedUsage09 () =
        let result = run (predicate .>> eof) """(f + g)'(x)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestCombinedUsage10 () =
        let result = run (predicate .>> eof) """(f + -g)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestCombinedUsage10a () =
        let result = run (predicate .>> eof) """(f + - g)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPrefix () =
        let result = run (definition .>> eof) """def func Minus prefix "-" (x: Nat) -> Nat { intr }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPrefixUsage01 () =
        let result = run (predicate .>> eof) """'x"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPrefixUsage01a () =
        let result = run (predicate .>> eof) """''x"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPrefixUsage02 () =
        let result = run (predicate .>> eof) """'x(i)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPrefixUsage03 () =
        let result = run (predicate .>> eof) """'x[i]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPrefixUsage04 () =
        let result = run (predicate .>> eof) """'x.SomeProperty()"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPrefixUsage05 () =
        let result = run (predicate .>> eof) """'1"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPrefixUsage06 () =
        let result = run (predicate .>> eof) """-(x)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPrefixUsage06a () =
        let result = run (predicate .>> eof) """-Test(x)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPrefixUsage07 () =
        let result = run (predicate .>> eof) """-x"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPrefixUsage07a () =
        let result = run (predicate .>> eof) """- x"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestAnd01 () =
        let result = run (predicate .>> eof) """and (x,y)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestAnd02 () =
        let result = run (predicate .>> eof) """(x and y)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestAnd03 () =
        let result = run (predicate .>> eof) """(x and not x)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))
