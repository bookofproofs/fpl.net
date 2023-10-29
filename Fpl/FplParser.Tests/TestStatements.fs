namespace FplParser.Tests

open FParsec
open FplParser
open Microsoft.VisualStudio.TestTools.UnitTesting


[<TestClass>]
type TestStatements () =
    let replaceWhiteSpace (input: string) =
        let whiteSpaceChars = [|' '; '\t'; '\n'; '\r'|]
        input.Split(whiteSpaceChars)
            |> String.concat ""

    [<TestMethod>]
    member this.TestFor01 () =
        let result = run forStatement """for proceedingResult in    p
                (
                    assert proceedingResult
                    a:=1
                    b:=1
                )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestFor02 () =
        let result = run forStatement """for    n in [1~4]
            (
            assert Equal(f(n),n)
            )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestFor03 () =
        let result = run forStatement """for n in [!1~!4]
            (
            assert Equal(f(n),n)
            )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestAssignment01 () =
        let result = run assignmentStatement """a:= 1"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestAssignment02 () =
        let result = run assignmentStatement """self := Zero()"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestDelegate01 () =
        let result = run fplDelegate """del.test(1,2)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestDelegate02 () =
        let result = run fplDelegate """del.decrement(x)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))
        
   
    [<TestMethod>]
    member this.TestAssertion01 () =
        let result = run assertionStatement """assert
                    all n
                    (
                        and
                        (
                            is(n, Set),
                            In(n, self)
                        )
                    )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestCases01 () =
        let result = run casesStatement """cases
                (
                    | Equal(x,0) :
                        self := Zero()
                    | Equal(x,1) :
                        self := Succ(Zero())
                    | Equal(x,2) :
                        self := Succ(Succ(Zero()))
                    ? // else case addressed using a python delegate
                        self := Succ(del.decrement(x))
                )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestCases02 () =
        let result = run casesStatement """cases
                (
                    | Equal(n,0): result := m.NeutralElem()
                    ? result :=
                            op(
                                y,
                                Exp( m(y,op), y, Sub(n,1))
                            )
                )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestCases03 () =
        let result = run casesStatement """cases
                    (
                        | <x = 0> : self := Zero() 
                        | <x = 1> : self := Succ(Zero())
                        | <x = 2> : self := Succ(Succ(Zero()))
                        ? self := Succ(delegate.decrement(x))  
                    )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestCases04 () =
        let result = run casesStatement """cases
                    (
                        | IsGreaterOrEqual(x.RightMember(), x.LeftMember()): self:=x.RightMember()
                        ? self:=undefined
                    )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestCases05 () =
        let result = run casesStatement """cases
            (
                | <m = 0>: result:= n
                | <Succ(m) = k>: result:= Succ(Add(n,k))
                ? result:= undef
            )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))



