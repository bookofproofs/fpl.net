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
    member this.TestRange01 () =
        let result = run forStatement """for proceedingResult in p
                (
                    assert proceedingResult
                    a:=1
                    b:=1
                )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestRange02 () =
        let result = run forStatement """for i in [1~n]
                (
                    self<i>:=field.AdditiveGroup().NeutralElement()
                )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestLoop01 () =
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
    member this.TestLoop02 () =
        let result = run forStatement """for    n in[1~4]
            (
            assert Equal(f(n),n)
            )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestLoop03 () =
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
                    ;
                    else
                        // else case addressed using a python delegate
                        self := Succ(del.decrement(x))
                )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))
