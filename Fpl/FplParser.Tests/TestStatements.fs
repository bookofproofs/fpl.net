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
        let result = run (forStatement .>> eof) """for proceedingResult in    p
                {
                    assert proceedingResult
                    a:=1
                    b:=1
                }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestFor02 () =
        let result = run (forStatement .>> eof) """for    n in Range(1,4)
            {
                assert Equal(f(n),n)
            }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestFor03 () =
        let result = run (forStatement .>> eof) """for n in Range($1,$4)
            {
                assert Equal(f(n),n)
            }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestFor04 () =
        let result = run (forStatement .>> eof) """for n in SomeType
            {
                x[n] := 1
            }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))
        
    [<TestMethod>]
    member this.TestAssignment01 () =
        let result = run (assignmentStatement .>> eof) """a:= 1"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestAssignment02 () =
        let result = run (assignmentStatement .>> eof) """self := Zero()"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestDelegate01 () =
        let result = run (fplDelegate .>> eof) """del.Test(1,2)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestDelegate02 () =
        let result = run (fplDelegate .>> eof) """del.Decrement(x)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))
        
   
    [<TestMethod>]
    member this.TestAssertion01 () =
        let result = run (assertionStatement .>> eof) """assert
                    all n:Set
                    {
                        In(n, self)
                    }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestCases01 () =
        // else case addressed using a python delegate
        let result = run (casesStatement .>> eof) """cases
                (
                    | Equal(x,0) :
                        self := Zero()
                    | Equal(x,1) :
                        self := Succ(Zero())
                    | Equal(x,2) :
                        self := Succ(Succ(Zero()))
                    ? self := Succ(del.Decrement(x))
                )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestCases02 () =
        let result = run (casesStatement .>> eof) """cases
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
        let result = run (casesStatement .>> eof) """cases
                    (
                        | (x = 0) : self := Zero() 
                        | (x = 1) : self := Succ(Zero())
                        | (x = 2) : self := Succ(Succ(Zero()))
                        ? self := Succ(delegate.Decrement(x))  
                    )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestCases04 () =
        let result = run (casesStatement .>> eof) """cases
                    (
                        | IsGreaterOrEqual(x.RightMember(), x.LeftMember()): self:=x.RightMember()
                        ? self:=undefined
                    )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestCases05 () =
        let result = run (casesStatement .>> eof) """cases
            (
                | (m = 0): result:= n
                | (Succ(m) = k): result:= Succ(Add(n,k))
                ? result:= undef
            )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    // new flavour / keyword of cases allowing proedicates to be the result
    [<TestMethod>]
    member this.TestCases06 () =
        let result = run (assignmentStatement .>> eof) """n:=mcases
                (
                    | (x = $1) : false 
                    | (x = $2) : true 
                    | (x = $3) : false 
                    ? undef  
                )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    // new flavour / keyword of cases allowing proedicates to be the result
    [<TestMethod>]
    member this.TestCases07 () =
        let result = run (mapCases .>> eof) """mcases
                (
                    | (x = $1) : false 
                    | (x = $2) : true 
                    | (x = $3) : false 
                    ? undef  
                )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))