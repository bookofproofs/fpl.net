namespace FplParser.Tests

open FParsec
open FplParser
open Microsoft.VisualStudio.TestTools.UnitTesting


[<TestClass>]
type TestAxioms () =
    let replaceWhiteSpace (input: string) =
        let whiteSpaceChars = [|' '; '\t'; '\n'; '\r'|]
        input.Split(whiteSpaceChars)
            |> String.concat ""

    [<TestMethod>]
    member this.TestAxiom01 () =
        let result = run axiom """axiom ZeroIsNat()
        {
            is(Zero,Nat)
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestAxiom02 () =
        let result = run axiom """axiom SuccessorExistsAndIsUnique()
        {
            dec
                ~ n, successor: Nat
            ;
            all n
            (
                exn$1 successor
                (
                    and
                    (
                        NotEqual(successor,n),
                        Equal(successor,Succ(n))
                    )
                )
            )
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestAxiom03 () =
        let result = run axiom """axiom ZeroIsNotSuccessor()
        {
            dec ~n: Nat;
            all n
            (
                NotEqual(Zero(), Succ(n))
            )
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))



    [<TestMethod>]
    member this.TestAxiom04 () =
        let result = run axiom """axiom SuccessorIsInjective()
        {
            dec ~n,m: Nat;
            all n,m
            (
                impl
                (
                    Equal(Succ(n),Succ(m)),
                    Equal(n,m)
                )
            )
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))


    [<TestMethod>]
    member this.TestAxiom05 () =
        let result = run axiom """axiom CompleteInduction()
        {
            dec 
                ~n: Nat
                ~p: pred
            ;
            all p
            (
                impl
                (
                    and ( p(0), all n ( impl ( p(n), p(Succ(n)) ) ) ),
                    all n ( p(n) )
                )
            )
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))


    [<TestMethod>]
    member this.TestAxiom06 () =
        let result = run axiom """axiom EmptySetExists()
        {
            dec ~x: Set;
            ex x
            (
                IsEmpty(x)
            )
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))


    [<TestMethod>]
    member this.TestAxiom07 () =
        let result = run axiom """axiom Extensionality()
        {
            dec ~x,y: Set ;
            all x,y
            (
                impl
                (
                    and
                    (
                        IsSubset(x,y),
                        IsSubset(y,x)
                    ),
                    Equal(x,y)
                )
            )
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

