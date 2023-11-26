namespace FplParser.Tests

open FParsec
open FplParser
open Microsoft.VisualStudio.TestTools.UnitTesting


[<TestClass>]
type TestTheoremLikeStatements () =
    let replaceWhiteSpace (input: string) =
        let whiteSpaceChars = [|' '; '\t'; '\n'; '\r'|]
        input.Split(whiteSpaceChars)
            |> String.concat ""

    [<TestMethod>]
    member this.TestProposition01 () =
        let result = run (proposition .>> eof) """proposition SuccessorExistsAndIsUnique()
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
    member this.TestTheorem01 () =
        let result = run (theorem .>> eof) """thm CompleteInduction()
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
    member this.TestTheorem02 () =
        let result = run (theorem .>> eof) """theorem ZeroIsNat()
        {
            is(Zero,Nat)
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestLemma01 () =
        let result = run (lemma .>> eof) """lem EmptySetExists()
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
    member this.TestLemma02 () =
        let result = run (lemma .>> eof) """lemma ZeroIsNotSuccessor()
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
    member this.TestConjecture01 () =
        let result = run (conjecture .>> eof) """conjecture SuccessorIsInjective()
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
    member this.TestConjecture02 () =
        let result = run (conjecture .>> eof) """conj Extensionality()
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

    [<TestMethod>]
    member this.TestCorollary01 () =
        let result = run (corollary .>> eof) """corollary SuccessorIsInjective! $1()
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
    member this.TestCorollary02 () =
        let result = run (corollary .>> eof) """cor Extensionality!$1()
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