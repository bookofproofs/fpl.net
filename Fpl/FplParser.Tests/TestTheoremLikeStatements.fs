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
        let result = run (proposition .>> eof) """proposition SuccessorExistsAndIsUnique
        {
            all n:Nat
            {
                exn$1 successor:Nat
                {
                    and
                    (
                        NotEqual(successor,n),
                        Equal(successor,Succ(n))
                    )
                }
            }
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestTheorem01 () =
        let result = run (theorem .>> eof) """thm CompleteInduction
        {
            all p:pred
            {
                impl
                (
                    and ( p(0), all n:Nat { impl ( p(n), p(Succ(n)) ) } ),
                    all n:Nat { p(n) }
                )
            }
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))


    [<TestMethod>]
    member this.TestTheorem02 () =
        let result = run (theorem .>> eof) """theorem ZeroIsNat
        {
            is(Zero,Nat)
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestLemma01 () =
        let result = run (lemma .>> eof) """lem EmptySetExists
        {
            ex x:Set
            {
                IsEmpty(x)
            }
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))


    [<TestMethod>]
    member this.TestLemma02 () =
        let result = run (lemma .>> eof) """lemma ZeroIsNotSuccessor
        {
            all n:Nat
            {
                NotEqual(Zero(), Succ(n))
            }
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestConjecture01 () =
        let result = run (conjecture .>> eof) """conjecture SuccessorIsInjective
        {
            all n,m: Nat
            {
                impl
                (
                    Equal(Succ(n),Succ(m)),
                    Equal(n,m)
                )
            }
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestConjecture02 () =
        let result = run (conjecture .>> eof) """conj Extensionality
        {
            all x,y: Set
            {
                impl
                (
                    and
                    (
                        IsSubset(x,y),
                        IsSubset(y,x)
                    ),
                    Equal(x,y)
                )
            }
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestCorollary01 () =
        let result = run (corollary .>> eof) """corollary SuccessorIsInjective$1()
        {
            all n,m: Nat
            {
                impl
                (
                    Equal(Succ(n),Succ(m)),
                    Equal(n,m)
                )
            }
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestCorollary02 () =
        let result = run (corollary .>> eof) """cor Extensionality$1()
        {
            all x,y:Set
            {
                impl
                (
                    and
                    (
                        IsSubset(x,y),
                        IsSubset(y,x)
                    ),
                    Equal(x,y)
                )
            }
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))