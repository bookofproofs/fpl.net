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
        tokenizer.ParsedTokens.Clear()
        let result = run (axiom .>> eof) """axiom ZeroIsNat
        {
            is(Zero,Nat)
        }"""
        let actual = sprintf "%O" result
        printf "%s" (tokenizer.ParsedTokens |> Seq.map (fun t -> t.Name + t.EndPos.ToString()) |> String.concat ", ")
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestAxiom02 () =
        let result = run (axiom .>> eof) """axiom SuccessorExistsAndIsUnique
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
    member this.TestAxiom03 () =
        let result = run (axiom .>> eof) """axiom ZeroIsNotSuccessor
        {
            all n: Nat
            {
                NotEqual(Zero(), Succ(n))
            }
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))



    [<TestMethod>]
    member this.TestAxiom04 () =
        let result = run (axiom .>> eof) """axiom SuccessorIsInjective
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
    member this.TestAxiom05 () =
        let result = run (axiom .>> eof) """axiom CompleteInduction
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
    member this.TestAxiom06 () =
        let result = run (axiom .>> eof) """axiom EmptySetExists
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
    member this.TestAxiom07 () =
        let result = run (axiom .>> eof) """axiom Extensionality
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
    member this.TestAxiom08 () =
        let result = run (axiom .>> eof) """axiom TestAxiom
        {
            true
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestAxiom09 () =
        let result = run (axiom .>> eof) """axiom A { all x:Nat {true} }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))
        

    [<TestMethod>]
    member this.TestAxiom10 () =
        let result = run (axiom .>> eof) """axiom TestId {true}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))
        

    [<TestMethod>]
    member this.TestAxiom11 () =
        let result = run (axiom .>> eof) """ax T {exn$1 x:obj {del.Equal(x,$1)}}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))
        