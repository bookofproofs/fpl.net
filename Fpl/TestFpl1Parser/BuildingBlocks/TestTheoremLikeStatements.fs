namespace TestFpl1Parser.BuildingBlocks

open FParsec
open Fpl1Parser.Grammar
open Microsoft.VisualStudio.TestTools.UnitTesting


[<TestClass>]
type TestTheoremLikeStatements () =

    [<DataRow("01", """proposition SuccessorExistsAndIsUnique { all n:Nat { exn$1 successor:Nat { and ( NotEqual(successor,n), Equal(successor,Succ(n)) ) } } }""")>]
    [<DataRow("02", """prop ZeroIsNat { is(Zero,Nat) }""")>]
    [<TestMethod>]
    member this.TestPropositionSuccess (no:string, fplCode:string) =
        let result = run (proposition .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("01", """thm CompleteInduction { all p:pred { impl ( and ( p(0), all n:Nat { impl ( p(n), p(Succ(n)) ) } ), all n:Nat { p(n) } ) } }""")>]
    [<DataRow("02", """theorem ZeroIsNat { is(Zero,Nat) }""")>]
    [<TestMethod>]
    member this.TestTheoremSuccess (no:string, fplCode:string) =
        let result = run (theorem .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("01", """lem EmptySetExists { ex x:Set { IsEmpty(x) } }""")>]
    [<DataRow("02", """lemma ZeroIsNotSuccessor { all n:Nat { NotEqual(Zero(), Succ(n)) } }""")>]
    [<TestMethod>]
    member this.TestLemmaSuccess (no:string, fplCode:string) =
        let result = run (lemma .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("01", """conjecture SuccessorIsInjective { all n,m: Nat { impl ( Equal(Succ(n),Succ(m)), Equal(n,m) ) } }""")>]
    [<DataRow("02", """conj Extensionality { all x,y: Set { impl ( and ( IsSubset(x,y), IsSubset(y,x) ), Equal(x,y) ) } }""")>]
    [<TestMethod>]
    member this.TestConjectureSuccess (no:string, fplCode:string) =
        let result = run (conjecture .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("01", """corollary SuccessorIsInjective$1 { all n,m: Nat { impl ( Equal(Succ(n),Succ(m)), Equal(n,m) ) } }""")>]
    [<DataRow("02", """cor Extensionality$1 { all x,y:Set { impl ( and ( IsSubset(x,y), IsSubset(y,x) ), Equal(x,y) ) } }""")>]
    [<TestMethod>]
    member this.TestCorollarySuccess (no:string, fplCode:string) =
        let result = run (corollary .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))
