namespace TestFpl1Parser.LSRelated.PrettyPrint

open Fpl1Parser.Grammar
open Microsoft.VisualStudio.TestTools.UnitTesting
open TestFpl1Parser.LSRelated.PrettyPrint.Commons

[<TestClass>]
type TestTheoremLikeStatements () =

    [<DataRow("01", """proposition SuccessorExistsAndIsUnique { all n:Nat { exn$1 successor:Nat { and ( NotEqual(successor,n), Equal(successor,Succ(n)) ) } } }""")>]
    [<DataRow("02", """prop ZeroIsNat { is(Zero,Nat) }""")>]
    [<TestMethod>]
    member _.TestPropositionSyntaxErrorFreeInput (no: string, fplCode: string) =
        allAssertionsSyntaxErrorFreeInputWithoutComments proposition fplCode

    [<DataRow("01", """thm CompleteInduction { all p:pred { impl ( and ( p(0), all n:Nat { impl ( p(n), p(Succ(n)) ) } ), all n:Nat { p(n) } ) } }""")>]
    [<DataRow("02", """theorem ZeroIsNat { is(Zero,Nat) }""")>]
    [<TestMethod>]
    member _.TestTheoremSyntaxErrorFreeInput (no: string, fplCode: string) =
        allAssertionsSyntaxErrorFreeInputWithoutComments theorem fplCode

    [<DataRow("01", """lem EmptySetExists { ex x:Set { IsEmpty(x) } }""")>]
    [<DataRow("02", """lemma ZeroIsNotSuccessor { all n:Nat { NotEqual(Zero(), Succ(n)) } }""")>]
    [<TestMethod>]
    member _.TestLemmaSyntaxErrorFreeInput (no: string, fplCode: string) =
        allAssertionsSyntaxErrorFreeInputWithoutComments lemma fplCode

    [<DataRow("01", """conjecture SuccessorIsInjective { all n,m: Nat { impl ( Equal(Succ(n),Succ(m)), Equal(n,m) ) } }""")>]
    [<DataRow("02", """conj Extensionality { all x,y: Set { impl ( and ( IsSubset(x,y), IsSubset(y,x) ), Equal(x,y) ) } }""")>]
    [<TestMethod>]
    member _.TestConjectureSyntaxErrorFreeInput (no: string, fplCode: string) =
        allAssertionsSyntaxErrorFreeInputWithoutComments conjecture fplCode

    [<DataRow("01", """corollary SuccessorIsInjective$1 { all n,m: Nat { impl ( Equal(Succ(n),Succ(m)), Equal(n,m) ) } }""")>]
    [<DataRow("02", """cor Extensionality$1 { all x,y:Set { impl ( and ( IsSubset(x,y), IsSubset(y,x) ), Equal(x,y) ) } }""")>]
    [<TestMethod>]
    member _.TestCorollarySyntaxErrorFreeInput (no: string, fplCode: string) =
        allAssertionsSyntaxErrorFreeInputWithoutComments corollary fplCode

