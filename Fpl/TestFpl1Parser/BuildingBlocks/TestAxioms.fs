namespace TestFpl1Parser.BuildingBlocks

open FParsec
open Fpl1Parser.Grammar
open Microsoft.VisualStudio.TestTools.UnitTesting


[<TestClass>]
type TestAxioms () =

    [<DataRow("01", """ax ZeroIsNat { is(Zero,Nat) }""")>]
    [<DataRow("02", """axiom SuccessorExistsAndIsUnique { all n:Nat { exn$1 successor:Nat { and ( NotEqual(successor,n), Equal(successor,Succ(n)) ) } } }""")>]
    [<DataRow("03", """axiom ZeroIsNotSuccessor { all n: Nat { NotEqual(Zero(), Succ(n)) } }""")>]
    [<DataRow("04", """axiom SuccessorIsInjective { all n,m: Nat { impl ( Equal(Succ(n),Succ(m)), Equal(n,m) ) } }""")>]
    [<DataRow("05", """ax CompleteInduction { all p:pred { impl ( and ( p(0), all n:Nat { impl ( p(n), p(Succ(n)) ) } ), all n:Nat { p(n) } ) } }""")>]
    [<DataRow("06", """axiom EmptySetExists { ex x:Set { IsEmpty(x) } }""")>]
    [<DataRow("07", """ax Extensionality { all x,y: Set { impl ( and ( IsSubset(x,y), IsSubset(y,x) ), Equal(x,y) ) } }""")>]
    [<DataRow("08", """axiom TestAxiom { true }""")>]
    [<DataRow("09", """axiom A { all x:Nat {true} }""")>]
    [<DataRow("10", """axiom TestId {true}""")>]
    [<DataRow("11", """ax T {exn$1 x:obj {del.Equal(x,$1)}}""")>]
    [<TestMethod>]
    member this.TestAxiomSuccess (no:string, fplCode:string) =
        let result = run (axiom .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

