namespace TestFpl1Parser.BuildingBlocks

open FParsec
open Fpl1Parser.Grammar
open Microsoft.VisualStudio.TestTools.UnitTesting
type TestProof () =

    [<DataRow("01", """proof Example4$1 {1. GreaterAB |- Greater(a,b) qed}""")>]
    [<DataRow("02", """prf AddIsUnique$1 {1: assume and(x,b) 2: trivial qed}""")>]
    [<DataRow("03", """proof T$1 {1: false ∧ true}""")>]
    [<DataRow("04", """proof T$1 {1. 2 |- false ∧ true}""")>]
    [<DataRow("05", """proof Example4$1 { 1. SomeCorollary$1 |- (a > b) qed }""")>]
    [<TestMethod>]
    member this.TestProofSuccess (no:string, test:string) =
        let result = run (proof .>> eof) test
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("01", """prf AddIsUnique$1 {1: assume pre 2: trivial}""")>]
    [<TestMethod>]
    member this.TestProofFailure (no:string, test:string) =
        let result = run (proof .>> eof) test
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))
