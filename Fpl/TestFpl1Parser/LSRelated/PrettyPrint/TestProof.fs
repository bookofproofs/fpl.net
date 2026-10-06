namespace TestFpl1Parser.LSRelated.PrettyPrint

open Fpl1Parser.Grammar
open Microsoft.VisualStudio.TestTools.UnitTesting
open TestFpl1Parser.LSRelated.PrettyPrint.Commons

[<TestClass>]
type TestProof () =

    [<DataRow("01", """proof Example4$1 {1. GreaterAB |- Greater(a,b) qed}""")>]
    [<DataRow("02", """prf AddIsUnique$1 {1: assume and(x,b) 2: trivial qed}""")>]
    [<DataRow("03", """proof T$1 {1: false ∧ true}""")>]
    [<DataRow("04", """proof T$1 {1. 2 |- false ∧ true}""")>]
    [<DataRow("05", """proof Example4$1 { 1. SomeCorollary$1 |- (a > b) qed }""")>]
    [<TestMethod>]
    member _.TestProofSyntaxErrorFreeInput (no: string, fplCode: string) =
        allAssertionsSyntaxErrorFreeInputWithoutComments proof fplCode

