namespace TestFpl1Parser.LowLevel

open FParsec
open Fpl0Base.Primitives
open Fpl1Parser.Grammar
open Microsoft.VisualStudio.TestTools.UnitTesting


[<TestClass>]
type TestProofRelated () =

    [<DataRow("01", """1. GreaterAB |-""")>]
    [<DataRow("02", """1. PrecedingResults, 1 |-""")>]
    [<DataRow("03", """1. 3, GreaterTransitive  |-""")>]
    [<DataRow("04", """1. 4, byinf ModusPonens |- """)>]
    [<DataRow("05", """1. 1,2,  3 |- """)>]
    [<TestMethod>]
    member this.TestJustificationSuccess (no:string, fplCode:string) =
        let result = run (justification .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("01", """3, GreaterTransitive  |-""")>]
    [<DataRow("02", """4, byinf ModusPonens |- """)>]
    [<DataRow("03", """1,2,  3 |- """)>]
    [<DataRow("04", """PrecedingResults, 1 |-""")>]
    [<DataRow("05", """GreaterAB""")>]
    [<DataRow("06", """|-""")>]
    [<DataRow("07", """ |- """)>]
    [<TestMethod>]
    member this.TestJustificationFailure (no:string, fplCode:string) =
        let result = run (justification .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<DataRow("01", LiteralTrivial)>]
    [<DataRow("02", """and(a,b)""")>]
    [<TestMethod>]
    member this.TestDerivedArgumentSuccess (no:string, fplCode:string) =
        let result = run (derivedArgument .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("01", LiteralQed)>]
    [<DataRow("02", LiteralCon)>]
    [<DataRow("03", LiteralConL)>]
    [<TestMethod>]
    member this.TestDerivedArgumentFailure (no:string, fplCode:string) =
        let result = run (derivedArgument .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<DataRow("01", """trivial""")>]
    [<DataRow("02", """and(a,b)""")>]
    [<TestMethod>]
    member this.TestArgumentInferenceSuccess (no:string, fplCode:string) =
        let result = run (argumentInference .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("01", """qed""")>]
    [<DataRow("02", """con""")>]
    [<DataRow("03", """conclusion""")>]
    [<DataRow("04", """|- revoke 2""")>]
    [<TestMethod>]
    member this.TestArgumentInferenceFailure (no:string, fplCode:string) =
        let result = run (argumentInference .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<DataRow("01", "1: and(a,b)")>]
    [<DataRow("02", "1: revoke 2")>]
    [<DataRow("03", "1. bydef A, 1, C |- revoke 2")>]
    [<DataRow("04", "1. T$1:1 |- revoke 2")>]
    [<DataRow("05", "1. T$1:1, T$1:2, T$1: 3 |- revoke 2")>]
    [<DataRow("06", "1: assume and(a,b)")>]
    [<DataRow("07", "1: assume true")>]
    [<DataRow("08", "1: revoke 2")>]
    [<TestMethod>]
    member this.TestJustifiedArgumentSuccess (no:string, test:string) =
        let result = run (justifiedArgument .>> eof) test
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("01", "1: B, C |- revoke 2")>]
    [<DataRow("02", "1. |- revoke 2")>]
    [<DataRow("03", "1: qed")>]
    [<DataRow("04", "1: |- trivial")>]
    [<DataRow("05", "1. |- trivial")>]
    [<DataRow("06", "2, 3 |- trivial")>]
    [<TestMethod>]
    member this.TestJustifiedArgumentFailure (no:string, test:string) =
        let result = run (justifiedArgument .>> eof) test
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<DataRow(LiteralByCor, "$1", ":1")>]
    [<DataRow(LiteralByDef, "$1", ":1")>]
    [<DataRow(LiteralByAx, "$1", ":1")>]
    [<DataRow(LiteralByInf, "$1", ":1")>]
    [<DataRow(LiteralByCor, "$1", "")>]
    [<DataRow(LiteralByDef, "$1", "")>]
    [<DataRow(LiteralByAx, "$1", "")>]
    [<DataRow(LiteralByInf, "$1", "")>]
    [<DataRow(LiteralByCor, "", ":1")>]
    [<DataRow(LiteralByDef, "", ":1")>]
    [<DataRow(LiteralByAx, "", ":1")>]
    [<DataRow(LiteralByInf, "", "")>]
    [<DataRow(LiteralByCor, "", "")>]
    [<DataRow(LiteralByDef, "", "")>]
    [<DataRow(LiteralByAx, "", "")>]
    [<DataRow(LiteralByInf, "", "")>]
    [<TestMethod>]
    member this.TestJustificationItemSuccess (keyword:string, corRef:string, argRef:string) =
        let result = run (justificationItem .>> eof) $"{keyword} A{corRef}{argRef}"
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("bydef x")>]
    [<TestMethod>]
    member this.TestJustificationItemByDefSuccess (fplCode:string) =
        let result = run (justificationItem .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))
