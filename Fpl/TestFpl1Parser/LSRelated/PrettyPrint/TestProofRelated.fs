namespace TestFpl1Parser.LSRelated.PrettyPrint

open Fpl1Parser.Grammar
open Fpl0Base.Primitives
open Microsoft.VisualStudio.TestTools.UnitTesting
open TestFpl1Parser.LSRelated.PrettyPrint.Commons

[<TestClass>]
type TestProofRelated() =

    [<DataRow("01", """1. GreaterAB |-""")>]
    [<DataRow("02", """1. PrecedingResults, 1 |-""")>]
    [<DataRow("03", """1. 3, GreaterTransitive  |-""")>]
    [<DataRow("04", """1. 4, byinf ModusPonens |- """)>]
    [<DataRow("05", """1. 1,2,  3 |- """)>]
    [<DataRow("06", """1: """)>]
    [<TestMethod>]
    member _.TestJustificationSyntaxErrorFreeInput (no: string, fplCode: string) =
        allAssertionsSyntaxErrorFreeInputWithoutComments justification fplCode

    [<DataRow("01", LiteralTrivial)>]
    [<DataRow("02", """and(a,b)""")>]
    [<TestMethod>]
    member _.TestDerivedArgumentSyntaxErrorFreeInput (no: string, fplCode: string) =
        allAssertionsSyntaxErrorFreeInputWithoutComments derivedArgument fplCode

    [<DataRow("01", LiteralTrivial)>]
    [<DataRow("02", """and(a,b)""")>]
    [<TestMethod>]
    member _.TestArgumentInferenceSyntaxErrorFreeInput (no: string, fplCode: string) =
        allAssertionsSyntaxErrorFreeInputWithoutComments argumentInference fplCode

    [<DataRow("01", "1: and(a,b)")>]
    [<DataRow("02", "1: revoke 2")>]
    [<DataRow("03", "1. bydef A, 1, C |- revoke 2")>]
    [<DataRow("04", "1. T$1:1 |- revoke 2")>]
    [<DataRow("05", "1. T$1:1, T$1:2, T$1: 3 |- revoke 2")>]
    [<DataRow("06", "1: assume and(a,b)")>]
    [<DataRow("07", "1: assume true")>]
    [<DataRow("08", "1: revoke 2")>]
    [<TestMethod>]
    member _.TestJustifiedArgumentSyntaxErrorFreeInput (no: string, fplCode: string) =
        allAssertionsSyntaxErrorFreeInputWithoutComments justifiedArgument fplCode

    [<DataRow("00", LiteralByCor, "$1", ":1")>]
    [<DataRow("01", LiteralByDef, "$1", ":1")>]
    [<DataRow("02", LiteralByAx, "$1", ":1")>]
    [<DataRow("03", LiteralByInf, "$1", ":1")>]
    [<DataRow("04", LiteralByCor, "$1", "")>]
    [<DataRow("05", LiteralByDef, "$1", "")>]
    [<DataRow("06", LiteralByAx, "$1", "")>]
    [<DataRow("07", LiteralByInf, "$1", "")>]
    [<DataRow("08", LiteralByCor, "", ":1")>]
    [<DataRow("09", LiteralByDef, "", ":1")>]
    [<DataRow("10", LiteralByAx, "", ":1")>]
    [<DataRow("11", LiteralByInf, "", "")>]
    [<DataRow("12", LiteralByCor, "", "")>]
    [<DataRow("13", LiteralByDef, "", "")>]
    [<DataRow("14", LiteralByAx, "", "")>]
    [<DataRow("15", LiteralByInf, "", "")>]
    [<TestMethod>]
    member _.TestJustificationItemSyntaxErrorFreeInput (no: string, keyword:string, corRef:string, argRef:string) =
        allAssertionsSyntaxErrorFreeInputWithoutComments justificationItem $"{keyword} A{corRef}{argRef}"

    [<DataRow("00", "bydef x")>]
    [<TestMethod>]
    member _.TestJustificationItemByDefSyntaxErrorFreeInput (no: string, fplCode: string) =
        allAssertionsSyntaxErrorFreeInputWithoutComments justificationItem fplCode

    [<DataRow("01", """1. GreaterAB |-""", "1. GreaterAB |-")>]
    [<DataRow("02", """1. PrecedingResults, 1 |-""", "1. PrecedingResults, 1 |-")>]
    [<TestMethod>]
    member _.TestJustificationPreservesTurnstileAndSpacing (no: string, fplCode: string, expectedFragment: string) =
        assertFragmentsPresent justification [ expectedFragment ] fplCode
