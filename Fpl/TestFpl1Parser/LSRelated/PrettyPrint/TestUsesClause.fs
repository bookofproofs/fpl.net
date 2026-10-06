namespace TestFpl1Parser.LSRelated.PrettyPrint

open Fpl1Parser.Grammar
open Microsoft.VisualStudio.TestTools.UnitTesting
open TestFpl1Parser.LSRelated.PrettyPrint.Commons

[<TestClass>]
type TestUsesClause () =

    [<DataRow("01", """uses TestNamespace""")>]
    [<DataRow("02", """uses Fpl.Commons""")>]
    [<DataRow("03", """uses TestNamespace1.TestNamespace2""")>]
    [<DataRow("04", """uses TestNamespace *""")>]
    [<DataRow("05", """uses TestNamespace1.TestNamespace2 *""")>]
    [<DataRow("06", """uses TestNamespace alias T1""")>]
    [<DataRow("07", """uses TestNamespace1.TestNamespace2 alias T2""")>]
    [<TestMethod>]
    member _.TestUsesClauseSyntaxErrorFreeInput (no: string, fplCode: string) =
        allAssertionsSyntaxErrorFreeInputWithoutComments usesClause fplCode

    [<DataRow("00", """uses Fpl.Test.fpl""")>]
    [<DataRow("01", """uses Fpl.Test*.fpl""")>]
    [<TestMethod>]
    member this.TestUsesClauseSyntaxErrorInput (no:string, fplCode:string) =
        allAssertionsForSyntaxErrorInput fplCode
