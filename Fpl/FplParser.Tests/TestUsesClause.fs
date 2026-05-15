namespace FplParser.Tests

open FParsec
open FplParser
open Microsoft.VisualStudio.TestTools.UnitTesting


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
    member this.TestUsesClauseSuccess (no:string, fplCode:string) =
        let result = run (usesClause .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("00", """uses Fpl.Test.fpl""")>]
    [<DataRow("01", """uses Fpl.Test*.fpl""")>]
    [<TestMethod>]
    member this.TestUsesClause03 (no:string, fplCode:string) =
        let result = run (usesClause .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))


