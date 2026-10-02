namespace TestFpl1Parser.LowLevel

open FParsec
open Fpl1Parser.Grammar
open Fpl0Base.Primitives
open Microsoft.VisualStudio.TestTools.UnitTesting


[<TestClass>]
type TestCoordPossibilities () =

    [<DataRow("01", """[@1]""")>]
    [<DataRow("02", """[$1]""")>]
    [<DataRow("03", """[xyz]""")>]
    [<DataRow("04", """[x.y().z()]""")>]
    [<DataRow("05", """[self]""")>]
    [<DataRow("06", """[parent]""")>]
    [<DataRow("07", """[PrimPascalCaseId]""")>]
    [<DataRow("08", """[PascalCaseId()]""")>]
    [<DataRow("09", """[PascalCaseId.PascalCaseId()]""")>]
    [<DataRow("10", """[PascalCaseId.PascalCaseId().PascalCaseId()]""")>]
    [<TestMethod>]
    member this.TestCoordinateSuccess (no:string, input:string) =
        let result = run (pCoords .>> eof) input
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))



