namespace TestFpl1Parser.LSRelated.PrettyPrint

open Fpl1Parser.Grammar
open Microsoft.VisualStudio.TestTools.UnitTesting
open TestFpl1Parser.LSRelated.PrettyPrint.Commons

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
    member _.TestPCoordsForSyntaxErrorFreeInput (no: string, fplCode: string) =
        allAssertionsForSyntaxErrorFreeInputWithoutComments bracketedCoords fplCode

