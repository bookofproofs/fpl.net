namespace TestFpl1Parser.LowLevel

open FParsec
open Fpl1Parser.Grammar
open Microsoft.VisualStudio.TestTools.UnitTesting


[<TestClass>]
type TestPredicateWithQualification () =

    [<DataRow("01", """Xx""")>]
    [<DataRow("02", """Xx.Xx""")>]
    [<DataRow("03", """Xx.Xx.Xx""")>]
    [<DataRow("04", """Xx()""")>]
    [<DataRow("05", """Xx.Xx()""")>]
    [<DataRow("06", """Xx.Xx.Xx()""")>]
    [<DataRow("07", """Xx().Yy""")>]
    [<DataRow("08", """Xx.Xx().Yy""")>]
    [<DataRow("09", """Xx.Xx.Xx().Yy""")>]
    [<DataRow("10", """Xx().Yy.Zz""")>]
    [<DataRow("11", """Xx.Xx().Yy.Zz""")>]
    [<DataRow("12", """Xx.Xx.Xx().Yy.Zz""")>]
    [<DataRow("13", """Xx().Yy().Zz()""")>]
    [<DataRow("14", """Xx[Xx.Xx]""")>]
    [<DataRow("15", """Xx[Xx.Xx()]""")>]
    [<DataRow("16", """Xx[Xx()].Yy""")>]
    [<DataRow("17", """Xx.Xx[Yy]""")>]
    [<DataRow("18", """Xx[Xx[Xx().Yy]]""")>]
    [<DataRow("19", """Xx[Yy().Zz]""")>]
    [<DataRow("20", """Xx[Xx[Yy().Zz]]""")>]
    [<DataRow("21", """Xx[Xx[Xx()].Yy[Zz]]""")>]
    [<DataRow("22", """Xx[Yy()].Zz()""")>]
    [<TestMethod>]
    member this.TestPredicateWithQualificationSuccess (no:string, input:string) =
        let result = run (predicateWithQualification .>> eof) input
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))
