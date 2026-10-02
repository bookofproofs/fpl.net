namespace TestFpl1Parser.LowLevel

open FParsec
open Fpl1Parser.Grammar
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestInfixPostfixPrefix () =

    [<DataRow("01", """x'""")>]
    [<DataRow("02", """x''""")>]
    [<DataRow("03", """x(i)'""")>]
    [<DataRow("04", """x[i]'""")>]
    [<DataRow("05", """x.SomeProperty()'""")>]
    [<DataRow("06", """1'""")>]
    [<DataRow("07", """-f(x)'""")>]
    [<DataRow("08", """-f((x + 1))!""")>]
    [<DataRow("09", """f(x)!""")>]
    [<DataRow("10", """-x!""")>]
    [<DataRow("11", """x'""")>]
    [<DataRow("12", """(x')'""")>]
    [<DataRow("13", """x''""")>]
    [<DataRow("14", """(x + y)""")>]
    [<DataRow("15", """(x + y + z)""")>]
    [<DataRow("16", """((x + y) + z)""")>]
    [<DataRow("17", """(x + (y + z))""")>]
    [<DataRow("18", """(x ∈ z)""")>]
    [<DataRow("19", """(x ∧ z)""")>]
    [<DataRow("20", """(x ∈ z)""")>]
    [<DataRow("21", """(x' + y'' < z)""")>]
    [<DataRow("22", """(-x' + -y'' < -z)""")>]
    [<DataRow("23", """(x' + (y'' < z))""")>]
    [<DataRow("24", """((x' + y'') < z)""")>]
    [<DataRow("25", """(-x' + -y'' + z)""")>]
    [<DataRow("26", """-(x + -y)'""")>]
    [<DataRow("27", """-(x + y)""")>]
    [<DataRow("28", """(f(x) -∘ g(x))""")>]
    [<DataRow("29", """(f(x)' + g(x)')""")>]
    [<DataRow("30", """(f(x) + g(x))'""")>]
    [<DataRow("31", """f(x)'""")>]
    [<DataRow("32", """(x + y)""")>]
    [<DataRow("33", """x + y""")>]
    [<DataRow("34", """(f + -g)""")>]
    [<DataRow("35", """(f + -g)""")>]
    [<DataRow("36", """'x""")>]
    [<DataRow("37", """''x""")>]
    [<DataRow("38", """'x(i)""")>]
    [<DataRow("39", """'x[i]""")>]
    [<DataRow("40", """'x.SomeProperty()""")>]
    [<DataRow("41", """'1""")>]
    [<DataRow("42", """-(x)""")>]
    [<DataRow("43", """-Test(x)""")>]
    [<DataRow("44", """-x'""")>]
    [<DataRow("45", """-x""")>]
    [<DataRow("46", """and (x,y)""")>]
    [<DataRow("47", """(x ∧ y)""")>]
    [<DataRow("48", """(x ∧ not x)""")>]
    [<DataRow("49", """-x""")>]
    [<DataRow("50", """x'""")>]
    [<DataRow("51", """(x + y)""")>]
    [<DataRow("52", """(x + y = 1)""")>]
    [<DataRow("53", """(x = y + 1)""")>]
    [<DataRow("54", """x""")>]
    [<TestMethod>]
    member this.TestInfixPostfixPrefixSuccess (no:string, fplCode:string) =
        let result = run (expression .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("01", """x'(i)'""")>]
    [<DataRow("02", """x '""")>]
    [<DataRow("03", """-(x + y).Test()""")>]
    [<DataRow("04", """(f -∘ g)(x)""")>]
    [<DataRow("05", """(f + g)'(x)""")>]
    [<DataRow("06", """f'(x)""")>]
    [<TestMethod>]
    member this.TestInfixPostfixPrefixFailure (no:string, fplCode:string) =
        let result = run (expression .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))


