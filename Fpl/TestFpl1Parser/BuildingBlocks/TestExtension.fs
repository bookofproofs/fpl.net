namespace TestFpl1Parser.BuildingBlocks

open FParsec
open Fpl1Parser.Grammar
open Microsoft.VisualStudio.TestTools.UnitTesting


[<TestClass>]
type TestExtension () =

    [<DataRow("01", """ext Digits x@/\d+/ -> A {return x}""")>]
    [<DataRow("02", """ext Alpha y@/[a-z]+/ -> A {return y}""")>]
    [<DataRow("03", """ext T z@/ / -> S {return z}""")>]
    [<DataRow("04", """ext Digits x@/\d+/ -> obj {ret x}""")>]
    [<DataRow("05", """extension Digits x@/\d+/ -> S {return x}""")>]
    [<DataRow("06", """extension Alpha x@/[a-z]+/ -> T {return x}""")>]
    [<TestMethod>]
    member this.TestExtensionSuccess (no:string, ext:string) =
        let result = run (definitionExtension .>> eof) ext
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("01", """ext Digits: x:=// {return x}""")>]
    [<DataRow("02", """ext Alpha: x:=/[a-z]+ {return x}""")>]
    [<DataRow("03", """ext Alpha: x:=[a-z]+/ {return x}""")>]
    [<TestMethod>]
    member this.TestExtensionFailure (no:string, ext:string) =
        let result = run (definitionExtension .>> eof) ext
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))


