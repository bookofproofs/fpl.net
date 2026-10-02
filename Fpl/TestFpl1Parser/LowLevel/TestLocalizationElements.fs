namespace TestFpl1Parser.LowLevel

open FParsec
open Fpl1Parser.Grammar
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestLocalizationElements () =

    [<DataRow("01", """x "\Leftrightarrow" y """)>]
    [<DataRow("02", """"\neg(" x ")" """)>]
    [<TestMethod>]
    member this.TestEbnfTermSuccess (no:string, input:string) =
        let result = run (ebnfTerm .>> eof) input
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("01", """x "\Leftrightarrow" y """)>]
    [<DataRow("02", """"\neg(" x ")" """)>]
    [<DataRow("03", """x "\Leftrightarrow" y | x "\Rightarrow" y """)>]
    [<DataRow("04", """"\neg(" x ")" | x "\Rightarrow" y """)>]
    [<TestMethod>]
    member this.TestEbnfTranslSuccess (no:string, input:string) =
        let result = run (ebnfTransl .>> eof) input
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("01", """!tex: x "\Leftrightarrow" y """)>]
    [<DataRow("02", """!tex: x "\Leftrightarrow" y | x "\Rightarrow" y """)>]
    [<TestMethod>]
    member this.TestLanguageSuccess (no:string, input:string) =
        let result = run (language .>> eof) input
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

