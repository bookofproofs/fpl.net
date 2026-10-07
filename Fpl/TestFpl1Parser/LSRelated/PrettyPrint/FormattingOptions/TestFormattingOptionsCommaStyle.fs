namespace TestFpl1Parser.LSRelated.PrettyPrint.FormattingOptions

open Fpl1Parser.LSRelated.FormattingOptions
open Microsoft.VisualStudio.TestTools.UnitTesting
open TestFpl1Parser.LSRelated.PrettyPrint.Commons

[<TestClass>]
type TestFormattingOptionsParameterStyle () =

    [<DataRow("Auto")>]
    [<DataRow("OneLiner")>]
    [<DataRow("Trailing")>]
    [<DataRow("Leading")>]
    [<TestMethod>]
    member _.TestParameterStyleNoSyntaxErrorsIdempotentCommentsPreserved (styleName: string) =
        let style =
            match styleName with
            | "Auto" -> CommaStyle.Auto
            | "OneLiner" -> CommaStyle.OneLiner
            | "Trailing" -> CommaStyle.Trailing
            | "Leading" -> CommaStyle.Leading
            | other -> failwith $"Unexpected CommaStyle name: {other}"

        let opts = { fplFormatDefaults with ParameterStyle = style }
        let fplCode = """def class TestId { ctor TestId(x:obj, y:pred, z:ind) {} }"""

        allAssertionsForFormattingOptions opts fplCode

[<TestClass>]
type TestFormattingOptionsArgumentStyle () =

    [<DataRow("Auto")>]
    [<DataRow("OneLiner")>]
    [<DataRow("Trailing")>]
    [<DataRow("Leading")>]
    [<TestMethod>]
    member _.TestArgumentStyleNoSyntaxErrorsIdempotentCommentsPreserved (styleName: string) =
        let style =
            match styleName with
            | "Auto" -> CommaStyle.Auto
            | "OneLiner" -> CommaStyle.OneLiner
            | "Trailing" -> CommaStyle.Trailing
            | "Leading" -> CommaStyle.Leading
            | other -> failwith $"Unexpected CommaStyle name: {other}"

        let opts = { fplFormatDefaults with ArgumentStyle = style }
        let fplCode = """def pred T1() { Add(m, k, n) }"""

        allAssertionsForFormattingOptions opts fplCode
