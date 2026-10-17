namespace TestFpl1Parser.LSRelated.PrettyPrint.FormattingOptions

open Fpl1Parser.LSRelated.FormattingOptions
open Microsoft.VisualStudio.TestTools.UnitTesting
open TestFpl1Parser.LSRelated.PrettyPrint.Commons

[<TestClass>]
type TestFormattingOptionsBraceStyle () =

    [<DataRow("Auto")>]
    [<DataRow("OneLiner")>]
    [<DataRow("Egyptian")>]
    [<DataRow("Allman")>]
    [<TestMethod>]
    member _.TestBraceStyleNoSyntaxErrorsIdempotentCommentsPreserved (styleName: string) =
        let style =
            match styleName with
            | "Auto" -> OpeningStyle.Auto
            | "OneLiner" -> OpeningStyle.OneLiner
            | "Egyptian" -> OpeningStyle.Egyptian
            | "Allman" -> OpeningStyle.Allman
            | other -> failwith $"Unexpected OpeningStyle name: {other}"

        let opts = { fplFormatDefaults with BraceStyle = style }
        let fplCode = """def class FieldPowerN: Set { ctor FieldPowerN(x:obj, y:pred) { dec base.Obj() ; } property pred T() { true } }"""

        allAssertionsForFormattingOptions opts fplCode

[<TestClass>]
type TestFormattingOptionsParenthesesStyle () =

    [<DataRow("Auto")>]
    [<DataRow("OneLiner")>]
    [<DataRow("Egyptian")>]
    [<DataRow("Allman")>]
    [<TestMethod>]
    member _.TestParenthesesStyleNoSyntaxErrorsIdempotentCommentsPreserved (styleName: string) =
        let style =
            match styleName with
            | "Auto" -> OpeningStyle.Auto
            | "OneLiner" -> OpeningStyle.OneLiner
            | "Egyptian" -> OpeningStyle.Egyptian
            | "Allman" -> OpeningStyle.Allman
            | other -> failwith $"Unexpected OpeningStyle name: {other}"

        let opts = { fplFormatDefaults with ParenthesesStyle = style }
        let fplCode = """def class FieldPowerN: Set { ctor FieldPowerN(x:obj, y:pred) { dec base.Obj() ; } property pred T() { true } }"""

        allAssertionsForFormattingOptions opts fplCode
