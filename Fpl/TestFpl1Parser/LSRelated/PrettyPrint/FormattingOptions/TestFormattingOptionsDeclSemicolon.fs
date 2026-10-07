namespace TestFpl1Parser.LSRelated.PrettyPrint.FormattingOptions

open Fpl1Parser.LSRelated.FormattingOptions
open Microsoft.VisualStudio.TestTools.UnitTesting
open TestFpl1Parser.LSRelated.PrettyPrint.Commons

[<TestClass>]
type TestFormattingOptionsDeclSemicolon () =

    [<DataRow("Compact")>]
    [<DataRow("Enclosing")>]
    [<TestMethod>]
    member _.TestDeclSemicolonNoSyntaxErrorsIdempotentCommentsPreserved (styleName: string) =
        let style =
            match styleName with
            | "Compact" -> BlockStyle.Compact
            | "Enclosing" -> BlockStyle.Enclosing
            | other -> failwith $"Unexpected BlockStyle name: {other}"

        let opts = { fplFormatDefaults with DeclSemicolon = style }
        let fplCode = """def class FieldPowerN: Set { dec x, y: obj a: pred ; intr }"""

        allAssertionsForFormattingOptions opts fplCode
