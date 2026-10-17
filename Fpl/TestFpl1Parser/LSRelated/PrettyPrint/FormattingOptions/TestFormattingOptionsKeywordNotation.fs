namespace TestFpl1Parser.LSRelated.PrettyPrint.FormattingOptions

open Fpl1Parser.LSRelated.FormattingOptions
open Microsoft.VisualStudio.TestTools.UnitTesting
open TestFpl1Parser.LSRelated.PrettyPrint.Commons

[<TestClass>]
type TestFormattingOptionsKeywordStyle () =

    [<DataRow("Short")>]
    [<DataRow("Long")>]
    [<TestMethod>]
    member _.TestKeywordStyleNoSyntaxErrorsIdempotentCommentsPreserved (lengthName: string) =
        let value =
            match lengthName with
            | "Short" -> KeywordLength.Short
            | "Long" -> KeywordLength.Long
            | other -> failwith $"Unexpected KeywordLength name: {other}"

        let opts = { fplFormatDefaults with KeywordStyle = value }
        let fplCode = """def class FieldPowerN: Set { ctor FieldPowerN() { dec base.Obj() ; } property pred T() { true } }"""

        allAssertionsForFormattingOptions opts fplCode

