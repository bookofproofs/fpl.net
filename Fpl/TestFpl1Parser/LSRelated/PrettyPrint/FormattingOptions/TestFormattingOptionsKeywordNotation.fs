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

[<TestClass>]
type TestFormattingOptionsCompoundPredicateStyle () =

    [<DataRow("Keyword")>]
    [<DataRow("Symbol")>]
    [<TestMethod>]
    member _.TestCompoundPredicateStyleNoSyntaxErrorsIdempotentCommentsPreserved (notationName: string) =
        let value =
            match notationName with
            | "Keyword" -> Notation.Keyword
            | "Symbol" -> Notation.Symbol
            | other -> failwith $"Unexpected Notation name: {other}"

        let opts = { fplFormatDefaults with CompoundPredicateStyle = value }
        let fplCode = """def pred T1() { and(x,b) }"""

        allAssertionsForFormattingOptions opts fplCode

[<TestClass>]
type TestFormattingOptionsOperatorStyle () =

    [<DataRow("Keyword")>]
    [<DataRow("Symbol")>]
    [<TestMethod>]
    member _.TestOperatorStyleNoSyntaxErrorsIdempotentCommentsPreserved (notationName: string) =
        let value =
            match notationName with
            | "Keyword" -> Notation.Keyword
            | "Symbol" -> Notation.Symbol
            | other -> failwith $"Unexpected Notation name: {other}"

        let opts = { fplFormatDefaults with OperatorStyle = value }
        let fplCode = """def pred T1() { and(x,b) }"""

        allAssertionsForFormattingOptions opts fplCode

[<TestClass>]
type TestFormattingOptionsIsOperator () =

    [<DataRow("Infix")>]
    [<DataRow("Polish")>]
    [<TestMethod>]
    member _.TestIsOperatorNoSyntaxErrorsIdempotentCommentsPreserved (styleName: string) =
        let value =
            match styleName with
            | "Infix" -> IsOpStyle.Infix
            | "Polish" -> IsOpStyle.Polish
            | other -> failwith $"Unexpected IsOpStyle name: {other}"

        let opts = { fplFormatDefaults with IsOperator = value }
        let fplCode = """def pred T1() { dec x: obj ; is(x, Nat) }"""

        allAssertionsForFormattingOptions opts fplCode
