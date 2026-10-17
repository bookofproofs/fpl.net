namespace TestFpl1Parser.LSRelated.PrettyPrint.FormattingOptions

open Fpl1Parser.LSRelated.FormattingOptions
open Microsoft.VisualStudio.TestTools.UnitTesting
open TestFpl1Parser.LSRelated.PrettyPrint.Commons

[<TestClass>]
type TestFormattingOptionsSpacingInsideParentheses () =

    [<DataRow("Yes")>]
    [<DataRow("No")>]
    [<TestMethod>]
    member _.TestSpacingInsideParenthesesNoSyntaxErrorsIdempotentCommentsPreserved (yesNo: string) =
        let value =
            match yesNo with
            | "Yes" -> OptionYesNo.Yes
            | "No" -> OptionYesNo.No
            | other -> failwith $"Unexpected OptionYesNo name: {other}"

        let opts = { fplFormatDefaults with SpacingInsideParentheses = value }
        let fplCode = """def pred T1() { Add(m, k, n) }"""

        allAssertionsForFormattingOptions opts fplCode

[<TestClass>]
type TestFormattingOptionsSpacingBeforeParentheses () =

    [<DataRow("Yes")>]
    [<DataRow("No")>]
    [<TestMethod>]
    member _.TestSpacingBeforeParenthesesNoSyntaxErrorsIdempotentCommentsPreserved (yesNo: string) =
        let value =
            match yesNo with
            | "Yes" -> OptionYesNo.Yes
            | "No" -> OptionYesNo.No
            | other -> failwith $"Unexpected OptionYesNo name: {other}"

        let opts = { fplFormatDefaults with SpacingBeforeParentheses = value }
        let fplCode = """def pred T1() { Add(m, k, n) }"""

        allAssertionsForFormattingOptions opts fplCode

[<TestClass>]
type TestFormattingOptionsSpacingInsideBrackets () =

    [<DataRow("Yes")>]
    [<DataRow("No")>]
    [<TestMethod>]
    member _.TestSpacingInsideBracketsNoSyntaxErrorsIdempotentCommentsPreserved (yesNo: string) =
        let value =
            match yesNo with
            | "Yes" -> OptionYesNo.Yes
            | "No" -> OptionYesNo.No
            | other -> failwith $"Unexpected OptionYesNo name: {other}"

        let opts = { fplFormatDefaults with SpacingInsideBrackets = value }
        let fplCode = """def pred T1() { x[y,z] }"""

        allAssertionsForFormattingOptions opts fplCode

[<TestClass>]
type TestFormattingOptionsSpacingBeforeBrackets () =

    [<DataRow("Yes")>]
    [<DataRow("No")>]
    [<TestMethod>]
    member _.TestSpacingBeforeBracketsNoSyntaxErrorsIdempotentCommentsPreserved (yesNo: string) =
        let value =
            match yesNo with
            | "Yes" -> OptionYesNo.Yes
            | "No" -> OptionYesNo.No
            | other -> failwith $"Unexpected OptionYesNo name: {other}"

        let opts = { fplFormatDefaults with SpacingBeforeBrackets = value }
        let fplCode = """def pred T1() { x[y,z] }"""

        allAssertionsForFormattingOptions opts fplCode

[<TestClass>]
type TestFormattingOptionsSpacingAfterCommas () =

    [<DataRow("Yes")>]
    [<DataRow("No")>]
    [<TestMethod>]
    member _.TestSpacingAfterCommasNoSyntaxErrorsIdempotentCommentsPreserved (yesNo: string) =
        let value =
            match yesNo with
            | "Yes" -> OptionYesNo.Yes
            | "No" -> OptionYesNo.No
            | other -> failwith $"Unexpected OptionYesNo name: {other}"

        let opts = { fplFormatDefaults with SpacingAfterCommas = value }
        let fplCode = """def class TestId { ctor TestId(x:obj, y:pred, z:ind) {} }"""

        allAssertionsForFormattingOptions opts fplCode
