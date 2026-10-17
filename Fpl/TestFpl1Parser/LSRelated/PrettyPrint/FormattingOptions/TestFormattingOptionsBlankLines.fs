namespace TestFpl1Parser.LSRelated.PrettyPrint.FormattingOptions

open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl1Parser.LSRelated.FormattingOptions
open TestFpl1Parser.LSRelated.PrettyPrint.Commons

[<TestClass>]
type TestFormattingOptionsIndentSize () =

    [<DataRow(2)>]
    [<DataRow(4)>]
    [<DataRow(8)>]
    [<TestMethod>]
    member _.TestIndentSizeNoSyntaxErrorsIdempotentCommentsPreserved (size: int) =
        let opts = { fplFormatDefaults with IndentSize = size }
        let fplCode = """def class FieldPowerN: Set { ctor FieldPowerN(x:obj, y:pred) { dec base.Obj() ; } property pred T() { true } }"""

        allAssertionsForFormattingOptions opts fplCode

[<TestClass>]
type TestFormattingOptionsMaxLineLength () =

    [<DataRow(40)>]
    [<DataRow(100)>]
    [<DataRow(200)>]
    [<TestMethod>]
    member _.TestMaxLineLengthNoSyntaxErrorsIdempotentCommentsPreserved (maxLen: int) =
        let opts = { fplFormatDefaults with MaxLineLength = maxLen }
        let fplCode = """def class FieldPowerN: Set { ctor FieldPowerN(x:obj, y:pred, z:ind) { dec base.Obj() ; } property pred T() { true } }"""

        allAssertionsForFormattingOptions opts fplCode

[<TestClass>]
type TestFormattingOptionsEmptyLinesAfterBlocks () =

    [<DataRow(0)>]
    [<DataRow(1)>]
    [<DataRow(3)>]
    [<TestMethod>]
    member _.TestEmptyLinesAfterBlocksNoSyntaxErrorsIdempotentCommentsPreserved (n: int) =
        let opts = { fplFormatDefaults with EmptyLinesAfterBlocks = n }
        let fplCode = """def class FieldPowerN: Obj { intr } def pred T1() { true }"""

        allAssertionsForFormattingOptions opts fplCode

[<TestClass>]
type TestFormattingOptionsMaxConsecutiveBlankLines () =

    [<DataRow(0)>]
    [<DataRow(1)>]
    [<DataRow(3)>]
    [<TestMethod>]
    member _.TestMaxConsecutiveBlankLinesNoSyntaxErrorsIdempotentCommentsPreserved (n: int) =
        let opts = { fplFormatDefaults with MaxConsecutiveBlankLines = n }
        let fplCode = """def class FieldPowerN: Obj { intr }


        def pred T1() { true }"""

        allAssertionsForFormattingOptions opts fplCode
