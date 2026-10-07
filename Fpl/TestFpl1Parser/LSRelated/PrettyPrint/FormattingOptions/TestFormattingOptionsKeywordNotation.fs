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

    /// <summary>
    /// NOT YET IMPLEMENTABLE: <c>OperatorStyle</c> governs how calls to a <em>user-defined</em>
    /// infix/prefix/postfix operator (declared via the optional <c>userDefinedSymbol</c> in
    /// <c>simpleSignature</c>, see <c>Grammar.fs</c>'s <c>predicateSignature</c>/
    /// <c>functionalTermSignature</c>) should be rendered — e.g. whether a call written as
    /// <c>SomeInfixOp(x, y)</c> should print back as <c>SomeInfixOp(x, y)</c> (keyword/call form) or
    /// as <c>x SomeInfixOp y</c> (its declared infix symbol form). Resolving this requires knowing,
    /// for a given <c>PascalCaseId</c> reference, whether it was declared with an infix/prefix/postfix
    /// symbol and what that symbol is — information that lives in the <em>interpreter's symbol
    /// table</em>, not in the bare AST that <c>PrettyPrint.print</c> operates on. Supporting this
    /// option therefore requires <c>PrettyPrint</c> to build its own identifier-to-symbol map (by
    /// scanning all <c>simpleSignature</c> declarations up front, independently of the interpreter)
    /// before it can decide, at each call site, which spelling to print. Until that pre-pass exists,
    /// a 2a/2b/2c test here would pass vacuously, since the option has no observable effect on output.
    /// Replace this placeholder once the identifier-to-symbol map and the corresponding
    /// <c>operatorNotation</c> call sites are implemented in <c>PrettyPrint.print</c>.
    /// </summary>
    [<TestMethod>]
    member _.TestOperatorStyleNotYetImplementable () =
        Assert.Inconclusive(
            "OperatorStyle requires PrettyPrint to build its own identifier-to-symbol map from " +
            "simpleSignature's userDefinedSymbol declarations, since fixity information lives in the " +
            "interpreter's symbol table, not the bare AST. Revisit this test once that pre-pass exists.")

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
