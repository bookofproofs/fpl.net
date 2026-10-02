namespace TestFpl1Parser.LSRelated.PrettyPrint

open Fpl1Parser.Grammar
open Microsoft.VisualStudio.TestTools.UnitTesting
open TestFpl1Parser.LSRelated.PrettyPrint.Commons

/// <summary>
/// Category 1a/1b/1c tests against <c>FormattingOptions.fplFormDefaults</c> for the simplest
/// position-bearing leaf parsers: identifiers, boolean/undef literals, and short-spelling keywords,
/// exercised directly through their individual <c>Fpl1Parser.Grammar</c> productions rather than
/// through a full-document parse.
/// </summary>
[<TestClass>]
type TestLexicalAndKeywords () =

    // ------------------------------------------------------------------
    // 1a: no syntax errors introduced by reformatting
    // ------------------------------------------------------------------

    [<DataRow("SomeClass")>]
    [<DataRow("Nat")>]
    [<DataRow("X")>]
    [<TestMethod>]
    member _.PascalCaseIdRoundTrips (code: string) =
        assertReformattedCausesNoSyntaxErrors pascalCaseId code

    [<DataRow("true")>]
    [<DataRow("false")>]
    [<DataRow("undef")>]
    [<DataRow("self")>]
    [<TestMethod>]
    member _.TrueRoundTrips (code: string) =
        assertReformattedCausesNoSyntaxErrors predicate code

    // ------------------------------------------------------------------
    // 1b: idempotency
    // ------------------------------------------------------------------

    [<DataRow("SomeClass")>]
    [<DataRow("Nat")>]
    [<DataRow("X")>]
    [<TestMethod>]
    member _.PascalCaseIdIdempotent (code: string) =
        assertIdempotent pascalCaseId code

    [<DataRow("true")>]
    [<DataRow("false")>]
    [<DataRow("undef")>]
    [<DataRow("self")>]
    [<TestMethod>]
    member _.PredicateLiteralsIdempotent (code: string) =
        assertIdempotent predicate code

    // ------------------------------------------------------------------
    // 1c: comment preservation/placement (line-adjacency, not just Contains)
    // ------------------------------------------------------------------

    [<TestMethod>]
    member _.LeadingLineCommentAttachedDirectlyAboveNode () =
        let code = "// a leading comment\ntrue"
        assertLeadingCommentAdjacent predicate code "// a leading comment" "true"

    [<TestMethod>]
    member _.LeadingBlockCommentAttachedDirectlyAboveNode () =
        let code = "/* a block comment */\ntrue"
        assertLeadingCommentAdjacent predicate code "/* a block comment */" "true"

    [<TestMethod>]
    member _.TrailingLineCommentStaysOnSameLineAsNode () =
        let code = "true // trailing comment"
        assertTrailingCommentSameLine predicate code "true" "// trailing comment"

    [<TestMethod>]
    member _.TrailingBlockCommentStaysOnSameLineAsNode () =
        let code = "true /* trailing block comment */"
        assertTrailingCommentSameLine predicate code "true" "/* trailing block comment */"
