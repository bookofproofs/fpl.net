namespace TestFpl1Parser.LSRelated.PrettyPrint

open System
open FParsec
open Fpl1Parser.Types
open Fpl1Parser.LSRelated.CommentLexer
open Fpl1Parser.LSRelated.TriviaMap
open Fpl1Parser.LSRelated.Doc
open Fpl1Parser.LSRelated.FormattingOptions
open Fpl1Parser.LSRelated.PrettyPrint
open Microsoft.VisualStudio.TestTools.UnitTesting

/// <summary>
/// Shared pipeline helpers for pretty-print tests, covering the following test cases: 
/// <item> 
/// 1a: Per-Ast-node test: no syntax error introduced by reformatting
/// </item>
/// <item> 
/// 1b: Per-Ast-node test: idempotency
/// </item>
/// <item> 
/// 1b: Per-Ast-node test: comment-preservation / placement
/// </item>
/// <item> 
/// 2a - 2c: Same as 1a - 1c, but for Per-formattingOptions-field Tests
/// </item>
/// </summary>
/// <remarks>
/// Two independent pipelines are exposed, matching the two situations a pretty-print test can be in:
/// <list type="bullet">
/// <item>
/// <description>
/// <b>Individual-parser pipeline</b> (<see cref="parseOrFail"/>/<see cref="printNodeViaParser"/>/
/// <see cref="assertRoundTripsWithoutSyntaxErrors"/>/<see cref="assertIdempotentViaParser"/>): runs a
/// single <c>Fpl1Parser.Grammar</c> production (e.g. <c>pascalCaseId</c>, <c>axiom</c>) directly on a
/// comment-free snippet and calls <c>PrettyPrint.print</c> on the resulting node. Suitable **only**
/// for 1a (no new syntax errors) and 1b (idempotency), since individual grammar productions have no
/// notion of comments and will simply fail to parse any input containing <c>//</c> or <c>/* */</c>.
/// </description>
/// </item>
/// <item>
/// <description>
/// <b>Full-pipeline</b> (<see cref="formatViaFplParser"/>/<see cref="assertIdempotentViaFplParser"/>/
/// comment-placement assertions): runs <c>Fpl1Parser.Main.fplParser</c> (which strips comments via
/// <c>removeFplComments</c> before parsing) and <c>PrettyPrint.printAll</c>. This is the **only**
/// viable pipeline for 1c/2c (comment preservation/placement), since trivia attachment depends on
/// comments having been discovered independently (via <c>CommentLexer.findComments</c>, run on the
/// *original*, un-stripped source) and merged back in via <c>TriviaMap</c>.
/// </description>
/// </item>
/// </list>
/// </remarks>
module Commons =

    // ========================================================================
    // Individual-parser pipeline — for 1a/1b/2a/2b only (comment-free snippets).
    // ========================================================================

    /// <summary>Runs <paramref name="parser"/> on <paramref name="fplCode"/>, asserting success, and returns the <c>Ast</c>.</summary>
    let private parseOrFail (parser: Parser<Ast, unit>) (fplCode: string) : Ast =
        match run (parser .>> eof) fplCode with
        | Success(ast, _, _) -> ast
        | Failure(msg, _, _) -> failwith $"Expected a successful parse of '{fplCode}' but got: {msg}"

    /// <summary>Prints an already-parsed single node with no trivia attached (individual-parser pipeline never has comments).</summary>
    let private printAstWith (opts: FormattingOptions) (ast: Ast) : string =
        let map = TriviaMap()
        print opts map ast
        |> render opts.IndentSize opts.MaxLineLength

    /// <summary>Parses <paramref name="fplCode"/> with <paramref name="parser"/> and renders it via <c>print</c>, using <paramref name="opts"/>.</summary>
    let private printNodeViaParserWith (opts: FormattingOptions) (parser: Parser<Ast, unit>) (fplCode: string) : string =
        parseOrFail parser fplCode |> printAstWith opts

    /// <summary>Parses <paramref name="fplCode"/> with <paramref name="parser"/> and renders it via <c>print</c>, using <c>fplFormatDefaults</c>.</summary>
    let private printNodeViaParser (parser: Parser<Ast, unit>) (fplCode: string) : string =
        printNodeViaParserWith fplFormatDefaults parser fplCode

    /// <summary>
    /// Category 1a/2a (individual-parser variant): re-parses the rendered output with the same
    /// <paramref name="parser"/> and asserts it still succeeds.
    /// </summary>
    let private assertRoundTripsWithoutSyntaxErrorsWith (opts: FormattingOptions) (parser: Parser<Ast, unit>) (fplCode: string) =
        let rendered = printNodeViaParserWith opts parser fplCode
        match run (parser .>> eof) rendered with
        | Success _ -> ()
        | Failure(msg, _, _) ->
            Assert.Fail($"Reformatted output failed to re-parse: {msg}{Environment.NewLine}--- rendered ---{Environment.NewLine}{rendered}")

    /// <summary>Category 1a (individual-parser variant) using <c>fplFormatDefaults</c>.</summary>
    let private assertRoundTripsWithoutSyntaxErrors (parser: Parser<Ast, unit>) (fplCode: string) =
        assertRoundTripsWithoutSyntaxErrorsWith fplFormatDefaults parser fplCode

    /// <summary>
    /// Category 1b/2b (individual-parser variant): format → parse-the-output → format again; the two
    /// renderings must match.
    /// </summary>
    let private assertIdempotentWith (opts: FormattingOptions) (parser: Parser<Ast, unit>) (fplCode: string) =
        let firstPass = printNodeViaParserWith opts parser fplCode
        let secondPass = printNodeViaParserWith opts parser firstPass
        Assert.AreEqual(firstPass, secondPass, "Expected pretty-printing to be idempotent after one reformat.")

    /// <summary>Category 1b (individual-parser variant) using <c>fplFormatDefaults</c>.</summary>
    let private assertIdempotent (parser: Parser<Ast, unit>) (fplCode: string) =
        assertIdempotentWith fplFormatDefaults parser fplCode

    // ========================================================================
    // Full-pipeline (fplParser + printAll) — usable for 1a/1b/2a/2b, and REQUIRED for 1c/2c.
    // ========================================================================

    /// <summary>
    /// Runs <c>Fpl1Parser.Main.fplParser</c> on <paramref name="fplCode"/>, builds the <c>TriviaMap</c>
    /// from the *original* (un-stripped) <paramref name="fplCode"/>, and renders via <c>printAll</c>.
    /// </summary>
    /// <returns>The rendered text, plus the clean-parse success flag returned by <c>fplParser</c>.</returns>
    let private formatViaFplParserWith (opts: FormattingOptions) (fplCode: string) : string * bool =
        let asts, success = Fpl1Parser.Main.fplParser fplCode
        let comments = findComments fplCode
        let positions = asts |> List.collect Fpl1Parser.LSRelated.Trivia.getAllPositions
        let map = buildTriviaMap positions comments
        printAll opts map asts, success

    /// <summary>Full pipeline using <c>fplFormatDefaults</c>.</summary>
    let private formatViaFplParser (fplCode: string) : string * bool =
        formatViaFplParserWith fplFormatDefaults fplCode

    /// <summary>True if any top-level ast produced by <c>fplParser</c> is a syntax-error placeholder.</summary>
    let private isErrorAst (ast: Ast) =
        match ast with
        | ErrorSyntax _ | ErrorSyntaxBacktracking _ | ErrorSyntaxChain _ -> true
        | _ -> false

    /// <summary>Category 1b/2b (full-pipeline variant): format → format-again; the two outputs must match.</summary>
    let private assertFullPipelineIdempotent (fplCode: string) =
        let firstPass, _ = formatViaFplParser fplCode
        let secondPass, _ = formatViaFplParser firstPass
        Assert.AreEqual(firstPass, secondPass, "Expected printAll to be idempotent after one reformat.")

    /// <summary>
    /// Category 1c' (syntax-error variant): asserts that any comment present inside
    /// <paramref name="fplCode"/> survives unchanged in the <c>printAll</c> output, because syntax-error
    /// building blocks are no longer reformatted via <c>TriviaMap</c>/<c>withTrivia</c> attachment —
    /// they are reprinted verbatim from the original source (see <c>Ast.ErrorSyntax</c>/
    /// <c>ErrorSyntaxBacktracking</c>/<c>ErrorSyntaxChain</c>'s verbatim field and
    /// <c>PrettyPrint.print</c>'s dedicated cases for them). Comment *placement* relative to an
    /// anchor is therefore not meaningful for syntax-error input (nothing is re-laid-out at all);
    /// the only applicable guarantee is that the comment text itself is neither dropped nor altered.
    /// </summary>
    let assertCommentVerbatimForSyntaxErrorInput (commentText: string) (fplCode: string) =
        let rendered, success = formatViaFplParser fplCode
        Assert.IsFalse(success, $"Expected '{fplCode}' to contain a syntax error for this assertion to be meaningful.")
        Assert.IsTrue(
            rendered.Contains(commentText: string),
            $"Expected comment '{commentText}' to be preserved verbatim in the reformatted output of syntax-error input, but got:{Environment.NewLine}{rendered}")

    /// <summary>Splits rendered text into lines using the same newline convention <c>Doc.render</c> uses.</summary>
    let private toLines (rendered: string) : string[] =
        rendered.Split([| Environment.NewLine |], StringSplitOptions.None)

    /// <summary>Runs all tests assertions for syntax-free input without comments (used for individual parsers to avoid duplicating the same DataRow test for each test separately).</summary>
    let allAssertionsSyntaxErrorFreeInputWithoutComments (parser: Parser<Ast, unit>) (fplCode: string) =
        // 1a test: no syntax errors introduced by reformatting
        assertRoundTripsWithoutSyntaxErrors parser fplCode
        // 1b test: idempotency test for syntax-error-free input
        assertIdempotent parser fplCode

    /// <summary>Runs all tests assertions for syntax-error input (used to avoid duplicating the same DataRow test in different unit tests).</summary>
    /// <remarks>
    /// Does NOT assert comment placement: syntax-error building blocks are reprinted verbatim from
    /// the original source (see <c>Ast.ErrorSyntax</c>/<c>ErrorSyntaxBacktracking</c>/
    /// <c>ErrorSyntaxChain</c> and their dedicated cases in <c>PrettyPrint.print</c>), so there is no
    /// TriviaMap-driven re-layout for comment-placement assertions to meaningfully exercise.
    /// </remarks>
    let allAssertionsForSyntaxErrorInput (fplCode: string) =
        // 1b test: idempotency test for syntax-error input
        assertFullPipelineIdempotent fplCode


    /// <summary>Category 1c/2c (opts-parameterized): asserts leading-comment adjacency using <paramref name="opts"/> instead of <c>fplFormatDefaults</c>.</summary>
    let private assertLeadingCommentAdjacentWith (opts: FormattingOptions) (fplCode: string) (commentText: string) (anchorText: string) =
        let rendered, _ = formatViaFplParserWith opts fplCode
        let lines = toLines rendered
        match lines |> Array.tryFindIndex (fun l -> l.Contains(anchorText: string)) with
        | None -> Assert.Fail($"Expected rendered output to contain anchor '{anchorText}' but got:{Environment.NewLine}{rendered}")
        | Some 0 -> Assert.Fail($"Expected a preceding line for leading comment '{commentText}' but anchor was on the first line.")
        | Some i ->
            Assert.IsTrue(
                lines.[i - 1].Contains(commentText: string),
                $"Expected line immediately preceding anchor '{anchorText}' to contain leading comment '{commentText}', but it was: '{lines.[i - 1]}'{Environment.NewLine}--- full rendered ---{Environment.NewLine}{rendered}")

    /// <summary>Category 1c/2c (opts-parameterized): asserts trailing-comment placement using <paramref name="opts"/> instead of <c>fplFormatDefaults</c>.</summary>
    let private assertTrailingCommentSameLineWith (opts: FormattingOptions) (fplCode: string) (anchorText: string) (commentText: string) =
        let rendered, _ = formatViaFplParserWith opts fplCode
        let lines = toLines rendered
        match lines |> Array.tryFindIndex (fun l -> l.Contains(anchorText: string)) with
        | None -> Assert.Fail($"Expected rendered output to contain anchor '{anchorText}' but got:{Environment.NewLine}{rendered}")
        | Some i ->
            let line = lines.[i]
            let anchorPos = line.IndexOf(anchorText: string)
            let commentPos = line.IndexOf(commentText: string)
            Assert.IsTrue(
                commentPos > anchorPos,
                $"Expected trailing comment '{commentText}' to appear after anchor '{anchorText}' on the same rendered line, but line was: '{line}'{Environment.NewLine}--- full rendered ---{Environment.NewLine}{rendered}")

    /// <summary>Category 2c: comment preservation / placement for a given <paramref name="opts"/>.</summary>
    let private assertCommentPreservationPlacementWith (opts: FormattingOptions) (fplCode: string) =
        let anchorText = "/*anchor*/"
        let withLeadingComment = sprintf "// leading comment%s%s%s" Environment.NewLine anchorText fplCode
        assertLeadingCommentAdjacentWith opts withLeadingComment "// leading comment" anchorText

        let withTrailingComment = sprintf "%s %s // trailing comment" fplCode anchorText
        assertTrailingCommentSameLineWith opts withTrailingComment anchorText "// trailing comment"


    /// <summary>Category 2b (full-pipeline variant, opts-parameterized): format → format-again using <paramref name="opts"/>; the two outputs must match.</summary>
    let private assertFullPipelineIdempotentWith (opts: FormattingOptions) (fplCode: string) =
        let firstPass, _ = formatViaFplParserWith opts fplCode
        let secondPass, _ = formatViaFplParserWith opts firstPass
        Assert.AreEqual(firstPass, secondPass, "Expected printAll to be idempotent after one reformat (with custom FormattingOptions).")

    /// <summary>
    /// Category 2a (full-pipeline variant): formats <paramref name="fplCode"/> under <paramref name="opts"/>,
    /// then re-runs the full pipeline on the *rendered* output and asserts it still parses cleanly
    /// (i.e. reformatting did not introduce any new syntax errors).
    /// </summary>
    let private assertFullPipelineRoundTripsWithoutSyntaxErrorsWith (opts: FormattingOptions) (fplCode: string) =
        let rendered, originalSuccess = formatViaFplParserWith opts fplCode
        Assert.IsTrue(originalSuccess, $"Expected clean parse for '{fplCode}' prior to asserting 2a/2b/2c under custom FormattingOptions.")
        let _, reparseSuccess = formatViaFplParserWith opts rendered
        Assert.IsTrue(
            reparseSuccess,
            $"Expected reformatted output to be free of syntax errors under custom FormattingOptions:{Environment.NewLine}{rendered}")

    /// <summary>Runs all 2a/2b/2c assertions for a representative snippet under a given non-default <paramref name="opts"/>.</summary>
    let allAssertionsForFormattingOptions (opts: FormattingOptions) (fplCode: string) =
        // 2a test: no syntax errors introduced by reformatting under custom opts
        assertFullPipelineRoundTripsWithoutSyntaxErrorsWith opts fplCode
        // 2b test: idempotency under custom opts
        assertFullPipelineIdempotentWith opts fplCode
        // 2c test: comment preservation / placement under custom opts
        assertCommentPreservationPlacementWith opts fplCode


    /// <summary>
    /// Category 1c'' (syntax-error variant): asserts that each of <paramref name="expectedFragments"/>
    /// appears <em>exactly once</em> in the <c>printAll</c> output for <paramref name="fplCode"/>, and
    /// that <paramref name="fplCode"/> indeed contains a syntax error (so the assertion is meaningful).
    /// </summary>
    /// <remarks>
    /// Regression guard for the chunked error-recovery logic in <c>Fpl1Parser.Main</c>: a
    /// successfully-parsed prefix of a chunk is committed as its own <c>BuildingBlock</c> AST node,
    /// while the rest of the chunk is captured verbatim on the resulting error node. Miscomputing the
    /// boundary between the two can either duplicate the prefix (it appears both as a real
    /// <c>BuildingBlock</c> and again inside the error node's verbatim text) or truncate the start of
    /// the error node's verbatim text (dropping leading characters of the unconsumed suffix). Plain
    /// idempotency assertions do not catch either defect, since a duplicated/truncated rendering can
    /// still be stable under a second reformat. This helper instead checks each expected fragment's
    /// occurrence count directly against the rendered output.
    /// </remarks>
    let assertFragmentsAppearExactlyOnceForSyntaxErrorInput (expectedFragments: string list) (fplCode: string) =
        let rendered, success = formatViaFplParser fplCode
        Assert.IsFalse(success, $"Expected '{fplCode}' to contain a syntax error for this assertion to be meaningful.")
        for fragment in expectedFragments do
            let occurrences =
                if fragment = "" then 0
                else
                    let mutable count = 0
                    let mutable idx = rendered.IndexOf(fragment: string)
                    while idx >= 0 do
                        count <- count + 1
                        idx <- rendered.IndexOf(fragment, idx + 1)
                    count
            Assert.AreEqual(
                1, occurrences,
                $"Expected fragment '{fragment}' to appear exactly once in the reformatted output of syntax-error input, but it appeared {occurrences} time(s). Rendered:{Environment.NewLine}{rendered}")

    /// <summary>
    /// Category 1a' (individual-parser variant): asserts that every one of <paramref name="expectedFragments"/>
    /// appears, verbatim, somewhere in the rendered output of <paramref name="fplCode"/> via <paramref name="parser"/>.
    /// </summary>
    /// <remarks>
    /// Regression guard for missing-separator defects (e.g. a mapped return type printed directly
    /// adjacent to a following <c>postfix</c>/<c>prefix</c>/<c>infix</c> declaration with no space in
    /// between, such as <c>"Natinfix"</c>). A bare idempotency assertion cannot catch this class of
    /// bug, since a wrongly-concatenated rendering can still be stable under a second reformat; this
    /// helper instead checks that expected word-boundaries/separators actually produced the intended
    /// substrings.
    /// </remarks>
    let assertFragmentsPresent (parser: Parser<Ast, unit>) (expectedFragments: string list) (fplCode: string) =
        let rendered = printNodeViaParser parser fplCode
        for fragment in expectedFragments do
            Assert.IsTrue(
                rendered.Contains(fragment: string),
                $"Expected fragment '{fragment}' to appear in the reformatted output, but got:{Environment.NewLine}{rendered}")

