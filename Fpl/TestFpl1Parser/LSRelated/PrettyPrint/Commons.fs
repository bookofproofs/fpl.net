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
/// Shared pipeline helpers for pretty-print tests.
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

    /// <summary>Runs <paramref name="parser"/> on <paramref name="code"/>, asserting success, and returns the <c>Ast</c>.</summary>
    let parseOrFail (parser: Parser<Ast, unit>) (code: string) : Ast =
        match run (parser .>> eof) code with
        | Success(ast, _, _) -> ast
        | Failure(msg, _, _) -> failwith $"Expected a successful parse of '{code}' but got: {msg}"

    /// <summary>Prints an already-parsed single node with no trivia attached (individual-parser pipeline never has comments).</summary>
    let private printAstWith (opts: FormattingOptions) (ast: Ast) : string =
        let map = TriviaMap()
        print opts map ast
        |> render opts.IndentSize opts.MaxLineLength

    /// <summary>Parses <paramref name="code"/> with <paramref name="parser"/> and renders it via <c>print</c>, using <paramref name="opts"/>.</summary>
    let printNodeViaParserWith (opts: FormattingOptions) (parser: Parser<Ast, unit>) (code: string) : string =
        parseOrFail parser code |> printAstWith opts

    /// <summary>Parses <paramref name="code"/> with <paramref name="parser"/> and renders it via <c>print</c>, using <c>fplFormatDefaults</c>.</summary>
    let printNodeViaParser (parser: Parser<Ast, unit>) (code: string) : string =
        printNodeViaParserWith fplFormatDefaults parser code

    /// <summary>
    /// Category 1a/2a (individual-parser variant): re-parses the rendered output with the same
    /// <paramref name="parser"/> and asserts it still succeeds.
    /// </summary>
    let assertRoundTripsWithoutSyntaxErrorsWith (opts: FormattingOptions) (parser: Parser<Ast, unit>) (code: string) =
        let rendered = printNodeViaParserWith opts parser code
        match run (parser .>> eof) rendered with
        | Success _ -> ()
        | Failure(msg, _, _) ->
            Assert.Fail($"Reformatted output failed to re-parse: {msg}{Environment.NewLine}--- rendered ---{Environment.NewLine}{rendered}")

    /// <summary>Category 1a (individual-parser variant) using <c>fplFormatDefaults</c>.</summary>
    let assertRoundTripsWithoutSyntaxErrors (parser: Parser<Ast, unit>) (code: string) =
        assertRoundTripsWithoutSyntaxErrorsWith fplFormatDefaults parser code

    /// <summary>
    /// Category 1b/2b (individual-parser variant): format → parse-the-output → format again; the two
    /// renderings must match.
    /// </summary>
    let assertIdempotentWith (opts: FormattingOptions) (parser: Parser<Ast, unit>) (code: string) =
        let firstPass = printNodeViaParserWith opts parser code
        let secondPass = printNodeViaParserWith opts parser firstPass
        Assert.AreEqual(firstPass, secondPass, "Expected pretty-printing to be idempotent after one reformat.")

    /// <summary>Category 1b (individual-parser variant) using <c>fplFormatDefaults</c>.</summary>
    let assertIdempotent (parser: Parser<Ast, unit>) (code: string) =
        assertIdempotentWith fplFormatDefaults parser code

    // ========================================================================
    // Full-pipeline (fplParser + printAll) — usable for 1a/1b/2a/2b, and REQUIRED for 1c/2c.
    // ========================================================================

    /// <summary>
    /// Runs <c>Fpl1Parser.Main.fplParser</c> on <paramref name="code"/>, builds the <c>TriviaMap</c>
    /// from the *original* (un-stripped) <paramref name="code"/>, and renders via <c>printAll</c>.
    /// </summary>
    /// <returns>The rendered text, plus the clean-parse success flag returned by <c>fplParser</c>.</returns>
    let formatViaFplParserWith (opts: FormattingOptions) (code: string) : string * bool =
        let asts, success = Fpl1Parser.Main.fplParser code
        let comments = findComments code
        let positions = asts |> List.collect Fpl1Parser.LSRelated.Trivia.getAllPositions
        let map = buildTriviaMap positions comments
        printAll opts map asts, success

    /// <summary>Full pipeline using <c>fplFormatDefaults</c>.</summary>
    let formatViaFplParser (code: string) : string * bool =
        formatViaFplParserWith fplFormatDefaults code

    /// <summary>True if any top-level ast produced by <c>fplParser</c> is a syntax-error placeholder.</summary>
    let private isErrorAst (ast: Ast) =
        match ast with
        | ErrorSyntax _ | ErrorSyntaxBacktracking _ | ErrorSyntaxChain _ -> true
        | _ -> false

    /// <summary>
    /// Category 1a/2a (full-pipeline variant): formats <paramref name="code"/> via <c>fplParser</c> +
    /// <c>printAll</c>, then re-parses the *output* and asserts a clean parse (no error-recovery
    /// placeholders).
    /// </summary>
    let assertFullPipelineRoundTripsWithoutSyntaxErrors (code: string) =
        let formatted, _ = formatViaFplParser code
        let asts, success = Fpl1Parser.Main.fplParser formatted
        Assert.IsTrue(success, $"Expected a clean re-parse of the reformatted output but got error recovery:{Environment.NewLine}{formatted}")
        Assert.IsFalse(asts |> List.exists isErrorAst, $"Expected no error-syntax nodes in the re-parsed, reformatted output:{Environment.NewLine}{formatted}")

    /// <summary>Category 1b/2b (full-pipeline variant): format → format-again; the two outputs must match.</summary>
    let assertFullPipelineIdempotent (code: string) =
        let firstPass, _ = formatViaFplParser code
        let secondPass, _ = formatViaFplParser firstPass
        Assert.AreEqual(firstPass, secondPass, "Expected printAll to be idempotent after one reformat.")

    /// <summary>Splits rendered text into lines using the same newline convention <c>Doc.render</c> uses.</summary>
    let private toLines (rendered: string) : string[] =
        rendered.Split([| Environment.NewLine |], StringSplitOptions.None)

    /// <summary>
    /// Category 1c/2c: asserts that <paramref name="commentText"/> appears on the line immediately
    /// preceding the first line containing <paramref name="anchorText"/> in the <c>printAll</c> output
    /// of <paramref name="code"/> (i.e. attached as leading trivia of the node rendering <paramref name="anchorText"/>).
    /// </summary>
    let assertLeadingCommentAdjacent (code: string) (commentText: string) (anchorText: string) =
        let rendered, _ = formatViaFplParser code
        let lines = toLines rendered
        match lines |> Array.tryFindIndex (fun l -> l.Contains(anchorText: string)) with
        | None -> Assert.Fail($"Expected rendered output to contain anchor '{anchorText}' but got:{Environment.NewLine}{rendered}")
        | Some 0 -> Assert.Fail($"Expected a preceding line for leading comment '{commentText}' but anchor was on the first line.")
        | Some i ->
            Assert.IsTrue(
                lines.[i - 1].Contains(commentText: string),
                $"Expected line immediately preceding anchor '{anchorText}' to contain leading comment '{commentText}', but it was: '{lines.[i - 1]}'{Environment.NewLine}--- full rendered ---{Environment.NewLine}{rendered}")

    /// <summary>
    /// Category 1c/2c: asserts that <paramref name="commentText"/> appears on the same rendered line
    /// as, and after, <paramref name="anchorText"/> in the <c>printAll</c> output of <paramref name="code"/>
    /// (i.e. attached as trailing trivia of the node rendering <paramref name="anchorText"/>).
    /// </summary>
    let assertTrailingCommentSameLine (code: string) (anchorText: string) (commentText: string) =
        let rendered, _ = formatViaFplParser code
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
