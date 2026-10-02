namespace TestFpl1Parser.LSRelated.PrettyPrint

open System
open FParsec
open Fpl1Parser.Types
open Fpl1Parser.LSRelated.CommentLexer
open Fpl1Parser.LSRelated.Trivia
open Fpl1Parser.LSRelated.TriviaMap
open Fpl1Parser.LSRelated.Doc
open Fpl1Parser.LSRelated.FormattingOptions
open Fpl1Parser.LSRelated.PrettyPrint
open Microsoft.VisualStudio.TestTools.UnitTesting

/// <summary>
/// Shared pipeline helpers for per-node pretty-print tests: given a single leaf/compound FPL
/// grammar parser (e.g. <c>Fpl1Parser.Grammar.pascalCaseId</c>, <c>theorem</c>,
/// <c>definitionClass</c>, ...), runs it on a source snippet, builds the <c>TriviaMap</c> for just
/// that snippet, calls <c>PrettyPrint.print</c> directly on the resulting <c>Ast</c> node (now
/// public), and renders the result — without ever going through <c>printAll</c> or
/// <c>Fpl1Parser.Main.fplParser</c>'s full-document/error-recovery path.
/// </summary>
module Commons =

    /// <summary>
    /// Runs <paramref name="parser"/> on <paramref name="code"/>, asserting the parse succeeds, and
    /// returns the resulting <c>Ast</c> node.
    /// </summary>
    let parseOrFail (parser: Parser<Ast, unit>) (code: string) : Ast =
        match run (parser .>> eof) code with
        | Success(ast, _, _) -> ast
        | Failure(msg, _, _) -> failwith $"Expected a successful parse of '{code}' but got: {msg}"

    /// <summary>
    /// Builds the <c>TriviaMap</c> for a single already-parsed <paramref name="ast"/> node, scoped
    /// to the comments found in <paramref name="code"/> (the exact source the node was parsed from).
    /// </summary>
    let buildTrivia (code: string) (ast: Ast) : TriviaMap =
        let comments = findComments code
        let positions = getAllPositions ast
        buildTriviaMap positions comments

    /// <summary>
    /// Renders <paramref name="ast"/> (parsed from <paramref name="code"/>) via <c>print</c> and
    /// <c>render</c> directly, using <paramref name="opts"/>.
    /// </summary>
    let printNodeWith (opts: FormattingOptions) (parser: Parser<Ast, unit>) (code: string) : string =
        let ast = parseOrFail parser code
        let map = buildTrivia code ast
        print opts map ast
        |> render opts.IndentSize opts.MaxLineLength

    /// <summary>Renders a single node using <c>fplFormDefaults</c>.</summary>
    let printNode (parser: Parser<Ast, unit>) (code: string) : string =
        printNodeWith fplFormatDefaults parser code

    /// <summary>Category 1a: re-parses the rendered output with the same <paramref name="parser"/>
    /// and asserts it still succeeds — i.e. reformatting introduces no syntax errors.</summary>
    let assertReformattedCausesNoSyntaxErrors (parser: Parser<Ast, unit>) (code: string) =
        match run (parser .>> eof) code with
        | Success _ -> ()
        | Failure(msg, _, _) ->
            Assert.Fail($"Test input code failed to parse: {msg}{Environment.NewLine}--- original ---{Environment.NewLine}{code}")
        let rendered = printNode parser code
        match run (parser .>> eof) rendered with
        | Success _ -> ()
        | Failure(msg, _, _) ->
            Assert.Fail($"Reformatted output failed to re-parse: {msg}{Environment.NewLine}--- rendered ---{Environment.NewLine}{rendered}")

    /// <summary>Category 1b: format → parse-the-output → format again; the two renderings must match.</summary>
    let assertIdempotent (parser: Parser<Ast, unit>) (code: string) =
        let firstPass = printNode parser code
        let reparsed = parseOrFail parser firstPass
        let map2 = buildTrivia firstPass reparsed
        let secondPass = print fplFormatDefaults map2 reparsed |> render fplFormatDefaults.IndentSize fplFormatDefaults.MaxLineLength
        Assert.AreEqual(firstPass, secondPass, "Expected pretty-printing to be idempotent after one reformat.")

    /// <summary>Splits rendered text into lines using the same newline convention <c>Doc.render</c> uses.</summary>
    let private toLines (rendered: string) : string[] =
        rendered.Split([| Environment.NewLine |], StringSplitOptions.None)

    /// <summary>
    /// Category 1c: asserts that <paramref name="commentText"/> appears on the line immediately
    /// preceding the first line containing <paramref name="anchorText"/> (i.e. as leading trivia
    /// directly attached to the node whose rendering contains <paramref name="anchorText"/>).
    /// </summary>
    let assertLeadingCommentAdjacent (parser: Parser<Ast, unit>) (code: string) (commentText: string) (anchorText: string) =
        let rendered = printNode parser code
        let lines = toLines rendered
        let anchorIdx = lines |> Array.tryFindIndex (fun l -> l.Contains(anchorText: string))
        match anchorIdx with
        | None -> Assert.Fail($"Expected rendered output to contain anchor '{anchorText}' but got:{Environment.NewLine}{rendered}")
        | Some i when i = 0 -> Assert.Fail($"Expected a preceding line for leading comment '{commentText}' but anchor was on the first line.")
        | Some i ->
            Assert.IsTrue(
                lines.[i - 1].Contains(commentText: string),
                $"Expected line immediately preceding anchor '{anchorText}' to contain leading comment '{commentText}', but it was: '{lines.[i - 1]}'{Environment.NewLine}--- full rendered ---{Environment.NewLine}{rendered}")

    /// <summary>
    /// Category 1c: asserts that <paramref name="commentText"/> appears on the same rendered line as
    /// <paramref name="anchorText"/>, after it (i.e. as trailing trivia on the same line as the node).
    /// </summary>
    let assertTrailingCommentSameLine (parser: Parser<Ast, unit>) (code: string) (anchorText: string) (commentText: string) =
        let rendered = printNode parser code
        let lines = toLines rendered
        let anchorIdx = lines |> Array.tryFindIndex (fun l -> l.Contains(anchorText: string))
        match anchorIdx with
        | None -> Assert.Fail($"Expected rendered output to contain anchor '{anchorText}' but got:{Environment.NewLine}{rendered}")
        | Some i ->
            let line = lines.[i]
            let anchorPos = line.IndexOf(anchorText: string)
            let commentPos = line.IndexOf(commentText: string)
            Assert.IsTrue(
                commentPos > anchorPos,
                $"Expected trailing comment '{commentText}' to appear after anchor '{anchorText}' on the same rendered line, but line was: '{line}'{Environment.NewLine}--- full rendered ---{Environment.NewLine}{rendered}")
