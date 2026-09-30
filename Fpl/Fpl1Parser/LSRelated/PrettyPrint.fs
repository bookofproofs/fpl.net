/// <summary>
/// Pretty-prints an FPL AST back to canonically formatted source text, re-inserting comments
/// captured in a <c>TriviaMap</c> as leading/trailing trivia of their attached nodes.
/// </summary>
module Fpl1Parser.LSRelated.PrettyPrint
open Fpl1Parser.Types
open Fpl1Parser.LSRelated.CommentLexer
open Fpl1Parser.LSRelated.TriviaMap
open Fpl1Parser.LSRelated.Doc


/// <summary>Renders a single comment, honoring its <see cref="CommentLexer.CommentKind"/>.</summary>
/// <param name="c">The comment to render.</param>
/// <returns>
/// A <see cref="Doc"/> containing the comment's text: for <see cref="CommentLexer.LineComment"/>,
/// followed by a mandatory <see cref="line"/> break (since nothing may follow a <c>//</c> comment
/// on the same physical line); for <see cref="CommentLexer.BlockComment"/>, with no forced break,
/// since <c>/* ... */</c> comments are self-terminating and may remain inline.
/// </returns>
/// <remarks>
/// Used specifically for the <em>trailing</em> comment slot of <see cref="TriviaMap.Trivia"/>,
/// where whether a line break is required depends on the comment's kind. Leading comments always
/// occupy their own line regardless of kind and are instead rendered via
/// <see cref="renderLeadingComment"/>.
/// </remarks>
let private renderComment (c: CommentLexer.Comment) : Doc =
    match c.Kind with
    | CommentLexer.LineComment -> concat [ text c.Text; line ]   // must end the line
    | CommentLexer.BlockComment -> text c.Text                   // can stay inline

/// <summary>
/// Renders a single leading comment as its own line: the comment text followed by a line break,
/// regardless of <c>Kind</c>, since leading comments always occupy their own source line.
/// </summary>
/// <param name="c">The leading comment to render.</param>
/// <returns>A <see cref="Doc"/> containing the comment's text followed by a <see cref="line"/> break.</returns>
let private renderLeadingComment (c: Comment) : Doc =
    concat [ text c.Text; line ]

/// <summary>
/// Wraps <paramref name="body"/> with any leading-comment lines before it and any trailing
/// comment appended after it on the same line, as recorded for <paramref name="pos"/> in <paramref name="map"/>.
/// </summary>
/// <param name="map">The <see cref="TriviaMap"/> built from the parsed AST and discovered comments.</param>
/// <param name="pos">The <see cref="Positions"/> of the AST node whose trivia should be consulted.</param>
/// <param name="body">The already-rendered <see cref="Doc"/> for the node itself, to be wrapped with its trivia.</param>
/// <returns>
/// <paramref name="body"/> unchanged if <paramref name="pos"/> has no attached trivia; otherwise
/// <paramref name="body"/> preceded by each leading comment (each on its own line, via
/// <see cref="renderLeadingComment"/>) and followed by its trailing comment, if any (via
/// <see cref="renderComment"/>, preceded by a single space).
/// </returns>
let private withTrivia (map: TriviaMap) (pos: Positions) (body: Doc) : Doc =
    match tryGetTrivia map pos with
    | None -> body
    | Some trivia ->
        let leading =
            trivia.Leading
            |> List.map renderLeadingComment
            |> concat
        // a trailing LineComment gets its mandatory line break via renderComment's LineComment branch,
        // while a trailing BlockComment stays inline exactly as before
        let trailing =
            match trivia.Trailing with
            | Some c -> concat [ text " "; renderComment c ]
            | None -> concat []
        concat [ leading; body; trailing ]

/// <summary>
/// The recursive per-node printer: renders a single <see cref="Ast"/> node to a <see cref="Doc"/>,
/// consulting <paramref name="map"/> via <see cref="withTrivia"/> at each node so leading/trailing
/// comments are spliced in at the correct position, and recursing into child nodes in source order.
/// </summary>
/// <param name="map">The <see cref="TriviaMap"/> used to attach comments to the nodes being printed.</param>
/// <param name="ast">The AST node to render.</param>
/// <returns>The rendered <see cref="Doc"/> for <paramref name="ast"/>, including any attached trivia.</returns>
/// <remarks>
/// Has no notion of "this is the whole document" — each case renders exactly one construct and
/// delegates to itself for child nodes. <see cref="printAll"/> is the module's public entry point
/// and is the only function that should be called from outside this module; <c>print</c> is kept
/// <c>private</c> so callers cannot bypass <see cref="printAll"/>'s top-level layout policy
/// (blank-line separation and final rendering) by invoking it directly on an isolated sub-tree.
/// </remarks>
/// <exception cref="System.Exception">
/// Thrown by the current catch-all case for any <see cref="Ast"/> constructor not yet covered by an
/// explicit match arm. This is a temporary placeholder pending full case coverage mirroring
/// <c>Trivia.collectPositions</c>; it must not be relied upon in production until every case is
/// implemented. Note: the current placeholder also references an undefined identifier <c>p</c>
/// and will not compile until corrected (e.g. to <c>failwith $"PrettyPrint.print: unhandled Ast case {ast}"</c>
/// or similar, with no dependency on a node position that a wildcard match cannot provide).
/// </exception>
let rec private print (map: TriviaMap) (ast: Ast) : Doc =
    match ast with
    | PascalCaseId(pos, name) -> withTrivia map pos (text name)
    | Var(pos, name) -> withTrivia map pos (text name)
    | True(pos, _) -> withTrivia map pos (text "true")
    | False(pos, _) -> withTrivia map pos (text "false")
    | And(pos, (a1, a2)) ->
        withTrivia map pos (concat [ print map a1; text " and "; print map a2 ])
    // ... one case per Ast constructor, mirroring the exact grouping/order used in
    // Trivia.fs's collectPositions, so gaps are easy to spot by diffing the two files.
    | _ -> failwith $"PrettyPrint.print: unhandled Ast case: %A{ast}"

/// <summary>
/// The entry point for the formatting service: pretty-prints a list of top-level building-block
/// ASTs (as returned by <c>Fpl1Parser.Main.fplParser</c>), honoring trivia recorded in <paramref name="map"/>.
/// </summary>
/// <param name="indentSize">The number of spaces to use per indentation level when rendering.</param>
/// <param name="map">The <see cref="TriviaMap"/> built from the parsed AST and discovered comments.</param>
/// <param name="asts">The top-level building-block AST nodes to render, in source order.</param>
/// <returns>
/// The fully rendered, canonically formatted FPL source text, with a blank line separating each
/// top-level building block.
/// </returns>
/// <remarks>
/// This is the only function in the module intended to be called by <c>Fpl3LanguageServer</c>'s
/// <c>FormattingHandler</c>. It maps <see cref="print"/> over each top-level node, inserts a blank
/// line between building blocks, and delegates final text composition to <see cref="Doc.render"/>.
/// </remarks>
let printAll (indentSize: int) (map: TriviaMap) (asts: Ast list) : string =
    asts
    |> List.map (print map)
    |> List.collect (fun d -> [ d; line; line ])   // blank line between top-level blocks
    |> concat
    |> render indentSize
