/// <summary>
/// A minimal pretty-printing document type and renderer, used by the FPL formatting
/// service to compose indentation-aware output without manual column/whitespace bookkeeping.
/// </summary>
module Fpl1Parser.LSRelated.Doc
open System
open System.Text

/// <summary>
/// A minimal pretty-printing document, composed by <c>Fpl1Parser.LSRelated.PrettyPrint</c> to
/// represent formatted FPL output before it is rendered to a final string via <see cref="render"/>.
/// </summary>
/// <remarks>
/// Modeled after the classic Wadler-style pretty-printing document types: a <c>Doc</c> tree
/// separates the *structure* of the output (text, breaks, nesting) from the *rendering* policy
/// (indent width, line-ending), so print functions never need to track the current column or
/// manually pad indentation themselves.
/// </remarks>
type Doc =
    | Text of string
    | Line                      // hard line break (respects current indent)
    | Concat of Doc list
    | Indent of Doc             // increases indent by one level for the wrapped doc

/// <summary>Wraps a literal string as a <see cref="Doc"/>.</summary>
/// <param name="s">The literal text to emit.</param>
/// <returns>A <see cref="Doc.Text"/> node containing <paramref name="s"/>.</returns>
let text s = Text s

/// <summary>A hard line break document.</summary>
/// <returns>The <see cref="Doc.Line"/> node.</returns>
let line = Line

/// <summary>Concatenates a sequence of documents into a single document, preserving their order.</summary>
/// <param name="docs">The documents to concatenate, in the order they should be rendered.</param>
/// <returns>A <see cref="Doc.Concat"/> node wrapping <paramref name="docs"/>.</returns>
let concat docs = Concat docs

/// <summary>Increases the indentation level by one for the wrapped document.</summary>
/// <param name="d">The document to render at one additional indentation level.</param>
/// <returns>A <see cref="Doc.Indent"/> node wrapping <paramref name="d"/>.</returns>
let indent d = Indent d

/// <summary>Concatenates two documents, <paramref name="a"/> followed by <paramref name="b"/>.</summary>
/// <param name="a">The first document.</param>
/// <param name="b">The second document, rendered immediately after <paramref name="a"/>.</param>
/// <returns>A <see cref="Doc.Concat"/> node containing <paramref name="a"/> and <paramref name="b"/>, in order.</returns>
let (<+>) a b = Concat [a; b]

/// <summary>Renders a <see cref="Doc"/> tree to a single formatted string.</summary>
/// <param name="indentSize">The number of spaces to emit per indentation level.</param>
/// <param name="doc">The document tree to render.</param>
/// <returns>
/// The fully rendered text: every <see cref="Doc.Line"/> becomes <see cref="System.Environment.NewLine"/>,
/// and every <see cref="Doc.Text"/> that immediately follows a line break (or begins the document) is
/// preceded by <paramref name="indentSize"/> * (current indentation level) space characters.
/// </returns>
/// <remarks>
/// Indentation is tracked purely as a traversal-local <c>level</c> counter driven by
/// <see cref="Doc.Indent"/> nodes; no column-position bookkeeping is required because padding is
/// only ever inserted immediately before a <see cref="Doc.Text"/> node that starts a new line
/// (tracked via the <c>atLineStart</c> flag threaded through the recursive traversal).
/// Consecutive <see cref="Doc.Line"/> nodes therefore produce blank lines with no trailing
/// whitespace, since padding is only emitted right before actual text content.
/// </remarks>
let render (indentSize: int) (doc: Doc) : string =
    let sb = StringBuilder()
    let pad level = sb.Append(String.replicate (level * indentSize) " ") |> ignore
    let rec go level atLineStart d =
        match d with
        | Text s ->
            if atLineStart then pad level
            sb.Append(s) |> ignore
            false
        | Line ->
            sb.Append(Environment.NewLine) |> ignore
            true
        | Concat docs ->
            let mutable ls = atLineStart
            for d in docs do
                ls <- go level ls d
            ls
        | Indent d -> go (level + 1) atLineStart d
    go 0 true doc |> ignore
    sb.ToString()
