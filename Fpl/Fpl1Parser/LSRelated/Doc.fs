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
/// Modeled after the classic Wadler/Prettier-style pretty-printing document types: a <c>Doc</c>
/// tree separates the *structure* of the output (text, breaks, nesting, grouping) from the
/// *rendering* policy (indent width, max line width, line-ending), so print functions never need
/// to track the current column or manually pad indentation themselves.
/// <para>
/// <see cref="Group"/> and <see cref="SoftLine"/> exist specifically to support width-driven
/// ("Auto") formatting options: a <c>Group</c> is a region that the renderer first tries to lay
/// out flat (all its <see cref="SoftLine"/> nodes become single spaces); if the flattened width
/// would exceed <c>MaxLineLength</c> at the current column — or the group contains at least one
/// unconditional <see cref="Line"/>, which can never be flattened — the renderer falls back to
/// laying the group out "broken" (its <see cref="SoftLine"/> nodes become real line breaks at the
/// current indentation).
/// </para>
/// </remarks>
type Doc =
    | Text of string
    | Line                       // hard line break: never flattened, always a real newline
    | SoftLine                   // becomes " " when its enclosing Group is laid out flat,
                                  // or a real line break (like Line) when the group is broken
    | Concat of Doc list
    | Indent of Doc              // increases indent by one level for the wrapped doc
    | Group of Doc                // a region that is tried flat first, broken only if it doesn't fit

/// <summary>Wraps a literal string as a <see cref="Doc"/>.</summary>
/// <param name="s">The literal text to emit.</param>
/// <returns>A <see cref="Doc.Text"/> node containing <paramref name="s"/>.</returns>
let text s = Text s

/// <summary>A hard line break document. Never flattened by a surrounding <see cref="Group"/>.</summary>
/// <returns>The <see cref="Doc.Line"/> node.</returns>
let line = Line

/// <summary>
/// A soft line break: renders as a single space when its innermost enclosing <see cref="Group"/>
/// is laid out flat, or as a real line break (at the current indentation) when that group is broken.
/// </summary>
/// <returns>The <see cref="Doc.SoftLine"/> node.</returns>
/// <remarks>
/// A bare <c>SoftLine</c> not wrapped in any <see cref="Group"/> behaves exactly like <see cref="line"/>,
/// since there is no enclosing group to ever flatten it.
/// </remarks>
let softline = SoftLine

/// <summary>Concatenates a sequence of documents into a single document, preserving their order.</summary>
/// <param name="docs">The documents to concatenate, in the order they should be rendered.</param>
/// <returns>A <see cref="Doc.Concat"/> node wrapping <paramref name="docs"/>.</returns>
let concat docs = Concat docs

/// <summary>Increases the indentation level by one for the wrapped document.</summary>
/// <param name="d">The document to render at one additional indentation level.</param>
/// <returns>A <see cref="Doc.Indent"/> node wrapping <paramref name="d"/>.</returns>
let indent d = Indent d

/// <summary>
/// Marks <paramref name="d"/> as a group: the renderer measures its flattened width and lays it
/// out flat if it fits within <c>MaxLineLength</c> at the current column, or broken otherwise.
/// </summary>
/// <param name="d">The document to consider for flat-vs-broken layout.</param>
/// <returns>A <see cref="Doc.Group"/> node wrapping <paramref name="d"/>.</returns>
let group d = Group d

/// <summary>Concatenates two documents, <paramref name="a"/> followed by <paramref name="b"/>.</summary>
/// <param name="a">The first document.</param>
/// <param name="b">The second document, rendered immediately after <paramref name="a"/>.</param>
/// <returns>A <see cref="Doc.Concat"/> node containing <paramref name="a"/> and <paramref name="b"/>, in order.</returns>
let (<+>) a b = Concat [a; b]

/// <summary>Renders a <see cref="Doc"/> tree to a single formatted string.</summary>
/// <param name="indentSize">The number of spaces to emit per indentation level.</param>
/// <param name="maxLineLength">
/// The column budget used to decide whether a <see cref="Group"/> is laid out flat or broken.
/// </param>
/// <param name="doc">The document tree to render.</param>
/// <returns>
/// The fully rendered text: every <see cref="Doc.Line"/> becomes <see cref="System.Environment.NewLine"/>;
/// every <see cref="Doc.SoftLine"/> becomes a space or a <see cref="System.Environment.NewLine"/>
/// depending on whether its enclosing <see cref="Doc.Group"/> fit flat; and every
/// <see cref="Doc.Text"/> that starts a new line is preceded by <paramref name="indentSize"/> *
/// (current indentation level) space characters.
/// </returns>
/// <remarks>
/// A <see cref="Doc.Group"/> can only be laid out flat if its entire subtree — including any
/// nested groups — contains no unconditional <see cref="Doc.Line"/>; this is determined once via
/// <c>flatWidth</c> (returning <c>None</c> if flattening is impossible), and the actual flat
/// rendering is then performed by a dedicated, column-tracking-free <c>renderFlat</c> pass, since a
/// flattened group never needs indentation padding partway through.
/// </remarks>
let render (indentSize: int) (maxLineLength: int) (doc: Doc) : string =
    let sb = StringBuilder()
    let pad level = sb.Append(String.replicate (level * indentSize) " ") |> ignore

    /// Computes the width of `d` if it can be rendered entirely flat (no unconditional Line
    /// anywhere in its subtree), or None if flattening is impossible.
    let rec flatWidth (d: Doc) : int option =
        match d with
        | Text s -> Some s.Length
        | Line -> None
        | SoftLine -> Some 1
        | Group g -> flatWidth g
        | Indent d -> flatWidth d
        | Concat ds ->
            ds
            |> List.fold
                (fun acc d ->
                    match acc with
                    | None -> None
                    | Some w -> flatWidth d |> Option.map ((+) w))
                (Some 0)

    /// Renders `d` assuming it is known to be flattenable (per flatWidth); returns the width added.
    let rec renderFlat (d: Doc) : int =
        match d with
        | Text s ->
            sb.Append(s) |> ignore
            s.Length
        | SoftLine ->
            sb.Append(" ") |> ignore
            1
        | Line -> 0 // unreachable: flatWidth guards against this case
        | Group g -> renderFlat g
        | Indent d -> renderFlat d
        | Concat ds -> ds |> List.sumBy renderFlat

    let effCol level atLineStart column =
        if atLineStart then level * indentSize else column

    /// Main recursive renderer. Returns (atLineStart, column) after rendering `d`.
    let rec go level atLineStart column (d: Doc) : bool * int =
        match d with
        | Text s ->
            let ec = effCol level atLineStart column
            if atLineStart then pad level
            sb.Append(s) |> ignore
            (false, ec + s.Length)
        | Line
        | SoftLine ->
            sb.Append(Environment.NewLine) |> ignore
            (true, 0)
        | Group g ->
            let ec = effCol level atLineStart column
            match flatWidth g with
            | Some w when ec + w <= maxLineLength ->
                if atLineStart then pad level
                let added = renderFlat g
                (false, ec + added)
            | _ -> go level atLineStart column g
        | Indent d -> go (level + 1) atLineStart column d
        | Concat ds ->
            let mutable ls = atLineStart
            let mutable col = column
            for d in ds do
                let ls2, col2 = go level ls col d
                ls <- ls2
                col <- col2
            (ls, col)

    go 0 true 0 doc |> ignore
    sb.ToString()
