/// <summary>
/// A minimal pretty-printing document type and renderer, used by the FPL formatting
/// service to compose indentation-aware output without manual column/whitespace bookkeeping.
/// </summary>
module Fpl1Parser.LSRelated.Doc
open System.Text

type Doc =
    | Text of string
    | Line                      // hard line break (respects current indent)
    | Concat of Doc list
    | Indent of Doc             // increases indent by one level for the wrapped doc

let text s = Text s
let line = Line
let concat docs = Concat docs
let indent d = Indent d
let (<+>) a b = Concat [a; b]

/// <summary>Renders a <c>Doc</c> to a string using <paramref name="indentSize"/> spaces per level.</summary>
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
            sb.Append(System.Environment.NewLine) |> ignore
            true
        | Concat docs ->
            let mutable ls = atLineStart
            for d in docs do
                ls <- go level ls d
            ls
        | Indent d -> go (level + 1) atLineStart d
    go 0 true doc |> ignore
    sb.ToString()
