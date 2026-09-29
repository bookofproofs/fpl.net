/// <summary>
/// Pretty-prints an FPL AST back to canonically formatted source text, re-inserting comments
/// captured in a <c>TriviaMap</c> as leading/trailing trivia of their attached nodes.
/// </summary>
module Fpl1Parser.LSRelated.PrettyPrint
open Fpl1Parser.Types
open Fpl1Parser.LSRelated.CommentLexer
open Fpl1Parser.LSRelated.TriviaMap
open Fpl1Parser.LSRelated.Doc


let private renderComment (c: CommentLexer.Comment) : Doc =
    match c.Kind with
    | CommentLexer.LineComment -> concat [ text c.Text; line ]   // must end the line
    | CommentLexer.BlockComment -> text c.Text                   // can stay inline

/// <summary>
/// Wraps <paramref name="body"/> with any leading-comment lines before it and any trailing
/// comment appended after it on the same line, as recorded for <paramref name="pos"/> in <paramref name="map"/>.
/// </summary>
let private withTrivia (map: TriviaMap) (pos: Positions) (body: Doc) : Doc =
    match tryGetTrivia map pos with
    | None -> body
    | Some trivia ->
        let leading =
            trivia.Leading
            |> List.collect (fun c -> [ text c.Text; line ])
            |> concat
        let trailing =
            match trivia.Trailing with
            | Some c -> concat [ text " "; text c.Text ]
            | None -> concat []
        concat [ leading; body; trailing ]

/// <summary>
/// The recursive per-node printer. 
/// </summary>
/// </remarks>
/// It takes a single Ast node and turns it into a Doc, consulting withTrivia at each node so leading/trailing comments get spliced in correctly.
/// It calls itself recursively on child nodes. It has no notion of "this is the whole document"
/// — it just knows how to render one construct.
/// </remarks>
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
    | _ -> withTrivia map p (text "true") // "TODO: remaining Ast cases"

/// <summary>
/// the actual entry point for the formatting service. It's the only function that:
///	- accepts the list of top-level building-block ASTs (exactly what fplParser/fplParserWithTrivia returns as its first tuple element),
/// - decides the top-level layout policy (blank line between building blocks, via [ d; line; line ]),
/// - calls render indentSize to turn the composed Doc tree into the final string.
/// </summary>
let printAll (indentSize: int) (map: TriviaMap) (asts: Ast list) : string =
    asts
    |> List.map (print map)
    |> List.collect (fun d -> [ d; line; line ])   // blank line between top-level blocks
    |> concat
    |> render indentSize
