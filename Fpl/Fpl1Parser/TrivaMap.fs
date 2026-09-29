/// <summary>
/// Provides a matching algorithm building the TriviaMap comparing each comment's position
/// against all collected node positions to find the nearest one — fundamentally a "merge two sorted sequences" operation.
/// </summary>
/// <remarks>
/// This module exists to support comment-trivia attachment for a formatting service: after parsing,
/// a single post-pass walks the entire tree in source order and attaches each comment either as:
/// leading trivia of the next node whose start position follows the comment, or
/// trailing trivia of the previous node, if the comment starts on the same source line as that node's end.
/// </remarks>
module Fpl1Parser.TriviaMap
open FParsec
open System.Collections.Generic
open Fpl1Parser.Types

type Trivia = {
    Leading: (Positions * string) list
    Trailing: (Positions * string) option
}

type TriviaMap = Dictionary<Positions, Trivia>

/// <summary>
/// Builds a <c>TriviaMap</c> by associating each parsed comment with the nearest enclosing
/// AST node position: a comment is attached as leading trivia of the next node that starts
/// after it, unless it appears on the same source line as the end of the previous node, in
/// which case it is attached as that previous node's trailing comment.
/// </summary>
/// <param name="nodePositions">All node positions in source order, as produced by <c>getAllPositions</c>.</param>
/// <param name="comments">All comments discovered during parsing, as (Positions * text), in source order.</param>
/// <returns>A <c>TriviaMap</c> keyed by node <c>Positions</c>.</returns>
let buildTriviaMap (nodePositions: Positions list) (comments: (Positions * string) list) : TriviaMap =
    let map = TriviaMap()
    let nodes = nodePositions |> List.toArray

    let isSameLine (p1: Position) (p2: Position) = p1.Line = p2.Line

    let mutable nodeIdx = 0
    for (commentPos, text) in comments do
        let (cStart, _) = commentPos
        // advance past all nodes that start before this comment
        while nodeIdx < nodes.Length - 1 && (fst nodes.[nodeIdx]) < fst commentPos do
            nodeIdx <- nodeIdx + 1

        if nodeIdx > 0 && isSameLine (snd nodes.[nodeIdx - 1]) cStart then
            // trailing comment on the same line as the previous node's end
            let key = nodes.[nodeIdx - 1]
            let existing =
                match map.TryGetValue key with
                | true, t -> t
                | false, _ -> { Leading = []; Trailing = None }
            map.[key] <- { existing with Trailing = Some(commentPos, text) }
        elif nodeIdx < nodes.Length then
            // leading comment of the next node
            let key = nodes.[nodeIdx]
            let existing =
                match map.TryGetValue key with
                | true, t -> t
                | false, _ -> { Leading = []; Trailing = None }
            map.[key] <- { existing with Leading = existing.Leading @ [(commentPos, text)] }

    map
