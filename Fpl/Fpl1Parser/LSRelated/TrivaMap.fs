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
module Fpl1Parser.LSRelated.TriviaMap
open FParsec
open System.Collections.Generic
open Fpl1Parser.Types
open Fpl1Parser.LSRelated.CommentLexer

/// <summary>
/// Comment trivia attached to a single AST node: any comments that precede it (leading) and,
/// at most, one comment that follows it on the same source line (trailing).
/// </summary>
type Trivia = {
    /// <summary>
    /// Comments that appear before this node's start position and are not attached as trailing
    /// trivia of an earlier node, in source order. Typically comments on their own line(s)
    /// immediately preceding the node they document.
    /// </summary>
    Leading: Comment list
    /// <summary>
    /// The comment, if any, that starts on the same source line as this node's end position.
    /// Used for end-of-line comments (e.g. <c>x := 1; // explains x</c>) so they stay attached
    /// to the statement they annotate rather than drifting to the next node during reformatting.
    /// </summary>
    Trailing: Comment option
}

/// <summary>
/// Maps an AST node to its attached <c>Trivia</c>, keyed by the <c>Index</c> of the node's start
/// <c>Position</c> (see <c>Positions</c>).
/// </summary>
/// <remarks>
/// Keying on the start position's <c>Index</c> (an <c>int64</c>) rather than on the full
/// <c>Positions</c> tuple avoids relying on <see cref="FParsec.Position"/>'s structural equality
/// and hashing as an implicit dependency, and is cheaper to hash for trees with many nodes.
/// </remarks>
type TriviaMap = Dictionary<int64, Trivia>

let private key ((startPos, _): Positions) = startPos.Index

/// <summary>
/// Builds a <c>TriviaMap</c> by associating each comment discovered in the source with the
/// nearest AST node: a comment is attached as leading trivia of the next node that starts after
/// it, unless it begins on the same source line as the end of the previous node, in which case it
/// is attached as that previous node's trailing comment instead.
/// </summary>
/// <param name="nodePositions">
/// All node positions in source order, as produced by <c>Fpl1Parser.Trivia.getAllPositions</c>.
/// </param>
/// <param name="comments">
/// All comments discovered in the source, as produced by <c>Fpl1Parser.CommentLexer.findComments</c>,
/// in source order.
/// </param>
/// <returns>
/// A <c>TriviaMap</c> keyed by node start position (see <c>TriviaMap</c>) that the pretty-printer
/// can consult while emitting each node, in order to re-insert its leading and trailing comments.
/// </returns>
/// <remarks>
/// This is fundamentally a merge of two already-sorted sequences (node positions and comment
/// positions), advancing a single cursor over <paramref name="nodePositions"/> as it scans
/// <paramref name="comments"/> in source order.
/// </remarks>
let buildTriviaMap (nodePositions: Positions list) (comments: Comment list) : TriviaMap =
    let map = TriviaMap()
    let nodes = nodePositions |> List.toArray
    let isSameLine (p1: Position) (p2: Position) = p1.Line = p2.Line

    let mutable nodeIdx = 0
    for c in comments do
        let (cStart, _) = c.Positions
        while nodeIdx < nodes.Length - 1 && (fst nodes.[nodeIdx]).Index < cStart.Index do
            nodeIdx <- nodeIdx + 1

        if nodeIdx > 0 && isSameLine (snd nodes.[nodeIdx - 1]) cStart then
            let k = key nodes.[nodeIdx - 1]
            let existing = match map.TryGetValue k with true, t -> t | false, _ -> { Leading = []; Trailing = None }
            map.[k] <- { existing with Trailing = Some c }
        elif nodeIdx < nodes.Length then
            let k = key nodes.[nodeIdx]
            let existing = match map.TryGetValue k with true, t -> t | false, _ -> { Leading = []; Trailing = None }
            map.[k] <- { existing with Leading = existing.Leading @ [c] }

    map

/// <summary>
/// Looks up the <c>Trivia</c> attached to the node whose start position is <paramref name="p"/>.
/// </summary>
/// <param name="map">The <c>TriviaMap</c> to query, as produced by <c>buildTriviaMap</c>.</param>
/// <param name="p">The <c>Positions</c> of the node whose trivia should be retrieved.</param>
/// <returns>
/// <c>Some</c> trivia if any comments were attached to this node's start position; otherwise
/// <c>None</c>.
/// </returns>
let tryGetTrivia (map: TriviaMap) (p: Positions) : Trivia option =
    match map.TryGetValue(key p) with
    | true, t -> Some t
    | false, _ -> None
