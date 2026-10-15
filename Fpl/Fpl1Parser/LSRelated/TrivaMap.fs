/// Provides a matching algorithm building the TriviaMap comparing each comment's position
/// against all collected node positions to find the nearest one — fundamentally a "merge two sorted sequences" operation.
/// </summary>
/// <remarks>
/// This module exists to support comment-trivia attachment for a formatting service: after parsing,
/// a single post-pass walks the entire tree in source order and attaches each comment either as:
/// leading trivia of the next node whose start position follows the comment, or
/// trailing trivia of the widest (outermost) preceding node whose end lands on the same source
/// line as the comment, if any such node exists.
/// </remarks>
module Fpl1Parser.LSRelated.TriviaMap
open FParsec
open System.Collections.Generic
open Fpl1Parser.Types
open Fpl1Parser.LSRelated.CommentLexer

/// <summary>
/// Comment trivia attached to a single AST node: any comments that precede it (leading) and
/// any comments that follow it on the same source line (trailing), in source order.
/// </summary>
type Trivia = {
    /// <summary>
    /// Comments that appear before this node's start position and are not attached as trailing
    /// trivia of an earlier node, in source order. Typically comments on their own line(s)
    /// immediately preceding the node they document.
    /// </summary>
    Leading: Comment list
    /// <summary>
    /// Comments, if any, that start on the same source line as this node's end position, in
    /// source order. Used for end-of-line comments (e.g. <c>x := 1; // explains x</c>) so they
    /// stay attached to the statement they annotate rather than drifting to the next node during
    /// reformatting. A list rather than a single <c>Comment option</c>, since several distinct
    /// comments (e.g. a block comment immediately followed by a line comment) can legitimately
    /// all trail the same node on the same source line.
    /// </summary>
    Trailing: Comment list
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
/// it, unless at least one preceding node's end position lands on the same source line as the
/// comment, in which case the comment is attached as the trailing trivia of the WIDEST such node
/// — i.e. the one with the smallest start index among all candidates — so that e.g. a comment
/// following a class definition's closing brace stays attached to the whole class definition
/// rather than drifting to whatever innermost statement/expression happens to end on that same
/// line (which would otherwise "win" merely because it is the node whose start index is closest
/// to the comment).
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
/// <paramref name="comments"/> in source order. Because AST nodes nest, several ancestors can
/// share (or nearly share) an end position with their last-closing descendant (e.g. the last
/// statement of a block ends right where the block itself ends); among all nodes satisfying
/// "already closed, same line as the comment", the ancestor always has the smallest start index
/// (it was opened first), so picking the minimum start index reliably selects the outermost one.
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

        // Among all nodes that start before the comment, find candidates whose END lands on the
        // same source line as the comment and precedes it; among those, the one with the
        // smallest start index is the outermost (widest) enclosing node.
        let mutable bestIdx = -1
        for i in 0 .. nodeIdx - 1 do
            let (nStart, nEnd) = nodes.[i]
            if nEnd.Index <= cStart.Index && isSameLine nEnd cStart then
                if bestIdx = -1 || nStart.Index < (fst nodes.[bestIdx]).Index then
                    bestIdx <- i

        if bestIdx >= 0 then
            let k = key nodes.[bestIdx]
            let existing = match map.TryGetValue k with true, t -> t | false, _ -> { Leading = []; Trailing = [] }
            map.[k] <- { existing with Trailing = existing.Trailing @ [c] }
        elif nodeIdx < nodes.Length then
            let k = key nodes.[nodeIdx]
            let existing = match map.TryGetValue k with true, t -> t | false, _ -> { Leading = []; Trailing = [] }
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

/// <summary>
/// Looks up and removes the <c>Trivia</c> attached to the node whose start position is
/// <paramref name="p"/>, so that a comment is rendered at most once even when several AST nodes
/// (e.g. pass-through wrapper nodes) share the same start position.
/// </summary>
/// <param name="map">The <c>TriviaMap</c> to query, as produced by <c>buildTriviaMap</c>.</param>
/// <param name="p">The <c>Positions</c> of the node whose trivia should be retrieved.</param>
/// <returns>
/// <c>Some</c> trivia if any comments were attached to this node's start position (the entry is
/// then removed from <paramref name="map"/>); otherwise <c>None</c>.
/// </returns>
/// <remarks>
/// Prefer this over <c>tryGetTrivia</c> when rendering: several AST node kinds can legitimately
/// share the exact same start <c>Position</c> as a nested child (e.g. single-child pass-through
/// wrappers), and without removing the entry on first use both the wrapper and its child would
/// independently retrieve and re-emit the same leading/trailing comment, duplicating it in the
/// pretty-printed output.
/// </remarks>
let tryTakeTrivia (map: TriviaMap) (p: Positions) : Trivia option =
    let k = key p
    match map.TryGetValue k with
    | true, t ->
        map.Remove k |> ignore
        Some t
    | false, _ -> None
