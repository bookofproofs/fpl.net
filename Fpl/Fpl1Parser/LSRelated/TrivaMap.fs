/// Provides a matching algorithm building the TriviaMap comparing each comment's position
/// against all collected node positions to find the nearest one — fundamentally a "merge two sorted sequences" operation.
/// </summary>
/// <remarks>
/// This module exists to support comment-trivia attachment for a formatting service: after parsing,
/// a single post-pass walks the entire tree in source order and attaches each comment either as:
/// leading trivia of the next node whose start position follows the comment, or
/// trailing trivia of the widest (outermost) node among those that end nearest to (and on the same
/// source line as) the comment, if any such node exists.
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
/// Maps an AST node to its attached <c>Trivia</c>, keyed by the FULL <c>(start.Index, end.Index)</c>
/// pair of the node's <c>Positions</c>.
/// </summary>
/// <remarks>
/// Keying by start index ALONE (as an earlier version of this module did) is unsound: many
/// grammar productions wrap an outer composite node around an inner child node that starts at the
/// exact same token — e.g. <c>Ast.RuleOfInference</c> and its own <c>Ast.RuleOfInferenceSignature</c>
/// child both start at the <c>inf</c> keyword; <c>Ast.Proof</c>/<c>Ast.ProofSignature</c> both start
/// at <c>prf</c>; <c>Ast.DefinitionClass</c>/<c>Ast.ClassSignature</c> both start at the class
/// keyword. Keying by start index alone makes these distinct nodes alias to the same dictionary
/// slot, so whichever one happens to be rendered (and call <see cref="tryTakeTrivia"/>) FIRST —
/// typically the inner child, since <c>PrettyPrint.print</c> renders child sub-expressions before
/// the enclosing <c>withTrivia</c> call wraps the result — incorrectly steals trivia that was
/// actually attached to the outer node. Including the end index in the key disambiguates these
/// cases, since an outer node's span always strictly contains (and so has a different end index
/// from) its inner child's span whenever the two aren't literally the same AST node.
/// </remarks>
type TriviaMap = Dictionary<int64 * int64, Trivia>

let private key ((startPos, endPos): Positions) = (startPos.Index, endPos.Index)

/// <summary>
/// Builds a <c>TriviaMap</c> by associating each comment discovered in the source with the
/// nearest AST node: a comment is attached as leading trivia of the next node that starts after
/// it, unless at least one preceding node's end position lands on the same source line as the
/// comment, in which case the comment is attached as trailing trivia of a node chosen in two steps:
/// first, find the node(s) whose end is CLOSEST to (i.e. has the maximum end index not exceeding)
/// the comment's start and on the same source line; then, among any such nodes tied at that same
/// end index (which happens exactly when an outer node's span ends at the same point as its
/// last-closing descendant's), pick the one with the smallest start index — the outermost ancestor.
/// </summary>
/// <param name="nodePositions">
/// All node positions in source order, as produced by <c>Fpl1Parser.Trivia.getAllPositions</c>.
/// </param>
/// <param name="comments">
/// All comments discovered in the source, as produced by <c>Fpl1Parser.CommentLexer.findComments</c>,
/// in source order.
/// </param>
/// <returns>
/// A <c>TriviaMap</c> keyed by node <c>Positions</c> (see <c>TriviaMap</c>) that the pretty-printer
/// can consult while emitting each node, in order to re-insert its leading and trailing comments.
/// </returns>
/// <remarks>
/// This is fundamentally a merge of two already-sorted sequences (node positions and comment
/// positions), advancing a single cursor over <paramref name="nodePositions"/> as it scans
/// <paramref name="comments"/> in source order.
/// <para>
/// Picking by "maximum end index, tie-broken by minimum start index" (rather than simply "minimum
/// start index among any node ending on the comment's line", as an earlier version of this function
/// did) matters once several unrelated nodes happen to share a source line with the comment — which
/// is the common case for single-physical-line FPL snippets. Without first narrowing to the node(s)
/// closing NEAREST the comment, an earlier, unrelated, but textually "wider" node (e.g. a class's
/// signature, which starts earliest but ends long before the comment) could incorrectly win over the
/// node that is actually immediately adjacent to the comment (e.g. the class's last constructor
/// block).
/// </para>
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

        // Step 1: among all nodes that start before the comment and whose END lands on the same
        // source line as the comment and at or before it, find the maximum end index — i.e. the
        // node(s) closing NEAREST the comment.
        let mutable maxEnd = -1L
        for i in 0 .. nodeIdx - 1 do
            let (_, nEnd) = nodes.[i]
            if nEnd.Index <= cStart.Index && isSameLine nEnd cStart && nEnd.Index > maxEnd then
                maxEnd <- nEnd.Index

        // Step 2: among nodes tied at that maximum end index, pick the one with the smallest
        // start index — the outermost ancestor among any nodes that close at exactly that point.
        let mutable bestIdx = -1
        if maxEnd >= 0L then
            for i in 0 .. nodeIdx - 1 do
                let (nStart, nEnd) = nodes.[i]
                if nEnd.Index = maxEnd then
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
/// Looks up the <c>Trivia</c> attached to the node whose <c>Positions</c> is <paramref name="p"/>.
/// </summary>
/// <param name="map">The <c>TriviaMap</c> to query, as produced by <c>buildTriviaMap</c>.</param>
/// <param name="p">The <c>Positions</c> of the node whose trivia should be retrieved.</param>
/// <returns>
/// <c>Some</c> trivia if any comments were attached to this exact node; otherwise <c>None</c>.
/// </returns>
let tryGetTrivia (map: TriviaMap) (p: Positions) : Trivia option =
    match map.TryGetValue(key p) with
    | true, t -> Some t
    | false, _ -> None

/// <summary>
/// Looks up and removes the <c>Trivia</c> attached to the node whose <c>Positions</c> is
/// <paramref name="p"/>, so that a comment is rendered at most once even when several AST nodes
/// (e.g. pass-through wrapper nodes) share the exact same <c>Positions</c>.
/// </summary>
/// <param name="map">The <c>TriviaMap</c> to query, as produced by <c>buildTriviaMap</c>.</param>
/// <param name="p">The <c>Positions</c> of the node whose trivia should be retrieved.</param>
/// <returns>
/// <c>Some</c> trivia if any comments were attached to this exact node (the entry is then removed
/// from <paramref name="map"/>); otherwise <c>None</c>.
/// </returns>
/// <remarks>
/// Prefer this over <c>tryGetTrivia</c> when rendering: a handful of AST node kinds can still
/// legitimately share the exact same full <c>Positions</c> (start AND end) as a nested child (true
/// single-child pass-through wrappers, as opposed to the merely-same-start aliasing this module's
/// keying scheme now disambiguates), and without removing the entry on first use both the wrapper
/// and its child would independently retrieve and re-emit the same leading/trailing comment,
/// duplicating it in the pretty-printed output.
/// </remarks>
let tryTakeTrivia (map: TriviaMap) (p: Positions) : Trivia option =
    let k = key p
    match map.TryGetValue k with
    | true, t ->
        map.Remove k |> ignore
        Some t
    | false, _ -> None
