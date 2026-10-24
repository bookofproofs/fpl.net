/// <summary>
/// Resolves the declaration of a symbol-table node for the "go to definition" and "find references" services.
/// </summary>
module Fpl2Interpreter.SymbolTable.GoToDefinition

open System.Collections.Generic
open Fpl2Interpreter.BasicTypes
open Fpl2Interpreter.SymbolTable.Types1.TopLevel
open Fpl2Interpreter.SymbolTable.Types3.PredicativeBlocks
open Fpl2Interpreter.SymbolTable.Types4.Proofs

/// <summary>
/// Returns the declaration node that the given node refers to, if one exists.
/// </summary>
/// <param name="node">Symbol-table node under the cursor.</param>
/// <returns>The declaration node, or <c>None</c> if it cannot be resolved.</returns>
/// <remarks>
/// Proof arguments are resolved only through <c>RefersTo</c>. Proofs and corollaries refer to the
/// theorem-like statement they belong to (their parent node). Other nodes use <c>RefersTo</c> first.
/// Variables without <c>RefersTo</c> are looked up in the scope of their ultimate block node.
/// </remarks>
let resolveDeclaration (node: FplGenericNode) : FplGenericNode option =
    match node with
    | :? FplJustificationItemByRefArgument
    | :? FplJustificationItemByProofArgument -> node.RefersTo
    | :? FplProof
    | :? FplCorollary ->
        match node.Parent with
        | Some parent when not (parent :? FplRoot) -> Some parent
        | _ -> None
    | _ ->
        match node.RefersTo with
        | Some declaration -> Some declaration
        | None when checkStartsWithLowerCase node.FplId ->
            match node.UltimateBlockNode with
            | Some ultimateNode ->
                match ultimateNode.Scope.TryGetValue node.FplId with
                | true, declaration -> Some declaration
                | _ -> None
            | _ -> None
        | None -> None

/// <summary>
/// Inverse index: declaration node (reference identity) → nodes that resolve to it.
/// </summary>
type ReferenceIndex = Dictionary<FplGenericNode, ResizeArray<FplGenericNode>>

/// <summary>
/// Builds the declaration → referencing nodes index from the flattened nodes of one evaluation.
/// </summary>
/// <param name="nodes">All flattened symbol-table nodes, across all files.</param>
/// <returns>The inverse index.</returns>
let buildReferenceIndex (nodes: seq<FplGenericNode>) : ReferenceIndex =
    let index = ReferenceIndex(HashIdentity.Reference)
    for node in nodes do
        match resolveDeclaration node with
        | Some declaration ->
            match index.TryGetValue declaration with
            | true, references -> references.Add node
            | _ ->
                let references = ResizeArray<FplGenericNode>()
                references.Add node
                index.[declaration] <- references
        | None -> ()
    index

/// <summary>
/// Returns all nodes that resolve to the given declaration.
/// </summary>
/// <param name="index">Inverse index built by <c>buildReferenceIndex</c>.</param>
/// <param name="declaration">Declaration node to search references for.</param>
/// <returns>The referencing nodes, or an empty list.</returns>
let findReferences (index: ReferenceIndex) (declaration: FplGenericNode) : FplGenericNode list =
    match index.TryGetValue declaration with
    | true, references -> List.ofSeq references
    | _ -> []

/// <summary>
/// Moves from a structural node without an identifier (for example a justification) up to the nearest
/// ancestor that has one (for example the argument it belongs to). Stops at the root.
/// </summary>
/// <param name="node">Symbol-table node under the cursor.</param>
/// <returns>The node itself if it has an identifier, otherwise its nearest named ancestor.</returns>
let rec private toNamedNode (node: FplGenericNode) : FplGenericNode =
    if node.FplId <> "" then
        node
    else
        match node.Parent with
        | Some parent when not (parent :? FplRoot) -> toNamedNode parent
        | _ -> node

/// <summary>
/// Returns the nodes to report for "find references" at the given cursor node.
/// </summary>
/// <param name="cursorNode">Symbol-table node under the cursor.</param>
/// <param name="nodes">All flattened symbol-table nodes of the current evaluation.</param>
/// <returns>The declaration followed by all nodes resolving to it.</returns>
/// <remarks>
/// The cursor node is first moved to its nearest named ancestor, so a cursor on a justification
/// is treated as a cursor on the argument it belongs to. If the result does not resolve to a declaration,
/// it is treated as the declaration itself.
/// The index is built on each call, so results never depend on earlier evaluations.
/// </remarks>
let findReferenceNodes (cursorNode: FplGenericNode) (nodes: seq<FplGenericNode>) =
    let named = toNamedNode cursorNode
    let declaration =
        match resolveDeclaration named with
        | Some target -> target
        | None -> named
    let index = buildReferenceIndex nodes
    let references =
        findReferences index declaration
        |> List.filter (fun node -> not (obj.ReferenceEquals(node, declaration)))
    declaration :: references
