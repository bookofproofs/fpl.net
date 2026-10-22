/// <summary>
/// Resolves the declaration of a symbol-table node for the "go to definition" service.
/// </summary>
module Fpl2Interpreter.SymbolTable.GoToDefinition
open System.Collections.Generic
open Fpl2Interpreter.BasicTypes
open Fpl2Interpreter.SymbolTable.Types4.Proofs

/// <summary>
/// Returns the declaration node that the given node refers to, if one exists.
/// </summary>
/// <param name="node">Symbol-table node under the cursor.</param>
/// <returns>The declaration node, or <c>None</c> if it cannot be resolved.</returns>
/// <remarks>
/// Proof arguments are resolved only through <c>RefersTo</c>. Other nodes use <c>RefersTo</c> first.
/// Variables without <c>RefersTo</c> are looked up in the scope of their ultimate block node.
/// </remarks>
let resolveDeclaration (node: FplGenericNode) : FplGenericNode option =
    match node with
    | :? FplJustificationItemByRefArgument
    | :? FplJustificationItemByProofArgument -> node.RefersTo
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

/// Cached index and the symbol-table root it was built from (reference identity).
let mutable private cachedIndex : (obj * ReferenceIndex) option = None

/// <summary>
/// Returns the reference index for the given symbol-table root, rebuilding it only when the root changed.
/// </summary>
/// <param name="root">Symbol-table root of the current evaluation.</param>
/// <param name="nodes">All flattened symbol-table nodes of the current evaluation.</param>
/// <returns>The cached or freshly built inverse index.</returns>
let getReferenceIndex (root: obj) (nodes: seq<FplGenericNode>) : ReferenceIndex =
    match cachedIndex with
    | Some (cachedRoot, index) when obj.ReferenceEquals(cachedRoot, root) -> index
    | _ ->
        let index = buildReferenceIndex nodes
        cachedIndex <- Some (root, index)
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
