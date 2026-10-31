/// <summary>
/// Resolves the declaration of a symbol-table node for the "go to definition" and "find references" services.
/// </summary>
module Fpl2Interpreter.SymbolTable.GoToDefinition

open System.Collections.Generic
open Fpl2Interpreter.BasicTypes
open Fpl2Interpreter.SymbolTable.ExpressionMatching
open Fpl2Interpreter.SymbolTable.Types1.TopLevel
open Fpl2Interpreter.SymbolTable.Types2.Definitions
open Fpl2Interpreter.SymbolTable.Types3.PredicativeBlocks
open Fpl2Interpreter.SymbolTable.Types3.RulesOfInferences
open Fpl2Interpreter.SymbolTable.Types3.DefinitionProperties
open Fpl2Interpreter.SymbolTable.Types3.Extensions
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
    | :? FplTheorem 
    | :? FplProposition 
    | :? FplLemma 
    | :? FplConjecture 
    | :? FplRuleOfInference 
    | :? FplExtension 
    | :? FplClass 
    | :? FplPredicate 
    | :? FplFunctionalTerm 
    | :? FplMandatoryFunctionalTerm 
    | :? FplMandatoryPredicate
    | :? FplArgument
    | :? FplAxiom ->
        Some node
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
/// Maps the node under the cursor to the node that the "go to definition" and "find references" services act on.
/// </summary>
/// <param name="cursorNode">Innermost symbol-table node containing the cursor.</param>
/// <returns>The argument that owns a justification, otherwise the cursor node itself.</returns>
/// <remarks>
/// A justification has no identifier of its own. A cursor on a justification therefore refers to the
/// argument it belongs to, which carries the identifier that is resolved.
/// </remarks>
let cursorTargetNode (cursorNode: FplGenericNode) : FplGenericNode =
    match cursorNode with
    | :? FplJustification ->
        match cursorNode.Parent with
        | Some argument -> argument
        | None -> cursorNode
    | _ -> cursorNode

/// <summary>
/// Returns the nodes to report for "find references" at the given cursor node.
/// </summary>
/// <param name="cursorNode">Innermost symbol-table node containing the cursor.</param>
/// <param name="nodes">All flattened symbol-table nodes of the current evaluation.</param>
/// <returns>The declaration followed by all nodes resolving to it.</returns>
/// <remarks>
/// If the cursor target does not resolve to a declaration, it is treated as the declaration itself.
/// The index is built on each call, so results never depend on earlier evaluations.
/// </remarks>
let findReferenceNodes (cursorNode: FplGenericNode) (nodes: seq<FplGenericNode>) =
    let target = cursorTargetNode cursorNode
    match target with
    | :? FplGenericJustificationItem ->
        // justification items: X.RefersTo.Value, X, and all N of the same F# class with N.RefersTo = X.RefersTo
        match target.RefersTo with
        | Some refersTo ->
            let sameClassReferences =
                nodes
                |> Seq.filter (fun node ->
                    node.GetType() = target.GetType()
                    && (match node.RefersTo with
                        | Some r -> obj.ReferenceEquals(r, refersTo)
                        | None -> false))
                |> List.ofSeq
            let seen = HashSet<FplGenericNode>(HashIdentity.Reference)
            (refersTo :: target :: sameClassReferences)
            |> List.filter (fun node -> seen.Add node)
        | None -> [target]
    | _ ->
        // proofs and corollaries are their own declarations here, all other nodes resolve as before
        let declaration =
            match target with
            | :? FplProof
            | :? FplCorollary -> target
            | _ -> defaultArg (resolveDeclaration target) target

        let parentNodes =
            match declaration with
            | :? FplProof
            | :? FplCorollary ->
                match declaration.Parent with
                | Some parent when not (parent :? FplRoot) && not (parent :? FplTheory) -> [parent]
                | _ -> []
            | _ -> []

        // nodes that refer to the declaration
        let referencing =
            nodes
            |> Seq.filter (fun node ->
                match node.RefersTo with
                | Some refersTo -> obj.ReferenceEquals(refersTo, declaration)
                | None -> false)
            |> List.ofSeq

        // proofs and corollaries embedded as children
        let children =
            nodes
            |> Seq.filter (fun node ->
                (node :? FplProof || node :? FplCorollary)
                && (match node.Parent with
                    | Some parent -> obj.ReferenceEquals(parent, declaration)
                    | None -> false))
            |> List.ofSeq

        let seen = HashSet<FplGenericNode>(HashIdentity.Reference)
        (parentNodes @ [declaration] @ children @ referencing)
        |> List.filter (fun node -> seen.Add node)
