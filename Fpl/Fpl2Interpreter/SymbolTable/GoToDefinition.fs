/// <summary>
/// Resolves the declaration of a symbol-table node for the "go to definition" and "find references" services.
/// </summary>
module Fpl2Interpreter.SymbolTable.GoToDefinition

open System.Collections.Generic
open Fpl2Interpreter.BasicTypes
open Fpl2Interpreter.Helpers.Checks
open Fpl2Interpreter.SymbolTable.Types2.References
open Fpl2Interpreter.SymbolTable.ExpressionMatching
open Fpl2Interpreter.SymbolTable.Types1.TopLevel
open Fpl2Interpreter.SymbolTable.Types2.Definitions
open Fpl2Interpreter.SymbolTable.Types3.PredicativeBlocks
open Fpl2Interpreter.SymbolTable.Types3.RulesOfInferences
open Fpl2Interpreter.SymbolTable.Types3.DefinitionProperties
open Fpl2Interpreter.SymbolTable.Types3.Extensions
open Fpl2Interpreter.SymbolTable.Types4.Proofs
open Fpl2Interpreter.SymbolTable.Types2.Variables

/// <summary>
/// Returns the declaration node that the given node refers to, if one exists.
/// </summary>
/// <param name="node">Symbol-table node under the cursor.</param>
/// <returns>The declaration node, or <c>None</c> if it cannot be resolved.</returns>
/// <remarks>
/// Declaration kinds (theorem-like statements, blocks, functional terms, predicates, constructors, arguments and axioms)
/// resolve to themselves.
/// Justification items by reference or by proof argument resolve only through <c>RefersTo</c>.
/// Proofs and corollaries resolve to their parent node, unless the parent is the root.
/// All other nodes use <c>RefersTo</c> first. If <c>RefersTo</c> is missing and the identifier starts with a
/// lowercase letter, the node is looked up in the scope of its ultimate block node.
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
    | :? FplConstructor
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
/// <returns>The inverse index. Nodes that do not resolve to a declaration are not contained in it.</returns>
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
/// <returns>The referencing nodes, or an empty list if none exist.</returns>
/// <remarks>
/// This function is not used by <c>findReferenceNodes</c>, which scans the nodes directly.
/// </remarks>
let findReferences (index: ReferenceIndex) (declaration: FplGenericNode) : FplGenericNode list =
    match index.TryGetValue declaration with
    | true, references -> List.ofSeq references
    | _ -> []

/// <summary>
/// Maps the node under the cursor to the node that the "go to definition" and "find references" services act on.
/// </summary>
/// <param name="cursorNode">Innermost symbol-table node containing the cursor.</param>
/// <returns>
/// The argument that owns a justification, the <c>RefersTo</c> target of a reference, or the cursor node itself.
/// </returns>
/// <remarks>
/// A justification has no identifier of its own. A cursor on a justification therefore refers to the
/// argument it belongs to, which carries the identifier that is resolved.
/// A reference with an unresolved <c>RefersTo</c> is returned unchanged.
/// </remarks>
let cursorTargetNode (cursorNode: FplGenericNode) : FplGenericNode =
    match cursorNode with
    | :? FplJustification ->
        match cursorNode.Parent with
        | Some argument -> argument
        | None -> cursorNode
    | :? FplReference ->
        match cursorNode.RefersTo with
        | Some ref -> ref
        | None -> cursorNode
    | _ -> cursorNode


/// <summary>
/// Returns true if both nodes belong to the same justification kind for find-references purposes.
/// Argument references by ref-argument and by proof-argument are treated as the same kind.
/// </summary>
/// <param name="a">First node to compare.</param>
/// <param name="b">Second node to compare.</param>
/// <returns><c>true</c> if both nodes are argument references of any kind, or if they have the same runtime type.</returns>
let private isSameJustificationKind (a: FplGenericNode) (b: FplGenericNode) : bool =
    let isArgumentReference (node: FplGenericNode) =
        match node with
        | :? FplJustificationItemByRefArgument
        | :? FplJustificationItemByProofArgument -> true
        | _ -> false
    if isArgumentReference a && isArgumentReference b then
        true
    else
        a.GetType() = b.GetType()

/// <summary>
/// Returns the nodes to report for "find references" at the given cursor node.
/// </summary>
/// <param name="cursorNode">Innermost symbol-table node containing the cursor.</param>
/// <param name="nodes">All flattened symbol-table nodes of the current evaluation.</param>
/// <returns>The declaration followed by all nodes resolving to it, without duplicates.</returns>
/// <remarks>
/// The cursor target is resolved by <c>cursorTargetNode</c>.
/// Justification items: the resolved declaration (<c>RefersTo</c>), the target itself, and all nodes of the same
/// justification kind or plain references that point to that declaration. If the item has no <c>RefersTo</c>,
/// only the target is returned.
/// Other targets: the declaration (the target itself if it does not resolve), its proof or corollary parents
/// (excluding root and theory nodes), all variables whose type occurrence refers to it, all nodes whose
/// <c>RefersTo</c> is the declaration, and proofs or corollaries embedded as its children.
/// No index is built here. The nodes are scanned on each call, so results never depend on earlier evaluations.
/// </remarks>
let findReferenceNodes (cursorNode: FplGenericNode) (nodes: seq<FplGenericNode>) : FplGenericNode list =
    let target = cursorTargetNode cursorNode
    match target with
    | :? FplGenericJustificationItem ->
        // justification items: X.RefersTo.Value, X, and all N of the same kind with N.RefersTo = X.RefersTo
        match target.RefersTo with
        | Some refersTo ->
            let sameKindReferences =
                nodes
                |> Seq.filter (fun node ->
                    (isSameJustificationKind target node || (node :? FplReference))
                    && (match node.RefersTo with
                        | Some r -> obj.ReferenceEquals(r, refersTo)
                        | None -> false))
                |> List.ofSeq
            let seen = HashSet<FplGenericNode>(HashIdentity.Reference)
            (refersTo :: target :: sameKindReferences)
            |> List.filter (fun node -> seen.Add node)
        | None -> [target]
    | _ ->
        // proofs and corollaries are their own declarations here, all other nodes resolve as before
        let declaration =
            match target with
            | :? FplGenericVariable
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

        // nodes that refer to the declaration; variables are excluded because their RefersTo is
        // their declared type, which is reported by the type occurrence node at the type identifier
        let typeOccurences = HashSet<FplGenericNode>(HashIdentity.Reference)
        let referencingPre =
            nodes
            |> Seq.filter (fun node ->
                match node with
                | :? FplVariable as var ->
                    match var.TypeOccurrence with
                    | Some typeOcc when typeOcc.RefersTo.IsSome && obj.ReferenceEquals(typeOcc.RefersTo.Value, declaration) ->
                        typeOccurences.Add typeOcc |> ignore
                        false
                    | _ -> false
                | _ ->
                    match node.RefersTo with
                    | Some refersTo -> obj.ReferenceEquals(refersTo, declaration)
                    | None -> false)
            |> Seq.toList
        let referencing = referencingPre @ (typeOccurences |> Seq.toList)

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
