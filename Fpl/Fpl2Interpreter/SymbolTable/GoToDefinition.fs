/// <summary>
/// Resolves the declaration of a symbol-table node for the "go to definition" service.
/// </summary>
module Fpl2Interpreter.SymbolTable.GoToDefinition

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
