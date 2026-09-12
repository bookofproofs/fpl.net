(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)

/// <summary>
/// Utility functions to embed and resolve nodes into the interpreter symbol table.
/// </summary>
/// <remarks>
/// Functions in this module perform signature checks, conflict diagnostics, candidate
/// lookups and extraction of assertion/predicate expressions from definition nodes.
/// They operate on the global `heap` and raise or mark diagnostics via the emitter helpers.
/// </remarks>
module Fpl.Interpreter.SymbolTable.Storage.Util
open System.Collections.Generic
open Fpl.Primitives
open Fpl.Errors.Emitter
open Fpl.Interpreter.BasicTypes
open Fpl.Interpreter.Helpers.Checks
open Fpl.Interpreter.SymbolTable.Storage.Heap


/// <summary>
/// Checks whether a reference to a symbol, prefix, postfix or infix operator exists in any loaded theory.
/// </summary>
/// <param name="fv">The reference node to resolve.</param>
/// <returns>Unit; attaches SIG01 diagnostic flag to the node when resolution fails.</returns>
let checkSIG01Diagnostics (fv: FplGenericNode) =
    match fv.Name with
    | PrimRefL ->

        // collect candidates to match this reference from all theories and
        // add them to fplValues's scope
        let expressionId = fv.FplId

        heap.Root.Scope
        |> Seq.map (fun kv -> kv.Value)
        |> Seq.iter (fun theory ->
            theory.Scope
            |> Seq.map (fun kv -> kv.Value)
            |> Seq.filter isDefinition 
            |> Seq.iter (fun block ->
                match block.ExpressionType with
                | FixType.Prefix symbol
                | FixType.Symbol symbol
                | FixType.Postfix symbol ->
                    if expressionId = symbol then
                        fv.RefersTo <- Some block 
                        fv.TypeId <- block.TypeId
                | FixType.Infix(symbol, precedence) ->
                    if expressionId = symbol then
                        fv.RefersTo <- Some block
                        // replace the syntax infix operator by the semantics of the infix operator
                        // (including actual precedence)
                        fv.ExpressionType <- block.ExpressionType 
                        fv.TypeId <- block.TypeId
                | _ -> ()))

        if fv.RefersTo.IsNone then
            fv.ErrorOccurred <- emitSIG01Diagnostics expressionId fv.StartPos fv.EndPos
    | _ -> ()

/// <summary>
/// Validates that an infix operator precedence does not conflict with other declared precedences.
/// </summary>
/// <param name="fv">The node being checked (used for diagnostic attachment).</param>
/// <param name="symbol">Symbol string of the infix operator.</param>
/// <param name="precedence">Numeric precedence value to check.</param>
/// <param name="pos1">Qualified start position of the conflicting declaration.</param>
/// <param name="pos2">Qualified end position of the conflicting declaration.</param>
/// <returns>Unit; attaches SIG02 diagnostics to the conflicting node when necessary.</returns>
let checkSIG02Diagnostics (fv:FplGenericNode) symbol precedence pos1 pos2 = 
    let precedences = Dictionary<int, FplGenericNode>()
    let precedenceWasAlreadyThere precedence fv =
        if not (precedences.ContainsKey(precedence)) then
            precedences.Add(precedence, fv)
            false
        else
            true
    heap.Root.Scope
    |> Seq.map (fun kv -> kv.Value)
    |> Seq.iter (fun theory ->
        theory.Scope
        |> Seq.map (fun kv1 -> kv1.Value)
        |> Seq.iter (fun block ->
            match block.ExpressionType with
            | FixType.Infix(_, precedence) -> precedenceWasAlreadyThere precedence block |> ignore
            | _ -> ()))
    if precedences.ContainsKey(precedence) then
        let conflict = precedences[precedence].QualifiedStartPos
        precedences[precedence].ErrorOccurred <- emitSIG02Diagnostics symbol precedence conflict pos1 pos2

/// <summary>
/// Attempts to add an FPL block to its parent's scope using the block's typed signature.
/// </summary>
/// <param name="fplValue">Node to add to its parent's scope.</param>
/// <remarks>
/// On conflict, emits ID024 diagnostics and marks the node as erroneous instead of adding it.
/// </remarks>
let tryAddToParentUsingTypedSignature (fplValue:FplGenericNode) =
    let identifier = fplValue.Type SignatureType.Type
    let conflicts = 
        heap.Root.OrderedTheories
        |> Seq.map (fun theory -> 
            theory.Scope.Values
            |> Seq.filter (fun fv -> fv.Type SignatureType.Type = identifier)
        )
        |> Seq.concat
        |> Seq.toList

    if conflicts.Length > 0 then 
        fplValue.ErrorOccurred <- emitID024Diagnostics identifier (conflicts.Head.QualifiedStartPos) fplValue.StartPos fplValue.EndPos
    else
        let parent = fplValue.Parent.Value
        parent.Scope.Add(identifier, fplValue)

/// <summary>
/// Attempts to add an FPL block to its parent's scope using its mixed signature.
/// </summary>
/// <param name="fplValue">Node to add to its parent's scope.</param>
/// <remarks>
/// On conflict, emits ID001 diagnostics and marks the node as erroneous instead of adding it.
/// </remarks>
let tryAddToParentUsingMixedSignature (fplValue:FplGenericNode) =
    let identifier = fplValue.Type SignatureType.Mixed
    let conflicts = 
        heap.Root.OrderedTheories
        |> Seq.map (fun theory -> 
            theory.Scope
            |> Seq.filter (fun kvp -> kvp.Key = identifier)
            |> Seq.map (fun kvp -> kvp.Value)
        )
        |> Seq.concat
        |> Seq.toList

    if conflicts.Length > 0 then
        match box fplValue with
        | :? IHasSignature as hasSignature ->
            fplValue.ErrorOccurred <- emitID001Diagnostics identifier (conflicts.Head.QualifiedStartPos) hasSignature.SignStartPos hasSignature.SignEndPos
        | _ ->
            fplValue.ErrorOccurred <- emitID001Diagnostics identifier (conflicts.Head.QualifiedStartPos) fplValue.StartPos fplValue.EndPos
    else
        let parent = fplValue.Parent.Value
        parent.Scope.Add(identifier, fplValue)

/// <summary>
/// Finds all declared building blocks that match a specific name.
/// </summary>
/// <param name="name">FPL identifier or reference to search for.</param>
/// <param name="withClassConstructors">If true, include class constructors from matching classes.</param>
/// <param name="withCorollariesOrProofs">If true, include corollaries and proof nodes related to matches.</param>
/// <returns>List of matching <see cref="FplGenericNode"/> candidates (may be empty).</returns>
let findCandidatesByName (name: string) withClassConstructors withCorollariesOrProofs =
    let pm = List<FplGenericNode>()

    let rec flattenCorollariesAndProofs (tls:FplGenericNode) =
        tls.Scope.Values
        |> Seq.iter (fun fv -> 
            match fv.Name with
            | LiteralPrfL -> pm.Add(fv)
            | LiteralCorL -> 
                pm.Add(fv)
                flattenCorollariesAndProofs fv
            | _ -> ()
        )
    let nameWithoutProofOrCorRef = 
        if withCorollariesOrProofs && name.Contains("$") then 
            let parts = name.Split('$')
            parts.[0] 
        else
            name

    let nameWithProofOrCorRef = 
        if not (name.Contains("$")) then
            $"{name}$"
        else
            name

    if isUpper name then
        heap.Root.OrderedTheories // iterate all theories
        |> Seq.iter (fun theory ->
            theory.Scope
            // filter only blocks starting with the same FplId as the reference
            |> Seq.map (fun kvp -> kvp.Value)
            |> Seq.filter (fun fv -> 
                fv.FplId = name 
                || fv.FplId = nameWithoutProofOrCorRef 
                || $"{fv.FplId}$".StartsWith nameWithProofOrCorRef) 
            |> Seq.iter (fun (block: FplGenericNode) ->
                pm.Add(block)

                if withClassConstructors && block.IsClass() then
                    block.Scope
                    |> Seq.map (fun kvp -> kvp.Value)
                    |> Seq.filter (fun (fv: FplGenericNode) -> (fv.Name = LiteralCtorL || fv.Name = PrimDefaultConstructor))
                    |> Seq.iter (fun (fv: FplGenericNode) -> pm.Add(fv))

                if withCorollariesOrProofs && (isProvable block || isAxiomOrConnjecture block) then 
                    flattenCorollariesAndProofs block
            )
        )
        |> ignore


    pm
    |> Seq.toList


/// <summary>
/// Attempts to add an FPL block to its parent's scope using its `FplId`.
/// </summary>
/// <param name="fplValue">Node to add to its parent's scope.</param>
/// <remarks>
/// On conflict emits ID001 diagnostics and marks the node as erroneous instead of adding it.
/// The check considers both global conflicts and those already present in the parent scope.
/// </remarks>
let tryAddToParentUsingFplId (fplValue:FplGenericNode) =
    let identifier = fplValue.FplId
    let conflictsPre =
        heap.Root.OrderedTheories
        |> Seq.map (fun theory -> 
            theory.Scope
            |> Seq.filter (fun kvp -> kvp.Key = identifier)
            |> Seq.map (fun kvp -> kvp.Value)
        )
        |> Seq.concat
        |> Seq.toList

    let parent = fplValue.Parent.Value
    let conflicts = conflictsPre @ (parent.Scope.Values |> Seq.filter (fun fv -> (fv.Type SignatureType.Name) = identifier) |> Seq.toList)
    if conflicts.Length > 0 then 
        match box fplValue with
        | :? IHasSignature as hasSignature ->
            fplValue.ErrorOccurred <- emitID001Diagnostics identifier (conflicts.Head.QualifiedStartPos) hasSignature.SignStartPos hasSignature.SignEndPos
        | _ ->
            fplValue.ErrorOccurred <- emitID001Diagnostics identifier (conflicts.Head.QualifiedStartPos) fplValue.StartPos fplValue.EndPos
    else
        parent.Scope.Add(identifier, fplValue)

/// <summary>
/// Extracts all assertion expressions declared inside a definition node.
/// </summary>
/// <param name="def">Definition node to inspect.</param>
/// <returns>List of assertion expression nodes; empty if none present.</returns>
let extractAssertionExpressions (def:FplGenericNode) =
    def.ArgList
    |> Seq.filter (fun fv -> fv.Name = PrimAssertion) // extract assertions
    |> Seq.map (fun fv -> fv.ArgList |> Seq.tryHead) // extract expressions of the assertions
    |> Seq.filter (fun fv -> fv.IsSome)
    |> Seq.map (fun fv -> fv.Value)
    |> Seq.toList // returns list of expressions of assertions in the definition

/// <summary>
/// Extracts expressions of all predicative properties from a definition node.
/// </summary>
/// <param name="def">Definition node to inspect.</param>
/// <returns>List of predicative property expression nodes; empty if none present.</returns>
let extractPredicativePropertiesExpressions (def:FplGenericNode) =
    def.Scope.Values
    |> Seq.filter (fun fv -> fv.Name = PrimMandatoryPredicateL) // extract predicative property
    |> Seq.map (fun fv -> fv.ArgList |> Seq.tryLast) // extract expressions of the properties
    |> Seq.filter (fun fv -> fv.IsSome)
    |> Seq.map (fun fv -> fv.Value)
    |> Seq.toList // returns list of expressions of predicative properties in the definition

/// <summary>
/// Extracts the predicate expression from a predicate definition as a single-element list.
/// </summary>
/// <param name="def">The node representing a predicate definition.</param>
/// <returns>
/// A single-element list containing the predicate expression when the node is a predicate definition;
/// otherwise an empty list.
/// </returns>
let extractPredicateDefinitionExpressions (def:FplGenericNode) =
    match def.Name with
    | PrimPredicateL ->
        let lastExprOpt = def.ArgList |> Seq.tryLast 
        match lastExprOpt with
        | Some lastExpr ->
            [lastExpr]
        | _ -> []
    | _ -> []

