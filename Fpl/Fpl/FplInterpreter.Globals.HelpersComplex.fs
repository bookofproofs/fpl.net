/// This module contains functions helping the FplInterpreter
/// to embed nodes into the symbol table 

(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)
module FplInterpreter.Globals.HelpersComplex
open System.Collections.Generic
open FplPrimitives
open FplInterpreterDiagnosticsEmitter
open FplInterpreterBasicTypes
open FplInterpreter.Globals.Heap
open FplInterpreterChecks


/// Checks if a reference to a Symbol, Prefix, PostFix, or Infix exists
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
            |> Seq.filter (fun fv1 -> isDefinition fv1)
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
                        fv.TypeId <- block.TypeId
                | _ -> ()))

        if fv.RefersTo.IsNone then
            fv.ErrorOccurred <- emitSIG01Diagnostics expressionId fv.StartPos fv.EndPos
    | _ -> ()

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

// Tries to add an FPL block to its parent's scope using its FplId, or issues ID001 diagnostics if a conflict occurs
let tryAddToParentUsingFplId (fplValue:FplGenericNode) =
    let identifier = fplValue.FplId
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
        fplValue.ErrorOccurred <- emitID001Diagnostics identifier (conflicts.Head.QualifiedStartPos) fplValue.StartPos fplValue.EndPos
    else
        let parent = fplValue.Parent.Value
        parent.Scope.Add(identifier, fplValue)

// Tries to add an FPL block to its parent's scope using its typed signature, or issues ID001 diagnostics if a conflict occurs
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

// Tries to add an FPL block to its parent's scope using its mixed signature, or issues ID001 diagnostics if a conflict occurs
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
        fplValue.ErrorOccurred <- emitID001Diagnostics identifier (conflicts.Head.QualifiedStartPos) fplValue.StartPos fplValue.EndPos
    else
        let parent = fplValue.Parent.Value
        parent.Scope.Add(identifier, fplValue)

/// Looks for all declared building blocks with a specific name.
let findCandidatesByName (node: FplGenericNode) (name: string) withClassConstructors withCorollariesOrProofs =
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
        if withCorollariesOrProofs && not (name.Contains("$")) then 
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

                if withCorollariesOrProofs && (isProvable block) then 
                    flattenCorollariesAndProofs block
            )
        )
        |> ignore

    pm |> Seq.toList

