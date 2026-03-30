/// This module contains functions helping the FplInterpreter
/// to embed nodes into the symbol table 

(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)
module FplInterpreterSTEmbedding
open System.Collections.Generic
open FplPrimitives
open FplInterpreterDiagnosticsEmitter
open FplInterpreterBasicTypes
open FplInterpreterGlobals
open FplInterpreterChecks

// Returns the root node of any FplValue casted to FplRoot
let rec getRoot (fv:FplGenericNode) = (root fv) :?> FplRoot

// Tries to add for statement's domain or entity to its parent's for statement
let tryAddToParentForInStmt (fplValue:FplGenericNode) =
    let identifier = fplValue.Type SignatureType.Name
    let parent = fplValue.Parent.Value

    if parent.ArgList.Count = 1 then
        let entityIdentifier = parent.ArgList[0].Type SignatureType.Name
        if entityIdentifier = identifier then 
            fplValue.ErrorOccurred <- emitID027Diagnostics identifier fplValue.StartPos fplValue.EndPos
        else 
            parent.ArgList.Add fplValue
    else
        parent.ArgList.Add fplValue

// Tries to add a template to the ultimate block's scope, inside which it was used.
let tryAddTemplateToParent (templateNode:FplGenericNode) =
    let identifier = templateNode.FplId
    let nextOpt = templateNode.UltimateBlockNode // the scope of all templates ís inside the ultimate block
    match nextOpt with 
    | Some next when not (next.Scope.ContainsKey identifier) -> 
        next.Scope.Add(identifier, templateNode)
        // correct the parent of the template
        templateNode.Parent <- Some next
    | Some next -> 
        // template was already added to the ultimate node
        let templateAlreadyInScope = next.Scope[identifier] // return the templateNode that was already added instead of the input 
        match templateNode.Parent with 
        | Some var -> 
            // Replace the variable's newly created template type by 
            // the template already in the scope of its ultimate node
            // making sure that only one template with the same identifier will store 
            // its actual type to issue SIG12 diagnostics properly
            var.RefersTo <- Some templateAlreadyInScope 
        | _ -> () // should never occur, since only the root has no parent
    | _ ->  
        // should never occur, since FPL's syntax 
        // does not allow template without UltimateBlocks
        ()

// Tries to add an FPL block to its parent's scope using its FplId, or issues ID001 diagnostics if a conflict occurs
let tryAddToParentUsingFplId (fplValue:FplGenericNode) =
    let identifier = fplValue.FplId
    let root = getRoot fplValue
    let conflicts = 
        root.OrderedTheories
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

// Tries to add a constructor or property to it's parent FPL block's scope using its mixed signature, or issues ID001 diagnostics if a conflict occurs
let tryAddSubBlockToFplBlock (fplValue:FplGenericNode) =
    let identifier = fplValue.Type SignatureType.Mixed
    let parent = fplValue.Parent.Value
    if parent.Scope.ContainsKey(identifier) then 
        fplValue.ErrorOccurred <- emitID001Diagnostics identifier (parent.Scope[identifier].QualifiedStartPos) fplValue.StartPos fplValue.EndPos
    else
        parent.Scope.Add(identifier, fplValue)   

// Tries to add an FPL block to its parent's scope using its typed signature, or issues ID001 diagnostics if a conflict occurs
let tryAddToParentUsingTypedSignature (fplValue:FplGenericNode) =
    let identifier = fplValue.Type SignatureType.Type
    let root = getRoot fplValue
    let conflicts = 
        root.OrderedTheories
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

// Adds an expression to Parent's argument list
let addExpressionToParentArgList (fplValue:FplGenericNode) =
    let parent = fplValue.Parent.Value
    match parent.Name with 
    | LiteralLocL ->
        let identifier = fplValue.FplId
        parent.FplId <- identifier
        parent.TypeId <- identifier
    | _ -> ()
    parent.ArgList.Add fplValue

type IHasDotted = 
    abstract member DottedChild : FplGenericNode option with get, set

// Add an expression to a reference
let addExpressionToReference (fplValue:FplGenericNode) =
    let nextOpt = fplValue.Parent
    match box nextOpt with 
    | :? IHasDotted as dc when dc.DottedChild.IsSome -> ()
    | _ ->
        match nextOpt with
        | Some next when next.Name = PrimRefL && next.RefersTo.IsSome ->
            let referenced = next.RefersTo.Value
            
            match referenced.Name with 
            | PrimVariableArrayL 
            | LiteralParent 
            | LiteralSelf ->
                next.ArgList.Add fplValue
            | _ ->
                next.FplId <- fplValue.FplId
                next.TypeId <- fplValue.TypeId
                next.RefersTo <- Some fplValue
        | Some next when next.Name = PrimRefL && 
            (
                fplValue.Name = PrimDelegateEqualL 
             || fplValue.Name = PrimDelegateDecrementL
             ) ->
            next.FplId <- fplValue.FplId
            next.TypeId <- fplValue.TypeId
            next.RefersTo <- Some fplValue
        | Some next when next.Name = PrimRefL ->
            next.FplId <- fplValue.FplId
            next.TypeId <- fplValue.TypeId
            next.RefersTo <- Some fplValue 
            next.ErrorOccurred <- fplValue.ErrorOccurred
        | _ -> addExpressionToParentArgList fplValue 

// Tries to add an FPL block to its parent's scope using its mixed signature, or issues ID001 diagnostics if a conflict occurs
let tryAddToParentUsingMixedSignature (fplValue:FplGenericNode) =
    let identifier = fplValue.Type SignatureType.Mixed
    let root = getRoot fplValue
    let conflicts = 
        root.OrderedTheories
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

/// Generates a string of parameters based on SignatureType
let getParamTuple (fv:FplGenericNode) (signatureType:SignatureType) =
        let propagate = propagateSignatureType signatureType
        fv.Scope
        |> Seq.filter (fun (kvp: KeyValuePair<string, FplGenericNode>) ->
            isSignatureVar kvp.Value
            || (isVar fv) && not (kvp.Value.IsClass())
            || fv.IsMapping())
        |> Seq.map (fun (kvp: KeyValuePair<string, FplGenericNode>) -> kvp.Value.Type(propagate))
        |> String.concat ", "

let signatureRepresent (fv:FplGenericNode) = 
    let signatureVarRepresentations = 
        fv.GetVariables()
        |> List.filter (fun var -> isSignatureVar var) 
        |> List.map (fun var -> var.Represent())
        |> String.concat ", "
    $"{fv.FplId}({signatureVarRepresentations})"

let rec searchInUpperScopeByName (fv1: FplGenericNode) name =
    if fv1.Name = PrimTheoryL then 
        ScopeSearchResult.NotFound
    elif fv1.Scope.ContainsKey(name) then
        ScopeSearchResult.Found fv1.Scope[name]
    else
        searchInUpperScopeByName fv1.Parent.Value name

/// Tries to find a theorem-like statement, an axiom or a corollary
/// and returns different cases of ScopeSearchResult, depending on different semantical error situations.
let tryFindAssociatedBlockForJustificationItem (fvJi: FplGenericNode) (candidates:FplGenericNode list) =
    match candidates.Length with
    | 1 ->  // exactly one candidate found
        let potentialCandidate = candidates.Head
        match fvJi.Name, potentialCandidate.Name with
        | PrimJIByProofArgument, LiteralPrfL
        | PrimJIByDef, PrimClassL
        | PrimJIByDef, PrimPredicateL
        | PrimJIByDef, PrimFunctionalTermL
        | PrimJIByDef, PrimVariableL
        | PrimJIByConj, LiteralConjL
        | PrimJIByCor, LiteralCorL
        | PrimJIByAx, LiteralAxL
        | PrimJIByInf, PrimRuleOfInference
        | PrimJIByTheoremLikeStmt, LiteralThmL 
        | PrimJIByTheoremLikeStmt, LiteralPropL
        | PrimJIByTheoremLikeStmt, LiteralLemL ->
            ScopeSearchResult.FoundAssociate potentialCandidate
        | _ ->
            ScopeSearchResult.FoundIncorrectBlock potentialCandidate
    | 0 -> ScopeSearchResult.NotFound
    | _ -> 
        // multiple candidates found
        ScopeSearchResult.FoundMultiple(
            candidates
            |> List.map (fun fv -> sprintf "'%s' %s" fv.Name (fv.Type(SignatureType.Mixed)))
            |> String.concat ", "
        )

/// Tries to find a theorem-like statement, a conjecture, or an axiom for a corollary
/// and returns different cases of ScopeSearchResult, depending on different semantical error situations.
let tryFindAssociatedBlockForCorollary (fplValue: FplGenericNode) =
    match fplValue.Parent with
    | Some theory ->

        let flattenedScopes = flattenScopes theory.Parent.Value

        // The parent node of the proof is the theory. In its scope
        // we should find the theorem we are looking for.
        let buildingBlocksMatchingDollarDigitNameList =
            // the potential theorem name of the corollary is the
            // concatenated type signature of the name of the corollary
            // without the last dollar digit
            let potentialBlockName = stripLastDollarDigit (fplValue.Type(SignatureType.Mixed))

            flattenedScopes
            |> Seq.filter (fun fv -> fv.FplId = potentialBlockName)
            |> Seq.toList

        let potentialBlockList =
            buildingBlocksMatchingDollarDigitNameList
            |> List.filter (fun fv -> isProvable fv || isAxiomOrConnjecture fv)

        let notPotentialBlockList =
            buildingBlocksMatchingDollarDigitNameList
            |> List.filter (fun fv ->
                not (
                    isProvable fv || isAxiomOrConnjecture fv
                ))

        if potentialBlockList.Length > 1 then
            ScopeSearchResult.FoundMultiple(
                potentialBlockList
                |> List.map (fun fv -> sprintf "'%s' %s" fv.Name (fv.Type(SignatureType.Mixed)))
                |> String.concat ", "
            )
        elif potentialBlockList.Length > 0 then
            let potentialTheorem = potentialBlockList.Head
            ScopeSearchResult.FoundAssociate potentialTheorem
        elif notPotentialBlockList.Length > 0 then
            let potentialOther = notPotentialBlockList.Head
            ScopeSearchResult.FoundIncorrectBlock potentialOther
        else
            ScopeSearchResult.NotFound
    | None -> ScopeSearchResult.NotApplicable


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
        let root = getRoot node
        root.OrderedTheories // iterate all theories
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

let filterCandidates (candidatesPre:FplGenericNode list) identifier qualified =
    let candidates =
        candidatesPre
        |> List.filter (fun fv1 -> fv1.FplId = identifier)

    let candidatesNames =
        candidatesPre
        |> Seq.sortBy (fun fv -> $"{fv.Name}:{fv.FplId}")
        |> Seq.map (fun fv -> 
            if qualified then 
                qualifiedName fv false
            else
                $"`{fv.Type SignatureType.Mixed}`"
        )
        |> Seq.mapi (fun i s -> 
            if candidatesPre.Length > 1 then 
                sprintf "%d) %s" (i + 1) s
            else
                sprintf "%s" s
        )
        |> String.concat ", "
    (candidates, candidatesNames)
