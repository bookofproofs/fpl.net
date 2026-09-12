(* Copyright (c) 2021+ bookofproofs See LICENSE in the project root for license terms. *)

/// <summary>
/// This module contains helpers used in the Fpl.Interpreter.
/// </summary>
module Fpl.Interpreter.Helpers.Basic
open System.Collections.Generic
open System
open System.Text
open FParsec
open Fpl.Primitives
open Fpl.Errors.Messages
open Fpl.Errors.Emitter
open Fpl.Interpreter.BasicTypes
open Fpl.Interpreter.Helpers.Checks

/// <summary>
/// Helper container holding contextual state used during symbol-table construction and interpretation.
/// </summary>
type Helper() =
    let mutable _inSignatureEvaluation = false
    let mutable _inReferenceToProofOrCorollary = false
    // positions of the caller to prevent some diagnostics of being shown at the wrong position 
    let mutable _callerStartPos = Position("", 0,0,0)
    let mutable _callerEndPos = Position("", 0,0,0)
    let mutable _language = "tex" // the default language is tex, otherwise, it should be set in the FPL IDE extension

    let mutable _nextRunOrder = 0
    
    /// <summary>
    /// Indicates whether a signature on a building block is currently being evaluated.
    /// </summary>
    member this.InSignatureEvaluation
        with get () = _inSignatureEvaluation
        and set (value) = _inSignatureEvaluation <- value

    /// <summary>
    /// Current language identifier used when rendering localizations (e.g. "tex").
    /// </summary>
    member this.CurrentLanguage
        with get () = _language
        and set (value) = _language <- value

    /// <summary>
    /// Starting position of the caller used to adjust diagnostic positions.
    /// </summary>
    member this.CallerStartPos
        with get () = _callerStartPos
        and set (value) = _callerStartPos <- value

    /// <summary>
    /// Ending position of the caller used to adjust diagnostic positions.
    /// </summary>
    member this.CallerEndPos
        with get () = _callerEndPos
        and set (value) = _callerEndPos <- value

    /// <summary>
    /// Return the next available run-order integer used to schedule FPL building block execution.
    /// </summary>
    /// <returns>Monotonically increasing run-order identifier.</returns>
    member this.GetNextAvailableFplBlockRunOrder = 
        _nextRunOrder <- _nextRunOrder + 1
        _nextRunOrder

    /// <summary>
    /// Indicates whether the current evaluation stack is processing a reference to a proof or corollary.
    /// </summary>
    member this.InReferenceToProofOrCorollary
        with get () = _inReferenceToProofOrCorollary
        and set (value) = _inReferenceToProofOrCorollary <- value

/// <summary>
/// Append an expression node to its parent's argument list (or adjust parent fields for localizations).
/// </summary>
/// <param name="fplValue">Expression node to add to its parent.</param>
let addExpressionToParentArgList (fplValue:FplGenericNode) =
    let parent = fplValue.Parent.Value
    match parent.Name with 
    | LiteralLocL ->
        let identifier = fplValue.FplId
        parent.FplId <- identifier
        parent.TypeId <- identifier
    | _ -> ()
    parent.ArgList.Add fplValue

/// <summary>
/// Attach an expression to a referencing node or update the reference target when appropriate.
/// </summary>
/// <param name="fplValue">Expression node to add to a reference.</param>
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

/// <summary>
/// Try to add a for-statement domain/entity to its parent's for-statement argument list, emitting diagnostics if needed.
/// </summary>
/// <param name="fplValue">Node representing the for-statement domain or entity.</param>
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

/// <summary>
/// Attempt to insert a template node into the ultimate block's scope where it is used.
/// If a duplicate exists the existing template is reused and diagnostics may be emitted.
/// </summary>
/// <param name="templateNode">Template node to add.</param>
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

let tryAddSubBlockToFplBlock (fplValue:FplGenericNode) =
    let identifier = fplValue.Type SignatureType.Mixed
    let parent = fplValue.Parent.Value
    if parent.Scope.ContainsKey(identifier) then
        match box fplValue with
        | :? IHasSignature as hasSignature ->
            fplValue.ErrorOccurred <- emitID001Diagnostics identifier (parent.Scope[identifier].QualifiedStartPos) hasSignature.SignStartPos hasSignature.SignEndPos
        | _ ->
            fplValue.ErrorOccurred <- emitID001Diagnostics identifier (parent.Scope[identifier].QualifiedStartPos) fplValue.StartPos fplValue.EndPos
    else
        parent.Scope.Add(identifier, fplValue)

/// <summary>
/// Create a comma-separated parameter tuple string for `fv` based on its signature variables and the requested signature type.
/// </summary>
/// <param name="fv">Node whose parameters are to be listed.</param>
/// <param name="signatureType">Requested form of the signature (Name/Type/Mixed).</param>
/// <returns>Comma-separated parameter list string.</returns>
let getParamTuple (fv:FplGenericNode) (signatureType:SignatureType) =
        let propagate = propagateSignatureType signatureType
        fv.Scope
        |> Seq.filter (fun (kvp: KeyValuePair<string, FplGenericNode>) ->
            isSignatureVar kvp.Value
            || (isVar fv) && not (kvp.Value.IsClass())
            || fv.IsMapping())
        |> Seq.map (fun (kvp: KeyValuePair<string, FplGenericNode>) -> kvp.Value.Type(propagate))
        |> String.concat ", "

/// <summary>
/// Produce a string representation of a node's signature including its signature variables.
/// </summary>
/// <param name="fv">Node whose signature will be represented.</param>
/// <returns>Signature representation string.</returns>
let signatureRepresent (fv:FplGenericNode) = 
    let signatureVarRepresentations = 
        fv.GetVariables()
        |> List.filter (fun var -> isSignatureVar var) 
        |> List.map (fun var -> var.Represent())
        |> String.concat ", "
    $"{fv.FplId}({signatureVarRepresentations})"

/// <summary>
/// Search for a named symbol in upper scopes, ascending until a theory boundary is reached.
/// </summary>
/// <param name="fv1">Starting node to search from.</param>
/// <param name="name">Name to locate.</param>
/// <returns>ScopeSearchResult describing the lookup outcome.</returns>
let rec searchInUpperScopeByName (fv1: FplGenericNode) name =
    if fv1.Name = PrimTheoryL then 
        ScopeSearchResult.NotFound
    elif fv1.Scope.ContainsKey(name) then
        ScopeSearchResult.Found fv1.Scope[name]
    else
        searchInUpperScopeByName fv1.Parent.Value name

/// <summary>
/// Given a justification item and candidate targets, attempt to resolve the associated building block and
/// return detailed scope-search results describing success or semantic mismatches.
/// </summary>
/// <param name="fvJi">Justification item node.</param>
/// <param name="candidates">Candidate nodes to consider.</param>
/// <returns>ScopeSearchResult describing the best match or error case.</returns>
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
            |> List.map (fun fv -> $"`{fv.Name}` {fv.Type SignatureType.Mixed}")
            |> numbered
        )

/// <summary>
/// Resolve the block (theorem/axiom/conjecture) associated with a corollary using its parent theory scope.
/// </summary>
/// <param name="fplValue">Corollary node to resolve.</param>
/// <returns>ScopeSearchResult describing the associated block or error condition.</returns>
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
            |> Seq.filter (fun fv -> fv.Name <> PrimRoot && fv.Name <> PrimTheoryL && fv.Name <> PrimDefaultConstructor)
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

/// <summary>
/// Resolve the provable block associated with a proof node by searching the enclosing theory scope.
/// </summary>
/// <param name="fplValue">Proof node to resolve.</param>
/// <returns>ScopeSearchResult describing the associated provable block or error condition.</returns>
let tryFindAssociatedBlockForProof (fplValue: FplGenericNode) =
    match fplValue.Parent with
    | Some theory ->

        let flattenedScopes = flattenScopes theory.Parent.Value

        let potentialProvableName = stripLastDollarDigit (fplValue.FplId)

        // The parent node of the proof is the theory. In its scope
        // we should find the theorem we are looking for.
        let buildingBlocksMatchingDollarDigitNameList =
            // the potential block name of the proof is the
            // concatenated type signature of the name of the proof
            // without the last dollar digit
            flattenedScopes
            |> List.filter (fun fv -> fv.Name <> PrimRoot && fv.Name <> PrimTheoryL && fv.Name <> PrimDefaultConstructor)
            |> List.filter (fun fv -> fv.FplId = potentialProvableName)

        let provableBlocklist =
            buildingBlocksMatchingDollarDigitNameList
            |> List.filter (fun fv -> isProvable fv)

        let notProvableBlocklist =
            buildingBlocksMatchingDollarDigitNameList
            |> List.filter (fun fv -> not (isProvable fv ))

        if provableBlocklist.Length > 1 then
            ScopeSearchResult.FoundMultiple(
                provableBlocklist
                |> List.map (fun fv -> sprintf "'%s' %s" fv.Name (fv.Type(SignatureType.Mixed)))
                |> String.concat ", "
            )
        elif provableBlocklist.Length > 0 then
            let potentialTheorem = provableBlocklist.Head
            ScopeSearchResult.FoundAssociate potentialTheorem
        elif notProvableBlocklist.Length > 0 then
            let potentialOther = notProvableBlocklist.Head
            ScopeSearchResult.FoundIncorrectBlock potentialOther
        else
            ScopeSearchResult.NotFound
    | None -> ScopeSearchResult.NotApplicable

/// <summary>
/// Filter a list of candidate nodes to those matching the requested identifier and produce a human-readable
/// numbered list of candidate names for diagnostics.
/// </summary>
/// <param name="candidatesPre">Unfiltered candidate list.</param>
/// <param name="identifier">Identifier string to match against candidate FplId.</param>
/// <param name="qualified">When true produce qualified names in the returned listing.</param>
/// <returns>Tuple of (filteredCandidates, numberedCandidateNames).</returns>
let filterCandidates (candidatesPre:FplGenericNode list) identifier qualified =
    let candidates =
        candidatesPre
        |> List.filter (fun fv1 -> fv1.FplId = identifier)

    let candidatesNames =
        candidatesPre
        |> Seq.sortBy (fun fv -> $"{fv.Name}:{fv.FplId}")
        |> Seq.map (fun fv -> 
            if qualified then 
                qualifiedNameSimple fv 
            else
                $"`{fv.Type SignatureType.Mixed}`"
        )
        |> numbered
    (candidates, candidatesNames)

/// <summary>
/// Escape a string for safe JSON embedding (quotes, backslashes, control chars).
/// </summary>
/// <param name="s">Input string to escape.</param>
/// <returns>Escaped string suitable for JSON string content.</returns>
let escape (s: string) =
    if isNull s then String.Empty
    else
        let sb = StringBuilder()
        for c in s do
            match c with
            | '"'  -> sb.Append("\\\"") |> ignore
            | '\\' -> sb.Append("\\\\") |> ignore
            | '\b' -> sb.Append("\\b") |> ignore
            | '\f' -> sb.Append("\\f") |> ignore
            | '\n' -> sb.Append("\\n") |> ignore
            | '\r' -> sb.Append("\\r") |> ignore
            | '\t' -> sb.Append("\\t") |> ignore
            | c when System.Char.IsControl c -> sb.Append(sprintf "\\u%04x" (int c)) |> ignore
            | _    -> sb.Append(c) |> ignore
        sb.ToString()

/// <summary>
/// Produce an infix notation string for two-argument expressions, adding parentheses when elements are not simple.
/// </summary>
/// <param name="fv">Node whose arguments will be rendered.</param>
/// <param name="symbol">Infix symbol to use between the two arguments.</param>
/// <param name="signatureType">Signature form requested (Type/Name/Mixed).</param>
/// <param name="defaultVal">Default value to return when <c>SignatureType.Type</c> is requested.</param>
/// <returns>Rendered notation string.</returns>
let getNotationTwoArgs (fv:FplGenericNode) symbol signatureType defaultVal = 
    let separator = $" {symbol} "
    match signatureType with
    | SignatureType.Type -> defaultVal
    | _ ->
        fv.ArgList
        |> Seq.map (fun arg ->
            if isSimpleExpression arg then
                $"{arg.Type signatureType}"
            else
                $"({arg.Type signatureType})"
        )
        |> String.concat separator

/// <summary>
/// Return all items that occur before the given search item in a list, using reference equality.
/// </summary>
/// <param name="searchItem">Item to find.</param>
/// <param name="xs">List to search.</param>
/// <returns>List of items preceding <paramref name="searchItem"/> or empty list if not found.</returns>
let allBefore searchItem xs =
    let rec loop acc = function
        | [] ->
            // item not found → return []
            []
        | x :: _ when Object.ReferenceEquals(x, searchItem) ->
            // found the item → return reversed accumulator
            List.rev acc
        | x :: rest ->
            // keep collecting until we find the item
            loop (x :: acc) rest

    loop [] xs
