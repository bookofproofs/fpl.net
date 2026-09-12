(* Copyright (c) 2021+ bookofproofs See LICENSE in the project root for license terms. *)

/// <summary>
/// This module contains all functions used in the Fpl.Interpreter namespace
/// for type and consistency checking.
/// </summary>
module Fpl.Interpreter.Helpers.Checks
open System
open Fpl.Errors.Messages
open Fpl.Errors.Emitter
open Fpl.Primitives
open Fpl.Interpreter.BasicTypes

/// <summary>
/// Return a pair consisting of the argument's type string and a boolean indicating whether the argument is a predicate.
/// </summary>
/// <param name="arg">Node representing the argument to inspect.</param>
/// <returns>Tuple of (typeString, isPredicate).</returns>
let isArgPred (arg:FplGenericNode) = 
    let argType = arg.Type SignatureType.Type
    (argType, argType.StartsWith(LiteralPred))

/// <summary>
/// Verify that an argument is a predicate and emit LG001 diagnostics on failure.
/// </summary>
/// <param name="fv">The enclosing FPL node that will receive diagnostics.</param>
/// <param name="arg">Argument node to check.</param>
/// <remarks>
/// Localizations are skipped by this check.
/// </remarks>
let checkArgPred (fv:FplGenericNode) (arg:FplGenericNode)  = 
    match fv.UltimateBlockNode with 
    | Some node when node.Name = LiteralLocL -> () // skip this check for localizations
    | _ ->
        let argType, isPred = isArgPred (arg:FplGenericNode) 
        if isPred then 
            () 
        else
            let argName = arg.Type SignatureType.Name
            fv.ErrorOccurred <- emitLG001Diagnostics argType argName fv.Name arg.StartPos arg.StartPos

/// <summary>
/// Determine whether the given node represents a quantifier.
/// </summary>
/// <param name="arg">Node to test.</param>
/// <returns>True when the node is a quantifier.</returns>
let isQuantifier (arg:FplGenericNode) =
    match arg.Name with 
    | PrimQuantifierAll
    | PrimQuantifierExists
    | PrimQuantifierExistsN -> true
    | _ -> false

/// <summary>
/// Determine whether the given node is a compound predicate operator.
/// </summary>
/// <param name="arg">Node to test.</param>
/// <returns>True when the node is a compound predicate.</returns>
let isCompoundPredicate (arg:FplGenericNode) =
    match arg.Name with 
    | PrimExclusiveOr
    | PrimDisjunction
    | PrimNegation
    | PrimImplication
    | PrimEquivalence
    | PrimConjunction
    | PrimQuantifierAll
    | PrimQuantifierExists
    | PrimQuantifierExistsN
    | PrimIsOperator -> true
    | _ -> false

/// <summary>
/// Ensure the last argument of a node that should be a predicate indeed returns a predicate.
/// </summary>
/// <param name="fv">Node whose last argument will be validated.</param>
let checkPredicateExpressionReturnsPredicate (fv:FplGenericNode) =
    let exprOpt = fv.ArgList |> Seq.tryLast
    match exprOpt with 
    | Some expr -> checkArgPred fv expr
    | None -> ()

/// <summary>
/// Indicates whether a node is callable with parameters (expects argument list invocation).
/// </summary>
/// <param name="fv">Node to query.</param>
/// <returns>True if the node is callable with parameters.</returns>
let isCallableWithParams (fv:FplGenericNode) =
    match fv.Name with
    | LiteralCtorL
    | PrimDefaultConstructor
    | PrimBaseConstructorCall
    | PrimExtensionL
    | PrimPredicateL
    | PrimFunctionalTermL
    | PrimMandatoryFunctionalTermL
    | PrimMandatoryPredicateL -> true
    | _ -> false

/// <summary>
/// Indicates whether a node represents a building block callable without parameters.
/// </summary>
/// <param name="fv">Node to query.</param>
/// <returns>True if the node is callable without parameters.</returns>
let isCallableWithoutParams (fv:FplGenericNode) =
    match fv.Name with
    | LiteralAxL
    | LiteralConjL
    | LiteralCorL
    | LiteralPrfL
    | LiteralLemL
    | LiteralPropL
    | LiteralThmL -> true
    | _ -> false

/// <summary>
/// Test whether the node is the root of the symbol table.
/// </summary>
/// <param name="fv">Node to test.</param>
/// <returns>True for the root node.</returns>
let isRoot (fv:FplGenericNode) = 
    match fv.Name with
    | PrimRoot -> true
    | _ -> false

/// <summary>
/// Test whether the node represents a theory.
/// </summary>
/// <param name="fv">Node to test.</param>
/// <returns>True for theory nodes.</returns>
let isTheory (fv:FplGenericNode) = 
    match fv.Name with
    | PrimTheoryL -> true
    | _ -> false

/// <summary>
/// Determine whether the node denotes a definition-like construct (class, predicate, or functional term).
/// </summary>
/// <param name="fv1">Node to test.</param>
/// <returns>True for definition nodes.</returns>
let isDefinition (fv1:FplGenericNode) =
    match fv1.Name with
    | PrimClassL
    | PrimPredicateL
    | PrimFunctionalTermL -> true
    | _ -> false

/// <summary>
/// Check whether the node represents a provable building block (theorem, lemma, proposition, corollary).
/// </summary>
/// <param name="fv">Node to test.</param>
/// <returns>True if the node is provable.</returns>
let isProvable (fv: FplGenericNode) =
    match fv.Name with
    | LiteralThmL
    | LiteralLemL
    | LiteralPropL
    | LiteralCorL -> true
    | _ -> false

/// <summary>
/// Determine whether the node is an axiom or a conjecture.
/// </summary>
/// <param name="fv">Node to test.</param>
/// <returns>True for axiom or conjecture nodes.</returns>
let isAxiomOrConnjecture (fv:FplGenericNode) = 
    match fv.Name with
    | LiteralConjL 
    | LiteralAxL -> true
    | _ -> false

/// <summary>
/// Test whether the node represents a language declaration.
/// </summary>
/// <param name="fv">Node to test.</param>
/// <returns>True for language nodes.</returns>
let isLanguage (fv:FplGenericNode) =
    match fv.Name with
    | PrimLanguageL -> true
    | _ -> false

/// <summary>
/// Check whether the provided name starts with an uppercase character.
/// </summary>
/// <param name="name">Name string to inspect.</param>
/// <returns>True if the first character is uppercase and the string is non-empty.</returns>
let isUpper (name:string) =  
    name.Length > 0 && System.Char.IsUpper(name[0])

/// <summary>
/// Determine whether the node is a call-by-value site (parenthesized and with upper-case FplId).
/// </summary>
/// <param name="fv">Node to inspect.</param>
/// <returns>True for call-by-value nodes.</returns>
let isCallByValue (fv:FplGenericNode) =
    match fv.ArgType with 
    | ArgType.Parentheses when isUpper fv.FplId -> true
    | _ -> false

/// <summary>
/// Test whether a node implements signature position information.
/// </summary>
/// <param name="fv1">Node to test.</param>
/// <returns>True when the node implements <c>IHasSignature</c>.</returns>
let hasSignature (fv1:FplGenericNode) =
    match box fv1 with
    | :? IHasSignature -> true
    | _ -> false

/// <summary>
/// Check whether the node is a signature variable.
/// </summary>
/// <param name="fv1">Node to test.</param>
/// <returns>True when the node implements <c>IVariable</c> and is marked as a signature variable.</returns>
let isSignatureVar (fv1:FplGenericNode) = 
    match box fv1 with 
    | :? IVariable as var when var.IsSignatureVariable -> true
    | _ -> false

/// <summary>
/// Test whether the node represents a variable (simple or array).
/// </summary>
/// <param name="fv1">Node to test.</param>
/// <returns>True for variable nodes.</returns>
let isVar (fv1:FplGenericNode) =
    match fv1.Name with
    | PrimVariableL
    | PrimVariableArrayL -> true
    | _ -> false

/// <summary>
/// Determine whether the node is an extension declaration.
/// </summary>
/// <param name="fv">Node to test.</param>
/// <returns>True for extension nodes.</returns>
let isExtension (fv:FplGenericNode) =
    match fv.Name with
    | PrimExtensionL -> true
    | _ -> false

/// <summary>
/// Build a qualified dotted name for the provided node.
/// </summary>
/// <param name="fv">Node for which to construct the name.</param>
/// <param name="first">Flag indicating whether this is the top-level call formatting.</param>
/// <returns>Qualified name string for the node.</returns>
/// <remarks>
/// Recursively consults parent nodes and uses the node's signature/type when appropriate.
/// </remarks>
let rec getFullName (fv: FplGenericNode) (first: bool) =
    let fplValueType =
        match fv.Name with
        | LiteralLocL
        | PrimExclusiveOr 
        | PrimConjunction
        | PrimDisjunction 
        | PrimNegation
        | PrimImplication
        | PrimEquivalence 
        | PrimIsOperator 
        | PrimExtensionObj 
        | PrimDelegateEqualL 
        | PrimDelegateDecrementL 
        | PrimRefL -> fv.Type(SignatureType.Name)
        | LiteralCtorL
        | PrimBaseConstructorCall
        | PrimDefaultConstructor
        | PrimQuantifierAll
        | PrimQuantifierExists
        | PrimQuantifierExistsN
        | PrimClassL
        | PrimPredicateL
        | PrimFunctionalTermL
        | PrimMandatoryPredicateL
        | PrimMandatoryFunctionalTermL -> fv.Type(SignatureType.Mixed)
        | _ -> fv.FplId

    match fv.Name with
    | PrimRoot -> ""
    | _ -> 


        if first then
            if fv.Parent.Value.Name = PrimRoot then
                getFullName fv.Parent.Value false + fplValueType
            elif (isVar fv) && not (isVar fv.Parent.Value) then
                fplValueType
            elif fplValueType = String.Empty then
                getFullName fv.Parent.Value false
            else
                getFullName fv.Parent.Value false + "." + fplValueType
        elif fv.Parent.Value.Name = PrimRoot then
            getFullName fv.Parent.Value false + fplValueType
        elif (isVar fv) && not (isVar fv.Parent.Value) then
            fplValueType
        else
            getFullName fv.Parent.Value false + "." + fplValueType

/// <summary>
/// Produce a user-facing qualified name with an English article based on the node and determination flag.
/// </summary>
/// <param name="fplValue">Node to describe.</param>
/// <param name="determined">Determined flag used for English wording choices.</param>
/// <returns>Formatted qualified name.</returns>
let qualifiedName (fplValue:FplGenericNode) determined =
    $"{getEnglishName fplValue.Name determined} `{getFullName fplValue true}`"

/// <summary>
/// Produce a compact qualified name without English article for the given node.
/// </summary>
/// <param name="fplValue">Node to describe.</param>
/// <returns>Simple qualified name string.</returns>
let qualifiedNameSimple (fplValue:FplGenericNode) =
    $"{fplValue.Name} `{getFullName fplValue true}`"

/// <summary>
/// Validate SIG11 constraints on mappings and emit diagnostics when violated.
/// </summary>
/// <param name="fv">Mapping node to check.</param>
let checkSIG11Diagnostics (fv:FplGenericNode) =
    let mapOpt = getMapping fv
    match mapOpt with
    | Some map ->
        match map.RefersTo with 
        | Some ref ->
            match ref.Name with 
            | PrimClassL -> ()
                // mappings can point to classes 
            | PrimExtensionL ->
                let mapOfExtOpt = getMapping ref
                match mapOfExtOpt with
                | Some mapOfExt when mapOfExt.RefersTo.IsSome && Object.ReferenceEquals(ref, mapOfExt.RefersTo.Value) -> 
                // if a mapping points to an extension definition,
                // it is only allowed, if this extension does not delegate the mapping to another type
                // and, instead, points to itself
                    ()
                | _ ->
                    map.ErrorOccurred <- emitSIG11Diagnostics (getFullName ref true) ref.Name map.StartPos map.EndPos
            | _ ->
                // otherwise issue SIG11
                map.ErrorOccurred <- emitSIG11Diagnostics (getFullName ref true) ref.Name map.StartPos map.EndPos       
        | _ -> ()
    | _ -> ()

/// <summary>
/// Emit LG003 diagnostics if a signature-bearing node evaluated to a falsity value.
/// </summary>
/// <param name="fv">Node to validate.</param>
let checkLG003Diagnostics (fv:FplGenericNode) =
    match box fv with
    | :? IHasSignature as hasSignature ->
        let nodeRepr = fv.Represent()
        if nodeRepr = LiteralFalse then
            fv.ErrorOccurred <- emitLG003Diagnostics (fv.Type(SignatureType.Name)) fv.Name nodeRepr hasSignature.SignStartPos hasSignature.SignEndPos
    | _ -> ()

/// <summary>
/// Issue VAR10 diagnostics when bound variables of quantifiers are used elsewhere in the same formula.
/// </summary>
/// <param name="fv">Formula node to analyze for unclean bound-variable usage.</param>
/// <remarks>
/// This function inspects quantifiers, extracts used variables, and compares their ranges to quantifier ranges.
/// </remarks>
let checkCleanedUpFormula (fv:FplGenericNode) =
    let formulaCreationInSymbolTableCompleted (formula:FplGenericNode) =
        match formula.Parent with 
        | Some parent ->
            match parent.Name with 
            | PrimConjunction
            | PrimDisjunction
            | PrimImplication
            | PrimEquivalence
            | PrimExclusiveOr
            | PrimNegation
            | PrimQuantifierAll
            | PrimQuantifierExists
            | PrimQuantifierExistsN
            | PrimIsOperator
            | PrimRefL -> false
            | _ -> true
        | _ -> true

    let rec usedVariablesInFormula (formula:FplGenericNode) = 
        let extractFromSubFormula (subFormula:FplGenericNode) =
            subFormula.ArgList 
            |> Seq.map (fun subF -> usedVariablesInFormula subF)
            |> List.concat
        match formula.Name with 
        | PrimRefL when formula.RefersTo.IsSome ->
            match formula.RefersTo with
            | Some ref when ref.Name = PrimVariableL -> [formula] 
            | None when checkStartsWithLowerCase formula.FplId -> 
                [formula]  
            | _ -> extractFromSubFormula formula 
        | PrimQuantifierAll
        | PrimQuantifierExists
        | PrimQuantifierExistsN -> (formula.Scope.Values |> Seq.toList) @ extractFromSubFormula formula.ArgList[0]  
        | _ ->
            extractFromSubFormula formula 

    let rec extractQuantifiers (formula:FplGenericNode) =
        let extractFromSubFormula (subFormula:FplGenericNode) =
            (subFormula.ArgList |> Seq.map (fun subF -> extractQuantifiers subF) |> List.concat)
        match formula.Name with 
        | PrimQuantifierAll
        | PrimQuantifierExists
        | PrimQuantifierExistsN -> 
            [formula] @ extractFromSubFormula formula
        | _ -> 
            extractFromSubFormula formula

    let rec checkQuantifiers (formula:FplGenericNode) =
        let varUsedInQuantifier (varInFormula:FplGenericNode) (quantifier:FplGenericNode) =
            let varLStart = varInFormula.StartPos.Line
            let varCStart = varInFormula.StartPos.Column
            let varLEnd = varInFormula.EndPos.Line
            let varCEnd = varInFormula.EndPos.Column
            let quantifierLStart = quantifier.StartPos.Line
            let quantifierCStart = quantifier.StartPos.Column
            let quantifierLEnd = quantifier.EndPos.Line
            let quantifierCEnd = quantifier.EndPos.Column
            (
               (quantifierLStart < varLStart && quantifierLEnd > varLEnd) // lines(quantifier) contain lines(variable)
            || (quantifierLStart = varLStart && quantifierLEnd > varLEnd && quantifierCStart <= varCStart ) // if start line(q) = start line (v) && end line(q) > end line(v), compare starting columns
            || (quantifierLStart = varLStart && quantifierLEnd = varLEnd && quantifierCStart <= varCStart && quantifierCEnd >= varCEnd) // if line(q) = line(v) for start and end, compare starting and ending columns
            )

        let varIsBoundByQuantifier (varInFormula:FplGenericNode) (quantifier:FplGenericNode) =
            quantifier.Scope.ContainsKey(varInFormula.FplId)

        let quantifiers = extractQuantifiers formula
        let formulaName = formula.Type SignatureType.Name
        let usedVariables = usedVariablesInFormula formula
        quantifiers 
        |> List.map (fun quantifier ->
            usedVariables
            |> List.tryFind (fun varInFormula ->
                varIsBoundByQuantifier varInFormula quantifier && not (varUsedInQuantifier varInFormula quantifier)
            )
            |> Option.map (fun uncleanedUpVariableInFormula ->
                let quantifierVar = quantifier.Scope[uncleanedUpVariableInFormula.FplId]
                fv.ErrorOccurred <- emitVAR10Diagnostics uncleanedUpVariableInFormula.FplId formulaName quantifierVar.StartPos quantifierVar.EndPos
            )
        )
        |> ignore
            
    if formulaCreationInSymbolTableCompleted fv then
        // here, this reference points to a formula, which is final in the symbol table
        checkQuantifiers fv

/// <summary>
/// Determines whether the node is nested within a quantifier.
/// </summary>
/// <param name="fv">Node to inspect.</param>
/// <returns>True when the node has a quantifier ancestor.</returns>
let rec isInQuantifier (fv:FplGenericNode) =
    match fv.Name with 
    | PrimQuantifierAll
    | PrimQuantifierExists
    | PrimQuantifierExistsN -> true
    | _ ->
        match fv.Parent with 
        | Some parent -> isInQuantifier parent
        | _ -> false

/// <summary>
/// Search for a variable by name within the nearest enclosing block scope of the given node.
/// </summary>
/// <param name="fplValue">Node whose enclosing block scope is searched.</param>
/// <param name="name">Variable name to find.</param>
/// <param name="withNestedVariableSearch">When true, also searches nested scopes before ascending.</param>
/// <returns>A <see cref="ScopeSearchResult"/> describing the outcome of the lookup.</returns>
let variableInBlockScopeByName (fplValue: FplGenericNode) name withNestedVariableSearch =
    let rec firstBlockParent (fv: FplGenericNode) =

        let qualifiedVar (fv1: FplGenericNode) =
            let allVarsInScope = fv1.GetVariables()

            // try out all variables in scope
            let foundList =
                allVarsInScope
                |> Seq.map (fun (var: FplGenericNode) ->
                    if var.Scope.ContainsKey name then
                        ScopeSearchResult.Found(var.Scope[name])
                    else
                        ScopeSearchResult.NotFound)
                |> Seq.filter (fun ssr -> ssr <> ScopeSearchResult.NotFound)
                |> Seq.toList

            if foundList.IsEmpty then
                firstBlockParent fv1.Parent.Value
            else
                foundList.Head
        if isTheory fv then 
            ScopeSearchResult.NotFound
        else
            match fv.Name with 
            | LiteralThmL 
            | LiteralLemL
            | LiteralPropL 
            | LiteralCorL
            | LiteralConjL 
            | PrimPredicateL
            | LiteralAxL
            | PrimRuleOfInference -> 
                if fv.Scope.ContainsKey name then
                    ScopeSearchResult.Found(fv.Scope[name])
                elif fv.Parent.IsSome then
                    if withNestedVariableSearch then
                        match qualifiedVar fv with
                        | ScopeSearchResult.NotFound -> firstBlockParent fv.Parent.Value
                        | s -> s
                    else
                        firstBlockParent fv.Parent.Value
                else
                    ScopeSearchResult.NotFound
            | _ ->
                match fv.Name with
                | LiteralCtorL
                | LiteralLocL
                | PrimQuantifierAll
                | PrimQuantifierExists
                | PrimQuantifierExistsN
                | PrimMandatoryFunctionalTermL
                | PrimMandatoryPredicateL
                | LiteralPrfL
                | PrimExtensionL
                | PrimFunctionalTermL
                | PrimClassL ->
                    if fv.Scope.ContainsKey name then
                        ScopeSearchResult.Found(fv.Scope[name])
                    elif fv.Parent.IsSome then
                        if withNestedVariableSearch then
                            match qualifiedVar fv with
                            | ScopeSearchResult.NotFound -> firstBlockParent fv.Parent.Value
                            | s -> s
                        else
                            firstBlockParent fv.Parent.Value
                    else
                        ScopeSearchResult.NotFound
                | _ ->
                    if fv.Parent.IsSome then
                        firstBlockParent fv.Parent.Value
                    else
                        ScopeSearchResult.NotFound

    firstBlockParent fplValue

/// <summary>
/// Emit VAR09 diagnostics for arguments that reference a free variable not declared as a signature variable.
/// </summary>
/// <param name="arg">Argument node to inspect.</param>
let rec checkFreeAndNotSignatureVar (arg:FplGenericNode) = 
    match arg.RefersTo with 
    | Some ref ->
        match box ref, ref.UltimateBlockNode with 
        | :? IVariable as var, Some node when not (isProvable node || isAxiomOrConnjecture node) && node.Name <> PrimRuleOfInference && node.Name <> LiteralLocL && not var.IsBound && not var.IsSignatureVariable ->
            ref.ErrorOccurred <- emitVAR09Diagnostics ref.FplId ref.StartPos ref.EndPos
        | _ -> ()
    | None when arg.ExpressionType.IsParen ->
        // delegate parenthesized (arg) to a
        // arg has always a single argument due to symbol table structure of Ast.Parens
        checkFreeAndNotSignatureVar arg.ArgList[0]
    | _ -> ()

/// <summary>
/// Heuristic test whether a node is a syntactically simple expression.
/// </summary>
/// <param name="fv">Node to test.</param>
/// <returns>True for simple expressions.</returns>
let rec isSimpleExpression (fv:FplGenericNode) =
    match fv.Name with
    | PrimExtensionObj
    | PrimVariableL
    | PrimVariableArrayL
    | PrimQuantifierAll
    | PrimQuantifierExists
    | PrimQuantifierExistsN
    | PrimNegation
    | PrimClassL 
    | PrimIntrinsicInd
    | PrimFalse
    | PrimTrue -> true
    | PrimPredicateL when fv.ExpressionType.IsNoFix -> true
    | PrimMappingL when (fv.Scope.Count = 0) -> true
    | LiteralParent 
    | LiteralSelf 
    | PrimRefL ->
        match fv.RefersTo with
        | Some ref -> isSimpleExpression ref
        | _ -> true
    | _ -> false


/// <summary>
/// Decide whether the provided argument has a determined value.
/// </summary>
/// <param name="arg">Argument node to check.</param>
/// <returns>True if the argument's value is determined (not undetermined).</returns>
let isDetermined (arg: FplGenericNode) : bool =
    match arg with
    | :? FplGenericHasValue as argWithValue ->
        match argWithValue.Value with
        | None -> false
        | Some (:? FplUndetermined) -> false
        | Some v when v.FplId = LiteralUndet -> false
        | _ -> true
    | _ ->
        true

/// <summary>
/// In a list of nodes, find two examples with different names and return either a single-name or a pair of differing names.
/// </summary>
/// <param name="items">List of nodes to inspect.</param>
/// <returns>
/// <c>Choice1Of2 singleName</c> when all names are identical or the list is empty;
/// <c>Choice2Of2 (nameA, nameB)</c> when two different names are found.
/// </returns>
let findTwoDifferentNames (items:FplGenericNode list) =
    match items with
    | [] -> Choice1Of2 ""
    | first :: rest ->
        rest
        |> Seq.tryPick (fun ji ->
            if ji.Name = first.Name then None
            else Some (first.Name, ji.Name)
        )
        |> function
            | None -> Choice1Of2 first.Name
            | Some pair -> Choice2Of2 pair

/// <summary>
/// Emit SY010 diagnostics when the provided argument is parenthesized (disallowed form in some contexts).
/// </summary>
/// <param name="arg">Argument node to validate.</param>
let checkSY010 (arg:FplGenericNode) =
    if arg.ExpressionType.IsParen then
        arg.ErrorOccurred <- emitSY010Diagnostics arg.StartPos arg.EndPos
