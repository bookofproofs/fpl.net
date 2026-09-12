(* Copyright (c) 2021+ bookofproofs See LICENSE in the project root for license terms. *)

/// <summary>
/// Module providing utilities to match and instantiate expressions during proof
/// inference and justification processing.
/// </summary>
/// <remarks>
/// The module implements:
/// - Expression pattern matching with support for quantifiers, transparent references,
///   parameterized variables and delegate-equality unwrapping.
/// - Recording and verifying consistent pattern-variable usages across matches.
/// - Helpers to collect matching results across multiple pattern candidates and to
///   pretty-print mismatch diagnostics for error reporting.
/// Matching routines return optional diagnostic messages (None on success) and populate
/// substitution dictionaries used for instantiation.
/// </remarks>
module Fpl.Interpreter.SymbolTable.ExpressionMatching
open System
open System.Collections.Generic
open Fpl.Primitives
open Fpl.Parser.Types
open Fpl.Errors.Messages
open Fpl.Errors.Emitter
open Fpl.Interpreter.BasicTypes
open Fpl.Interpreter.Helpers.Checks
open Fpl.Interpreter.Helpers.Basic
open Fpl.Interpreter.Helpers.Debug
open Fpl.Interpreter.SymbolTable.Types2.Intrinsic
open Fpl.Interpreter.SymbolTable.Types2.Variables
open Fpl.Interpreter.SymbolTable.TypeMatching


/// <summary>
/// Helper wrapper to produce a standardized mismatch error when two quantifier-bound
/// variables have incompatible types during pattern matching.
/// </summary>
/// <param name="a">The candidate expression node containing the quantifier.</param>
/// <param name="p">The pattern expression node containing the quantifier.</param>
/// <param name="x">The candidate bound-variable node.</param>
/// <param name="y">The pattern bound-variable node.</param>
/// <param name="index">Index of the bound variable in the quantifier's variable list.</param>
/// <returns>Optional mismatch message string.</returns>
let private errExprMismatchQuantifierVariableTypesWrapper (a:FplGenericNode) (p:FplGenericNode) (x:FplGenericNode) (y:FplGenericNode) index =
    let xName = $"{x.FplId}:{x.Type SignatureType.Type}"
    let yName = $"{y.FplId}:{y.Type SignatureType.Type}"
    let aName = a.Type SignatureType.Name
    let pName = p.Type SignatureType.Name
    errExprMismatchQuantifierVariableTypes aName pName xName yName index  

/// <summary>
/// Instantiates an expression by replacing pattern variables with the expressions
/// recorded in the provided variable-usage dictionary.
/// </summary>
/// <param name="expression">Expression in which variables should be replaced.</param>
/// <param name="dictParameterUsage">Dictionary mapping pattern variable ids to matched expressions.</param>
/// <returns>The instantiated expression.</returns>
let instantiateExpressionByVarUsages (expression: FplGenericNode) (dictParameterUsage: Dictionary<string, FplGenericNode>) : FplGenericNode =
    let isVariableWithMatchedExpression (arg:FplGenericNode) =
        match arg.Name with
        | PrimRefL when arg.RefersTo.IsSome ->
            match arg.RefersTo with
            | Some var when var.Name = PrimVariableL ->
                dictParameterUsage.ContainsKey(var.FplId) 
            | _ ->  false
        | _ ->  false

    // Replace matched variables recursively throughout the cloned expression.
    let rec replaceVarsByUsages (expr:FplGenericNode) =
        let newArgList = List<FplGenericNode>()
        expr.ArgList
        |> Seq.iter (fun arg ->
            if isVariableWithMatchedExpression arg then
                newArgList.Add (dictParameterUsage[arg.FplId].Clone())  // clone to avoid sharing
            else 
                newArgList.Add (replaceVarsByUsages arg)
        )
        let exprVarList = expr.GetVariables()
        exprVarList
        |> Seq.iter (fun var ->
            // Update variable metadata in the cloned expression so its structure and
            // printed representation reflect the instantiation consistently.
            if dictParameterUsage.ContainsKey(var.FplId) then
                var.TypeId <- dictParameterUsage[var.FplId].TypeId
                var.FplId <- dictParameterUsage[var.FplId].FplId
        )
            
        // replace expression arguments by new expressions where variables were replaced by their usages
        newArgList
        |> Seq.iteri (fun i arg -> expr.ArgList[i] <- arg)
        expr

    // Propagate the recorded substitutions into the variable scope of the cloned
    // expression so the resulting expression reflects the matched instantiation.
    if isVariableWithMatchedExpression expression then
        // If the expression is a single variable
        // and this variable was matched with some expression,
        // we replace the whole expression with this matched expression
        dictParameterUsage[expression.FplId]
    else
        // otherwise we replace it with the expression in which
        // we recursively replace all variables by matched expressions
        replaceVarsByUsages expression


/// <summary>
/// Produces a string representation of a quantifier formula where bound-variable names
/// are replaced by numbered placeholders in the order of the bound variables. Used to compare quantifier bodies modulo
/// bound variable renaming.
/// </summary>
/// <param name="fv">The quantifier formula node.</param>
/// <returns>String representation of the formula modulo bound variable names.</returns>
let private getNameOfQuantifierFormulaModuloBoundVarNames (fv:FplGenericNode) =
    let originalNames = HashSet<string>()
    fv.Scope
    |> Seq.filter (fun kvp ->
        match kvp.Value with
        | :? FplVariable as var when var.IsBound -> true
        | _ -> false
    )
    |> Seq.iteri (fun i kvp ->
        let dummyVarname = $"[{i}]" // a numbered placeholder of the bound variable
        originalNames.Add kvp.Key |> ignore
        kvp.Value.FplId <- dummyVarname
    )
    let result = fv.Type SignatureType.Name // create a formula representation with the placeholders
    // restore the original names of the bound variables to prevent side effects
    originalNames
    |> Seq.iter(fun originalVarName ->
        let var = fv.Scope[originalVarName] 
        var.FplId <- originalVarName // restore original
    )
    result

/// <summary>
/// Records the first usage of a pattern variable mapping and checks for mismatches
/// on subsequent occurrences. Returns an optional mismatch message.
/// </summary>
/// <param name="varName">Pattern variable identifier.</param>
/// <param name="a">Candidate expression matched to the variable.</param>
/// <param name="dictParameterUsage">Dictionary holding established substitutions.</param>
/// <returns>Optional mismatch message string; None on success.</returns>
let private checkMismatchingUsageOfVars varName (a:FplGenericNode) (dictParameterUsage:Dictionary<string, FplGenericNode>) = 
    if dictParameterUsage.TryAdd (varName, a) then
        errExprMismatchOK
    else
        let previouslyMatchedFormula = dictParameterUsage[varName]
        if a.Name = previouslyMatchedFormula.Name && isQuantifier a && isQuantifier previouslyMatchedFormula then
            let expectedExprModVarNames = getNameOfQuantifierFormulaModuloBoundVarNames previouslyMatchedFormula
            let actualExprModVarNames = getNameOfQuantifierFormulaModuloBoundVarNames a
            if expectedExprModVarNames<>actualExprModVarNames then
                let expectedExpr = previouslyMatchedFormula.Type SignatureType.Name
                let actualExpr = (a.Type SignatureType.Name)
                errExprMismatchVarMatchedDifferentlyQuantfier varName expectedExpr actualExpr
            else
                errExprMismatchOK
        else
            let expectedExpr = previouslyMatchedFormula.Type SignatureType.Name
            let actualExpr = (a.Type SignatureType.Name)
            if expectedExpr<>actualExpr then
                errExprMismatchVarMatchedDifferently varName expectedExpr actualExpr
            else
                errExprMismatchOK

/// <summary>
/// Attempts to match a candidate expression against a pattern expression while
/// recording a consistent variable-usage dictionary for later instantiation.
/// </summary>
/// <param name="candidate">The candidate expression node to match.</param>
/// <param name="pattern">The pattern expression node to match against.</param>
/// <param name="dictParameterUsage">Dictionary that will be populated with substitutions from pattern variables to matched expressions.</param>
/// <returns>Optional mismatch message string; None if the match succeeds.</returns>
let matchExpressionAgainstPattern (candidate:FplGenericNode) (pattern:FplGenericNode) (dictParameterUsage: Dictionary<string, FplGenericNode>) =

    // Tracks bound-variable correspondences established by quantifier matching.
    // Keyed by the pattern variable node (reference equality), not its string name,
    // so that identically-named bound variables in different quantifier scopes never collide.
    let boundVarMap = Dictionary<FplGenericNode, FplGenericNode>()

    /// <summary>
    /// Compares the bound variables of two quantifier expressions and establishes a correspondence
    /// between pattern-bound variables and candidate-bound variables. Records correspondences in
    /// the local bound-variable map and propagates matched correspondences into the substitution
    /// dictionary. Performs arity and type checks for bound variables and reports appropriate
    /// mismatch messages when they differ.
    /// </summary>
    /// <param name="a">The candidate quantifier expression node.</param>
    /// <param name="p">The pattern quantifier expression node.</param>
    /// <returns>
    /// An optional mismatch message string: None when the quantifier-variable lists are compatible
    /// and correspondences were recorded; Some error message when a mismatch was detected.
    /// </returns>
    let compareQuantifierVariables (a:FplGenericNode) (p:FplGenericNode) =
        let pVars = p.GetVariables()
        let aVars = a.GetVariables()
        let rec loop l1 l2 index =
            match l1, l2 with
            | [], [] ->
                match a.Name with
                | PrimQuantifierExistsN when a.Name = p.Name && a.FplId <> p.FplId ->
                    errExprMismatchExistsN a.FplId (a.Type SignatureType.Name) p.FplId (p.Type SignatureType.Name)
                | _ ->
                    errExprMismatchOK
            | (x:FplGenericNode)::xs, (y:FplGenericNode)::ys ->
                match FplTypeMatcher.MatchPwA [x] [y] with
                | Some _ ->
                    errExprMismatchQuantifierVariableTypesWrapper a p x y index
                | _ ->
                    boundVarMap[y] <- x
                    if dictParameterUsage.ContainsKey(y.FplId) then
                        dictParameterUsage[y.FplId] <- x
                    else
                        dictParameterUsage.TryAdd(y.FplId, x) |> ignore
                    loop xs ys (index + 1)
            | _ ->
                errExprMismatchQuantifierVariableCounts (a.Type SignatureType.Name) (p.Type SignatureType.Name) aVars.Length pVars.Length
        loop aVars pVars 0

    /// <summary>
    /// (Inner helper) Produces a mocked clone of a parameterized variable definition where
    /// cloned parameters are projected from a given reference's arguments. Used so matching
    /// can treat parameterized variables consistently with bound variables.
    /// </summary>
    /// <param name="refQ">The reference expression containing actual arguments.</param>
    /// <param name="q">The parameterized variable definition to clone and adjust.</param>
    /// <returns>A cloned and adjusted variable node for matching.</returns>
    let mockVariableWithParams (refQ:FplGenericNode) (q:FplGenericNode) =
        if refQ.Name = PrimRefL && q.Name = PrimVariableL then
            let qMocked = q.Clone()
            let pars = getParameters q
            let args = getArguments refQ
            qMocked.Scope.Clear()
            Seq.zip pars args
            |> Seq.map (fun (p, a) -> (p, a.RefersTo))
            |> Seq.iteri (fun i (p, aRefOpt) ->
                match aRefOpt, p with
                | Some (:? FplVariable as aVar), (:? FplVariable as pVar) when aVar.IsBound ->
                    pVar.SetIsBound() // set cloned parameter variable bound if the argument variable is bound
                    // for better mismatch error reporting, replace declared parameter names/types with used parameter names/types 
                    pVar.FplId <- aVar.FplId 
                    pVar.TypeId <- aVar.TypeId 
                    qMocked.Scope.Add(i.ToString(), pVar)
                | _ -> ()
            )
            qMocked // replace var q(...) with ... being set to 
        else
            // in all other cases leave q unchanged
            q

    /// <summary>
    /// (Inner helper) Normalizes expressions for matching, e.g., unwraps transparent
    /// delegate-equality references to the actual represented expression.
    /// </summary>
    /// <param name="expression">Expression to normalize.</param>
    /// <returns>The normalized expression used for matching comparisons.</returns>
    let getNormalizedExpressionForMatching (expression: FplGenericNode) =
        match expression.Name, expression.RefersTo with
        | PrimRefL, Some referenced when referenced.Name = PrimDelegateEqualL ->
            referenced
        | _ ->
            expression

    /// <summary>
    /// (Inner helper) Attempts to extract the "transparent" operator name referenced by a
    /// reference node. Returns Some operator name if a transparent operator exists.
    /// </summary>
    /// <param name="reference">Reference expression to inspect.</param>
    /// <returns>Optional operator name used for transparent comparisons.</returns>
    let tryGetTransparentReferenceOperator (reference: FplGenericNode) =
        match reference.RefersTo with
        | Some (:? FplGenericHasValue as definition) when definition.ArgList.Count > 0 ->
            definition.ArgList[0]
            |> getNormalizedExpressionForMatching
            |> fun expression -> Some expression.Name
        | _ ->
            None

    /// <summary>
    /// (Inner helper) Checks whether two expressions expose the same transparent reference operator.
    /// </summary>
    /// <param name="a">First expression to compare.</param>
    /// <param name="p">Second expression to compare.</param>
    /// <returns>True if both expose the same transparent operator; otherwise false.</returns>
    let haveSameTransparentReferenceOperator (a: FplGenericNode) (p: FplGenericNode) =
        match tryGetTransparentReferenceOperator a, tryGetTransparentReferenceOperator p with
        | Some aOperator, Some pOperator -> aOperator = pOperator
        | _ -> false

    /// <summary>
    /// (Inner helper) Checks whether a concrete candidate expression is compatible with a
    /// pattern variable reference. Validates types and parameter arity and records variable usages.
    /// </summary>
    /// <param name="cand">Candidate expression to be checked.</param>
    /// <param name="variableReference">Pattern variable reference node.</param>
    /// <returns>Optional mismatch message string; None on success.</returns>
    let checkCandidateAgainstVarReference (cand:FplGenericNode) (variableReference:FplGenericNode) =
        let (errMsgOpt,_) = FplTypeMatcher.ComparisonBasedOnOpenFormulas cand variableReference
        match errMsgOpt, variableReference.RefersTo with
        | None, Some var when var.Name = PrimVariableL ->
            let mismatchUsageVarOpt = checkMismatchingUsageOfVars variableReference.FplId cand dictParameterUsage
            match mismatchUsageVarOpt with
            | Some errMsg -> Some errMsg
            | None when var.Scope.Count > 0 ->
                let pPars = getArguments variableReference
                let aPars = getDistinctVarsOfExpression cand
                if aPars.Length <> pPars.Length then
                    let aVars = aPars |> List.map (fun v -> $"{v.FplId}") |> String.concat ", "
                    let pName = variableReference.Type SignatureType.Name
                    errExprMismatchVarNumbDifferent aPars.Length aVars pPars.Length pName
                else
                    let lstOfErrMessages =
                        List.zip pPars aPars
                        |> List.map (fun (pArg, aArg) ->
                            checkMismatchingUsageOfVars pArg.FplId aArg dictParameterUsage
                        ) 
                    let secondResult = lstOfErrMessages |> List.tryPick (fun errMsgOpt -> errMsgOpt)
                    secondResult
            | _ -> errExprMismatchOK
        | Some errMsg, _ -> Some errMsg
        | _,_ ->
            errExprMismatchOK

    /// <summary>
    /// (Inner recursive helper) Core recursive routine that checks whether a candidate expression
    /// matches a pattern expression. May update the provided substitutions dictionary.
    /// </summary>
    /// <param name="cand">Candidate expression node.</param>
    /// <param name="pat">Pattern expression node.</param>
    /// <returns>Optional mismatch message string; None if the match succeeds.</returns>
    /// <remarks>This function contains nested recursion and calls additional inner helpers.
    /// It is intentionally recursive to traverse expression trees.</remarks>
    let rec checkExpr (cand:FplGenericNode) (pat:FplGenericNode) =

        let rec checkExpressions (args:FplGenericNode list) (pars:FplGenericNode list) =
            match args, pars with
            | a::ars, p::prs ->
                let msgOpt = checkExpr a p 
                match msgOpt with
                | None -> checkExpressions ars prs
                | Some msg -> Some msg
            | a::_, [] ->
                errExprMismatchExpectedEndOfFormula (a.Type SignatureType.Name)
            | [], p::_ ->
                errExprMismatchFoundEndOfFormula (p.Type SignatureType.Name)
            | [], [] ->
                errExprMismatchOK

        match cand.Name, pat.Name with
        | PrimConjunction, PrimConjunction
        | PrimDisjunction, PrimDisjunction
        | PrimImplication, PrimImplication
        | PrimEquivalence, PrimEquivalence
        | PrimExclusiveOr, PrimExclusiveOr
        | PrimNegation, PrimNegation -> checkExpressions (cand.ArgList |> Seq.toList) (pat.ArgList |> Seq.toList) 
        | PrimIsOperator, PrimIsOperator ->
            // first argument: the value expression (recurse normally)
            match checkExpr (cand.ArgList[0]) (pat.ArgList[0]) with
            | Some err -> Some err
            | None ->
                // second argument: the type of is operator — match only by referred definition identity
                let candType = cand.ArgList[1]
                let patType = pat.ArgList[1]
                match candType.RefersTo, patType.RefersTo with
                | Some candRef, Some patRef when Object.ReferenceEquals(candRef, patRef) ->
                    errExprMismatchOK
                | _ when candType.FplId = patType.FplId ->
                    // fallback: same built-in type name (obj, ind, pred, func)
                    errExprMismatchOK
                | _ ->
                    errExprMismatchMsgStandard (cand.Type SignatureType.Name) (pat.Type SignatureType.Name)
        | PrimQuantifierAll, PrimQuantifierAll 
        | PrimQuantifierExists, PrimQuantifierExists 
        | PrimQuantifierExistsN, PrimQuantifierExistsN ->
        // match number of quantifier variables
            match compareQuantifierVariables cand pat with
            | None ->
                // and now check the expressions inside the quantifiers
                checkExpressions (cand.ArgList |> Seq.toList) (pat.ArgList |> Seq.toList) 
            | Some err -> Some err
        | PrimFalse, PrimFalse 
        | PrimTrue, PrimTrue ->
            errExprMismatchOK
        | PrimDelegateEqualL, PrimDelegateEqualL ->
            checkExpressions (cand.ArgList |> Seq.toList) (pat.ArgList |> Seq.toList)
        // match parentheses
        | PrimRefL, PrimRefL when cand.ExpressionType.IsParen && pat.ExpressionType.IsParen ->
            checkExpr cand.ArgList[0] pat.ArgList[0]
        | PrimRefL, _ when cand.ExpressionType.IsParen ->
            checkExpr cand.ArgList[0] pat
        | _, PrimRefL when pat.ExpressionType.IsParen ->
            checkExpr cand pat.ArgList[0]
        | PrimRefL, _ when cand.RefersTo.IsSome && cand.RefersTo.Value.Name = PrimDelegateEqualL ->
            checkExpr cand.RefersTo.Value pat
        | _, PrimRefL when pat.RefersTo.IsSome && pat.RefersTo.Value.Name = PrimDelegateEqualL ->
            checkExpr cand pat.RefersTo.Value
        | _, PrimRefL when tryGetTransparentReferenceOperator pat = Some cand.Name ->
            checkExpressions (cand.ArgList |> Seq.toList) (pat.ArgList |> Seq.toList)
        | PrimRefL, _ when tryGetTransparentReferenceOperator cand = Some pat.Name ->
            checkExpressions (cand.ArgList |> Seq.toList) (pat.ArgList |> Seq.toList)
        | PrimRefL, PrimRefL when haveSameTransparentReferenceOperator cand pat ->
            checkExpressions (cand.ArgList |> Seq.toList) (pat.ArgList |> Seq.toList)
        | _, PrimRefL when pat.RefersTo.IsSome && boundVarMap.ContainsKey(pat.RefersTo.Value) ->
            let expectedCandidateVar = boundVarMap[pat.RefersTo.Value]
            match cand.RefersTo with
            | Some actualCandidateVar when Object.ReferenceEquals(actualCandidateVar, expectedCandidateVar) ->
                errExprMismatchOK
            | _ ->
                errExprMismatchMsgStandard (cand.Type SignatureType.Name) (pat.Type SignatureType.Name)
        | PrimRefL, PrimRefL ->
            match cand.RefersTo, pat.RefersTo with
            | Some aRef, Some pRef when aRef.Name <> PrimVariableL && pRef.Name = PrimVariableL ->
                checkCandidateAgainstVarReference cand pat
            | Some aRef, Some pRef when aRef.Name = PrimVariableL && pRef.Name <> PrimVariableL ->
                // The candidate is a plain variable reference (e.g. `x`), but the pattern refers to a
                // fixed operator/predicate application (e.g. `p ⇒ q` referring to `Impl(f, g)`).
                // Report the mismatch at the premise's own printed form instead of recursing into
                // the referenced definition's internals, which would produce a confusing, overly
                // deep error message anchored to the definition rather than to the premise pattern.
                errExprMismatchMsgNotAnInstanceOfPremise (cand.Type SignatureType.Name) (pat.Type SignatureType.Name)
            | Some aRef, Some pRef when Object.ReferenceEquals(aRef, pRef) ->
                checkExpressions (getArguments cand) (getArguments pat)
            | Some aRef, Some pRef ->
                checkExpr (mockVariableWithParams cand aRef) (mockVariableWithParams pat pRef)
            | Some aRef, None when pat.ArgList.Count > 0 && not pat.ExpressionType.IsParen ->
                checkExpr (mockVariableWithParams cand aRef) pat
            | None, Some pRef when cand.ArgList.Count > 0 && not cand.ExpressionType.IsParen ->
                checkExpr cand (mockVariableWithParams pat pRef)
            | None, None when cand.ExpressionType.IsParen && not pat.ExpressionType.IsParen ->
                errExprMismatchMsgParensOnlyLeft (cand.Type SignatureType.Name) (pat.Type SignatureType.Name)
            | None, None when not cand.ExpressionType.IsParen && pat.ExpressionType.IsParen ->
                errExprMismatchMsgParensOnlyRight (cand.Type SignatureType.Name) (pat.Type SignatureType.Name)
            | _, _ ->
                errExprMismatchOK
        | _, PrimRefL when pat.RefersTo.IsSome && pat.RefersTo.Value.Name = PrimVariableL ->
            checkCandidateAgainstVarReference cand pat
        | _, PrimVariableL ->
            match FplTypeMatcher.MatchArgumentsWithParameters cand pat with
            | Some err -> Some err
            | None -> checkMismatchingUsageOfVars pat.FplId cand dictParameterUsage
        | _, _ ->
            errExprMismatchMsgStandard (cand.Type SignatureType.Name) (pat.Type SignatureType.Name)
    checkExpr candidate pattern


/// <summary>
/// Computes match results of an expression against multiple pattern candidates.
/// </summary>
/// <param name="argumentInferredFormula">The candidate expression to match.</param>
/// <param name="patternCandidates">List of pattern expressions to try.</param>
/// <returns>
/// A list where each entry is a tuple: (patternCandidate, optionalMismatchMessage, substitutionDictionary).
/// The substitution dictionary records how pattern variables were instantiated for each attempt.
/// </returns>
let collectMatchResultsForExpressionPatternCandidates (argumentInferredFormula:FplGenericNode) (patternCandidates:FplGenericNode list) =
    patternCandidates
    |> List.map (fun patternCandidate ->
        let dictParameterUsage = Dictionary<string, FplGenericNode>()
        let mismatchMessageOpt = matchExpressionAgainstPattern argumentInferredFormula patternCandidate dictParameterUsage
        patternCandidate, mismatchMessageOpt, dictParameterUsage
    )

/// <summary>
/// Formats a list of match attempts into a readable diagnostic string. Each attempted
/// pattern candidate is annotated with its optional mismatch message and any substitutions.
/// </summary>
/// <param name="matchResults">List of tuples produced by match attempts:
/// (candidatePattern, optionalMismatchMessage, substitutionDictionary).</param>
/// <returns>Concatenated formatted diagnostic string for all attempts.</returns>
let prettifyMismatchErrors (matchResults:(FplGenericNode * string option * Dictionary<string, FplGenericNode>) list) =
    matchResults
    |> List.mapi (fun i (justificationCandidate, mismatchMessageOpt, dictParameterUsage) ->
        let substitutions =
            if dictParameterUsage.Count > 0 then 
                $"{Environment.NewLine}     Substitutions:" +       
                (
                    dictParameterUsage
                    |> Seq.map (fun kvp -> $"{Environment.NewLine}       `{kvp.Key} := {kvp.Value.Type SignatureType.Name}`")
                    |> String.concat ", "
                )
            else
                $"{Environment.NewLine}     No substitutions found" 
                                    
        match mismatchMessageOpt with
        | Some errorMsg ->
            $"{Environment.NewLine}  {i + 1}) `{justificationCandidate.Type SignatureType.Name}`{Environment.NewLine}     {errorMsg}{substitutions}"
        | None -> 
            $"{Environment.NewLine}  {i + 1}) `{justificationCandidate.Type SignatureType.Name}`{substitutions}" 
    )
    |> String.concat ", "

/// <summary>
/// Tries to match a premise with expressions from a list.
/// Returns a list of matched expressions together with the substitution dictionary,
/// and a concatenated string with failure diagnostics for tried candidates.
/// </summary>
/// <param name="exprList">Candidate expressions to try for matching.</param>
/// <param name="pre">Premise pattern to match against.</param>
/// <param name="dictParameterUsage">Dictionary used to record substitutions of pattern variables during matching.</param>
/// <returns>
/// A tuple where the first item is a list of matched candidates as (expression * substitutions) and the second
/// item is a concatenated error string summarizing failed candidates.
/// </returns>
/// <remarks>To expose parameter documentation in generated XML ensure this function is part of the public surface or declared in the module signature (.fsi).</remarks>
let matchPremiseWithSomeExpressions (exprList:FplGenericNode list) (pre:FplGenericNode) (dictParameterUsage:Dictionary<string, FplGenericNode>)=

    let result = List<FplGenericNode * Dictionary<string, FplGenericNode>>()
    let failedMatchResults = List<FplGenericNode * string option * Dictionary<string, FplGenericNode>>()

    exprList
    |> List.iter (fun expr ->
        let errOpt = matchExpressionAgainstPattern expr pre dictParameterUsage
        match errOpt with
        | None -> result.Add (expr, dictParameterUsage)
        | Some err -> failedMatchResults.Add (expr, Some err, dictParameterUsage)
    )
    result |> Seq.toList, (prettifyMismatchErrors (failedMatchResults |> Seq.toList))

/// <summary>
/// Emits PR022 diagnostics indicating that a justification or inference cannot collect preceding results,
/// sets an appropriate error state on the provided node, and assigns a default value.
/// </summary>
/// <param name="fv">Node that will receive the error and default value.</param>
/// <param name="nodeOpt">Optional referenced node involved in the failure.</param>
/// <param name="varOpt">Optional referenced variable involved in the failure.</param>
let issuePR022AndSetDefault (fv:FplGenericHasValue) (nodeOpt:FplGenericNode option) (varOpt:FplGenericNode option) =
    match nodeOpt, varOpt with
    | Some node, Some var ->
        let reason = $"The {var.Name} `{var.FplId}` and its {node.Name} `{node.Type SignatureType.Name}` were found, but the {node.Name} definition does not contain any predicative expressions that support argument inference."
        fv.ErrorOccurred <- emitPR022Diagnostics reason fv.StartPos fv.EndPos
    | None, Some var ->
        let reason = $"The {var.Name} `{var.FplId}` was found, but its pre‑defined type contains no predicative expressions that support argument inference."
        fv.ErrorOccurred <- emitPR022Diagnostics reason fv.StartPos fv.EndPos
    | Some node, None ->
        let reason = $"The {node.Name} `{node.Type SignatureType.Name}` was found, but its definition does not contain any predicative expressions that support argument inference."
        fv.ErrorOccurred <- emitPR022Diagnostics reason fv.StartPos fv.EndPos
    | None, None ->
        let reason = $"No reference for `{fv.FplId}` was found that contains any predicative expressions that support argument inference."
        fv.ErrorOccurred <- emitPR022Diagnostics reason fv.StartPos fv.EndPos
    fv.SetDefaultValue()

/// <summary>
/// Emits diagnostics with a custom reason indicating that a justification or inference cannot
/// collect preceding results and sets a default value on the provided node.
/// </summary>
/// <param name="fv">Node to mark with an error and default value.</param>
/// <param name="reason">Custom textual reason for the diagnostic.</param>
let issuePR022SpecialReasonAndSetDefault (fv:FplGenericHasValue) reason =
    fv.ErrorOccurred <- emitPR022Diagnostics reason fv.StartPos fv.EndPos
    fv.SetDefaultValue()


/// <summary>
/// Abstract base class representing an inferring construct that can yield inferred expression candidates.
/// </summary>
[<AbstractClass>]
type FplGenericInfering(positions: Positions, parent: FplGenericNode) =
    inherit FplGenericPredicate(positions, parent)

    abstract member InferredExprCandidates: FplGenericNode list with get


/// <summary>
/// Abstract base class for justification items used in proof steps. Provides embedding and evaluation behavior
/// common to justification items.
/// </summary>
[<AbstractClass>]
type FplGenericJustificationItem(positions: Positions, parent: FplGenericNode) =
    inherit FplGenericInfering(positions, parent)

    override this.ShortName = PrimJustification

    override this.Type signatureType =
        let head = getFplHead this signatureType
        head

    override this.EmbedInSymbolTable _ = 
        let thisJustificationItemId = this.Type(SignatureType.Mixed)

        let alreadyAddedIdOpt = 
            this.Parent.Value.ArgList
            |> Seq.map (fun argJi -> argJi.Type(SignatureType.Mixed))
            |> Seq.tryFind (fun otherId -> otherId = thisJustificationItemId)
        match alreadyAddedIdOpt with
        | Some _ ->
            this.ErrorOccurred <- emitPR004Diagnostics thisJustificationItemId this.StartPos this.EndPos 
        | _ -> ()
        addExpressionToParentArgList this

    override this.Run() = 
        StaticDebug.Debug(this,Debug.Start)
        match this.RefersTo with 
        | Some _ ->
            // a justification item is to be evaluated to "true" if
            // its RefersTo node was assigned successfully (it refers to something that
            // could be successfully referred to in the remaining Fpl Code)
            let v = new FplIntrinsicTrue((this.StartPos, this.EndPos), this)
            this.SetValue v 
        | _ ->
            issuePR022AndSetDefault this None None
        StaticDebug.Debug(this,Debug.Stop)

/// <summary>
/// Abstract base class representing an argument-inference construct inside a justification/inference.
/// </summary>
[<AbstractClass>]
type FplGenericArgInference(positions: Positions, parent: FplGenericNode) =
    inherit FplGenericInfering(positions, parent)

    override this.Type signatureType =
        let head = getFplHead this signatureType
        head

    override this.EmbedInSymbolTable _ = addExpressionToParentArgList this

/// <summary>
/// Matches the expressions inferred by justification items against a list of premise patterns
/// for a given inference node. Returns the flattened list of matched expression/substitution pairs.
/// </summary>
/// <param name="tuplesJustItemWithInferredExpressionsList">List of pairs: (justification item, its inferred expressions).</param>
/// <param name="premiseList">List of premise pattern nodes to match against.</param>
/// <param name="byInferenceNode">The inference node that requested the matching (used for diagnostics).</param>
/// <returns>List of matched (expression * substitutionDictionary) pairs corresponding to premises.</returns>
let matchJustItemsExpressionsAgainstPremiseList (tuplesJustItemWithInferredExpressionsList:(FplGenericJustificationItem * FplGenericNode list) list) (premiseList:FplGenericNode list) (byInferenceNode:FplGenericNode) =
    let varUsageDict = Dictionary<string, FplGenericNode>()
    let result = List<(FplGenericNode * Dictionary<string, FplGenericNode>) list>()

    /// <summary>
    /// (Inner helper) Produces a short textual description of how a justification item was used
    /// to satisfy a particular premise pattern. Includes the justification id and the first inferred expression if any.
    /// </summary>
    /// <param name="justItemWithExprs">Tuple of justification item and its inferred expression list.</param>
    /// <param name="premisePattern">Premise pattern node being described.</param>
    /// <returns>Multi-line string describing the premise usage for diagnostics.</returns>
    let describePremiseUsage ((justItem, inferredExprs): FplGenericJustificationItem * FplGenericNode list) (premisePattern:FplGenericNode) =
        let premiseExpression = premisePattern.Type SignatureType.Name
        let argId = justItem.FplId
        match inferredExprs with
        | [] ->
            $"`{premiseExpression}`{Environment.NewLine}     ... pattern for argument `{argId}`"
        | expr :: _ ->
            $"`{premiseExpression}`{Environment.NewLine}     ... pattern for argument `{argId}` ...{Environment.NewLine}     `{expr.Type SignatureType.Name}`"

    /// <summary>
    /// (Inner recursive helper) Walks the lists of justification items with inferred expressions and
    /// premise patterns to perform the matching and emit diagnostics on mismatches.
    /// </summary>
    /// <param name="iJeLists">Remaining justification-item-with-expression pairs to process.</param>
    /// <param name="preList">Remaining premise patterns to match.</param>
    let rec matchJustItemsExpressionsAgainstPremiseListRec (iJeLists:(FplGenericJustificationItem * FplGenericNode list) list) (preList:FplGenericNode list) =
        match iJeLists, preList with
        | iJel::iJels, pre::pres ->
            let just = fst iJel
            let inferredExpressionsOfJust = snd iJel
            match matchPremiseWithSomeExpressions inferredExpressionsOfJust pre varUsageDict with
            | [], errList ->
                // emit diagnostics at just's position that there was no matching candidate for a premise, listing all tried-out candidates (contained in errList)
                let premisesPre =
                    List.zip tuplesJustItemWithInferredExpressionsList premiseList
                    |> List.map (fun (iJe, prem) -> describePremiseUsage iJe prem)
                let premises =
                    if premiseList.Length > 1 then
                        premisesPre |> numbered
                    else
                        premisesPre |> String.concat ""
                just.ErrorOccurred <- emitPR008Diagnostics (byInferenceNode.Type SignatureType.Name) premiseList.Length premises errList just.StartPos just.EndPos
                matchJustItemsExpressionsAgainstPremiseListRec iJels pres 
            | matchedExprList, _ ->
                result.Add matchedExprList
                matchJustItemsExpressionsAgainstPremiseListRec iJels pres 
        | [], _::_ ->
            byInferenceNode.ErrorOccurred <- emitPR020Diagnostics premiseList.Length tuplesJustItemWithInferredExpressionsList.Length byInferenceNode.StartPos byInferenceNode.EndPos
        | _::_, [] ->
            byInferenceNode.ErrorOccurred <- emitPR020Diagnostics premiseList.Length tuplesJustItemWithInferredExpressionsList.Length byInferenceNode.StartPos byInferenceNode.EndPos
        | [], [] -> ()
            
    matchJustItemsExpressionsAgainstPremiseListRec tuplesJustItemWithInferredExpressionsList premiseList
    let res = result |> List.concat
    res
