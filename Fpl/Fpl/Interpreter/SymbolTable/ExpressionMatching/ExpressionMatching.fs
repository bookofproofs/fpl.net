/// This module contains functions for matching and instantiating expressions during proof inference.

(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)
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


let private errExprMismatchQuantifierVariableTypesWrapper (a:FplGenericNode) (p:FplGenericNode) (x:FplGenericNode) (y:FplGenericNode) index =
    let xName = $"{x.FplId}:{x.Type SignatureType.Type}"
    let yName = $"{y.FplId}:{y.Type SignatureType.Type}"
    let aName = a.Type SignatureType.Name
    let pName = p.Type SignatureType.Name
    errExprMismatchQuantifierVariableTypes aName pName xName yName index  

/// Instantiates an expression by replacing variables with the expressions
/// recorded in the variable-usage dictionary.
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


/// Creates a string representation of a quantifier formula in which its bound variables are replaced by
/// placeholders numbered according to the order of the bound variables
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

/// Matches a candidate expression against a pattern expression while recording
/// a consistent variable-usage map for later substitution.
let matchExpressionAgainstPattern (candidate:FplGenericNode) (pattern:FplGenericNode) (dictParameterUsage: Dictionary<string, FplGenericNode>) =

    // Tracks bound-variable correspondences established by quantifier matching.
    // Keyed by the pattern variable node (reference equality), not its string name,
    // so that identically-named bound variables in different quantifier scopes never collide.
    let boundVarMap = Dictionary<FplGenericNode, FplGenericNode>()

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

    // If the pattern is a parameterized variable reference, clone the variable and
    // project the referenced arguments onto the cloned parameters so matching can
    // account for bound variables consistently.
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

    let getNormalizedExpressionForMatching (expression: FplGenericNode) =
        match expression.Name, expression.RefersTo with
        | PrimRefL, Some referenced when referenced.Name = PrimDelegateEqualL ->
            referenced
        | _ ->
            expression

    let tryGetTransparentReferenceOperator (reference: FplGenericNode) =
        match reference.RefersTo with
        | Some (:? FplGenericHasValue as definition) when definition.ArgList.Count > 0 ->
            definition.ArgList[0]
            |> getNormalizedExpressionForMatching
            |> fun expression -> Some expression.Name
        | _ ->
            None

    let haveSameTransparentReferenceOperator (a: FplGenericNode) (p: FplGenericNode) =
        match tryGetTransparentReferenceOperator a, tryGetTransparentReferenceOperator p with
        | Some aOperator, Some pOperator -> aOperator = pOperator
        | _ -> false

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


    // The usage dictionary records the first expression matched to each pattern
    // variable and enforces that every later occurrence matches the same expression.
    // It is also reused to instantiate the final matched expression.
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

/// Tries to match a premise with expressions from a list and returns 
/// a list of matched expressions and a string of concatenated failed candidate expressions
let private matchPremiseWithSomeExpressions (exprList:FplGenericNode list) (pre:FplGenericNode) (dictParameterUsage:Dictionary<string, FplGenericNode>)=

    let result = List<FplGenericNode * Dictionary<string, FplGenericNode>>()
    let failedCandidates = List<string>()

    exprList
    |> List.iter (fun expr ->
        let errOpt = matchExpressionAgainstPattern expr pre dictParameterUsage
        match errOpt with
        | None -> result.Add (expr, dictParameterUsage)
        | Some err -> failedCandidates.Add ($"`{expr.Type SignatureType.Name}`{Environment.NewLine}  ⚡{err}")
    )
    result |> Seq.toList, (numbered failedCandidates)

/// Flag that a proof justification or inference cannot collect preceding results
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

/// Flag that a proof justification or inference cannot collect preceding results with a special reason
let issuePR022SpecialReasonAndSetDefault (fv:FplGenericHasValue) reason =
    fv.ErrorOccurred <- emitPR022Diagnostics reason fv.StartPos fv.EndPos
    fv.SetDefaultValue()


[<AbstractClass>]
type FplGenericInfering(positions: Positions, parent: FplGenericNode) =
    inherit FplGenericPredicate(positions, parent)

    abstract member InferredExprCandidates: FplGenericNode list with get


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

[<AbstractClass>]
type FplGenericArgInference(positions: Positions, parent: FplGenericNode) =
    inherit FplGenericInfering(positions, parent)

    override this.Type signatureType =
        let head = getFplHead this signatureType
        head

    override this.EmbedInSymbolTable _ = addExpressionToParentArgList this



let matchJustItemsExpressionsAgainstPremiseList (tuplesJustItemWithInferredExpressionsList:(FplGenericJustificationItem * FplGenericNode list) list) (premiseList:FplGenericNode list) (byInferenceNode:FplGenericNode) =
    let varUsageDict = Dictionary<string, FplGenericNode>()
    let result = List<(FplGenericNode * Dictionary<string, FplGenericNode>) list>()
    let rec matchJustItemsExpressionsAgainstPremiseListRec (iJeLists:(FplGenericJustificationItem * FplGenericNode list) list) (preList:FplGenericNode list) =
        match iJeLists, preList with
        | iJel::iJels, pre::pres ->
            let just = fst iJel
            let inferredExpressionsOfJust = snd iJel
            match matchPremiseWithSomeExpressions inferredExpressionsOfJust pre varUsageDict with
            | [], errList ->
                // emit diagnostics at just's position that there was no matching candidate for a premise, listing all tried-out candidates (contained in errList)
                let premisesPre =
                    premiseList
                    |> List.map (fun prem -> prem.Type SignatureType.Name)
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

