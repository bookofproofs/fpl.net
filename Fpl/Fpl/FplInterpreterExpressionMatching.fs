/// This module contains all functions needed by the FplInterpreter
/// to match expressions for proof arguments by rules of inferences.

(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)
module FplInterpreterExpressionMatching
open System
open System.Collections.Generic
open FplPrimitives
open FplGrammarTypes
open ErrMessages
open FplInterpreterDiagnosticsEmitter
open FplInterpreterChecks
open FplInterpreterBasicTypes
open FplInterpreter.Globals.Debug
open FplInterpreter.Globals.HelpersBasic
open FplInterpreterIntrinsicTypes
open FplInterpreterVariables
open FplInterpreterFplTypeMatching


let private errExprMismatchQuantorVariableTypesWrapper (a:FplGenericNode) (p:FplGenericNode) (x:FplGenericNode) (y:FplGenericNode) index =
    let xName = $"{x.FplId}:{x.Type SignatureType.Type}"
    let yName = $"{y.FplId}:{y.Type SignatureType.Type}"
    let aName = a.Type SignatureType.Name
    let pName = p.Type SignatureType.Name
    errExprMismatchQuantorVariableTypes aName pName xName yName index  

let private compareQuantorVariables (a:FplGenericNode) (p:FplGenericNode) (dictParameterUsage:Dictionary<string, FplGenericNode>) =
    let pVars = p.GetVariables()
    let aVars = a.GetVariables()
    let rec loop l1 l2 index =
        match l1, l2 with
        | [], [] ->
            match a.Name with
            | PrimQuantorExistsN when a.Name = p.Name && a.FplId <> p.FplId ->
                errExprMismatchExistsN a.FplId (a.Type SignatureType.Name) p.FplId (p.Type SignatureType.Name)
            | _ ->
                errExprMismatchOK   // no mismatches
        | (x:FplGenericNode)::xs, (y:FplGenericNode)::ys ->
            match FplTypeMatcher.MatchPwA [x] [y] with
            | Some _ ->
                errExprMismatchQuantorVariableTypesWrapper a p x y index
            | _ ->
                // remember corresponding quantor variables of the matched quantors 
                dictParameterUsage.TryAdd (y.FplId, x) |> ignore 
                loop xs ys (index + 1)
        | _ ->
            // Should not happen if lengths are equal, but included for safety
            errExprMismatchQuantorVariableCounts (a.Type SignatureType.Name) (p.Type SignatureType.Name) aVars.Length pVars.Length
    loop aVars pVars 0

/// Creates a string representation of a quantor formula in which its bound variables are replaced by
/// placeholders numbered according to the order of the bound variables
let private getNameOfQuantorFormulaModuloBoundVarNames (fv:FplGenericNode) =
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
        if a.Name = previouslyMatchedFormula.Name && isQuantor a && isQuantor previouslyMatchedFormula then
            let expectedExprModVarNames = getNameOfQuantorFormulaModuloBoundVarNames previouslyMatchedFormula
            let actualExprModVarNames = getNameOfQuantorFormulaModuloBoundVarNames a
            if expectedExprModVarNames<>actualExprModVarNames then
                let expectedExpr = previouslyMatchedFormula.Type SignatureType.Name
                let actualExpr = (a.Type SignatureType.Name)
                errExprMismatchVarMatchedDifferentlyQuantor varName expectedExpr actualExpr
            else
                errExprMismatchOK
        else
            let expectedExpr = previouslyMatchedFormula.Type SignatureType.Name
            let actualExpr = (a.Type SignatureType.Name)
            if expectedExpr<>actualExpr then
                errExprMismatchVarMatchedDifferently varName expectedExpr actualExpr
            else
                errExprMismatchOK

let private checkExprWrapper (a:FplGenericNode) (p:FplGenericNode) (dictParameterUsage: Dictionary<string, FplGenericNode>) =

    // When a reference refQ refers to a variable q and the variable q has parameter variables q(a,b,...), we need to mark which parameter variables are bound and which are not
    // The information is in the arguments x,y, of the original refQ(x,y, ...)
    // The best way to do replace q by a cloned version of q and replace its declared parameters with used ones.
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

    // When p is a variable, the dict stores the variable names and their usage in a first matched a.
    // The dictionary is used to check the consistency of the usage of the same variable p in the whole formula
    // during the matching process. Moreover, the dict is used generate the
    // conclusion of the rule of inference after all variables declared in its premise were used.
    let rec checkExpr (a:FplGenericNode) (p:FplGenericNode) =
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

        match a.Name, p.Name with
        | PrimConjunction, PrimConjunction
        | PrimDisjunction, PrimDisjunction
        | PrimImplication, PrimImplication
        | PrimEquivalence, PrimEquivalence
        | PrimExclusiveOr, PrimExclusiveOr
        | PrimNegation, PrimNegation -> checkExpressions (a.ArgList |> Seq.toList) (p.ArgList |> Seq.toList) 
        | PrimQuantorAll, PrimQuantorAll 
        | PrimQuantorExists, PrimQuantorExists 
        | PrimQuantorExistsN, PrimQuantorExistsN ->
        // match number of quantor variables
            match compareQuantorVariables a p dictParameterUsage with
            | None ->
                // and now check the expressions inside the quantors
                checkExpressions (a.ArgList |> Seq.toList) (p.ArgList |> Seq.toList) 
            | Some err -> Some err
        | PrimFalse, PrimFalse 
        | PrimTrue, PrimTrue ->
            errExprMismatchOK
        | PrimRefL, PrimRefL ->
            match a.RefersTo, p.RefersTo with
            | Some aRef, Some pRef ->
                checkExpr (mockVariableWithParams a aRef) (mockVariableWithParams p pRef)
            | Some aRef, None when p.ArgList.Count > 0 && not p.ExpressionType.IsParen ->
                checkExpr (mockVariableWithParams a aRef) p
            | None, Some pRef when a.ArgList.Count > 0 && not a.ExpressionType.IsParen ->
                checkExpr a (mockVariableWithParams p pRef)
            | None, Some pRef when a.ExpressionType.IsParen ->
                // delegate parenthesized (a) to a
                checkExpr a.ArgList[0] p
            | Some aRef, None when p.ExpressionType.IsParen ->
                // delegate parenthesized (p) to p
                checkExpr a p.ArgList[0]
            | None, None when a.ExpressionType.IsParen && p.ExpressionType.IsParen ->
                // delegate parenthesized (a) (p) to a p
                checkExpr a.ArgList[0] p.ArgList[0]
            | None, None when a.ExpressionType.IsParen && not p.ExpressionType.IsParen ->
                errExprMismatchMsgParensOnlyLeft (a.Type SignatureType.Name) (p.Type SignatureType.Name)
            | None, None when not a.ExpressionType.IsParen && p.ExpressionType.IsParen ->
                errExprMismatchMsgParensOnlyRight (a.Type SignatureType.Name) (p.Type SignatureType.Name)
            | _, _ ->
                errExprMismatchOK
        | _, PrimRefL when p.RefersTo.IsSome && p.RefersTo.Value.Name = PrimVariableL ->
            let (errMsgOpt,_) = FplTypeMatcher.ComparisonBasedOnOpenFormulas a p
            match errMsgOpt, p.RefersTo with
            | None, Some var when var.Name = PrimVariableL ->
                let firstResult = checkMismatchingUsageOfVars p.FplId a dictParameterUsage
                match firstResult with
                | Some errMsg -> Some errMsg
                | None when var.Scope.Count > 0 ->
                    let pPars = getArguments p
                    let aPars = getDistinctVarsOfExpression a
                    if aPars.Length <> pPars.Length then
                        let aVars = aPars |> List.map (fun v -> $"{v.FplId}") |> String.concat ", "
                        let pName = p.Type SignatureType.Name
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
        | _, PrimVariableL ->
            match FplTypeMatcher.MatchArgumentsWithParameters a p with
            | Some err -> Some err
            | None -> checkMismatchingUsageOfVars p.FplId a dictParameterUsage
        | _, _ ->
            errExprMismatchMsgStandard (a.Type SignatureType.Name) (p.Type SignatureType.Name)
    checkExpr a p

/// Tries to match a premise with expressions from a list and returns 
/// a list of matched expressions and a string of concatenated failed candidate expressions
let private matchPremiseWithSomeExpressions (exprList:FplGenericNode list) (pre:FplGenericNode) (dictParameterUsage:Dictionary<string, FplGenericNode>)=

    let result = List<FplGenericNode * Dictionary<string, FplGenericNode>>()
    let failedCandidates = List<string>()

    exprList
    |> List.iter (fun expr ->
        let errOpt = checkExprWrapper expr pre dictParameterUsage
        match errOpt with
        | None -> result.Add (expr, dictParameterUsage)
        | Some err -> failedCandidates.Add ($"`{expr.Type SignatureType.Name}`{Environment.NewLine}  ⚡{err}")
    )
    result |> Seq.toList, (numbered failedCandidates)



[<AbstractClass>]
type FplGenericWithProceedingExpression(positions: Positions, parent: FplGenericNode) =
    inherit FplGenericPredicate(positions, parent)

    abstract member ProceedingExprCandidates: FplGenericNode list with get


/// This functions checs if ref is a predicate expression and assumes that it was already run (evaluated)
/// so and its value can be copied into fv. If ref is not a predicate expression, fv's value will be set to default and a diagnostics will be issued.
let checkIfIsPredicateAndSetDefaultValueIfNot (fv:FplGenericHasValue) (ref:FplGenericHasValue) = 
    let refType, isRefPred = isArgPred ref
    if isRefPred then 
        // copy the value value into fv
        fv.SetValueOf ref
    else
        // if there is a value but ref is not a predicate, 
        // set the value of "this" to undetermined
        fv.SetDefaultValue()
        // and issue diagnostics saying that this requires a predicate
        let refName = ref.Type SignatureType.Name
        fv.ErrorOccurred <- emitLG001Diagnostics refType refName fv.Name ref.StartPos ref.StartPos

[<AbstractClass>]
type FplGenericJustificationItem(positions: Positions, parent: FplGenericNode) =
    inherit FplGenericWithProceedingExpression(positions, parent)

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
        | Some otherId ->
            this.ErrorOccurred <- emitPR004Diagnostics thisJustificationItemId otherId this.StartPos this.EndPos 
        | _ -> ()
        addExpressionToParentArgList this

    override this.Run() = 
        debug this Debug.Start
        match this.RefersTo with 
        | Some (:? FplGenericHasValue as ref) when ref.Value.IsSome ->
            checkIfIsPredicateAndSetDefaultValueIfNot this ref
        | Some (:? FplGenericHasValue as ref) when ref.Value.IsNone ->
            // set the value of "this" to undetermined
            ref.SetDefaultValue()
            this.SetValueOf ref
            // TODO issue diagnostics saying that a predicate expression was expected but has No value
        | _ -> 
            this.SetDefaultValue()
        debug this Debug.Stop

[<AbstractClass>]
type FplGenericArgInference(positions: Positions, parent: FplGenericNode) =
    inherit FplGenericWithProceedingExpression(positions, parent)

    override this.Type signatureType =
        let head = getFplHead this signatureType
        head

    override this.EmbedInSymbolTable _ = addExpressionToParentArgList this



let matchJustItemsExpressionsAgainstPremiseList (tuplesJustItemWithProceedingExpressionsList:(FplGenericJustificationItem * FplGenericNode list) list) (premiseList:FplGenericNode list) (byInferenceNode:FplGenericNode) =
    let varUsageDict = Dictionary<string, FplGenericNode>()
    let result = List<(FplGenericNode * Dictionary<string, FplGenericNode>) list>()
    let rec matchJustItemsExpressionsAgainstPremiseListRec (iJeLists:(FplGenericJustificationItem * FplGenericNode list) list) (preList:FplGenericNode list) =
        match iJeLists, preList with
        | iJel::iJels, pre::pres ->
            let just = fst iJel
            let proceedingExpressionsOfJust = snd iJel
            match matchPremiseWithSomeExpressions proceedingExpressionsOfJust pre varUsageDict with
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
            byInferenceNode.ErrorOccurred <- emitPR020Diagnostics (preList.Length + 1) (iJeLists.Length + 1) byInferenceNode.StartPos byInferenceNode.EndPos
        | _::_, [] ->
            byInferenceNode.ErrorOccurred <- emitPR020Diagnostics (preList.Length + 1) (iJeLists.Length + 1) byInferenceNode.StartPos byInferenceNode.EndPos
        | [], [] -> ()
            
    matchJustItemsExpressionsAgainstPremiseListRec tuplesJustItemWithProceedingExpressionsList premiseList
    let res = result |> List.concat
    res

