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
open FplInterpreterDiagnosticsEmitter
open FplInterpreter.Globals.HelpersBasic
open FplInterpreterBasicTypes
open FplInterpreterIntrinsicTypes
open FplInterpreterFplTypeMatching

let private errMsgMismatchingExistsN (a:FplGenericNode) (p:FplGenericNode) =
    false, $"found mismatching exists `{a.FplId}` in `{a.Type SignatureType.Name}`, expecting type `{p.FplId}` in `{p.Type SignatureType.Name}`"

let private errMsgMismatchingQuantorVariableTypes (a:FplGenericNode) (p:FplGenericNode) (x:FplGenericNode) (y:FplGenericNode) index =
    let xType = x.Type SignatureType.Type
    let yType = y.Type SignatureType.Type
    false, $"found mismatching type `{x.FplId}:{xType}` at {ordinalPostfix index} quantor variable in `{a.Type SignatureType.Name}`, expecting type `{y.FplId}:{yType}` in `{p.Type SignatureType.Name}`"

let private errMsgMismatchingQuantorVariableCounts (a:FplGenericNode) (p:FplGenericNode) aVarsCount pVarsCount =
    false, $"found {aVarsCount} quantor variables in `{a.Type SignatureType.Name}`, expected {pVarsCount} in `{p.Type SignatureType.Name}`" 

let private errMsgMismatchingOpenFormulas (aOriginal:FplGenericNode) (aOpenFormula:FplGenericNode) (aFreeVars:FplGenericNode list) (pOriginal:FplGenericNode) (pOpenFormula:FplGenericNode) (pFreeVars:FplGenericNode list) = 
    let aName = aOriginal.Type SignatureType.Name
    let aOpenFormulaType = aOpenFormula.Type SignatureType.Type
    let pName = pOriginal.Type SignatureType.Name
    let pOpenFormulaType = pOpenFormula.Type SignatureType.Type
    let openClosedStr (lstFreeVars:FplGenericNode list) =
        if lstFreeVars.Length > 0 then
            $"an open formula with the free variables {lstToString lstFreeVars SignatureType.Name}"
        else
            "a closed formula"
    false, $"found expression `{aName}` ({openClosedStr aFreeVars} typed `{aOpenFormulaType}`), expected `{pName}` typed `{openClosedStr pFreeVars} typed {pOpenFormulaType}`"

let private errMsgExpectedEndOfFormula (a:FplGenericNode) =
    false, $"`found {a.Type SignatureType.Name}`, expected end of formula"

let private errMsgFoundEndOfFormula (p:FplGenericNode) =
    false, $"found end of formula, expected `{p.Type SignatureType.Name}`"

let private errMsgVarMatchedDifferently varName expectedExpr actualExpr = 
    false, $"variable `{varName}` matched with different formulas `{expectedExpr}` and `{actualExpr}`"

let private errMsgStandard (a:FplGenericNode) (p:FplGenericNode) = 
    false, $"found `{a.Type SignatureType.Name}`, expected `{p.Type SignatureType.Name}`"

let private noErr = 
    true, ""

let private compareQuantorVariables (a:FplGenericNode) (p:FplGenericNode) (dictParameterUsage:Dictionary<string, FplGenericNode>) =
    let pVars = p.GetVariables()
    let aVars = a.GetVariables()
    let rec loop l1 l2 index =
        match l1, l2 with
        | [], [] ->
            match a.Name with
            | PrimQuantorExistsN when a.Name = p.Name && a.FplId <> p.FplId ->
                errMsgMismatchingExistsN a p
            | _ ->
                noErr   // no mismatches
        | (x:FplGenericNode)::xs, (y:FplGenericNode)::ys ->
            match FplTypeMatcher.MatchPwA [x] [y] with
            | Some _ ->
                errMsgMismatchingQuantorVariableTypes a p x y index
            | _ ->
                // remember corresponding quantor variables of the matched quantors 
                dictParameterUsage.TryAdd (y.FplId, x) |> ignore 
                loop xs ys (index + 1)
        | _ ->
            // Should not happen if lengths are equal, but included for safety
            errMsgMismatchingQuantorVariableCounts a p aVars.Length pVars.Length
    loop aVars pVars 0

let private checkMismatchingUsageOfVars varName (a:FplGenericNode) (dictParameterUsage:Dictionary<string, FplGenericNode>) = 
    if dictParameterUsage.TryAdd (varName, a) then
        noErr
    else
        let expectedExpr = (dictParameterUsage[varName].Type SignatureType.Name)
        let actualExpr = (a.Type SignatureType.Name)
        if expectedExpr<>actualExpr then
            errMsgVarMatchedDifferently varName expectedExpr actualExpr
        else
            noErr

let private comparisonBasedOnOpenFormulas (a:FplGenericNode) (p:FplGenericNode) (dictParameterUsage:Dictionary<string, FplGenericNode>) = 
    let aOpenFormulaOpt = FplTypeMatcher.GetOpenFormulaOfExpression a
    let pOpenFormulaOpt = FplTypeMatcher.GetOpenFormulaOfExpression p

    match aOpenFormulaOpt, pOpenFormulaOpt with
    | Some aOpenFormula, Some pOpenFormula ->
        let aFreeVars = getParameters aOpenFormula
        let pFreeVars = getParameters pOpenFormula
        match FplTypeMatcher.MatchPwA aFreeVars pFreeVars with
        | Some _ ->
            errMsgMismatchingOpenFormulas a aOpenFormula aFreeVars p pOpenFormula pFreeVars
        | None when aOpenFormula.TypeId <> pOpenFormula.TypeId ->
            errMsgMismatchingOpenFormulas a aOpenFormula aFreeVars p pOpenFormula pFreeVars
        | _ when p.Name = PrimRefL ->
            match p.RefersTo with
            | Some var when var.Name = PrimVariableL ->
                checkMismatchingUsageOfVars p.FplId a dictParameterUsage
            | _ -> noErr
        | _ -> 
            noErr
    | _, _ ->
        errMsgStandard a p // fallback, should never happen unless open formula calculation somehow fails

let private checkExprWrapper (a:FplGenericNode) (p:FplGenericNode) =
    // When p is a variable, the dict stores the variable names and their usage in a first matched a.
    // The dictionary is used to check the consistency of the usage of the same variable p in the whole formula
    // during the matching process. Moreover, the dict is used generate the
    // conclusion of the rule of inference after all variables declared in its premise were used.
    let dictParameterUsage = Dictionary<string, FplGenericNode>()
    let rec checkExpr (a:FplGenericNode) (p:FplGenericNode) =
        let rec checkExpressions (args:FplGenericNode list) (pars:FplGenericNode list) =
            match args, pars with
            | a::ars, p::prs ->
                let ok, msg = checkExpr a p 
                if ok then
                    checkExpressions ars prs
                else
                    false, msg
            | a::_, [] ->
                errMsgExpectedEndOfFormula a
            | [], p::_ ->
                errMsgFoundEndOfFormula p
            | [], [] ->
                noErr

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
            | true, "" ->
                // and now check the expressions inside the quantors
                checkExpressions (a.ArgList |> Seq.toList) (p.ArgList |> Seq.toList) 
            | _, err -> false, err
        | PrimFalse, PrimFalse 
        | PrimTrue, PrimTrue ->
            noErr
        | PrimRefL, PrimRefL ->
            match a.RefersTo, p.RefersTo with
            | Some aRef, Some pRef ->
                checkExpr aRef pRef
            | Some aRef, None ->
                errMsgExpectedEndOfFormula aRef
            | None, Some pRef ->
                errMsgFoundEndOfFormula pRef
            | None, None ->
                noErr
        | _, PrimRefL when p.RefersTo.IsSome && p.RefersTo.Value.Name = PrimVariableL ->
            comparisonBasedOnOpenFormulas a p dictParameterUsage
        | _, PrimVariableL ->
            match FplTypeMatcher.MatchArgumentsWithParameters a p with
            | Some err ->
                false, err
            | None ->
                checkMismatchingUsageOfVars p.FplId a dictParameterUsage
        | _, _ ->
            errMsgStandard a p 
    checkExpr a p, dictParameterUsage

/// Tries to match a premise with expressions from a list and returns as a result
/// a list of matched expressions and a  string of concatenated failed candidate expressions
let private matchPremiseWithSomeExpressions (exprList:FplGenericNode list) (pre:FplGenericNode) (iJel:FplGenericNode) =
    let result = List<FplGenericNode>()
    let failedCandidates = List<string>()
    exprList
    |> List.iter (fun expr ->
        let ((ok, err), varUsageDict) = checkExprWrapper expr pre
        match ok, err with
        | true, _ ->
            result.Add expr
        | false, err ->
            failedCandidates.Add ($"`{expr.Type SignatureType.Name}`{Environment.NewLine}  ⚡{err}")
    )
    result |> Seq.toList, (numbered failedCandidates)

let matchJustItemsExpressionsAgainstPremiseList (tuplesJustItemWithProceedingExpressionsList:(FplGenericJustificationItem * FplGenericNode list) list) (premiseList:FplGenericNode list) (byInferenceNode:FplGenericNode) =
    let result = List<FplGenericNode list>()
    let rec matchJustItemsExpressionsAgainstPremiseListRec (iJeLists:(FplGenericJustificationItem * FplGenericNode list) list) (preList:FplGenericNode list) =
        match iJeLists, preList with
        | iJel::iJels, pre::pres ->
            let just = fst iJel
            let proceedingExpressionsOfJust = snd iJel
            match matchPremiseWithSomeExpressions proceedingExpressionsOfJust pre just with
            | [], errList ->
                // emit diagnostics at just's position that there was no matching candidate for a premise, listing all tried-out candidates (contained in errList)
                just.ErrorOccurred <- emitPR008Diagnostics (byInferenceNode.Type SignatureType.Name) (pre.Type SignatureType.Name) errList just.StartPos just.EndPos
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
    result |> Seq.toList
