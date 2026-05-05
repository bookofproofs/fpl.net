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
open ErrMessages
open FplInterpreterDiagnosticsEmitter
open FplInterpreterBasicTypes
open FplInterpreterIntrinsicTypes
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

let private checkMismatchingUsageOfVars varName (a:FplGenericNode) (dictParameterUsage:Dictionary<string, FplGenericNode>) = 
    if dictParameterUsage.TryAdd (varName, a) then
        errExprMismatchOK
    else
        let expectedExpr = (dictParameterUsage[varName].Type SignatureType.Name)
        let actualExpr = (a.Type SignatureType.Name)
        if expectedExpr<>actualExpr then
            errExprMismatchVarMatchedDifferently varName expectedExpr actualExpr
        else
            errExprMismatchOK

let private checkExprWrapper (a:FplGenericNode) (p:FplGenericNode) (dictParameterUsage: Dictionary<string, FplGenericNode>) =
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
                checkExpr aRef pRef
            | Some aRef, None ->
                errExprMismatchExpectedEndOfFormula (aRef.Type SignatureType.Name)
            | None, Some pRef ->
                errExprMismatchFoundEndOfFormula (pRef.Type SignatureType.Name)
            | None, None ->
                errExprMismatchOK
        | _, PrimRefL when p.RefersTo.IsSome && p.RefersTo.Value.Name = PrimVariableL ->
            let (errMsgOpt,_) = FplTypeMatcher.ComparisonBasedOnOpenFormulas a p
            match errMsgOpt, p.RefersTo with
            | None, Some var when var.Name = PrimVariableL ->
                checkMismatchingUsageOfVars p.FplId a dictParameterUsage
            | Some errMsg, _ -> Some errMsg
            | _,_ ->
                errExprMismatchOK
        | _, PrimVariableL ->
            match FplTypeMatcher.MatchArgumentsWithParameters a p with
            | Some err -> Some err
            | None -> checkMismatchingUsageOfVars p.FplId a dictParameterUsage
        | _, _ ->
            errExprMismatchMsgStandard (a.Type SignatureType.Name) (p.Type SignatureType.Name)
    checkExpr a p, dictParameterUsage

/// Tries to match a premise with expressions from a list and returns 
/// a list of matched expressions and a string of concatenated failed candidate expressions
let private matchPremiseWithSomeExpressions (exprList:FplGenericNode list) (pre:FplGenericNode) (iJel:FplGenericNode) =

    let result = List<FplGenericNode * Dictionary<string, FplGenericNode>>()
    let failedCandidates = List<string>()

    exprList
    |> List.iter (fun expr ->
        let dictParameterUsage = Dictionary<string, FplGenericNode>()
        let (errOpt, varUsageDict) = checkExprWrapper expr pre dictParameterUsage
        match errOpt with
        | None -> result.Add (expr, varUsageDict)
        | Some err -> failedCandidates.Add ($"`{expr.Type SignatureType.Name}`{Environment.NewLine}  ⚡{err}")
    )
    result |> Seq.toList, (numbered failedCandidates)

let matchJustItemsExpressionsAgainstPremiseList (tuplesJustItemWithProceedingExpressionsList:(FplGenericJustificationItem * FplGenericNode list) list) (premiseList:FplGenericNode list) (byInferenceNode:FplGenericNode) =
    let result = List<(FplGenericNode * Dictionary<string, FplGenericNode>) list>()
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
    result |> List.concat
