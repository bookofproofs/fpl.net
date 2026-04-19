/// This module contains all functions needed by the FplInterpreter
/// to match expressions for proof arguments by rules of inferences.

(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)
module FplInterpreterExpressionMatching
open System.Collections.Generic
open FplPrimitives
open FplInterpreterDiagnosticsEmitter
open FplInterpreterBasicTypes
open FplInterpreterIntrinsicTypes
open FplInterpreter.Globals.HelpersBasic


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
            false, $"`found {a.Type SignatureType.Name}`, expected end of formula"
        | [], p::_ ->
            false, $"found end of formula, expected `{p.Type SignatureType.Name}`"
        | [], [] ->
            true, ""

    match a.Name, p.Name with
    | PrimConjunction, PrimConjunction
    | PrimDisjunction, PrimDisjunction
    | PrimImplication, PrimImplication
    | PrimEquivalence, PrimEquivalence
    | PrimExclusiveOr, PrimExclusiveOr 
    | PrimNegation, PrimNegation -> checkExpressions (a.ArgList |> Seq.toList) (p.ArgList |> Seq.toList) 
    | _, _ -> false, $"found `{a.Type SignatureType.Name}`, expected `{p.Type SignatureType.Name}`"


/// Tries to match a premise with expressions from a list and returns as a result
/// a list of matched expressions and a  string of concatenated failed candidate expressions
let matchPremiseWithSomeExpressions (exprList:FplGenericNode list) (pre:FplGenericNode) (iJel:FplGenericNode) =
    let result = List<FplGenericNode>()
    let failedCandidates = List<string>()
    exprList
    |> List.iter (fun expr ->
        match checkExpr expr pre with
        | true, _ ->
            result.Add expr
        | false, err ->
            failedCandidates.Add ($"`{expr.Type SignatureType.Name}` [{err}]")
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
            byInferenceNode.ErrorOccurred <- emitPR020Diagnostics preList.Length iJeLists.Length byInferenceNode.StartPos byInferenceNode.EndPos
        | _::_, [] ->
            byInferenceNode.ErrorOccurred <- emitPR020Diagnostics preList.Length iJeLists.Length byInferenceNode.StartPos byInferenceNode.EndPos
        | [], [] -> ()
            
    matchJustItemsExpressionsAgainstPremiseListRec tuplesJustItemWithProceedingExpressionsList premiseList
    result |> Seq.toList
