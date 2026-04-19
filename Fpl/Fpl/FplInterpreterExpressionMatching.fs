/// This module contains all functions needed by the FplInterpreter
/// to match expressions for proof arguments by rules of inferences.

(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)
module FplInterpreterExpressionMatching
open System.Text
open System.Collections.Generic
open FplPrimitives
open FplInterpreterDiagnosticsEmitter
open FplInterpreterBasicTypes


let checkExpr (a:FplGenericNode) (p:FplGenericNode) =
    match a.Name, p.Name with
    | PrimConjunction, PrimConjunction
    | PrimDisjunction, PrimDisjunction
    | PrimImplication, PrimImplication
    | PrimEquivalence, PrimEquivalence
    | PrimExclusiveOr, PrimExclusiveOr 
    | PrimNegation, PrimNegation -> true
    | _, _ -> false



let rec checkExpressions (args:FplGenericNode list) (pars:FplGenericNode list) (fvJi:FplGenericNode) =
    match args, pars with
    | a::ars, p::prs ->
        match checkExpr a p with
        | false ->
            fvJi.ErrorOccurred <- emitPR008Diagnostics "nodeName" "expectedInput" "expectedOutput" fvJi.StartPos fvJi.EndPos
            false
        | true ->
            (checkExpressions (a.ArgList |> Seq.toList) (p.ArgList |> Seq.toList) fvJi && checkExpressions ars prs fvJi)
    | a::_, [] ->
        fvJi.ErrorOccurred <- emitPR008Diagnostics "nodeName" "expectedInput" "expectedOutput" fvJi.StartPos fvJi.EndPos
        false
    | [], p::_ ->
        fvJi.ErrorOccurred <- emitPR008Diagnostics "nodeName" "expectedInput" "expectedOutput" fvJi.StartPos fvJi.EndPos
        false
    | [], [] ->
        true

/// Tries to match a premise with expressions from a list and returns as a result
/// a list of matched expressions and a string of concatenated failed candidate expressions
let matchPremiseWithSomeExpressions (exprList:FplGenericNode list) (pre:FplGenericNode) =
    let result = List<FplGenericNode>()
    let failedCandidates = List<string>()
    exprList
    |> List.iter (fun expr ->
        if checkExpr expr pre then
            result.Add expr
        else
            failedCandidates.Add ($"`{expr.Type SignatureType.Name}`")
    )
    result |> Seq.toList, failedCandidates |> String.concat ", "

let matchInputJustificationItemsWithPremiseList (inputJustificationExpressionLists:FplGenericNode list list) (premiseList:FplGenericNode list) (fvJi:FplGenericNode) =
    let result = List<FplGenericNode list>()
    match inputJustificationExpressionLists, premiseList with
    | iJel::iJels, pre::pres ->
        match matchPremiseWithSomeExpressions iJel pre with
        | [], errList ->
            () // TODO: emit diagnostics at iJel's position that there was no matching candidate for a premise, listing all tried-out candidates (contained in errList)
        | matchedExprList, _ ->
            result.Add matchedExprList
    | [], _ ->
        () // TODO: emit diagnostitics at (pos1, pos2) that there are less input justification expression lists as premises
    | _, [] ->
        () // TODO: emit diagnostitics at (pos1, pos2) that there are more input justification expression lists as premises
    result |> Seq.toList
