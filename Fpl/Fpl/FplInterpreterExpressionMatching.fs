/// This module contains all functions needed by the FplInterpreter
/// to match expressions for proof arguments by rules of inferences.

(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)
module FplInterpreterExpressionMatching
open FplPrimitives
open FplInterpreterDiagnosticsEmitter
open FplInterpreterBasicTypes


type MatchExprType =
    | MatchWith2Args
    | MatchWith1Arg
    | Mismatch

let checkExpr (a:FplGenericNode) (p:FplGenericNode) =
    match a.Name, p.Name with
    | PrimConjunction, PrimConjunction
    | PrimDisjunction, PrimDisjunction
    | PrimImplication, PrimImplication
    | PrimEquivalence, PrimEquivalence
    | PrimExclusiveOr, PrimExclusiveOr -> MatchExprType.MatchWith2Args
    | PrimNegation, PrimNegation -> MatchExprType.MatchWith1Arg
    | _, _ -> MatchExprType.Mismatch

let rec checkExpressions (args:FplGenericNode list) (pars:FplGenericNode list) (fvJi:FplGenericNode) =
    match args, pars with
    | a::ars, p::prs ->
        match checkExpr a p with
        | MatchExprType.Mismatch ->
            fvJi.ErrorOccurred <- emitPR008Diagnostics "nodeName" "expectedInput" "expectedOutput" fvJi.StartPos fvJi.EndPos
            false
        | MatchExprType.MatchWith2Args ->
            (checkExpressions (a.ArgList |> Seq.toList) (p.ArgList |> Seq.toList) fvJi && checkExpressions ars prs fvJi)
        | MatchExprType.MatchWith1Arg ->
            (checkExpressions (a.ArgList |> Seq.toList) (p.ArgList |> Seq.toList) fvJi && checkExpressions ars prs fvJi)
    | a::_, [] ->
        fvJi.ErrorOccurred <- emitPR008Diagnostics "nodeName" "expectedInput" "expectedOutput" fvJi.StartPos fvJi.EndPos
        false
    | [], p::_ ->
        fvJi.ErrorOccurred <- emitPR008Diagnostics "nodeName" "expectedInput" "expectedOutput" fvJi.StartPos fvJi.EndPos
        false
    | [], [] ->
        true
