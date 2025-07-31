/// This module implements the inbuilt compound predicates of FPL (e.g., negation, implication, etc).
module FplInterpreterPredicateEvaluator
open FplInterpreterTypes

(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)

let evaluateNegation (fplValue:FplValue) = 
    let argOpt = fplValue.ArgList[0].GetArgument
    match argOpt with
    | Some arg ->
        match arg.ReprId with
        | "false" -> 
            fplValue.ReprId <- "true"
        | "true" -> 
            fplValue.ReprId <- "false"
        | _ -> fplValue.ReprId <- "undetermined"
    | _ -> fplValue.ReprId <- "undetermined"
    
let evaluateConjunction (fplValue:FplValue) =
    let arg1Opt = fplValue.ArgList[0].GetArgument
    let arg2Opt = fplValue.ArgList[1].GetArgument
    match (arg1Opt,arg2Opt) with
    | (Some arg1, Some arg2) -> 
        match (arg1.ReprId, arg2.ReprId) with
        | ("true", "false") 
        | ("false", "true") 
        | ("false", "false") -> fplValue.ReprId <- "false" 
        | ("true", "true") -> fplValue.ReprId <- "true"
        | _ -> fplValue.ReprId <- "undetermined"
    | _ ->
        fplValue.ReprId <- "undetermined"

let evaluateDisjunction (fplValue:FplValue) = 
    let arg1Opt = fplValue.ArgList[0].GetArgument
    let arg2Opt = fplValue.ArgList[1].GetArgument
    match (arg1Opt,arg2Opt) with
    | (Some arg1, Some arg2) -> 
        match (arg1.ReprId, arg2.ReprId) with
        | ("true", "false") 
        | ("false", "true") 
        | ("true", "true") -> fplValue.ReprId <- "true"
        | ("false", "false") -> fplValue.ReprId <- "false" 
        | _ -> fplValue.ReprId <- "undetermined"
    | _ ->
        fplValue.ReprId <- "undetermined"

let evaluateExclusiveOr (fplValue:FplValue) = 
    let arg1Opt = fplValue.ArgList[0].GetArgument
    let arg2Opt = fplValue.ArgList[1].GetArgument
    match (arg1Opt,arg2Opt) with
    | (Some arg1, Some arg2) -> 
        match (arg1.ReprId, arg2.ReprId) with
        | ("true", "false") 
        | ("false", "true") -> fplValue.ReprId <- "true" 
        | ("true", "true") 
        | ("false", "false") -> fplValue.ReprId <- "false" 
        | _ -> fplValue.ReprId <- "undetermined"
    | _ ->
        fplValue.ReprId <- "undetermined"

let evaluateImplication (fplValue:FplValue) = 
    let arg1Opt = fplValue.ArgList[0].GetArgument
    let arg2Opt = fplValue.ArgList[1].GetArgument
    match (arg1Opt,arg2Opt) with
    | (Some arg1, Some arg2) -> 
        match (arg1.ReprId, arg2.ReprId) with
        | ("true", "false") -> fplValue.ReprId <- "false"
        | ("false", "true") 
        | ("false", "false") 
        | ("true", "true") -> fplValue.ReprId <- "true"
        | _ -> fplValue.ReprId <- "undetermined"
    | _ ->
        fplValue.ReprId <- "undetermined"

let evaluateEquivalence (fplValue:FplValue) = 
    let arg1Opt = fplValue.ArgList[0].GetArgument
    let arg2Opt = fplValue.ArgList[1].GetArgument
    match (arg1Opt,arg2Opt) with
    | (Some arg1, Some arg2) -> 
        match (arg1.ReprId, arg2.ReprId) with
        | ("true", "true") 
        | ("false", "false") -> fplValue.ReprId <- "true"
        | ("false", "true") 
        | ("true", "false") -> fplValue.ReprId <- "false"
        | _ -> fplValue.ReprId <- "undetermined"
    | _ ->
        fplValue.ReprId <- "undetermined"
    
let evaluateIsOperator (fv:FplValue) (operand:FplValue) (typeOfOperand:FplValue) = 
    match mpwa [operand] [typeOfOperand] with
    | Some errMsg -> fv.ReprId <- "false"
    | None -> fv.ReprId <- "true"
