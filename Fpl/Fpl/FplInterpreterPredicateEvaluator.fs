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
    let arg = fplValue.ArgList[0]
    match getRepresentation arg with 
    | "false" -> 
        let newValue = createFplValue((fplValue.StartPos, fplValue.EndPos), FplBlockType.IntrinsicPred, fplValue)
        newValue.FplId <- "true"
        fplValue.ValueList.Add(newValue)
    | "true" -> 
        let newValue = createFplValue((fplValue.StartPos, fplValue.EndPos), FplBlockType.IntrinsicPred, fplValue)
        newValue.FplId <- "false"
        fplValue.ValueList.Add(newValue)
    | "undetermined" -> 
        fplValue.ValueList.Add(arg)
    | _ -> 
        let newValue = createFplValue((fplValue.StartPos, fplValue.EndPos), FplBlockType.IntrinsicPred, fplValue)
        fplValue.ValueList.Add(newValue)
    
let evaluateConjunction (fplValue:FplValue) =
    let arg1 = fplValue.ArgList[0]
    let arg2 = fplValue.ArgList[1]
    let arg1Repr = getRepresentation arg1
    let arg2Repr = getRepresentation arg2
    match (arg1Repr, arg2Repr) with
    | ("true", "false") 
    | ("false", "true") 
    | ("false", "false") -> 
        let newValue = createFplValue((fplValue.StartPos, fplValue.EndPos), FplBlockType.IntrinsicPred, fplValue)
        newValue.FplId <- "false"
        fplValue.ValueList.Add(newValue)
    | ("true", "true") -> 
        let newValue = createFplValue((fplValue.StartPos, fplValue.EndPos), FplBlockType.IntrinsicPred, fplValue)
        newValue.FplId <- "true"
        fplValue.ValueList.Add(newValue)
    | _ -> 
        let newValue = createFplValue((fplValue.StartPos, fplValue.EndPos), FplBlockType.IntrinsicPred, fplValue)
        fplValue.ValueList.Add(newValue)

let evaluateDisjunction (fplValue:FplValue) = 
    let arg1 = fplValue.ArgList[0]
    let arg2 = fplValue.ArgList[1]
    let arg1Repr = getRepresentation arg1
    let arg2Repr = getRepresentation arg2
    match (arg1Repr, arg2Repr) with
    | ("true", "false") 
    | ("false", "true") 
    | ("true", "true") -> 
        let newValue = createFplValue((fplValue.StartPos, fplValue.EndPos), FplBlockType.IntrinsicPred, fplValue)
        newValue.FplId <- "true"
        fplValue.ValueList.Add(newValue)
    | ("false", "false") -> 
        let newValue = createFplValue((fplValue.StartPos, fplValue.EndPos), FplBlockType.IntrinsicPred, fplValue)
        newValue.FplId <- "false"
        fplValue.ValueList.Add(newValue)
    | _ -> 
        let newValue = createFplValue((fplValue.StartPos, fplValue.EndPos), FplBlockType.IntrinsicPred, fplValue)
        fplValue.ValueList.Add(newValue)

let evaluateExclusiveOr (fplValue:FplValue) = 
    let arg1 = fplValue.ArgList[0]
    let arg2 = fplValue.ArgList[1]
    let arg1Repr = getRepresentation arg1
    let arg2Repr = getRepresentation arg2
    match (arg1Repr, arg2Repr) with
    | ("true", "false") 
    | ("false", "true") -> 
        let newValue = createFplValue((fplValue.StartPos, fplValue.EndPos), FplBlockType.IntrinsicPred, fplValue)
        newValue.FplId <- "true"
        fplValue.ValueList.Add(newValue)
    | ("true", "true") 
    | ("false", "false") -> 
        let newValue = createFplValue((fplValue.StartPos, fplValue.EndPos), FplBlockType.IntrinsicPred, fplValue)
        newValue.FplId <- "false"
        fplValue.ValueList.Add(newValue)
    | _ -> 
        let newValue = createFplValue((fplValue.StartPos, fplValue.EndPos), FplBlockType.IntrinsicPred, fplValue)
        fplValue.ValueList.Add(newValue)

let evaluateImplication (fplValue:FplValue) = 
    let arg1 = fplValue.ArgList[0]
    let arg2 = fplValue.ArgList[1]
    let arg1Repr = getRepresentation arg1
    let arg2Repr = getRepresentation arg2
    match (arg1Repr, arg2Repr) with
    | ("true", "false") -> 
        let newValue = createFplValue((fplValue.StartPos, fplValue.EndPos), FplBlockType.IntrinsicPred, fplValue)
        newValue.FplId <- "false"
        fplValue.ValueList.Add(newValue)
    | ("false", "true") 
    | ("false", "false") 
    | ("true", "true") -> 
        let newValue = createFplValue((fplValue.StartPos, fplValue.EndPos), FplBlockType.IntrinsicPred, fplValue)
        newValue.FplId <- "true"
        fplValue.ValueList.Add(newValue)
    | _ -> 
        let newValue = createFplValue((fplValue.StartPos, fplValue.EndPos), FplBlockType.IntrinsicPred, fplValue)
        fplValue.ValueList.Add(newValue)

let evaluateEquivalence (fplValue:FplValue) = 
    let arg1 = fplValue.ArgList[0]
    let arg2 = fplValue.ArgList[1]
    let arg1Repr = getRepresentation arg1
    let arg2Repr = getRepresentation arg2
    match (arg1Repr, arg2Repr) with
    | ("true", "true") 
    | ("false", "false") -> 
        let newValue = createFplValue((fplValue.StartPos, fplValue.EndPos), FplBlockType.IntrinsicPred, fplValue)
        newValue.FplId <- "true"
        fplValue.ValueList.Add(newValue)
    | ("false", "true") 
    | ("true", "false") -> 
        let newValue = createFplValue((fplValue.StartPos, fplValue.EndPos), FplBlockType.IntrinsicPred, fplValue)
        newValue.FplId <- "false"
        fplValue.ValueList.Add(newValue)
    | _ -> 
        let newValue = createFplValue((fplValue.StartPos, fplValue.EndPos), FplBlockType.IntrinsicPred, fplValue)
        fplValue.ValueList.Add(newValue)
    
let evaluateIsOperator (fplValue:FplValue) (operand:FplValue) (typeOfOperand:FplValue) = 
    match mpwa [operand] [typeOfOperand] with
    | Some errMsg -> 
        let newValue = createFplValue((fplValue.StartPos, fplValue.EndPos), FplBlockType.IntrinsicPred, fplValue)
        newValue.FplId <- "false"
        fplValue.ValueList.Add(newValue)
    | None -> 
        let newValue = createFplValue((fplValue.StartPos, fplValue.EndPos), FplBlockType.IntrinsicPred, fplValue)
        newValue.FplId <- "true"
        fplValue.ValueList.Add(newValue)        
