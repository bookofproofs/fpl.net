/// This module implements the inbuilt compound predicates of FPL (e.g., negation, implication, etc).
module FplInterpreterPredicateEvaluator
open FplGrammarCommons
open FplInterpreterTypes

(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)

let evaluateNegation (fplValue:FplValue) = 
    let arg = fplValue.ArgList[0]
    match arg.Represent() with 
    | FplGrammarCommons.literalFalse -> 
        let newValue = new FplIntrinsicPred((fplValue.StartPos, fplValue.EndPos), fplValue)
        newValue.FplId <- literalTrue
        fplValue.ValueList.Add(newValue)
    | FplGrammarCommons.literalTrue -> 
        let newValue =  new FplIntrinsicPred((fplValue.StartPos, fplValue.EndPos), fplValue)
        newValue.FplId <- literalFalse
        fplValue.ValueList.Add(newValue)
    | FplGrammarCommons.literalUndetermined -> 
        fplValue.ValueList.Add(arg)
    | _ -> 
        let newValue = new FplIntrinsicPred((fplValue.StartPos, fplValue.EndPos), fplValue)
        fplValue.ValueList.Add(newValue)

let evaluateImplication (fplValue:FplValue) = 
    let arg1 = fplValue.ArgList[0]
    let arg2 = fplValue.ArgList[1]
    let arg1Repr = arg1.Represent()
    let arg2Repr = arg2.Represent()
    match (arg1Repr, arg2Repr) with
    | (FplGrammarCommons.literalTrue, FplGrammarCommons.literalFalse) -> 
        let newValue = new FplIntrinsicPred((fplValue.StartPos, fplValue.EndPos), fplValue)
        newValue.FplId <-literalFalse
        fplValue.ValueList.Add(newValue)
    | (FplGrammarCommons.literalFalse, FplGrammarCommons.literalTrue) 
    | (FplGrammarCommons.literalFalse, FplGrammarCommons.literalFalse) 
    | (FplGrammarCommons.literalTrue, FplGrammarCommons.literalTrue) -> 
        let newValue = new FplIntrinsicPred((fplValue.StartPos, fplValue.EndPos), fplValue)
        newValue.FplId <- literalTrue
        fplValue.ValueList.Add(newValue)
    | _ -> 
        let newValue = new FplIntrinsicPred((fplValue.StartPos, fplValue.EndPos), fplValue)
        fplValue.ValueList.Add(newValue)

let evaluateEquivalence (fplValue:FplValue) = 
    let arg1 = fplValue.ArgList[0]
    let arg2 = fplValue.ArgList[1]
    let arg1Repr = arg1.Represent()
    let arg2Repr = arg2.Represent()
    match (arg1Repr, arg2Repr) with
    | (FplGrammarCommons.literalTrue, FplGrammarCommons.literalTrue) 
    | (FplGrammarCommons.literalFalse, FplGrammarCommons.literalFalse) -> 
        let newValue = new FplIntrinsicPred((fplValue.StartPos, fplValue.EndPos), fplValue)
        newValue.FplId <- literalTrue
        fplValue.ValueList.Add(newValue)
    | (FplGrammarCommons.literalFalse, FplGrammarCommons.literalTrue) 
    | (FplGrammarCommons.literalTrue, FplGrammarCommons.literalFalse) -> 
        let newValue = new FplIntrinsicPred((fplValue.StartPos, fplValue.EndPos), fplValue)
        newValue.FplId <- literalFalse
        fplValue.ValueList.Add(newValue)
    | _ -> 
        let newValue = new FplIntrinsicPred((fplValue.StartPos, fplValue.EndPos), fplValue)
        fplValue.ValueList.Add(newValue)
    
let evaluateIsOperator (fplValue:FplValue) (operand:FplValue) (typeOfOperand:FplValue) = 
    match mpwa [operand] [typeOfOperand] with
    | Some errMsg -> 
        let newValue = new FplIntrinsicPred((fplValue.StartPos, fplValue.EndPos), fplValue)
        newValue.FplId <- literalFalse
        fplValue.ValueList.Add(newValue)
    | None -> 
        let newValue = new FplIntrinsicPred((fplValue.StartPos, fplValue.EndPos), fplValue)
        newValue.FplId <- literalTrue
        fplValue.ValueList.Add(newValue)        
