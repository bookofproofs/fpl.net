﻿module FplInterpreterPredicateEvaluator
open FplInterpreterDiagnosticsEmitter
open FplInterpreterTypes
open ErrDiagnostics

let evaluateNegation (fplValue:FplValue) = 
    let argOpt = fplValue.ValueList[0].GetValue
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
    let arg1Opt = fplValue.ValueList[0].GetValue
    let arg2Opt = fplValue.ValueList[1].GetValue
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
    let vlist = fplValue.ValueList |> Seq.toList 
    let rec aggr (fv: FplValue list) =
        match fv with
        | x::xs -> 
            match x.ReprId with
            | "false" -> aggr xs 
            | "true" -> Some true
            | _ -> None
        | [] -> Some false
    match aggr vlist with 
    | Some true -> fplValue.ReprId <- "true"
    | Some false -> fplValue.ReprId <- "false"
    | _ -> fplValue.ReprId <- "undetermined"

let evaluateExclusiveOr (fplValue:FplValue) = 
    let vlist = fplValue.ValueList |> Seq.toList 
    let rec aggr (fv: FplValue list) oddCounterOfTrue counterNonBoolean =
        match fv with
        | x::xs -> 
            match x.ReprId with
            | "false" -> aggr xs oddCounterOfTrue counterNonBoolean
            | "true" -> aggr xs (oddCounterOfTrue + 1) counterNonBoolean
            | _ -> (oddCounterOfTrue, counterNonBoolean + 1) 
        | [] -> (oddCounterOfTrue, counterNonBoolean)
    let (oddCounterOfTrue, counterNonBoolean) = aggr vlist 0 0 
    if counterNonBoolean=0 && (oddCounterOfTrue % 2) = 1 then 
        fplValue.ReprId <- "true"
    elif counterNonBoolean=0 && (oddCounterOfTrue % 2) <> 1 then 
        fplValue.ReprId <- "false"
    else
        fplValue.ReprId <- "undetermined"

let evaluateImplication (fplValue:FplValue) = 
    let arg1Opt = fplValue.ValueList[0].GetValue
    let arg2Opt = fplValue.ValueList[1].GetValue
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
    let arg1Opt = fplValue.ValueList[0].GetValue
    let arg2Opt = fplValue.ValueList[1].GetValue
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
