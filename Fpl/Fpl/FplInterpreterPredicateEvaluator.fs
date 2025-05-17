module FplInterpreterPredicateEvaluator
open FplInterpreterTypes

let evaluateNegation (fplValue:FplValue) = 
    let argOpt = fplValue.ValueList[0].GetValue
    match argOpt with
    | Some arg ->
        match arg.ReprId with
        | "pred{false}" -> 
            fplValue.ReprId <- "pred{true}"
        | "pred{true}" -> 
            fplValue.ReprId <- "pred{false}"
        | _ -> fplValue.ReprId <- "pred{undetermined}"
    | _ -> fplValue.ReprId <- "pred{undetermined}"
    
let evaluateConjunction (fplValue:FplValue) =
    let arg1Opt = fplValue.ValueList[0].GetValue
    let arg2Opt = fplValue.ValueList[1].GetValue
    match (arg1Opt,arg2Opt) with
    | (Some arg1, Some arg2) -> 
        match (arg1.ReprId, arg2.ReprId) with
        | ("pred{true}", "pred{false}") 
        | ("pred{false}", "pred{true}") 
        | ("pred{false}", "pred{false}") -> fplValue.ReprId <- "pred{false}" 
        | ("pred{true}", "pred{true}") -> fplValue.ReprId <- "pred{true}"
        | _ -> fplValue.ReprId <- "pred{undetermined}"
    | _ ->
        fplValue.ReprId <- "pred{undetermined}"

let evaluateDisjunction (fplValue:FplValue) = 
    let arg1Opt = fplValue.ValueList[0].GetValue
    let arg2Opt = fplValue.ValueList[1].GetValue
    match (arg1Opt,arg2Opt) with
    | (Some arg1, Some arg2) -> 
        match (arg1.ReprId, arg2.ReprId) with
        | ("pred{true}", "pred{false}") 
        | ("pred{false}", "pred{true}") 
        | ("pred{true}", "pred{true}") -> fplValue.ReprId <- "pred{true}"
        | ("pred{false}", "pred{false}") -> fplValue.ReprId <- "pred{false}" 
        | _ -> fplValue.ReprId <- "pred{undetermined}"
    | _ ->
        fplValue.ReprId <- "pred{undetermined}"

let evaluateExclusiveOr (fplValue:FplValue) = 
    let arg1Opt = fplValue.ValueList[0].GetValue
    let arg2Opt = fplValue.ValueList[1].GetValue
    match (arg1Opt,arg2Opt) with
    | (Some arg1, Some arg2) -> 
        match (arg1.ReprId, arg2.ReprId) with
        | ("pred{true}", "pred{false}") 
        | ("pred{false}", "pred{true}") -> fplValue.ReprId <- "pred{true}" 
        | ("pred{true}", "pred{true}") 
        | ("pred{false}", "pred{false}") -> fplValue.ReprId <- "pred{false}" 
        | _ -> fplValue.ReprId <- "pred{undetermined}"
    | _ ->
        fplValue.ReprId <- "pred{undetermined}"

let evaluateImplication (fplValue:FplValue) = 
    let arg1Opt = fplValue.ValueList[0].GetValue
    let arg2Opt = fplValue.ValueList[1].GetValue
    match (arg1Opt,arg2Opt) with
    | (Some arg1, Some arg2) -> 
        match (arg1.ReprId, arg2.ReprId) with
        | ("pred{true}", "pred{false}") -> fplValue.ReprId <- "pred{false}"
        | ("pred{false}", "pred{true}") 
        | ("pred{false}", "pred{false}") 
        | ("pred{true}", "pred{true}") -> fplValue.ReprId <- "pred{true}"
        | _ -> fplValue.ReprId <- "pred{undetermined}"
    | _ ->
        fplValue.ReprId <- "pred{undetermined}"

let evaluateEquivalence (fplValue:FplValue) = 
    let arg1Opt = fplValue.ValueList[0].GetValue
    let arg2Opt = fplValue.ValueList[1].GetValue
    match (arg1Opt,arg2Opt) with
    | (Some arg1, Some arg2) -> 
        match (arg1.ReprId, arg2.ReprId) with
        | ("pred{true}", "pred{true}") 
        | ("pred{false}", "pred{false}") -> fplValue.ReprId <- "pred{true}"
        | ("pred{false}", "pred{true}") 
        | ("pred{true}", "pred{false}") -> fplValue.ReprId <- "pred{false}"
        | _ -> fplValue.ReprId <- "pred{undetermined}"
    | _ ->
        fplValue.ReprId <- "pred{undetermined}"
    
let evaluateIsOperator (fv:FplValue) (operand:FplValue) (typeOfOperand:FplValue) = 
    match mpwa [operand] [typeOfOperand] with
    | Some errMsg -> fv.ReprId <- "pred{false}"
    | None -> fv.ReprId <- "pred{true}"
