module FplInterpreterPredicateEvaluator
open FplInterpreterDiagnosticsEmitter
open FplInterpreterTypes
open ErrDiagnostics

let evaluateNegation (fplValue:FplValue) = 
    let arg = fplValue.ValueList[0]
    match arg.FplRepresentation with
    | FplRepresentation.PredRepr FplPredicate.False -> 
        fplValue.FplRepresentation <- FplRepresentation.PredRepr FplPredicate.True
    | FplRepresentation.PredRepr FplPredicate.True -> 
        fplValue.FplRepresentation <- FplRepresentation.PredRepr FplPredicate.False
    | _ -> fplValue.FplRepresentation <- FplRepresentation.PredRepr FplPredicate.Undetermined
    
let evaluateConjunction (fplValue:FplValue) = 
    let vlist = fplValue.ValueList |> Seq.toList 
    let rec aggr (fv: FplValue list) =
        match fv with
        | x::xs -> 
            match x.FplRepresentation with
            | FplRepresentation.PredRepr FplPredicate.False -> Some false
            | FplRepresentation.PredRepr FplPredicate.True -> aggr xs 
            | _ -> None
        | [] -> Some true
    match aggr vlist with 
    | Some true -> fplValue.FplRepresentation <- FplRepresentation.PredRepr FplPredicate.True
    | Some false -> fplValue.FplRepresentation <- FplRepresentation.PredRepr FplPredicate.False
    | _ -> fplValue.FplRepresentation <- FplRepresentation.PredRepr FplPredicate.Undetermined

let evaluateDisjunction (fplValue:FplValue) = 
    let vlist = fplValue.ValueList |> Seq.toList 
    let rec aggr (fv: FplValue list) =
        match fv with
        | x::xs -> 
            match x.FplRepresentation with
            | FplRepresentation.PredRepr FplPredicate.False -> aggr xs 
            | FplRepresentation.PredRepr FplPredicate.True -> Some true
            | _ -> None
        | [] -> Some false
    match aggr vlist with 
    | Some true -> fplValue.FplRepresentation <- FplRepresentation.PredRepr FplPredicate.True
    | Some false -> fplValue.FplRepresentation <- FplRepresentation.PredRepr FplPredicate.False
    | _ -> fplValue.FplRepresentation <- FplRepresentation.PredRepr FplPredicate.Undetermined

let evaluateExclusiveOr (fplValue:FplValue) = 
    let vlist = fplValue.ValueList |> Seq.toList 
    let rec aggr (fv: FplValue list) oddCounterOfTrue counterNonBoolean =
        match fv with
        | x::xs -> 
            match x.FplRepresentation with
            | FplRepresentation.PredRepr FplPredicate.False -> aggr xs oddCounterOfTrue counterNonBoolean
            | FplRepresentation.PredRepr FplPredicate.True -> aggr xs (oddCounterOfTrue + 1) counterNonBoolean
            | _ -> (oddCounterOfTrue, counterNonBoolean + 1) 
        | [] -> (oddCounterOfTrue, counterNonBoolean)
    let (oddCounterOfTrue, counterNonBoolean) = aggr vlist 0 0 
    if counterNonBoolean=0 && (oddCounterOfTrue % 2) = 1 then 
        fplValue.FplRepresentation <- FplRepresentation.PredRepr FplPredicate.True
    elif counterNonBoolean=0 && (oddCounterOfTrue % 2) <> 1 then 
        fplValue.FplRepresentation <- FplRepresentation.PredRepr FplPredicate.False
    else
        fplValue.FplRepresentation <- FplRepresentation.PredRepr FplPredicate.Undetermined

let evaluateImplication (fplValue:FplValue) = 
    let arg1 = fplValue.ValueList[0]
    let arg2 = fplValue.ValueList[1]
    match (arg1.FplRepresentation, arg2.FplRepresentation) with
    | (FplRepresentation.PredRepr FplPredicate.True, FplRepresentation.PredRepr FplPredicate.False) -> 
        fplValue.FplRepresentation <- FplRepresentation.PredRepr FplPredicate.False
    | (FplRepresentation.PredRepr _, FplRepresentation.PredRepr _) -> 
        fplValue.FplRepresentation <- FplRepresentation.PredRepr FplPredicate.True
    | _ -> fplValue.FplRepresentation <- FplRepresentation.PredRepr FplPredicate.Undetermined

let evaluateEquivalence (fplValue:FplValue) = 
    let arg1 = fplValue.ValueList[0]
    let arg2 = fplValue.ValueList[1]
    match (arg1.FplRepresentation, arg2.FplRepresentation) with
    | (FplRepresentation.PredRepr FplPredicate.True, FplRepresentation.PredRepr FplPredicate.True) 
    | (FplRepresentation.PredRepr FplPredicate.False, FplRepresentation.PredRepr FplPredicate.False) -> 
        fplValue.FplRepresentation <- FplRepresentation.PredRepr FplPredicate.True
    | (FplRepresentation.PredRepr _, FplRepresentation.PredRepr _) -> 
        fplValue.FplRepresentation <- FplRepresentation.PredRepr FplPredicate.False
    | _ -> fplValue.FplRepresentation <- FplRepresentation.PredRepr FplPredicate.Undetermined
    