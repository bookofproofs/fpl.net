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

let evaluateImplication (fplValue:FplValue) = 
    let arg1 = fplValue.ValueList[0]
    let arg2 = fplValue.ValueList[1]
    match (arg1.FplRepresentation, arg2.FplRepresentation) with
    | (FplRepresentation.PredRepr FplPredicate.True, FplRepresentation.PredRepr FplPredicate.False) -> 
        fplValue.FplRepresentation <- FplRepresentation.PredRepr FplPredicate.False
    | (FplRepresentation.PredRepr _, FplRepresentation.PredRepr _) -> 
        fplValue.FplRepresentation <- FplRepresentation.PredRepr FplPredicate.True
    | _ -> fplValue.FplRepresentation <- FplRepresentation.PredRepr FplPredicate.Undetermined
    