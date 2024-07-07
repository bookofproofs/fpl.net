module FplInterpreterPredicateEvaluator
open System.Collections.Generic
open FParsec
open ErrDiagnostics
open FplDelegates
open FplParser
open FplInterpreterTypes

let aggregateConjunction (fplValue:FplValue) = 
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
    