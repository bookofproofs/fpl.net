/// This module provides specialized evaluators for the AST nodes related to FPL lexical and leaf tokens.


(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)

module Fpl.Interpreter.SymbolTable.Creation.LeafTokens
open System
open Fpl.Primitives
open Fpl.Parser.Types
open Fpl.Errors.Emitter
open Fpl.Interpreter.BasicTypes
open Fpl.Interpreter.Helpers.Basic
open Fpl.Interpreter.SymbolTable.Storage.Heap
open Fpl.Interpreter.SymbolTable.Storage.Util
open Fpl.Interpreter.SymbolTable.Types2.Intrinsic
open Fpl.Interpreter.SymbolTable.Types2.Variables
open Fpl.Interpreter.SymbolTable.Types2.References
open Fpl.Interpreter.SymbolTable.Types3.DefinitionProperties
open Fpl.Interpreter.SymbolTable.Types3.Quantors
open Fpl.Interpreter.SymbolTable.Types3.Extensions
open Fpl.Interpreter.SymbolTable.Types3.ForStmt
open Fpl.Interpreter.SymbolTable.ExpressionMatching

let evalLeafTokens ast =
    match ast with
    | Ast.Alias((_, _), _) -> ()
    | Ast.Dot() -> ()
    | Ast.Star((_, _),()) -> ()
    | Ast.Digits s -> 
        let fv = heap.Eval.PeekEvalStack()
        fv.FplId <- s
        fv.TypeId <- s
    | Ast.Exists1() ->
        let fv = heap.Eval.PeekEvalStack()
        fv.FplId <- fv.FplId + "$1"
    | Ast.DollarDigits((pos1, pos2), s) -> 
        let fv = heap.Eval.PeekEvalStack()
        let sid = $"${s.ToString()}"
        match fv with 
        | :? FplReference when fv.FplId = String.Empty && not heap.Helper.InReferenceToProofOrCorollary ->
            let value = new FplIntrinsicInd((pos1, pos2), fv)
            value.FplId <- sid
            heap.Eval.PushEvalStack(value)
            heap.Eval.PopEvalStack()
        | :? FplQuantorExistsN ->
            match (int)s with
            | 0 ->
                fv.ErrorOccurred <- emitSY011diagnostics pos1 pos2
            | 1 ->
                fv.ErrorOccurred <- emitSY012diagnostics pos1 pos2
            | _ -> ()
            fv.FplId <- fv.FplId + sid
        | _  ->
            fv.FplId <- fv.FplId + sid
            match fv.TypeId with 
            | "" when not heap.Helper.InReferenceToProofOrCorollary -> fv.TypeId <- LiteralInd
            | LiteralPred -> ()
            | _ -> fv.TypeId <- fv.TypeId + sid

    | Ast.ExtensionRegex s -> 
        let fv = heap.Eval.PeekEvalStack()
        fv.TypeId <- s
    | Ast.ExtensionName((pos1, pos2), extensionName) ->
        let fv = heap.Eval.PeekEvalStack()
        match fv with 
        | :? FplExtension ->
            fv.FplId <- extensionName
            fv.TypeId <- extensionName
        | _ -> ()
    | Ast.LanguageCode((pos1, pos2), s) -> 
        let fv = heap.Eval.PeekEvalStack()
        fv.FplId <- s
        fv.TypeId <- s
        fv.StartPos <- pos1
        fv.EndPos <- pos2
    | Ast.LocalizationString((pos1, pos2), s) -> 
        let fv = heap.Eval.PeekEvalStack()
        fv.FplId <- s
        fv.TypeId <- s

    | Ast.PascalCaseId ((pos1, pos2), pascalCaseId) ->
        let fv = heap.Eval.PeekEvalStack()
        match fv.Name with
        | LiteralAxL
        | LiteralThmL
        | LiteralPropL
        | LiteralLemL
        | LiteralConjL
        | LiteralCorL
        | PrimFunctionalTermL
        | PrimPredicateL
        | LiteralPrfL
        | PrimMandatoryFunctionalTermL
        | PrimMandatoryPredicateL
        | PrimPredicateL
        | PrimFunctionalTermL
        | PrimRuleOfInference -> 
            fv.FplId <- pascalCaseId
        | LiteralCtorL ->
            fv.FplId <- pascalCaseId
            fv.TypeId <- pascalCaseId
            fv.ErrorOccurred <- emitID008Diagnostics pascalCaseId fv.Parent.Value.FplId pos1 pos2
        | PrimClassL ->
            fv.FplId <- pascalCaseId
            fv.TypeId <- pascalCaseId
        | _ -> ()

    | Ast.BaseClassName((pos1, pos2), identifier) ->
        let fv = heap.Eval.PeekEvalStack()
        fv.FplId <- identifier
        fv.TypeId <- identifier
        let candidates = findCandidatesByName identifier false true
        if candidates.Length = 0 then 
            fv.ErrorOccurred <- emitID010Diagnostics identifier pos1 pos2
    | Ast.PredicateIdentifier((pos1, pos2), identifier) ->
        let fv = heap.Eval.PeekEvalStack()
        let searchIdentifier = 
            if heap.Helper.InReferenceToProofOrCorollary then 
                $"{identifier}{fv.FplId}"
            else
                identifier
            
        let candidatesFromTheory = findCandidatesByName searchIdentifier false heap.Helper.InReferenceToProofOrCorollary
        let candidatesLocal = findPropertyCandidatesByNameInBlock fv searchIdentifier
        let candidatesOfMapping = findCandidateOfExtensionMapping fv searchIdentifier
        let candidates, candidatesNames =  filterCandidates (candidatesFromTheory @ candidatesLocal @ candidatesOfMapping) searchIdentifier true
        let correctIds (fv1:FplGenericNode) = 
            match fv with 
            | :? FplForInStmtDomain -> 
                fv1.FplId <- searchIdentifier
                fv1.TypeId <- searchIdentifier
            | :? FplReference -> 
                fv1.FplId <- searchIdentifier
                fv1.TypeId <- searchIdentifier
            | :? FplGenericJustificationItem as fvJi -> 
                fvJi.FplId <- searchIdentifier
            | _ -> ()

        match candidates.Length with
        | 0 -> 
            match fv.Parent with
            | Some (:? FplReference as parent) when parent.DottedChild.IsSome && Object.ReferenceEquals(fv, parent.DottedChild.Value) ->
                // do not emit ID010 diagnostics, if fv is a dotted child, whose identifier we are still being evaluated
                // only with this identifier, it will be possible in AST.PredicateWithOptSpecification to search for correct candidates 
                () 
            | _ -> 
                // otherwise, issue ID010 diagnostics
                fv.ErrorOccurred <- emitID010Diagnostics identifier pos1 pos2
            match fv with 
            | :? FplVariableArray as arr -> arr.SetType identifier None pos1 pos2
            | :? FplMapping as map -> map.SetType identifier None pos1 pos2
            | :? FplVariable ->
                let fvWithValue = fv :?> FplGenericHasValue
                fvWithValue.TypeId <- identifier
                fvWithValue.SetDefaultValue()
            | _ -> correctIds fv 
        | 1 ->
            let candidate = candidates.Head
            match fv with 
            | :? FplVariableArray as arr ->  arr.SetType identifier (Some candidate) pos1 pos2
            | :? FplMapping as map -> 
                let candidate = candidates.Head
                // mappings can point to classes 
                map.SetType identifier (Some candidate) pos1 pos2
            | :? FplVariable -> 
                fv.TypeId <- identifier
                fv.RefersTo <- Some candidate
            | _ -> correctIds fv
        | _ ->
            match fv with 
            | :? FplMapping 
            | :? FplVariable -> 
                fv.ErrorOccurred <- emitID017Diagnostics identifier candidatesNames pos1 pos2
            | _ -> correctIds fv
    | _ ->
        failwith (sprintf "{%O} is not a leaf token" ast) 
