/// This module creates the SymbolTable
/// and deals with lexical / leaf tokens of the AST

(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)
module FplInterpreter.ST.Lexical

open System
open FplPrimitives
open FplGrammarTypes
open FplInterpreterDiagnosticsEmitter
open FplInterpreterBasicTypes
open FplInterpreterGlobals
open FplInterpreterST
open FplInterpreterSTEmbedding
open FplInterpreterIntrinsicTypes
open FplInterpreterVariables
open FplInterpreterExtensions
open FplInterpreterReferences
open FplInterpreterDefinitions
open FplInterpreterReferencesSelfParent
open FplInterpreterDefinitionProperties
open FplInterpreterProofs
open FplInterpreterForStmt

// Lexical: primitive tokens and simple identifiers (leaf nodes used across the interpreter).
// This module keeps the handling of smallest AST nodes focused and isolated so higher-level
// modules can assume these leaves are normalized and pushed onto the eval stack correctly.
let eval (st: SymbolTable) (ast: Ast) =
    match ast with
    | Ast.Alias((pos1, pos2), s) -> ()
    | Ast.Dot((pos1, pos2),()) -> ()
    | Ast.Star((pos1, pos2),()) -> ()

    | Ast.Digits s -> 
        let fv = variableStack.PeekEvalStack()
        fv.FplId <- s
        fv.TypeId <- s

    | Ast.DollarDigits((pos1, pos2), s) -> 
        let fv = variableStack.PeekEvalStack()
        let sid = $"${s.ToString()}"
        match fv with 
        | :? FplReference when fv.FplId = String.Empty && not variableStack.InReferenceToProofOrCorollary ->
            let value = new FplIntrinsicInd((pos1, pos2), fv)
            value.FplId <- sid
            variableStack.PushEvalStack(value)
            variableStack.PopEvalStack()
        | _  ->
            fv.FplId <- fv.FplId + sid
            match fv.TypeId with 
            | "" when not variableStack.InReferenceToProofOrCorollary -> fv.TypeId <- LiteralInd
            | LiteralPred -> ()
            | _ -> fv.TypeId <- fv.TypeId + sid

    | Ast.ExtensionRegex s -> 
        let fv = variableStack.PeekEvalStack()
        fv.TypeId <- s

    | Ast.ExtensionName((pos1, pos2), s) ->
        let fv = variableStack.PeekEvalStack()
        let extensionName = s
        match fv with 
        | :? FplExtension ->
            fv.FplId <- extensionName
            fv.TypeId <- extensionName
        | _ -> ()

    | Ast.LanguageCode((pos1, pos2), s) -> 
        let fv = variableStack.PeekEvalStack()
        fv.FplId <- s
        fv.TypeId <- s
        fv.StartPos <- pos1
        fv.EndPos <- pos2
    | Ast.LocalizationString((pos1, pos2), s) -> 
        let fv = variableStack.PeekEvalStack()
        fv.FplId <- s
        fv.TypeId <- s

    | Ast.PascalCaseId ((pos1, pos2), pascalCaseId) -> 
        let fv = variableStack.PeekEvalStack()
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

    | Ast.PredicateIdentifier((pos1, pos2), identifier) ->
        let fv = variableStack.PeekEvalStack()
        let searchIdentifier = 
            if variableStack.InReferenceToProofOrCorollary then 
                $"{identifier}{fv.FplId}"
            else
                identifier
            
        let candidatesFromTheory = findCandidatesByName fv searchIdentifier false variableStack.InReferenceToProofOrCorollary
        let candidatesLocal = findPropertyCandidatesByNameInBlock fv searchIdentifier
        let candidatesOfMapping = findCandidateOfExtensionMapping fv searchIdentifier
        let candidates, candidatesNames =  filterCandidates (candidatesFromTheory @ candidatesLocal @ candidatesOfMapping) searchIdentifier true
        let correctIds (fv1:FplGenericNode) = 
            match fv with 
            | :? FplBase 
            | :? FplBaseConstructorCall 
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
        // Not a leaf lexical node — caller should route to another focused module.
        failwith "FplInterpreter.ST.Lexical:eval: unexpected AST node"
