/// This module provides specialized evaluators for the AST nodes related to FPL identifiers and identifier dispatchers.


(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)

module Fpl.Interpreter.SymbolTable.Creation.Identifiers
open System
open Fpl.Primitives
open Fpl.Parser.Types
open Fpl.Errors.Emitter
open Fpl.Interpreter.BasicTypes
open Fpl.Interpreter.Helpers.Checks
open Fpl.Interpreter.Helpers.Basic
open Fpl.Interpreter.SymbolTable.Storage.Heap
open Fpl.Interpreter.SymbolTable.Storage.Util
open Fpl.Interpreter.SymbolTable.Types2.Variables
open Fpl.Interpreter.SymbolTable.Types2.References
open Fpl.Interpreter.SymbolTable.Types3.DefinitionProperties
open Fpl.Interpreter.SymbolTable.Types3.Extensions
open Fpl.Interpreter.SymbolTable.Types3.Delegates
open Fpl.Interpreter.SymbolTable.Types3.ForStmt
open Fpl.Interpreter.SymbolTable.ExpressionMatching
open Fpl.Interpreter.SymbolTable.Types4.Proofs
open Fpl.Interpreter.SymbolTable.Creation.Forward

let evalIdentifiers ast =
    match ast with
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
    | Ast.NamespaceIdentifier((pos1, pos2), asts) ->
        asts |> List.map evalRef.Value |> ignore
    | Ast.ClassIdentifier((pos1, pos2), ast1) ->
        evalRef.Value ast1
        let fv = heap.Eval.PeekEvalStack()
        fv.EndPos <- pos2
    | Ast.AliasedNamespaceIdentifier((pos1, pos2), (ast1, optAst)) ->
        evalRef.Value ast1
        optAst |> Option.map evalRef.Value |> ignore
    | Ast.ArgumentIdentifier((pos1, pos2), argumentId) -> 
        let testNode = heap.Eval.PeekEvalStack()
        match testNode with
        | :? FplJustification as justification ->
            match justification.Parent with
            | Some argument -> argument.FplId <- argumentId.Substring(0,argumentId.Length-1) // argument id without the "." at the end
            | _ -> ()
        | :? FplArgument as argument ->
            argument.FplId <- argumentId.Substring(0,argumentId.Length-1)
        | _ -> ()
    | Ast.RefArgumentIdentifier((pos1, pos2), argumentId) -> 
        let fv = heap.Eval.PeekEvalStack()
        match fv.Name with 
        | PrimJIByProofArgument -> fv.FplId <- $"{fv.FplId}:{argumentId}"
        | PrimArgInfRevoke -> fv.FplId <- argumentId
        | PrimJustificationL -> 
            let fvAi = new FplJustificationItemByRefArgument((pos1, pos2), fv)
            fvAi.FplId <- argumentId
            let just = fvAi.ParentJustification
            let arg = just.ParentArgument
            let proof = arg.ParentProof
            if not (proof.HasArgument argumentId) then
                fvAi.ErrorOccurred <- emitPR005Diagnostics argumentId pos1 pos2
            else
                fvAi.RefersTo <- Some proof.Scope[argumentId]
            heap.Eval.PushEvalStack(fvAi)
            heap.Eval.PopEvalStack()
        | _ -> ()
    | Ast.DelegateName((pos1, pos2), delegateId) ->
        let fv = heap.Eval.PeekEvalStack()
        match delegateId with 
        | PrimDelegateEqualL -> 
            let deleg = new FplEquality(delegateId, (pos1, pos2), fv)
            heap.Eval.PushEvalStack(deleg)
        | PrimDelegateDecrementL -> 
            let deleg = new FplDecrement(delegateId, (pos1, pos2), fv)
            heap.Eval.PushEvalStack(deleg)
        | _ -> 
            let deleg = new FplReference((pos1, pos2), fv)
            deleg.FplId <- delegateId
            deleg.TypeId <- delegateId
            heap.Eval.PushEvalStack(deleg)
            deleg.ErrorOccurred <- emitID013Diagnostics $"Unknown delegate `{delegateId}`" pos1 pos2
    | Ast.ExtensionName((pos1, pos2), extensionName) ->
        let fv = heap.Eval.PeekEvalStack()
        match fv with 
        | :? FplExtension ->
            fv.FplId <- extensionName
            fv.TypeId <- extensionName
        | _ -> ()
    | Ast.ReferencingIdentifier((pos1, pos2), (predicateIdentifierAst, dollarDigitListAsts)) ->
        dollarDigitListAsts |> List.map evalRef.Value |> ignore
        evalRef.Value predicateIdentifierAst
        let fv = heap.Eval.PeekEvalStack()
        match fv with 
        | :? FplReference ->
            let candidates = findCandidatesByName fv.FplId false true
            if candidates.Length > 0 then 
                let candidate = candidates.Head
                fv.RefersTo <- Some candidate
                match fv.UltimateBlockNode with
                | Some block ->
                    fv.ErrorOccurred <- checkID025Diagnostics (qualifiedName candidate false) block.Name fv.StartPos fv.EndPos
                | _ -> ()
        | _ -> ()
    | _ ->
        failwith (sprintf "{%O} is not an identifier or identifier dispatcher" ast) 
