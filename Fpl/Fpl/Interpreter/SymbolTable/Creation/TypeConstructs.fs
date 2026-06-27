/// This module provides specialized evaluators for the AST nodes related to FPL types and FPL type related constructs.


(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)

module Fpl.Interpreter.SymbolTable.Creation.TypeConstructs
open Fpl.Primitives
open Fpl.Parser.Types
open Fpl.Errors.Emitter
open Fpl.Interpreter.BasicTypes
open Fpl.Interpreter.SymbolTable.Storage.Heap
open Fpl.Interpreter.SymbolTable.Storage.Util
open Fpl.Interpreter.SymbolTable.Types2.Intrinsic
open Fpl.Interpreter.SymbolTable.Types2.Variables
open Fpl.Interpreter.SymbolTable.Types2.Definitions
open Fpl.Interpreter.SymbolTable.TypeMatching
open Fpl.Interpreter.SymbolTable.Creation.Forward

let private setKeywordType keywordType pos1 pos2 = 
    let fv = heap.Eval.PeekEvalStack()
    match fv with
    | :? FplVariableArray as arr -> arr.SetType keywordType None pos1 pos2 
    | :? FplMapping as map -> map.SetType keywordType None pos1 pos2
    | _ ->  fv.TypeId <- keywordType

let evalTypeConstructs ast =
    match ast with
    | Ast.IndexType((pos1, pos2),()) -> 
        setKeywordType LiteralInd pos1 pos2
    | Ast.FunctionalTermType((pos1, pos2),()) -> 
        setKeywordType LiteralFunc pos1 pos2
    | Ast.ObjectType((pos1, pos2),()) -> 
        setKeywordType LiteralObj pos1 pos2
    | Ast.PredicateType((pos1, pos2),()) -> 
        setKeywordType LiteralPred pos1 pos2
    | Ast.TemplateType((pos1, pos2), s) -> 
        let fv = heap.Eval.PeekEvalStack()
        setKeywordType s pos1 pos2
        let templateNode = new FplIntrinsicTpl(s, (pos1, pos2), fv)
        match fv with 
        | :? FplGenericVariable as var -> 
            // attach template type to declared variable 
            var.RefersTo <- Some templateNode
        | _ -> () // RefersTo's semantics in other FplValues is different, do not interfere with it
        heap.Eval.PushEvalStack(templateNode)
        heap.Eval.PopEvalStack()

    | Ast.ArrayType((pos1, pos2), (mainTypeAst,  indexAllowedTypeListAst)) ->
        let fv = heap.Eval.PeekEvalStack()
        match fv with 
        | :? FplMapping as mapping -> mapping.SetIsArray()
        | _ -> ()
        evalRef.Value mainTypeAst
        indexAllowedTypeListAst |> List.map evalRef.Value |> ignore
    | Ast.SimpleVariableType((pos1, pos2), simpleVariableTypeAst) ->
        evalRef.Value simpleVariableTypeAst
    | Ast.IndexAllowedType((pos1, pos2), indexAllowedTypeAst) ->
        evalRef.Value indexAllowedTypeAst
    | Ast.InheritedType ((pos1, pos2), identifier) -> 
        let fv = heap.Eval.PeekEvalStack()
        fv.FplId <- identifier
        fv.TypeId <- identifier
        let candidates = findCandidatesByName identifier false true
        if candidates.Length = 0 then 
            fv.ErrorOccurred <- emitID010Diagnostics identifier pos1 pos2
    | Ast.InheritedTypeList inheritedTypeAsts -> 
        let beingCreatedNode = heap.Eval.PeekEvalStack()
        let addVariablesAndPropertiesOfBaseNode (bNode:FplGenericNode) = 
            match box beingCreatedNode with
            | :? FplGenericInheriting as inheritingNode -> 
                inheritingNode.InheritVariables bNode
                inheritingNode.InheritProperties bNode
            | _ -> ()

        inheritedTypeAsts
        |> List.iter (fun inheritedType ->
            match inheritedType with
            | Ast.InheritedType((pos1, pos2), _) ->
                // retrieve the name of the class and the class (if it exists)
                let baseNode = new FplBase((pos1, pos2), beingCreatedNode)
                heap.Eval.PushEvalStack(baseNode)            
                evalRef.Value inheritedType
                heap.Eval.PopEvalStack() |> ignore
                let candidates = findCandidatesByName baseNode.FplId false true
                if candidates.Length > 0 then 
                    let foundBase = candidates.Head
                    match beingCreatedNode, foundBase with
                    | :? FplPredicate, :? FplPredicate 
                    | :? FplFunctionalTerm, :? FplFunctionalTerm ->
                        let nodeType = beingCreatedNode.Type SignatureType.Type
                        let baseType = foundBase.Type SignatureType.Type
                        if nodeType <> baseType then 
                            baseNode.ErrorOccurred <- emitID007Diagnostics beingCreatedNode.Name nodeType foundBase.Name baseType pos1 pos2
                        else 
                            baseNode.RefersTo <- Some foundBase // add found base class to base
                            addVariablesAndPropertiesOfBaseNode foundBase
                    | :? FplClass, :? FplClass -> 
                        baseNode.RefersTo <- Some foundBase // add found base class to base
                        addVariablesAndPropertiesOfBaseNode foundBase
                    | :? FplPredicate, _
                    | :? FplFunctionalTerm, _
                    | :? FplClass, _ ->
                        let nodeType = beingCreatedNode.Type SignatureType.Type
                        let baseType = foundBase.Type SignatureType.Type
                        baseNode.ErrorOccurred <- emitID007Diagnostics beingCreatedNode.Name nodeType foundBase.Name baseType pos1 pos2
                    | _ -> () // does not occur, since syntax of inherited base is not supported from non-classes, non-functional terms, and non-predicates
                else
                    baseNode.ErrorOccurred <- emitID010Diagnostics baseNode.FplId pos1 pos2
                if baseNode.FplId = beingCreatedNode.FplId then 
                    baseNode.ErrorOccurred <- emitID009Diagnostics baseNode.FplId pos1 pos2
            | _ -> ()
        )
        let classInheritanceChains = findInheritanceChains beingCreatedNode 
        classInheritanceChains
        |> Seq.filter (fun kvp -> kvp.Value <> "ok")
        |> Seq.iter (fun kvp -> 
            beingCreatedNode.ErrorOccurred <- emitID011Diagnostics kvp.Key kvp.Value beingCreatedNode.StartPos beingCreatedNode.EndPos
        )
    | Ast.CompoundPredicateType((pos1, pos2), (ast1, optAst)) ->
        evalRef.Value ast1
        optAst |> Option.map evalRef.Value |> ignore
    | Ast.CompoundFunctionalTermType((pos1, pos2), (ast1, astTupleOption)) ->
        evalRef.Value ast1
        match astTupleOption with 
        | Some (ast2, _) -> evalRef.Value ast2 |> ignore
        | _ -> ()
        match astTupleOption with 
        | Some (_, ast3) -> evalRef.Value ast3 |> ignore
        | _ -> ()
    | _ ->
        failwith (sprintf "{%O} is not a type construct" ast) 
