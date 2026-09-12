(* Copyright (c) 2021+ bookofproofs See LICENSE in the project root for license terms. *)

/// <summary>
/// Evaluators for AST nodes that denote FPL type constructs (keywords, templates, array types,
/// inherited types and compound type forms).
/// </summary>
/// <remarks>
/// This module updates the interpreter evaluation context (top of the eval stack) according to
/// the parsed type construct. It sets type identifiers on variables/mappings, creates intrinsic
/// template nodes, resolves inherited base classes and emits diagnostics through the project's
/// emitter helpers when types or inheritance relations are invalid or ambiguous.
/// </remarks>
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

/// <summary>
/// Assign a keyword-derived type to the top evaluation node or to specific container nodes.
/// </summary>
/// <param name="keywordType">The type literal or identifier to assign (for example <c>LiteralInd</c>, template id, etc.).</param>
/// <param name="pos1">Start position of the type construct in the source.</param>
/// <param name="pos2">End position of the type construct in the source.</param>
/// <returns>Unit. Side effects: updates the top-of-stack node's <c>TypeId</c> or calls container-specific setters.</returns>
/// <remarks>
/// If the top-of-stack node is an <c>FplVariableArray</c> or an <c>FplMapping</c>, the function uses their
/// specific <c>SetType</c> logic which accepts optional candidate resolution and position parameters.
/// For other node kinds it performs a simple assignment to <c>TypeId</c>.
/// </remarks>
let private setKeywordType keywordType pos1 pos2 = 
    let fv = heap.Eval.PeekEvalStack()
    match fv with
    | :? FplVariableArray as arr -> arr.SetType keywordType None pos1 pos2 
    | :? FplMapping as map -> map.SetType keywordType None pos1 pos2
    | _ ->  fv.TypeId <- keywordType

/// <summary>
/// Evaluate a type-construct AST node and apply its semantics to the interpreter evaluation context.
/// </summary>
/// <param name="ast">An AST node representing a type construct (index, predicate, object, template, array, inherited types, compound forms, ...).</param>
/// <returns>Unit. Side effects include updating the top-of-stack node's type, creating and attaching template or base nodes,
/// invoking the main evaluator for nested type ASTs, resolving candidate types and emitting diagnostics.</returns>
/// <remarks>
/// Supported AST cases include:
/// - Keyword types: index, functional term, object, predicate.
/// - Template types: creates a <c>FplIntrinsicTpl</c> node and attaches it when appropriate.
/// - Array types: marks mapping nodes as arrays and evaluates element/index type ASTs.
/// - Inherited types and lists: resolves base classes, updates inheritance relationships, collects variables/properties from bases,
///   and emits diagnostics for missing bases, self-inheritance, or incompatible base types.
/// - Compound predicate/functional-term types and other nested constructs delegate evaluation to <c>evalRef.Value</c>.
/// </remarks>
/// <exception cref="System.Exception">
/// Thrown via <c>failwith</c> when the supplied <paramref name="ast"/> is not recognized as a supported type construct.
/// </exception>
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
                        let nodeName = beingCreatedNode.Type SignatureType.Mixed
                        let baseName = foundBase.Type SignatureType.Mixed
                        if nodeType <> baseType then 
                            baseNode.ErrorOccurred <- emitID007Diagnostics beingCreatedNode.Name nodeName foundBase.Name baseName pos1 pos2
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
                        let nodeName = beingCreatedNode.Type SignatureType.Mixed
                        let baseName = foundBase.Type SignatureType.Mixed
                        baseNode.ErrorOccurred <- emitID007Diagnostics beingCreatedNode.Name nodeName foundBase.Name baseName pos1 pos2
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
