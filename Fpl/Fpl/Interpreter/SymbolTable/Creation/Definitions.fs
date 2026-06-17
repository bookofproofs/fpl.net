/// This module provides specialized evaluators for the AST nodes related to FPL mathematical definitions and related blocks.


(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)

module Fpl.Interpreter.SymbolTable.Creation.Definitions
open Fpl.Primitives
open Fpl.Parser.Types
open Fpl.Errors.Emitter
open Fpl.Interpreter.SymbolTable.Storage.Heap
open Fpl.Interpreter.SymbolTable.Types2.Variables
open Fpl.Interpreter.SymbolTable.Types2.Definitions
open Fpl.Interpreter.SymbolTable.Types3.SelfParent
open Fpl.Interpreter.SymbolTable.Types3.DefinitionProperties
open Fpl.Interpreter.SymbolTable.Types3.Extensions
open Fpl.Interpreter.SymbolTable.Creation.Forward


let evalDefinitions ast =
    match ast with
    /// Definitions of classes
    | Ast.DefinitionClass((pos1, pos2),(((classSignatureAst, optInheritedClassTypeListAst), optUserDefinedObjSymAst), classBlockAst)) ->
        let parent = heap.Eval.PeekEvalStack()
        let fv = new FplClass((pos1, pos2), parent, heap.Helper.GetNextAvailableFplBlockRunOrder)
        heap.Eval.PushEvalStack(fv)
        evalRef.Value classSignatureAst
        optInheritedClassTypeListAst |> Option.map evalRef.Value |> Option.defaultValue ()
        optUserDefinedObjSymAst |> Option.map evalRef.Value |> Option.defaultValue ()
        evalRef.Value classBlockAst
        heap.Eval.PopEvalStack()
    | Ast.ClassSignature((pos1, pos2), simpleSignatureAst) ->
        heap.Helper.InSignatureEvaluation <- true
        evalRef.Value simpleSignatureAst
        setSignaturePositions pos1 pos2
        heap.Helper.InSignatureEvaluation <- false
    | Ast.ClassDefinitionBlock((pos1, pos2), optDefBlock) ->
        let classBlock = heap.Eval.PeekEvalStack()
        let cl = classBlock :?> FplClass
        match optDefBlock with 
        | Some (classContentAst, optPropertyListAsts) ->
            evalRef.Value classContentAst
            optPropertyListAsts |> Option.map (List.map evalRef.Value >> ignore) |> Option.defaultValue ()
            let properties = cl.GetProperties()
            let constructors = cl.GetConstructors()
            let classContent =  cl.ArgList |> Seq.filter (fun node -> node.Name <> LiteralBase) |> Seq.toList
            if properties.IsEmpty && classContent.Length = 0 && constructors.IsEmpty then
                classBlock.ErrorOccurred <- emitST001diagnostics classBlock.Name pos1 pos2
        | None -> 
            cl.IsIntrinsic <- true
            cl.AddDefaultConstructor()
    | Ast.DefClassCompleteContent(varDeclBlock, constructorListAsts) ->
        evalRef.Value varDeclBlock 
        constructorListAsts |> List.map evalRef.Value |> ignore
    | Ast.Constructor((pos1, pos2), (signatureAst, constructorBlockAst)) ->
        let parent = heap.Eval.PeekEvalStack()
        let fv = new FplConstructor((pos1, pos2), parent)
        heap.Eval.PushEvalStack(fv)
        evalRef.Value signatureAst
        evalRef.Value constructorBlockAst
        heap.Eval.PopEvalStack()
    | Ast.ConstructorSignature((pos1, pos2), (simpleSignatureAst, paramTupleAst)) ->
        heap.Helper.InSignatureEvaluation <- true
        evalRef.Value simpleSignatureAst
        evalRef.Value paramTupleAst
        setSignaturePositions pos1 pos2
        heap.Helper.InSignatureEvaluation <- false
    | Ast.ConstructorBlock varDeclBlock ->
        let parent = heap.Eval.PeekEvalStack()
        // evaluate the construction block 
        evalRef.Value varDeclBlock
        if parent.ArgList.Count = 0 then
            parent.ErrorOccurred <- emitST002diagnostics parent.Name parent.StartPos parent.EndPos
    | Ast.BaseConstructorCall((pos1, pos2), (inheritedClassTypeAst, argumentTupleAst)) ->
        let parent = heap.Eval.PeekEvalStack()
        let fvNew = new FplBaseConstructorCall((pos1, pos2), parent) 
        heap.Eval.PushEvalStack(fvNew)
        evalRef.Value inheritedClassTypeAst
        evalRef.Value argumentTupleAst
        heap.Eval.PopEvalStack()

    /// Definitions of predicates
    | Ast.DefinitionPredicate((pos1, pos2), (predicateSignatureAst, optDefBlock)) ->
        let parent = heap.Eval.PeekEvalStack()
        let fv = new FplPredicate((pos1, pos2), parent, heap.Helper.GetNextAvailableFplBlockRunOrder)
        heap.Eval.PushEvalStack(fv)
        match predicateSignatureAst with
        | Ast.PredicateSignature(((pos1, pos2), ((simpleSignatureAst, inhPredicateTypeListAstsOpt), paramTupleAst)), optUserDefinedSymbolAst) ->
            heap.Helper.InSignatureEvaluation <- true
            evalRef.Value simpleSignatureAst
            evalRef.Value paramTupleAst
            heap.Helper.InSignatureEvaluation <- false
            optUserDefinedSymbolAst |> Option.map evalRef.Value |> Option.defaultValue () |> ignore
            // The reason why PredicateSignature hast to be evaluated inside DefinitionPredicate
            // is that optDefBlock must be evaluated after the signature 
            match optDefBlock with 
            | Some (predicateContentAst, optPropertyListAsts) ->
                evalRef.Value predicateContentAst
                optPropertyListAsts |> Option.map (List.map evalRef.Value >> ignore) |> Option.defaultValue ()
            | None -> fv.IsIntrinsic <- true
            // and before inherited base types 
            inhPredicateTypeListAstsOpt |> Option.map evalRef.Value |> Option.defaultValue ()
            setSignaturePositions pos1 pos2
        | _ -> ()
        heap.Eval.PopEvalStack()
    | Ast.PredicateSignature(((pos1, pos2), ((simpleSignatureAst, inhPredicateTypeListAstsOpt), paramTupleAst)), optUserDefinedSymbolAst) -> 
        ()
        // empty since the pattern will be matched in DefinitionPredicate 
        // we list it her to remove FS0025 incomplete pattern warnings
    | Ast.DefPredicateContent(varDeclBlock, predicateAst) ->
        evalRef.Value varDeclBlock
        evalRef.Value predicateAst

    // Definitions of functional terms
    | Ast.DefinitionFunctionalTerm((pos1, pos2), (functionalTermSignatureAst, functionalTermDefBlockAst)) ->
        let parent = heap.Eval.PeekEvalStack()
        let fv = new FplFunctionalTerm((pos1, pos2), parent, heap.Helper.GetNextAvailableFplBlockRunOrder)
        heap.Eval.PushEvalStack(fv)
        match functionalTermSignatureAst with
        | Ast.FunctionalTermSignature(((pos1, pos2), (((simpleSignatureAst, inhFunctionalTypeListAstsOpt), paramTupleAst), mappingAst)), optUserDefinedSymbolAst) -> 
            evalRef.Value mappingAst
            heap.Helper.InSignatureEvaluation <- true
            evalRef.Value simpleSignatureAst
            evalRef.Value paramTupleAst
            heap.Helper.InSignatureEvaluation <- false
            optUserDefinedSymbolAst |> Option.map evalRef.Value |> Option.defaultValue () 
            // The reason why FunctionalTermSignature hast to be evaluated inside DefinitionFunctionalTerm
            // is that functionalTermDefBlockAst must be evaluated after signature 
            evalRef.Value functionalTermDefBlockAst
            // and before inherited base types 
            inhFunctionalTypeListAstsOpt |> Option.map evalRef.Value |> Option.defaultValue () 
            setSignaturePositions pos1 pos2
        | _ -> ()
        heap.Eval.PopEvalStack()
    | Ast.FunctionalTermSignature(((pos1, pos2), (((simpleSignatureAst, inhFunctionalTypeListAstsOpt), paramTupleAst), mappingAst)), optUserDefinedSymbolAst) -> 
        ()
        // empty since the pattern will be matched in DefinitionFunctionalTerm 
        // we list it her to remove FS0025 incomplete pattern warnings

    | Ast.Mapping((pos1, pos2), variableTypeAst) ->
        let fv = heap.Eval.PeekEvalStack()
        let map = new FplMapping((pos1, pos2), fv)
        heap.Eval.PushEvalStack(map)
        evalRef.Value variableTypeAst
        heap.Eval.PopEvalStack()
    | Ast.FunctionalTermDefinitionBlock((pos1, pos2), optDefBlock) ->
        let functionaTermBlock = heap.Eval.PeekEvalStack()
        match optDefBlock with 
        | Some (funcContentAst, optPropertyListAsts) ->
            evalRef.Value funcContentAst
            optPropertyListAsts |> Option.map (List.map evalRef.Value >> ignore) |> Option.defaultValue ()
            let properties = functionaTermBlock.GetProperties()
            if properties.IsEmpty && functionaTermBlock.ArgList.Count = 1 then
                functionaTermBlock.ErrorOccurred <- emitST001diagnostics functionaTermBlock.Name pos1 pos2
        | None -> functionaTermBlock.IsIntrinsic <- true
    | Ast.DefFunctionContent(varDeclBlock, retStmtAst) ->
        evalRef.Value varDeclBlock
        evalRef.Value retStmtAst

    // Definition properties
    | Ast.PredicateInstance((pos1, pos2), (signatureAst, predInstanceBlockAstOpt)) ->
        let parent = heap.Eval.PeekEvalStack()
        let fvNew = new FplMandatoryPredicate((pos1, pos2), parent)
        heap.Eval.PushEvalStack(fvNew)
        evalRef.Value signatureAst
        match predInstanceBlockAstOpt with 
        | Some predInstanceBlockAst ->
            evalRef.Value predInstanceBlockAst
        | None -> fvNew.IsIntrinsic <- true
        heap.Eval.PopEvalStack()
    | Ast.PredicateInstanceSignature((pos1, pos2), (simpleSignatureAst, paramTupleAst)) ->
        heap.Helper.InSignatureEvaluation <- true
        evalRef.Value simpleSignatureAst
        evalRef.Value paramTupleAst
        setSignaturePositions pos1 pos2
        heap.Helper.InSignatureEvaluation <- false
    | Ast.FunctionalTermInstance((pos1, pos2), (functionalTermInstanceSignatureAst, functionalTermInstanceBlockOptAst)) ->
        let parent = heap.Eval.PeekEvalStack()
        let fvNew = new FplMandatoryFunctionalTerm((pos1, pos2), parent)
        heap.Eval.PushEvalStack(fvNew)
        evalRef.Value functionalTermInstanceSignatureAst
        match functionalTermInstanceBlockOptAst with 
        | Some functionalTermInstanceBlockAst ->
            evalRef.Value functionalTermInstanceBlockAst
        | None -> fvNew.IsIntrinsic <- true
        heap.Eval.PopEvalStack()
    | Ast.FunctionalTermInstanceSignature((pos1, pos2), ((simpleSignatureAst, paramTupleAst), mappingAst)) ->
        heap.Helper.InSignatureEvaluation <- true
        evalRef.Value simpleSignatureAst
        evalRef.Value paramTupleAst
        heap.Helper.InSignatureEvaluation <- false
        evalRef.Value mappingAst
        setSignaturePositions pos1 pos2

    // Definitions of extensions
    | Ast.DefinitionExtension((pos1, pos2), ((extensionNameAst, extensionSignatureAst), extensionTermAst)) ->
        let parent = heap.Eval.PeekEvalStack()
        let fv = new FplExtension((pos1,pos2), parent, heap.Helper.GetNextAvailableFplBlockRunOrder)
        heap.Eval.PushEvalStack(fv)
        evalRef.Value extensionNameAst
        evalRef.Value extensionSignatureAst
        evalRef.Value extensionTermAst
        heap.Eval.PopEvalStack()
    | Ast.ExtensionSignature((pos1, pos2), (extensionAssignmentAst, extensionMappingAst)) ->
        evalRef.Value extensionAssignmentAst
        evalRef.Value extensionMappingAst
    | Ast.ExtensionAssignment((pos1, pos2), (varAst, extensionRegexAst)) ->
        heap.Helper.InSignatureEvaluation <- true
        evalRef.Value varAst
        evalRef.Value extensionRegexAst
        heap.Helper.InSignatureEvaluation <- false
    | _ ->
        failwith (sprintf "{%O} is not a top definition or related node" ast) 
