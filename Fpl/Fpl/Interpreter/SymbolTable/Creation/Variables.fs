/// This module provides specialized evaluators for the AST nodes related to FPL variables and their declarations.


(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)

module Fpl.Interpreter.SymbolTable.Creation.Variables
open Fpl.Primitives
open Fpl.Parser.Types
open Fpl.Errors.Emitter
open Fpl.Interpreter.BasicTypes
open Fpl.Interpreter.Helpers.Basic
open Fpl.Interpreter.Helpers.Checks
open Fpl.Interpreter.SymbolTable.Storage.Heap
open Fpl.Interpreter.SymbolTable.Types2.Variables
open Fpl.Interpreter.SymbolTable.Types3.Localization
open Fpl.Interpreter.SymbolTable.Creation.Forward

let evalVariables ast =
    match ast with
    | Ast.VarDeclBlock varDeclOrStmtAstList ->
        varDeclOrStmtAstList |> Option.map (List.map evalRef.Value >> ignore) |> Option.defaultValue ()
    | Ast.NamedVarDecl((pos1, pos2), (variableListAst, variableTypeAst)) ->
        let parent = heap.Eval.PeekEvalStack()
        parent.AuxiliaryInfo <- variableListAst |> List.length // remember how many variables to create
        // create all variables of the named variable declaration in the current scope
        variableListAst |> List.iter (fun varAst ->
            match variableTypeAst with 
            | Ast.ArrayType((posMan1, posMan2),(mainTypeAst,  indexAllowedTypeListAst)) ->
                let numberOfVariadicVars = parent.AuxiliaryInfo
                if numberOfVariadicVars > 1 then
                    parent.ErrorOccurred <- emitVAR00Diagnostics posMan1 posMan2        
                match varAst with 
                | Ast.Var((varPos1, varPos2), varName) ->
                    let newVar = new FplVariableArray(varName, (varPos1, varPos2), parent)
                    newVar.IsSignatureVariable <- (heap.Helper.InSignatureEvaluation && hasSignature parent)
                    heap.Eval.PushEvalStack(newVar)
                    evalRef.Value mainTypeAst
                    indexAllowedTypeListAst |> List.map evalRef.Value |> ignore
                    heap.Eval.PopEvalStack()
                | _ -> ()
            | Ast.SimpleVariableType((_, _),simplVariableTypeAst) ->
                match varAst with 
                | Ast.Var((varPos1, varPos2), varName) ->
                    let newVar = new FplVariable(varName, (varPos1, varPos2), parent)
                    newVar.IsSignatureVariable <- (heap.Helper.InSignatureEvaluation && hasSignature parent)
                    heap.Eval.PushEvalStack(newVar)
                    evalRef.Value simplVariableTypeAst
                    heap.Eval.PopEvalStack()
                | _ -> ()
            | _ -> ()
        ) |> ignore 
    | Ast.Var((pos1, pos2), name) ->
        let searchVarByName (fv:FplGenericNode) = 
            match (searchInUpperScopeByName fv name) with
            | ScopeSearchResult.Found foundVar -> 
                // variable was declared in the scope
                match fv.Name with 
                | PrimJIByDefVar 
                | PrimRefL 
                | PrimForInStmtEntityL 
                | PrimForInStmtDomainL ->
                    fv.FplId <- name
                    fv.RefersTo <- Some foundVar
                | PrimTranslationL ->
                    // for translations, use the name of the variable
                    fv.FplId <- foundVar.FplId
                    fv.TypeId <- foundVar.TypeId 
                | _ -> ()
                match foundVar with
                | :? FplGenericVariable as var -> var .SetIsUsed()
                | _ -> ()
            | _ ->
                match fv.UltimateBlockNode with 
                | Some (:? FplLocalization as loc) when heap.Helper.InSignatureEvaluation -> 
                    () // localizations during 
                | _ ->
                    // otherwise emit variable not declared 
                    fv.ErrorOccurred <- emitVAR01diagnostics name pos1 pos2
                
                // if no variable in scope was found, spawn an undefined variable
                let undefVar = new FplVariable(name, (pos1, pos2), fv)
                undefVar.TypeId <- LiteralUndef
                undefVar.SetDefaultValue()
                heap.Eval.PushEvalStack(undefVar)
                heap.Eval.PopEvalStack()
            
        let fv = heap.Eval.PeekEvalStack()
        let parentFv = fv.Parent.Value
        match fv.Name with 
        | PrimVariableL
        | PrimVariableArrayL -> 
            // in the context of variable declarations, we set the name and positions of the variables
            fv.FplId <- name
            fv.TypeId <- LiteralUndef 
            fv.StartPos <- pos1
            fv.EndPos <- pos2
        | PrimExtensionL -> 
            let newVar = new FplVariable(name, (pos1, pos2), fv)
            newVar.TypeId <- fv.FplId
            newVar.IsSignatureVariable <- heap.Helper.InSignatureEvaluation
            heap.Eval.PushEvalStack(newVar)
            heap.Eval.PopEvalStack()
        | PrimRefL ->
            match box parentFv with 
            | :? IHasDotted as dotted when dotted.DottedChild.IsSome ->
                fv.FplId <- name
                fv.TypeId <- LiteralUndef
            | _ -> searchVarByName fv
        | _ -> 
            // in all other contexts, check by name, if this variable was declared in some scope
            searchVarByName fv

        match fv.UltimateBlockNode with 
        | Some (:? FplLocalization as loc) when loc.ArgList.Count = 0 && fv.RefersTo.IsSome -> 
            let variable = fv.RefersTo.Value
            if loc.Scope.ContainsKey(name) then 
                let other = loc.Scope[name]
                variable.ErrorOccurred <- emitVAR11diagnostics name other.QualifiedStartPos pos1 pos2 
            else 
                loc.Scope.Add(name, variable)
                variable.Parent <- Some loc
        | _ -> ()
    | _ ->
        failwith (sprintf "{%O} is not a variable node and not a variable declaration node" ast) 
