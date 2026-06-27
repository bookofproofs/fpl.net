/// This module provides specialized evaluators for the AST nodes related to FPL namespace and top level blocks.


(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)

module Fpl.Interpreter.SymbolTable.Creation.TopLevel
open System.Collections.Generic
open Fpl.Parser.Types
open Fpl.Errors.Emitter
open Fpl.Interpreter.SymbolTable.Storage.Asts
open Fpl.Interpreter.SymbolTable.Creation.Forward


let tryFindParsedAstUsesClausesEvaluated (parsedAsts: List<ParsedAst>) =
    if parsedAsts.Exists(fun pa -> pa.Status = ParsedAstStatus.UsesClausesEvaluated) then
        Some(parsedAsts.Find(fun pa -> pa.Status = ParsedAstStatus.UsesClausesEvaluated))
    else
        None

let evalTopLevel ast =
    match ast with
    | Ast.AST((pos1, pos2), ast1) ->
        evalRef.Value ast1
    | Ast.Namespace(theoryAst) ->
        theoryAst |> List.map evalRef.Value |> ignore
    | Ast.UsesClause((pos1, pos2), ast1) ->
        evalRef.Value ast1
    | Ast.BuildingBlock((_, _),buidlingBlockAst) ->
        evalRef.Value buidlingBlockAst
    | Ast.ErrorSyntax((pos1, pos2), errMsg) ->
        emitSY000Diagnostics errMsg pos1 pos2 
    | Ast.ErrorSyntaxBacktracking((pos1, pos2), errMsg) ->
        emitSY001Diagnostics errMsg pos1 pos2 
    | Ast.ErrorSyntaxChain(((pos1, pos2),_), (errMsg, chain)) ->
        emitSY002Diagnostics errMsg chain pos1 pos2 
    | _ ->
        failwith (sprintf "{%O} is not a top level node" ast) 
