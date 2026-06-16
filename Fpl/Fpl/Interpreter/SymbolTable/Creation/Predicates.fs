/// This module provides specialized evaluators for the AST nodes related to FPL namespace and top level blocks.


(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)

module Fpl.Interpreter.SymbolTable.Creation.Predicates
open Fpl.Primitives
open Fpl.Parser.Types
open Fpl.Interpreter.Helpers.Checks
open Fpl.Interpreter.SymbolTable.Storage.Heap
open Fpl.Interpreter.SymbolTable.Types2.Intrinsic
open Fpl.Interpreter.SymbolTable.Types2.Variables
open Fpl.Interpreter.SymbolTable.Types2.References
open Fpl.Interpreter.SymbolTable.Types2.CompoundPredicates
open Fpl.Interpreter.SymbolTable.Types3.IsOperator
open Fpl.Interpreter.SymbolTable.Types3.Quantors
open Fpl.Interpreter.SymbolTable.Creation.Forward


let evalPredicates ast =
    match ast with
    | Ast.True((pos1, pos2), _) -> 
        let fv = heap.Eval.PeekEvalStack()
        let value = new FplIntrinsicTrue((pos1, pos2), fv)
        heap.Eval.PushEvalStack(value)
        heap.Eval.PopEvalStack()
    | Ast.False((pos1, pos2), _) -> 
        let fv = heap.Eval.PeekEvalStack()
        let value = new FplIntrinsicFalse((pos1, pos2), fv)
        value.TypeId <- LiteralPred
        heap.Eval.PushEvalStack(value)
        heap.Eval.PopEvalStack()
    | Ast.And((pos1, pos2), (predicateAst1, predicateAst2)) ->
        let fv = heap.Eval.PeekEvalStack()
        let fvNew = new FplConjunction((pos1, pos2), fv)
        heap.Eval.PushEvalStack(fvNew)
        evalRef.Value predicateAst1
        evalRef.Value predicateAst2
        heap.Eval.PopEvalStack()
    | Ast.Or((pos1, pos2), (predicateAst1, predicateAst2)) ->
        let fv = heap.Eval.PeekEvalStack()
        let fvNew = new FplDisjunction((pos1, pos2), fv)
        heap.Eval.PushEvalStack(fvNew)
        evalRef.Value predicateAst1
        evalRef.Value predicateAst2
        heap.Eval.PopEvalStack()
    | Ast.Xor((pos1, pos2), (predicateAst1, predicateAst2)) ->
        let fv = heap.Eval.PeekEvalStack()
        let fvNew = new FplExclusiveOr((pos1, pos2), fv)
        heap.Eval.PushEvalStack(fvNew)
        evalRef.Value predicateAst1
        evalRef.Value predicateAst2
        heap.Eval.PopEvalStack()
    | Ast.Impl((pos1, pos2), (predicateAst1, predicateAst2)) ->
        let fv = heap.Eval.PeekEvalStack()
        let fvNew = new FplImplication((pos1, pos2), fv)
        heap.Eval.PushEvalStack(fvNew)
        evalRef.Value predicateAst1
        evalRef.Value predicateAst2
        heap.Eval.PopEvalStack()
        
    | Ast.Iif((pos1, pos2), (predicateAst1, predicateAst2)) ->
        let fv = heap.Eval.PeekEvalStack()
        let fvNew = new FplEquivalence((pos1, pos2), fv)
        heap.Eval.PushEvalStack(fvNew)
        evalRef.Value predicateAst1
        evalRef.Value predicateAst2
        heap.Eval.PopEvalStack()
    | Ast.Not((pos1, pos2), predicateAst) ->
        let fv = heap.Eval.PeekEvalStack()
        let fvNew = new FplNegation((pos1, pos2), fv)
        heap.Eval.PushEvalStack(fvNew)
        evalRef.Value predicateAst
        heap.Eval.PopEvalStack()
    | Ast.All((pos1, pos2), (namedVarDeclAstList, predicateAst)) ->
        let parent = heap.Eval.PeekEvalStack()
        let fv = new FplQuantorAll((pos1, pos2), parent)
        heap.Eval.PushEvalStack(fv) // add all quantor
        fv.Arity <- fv.Arity + (namedVarDeclAstList |> List.length)
        namedVarDeclAstList
        |> List.map (fun namedVarDeclAst ->
            evalRef.Value namedVarDeclAst
        )
        |> ignore
        evalRef.Value predicateAst
        heap.Eval.PopEvalStack() // remove all quantor
    | Ast.Exists((pos1, pos2), (namedVarDeclAstList, predicateAst)) ->
        let parent = heap.Eval.PeekEvalStack()
        let fv = new FplQuantorExists((pos1, pos2), parent)
        heap.Eval.PushEvalStack(fv) // add exists quantor
        fv.Arity <- fv.Arity + (namedVarDeclAstList |> List.length)
        namedVarDeclAstList
        |> List.map (fun namedVarDeclAst ->
            evalRef.Value namedVarDeclAst
        )
        |> ignore
        evalRef.Value predicateAst
        heap.Eval.PopEvalStack() // remove exists quantor
    | Ast.Exists1() ->
        let fv = heap.Eval.PeekEvalStack()
        fv.FplId <- fv.FplId + "$1"
    | Ast.ExistsN((pos1, pos2), ((dollarDigitsAst, namedVarDeclListAst), predicateAst)) ->
        let parent = heap.Eval.PeekEvalStack()
        let fv = new FplQuantorExistsN((pos1, pos2), parent)
        heap.Eval.PushEvalStack(fv) // add exists n quantor
        evalRef.Value dollarDigitsAst
        namedVarDeclListAst
        |> List.map (fun namedVarDeclAst ->
            evalRef.Value namedVarDeclAst
        )
        |> ignore
        evalRef.Value predicateAst
        heap.Eval.PopEvalStack() // remove exists n quantor
    | Ast.IsOperator((pos1, pos2), (isOpArgAst, variableTypeAst)) ->
        let fv = heap.Eval.PeekEvalStack()
        let fvNew = new FplIsOperator((pos1, pos2), fv)
        heap.Eval.PushEvalStack(fvNew)
        let operand = new FplReference((pos1, pos2), fvNew) 
        heap.Eval.PushEvalStack(operand)
        evalRef.Value isOpArgAst
        heap.Eval.PopEvalStack()
        fvNew.ArgList |> Seq.iter checkSY010 
        let typeOfOperand = new FplMapping((pos1, pos2), fvNew) 
        heap.Eval.PushEvalStack(typeOfOperand)
        evalRef.Value variableTypeAst
        heap.Eval.PopEvalStack()
        heap.Eval.PopEvalStack()
    | _ ->
        failwith (sprintf "{%O} is not a predicate" ast) 
