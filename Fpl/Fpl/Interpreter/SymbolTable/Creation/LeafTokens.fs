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
open Fpl.Interpreter.SymbolTable.Storage.Heap
open Fpl.Interpreter.SymbolTable.Storage.Util
open Fpl.Interpreter.SymbolTable.Types2.Intrinsic
open Fpl.Interpreter.SymbolTable.Types2.References
open Fpl.Interpreter.SymbolTable.Types3.Quantors


let evalLeafTokens ast =
    match ast with
    | Ast.Alias((_, _), _) -> ()
    | Ast.Dot() -> ()
    | Ast.Star((_, _),()) -> ()
    | Ast.Digits s -> 
        let fv = heap.Eval.PeekEvalStack()
        fv.FplId <- s
        fv.TypeId <- s
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
    | Ast.PrefixDecl((pos1, pos2), symbol) -> 
        let fv = heap.Eval.PeekEvalStack()
        fv.ExpressionType <- FixType.Prefix symbol
    | Ast.PostfixDecl((pos1, pos2), symbol) -> 
        let fv = heap.Eval.PeekEvalStack()
        fv.ExpressionType <- FixType.Postfix symbol
    | Ast.SymbolDecl((pos1, pos2), symbol) -> 
        let fv = heap.Eval.PeekEvalStack()
        fv.ExpressionType <- FixType.Symbol symbol
    | Ast.ObjectSymbolWithPos((pos1, pos2), symbol) -> 
        let fv = heap.Eval.PeekEvalStack()
        fv.FplId <- symbol
        fv.TypeId <- symbol
        fv.StartPos <- pos1
        fv.EndPos <- pos2
        checkSIG01Diagnostics fv
    | Ast.InfixSymbolWithPos((pos1, pos2), symbol) -> 
        let fv = heap.Eval.PeekEvalStack()
        fv.FplId <- symbol
        fv.TypeId <- symbol
        fv.StartPos <- pos1
        fv.EndPos <- pos2
        fv.ExpressionType <- FixType.Infix(symbol,-1)
        checkSIG01Diagnostics fv
    | Ast.PostFixSymbolWithPos((pos1, pos2), symbol) -> 
        let fv = heap.Eval.PeekEvalStack()
        fv.FplId <- symbol
        fv.TypeId <- symbol
        fv.StartPos <- pos1
        fv.EndPos <- pos2
        fv.ExpressionType <- FixType.Postfix symbol
        checkSIG01Diagnostics fv
    | Ast.PrefixSymbolWithPos((pos1, pos2), symbol) -> 
        let fv = heap.Eval.PeekEvalStack()
        fv.FplId <- symbol
        fv.TypeId <- symbol
        fv.StartPos <- pos1
        fv.EndPos <- pos2
        fv.ExpressionType <- FixType.Prefix symbol
        checkSIG01Diagnostics fv

    | _ ->
        failwith (sprintf "{%O} is not a leaf token" ast) 
