/// This module provides specialized evaluators for the AST nodes related to FPL tuple-like constructs and dotted qualifiers


(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)

module Fpl.Interpreter.SymbolTable.Creation.Tuples
open Fpl.Parser.Types
open Fpl.Interpreter.BasicTypes
open Fpl.Interpreter.SymbolTable.Storage.Heap
open Fpl.Interpreter.SymbolTable.Types2.References
open Fpl.Interpreter.SymbolTable.Creation.Forward

let evalTuples ast =
    match ast with
    | Ast.BrackedCoordList((pos1, pos2), coordListAst) ->
        let getProceedingReference = heap.Eval.GetProceedingReference()

        match getProceedingReference with 
        | Some ref -> 
            ref.ArgType <- ArgType.Brackets
            if coordListAst.Length > 0 then 
                coordListAst 
                |> List.iter (fun pred -> 
                    let ref = new FplReference((pos1, pos2), ref)
                    heap.Eval.PushEvalStack(ref)
                    evalRef.Value pred
                    heap.Eval.PopEvalStack()
                ) 
        | _ -> ()
    | Ast.ArgumentTuple((pos1, pos2), predicateListAst) ->
        let next = heap.Eval.PeekEvalStack()
        evalArgumentTuple next predicateListAst pos1 pos2
    | Ast.QualificationList((pos1, pos2), asts) ->
        asts |> List.map evalRef.Value |> ignore
    | Ast.DottedPredicate((pos1, pos2), predicateWithOptSpecificationAst) ->
        let fv = heap.Eval.PeekEvalStack()
        let refBlock = new FplReference((pos1, pos2), fv) 
        match fv with 
        | :? FplReference as ref ->
            ref.DottedChild <- Some refBlock
        | _ -> ()
        heap.Eval.PushEvalStack(refBlock)
        evalRef.Value predicateWithOptSpecificationAst
        heap.Eval.PopEvalStack()
    | Ast.ParamTuple namedVariableDeclarationListAsts ->
        let fv = heap.Eval.PeekEvalStack()
        fv.ArgType <- ArgType.Parentheses
        namedVariableDeclarationListAsts |> List.map (fun child ->
            match child with 
            | Ast.NamedVarDecl(_,(varList,_)) -> fv.Arity <- fv.Arity + varList.Length
            | _ -> ()
            evalRef.Value child
        ) |> ignore

    | _ ->
        failwith (sprintf "{%O} is not a tuple-like construct or qualifier" ast) 
