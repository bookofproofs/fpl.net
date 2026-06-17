/// This module provides a globally accessible reference to the AST evaluator evalRef.
/// The real evaluator is assigned only at the end, in Fpl.Interpreter.SymbolTable.Creation.Main module.
/// Specialized evaluator modules can call the main evaluator via evalRef.Value
/// Moreover, the module provides some helper functions that are needed in modules dependent from that module.
(* MIT License

Copyright (c) 2026+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)


module Fpl.Interpreter.SymbolTable.Creation.Forward
open Fpl.Parser.Types
open Fpl.Interpreter.BasicTypes
open Fpl.Interpreter.SymbolTable.Storage.Heap
open Fpl.Interpreter.SymbolTable.Types2.References
open Fpl.Interpreter.SymbolTable.Types3.SelfParent
open Fpl.Interpreter.SymbolTable.Types3.Delegates

let evalRef : (Ast -> unit) ref = ref (fun _ ->
    failwith "evalRef not initialized")

let setSignaturePositions pos1 pos2 = 
    let fv = heap.Eval.PeekEvalStack()
    match box fv with 
    | :? IHasSignature as withSignature -> 
        withSignature.SignStartPos <- pos1  
        withSignature.SignEndPos <- pos2
    | _ -> ()

let evalCommonStepsVarDeclPredicate varDeclBlock predicateAst =
    evalRef.Value varDeclBlock
    evalRef.Value predicateAst

let evalArgumentTuple (next:FplGenericNode) (predicateListAst:Ast list) pos1 pos2 =
    let consumeArgumentsWithParent (parent:FplGenericNode) =
        if predicateListAst.Length > 0 then 
            predicateListAst 
            |> List.iter (fun pred -> 
                let ref = new FplReference((pos1, pos2), parent)
                heap.Eval.PushEvalStack(ref)
                evalRef.Value pred
                heap.Eval.PopEvalStack()
            )
        
    match next with 
    | :? FplEquality 
    | :? FplDecrement
    | :? FplBaseConstructorCall -> 
        consumeArgumentsWithParent next
    | _ -> 
        match heap.Eval.GetProceedingReference() with 
        | Some ref ->
            ref.ArgType <- ArgType.Parentheses
            consumeArgumentsWithParent ref
        | _ -> ()
