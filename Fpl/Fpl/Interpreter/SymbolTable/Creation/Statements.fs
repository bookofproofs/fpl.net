/// This module provides specialized evaluators for the AST nodes related to FPL axioms, conjectures, and theorem-like statements.

(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)

module Fpl.Interpreter.SymbolTable.Creation.TheoremLikeStmts
open Fpl.Parser.Types
open Fpl.Interpreter.BasicTypes
open Fpl.Interpreter.SymbolTable.Storage.Heap
open Fpl.Interpreter.SymbolTable.Types3.PredicativeBlocks
open Fpl.Interpreter.SymbolTable.Creation.Forward


let private commonEvalStatement signatureAst optVarDeclOrSpecList predicateAst (fv:FplGenericNode) = 
    heap.Eval.PushEvalStack(fv)
    evalRef.Value signatureAst
    evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst
    heap.Eval.PopEvalStack()

let evalStatements ast =
    match ast with
    | Ast.Axiom((pos1, pos2), (signatureAst, (optVarDeclOrSpecList, predicateAst))) ->
        let parent = heap.Eval.PeekEvalStack()
        let fv = new FplAxiom((pos1, pos2), parent, heap.Helper.GetNextAvailableFplBlockRunOrder)
        commonEvalStatement signatureAst optVarDeclOrSpecList predicateAst fv
    | Ast.Theorem((pos1, pos2), (signatureAst, (optVarDeclOrSpecList, predicateAst))) ->
        let parent = heap.Eval.PeekEvalStack()
        let fv = new FplTheorem((pos1, pos2), parent, heap.Helper.GetNextAvailableFplBlockRunOrder)
        commonEvalStatement signatureAst optVarDeclOrSpecList predicateAst fv
    | Ast.Lemma((pos1, pos2), (signatureAst, (optVarDeclOrSpecList, predicateAst))) ->
        let parent = heap.Eval.PeekEvalStack()
        let fv = new FplLemma((pos1, pos2), parent, heap.Helper.GetNextAvailableFplBlockRunOrder)
        commonEvalStatement signatureAst optVarDeclOrSpecList predicateAst fv
    | Ast.Proposition((pos1, pos2), (signatureAst, ((optVarDeclOrSpecList, predicateAst)))) ->
        let parent = heap.Eval.PeekEvalStack()
        let fv = new FplProposition((pos1, pos2), parent, heap.Helper.GetNextAvailableFplBlockRunOrder)
        commonEvalStatement signatureAst optVarDeclOrSpecList predicateAst fv
    | Ast.Conjecture((pos1, pos2), (signatureAst, (optVarDeclOrSpecList, predicateAst))) ->
        let parent = heap.Eval.PeekEvalStack()
        let fv = new FplConjecture((pos1, pos2), parent, heap.Helper.GetNextAvailableFplBlockRunOrder)
        commonEvalStatement signatureAst optVarDeclOrSpecList predicateAst fv
    | Ast.AxiomSignature((pos1, pos2), simpleSignatureAst) 
    | Ast.TheoremSignature((pos1, pos2), simpleSignatureAst) 
    | Ast.PropositionSignature((pos1, pos2), simpleSignatureAst) 
    | Ast.LemmaSignature((pos1, pos2), simpleSignatureAst) 
    | Ast.ConjectureSignature((pos1, pos2), simpleSignatureAst) ->
        heap.Helper.InSignatureEvaluation <- true
        evalRef.Value simpleSignatureAst
        setSignaturePositions pos1 pos2
        heap.Helper.InSignatureEvaluation <- false
    | Ast.Corollary ((pos1, pos2), (corollarySignatureAst, (optVarDeclOrSpecList, predicateAst))) ->
        let parent = heap.Eval.PeekEvalStack()
        let fv = new FplCorollary((pos1, pos2), parent, heap.Helper.GetNextAvailableFplBlockRunOrder)
        heap.Eval.PushEvalStack(fv)
        evalRef.Value corollarySignatureAst
        heap.Eval.PopEvalStack() // add to parent theorem (if any) 
        heap.Eval.PushEvalStack(fv) // push again to have the current corollary on stack
        evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst
        // now, we are ready to emit VAR04 diagnostics for all variables declared in the signature of the corollary.
        fv.CheckConsistency()
        heap.Eval.Pop() |> ignore // pop 
    | Ast.CorollarySignature((pos1, pos2), (simpleSignatureAst, dollarDigitListAsts)) ->
        heap.Helper.InSignatureEvaluation <- true
        evalRef.Value simpleSignatureAst
        dollarDigitListAsts |> List.map evalRef.Value |> ignore
        setSignaturePositions pos1 pos2
        heap.Helper.InSignatureEvaluation <- false
    | _ ->
        failwith (sprintf "{%O} is not a theorem-like statement or dependent node" ast) 
