(* Copyright (c) 2021+ bookofproofs See LICENSE in the project root for license terms. *)

/// <summary>
/// Provides specialized evaluators for AST nodes that implement FPL rules of inference.
/// </summary>
/// <remarks>
/// This module evaluates AST nodes related to rule-of-inference constructs and updates
/// the interpreter symbol-table heap state. Evaluation can push and pop frames on the
/// evaluation stack, set signature-evaluation flags and delegate to the forward evaluator
/// referenced by <c>evalRef</c> for nested ASTs.
/// </remarks>
/// <exception cref="System.Exception">Thrown when an unsupported AST node is supplied.</exception>
module Fpl.Interpreter.SymbolTable.Creation.RulesOfInferences
open Fpl.Parser.Types
open Fpl.Interpreter.SymbolTable.Storage.Heap
open Fpl.Interpreter.SymbolTable.Types3.RulesOfInferences
open Fpl.Interpreter.SymbolTable.Creation.Forward


/// <summary>
/// Evaluate AST nodes that represent rules of inference and related structures.
/// </summary>
/// <param name="ast">The AST node to evaluate. Expected shapes include:
/// <c>Ast.RuleOfInference</c>, <c>Ast.RuleOfInferenceSignature</c>,
/// <c>Ast.PremiseConclusionBlock</c>, and <c>Ast.PremiseList</c>.</param>
/// <returns>Unit. Side effects: updates the global <c>heap</c> (evaluation stack, helper flags,
/// and rule/predicate list objects) and delegates evaluation of child nodes to <c>evalRef.Value</c>.</returns>
/// <remarks>
/// - For <c>Ast.RuleOfInference</c> the function creates a new <c>FplRuleOfInference</c> frame,
///   pushes it on the evaluation stack, evaluates the signature and premise/conclusion block,
///   then pops the frame.
/// - For <c>Ast.RuleOfInferenceSignature</c> the function sets <c>heap.Helper.InSignatureEvaluation</c>
///   to true while evaluating the contained signature, records signature positions and restores the flag.
/// - For <c>Ast.PremiseConclusionBlock</c> the function evaluates the variable declaration block,
///   then the premise and conclusion ASTs.
/// - For <c>Ast.PremiseList</c> the function creates a new <c>FplPredicateList</c> frame,
///   pushes it, evaluates each predicate AST, then pops the frame.
/// </remarks>
/// <exception cref="System.Exception">Thrown when <paramref name="ast"/> is not a recognized top-level node.</exception>
let evalRulesOfInferences ast =
    match ast with
    | Ast.RuleOfInference((pos1, pos2), (signatureAst, premiseConclusionBlockAst)) ->
        let parent = heap.Eval.PeekEvalStack()
        let fv = new FplRuleOfInference((pos1, pos2), parent, heap.Helper.GetNextAvailableFplBlockRunOrder)
        heap.Eval.PushEvalStack(fv)
        evalRef.Value signatureAst
        evalRef.Value premiseConclusionBlockAst
        heap.Eval.PopEvalStack() 
    | Ast.RuleOfInferenceSignature((pos1, pos2), simpleSignatureAst) ->
        heap.Helper.InSignatureEvaluation <- true
        evalRef.Value simpleSignatureAst
        setSignaturePositions pos1 pos2
        heap.Helper.InSignatureEvaluation <- false
    | Ast.PremiseConclusionBlock (varDeclBlock, (premiseAst, conclusionAst)) ->
        evalRef.Value varDeclBlock 
        evalRef.Value premiseAst
        evalRef.Value conclusionAst
    | Ast.PremiseList((pos1, pos2), predicateListAsts) ->
        let parent = heap.Eval.PeekEvalStack()
        let fv = new FplPredicateList((pos1, pos2), parent, heap.Helper.GetNextAvailableFplBlockRunOrder) 
        heap.Eval.PushEvalStack(fv)
        predicateListAsts |> List.map evalRef.Value |> ignore
        heap.Eval.PopEvalStack()
    | _ ->
        failwith (sprintf "{%O} is not a top level node" ast) 
