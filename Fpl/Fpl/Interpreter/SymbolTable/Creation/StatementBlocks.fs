(* Copyright (c) 2021+ bookofproofs See LICENSE in the project root for license terms. *)

/// <summary>
/// Provides specialized evaluators for AST nodes related to FPL axioms, conjectures,
/// theorems, propositions, lemmas and corollaries.
/// </summary>
/// <remarks>
/// The evaluators in this module update the interpreter symbol-table heap state,
/// push/pop frames on the evaluation stack, delegate nested AST evaluation to
/// <c>evalRef.Value</c>, and manage signature-evaluation flags via <c>heap.Helper</c>.
/// </remarks>
/// <exception cref="System.Exception">Thrown when an unsupported AST node is supplied to the top-level evaluator.</exception>
module Fpl.Interpreter.SymbolTable.Creation.StatementBlocks
open Fpl.Parser.Types
open Fpl.Interpreter.BasicTypes
open Fpl.Interpreter.SymbolTable.Storage.Heap
open Fpl.Interpreter.SymbolTable.Types3.PredicativeBlocks
open Fpl.Interpreter.SymbolTable.Creation.Forward


/// <summary>
/// Perform the common evaluation steps shared by theorem-like statements.
/// </summary>
/// <param name="signatureAst">AST node representing the statement signature (may include identifiers and types).</param>
/// <param name="optVarDeclOrSpecList">Optional AST node for variable declarations or specifications associated with the statement.</param>
/// <param name="predicateAst">AST node for the predicate (the logical content) of the statement.</param>
/// <param name="fv">The <c>FplGenericNode</c> frame created for the statement to be pushed on the evaluation stack.</param>
/// <returns>Unit. Side effects: pushes <c>fv</c> to the evaluation stack, evaluates the provided child ASTs, and then pops the frame.</returns>
/// <remarks>
/// This helper centralizes the push/evaluate/pop pattern used by `Axiom`, `Theorem`, `Lemma`,
/// `Proposition` and `Conjecture` evaluators. It relies on global `heap` and `evalRef` values
/// and must be invoked only when `fv` is already constructed with the correct parent and position.
/// </remarks>
/// <exception cref="System.Exception">Propagates exceptions thrown by nested evaluators or heap operations.</exception>
let private commonEvalStatement signatureAst optVarDeclOrSpecList predicateAst (fv:FplGenericNode) = 
    heap.Eval.PushEvalStack(fv)
    evalRef.Value signatureAst
    evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst
    heap.Eval.PopEvalStack()

/// <summary>
/// Evaluate AST nodes that represent top-level statement blocks (axioms, theorems, lemmas,
/// propositions, conjectures and corollaries) and their signatures and dependent nodes.
/// </summary>
/// <param name="ast">The AST node to evaluate. Recognized shapes include:
/// <c>Ast.Axiom</c>, <c>Ast.Theorem</c>, <c>Ast.Lemma</c>, <c>Ast.Proposition</c>,
/// <c>Ast.Conjecture</c>, <c>Ast.Corollary</c>, and their signature variants.</param>
/// <returns>Unit. Side effects: constructs FPL block frames, manipulates the evaluation stack,
/// evaluates signatures, variable declaration/spec lists, predicates, and performs consistency checks.</returns>
/// <remarks>
/// - For `Axiom`, `Theorem`, `Lemma`, `Proposition` and `Conjecture` a specific <c>Fpl*</c> frame
///   is created, pushed, and processed through <c>commonEvalStatement</c>.
/// - Signature nodes set <c>heap.Helper.InSignatureEvaluation</c> to true while the contained
///   signature AST is evaluated, record signature positions via <c>setSignaturePositions</c>,
///   and then restore the flag.
/// - `Corollary` nodes receive special handling: the corollary frame is briefly pushed to
///   register the signature, popped to attach to a parent theorem if present, pushed again
///   for processing the variable/predicate portion, and finally checked for consistency.
/// </remarks>
/// <exception cref="System.Exception">Thrown when <paramref name="ast"/> is not a recognized theorem-like statement or dependent node.</exception>
let evalStatementBlocks ast =
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
