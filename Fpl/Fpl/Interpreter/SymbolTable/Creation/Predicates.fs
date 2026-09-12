(* Copyright (c) 2021+ bookofproofs See LICENSE in the project root for license terms. *)

/// <summary>
/// Specialized evaluators for AST nodes that represent compound and primitive predicates in FPL.
/// </summary>
/// <remarks>
/// This module constructs appropriate interpreter nodes for logical connectives, quantifiers, boolean
/// literals and other predicate-related constructs. It pushes temporary nodes onto the evaluation stack,
/// delegates nested AST evaluation to the global evaluator via <c>evalRef.Value</c>, performs
/// simplification of trivially nested expressions and emits diagnostics using project emitters.
/// All operations are performed via side effects on the global <c>heap</c>.
/// </remarks>
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
open Fpl.Interpreter.SymbolTable.Types3.Quantifiers
open Fpl.Interpreter.SymbolTable.Creation.Forward


/// <summary>
/// Evaluate a predicate AST node and apply its semantics to the current evaluation context.
/// </summary>
/// <param name="ast">AST node expected to represent a predicate expression (literals, connectives, quantifiers, is-operator, etc.).</param>
/// <returns>Unit. The function mutates the interpreter evaluation stack and nodes (top of stack) to represent the evaluated predicate.</returns>
/// <remarks>
/// The evaluator:
/// - creates appropriate FPL nodes for each construct (for example <c>FplConjunction</c>, <c>FplQuantifierAll</c>, <c>FplIsOperator</c>),
/// - pushes and pops nodes on the global evaluation stack,
/// - delegates nested AST evaluation to <c>evalRef.Value</c>,
/// - performs simplification where appropriate via <c>simplifyTriviallyNestedExpressions</c>,
/// - and sets type or diagnostic information using helpers like <c>checkSY010</c>.
/// </remarks>
/// <exception cref="System.Exception">
/// Thrown (via <c>failwith</c>) when <paramref name="ast"/> is not recognized as a predicate AST node.
/// </exception>
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
        let operand = new FplReference((pos1, pos2), fvNew)
        heap.Eval.PushEvalStack(operand)
        evalRef.Value predicateAst
        simplifyTriviallyNestedExpressions operand
        heap.Eval.PopEvalStack()
        heap.Eval.PopEvalStack()
    | Ast.All((pos1, pos2), (namedVarDeclAstList, predicateAst)) ->
        let parent = heap.Eval.PeekEvalStack()
        let fv = new FplQuantifierAll((pos1, pos2), parent)
        heap.Eval.PushEvalStack(fv) // add all quantifier
        fv.Arity <- fv.Arity + (namedVarDeclAstList |> List.length)
        namedVarDeclAstList
        |> List.map (fun namedVarDeclAst ->
            evalRef.Value namedVarDeclAst
        )
        |> ignore
        evalRef.Value predicateAst
        heap.Eval.PopEvalStack() // remove all quantifier
    | Ast.Exists((pos1, pos2), (namedVarDeclAstList, predicateAst)) ->
        let parent = heap.Eval.PeekEvalStack()
        let fv = new FplQuantifierExists((pos1, pos2), parent)
        heap.Eval.PushEvalStack(fv) // add exists quantifier
        fv.Arity <- fv.Arity + (namedVarDeclAstList |> List.length)
        namedVarDeclAstList
        |> List.map (fun namedVarDeclAst ->
            evalRef.Value namedVarDeclAst
        )
        |> ignore
        evalRef.Value predicateAst
        heap.Eval.PopEvalStack() // remove exists quantifier
    | Ast.Exists1() ->
        let fv = heap.Eval.PeekEvalStack()
        fv.FplId <- fv.FplId + "$1"
    | Ast.ExistsN((pos1, pos2), ((dollarDigitsAst, namedVarDeclListAst), predicateAst)) ->
        let parent = heap.Eval.PeekEvalStack()
        let fv = new FplQuantifierExistsN((pos1, pos2), parent)
        heap.Eval.PushEvalStack(fv) // add exists n quantifier
        evalRef.Value dollarDigitsAst
        namedVarDeclListAst
        |> List.map (fun namedVarDeclAst ->
            evalRef.Value namedVarDeclAst
        )
        |> ignore
        evalRef.Value predicateAst
        heap.Eval.PopEvalStack() // remove exists n quantifier
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
