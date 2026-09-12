(* Copyright (c) 2021+ bookofproofs See LICENSE in the project root for license terms. *)

/// <summary>
/// Module that provides specialized evaluators for AST nodes representing lexical and leaf tokens
/// of the FPL language.
/// </summary>
/// <remarks>
/// The evaluator inspects lexical AST nodes (digits, symbols, object names, dollar-digit quantifiers, etc.)
/// and mutates the interpreter evaluation stack and the current evaluation node (top of the stack).
/// It also emits diagnostics for invalid or ambiguous token usages via the project's diagnostics emitters.
/// </remarks>
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
open Fpl.Interpreter.SymbolTable.Types3.Quantifiers


/// <summary>
/// Evaluate a leaf AST node and apply its lexical meaning to the current evaluation context.
/// </summary>
/// <param name="ast">An AST node that is expected to be a leaf/lexical token.</param>
/// <returns>Unit. The function performs side effects on the interpreter heap and evaluation stack.</returns>
/// <remarks>
/// Behavior by AST case:
/// - <c>Ast.Digits</c>: sets <c>FplId</c> and <c>TypeId</c> of the top evaluation node.
/// - <c>Ast.DollarDigits</c>: handles indexed intrinsic identifiers and quantified existentials, emits
///   diagnostics for invalid quantifier values and updates <c>FplId</c>/<c>TypeId</c> appropriately.
/// - Symbol forms (<c>ObjectSymbolWithPos</c>, <c>InfixSymbolWithPos</c>, <c>PostFixSymbolWithPos</c>,
///   <c>PrefixSymbolWithPos</c>) assign identifier, type and position, and update expression fixing type.
/// - Other leaf ASTs that are syntactically allowed but semantically ignored (alias, dot, star) are no-ops.
/// The function calls diagnostic helpers such as <c>checkSIG01Diagnostics</c> and <c>emitSY011Diagnostics</c>
/// where appropriate.
/// </remarks>
/// <exception cref="System.Exception">
/// Thrown when <paramref name="ast"/> is not recognized as a leaf token. The function uses <c>failwith</c>
/// in that case with a message describing the offending AST node.
/// </exception>
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
        | :? FplQuantifierExistsN ->
            match (int)s with
            | 0 ->
                fv.ErrorOccurred <- emitSY011Diagnostics pos1 pos2
            | 1 ->
                fv.ErrorOccurred <- emitSY012Diagnostics pos1 pos2
            | _ -> ()
            fv.FplId <- fv.FplId + sid
        | _  ->
            fv.FplId <- fv.FplId + sid
            match fv.TypeId with 
            | "" when not heap.Helper.InReferenceToProofOrCorollary -> fv.TypeId <- LiteralInd
            | LiteralPred -> ()
            | _ -> fv.TypeId <- fv.TypeId + sid

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
