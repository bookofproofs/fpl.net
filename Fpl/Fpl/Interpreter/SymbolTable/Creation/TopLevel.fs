(* Copyright (c) 2021+ bookofproofs See LICENSE in the project root for license terms. *)

/// <summary>
/// Provides specialized evaluators for AST nodes related to FPL namespaces and top-level blocks.
/// </summary>
/// <remarks>
/// The evaluator dispatches top-level AST nodes to the forward evaluator (<c>evalRef.Value</c>),
/// and emits syntax diagnostics for parse errors. It is responsible for handling the entry points
/// produced by the parser such as <c>Ast.AST</c>, <c>Ast.Namespace</c>, <c>Ast.UsesClause</c>,
/// and diagnostic nodes (<c>Ast.ErrorSyntax*</c>).
/// </remarks>
/// <exception cref="System.Exception">Thrown by <c>evalTopLevel</c> when an unexpected AST node is supplied.</exception>
module Fpl.Interpreter.SymbolTable.Creation.TopLevel
open System.Collections.Generic
open Fpl.Parser.Types
open Fpl.Errors.Emitter
open Fpl.Interpreter.SymbolTable.Storage.Asts
open Fpl.Interpreter.SymbolTable.Creation.Forward


/// <summary>
/// Find a parsed AST in the provided list whose status is <c>UsesClausesEvaluated</c>.
/// </summary>
/// <param name="parsedAsts">A list of parsed AST wrappers (<c>ParsedAst</c>) to search.</param>
/// <returns>
/// <c>Some ParsedAst</c> when an entry with status <c>ParsedAstStatus.UsesClausesEvaluated</c>
/// exists; otherwise <c>None</c>.
/// </returns>
/// <remarks>
/// This helper performs a linear search using <c>List.Exists</c> and <c>List.Find</c>.
/// It returns the first matching entry. Callers should ensure the list is not <c>null</c>
/// (F# code typically does not use <c>null</c> for lists).
/// </remarks>
/// <exception cref="System.ArgumentNullException">May be thrown if <paramref name="parsedAsts"/> is null.</exception>
let tryFindParsedAstUsesClausesEvaluated (parsedAsts: List<ParsedAst>) =
    if parsedAsts.Exists(fun pa -> pa.Status = ParsedAstStatus.UsesClausesEvaluated) then
        Some(parsedAsts.Find(fun pa -> pa.Status = ParsedAstStatus.UsesClausesEvaluated))
    else
        None

/// <summary>
/// Evaluate a top-level AST node.
/// </summary>
/// <param name="ast">The top-level AST node to evaluate. Recognized shapes include:
/// <c>Ast.AST</c>, <c>Ast.Namespace</c>, <c>Ast.UsesClause</c>, <c>Ast.BuildingBlock</c>,
/// and syntax-error variants (<c>Ast.ErrorSyntax*</c>).</param>
/// <returns>Unit. The function performs side effects such as delegating evaluation to <c>evalRef.Value</c>
/// and emitting syntax diagnostics via the emitter helpers.</returns>
/// <remarks>
/// - <c>Ast.AST</c> delegates evaluation of its payload to the forward evaluator.
/// - <c>Ast.Namespace</c> evaluates each child node in the provided theory list.
/// - <c>Ast.UsesClause</c> and <c>Ast.BuildingBlock</c> delegate to the forward evaluator.
/// - Syntax error nodes call the appropriate emitters to report diagnostics.
/// - Unrecognized nodes cause an exception to be raised to indicate incorrect dispatching.
/// </remarks>
/// <exception cref="System.Exception">Thrown when <paramref name="ast"/> is not a recognized top-level node.</exception>
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
