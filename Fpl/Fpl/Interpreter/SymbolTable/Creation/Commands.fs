(* Copyright (c) 2021+ bookofproofs See LICENSE in the project root for license terms. *)

/// <summary>
/// Provides specialized evaluators for AST nodes related to FPL commands and actions.
/// The evaluators update interpreter state on the shared heap, manipulate the evaluation
/// stack and create or mutate statement/reference nodes (for example <c>FplAssertion</c>,
/// <c>FplAssignment</c>, <c>FplCases</c>, <c>FplForInStmt</c>, etc.) used by the symbol table.
/// </summary>
/// <remarks>
/// This module matches on AST nodes produced by the parser and performs side-effecting
/// operations on <c>heap.Eval</c>. Each matched AST branch typically:
/// - Peeks or pops the current evaluation stack entry as the parent context.
/// - Constructs a new domain-specific object (e.g. a case, assignment or return node).
/// - Pushes the new node to the eval stack, evaluates nested AST nodes via <c>evalRef.Value</c>,
///   and then pops the node once its children have been processed.
/// The function <c>evalCommands</c> is the primary entry point for interpreting command/action AST nodes.
/// </remarks>
module Fpl.Interpreter.SymbolTable.Creation.Commands
open Fpl.Parser.Types
open Fpl.Interpreter.SymbolTable.Storage.Heap
open Fpl.Interpreter.SymbolTable.Types2.References
open Fpl.Interpreter.SymbolTable.Types3.Extensions
open Fpl.Interpreter.SymbolTable.Types3.MapCases
open Fpl.Interpreter.SymbolTable.Types3.AssertStmt
open Fpl.Interpreter.SymbolTable.Types3.Assignments
open Fpl.Interpreter.SymbolTable.Types3.CasesStmt
open Fpl.Interpreter.SymbolTable.Types3.ForStmt
open Fpl.Interpreter.SymbolTable.Creation.Forward


/// <summary>
/// Evaluate an AST node representing a command or action and update the symbol-table
/// evaluation state accordingly.
/// </summary>
/// <param name="ast">The AST node to evaluate. Expected forms include:
/// <c>Ast.Delegate</c>, <c>Ast.Assertion</c>, <c>Ast.Cases</c>, <c>Ast.CaseSingle</c>,
/// <c>Ast.CaseElse</c>, <c>Ast.MapCases</c>, <c>Ast.MapCaseSingle</c>, <c>Ast.MapCaseElse</c>,
/// <c>Ast.Assignment</c>, <c>Ast.ForIn</c>, <c>Ast.InEntity</c>, and <c>Ast.Return</c>.</param>
/// <returns>Unit. The function performs side effects on the interpreter heap and evaluation stack.</returns>
/// <remarks>
/// The function uses <c>heap.Eval</c> operations (<c>PeekEvalStack</c>, <c>PushEvalStack</c>, <c>PopEvalStack</c>)
/// and delegates nested node processing to <c>evalRef.Value</c> and other creation helpers such as
/// <c>evalArgumentTuple</c>. Position tuples (pos1,pos2) are propagated to newly created objects
/// to improve diagnostics and source mapping.
/// </remarks>
/// <exception cref="System.Exception">
/// Thrown via <c>failwith</c> when <paramref name="ast"/> is not recognized as an action/command.
/// </exception>
let evalCommands ast =
    match ast with
    | Ast.Delegate(delegateNameAst, argumentTupleAst) ->
        evalRef.Value delegateNameAst
        evalRef.Value argumentTupleAst
        heap.Eval.PopEvalStack()
    | Ast.Assertion((pos1, pos2), predicateAst) ->
        let fv = heap.Eval.PeekEvalStack()
        let fvNew = new FplAssertion((pos1, pos2), fv)
        heap.Eval.PushEvalStack(fvNew)
        evalRef.Value predicateAst
        heap.Eval.PopEvalStack()
    | Ast.Cases((pos1, pos2), (caseSingleListAsts, caseElseAst)) ->
        let parent = heap.Eval.PeekEvalStack()
        let casesStmt = new FplCases((pos1, pos2), parent)
        heap.Eval.PushEvalStack(casesStmt) // add cases 
        caseSingleListAsts |> List.map (fun caseAst -> evalRef.Value caseAst) |> ignore
        evalRef.Value caseElseAst
        heap.Eval.PopEvalStack() // remove cases
    | Ast.CaseSingle((pos1, pos2), (predicateAst, statementListAsts)) ->
        let parent = heap.Eval.PeekEvalStack()
        let singleCase = new FplCaseSingle((pos1,pos2), parent)
        heap.Eval.PushEvalStack(singleCase) // add single case
        evalRef.Value predicateAst
        statementListAsts |> List.map evalRef.Value |> ignore
        heap.Eval.PopEvalStack() // remove single case 
    | Ast.CaseElse((pos1, pos2), statementListAsts) ->
        let parent = heap.Eval.PeekEvalStack()
        let elseCase = new FplCaseElse((pos1,pos2), parent)
        heap.Eval.PushEvalStack(elseCase) // add else 
        statementListAsts |> List.map evalRef.Value |> ignore
        heap.Eval.PopEvalStack() // remove else 
    | Ast.MapCases((pos1, pos2), (mapCaseSingleAstList, elseStatementAst)) ->
        let parent = heap.Eval.PeekEvalStack()
        let fvNew = new FplMapCases((pos1, pos2), parent)
        heap.Eval.PushEvalStack(fvNew) // add mcases
        mapCaseSingleAstList |> List.map (fun caseAst -> evalRef.Value caseAst) |> ignore
        evalRef.Value elseStatementAst
        heap.Eval.PopEvalStack() // remove mcases
    | Ast.MapCaseSingle((pos1, pos2), (predicateFirstAst, predicateSecondAst)) ->
        let parent = heap.Eval.PeekEvalStack()
        let mapCaseSingle = new FplMapCaseSingle((pos1,pos2), parent)
        heap.Eval.PushEvalStack(mapCaseSingle) // add mcases single
        evalRef.Value predicateFirstAst
        evalRef.Value predicateSecondAst 
        heap.Eval.PopEvalStack() // remove mcases single
    | Ast.MapCaseElse((pos1, pos2), predicateAst) ->
        let parent = heap.Eval.PeekEvalStack()
        let elseCase = new FplMapCaseElse((pos1,pos2), parent)
        heap.Eval.PushEvalStack(elseCase) // add mcases else
        evalRef.Value predicateAst 
        heap.Eval.PopEvalStack() // remove mcases else
    | Ast.Assignment((pos1, pos2), (predicateWithQualificationAst, predicateAst)) ->
        let parent = heap.Eval.PeekEvalStack()
        let fvNew = new FplAssignment((pos1, pos2), parent)
        heap.Eval.PushEvalStack(fvNew) // add assignment
        let assigneeReference = 
            match predicateWithQualificationAst with 
            | Ast.PredicateWithQualification(predicateWithOptSpecificationAst, _) ->
                match predicateWithOptSpecificationAst with 
                | Ast.PredicateWithOptSpecification ((assigneePos1,assigneePos2),(_,_)) ->
                    // create assigneeReference with correct positioning of the assignee (to improve related diagnostics positions)
                    new FplReference((assigneePos1,assigneePos2), fvNew)
                | _ ->
                    new FplReference((pos1,pos2), fvNew)
            | _ ->
                new FplReference((pos1,pos2), fvNew)
        heap.Eval.PushEvalStack(assigneeReference) // add assignee
        evalRef.Value predicateWithQualificationAst
        heap.Eval.PopEvalStack() // remove assignee
        evalRef.Value predicateAst
        heap.Eval.PopEvalStack() // remove Assignment
    | Ast.ForIn((pos1, pos2), (((entityAst, inDomainAst), statementListAst))) ->
        let parent = heap.Eval.PeekEvalStack()
        let forStmt = new FplForInStmt((pos1, pos2), parent)
        heap.Eval.PushEvalStack(forStmt) // add ForInStmt
        let entity = new FplForInStmtEntity((pos1,pos2), forStmt)
        heap.Eval.PushEvalStack(entity) // add ForInStmtEntity
        evalRef.Value entityAst
        heap.Eval.PopEvalStack() // remove ForInStmtEntity
        evalRef.Value inDomainAst
        statementListAst |> List.map (fun stmtAst -> evalRef.Value stmtAst) |> ignore
        heap.Eval.PopEvalStack() // remove ForInStmt
    | Ast.InEntity((pos1, pos2), inDomainAst) ->
        let forStmt = heap.Eval.PeekEvalStack()
        let inDomain = new FplForInStmtDomain((pos1,pos2), forStmt)
        heap.Eval.PushEvalStack(inDomain) // add ForInStmtDomain
        evalRef.Value inDomainAst
        heap.Eval.PopEvalStack() // remove ForInStmtDomain
    | Ast.Return((pos1, pos2), returneeAst) ->
        let fv = heap.Eval.PeekEvalStack()
        let stmt = new FplReturn((pos1,pos2), fv)
        heap.Eval.PushEvalStack(stmt)
        evalRef.Value returneeAst
        heap.Eval.PopEvalStack() 
    | _ ->
        failwith (sprintf "{%O} is not an action" ast)
