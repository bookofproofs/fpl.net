/// This module provides specialized evaluators for the AST nodes related to commands and actions.


(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)

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
