(* Copyright (c) 2021+ bookofproofs See LICENSE in the project root for license terms. *)

/// <summary>
/// Provides specialized evaluators for AST nodes related to FPL tuple-like constructs
/// and dotted qualifiers. The evaluators update interpreter state on the shared heap,
/// manipulate the evaluation stack and create or mutate <see cref="FplReference"/> instances
/// to represent tuple-like structures and qualified names.
/// </summary>
/// <remarks>
/// This module handles the following AST node kinds:
/// - <c>Ast.BrackedCoordList</c>: creates bracketed references for preceding reference and evaluates nested predicates.
/// - <c>Ast.ArgumentTuple</c>: invokes <c>evalArgumentTuple</c> for argument tuples using the next stack entry.
/// - <c>Ast.QualificationList</c>: evaluates a list of qualifiers.
/// - <c>Ast.DottedPredicate</c>: builds dotted child references and evaluates the qualified predicate.
/// - <c>Ast.ParamTuple</c>: marks the current reference as a parenthesized parameter tuple and updates arity
///   according to declared named variables.
/// The functions in this module operate via side effects on <c>heap.Eval</c> and related reference types.
/// </remarks>
module Fpl.Interpreter.SymbolTable.Creation.Tuples
open Fpl.Parser.Types
open Fpl.Interpreter.BasicTypes
open Fpl.Interpreter.SymbolTable.Storage.Heap
open Fpl.Interpreter.SymbolTable.Types2.References
open Fpl.Interpreter.SymbolTable.Creation.Forward

/// <summary>
/// Evaluate an AST node that represents a tuple-like construct or a dotted qualifier.
/// </summary>
/// <param name="ast">The AST node to evaluate. Expected forms include
/// <c>Ast.BrackedCoordList</c>, <c>Ast.ArgumentTuple</c>, <c>Ast.QualificationList</c>,
/// <c>Ast.DottedPredicate</c> and <c>Ast.ParamTuple</c>.</param>
/// <returns>Unit. This function performs side-effecting updates to the interpreter heap and evaluation stack.</returns>
/// <remarks>
/// Behavior by AST shape:
/// - <c>Ast.BrackedCoordList((pos1,pos2), coordListAst)</c>:
///   - Retrieves the preceding reference from <c>heap.Eval.GetPrecedingReference()</c>.
///   - Sets its <c>ArgType</c> to <c>ArgType.Brackets</c>.
///   - For each coordinate predicate creates a new <c>FplReference</c>, pushes it on the eval stack,
///     evaluates the predicate via <c>evalRef.Value</c>, and then pops the stack.
/// - <c>Ast.ArgumentTuple((pos1,pos2), predicateListAst)</c>:
///   - Peeks the next evaluation stack entry and calls <c>evalArgumentTuple</c> to process arguments.
/// - <c>Ast.QualificationList((pos1,pos2), asts)</c>:
///   - Evaluates each qualifier via <c>evalRef.Value</c>.
/// - <c>Ast.DottedPredicate((pos1,pos2), predicateWithOptSpecificationAst)</c>:
///   - Peeks the current value from the eval stack, creates a dotted child <c>FplReference</c>,
///     links it as <c>DottedChild</c> when the current value is an <c>FplReference</c>, pushes the new
///     block, evaluates the predicate and then pops the stack.
/// - <c>Ast.ParamTuple namedVariableDeclarationListAsts</c>:
///   - Marks the current reference's <c>ArgType</c> as <c>ArgType.Parentheses</c> and increments its
///     <c>Arity</c> according to declared named variables, evaluating each child via <c>evalRef.Value</c>.
/// </remarks>
/// <exception cref="System.Exception">
/// Thrown when <paramref name="ast"/> is not a recognized tuple-like construct or qualifier.
/// This occurs via <c>failwith</c> in the default match case.
/// </exception>
let evalTuples ast =
    match ast with
    | Ast.BrackedCoordList((pos1, pos2), coordListAst) ->
        let getPrecedingReference = heap.Eval.GetPrecedingReference()

        match getPrecedingReference with 
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
