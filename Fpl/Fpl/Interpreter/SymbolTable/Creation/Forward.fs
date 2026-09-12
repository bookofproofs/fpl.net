(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)

/// <summary>
/// Provides a globally accessible forward reference to the main AST evaluator and a set of
/// small helper functions used by modules that depend on the evaluator.
/// </summary>
/// <remarks>
/// The actual evaluator is assigned to <c>evalRef</c> by the main creation module
/// (see <c>Fpl.Interpreter.SymbolTable.Creation.Main</c>) once all specialized evaluators
/// are available. This module exposes light-weight utilities that operate on the
/// interpreter heap and evaluation stack and that require the evaluator to be invoked.
/// </remarks>
module Fpl.Interpreter.SymbolTable.Creation.Forward
open Fpl.Parser.Types
open Fpl.Interpreter.BasicTypes
open Fpl.Interpreter.SymbolTable.Storage.Heap
open Fpl.Interpreter.SymbolTable.Types2.References
open Fpl.Interpreter.SymbolTable.Types3.SelfParent
open Fpl.Interpreter.SymbolTable.Types3.Delegates

/// <summary>
/// Reference that holds the global AST evaluator function.
/// </summary>
/// <remarks>
/// The reference is initialized with a placeholder function that raises when used.
/// The real evaluator must be assigned to <c>evalRef.Value</c> by the creation sequence
/// before any evaluation that relies on it is performed.
/// </remarks>
/// <exception cref="System.Exception">Thrown if the placeholder evaluator is invoked before initialization.</exception>
let evalRef : (Ast -> unit) ref = ref (fun _ ->
    failwith "evalRef not initialized")

/// <summary>
/// Set signature start and end positions for the topmost evaluation item that implements <c>IHasSignature</c>.
/// </summary>
/// <param name="pos1">Start position to assign to the signature.</param>
/// <param name="pos2">End position to assign to the signature.</param>
/// <remarks>
/// The function inspects the top of the evaluator stack and sets the signature bounds
/// only if the stack item implements <c>IHasSignature</c>. Otherwise it is a no-op.
/// </remarks>
let setSignaturePositions pos1 pos2 = 
    let fv = heap.Eval.PeekEvalStack()
    match box fv with 
    | :? IHasSignature as withSignature -> 
        withSignature.SignStartPos <- pos1  
        withSignature.SignEndPos <- pos2
    | _ -> ()

/// <summary>
/// Evaluate the variable declaration block and its predicate AST by invoking the global evaluator.
/// </summary>
/// <param name="varDeclBlock">AST node representing the variable declaration block.</param>
/// <param name="predicateAst">AST node representing the predicate to evaluate for the declaration.</param>
/// <remarks>
/// Both nodes are evaluated via <c>evalRef.Value</c>. The function assumes the global evaluator
/// has been correctly assigned prior to invocation.
/// </remarks>
let evalCommonStepsVarDeclPredicate varDeclBlock predicateAst =
    evalRef.Value varDeclBlock
    evalRef.Value predicateAst

/// <summary>
/// Consume and evaluate an argument tuple for expression forms that accept argument lists.
/// </summary>
/// <param name="next">The parent node that may receive the arguments (used to create references).</param>
/// <param name="predicateListAst">List of AST nodes representing the argument expressions.</param>
/// <param name="pos1">Start position used for created references.</param>
/// <param name="pos2">End position used for created references.</param>
/// <returns>Unit. Side effects: pushes/pops references on the evaluation stack and invokes the evaluator on argument ASTs.</returns>
/// <remarks>
/// For certain node types (<c>FplEquality</c>, <c>FplDecrement</c>, <c>FplBaseConstructorCall</c>) the parent
/// is used directly. Otherwise, if a preceding reference exists on the eval stack, it will be used
/// and its <c>ArgType</c> is set to <c>ArgType.Parentheses</c>.
/// </remarks>
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
        match heap.Eval.GetPrecedingReference() with 
        | Some ref ->
            ref.ArgType <- ArgType.Parentheses
            consumeArgumentsWithParent ref
        | _ -> ()

/// <summary>
/// Simplify trivially nested expressions by replacing removable wrapper reference nodes with their single sub-node.
/// </summary>
/// <param name="rb1">The possibly nested expression node to simplify.</param>
/// <returns>Unit. Side effects: mutates the evaluation stack and updates parent/end position/scope references.</returns>
/// <remarks>
/// A removable reference is identified as an <c>FplReference</c> with a single argument, an empty <c>FplId</c>
/// and an expression type that is not parenthesized. The wrapper is popped from the eval stack and its single
/// sub-node is pushed instead; sub-node parent, end position and scope are adjusted. The routine recurses
/// until no more trivial nesting is present.
/// </remarks>
let rec simplifyTriviallyNestedExpressions (rb1:FplGenericNode) = 
    match rb1 with 
    | :? FplReference as rb when rb.ArgList.Count = 1 && rb.FplId = "" && not rb.ExpressionType.IsParen ->
        // removable reference blocks are those with only a single argument and unset FplId 
        let subNode = rb.ArgList[0] 
        heap.Eval.Pop() |> ignore // pop the removable reference block and ignored it
        heap.Eval.PushEvalStack(subNode) // push its subNode instead
        // adjust subNode's Parent, EndPos, Scope
        subNode.Parent <- rb.Parent 
        subNode.EndPos <- rb.EndPos
        // prevent recursive loops
        rb.ArgList.Clear() 
        rb.Value <- None
        rb.Scope.Clear()
        simplifyTriviallyNestedExpressions subNode
    | _ -> ()
