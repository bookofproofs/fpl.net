(* Copyright (c) 2021+ bookofproofs See LICENSE in the project root for license terms. *)

/// <summary>
/// Provides specialized evaluators for AST nodes that represent FPL special references,
/// such as intrinsics, undefined markers, self/parent references, and extensions.
/// </summary>
/// <remarks>
/// The evaluator mutates the global interpreter symbol-table <c>heap</c>: it may set flags
/// on the current frame (for example <c>IsIntrinsic</c>), create and push new FPL frames,
/// resolve scope references (<c>SelfBlock</c> / <c>ParentBlock</c>) and assign <c>RefersTo</c>,
/// and assign <c>FplId</c> for extension objects. Nested AST nodes are delegated to
/// <c>evalRef.Value</c> for further processing.
/// </remarks>
/// <exception cref="System.Exception">Thrown when an AST node not recognized as a special reference is supplied.</exception>
module Fpl.Interpreter.SymbolTable.Creation.SpecialReferences
open Fpl.Primitives
open Fpl.Parser.Types
open Fpl.Interpreter.BasicTypes
open Fpl.Interpreter.SymbolTable.Storage.Heap
open Fpl.Interpreter.SymbolTable.Types2.Intrinsic
open Fpl.Interpreter.SymbolTable.Types2.Definitions
open Fpl.Interpreter.SymbolTable.Types3.SelfParent
open Fpl.Interpreter.SymbolTable.Types3.Extensions
open Fpl.Interpreter.SymbolTable.Creation.Forward


/// <summary>
/// Evaluate AST nodes that correspond to special reference constructs.
/// </summary>
/// <param name="ast">The AST node to evaluate. Expected shapes include:
/// <c>Ast.Intrinsic</c>, <c>Ast.Undefined</c>, <c>Ast.SelfOrParent</c>,
/// <c>Ast.Self</c>, <c>Ast.Parent</c>, and <c>Ast.Extension</c>.</param>
/// <returns>Unit. Side effects: updates the current evaluation frame(s) on <c>heap</c>,
/// may push/pop transient frames and may set reference resolutions on created frames.</returns>
/// <remarks>
/// - <c>Ast.Intrinsic</c>: marks the current frame as intrinsic (<c>IsIntrinsic</c>).
///   When the current frame is a class (<c>PrimClassL</c>), a default constructor is added.
/// - <c>Ast.Undefined</c>: creates a transient <c>FplIntrinsicUndef</c> frame and immediately
///   pushes and pops it to register the undefined marker in the current scope.
/// - <c>Ast.Self</c> and <c>Ast.Parent</c>: create <c>FplSelf</c>/<c>FplParent</c> frames,
///   attempt to resolve the corresponding block and set <c>RefersTo</c> when found.
/// - <c>Ast.Extension</c>: creates an <c>FplExtensionObj</c>, assigns its <c>FplId</c> to the
///   provided extension string and registers it in the current frame context.
/// - Nested AST nodes are evaluated by delegating to <c>evalRef.Value</c>.</remarks>
/// <exception cref="System.Exception">Thrown when <paramref name="ast"/> is not a special reference node.</exception>
let evalSpecRef ast =
    match ast with
    | Ast.Intrinsic((pos1, pos2),()) -> 
        let fv = heap.Eval.PeekEvalStack()
        fv.IsIntrinsic <- true // flag that this block is intrinsic
        match fv.Name with 
        | PrimClassL ->
            let cl = fv :?> FplClass
            cl.AddDefaultConstructor()
        | _ -> ()
    | Ast.Undefined((pos1, pos2), _) -> 
        let fv = heap.Eval.PeekEvalStack()
        let fvNew = new FplIntrinsicUndef((pos1, pos2), fv)
        heap.Eval.PushEvalStack(fvNew)
        heap.Eval.PopEvalStack()
    | Ast.SelfOrParent((pos1, pos2), selforParentAst) -> 
        evalRef.Value selforParentAst
    | Ast.Self((pos1, pos2), _) -> 
        let parent = heap.Eval.PeekEvalStack()
        let fv = new FplSelf((pos1, pos2), parent)
        match fv.SelfBlock with
        | ScopeSearchResult.Found block ->
            fv.RefersTo <- Some block
        | _ -> ()
        heap.Eval.PushEvalStack(fv)
        heap.Eval.PopEvalStack()
    | Ast.Parent((pos1, pos2), _) -> 
        let parent = heap.Eval.PeekEvalStack()
        let fv = new FplParent((pos1, pos2), parent)
        match fv.ParentBlock with
        | ScopeSearchResult.Found block ->
            fv.RefersTo <- Some block
        | _ -> ()
        heap.Eval.PushEvalStack(fv)
        heap.Eval.PopEvalStack()
    | Ast.Extension((pos1, pos2), extensionString) ->
        let fv = heap.Eval.PeekEvalStack()
        let fplNew = new FplExtensionObj((pos1,pos2), fv)
        heap.Eval.PushEvalStack(fplNew)
        fplNew.FplId <- extensionString
        heap.Eval.PopEvalStack()
    | _ ->
        failwith (sprintf "{%O} is not a special reference node" ast)
