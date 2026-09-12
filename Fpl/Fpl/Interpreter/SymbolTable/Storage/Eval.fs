(* Copyright (c) 2021+ bookofproofs See LICENSE in the project root for license terms. *)

/// <summary>
/// This module contains classes and utilities used during the transition process 
/// between the AST of the FPL parser and the symbol table of the FPL interpreter.
/// </summary>
module Fpl.Interpreter.SymbolTable.Storage.Eval
open System.Collections.Generic
open Fpl.Primitives
open Fpl.Interpreter.BasicTypes


/// <summary>
/// A stack memory storing potential new nodes of the symbol table during its evaluation process.
/// </summary>
type EvalStack() =
    let _valueStack = Stack<FplGenericNode>()

    /// <summary>
    /// Underlying low-level .NET stack containing candidate <see cref="FplGenericNode"/> values.
    /// </summary>
    member this.EvalStack = _valueStack

    /// <summary>
    /// Pops an <see cref="FplGenericNode"/> from the stack without propagating its name and signature to the next node.
    /// </summary>
    /// <returns>The popped <see cref="FplGenericNode"/>.</returns>
    member this.Pop() = _valueStack.Pop()

    /// <summary>
    /// Pops an <see cref="FplGenericNode"/> and propagates its name and signature to the next node on the stack.
    /// </summary>
    /// <remarks>
    /// If the stack becomes empty after popping, no propagation occurs.
    /// </remarks>
    member this.PopEvalStack() = 
        let fv = _valueStack.Pop()
        if _valueStack.Count > 0 then
            let next = _valueStack.Peek()
            fv.EmbedInSymbolTable (Some next) 

    /// <summary>
    /// Pushes a new symbol table node onto the evaluation stack.
    /// </summary>
    /// <param name="fv">The <see cref="FplGenericNode"/> to push.</param>
    member this.PushEvalStack fv = _valueStack.Push fv

    /// <summary>
    /// Peeks the node currently on top of the evaluation stack without removing it.
    /// </summary>
    /// <returns>The top <see cref="FplGenericNode"/>.</returns>
    member this.PeekEvalStack() = _valueStack.Peek()

    /// <summary>
    /// Clears the entire evaluation stack.
    /// </summary>
    member this.Clear() = _valueStack.Clear()

    /// <summary>
    /// Finds the nearest preceding reference node on the stack.
    /// </summary>
    /// <returns>
    /// <c>Some(FplGenericNode)</c> if a node with name <c>PrimRefL</c> is found; otherwise <c>None</c>.
    /// </returns>
    member this.GetPrecedingReference() =
        _valueStack
        |> Seq.tryFind (fun fv -> fv.Name = PrimRefL)
