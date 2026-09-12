
/// <summary>
/// Module providing a process-wide heap memory used by the interpreter to hold
/// evaluation state, parsed ASTs, the symbol table and auxiliary stores.
/// </summary>
/// <remarks>
/// The heap aggregates several stores and helpers used during AST-to-symbol-table
/// evaluation and runtime execution. The singleton instance `heap` exposes the shared state.
/// </remarks>

(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)
module Fpl.Interpreter.SymbolTable.Storage.Heap
open System
open System.Text
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.BasicTypes
open Fpl.Interpreter.SymbolTable.Storage.Asts
open Fpl.Interpreter.Helpers.Basic
open Fpl.Interpreter.SymbolTable.Types1.TopLevel
open Fpl.Interpreter.SymbolTable.Storage.Eval
open Fpl.Interpreter.SymbolTable.Storage.RunState
open Fpl.Interpreter.SymbolTable.Storage.ValidStmts

/// <summary>
/// Aggregates interpreter-wide working memory and result stores used during evaluation
/// and symbol-table construction.
/// </summary>
type HeapMemory() = 
    let _validStmtStore = ValidStmtStore()
    let _evalStack = EvalStack()
    let _helper = Helper()
    let _state = State()
    let _parsedAsts = ParsedAstList()
    let _symbolTable = SymbolTable()
    let mutable _isEvaluating = false

    /// <summary>
    /// Evaluation stack used while creating and embedding symbol-table nodes.
    /// </summary>
    member this.Eval = _evalStack

    /// <summary>
    /// Helper utilities and temporary context used during symbol-table construction.
    /// </summary>
    member this.Helper = _helper

    /// <summary>
    /// Store of validated statements (axioms, theorems, inference rules, derived arguments).
    /// </summary>
    member this.ValidStmtStore = _validStmtStore

    /// <summary>
    /// State store separating variable scopes for called FPL nodes (functions, predicates, etc.).
    /// </summary>
    member this.State = _state

    /// <summary>
    /// Parsed ASTs indexed by their identifiers/theory; used as input to the evaluation pipeline.
    /// </summary>
    member this.ParsedAsts = _parsedAsts

    /// <summary>
    /// The interpreter symbol table containing all created FPL nodes and theories.
    /// </summary>
    member this.SymbolTable = _symbolTable

    /// <summary>
    /// A convenience shortcut to the root node of the internal symbol table.
    /// </summary>
    member this.Root = _symbolTable.Root

    /// <summary>
    /// Flag indicating whether the interpreter is currently performing an evaluation.
    /// </summary>
    /// <remarks>
    /// This flag is set by the interpreter to avoid re-entrance or conflicting operations
    /// while evaluation is in progress.
    /// </remarks>
    member this.IsEvaluating
        with get () = _isEvaluating
        and set (value) = _isEvaluating <- value

    /// <summary>
    /// Clears working-memory structures used during evaluation (evaluation stack and call-state).
    /// </summary>
    /// <returns>Unit.</returns>
    member this.ClearWorkingMemory() = 
        _evalStack.Clear()
        _state.Clear()

    /// <summary>
    /// Clears result-memory structures including parsed ASTs, the symbol table and the validity store.
    /// </summary>
    /// <returns>Unit.</returns>
    member this.ClearResultMemory() = 
        _parsedAsts.Clear()
        _symbolTable.Clear()
        _validStmtStore.ClearValidityStore()

    /// <summary>
    /// Clears the entire heap (working and result memory) and the diagnostics container.
    /// </summary>
    /// <remarks>
    /// Use with care: this resets most interpreter global state.
    /// </remarks>
    member this.ClearAll() = 
        this.ClearWorkingMemory()
        this.ClearResultMemory()
        diagnosticsContainer.Clear()

    /// <summary>
    /// Produces a human-readable representation of uses/dependency information for debugging.
    /// </summary>
    /// <returns>A string describing symbol-table theories and parsed-AST dependencies.</returns>
    member this.UsesDependencies() =
        let sb = StringBuilder()
        sb.AppendLine() |> ignore
        sb.AppendLine("SymbolTable: ") |> ignore

        _symbolTable.Root.Scope
        |> Seq.map (fun theory -> $"{theory.Value.Type(SignatureType.Mixed)} ({theory.Value.Scope.Count})")
        |> String.concat Environment.NewLine
        |> sb.AppendLine
        |> ignore

        sb.AppendLine("ParsedAsts: ") |> ignore

        _parsedAsts.EnrichDependencies sb

        sb.ToString()

/// <summary>
/// The global heap instance used by the FPL interpreter.
/// </summary>
let heap = HeapMemory()



