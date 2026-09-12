(* Copyright (c) 2021+ bookofproofs See LICENSE in the project root for license terms. *)

/// <summary>
/// Entry point helper for running the FPL interpreter on a single input buffer.
/// </summary>
/// <remarks>
/// This module coordinates loading of `uses` clauses, symbol-table creation and
/// manages interpreter evaluation state in `heap`. It also ensures unexpected
/// runtime exceptions are converted into a stable GEN00 diagnostic so the
/// language server remains resilient.
/// </remarks>
module Fpl.Interpreter.Main

open System
open Fpl.Errors.Diagnostics
open Fpl.Errors.Emitter
open Fpl.Interpreter.SymbolTable.Storage.Heap
open Fpl.Interpreter.SymbolTable.Creation.UsesClauses
open Fpl.Interpreter.SymbolTable.Creation.Main

/// <summary>
/// Runs the interpreter pipeline for the provided FPL source buffer and URI.
/// </summary>
/// <param name="input">The full FPL source text to interpret.</param>
/// <param name="uri">The normalized <see cref="PathEquivalentUri"/> identifying the source (used for diagnostics and symbol-table naming).</param>
/// <param name="fplLibUrl">Optional library URL used when resolving `uses` clauses (passed to loader).</param>
/// <returns>Unit. Side-effects include populating the global `heap` symbol table and emitting diagnostics.</returns>
/// <remarks>
/// - If <paramref name="input"/> equals the special string "~testGEN00~" a mocked GEN00 diagnostic is emitted (useful for tests).
/// - The function marks evaluation start/end on `heap.IsEvaluating` and increments `heap.SymbolTable.EvalCounter`.
/// - Load and symbol-table creation are performed by calling `loadAllUsesClauses` and `createSymbolTable`.
/// </remarks>
/// <exception cref="System.Exception">
/// All runtime exceptions are caught and converted to a GEN00 diagnostic via <c>emitUnexpectedErrorDiagnostics</c>
/// so the language server does not crash. The original exception is not rethrown.
/// </exception>
let fplInterpreter input (uri:PathEquivalentUri) fplLibUrl = 
    if input = "~testGEN00~" then
        // since GEN00 is a diagnostics for any unexpected errors, this specific input is used to 
        // mock GEN00 for testing purposes
        emitUnexpectedErrorDiagnostics ("mocked GEN00 error")
    else
        try
            heap.ClearAll()
            heap.SymbolTable.EvalCounter <- heap.SymbolTable.EvalCounter + 1
            // mark evaluation started (keeps UI informed)
            heap.IsEvaluating <- true

            heap.SymbolTable.MainTheory <- uri.TheoryName

            loadAllUsesClauses input uri fplLibUrl 
            createSymbolTable()

            // mark evaluation ended (keeps UI informed)
            heap.IsEvaluating <- false
        with ex -> 
            emitUnexpectedErrorDiagnostics $"{ex.Message}{Environment.NewLine}{ex.StackTrace}"
