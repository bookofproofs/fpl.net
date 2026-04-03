/// This module contains a type used as a heap memory of the FplInterpreter

(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)
module FplInterpreter.Globals.Heap
open System
open System.Text
open ErrDiagnostics
open FplInterpreterBasicTypes
open FplInterpreterAstPreprocessing
open FplInterpreter.Globals.Helpers
open FplInterpreter.Globals.ST
open FplInterpreter.Globals.STEval
open FplInterpreter.Globals.State
open FplInterpreter.Globals.Validity

type HeapMemory() = 
    let _validStmtStore = ValidStmtStore()
    let _evalStack = EvalStack()
    let _helper = Helper()
    let _state = State()
    let _parsedAsts = ParsedAstList()
    let _symbolTable = SymbolTable()
    /// A stack memory storing potential new nodes of the symbol table during its evaluation process
    member this.Eval = _evalStack

    /// A handle for helper variables storing some context during the creation process of the symbol table
    member this.Helper = _helper

    /// A handle for the store with valid statements verified by the FPL interpreter
    member this.ValidStmtStore = _validStmtStore

    /// A handle for the state store used as a memory separating the scope of called FPL nodes like functions or predicates
    member this.State = _state

    /// A handle for the parsed asts (by theory) from the FPL parser
    member this.ParsedAsts = _parsedAsts

    /// A handle for the symbol table from the FPL interpreter
    member this.SymbolTable = _symbolTable

    member this.Root = _symbolTable.Root


    // Clears the heap except parsed asts.
    member this.ClearExceptParsedAsts() = 
        _evalStack.Clear()
        _validStmtStore.Clear()
        _state.Clear()

    // Clears the heap except parsed asts.
    member this.ClearParsedAsts() = 
        _parsedAsts.Clear()
        _symbolTable.Clear()

    // Clears the heap except parsed asts.
    member this.ClearAll() = 
        this.ClearExceptParsedAsts()
        this.ClearParsedAsts()
        ad.Clear()

    /// Returns the uses dependencies of this symbol table needed e.g. for debugging purposes in the FPL language server.
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


let heap = HeapMemory()



