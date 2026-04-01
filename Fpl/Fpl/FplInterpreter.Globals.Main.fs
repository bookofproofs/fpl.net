/// This module contains a type used as a heap memory of the FplInterpreter

(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)
module FplInterpreter.Globals.Heap
open FplInterpreter.Globals.Helpers
open FplInterpreter.Globals.STEval
open FplInterpreter.Globals.State
open FplInterpreter.Globals.Validity

type HeapMemory() = 
    let _validStmtStore = ValidStmtStore()
    let _evalStack = EvalStack()
    let _helper = Helper()
    let _state = State()
    

    /// A stack memory storing potential new nodes of the symbol table during its evaluation process
    member this.Eval = _evalStack

    /// A handle for helper variables storing some context during the creation process of the symbol table
    member this.Helper = _helper

    /// A handle for the store with valid statements verified by the FPL interpreter
    member this.ValidStmtStore = _validStmtStore

    /// A handle for the state store used as a memory separating the scope of called FPL nodes like functions or predicates
    member this.State = _state


    // Clears the globals.
    member this.ClearEvalStack() = 
        _evalStack.Clear()
        _validStmtStore.Clear()
        _state.Clear()

let heap = HeapMemory()



