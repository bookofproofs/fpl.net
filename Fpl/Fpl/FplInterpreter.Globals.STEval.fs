/// This module contains classes and utilities used during the transition process 
/// between the AST of the FPL parser and the symbol table of the FPL interpreter.

(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)

module FplInterpreter.Globals.STEval
open System.Collections.Generic
open FplPrimitives
open FplInterpreterBasicTypes


/// A stack memory storing potential new nodes of the symbol table during its evaluation process
type EvalStack() =
    let _valueStack = Stack<FplGenericNode>()

    member this.EvalStack = _valueStack

    // Pops an FplValue from stack without propagating it's name and signature to the next FplValue on the stack.
    member this.Pop() = _valueStack.Pop()

    // Pops an FplValue from stack and propagates it's name and signature to the next FplValue on the stack.
    member this.PopEvalStack() = 
        let fv = _valueStack.Pop()
        if _valueStack.Count > 0 then
            let next = _valueStack.Peek()
            fv.EmbedInSymbolTable (Some next) 

    // Pushes an new symbol table node on the stack.
    member this.PushEvalStack fv = _valueStack.Push fv

    // Peeks a potential new symbol table node from the stack.
    member this.PeekEvalStack() = _valueStack.Peek()

    member this.Clear() = _valueStack.Clear()

    member this.GetProceedingReference() =
        _valueStack
        |> Seq.tryFind (fun fv -> fv.Name = PrimRefL)
