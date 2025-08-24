/// This module is used to manage the evalutation stack of the FPL interpreter.

(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)
module EvalStackHandler
open System.Collections.Generic
open ErrDiagnostics
open FplInterpreterTypes
open FplInterpreterDiagnosticsEmitterPre

type EvalStack() = 
    let _valueStack = Stack<FplValue>()

    // Pops an FplValue from stack without propagating it's name and signature to the next FplValue on the stack.
    member this.Pop() = _valueStack.Pop()

    // Pops an FplValue from stack and propagates it's name and signature to the next FplValue on the stack.
    member this.PopEvalStack() = 
        let fv = _valueStack.Pop()
        if _valueStack.Count > 0 then
            let next = _valueStack.Peek()

            match fv.Name with 
            | "intrinsic object"
            | "quantor"
            | "justification"
            | "argument inference"
            | "mapping"
            | "translation" 
            | "statement"
            | "return statement"
            | "assertion"
            | "intrinsic predicate"
            | "intrinsic functional term"
            | "undef"
            | "tpl"
            | "instance" 
            | "theory" 
            | "intrinsic index" ->
                fv.TryAddToParentsArgList() 
            | "theorem" 
            | "lemma"  
            | "proposition"  
            | "corollary"  
            | "predicate definition"  
            | "conjecture"  
            | "axiom"  
            | "rule of inference"
            | "proof"
            | "class definition"
            | "localization"
            | "constructor"
            | "predicate property"
            | "optional predicate property"
            | "functional term property"
            | "optional functional term property"
            | "extension definition"
            | "argument"
            | "language"
            | "functional term definition" ->
                fv.TryAddToParentsScope()
            | "conjunction" 
            | "disjunction"
            | "exclusive disjunction" 
            | "negation"
            | "implication" 
            | "equivalence" 
            | "is operator" 
            | "equality" 
            | "extension object" 
            | "decrement"
            | "reference" ->
                match next.Name with 
                | "theorem"
                | "lemma"   
                | "proposition"   
                | "corollary" 
                | "conjecture"  
                | "predicate definition"  
                | "axiom"   
                | "rule of inference" 
                | "argument"
                | "proof"
                | "functional term definition"
                | "class definition" 
                | "constructor"
                | "functional term property"
                | "optional functional term property"
                | "predicate property"
                | "optional predicate property" ->
                    fv.TryAddToParentsArgList() 
                | "localization" -> 
                    next.FplId <- fv.FplId
                    next.TypeId <- fv.TypeId
                    next.EndPos <- fv.EndPos
                | "justification" -> 
                    fv.TryAddToParentsScope()
                | "quantor" ->
                    fv.TryAddToParentsArgList() 
                    next.EndPos <- fv.EndPos
                | _ -> 
                    if next.Scope.ContainsKey(".") then 
                        ()
                    else
                        fv.TryAddToParentsArgList()
                    next.EndPos <- fv.EndPos
            | "zero-or-more variable"
            | "one-or-more variable"
            | "variable" ->
                match next.Name with 
                | "theorem"
                | "lemma" 
                | "proposition"
                | "corollary" 
                | "conjecture"  
                | "predicate definition"   
                | "axiom"  
                | "rule of inference"
                | "constructor"
                | "proof"
                | "predicate property"
                | "optional predicate property"
                | "functional term property"
                | "optional functional term property"
                | "class definition"
                | "mapping" 
                | "extension definition" 
                | "zero-or-more variable"
                | "one-or-more variable"
                | "variable" 
                | "functional term definition"
                | "quantor"  
                | "localization" -> 
                    fv.TryAddToParentsScope()
                | "conjunction"
                | "disjunction" 
                | "exclusive disjunction" 
                | "negation" 
                | "implication" 
                | "equivalence" 
                | "is operator" 
                | "equality" 
                | "extension object"
                | "decrement" 
                | "reference" ->
                    fv.TryAddToParentsArgList()
                | _ -> ()
            | _ -> () 

    // Pushes an FplValue to the stack.
    member this.PushEvalStack fv = _valueStack.Push fv

    // Peeks an FplValue from the stack.
    member this.PeekEvalStack() = _valueStack.Peek()

    // Clears stack.
    member this.ClearEvalStack() = _valueStack.Clear()
