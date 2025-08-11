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
open FplInterpreterDiagnosticsEmitter

type EvalStack() = 
    let _valueStack = Stack<FplValue>()
    let mutable _inSignatureEvaluation = false
    let mutable _classCounters = Dictionary<string,FplValue option>()

    /// Indicates if this EvalStack is evaluating a signature on a FPL building block
    member this.InSignatureEvaluation
        with get () = _inSignatureEvaluation
        and set (value) = _inSignatureEvaluation <- value

    /// In the context of a class being evaluated, this dictionary provides a dictionary
    /// of potential calls of parent classes (=Key). The optional FplValue values become some values
    /// if the a particular call was found. 
    /// This dictionary is used to emit ID020/ID021 diagnostics. If a class A inherits from class B but doesn't call its base constructor
    /// ID020 diagnostics will be emitted. If a class A inherits from class B and calls its base constructor more than once
    /// ID021 diagnostics will be emitted.
    member this.ParentClassCalls = _classCounters
      
    /// Resets the counters of th ID020 diagnostics evaluation.
    member this.ParentClassCountersInitialize() = 
        _classCounters |> Seq.iter (fun kvp -> _classCounters[kvp.Key] <- None)

    /// Adds the FplValue to it's parent's Scope.
    static member tryAddToScope (fv:FplValue) = 
        let next = fv.Parent.Value
        let identifier = 
            match fv.FplBlockType with
            |  FplBlockType.Constructor -> 
                fv.Type(SignatureType.Mixed)
            | _ -> 
                if fv.IsBlock() then 
                    fv.Type(SignatureType.Mixed)
                elif fv.IsVariable() then 
                    fv.FplId
                else
                    fv.Type(SignatureType.Name)
        match inScopeOfParent fv identifier with
        | ScopeSearchResult.Found conflict -> 
            match next.FplBlockType with
            | FplBlockType.Justification -> 
                emitPR004Diagnostics fv conflict 
            | _ -> 
                match fv.FplBlockType with
                | FplBlockType.Language -> 
                    let oldDiagnosticsStopped = ad.DiagnosticsStopped
                    ad.DiagnosticsStopped <- false
                    emitID014diagnostics fv conflict 
                    ad.DiagnosticsStopped <- oldDiagnosticsStopped
                | FplBlockType.Argument -> 
                    emitPR003diagnostics fv conflict 
                | FplBlockType.Variable -> 
                    ()
                | _ ->
                    emitID001diagnostics fv conflict 
        | _ -> 
            next.Scope.Add(identifier,fv)

    /// adds the FplValue to it's parent's ArgList
    static member tryAddToArgList (fv:FplValue) = 
        let next = fv.Parent.Value
        next.ArgList.Add(fv)

    // Pops an FplValue from stack without propagating it's name and signature to the next FplValue on the stack.
    member this.Pop() = _valueStack.Pop()

    // Pops an FplValue from stack and propagates it's name and signature to the next FplValue on the stack.
    member this.PopEvalStack() = 
        let fv = _valueStack.Pop()
        if _valueStack.Count > 0 then
            let next = _valueStack.Peek()

            match fv.FplBlockType with
            | FplBlockType.Proof 
            | FplBlockType.Corollary ->
                EvalStack.tryAddToScope fv
            | FplBlockType.Class 
            | FplBlockType.Theorem
            | FplBlockType.Localization
            | FplBlockType.Lemma
            | FplBlockType.Proposition
            | FplBlockType.Conjecture
            | FplBlockType.RuleOfInference
            | FplBlockType.Constructor
            | FplBlockType.MandatoryPredicate
            | FplBlockType.OptionalPredicate
            | FplBlockType.MandatoryFunctionalTerm
            | FplBlockType.OptionalFunctionalTerm
            | FplBlockType.Axiom
            | FplBlockType.Predicate
            | FplBlockType.Extension
            | FplBlockType.Argument 
            | FplBlockType.Language 
            | FplBlockType.FunctionalTerm ->
                EvalStack.tryAddToScope fv
            | FplBlockType.Reference ->
                match next.FplBlockType with
                | FplBlockType.Localization -> 
                    next.FplId <- fv.FplId
                    next.TypeId <- fv.TypeId
                    next.EndPos <- fv.EndPos
                | FplBlockType.Justification -> 
                    EvalStack.tryAddToScope fv
                | FplBlockType.Argument ->
                    EvalStack.tryAddToArgList fv 
                | FplBlockType.Axiom
                | FplBlockType.Theorem 
                | FplBlockType.Lemma 
                | FplBlockType.Proposition 
                | FplBlockType.Corollary 
                | FplBlockType.Conjecture 
                | FplBlockType.Proof 
                | FplBlockType.RuleOfInference 
                | FplBlockType.Predicate 
                | FplBlockType.FunctionalTerm 
                | FplBlockType.Class 
                | FplBlockType.Constructor
                | FplBlockType.MandatoryFunctionalTerm
                | FplBlockType.OptionalFunctionalTerm
                | FplBlockType.MandatoryPredicate
                | FplBlockType.OptionalPredicate ->
                    EvalStack.tryAddToArgList fv 
                | FplBlockType.Quantor ->
                    EvalStack.tryAddToArgList fv 
                    next.EndPos <- fv.EndPos
                | _ -> 
                    if next.Scope.ContainsKey(".") then 
                        ()
                    else
                        EvalStack.tryAddToArgList fv
                    next.EndPos <- fv.EndPos
            | FplBlockType.Variable
            | FplBlockType.VariadicVariableMany
            | FplBlockType.VariadicVariableMany1 ->
                match next.FplBlockType with 
                | FplBlockType.Theorem
                | FplBlockType.Lemma
                | FplBlockType.Proposition
                | FplBlockType.Conjecture
                | FplBlockType.RuleOfInference
                | FplBlockType.Constructor
                | FplBlockType.Corollary
                | FplBlockType.Proof
                | FplBlockType.MandatoryPredicate
                | FplBlockType.OptionalPredicate
                | FplBlockType.MandatoryFunctionalTerm
                | FplBlockType.OptionalFunctionalTerm
                | FplBlockType.Axiom
                | FplBlockType.Predicate
                | FplBlockType.Class
                | FplBlockType.Mapping 
                | FplBlockType.Extension 
                | FplBlockType.Variable 
                | FplBlockType.VariadicVariableMany
                | FplBlockType.VariadicVariableMany1 
                | FplBlockType.FunctionalTerm ->
                    EvalStack.tryAddToScope fv
                | FplBlockType.Quantor  
                | FplBlockType.Localization -> 
                    EvalStack.tryAddToScope fv
                | FplBlockType.Reference ->
                    EvalStack.tryAddToArgList fv
                | _ -> ()
            | FplBlockType.IntrinsicObj
            | FplBlockType.Quantor
            | FplBlockType.Theory
            | FplBlockType.Justification 
            | FplBlockType.ArgInference 
            | FplBlockType.Mapping 
            | FplBlockType.Translation 
            | FplBlockType.Stmt
            | FplBlockType.Assertion
            | FplBlockType.IntrinsicInd
            | FplBlockType.IntrinsicPred
            | FplBlockType.IntrinsicFunc
            | FplBlockType.IntrinsicObj
            | FplBlockType.IntrinsicUndef
            | FplBlockType.IntrinsicTpl
            | FplBlockType.Instance
            | FplBlockType.Root -> 
                EvalStack.tryAddToArgList fv 


    // Pushes an FplValue to the stack.
    member this.PushEvalStack fv = _valueStack.Push fv

    // Peeks an FplValue from the stack.
    member this.PeekEvalStack() = _valueStack.Peek()

    // Clears stack.
    member this.ClearEvalStack() = _valueStack.Clear()
