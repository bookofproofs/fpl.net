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
            match fv with
            | :? FplConstructor -> 
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
            match next with
            | :? FplJustification -> 
                emitPR004Diagnostics fv conflict 
            | _ -> 
                match fv with
                | :? FplLanguage -> 
                    let oldDiagnosticsStopped = ad.DiagnosticsStopped
                    ad.DiagnosticsStopped <- false
                    emitID014diagnostics fv conflict 
                    ad.DiagnosticsStopped <- oldDiagnosticsStopped
                | :? FplArgument -> 
                    emitPR003diagnostics fv conflict 
                | :? FplVariable -> 
                    ()
                | _ ->
                    emitID001diagnostics fv conflict 
        | _ -> 
            next.Scope.Add(identifier,fv)

    // Pops an FplValue from stack without propagating it's name and signature to the next FplValue on the stack.
    member this.Pop() = _valueStack.Pop()

    // Pops an FplValue from stack and propagates it's name and signature to the next FplValue on the stack.
    member this.PopEvalStack() = 
        let fv = _valueStack.Pop()
        if _valueStack.Count > 0 then
            let next = _valueStack.Peek()

            match fv with 
            | :? FplIntrinsicObj
            | :? FplQuantor
            | :? FplJustification 
            | :? FplArgInference 
            | :? FplMapping 
            | :? FplTranslation 
            | :? FplStmt
            | :? FplAssertion
            | :? FplIntrinsicPred
            | :? FplIntrinsicFunc
            | :? FplIntrinsicObj
            | :? FplIntrinsicUndef
            | :? FplIntrinsicTpl
            | :? FplInstance 
            | :? FplTheory 
            | :? FplIntrinsicInd ->
                fv.TryAddToParentsArgList() 
            | :? FplTheorem  
            | :? FplLemma  
            | :? FplProposition  
            | :? FplCorollary  
            | :? FplPredicate  
            | :? FplConjecture  
            | :? FplAxiom  
            | :? FplRuleOfInference
            | :? FplProof 
            | :? FplClass 
            | :? FplLocalization
            | :? FplConstructor
            | :? FplMandatoryPredicate
            | :? FplOptionalPredicate
            | :? FplMandatoryFunctionalTerm
            | :? FplOptionalFunctionalTerm
            | :? FplExtension
            | :? FplArgument 
            | :? FplLanguage 
            | :? FplFunctionalTerm ->
                EvalStack.tryAddToScope fv
            | :? FplConjunction 
            | :? FplDisjunction 
            | :? FplExclusiveOr 
            | :? FplNegation 
            | :? FplImplication 
            | :? FplEquivalence 
            | :? FplReference ->
                match next with 
                | :? FplTheorem  
                | :? FplLemma  
                | :? FplProposition  
                | :? FplCorollary  
                | :? FplConjecture  
                | :? FplPredicate  
                | :? FplAxiom  
                | :? FplRuleOfInference 
                | :? FplArgument 
                | :? FplProof 
                | :? FplFunctionalTerm 
                | :? FplClass 
                | :? FplConstructor
                | :? FplMandatoryFunctionalTerm
                | :? FplOptionalFunctionalTerm
                | :? FplMandatoryPredicate
                | :? FplOptionalPredicate ->
                    fv.TryAddToParentsArgList() 
                | :? FplLocalization -> 
                    next.FplId <- fv.FplId
                    next.TypeId <- fv.TypeId
                    next.EndPos <- fv.EndPos
                | :? FplJustification -> 
                    EvalStack.tryAddToScope fv
                | :? FplQuantor ->
                    fv.TryAddToParentsArgList() 
                    next.EndPos <- fv.EndPos
                | _ -> 
                    if next.Scope.ContainsKey(".") then 
                        ()
                    else
                        fv.TryAddToParentsArgList()
                    next.EndPos <- fv.EndPos
            | :? FplVariable ->
                match next with 
                | :? FplTheorem 
                | :? FplLemma 
                | :? FplProposition 
                | :? FplCorollary
                | :? FplConjecture
                | :? FplPredicate 
                | :? FplAxiom 
                | :? FplRuleOfInference 
                | :? FplConstructor
                | :? FplProof
                | :? FplMandatoryPredicate
                | :? FplOptionalPredicate
                | :? FplMandatoryFunctionalTerm
                | :? FplOptionalFunctionalTerm
                | :? FplClass
                | :? FplMapping 
                | :? FplExtension 
                | :? FplVariable 
                | :? FplFunctionalTerm 
                | :? FplQuantor  
                | :? FplLocalization -> 
                    EvalStack.tryAddToScope fv
                | :? FplConjunction
                | :? FplDisjunction 
                | :? FplExclusiveOr 
                | :? FplNegation 
                | :? FplImplication 
                | :? FplEquivalence 
                | :? FplReference ->
                    fv.TryAddToParentsArgList()
                | _ -> ()
            | _ -> () 

    // Pushes an FplValue to the stack.
    member this.PushEvalStack fv = _valueStack.Push fv

    // Peeks an FplValue from the stack.
    member this.PeekEvalStack() = _valueStack.Peek()

    // Clears stack.
    member this.ClearEvalStack() = _valueStack.Clear()
