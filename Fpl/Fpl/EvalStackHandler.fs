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
                if FplValue.IsBlock(fv) then 
                    fv.Type(SignatureType.Mixed)
                elif FplValue.IsVariable(fv) then 
                    fv.FplId
                else
                    fv.Type(SignatureType.Name)
        match FplValue.InScopeOfParent(fv) identifier with
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

    /// adds the FplValue to it's parent's ValueList
    static member tryAddToValueList (fv:FplValue) = 
        let next = fv.Parent.Value
        next.ValueList.Add(fv)

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
                    EvalStack.tryAddToValueList fv 
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
                    EvalStack.tryAddToValueList fv 
                | FplBlockType.Quantor ->
                    EvalStack.tryAddToValueList fv 
                    next.EndPos <- fv.EndPos
                | _ -> 
                    if next.Scope.ContainsKey(".") then 
                        ()
                    else
                        EvalStack.tryAddToValueList fv
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
                    EvalStack.tryAddToValueList fv
                | _ -> ()
            | FplBlockType.Object
            | FplBlockType.Quantor
            | FplBlockType.Theory
            | FplBlockType.Justification 
            | FplBlockType.ArgInference 
            | FplBlockType.Mapping 
            | FplBlockType.Translation 
            | FplBlockType.Stmt
            | FplBlockType.Assertion
            | FplBlockType.Index
            | FplBlockType.Instance
            | FplBlockType.Root -> 
                EvalStack.tryAddToValueList fv 


    // Pushes an FplValue to the stack.
    member this.PushEvalStack fv = _valueStack.Push fv

    // Peeks an FplValue from the stack.
    member this.PeekEvalStack() = _valueStack.Peek()

    // Clears stack.
    member this.ClearEvalStack() = _valueStack.Clear()
