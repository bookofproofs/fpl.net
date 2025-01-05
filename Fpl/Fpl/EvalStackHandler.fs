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
    /// of its parent classes (=Key) and integer values (=Value). The dictionary is used to 
    /// count if for all of these classes a base constructor call is contained in the body 
    /// of a specific class constructor. If it is not the case for alll of the parent classes,
    /// ID020 diagnostics will be emitted.
    member this.ParentClassCalls = _classCounters

      
    /// Resets the counters of th ID020 diagnostics evaluation.
    member this.ParentClassCountersInitialize() = 
        _classCounters |> Seq.iter (fun kvp -> _classCounters[kvp.Key] <- None)

    /// Adds the FplValue to it's parent's Scope.
    static member tryAddToScope (fv:FplValue) = 
        let next = fv.Parent.Value
        let identifier = 
            match fv.BlockType with
            |  FplValueType.Constructor -> 
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
            Some conflict 
            match next.BlockType with
            | FplValueType.Justification -> 
                emitPR004Diagnostics fv conflict 
            | _ -> 
                match fv.BlockType with
                | FplValueType.Language -> 
                    let oldDiagnosticsStopped = ad.DiagnosticsStopped
                    ad.DiagnosticsStopped <- false
                    emitID014diagnostics fv conflict 
                    ad.DiagnosticsStopped <- oldDiagnosticsStopped
                | FplValueType.Argument -> 
                    emitPR003diagnostics fv conflict 
                | FplValueType.Variable -> 
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

            match fv.BlockType with
            | FplValueType.Proof 
            | FplValueType.Corollary ->
                EvalStack.tryAddToScope fv
            | FplValueType.Class 
            | FplValueType.Theorem
            | FplValueType.Localization
            | FplValueType.Lemma
            | FplValueType.Proposition
            | FplValueType.Conjecture
            | FplValueType.RuleOfInference
            | FplValueType.Constructor
            | FplValueType.MandatoryPredicate
            | FplValueType.OptionalPredicate
            | FplValueType.MandatoryFunctionalTerm
            | FplValueType.OptionalFunctionalTerm
            | FplValueType.Axiom
            | FplValueType.Predicate
            | FplValueType.Extension
            | FplValueType.Argument 
            | FplValueType.Language 
            | FplValueType.FunctionalTerm ->
                EvalStack.tryAddToScope fv
            | FplValueType.Reference ->
                match next.BlockType with
                | FplValueType.Localization -> 
                    next.FplId <- fv.FplId
                    next.TypeId <- fv.TypeId
                    next.NameEndPos <- fv.NameEndPos
                | FplValueType.Justification -> 
                    EvalStack.tryAddToScope fv
                | FplValueType.Argument ->
                    EvalStack.tryAddToValueList fv 
                | FplValueType.Axiom
                | FplValueType.Theorem 
                | FplValueType.Lemma 
                | FplValueType.Proposition 
                | FplValueType.Corollary 
                | FplValueType.Conjecture 
                | FplValueType.Proof 
                | FplValueType.RuleOfInference 
                | FplValueType.Predicate 
                | FplValueType.FunctionalTerm 
                | FplValueType.Class 
                | FplValueType.Constructor
                | FplValueType.MandatoryFunctionalTerm
                | FplValueType.OptionalFunctionalTerm
                | FplValueType.MandatoryPredicate
                | FplValueType.OptionalPredicate ->
                    EvalStack.tryAddToValueList fv 
                | FplValueType.Quantor ->
                    EvalStack.tryAddToValueList fv 
                    next.NameEndPos <- fv.NameEndPos
                | _ -> 
                    if next.Scope.ContainsKey(".") then 
                        ()
                    else
                        EvalStack.tryAddToValueList fv
                    next.NameEndPos <- fv.NameEndPos
            | FplValueType.Variable
            | FplValueType.VariadicVariableMany
            | FplValueType.VariadicVariableMany1 ->
                match next.BlockType with 
                | FplValueType.Theorem
                | FplValueType.Lemma
                | FplValueType.Proposition
                | FplValueType.Conjecture
                | FplValueType.RuleOfInference
                | FplValueType.Constructor
                | FplValueType.Corollary
                | FplValueType.Proof
                | FplValueType.MandatoryPredicate
                | FplValueType.OptionalPredicate
                | FplValueType.MandatoryFunctionalTerm
                | FplValueType.OptionalFunctionalTerm
                | FplValueType.Axiom
                | FplValueType.Predicate
                | FplValueType.Class
                | FplValueType.Mapping 
                | FplValueType.Variable 
                | FplValueType.VariadicVariableMany
                | FplValueType.VariadicVariableMany1 
                | FplValueType.FunctionalTerm ->
                    EvalStack.tryAddToScope fv
                | FplValueType.Quantor  
                | FplValueType.Localization -> 
                    EvalStack.tryAddToScope fv
                | FplValueType.Reference ->
                    EvalStack.tryAddToValueList fv
                | _ -> ()
            | FplValueType.Object
            | FplValueType.Quantor
            | FplValueType.Theory
            | FplValueType.Justification 
            | FplValueType.ArgInference 
            | FplValueType.Mapping 
            | FplValueType.Translation 
            | FplValueType.Stmt
            | FplValueType.Assertion
            | FplValueType.Root -> 
                EvalStack.tryAddToValueList fv 


    // Pushes an FplValue to the stack.
    member this.PushEvalStack fv = _valueStack.Push fv

    // Peeks an FplValue from the stack.
    member this.PeekEvalStack() = _valueStack.Peek()

    // Clears stack.
    member this.ClearEvalStack() = _valueStack.Clear()
