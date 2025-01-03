module FplInterpreterRunner
open System.Collections.Generic
open FplInterpreterTypes
open FplInterpreterPredicateEvaluator
open FplInterpreterDiagnosticsEmitter
open System

type FplRunner() =
    let _stack = Stack<KeyValuePair<string, Dictionary<string,FplValue>>>()

    // The stack memory of the runner collecting the variables of the current Runner
    member this.Stack = _stack


    member private this.ReplaceVariables(parameters:FplValue list) (arguments:FplValue list) =
        let rec replace (pars:FplValue list) (args:FplValue list) = 
            match (pars, args) with
            | (p::ps, ar::ars) -> 
                match (p.BlockType, ar.BlockType) with
                // p is variadic, ar is variadic 
                | (FplValueType.VariadicVariableMany,FplValueType.VariadicVariableMany)
                | (FplValueType.VariadicVariableMany,FplValueType.VariadicVariableMany1)
                | (FplValueType.VariadicVariableMany1,FplValueType.VariadicVariableMany)
                | (FplValueType.VariadicVariableMany1,FplValueType.VariadicVariableMany1) ->
                    // copy the value list of the variadic ar to the value list of the variadic p
                    // by inserting the clones of the elements
                    ar.ValueList 
                    |> Seq.iter (fun fv -> 
                        let fvClone = fv.Clone() 
                        p.ValueList.Add(fvClone)
                    )
                    // continue replacling variables with the remaining lists
                    replace ps ars
                // p is variadic, ar is not variadic 
                | (FplValueType.VariadicVariableMany,_)
                | (FplValueType.VariadicVariableMany1,_) -> 
                    let arClone = ar.Clone()
                    p.ValueList.Add(arClone)
                    // continue replacling variables with the original pars and the remaining ars list
                    replace pars ars
                | _ -> 
                    // otherwise, simply assign the argument's representation to the parameter's representation
                    p.ReprId <- ar.Type(SignatureType.Repr)
                    // continue replacling variables with the remaining lists
                    replace ps ars
            | (p::ps, []) -> ()
            | ([], ar::ars) -> ()
            | ([], []) -> ()
        replace parameters arguments

    // Saves the clones (!) of the original scope variables of an FplValue block as a KeyValuePair to a stack memory.
    // where the key is the block's FplId and the value is a dictionary of all scope variables.
    // Returns a list of parameters of the called FplValue, i.e. its Signature Variables
    member private this.SaveVariables(called:FplValue) = 
        // now process all scope variables and push by replacing them with their clones
        // and pushing the originals on the stack
        let toBeSavedScopeVariables = Dictionary<string, FplValue>()
        let pars = List<FplValue>()
        called.Scope
        |> Seq.filter (fun kvp -> kvp.Value.BlockType = FplValueType.Variable || 
                                  kvp.Value.BlockType = FplValueType.VariadicVariableMany || 
                                  kvp.Value.BlockType = FplValueType.VariadicVariableMany1) 
        |> Seq.iter (fun paramKvp -> 
            // save the clone of the original parameter variable
            let parOriginal = paramKvp.Value
            let parClone = parOriginal.Clone()
            toBeSavedScopeVariables.Add(paramKvp.Key, parClone)
            if paramKvp.Value.IsSignatureVariable then 
                pars.Add(parOriginal)
        )
        let kvp = KeyValuePair(called.FplId,toBeSavedScopeVariables)
        _stack.Push(kvp)
        pars |> Seq.toList
       
    // Restores the scope variables of an FplValue block from the stack.
    member private this.RestoreVariables(fvPar:FplValue) = 
        let blockVars = _stack.Pop()
        blockVars.Value
        |> Seq.iter (fun kvp -> 
            let orig = fvPar.Scope[kvp.Key] 
            orig.Copy(kvp.Value)
        )

    member this.Run(caller:FplValue) = 
        match caller.BlockType with 
        | FplValueType.Reference ->
            if caller.Scope.Count > 0 then 
                let called = 
                    caller.Scope 
                    |> Seq.map (fun kvp -> kvp.Value) 
                    |> Seq.toList 
                    |> List.head
                match called.BlockType with 
                | FplValueType.Predicate -> 
                    let pars = this.SaveVariables(called)
                    let args = caller.ValueList |> Seq.toList
                    this.ReplaceVariables pars args
                    let mutable lastRepr = ""
                    // run all statements of the called node
                    called.ValueList
                    |> Seq.iter (fun fv -> 
                        this.Run(fv)
                        lastRepr <- fv.ReprId
                    )
                    |> ignore
                    caller.ReprId <- lastRepr
                    this.RestoreVariables(called)
                | _ -> ()
            else
                match caller.FplId with 
                | "iif" -> evaluateEquivalence caller
                | "impl" ->  evaluateImplication caller
                | "not" ->  evaluateNegation caller
                | "and" ->  evaluateConjunction caller
                | "xor" ->  evaluateExclusiveOr caller
                | "or" ->  evaluateDisjunction caller
                | _ when caller.FplId.StartsWith("del.") ->
                    emitID013Diagnostics caller (caller.NameStartPos) (caller.NameEndPos) |> ignore
                | _ -> ()
        | _ -> ()
        