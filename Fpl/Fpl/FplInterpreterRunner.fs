module FplInterpreterRunner
open System.Collections.Generic
open FplInterpreterTypes
open FplInterpreterPredicateEvaluator

type FplRunner() =
    let _stack = Stack<KeyValuePair<string, Dictionary<string,FplValue>>>()

    // The stack memory of the runner collecting the variables of the current Runner
    member this.Stack = _stack

    // Saves the original scope variables of an FplValue block as a KeyValuePair to a stack memory.
    // where the key is the block's FplId and the value is a dictionary of all scope variables.
    // At the same time, the original scope variables are being replaced by their clones.
    member private this.SaveVariables(fvPar:FplValue, fvArgs:FplValue) = 
        // now process all scope variables and push by replacing them with their clones
        // and pushing the originals on the stack
        let toBeSavedScopeVariables = Dictionary<string, FplValue>()
        let mutable argIndex = 0
        fvPar.Scope
        |> Seq.filter (fun kvp -> kvp.Value.BlockType = FplValueType.Variable || 
                                  kvp.Value.BlockType = FplValueType.VariadicVariableMany || 
                                  kvp.Value.BlockType = FplValueType.VariadicVariableMany1) 
        |> Seq.iter (fun kvp -> 
            toBeSavedScopeVariables.Add(kvp.Key, kvp.Value)
            let clone = 
                if fvArgs.Scope.ContainsKey(kvp.Key) then
                    fvArgs.Scope[kvp.Key].Clone()
                elif kvp.Value.IsSignatureVariable then 
                    let candidateList = fvArgs.ValueList |> Seq.filter (fun fv -> fv.FplId = kvp.Key) |> Seq.toList 
                    if candidateList.IsEmpty then 
                        let candidate = kvp.Value.Clone()
                        if fvArgs.ValueList.Count>argIndex then 
                            candidate.ReprId <- fvArgs.ValueList[argIndex].Type(SignatureType.Repr)
                            argIndex <- argIndex + 1
                        candidate
                    else
                        candidateList.Head.Clone()
                else
                    kvp.Value.Clone()
            fvPar.Scope[kvp.Key] <- clone

        )
        let kvp = KeyValuePair(fvPar.FplId,toBeSavedScopeVariables)
        _stack.Push(kvp)
       
    // Restores the scope variables of an FplValue block from the stack.
    member private this.RestoreVariables(fvPar:FplValue) = 
        let blockVars = _stack.Pop()
        blockVars.Value
        |> Seq.iter (fun kvp -> 
            fvPar.Scope[kvp.Key] <- kvp.Value
        )

    member this.Run(fvArgs:FplValue) = 
        match fvArgs.BlockType with 
        | FplValueType.Reference ->
            if fvArgs.Scope.Count > 0 then 
                let called = fvArgs.Scope |> Seq.map (fun kvp -> kvp.Value) |> Seq.toList |> List.head
                match called.BlockType with 
                | FplValueType.Predicate -> 
                    this.SaveVariables(called, fvArgs)
                    called.ValueList
                    |> Seq.iter (fun fv -> 
                        this.Run(fv)
                    )
                    |> ignore
                    this.RestoreVariables(called)
                | _ -> ()
            else
                match fvArgs.FplId with 
                | "iif" -> evaluateEquivalence fvArgs
                | _ -> ()
        | _ -> ()
        