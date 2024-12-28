module FplInterpreterRunner
open System.Collections.Generic
open FplInterpreterTypes
open FplInterpreterPredicateEvaluator
open System

type FplRunner() =
    let _stack = Stack<KeyValuePair<string, Dictionary<string,FplValue>>>()

    // The stack memory of the runner collecting the variables of the current Runner
    member this.Stack = _stack

    // Saves the clones (!) of the original scope variables of an FplValue block as a KeyValuePair to a stack memory.
    // where the key is the block's FplId and the value is a dictionary of all scope variables.
    // At the same time, the original scope variables are being assigned to the values of .
    member private this.SaveVariables(called:FplValue, caller:FplValue) = 
        // now process all scope variables and push by replacing them with their clones
        // and pushing the originals on the stack
        let toBeSavedScopeVariables = Dictionary<string, FplValue>()
        let mutable argIndex = 0
        called.Scope
        |> Seq.filter (fun kvp -> kvp.Value.BlockType = FplValueType.Variable || 
                                  kvp.Value.BlockType = FplValueType.VariadicVariableMany || 
                                  kvp.Value.BlockType = FplValueType.VariadicVariableMany1) 
        |> Seq.iter (fun paramKvp -> 
            // save the clone of the original parameter variable
            let original = paramKvp.Value
            let clone = original.Clone()
            toBeSavedScopeVariables.Add(paramKvp.Key, clone)
            if paramKvp.Value.IsSignatureVariable then 
                // if the parameter is a signature variable
                if caller.ValueList.Count>argIndex then 
                    // replace the representation of the original parameter with the
                    // representation of the argument
                    let arg = caller.ValueList[argIndex]
                    original.ReprId <- arg.Type(SignatureType.Repr)
                    // increase the counter of pairs of arguments and singature variables 
                    argIndex <- argIndex + 1
        )
        let kvp = KeyValuePair(called.FplId,toBeSavedScopeVariables)
        _stack.Push(kvp)
       
    // Restores the scope variables of an FplValue block from the stack.
    member private this.RestoreVariables(fvPar:FplValue) = 
        let blockVars = _stack.Pop()
        blockVars.Value
        |> Seq.iter (fun kvp -> 
            fvPar.Scope[kvp.Key] <- kvp.Value
        )

    member this.Run(caller:FplValue) = 
        match caller.BlockType with 
        | FplValueType.Reference ->
            if caller.Scope.Count > 0 then 
                let called = caller.Scope |> Seq.map (fun kvp -> kvp.Value) |> Seq.toList |> List.head
                match called.BlockType with 
                | FplValueType.Predicate -> 
                    this.SaveVariables(called, caller)
                    let mutable lastRepr = ""
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
                | _ -> ()
        | _ -> ()
        