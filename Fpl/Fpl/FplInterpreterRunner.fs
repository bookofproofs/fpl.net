/// This module implements functionality needed to "run" FPL statements step-by-step
/// while managing the storage of variables. FPL uses a call-by-value approach when it comes to 
/// replacing parameters by a calling function with arguments.
module FplInterpreterRunner
open System.Collections.Generic
open FplInterpreterTypes
open FplInterpreterPredicateEvaluator
open FplInterpreterDiagnosticsEmitter

(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)

type FplRunner() =
    let _stack = Stack<KeyValuePair<string, Dictionary<string,FplValue>>>()

    // The stack memory of the runner to store the variables of all run programs
    member this.Stack = _stack


    member private this.ReplaceVariables (parameters:FplValue list) (arguments:FplValue list) =
        /// Copy the ValueList of the variadic ar to the ValueList of the variadic p
        /// by removing the previous values (if any) and
        /// inserting the clones of the elements.
        let replaceValues (p:FplValue) (ar:FplValue) =
            p.ValueList.Clear()
            ar.ValueList 
            |> Seq.iter (fun fv -> 
                let fvClone = fv.Clone() 
                p.ValueList.Add(fvClone)
            )

        let rec replace (pars:FplValue list) (args:FplValue list) = 
            match (pars, args) with
            | (p::ps, ar::ars) -> 
                match (p.FplBlockType, ar.FplBlockType) with
                // p is variadic, ar is variadic 
                | (FplBlockType.VariadicVariableMany, FplBlockType.VariadicVariableMany)
                | (FplBlockType.VariadicVariableMany, FplBlockType.VariadicVariableMany1)
                | (FplBlockType.VariadicVariableMany1, FplBlockType.VariadicVariableMany)
                | (FplBlockType.VariadicVariableMany1, FplBlockType.VariadicVariableMany1) ->
                    replaceValues p ar
                    // continue replacing variables with the remaining lists
                    replace ps ars
                // p is variadic, ar is variable
                | (FplBlockType.VariadicVariableMany, FplBlockType.Variable)
                | (FplBlockType.VariadicVariableMany1, FplBlockType.Variable) ->
                    replaceValues p ar              
                    // continue replacing variables with the original pars and the remaining ars list
                    replace pars ars
                | (FplBlockType.Variable, FplBlockType.Variable) -> 
                    // otherwise, simply assign the argument's representation to the parameter's representation
                    replaceValues p ar
                    // continue replacing variables with the remaining lists
                    replace ps ars
                | _ -> ()
            | (p::ps, []) -> ()
            | ([], ar::ars) -> ()
            | ([], []) -> ()
        replace parameters arguments

    /// Saves the clones (!) of the original scope variables of an FplValue block as a KeyValuePair to a stack memory.
    /// where the key is the block's FplId and the value is a dictionary of all scope variables.
    /// Returns a list of parameters of the called FplValue, i.e. its signature variables.
    /// Since the block's FplId is unique in the scope, all variables are stored in a separate scope.
    member private this.SaveVariables (called:FplValue) = 
        // now process all scope variables and push by replacing them with their clones
        // and pushing the originals on the stack
        let toBeSavedScopeVariables = Dictionary<string, FplValue>()
        let pars = List<FplValue>()
        called.Scope
        |> Seq.filter (fun kvp -> kvp.Value.FplBlockType = FplBlockType.Variable || 
                                  kvp.Value.FplBlockType = FplBlockType.VariadicVariableMany || 
                                  kvp.Value.FplBlockType = FplBlockType.VariadicVariableMany1) 
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
       
    member private this.SaveResult (caller:FplValue) (result:FplValue) = 
        let toBeSavedResult = Dictionary<string, FplValue>()
        toBeSavedResult.Add("result", result)
        let kvp = KeyValuePair(caller.FplId,toBeSavedResult)
        _stack.Push(kvp)

    member this.TryGetResult (caller:FplValue) =
        if _stack.Count = 0 then
            None
        else
            let head = _stack.Peek()
            if head.Key = caller.FplId && head.Value.ContainsKey("result") then
                let popped = _stack.Pop()
                Some popped.Value["result"]
            else
                None

    /// Restores the scope variables of an FplValue block from the stack.
    member private this.RestoreVariables(fvPar:FplValue) = 
        let blockVars = _stack.Pop()
        blockVars.Value
        |> Seq.iter (fun kvp -> 
            let orig = fvPar.Scope[kvp.Key] 
            orig.Copy(kvp.Value)
        )

    member this.Run (rootCaller:FplValue) (caller:FplValue) = 
        match caller.FplBlockType with 
        | FplBlockType.Reference ->
            if caller.Scope.Count > 0 then 
                let called = 
                    caller.Scope 
                    |> Seq.map (fun kvp -> kvp.Value) 
                    |> Seq.toList 
                    |> List.head
                match called.FplBlockType with 
                | FplBlockType.Predicate -> 
                    let pars = this.SaveVariables(called)
                    let args = caller.ArgList |> Seq.toList
                    this.ReplaceVariables pars args
                    let lastRepr = FplValue.CreateRoot()
                    // run all statements of the called node
                    called.ArgList
                    |> Seq.iter (fun fv -> 
                        this.Run caller fv 
                        lastRepr.ValueList.Clear()
                        lastRepr.ValueList.AddRange(fv.ValueList)
                    )
                    |> ignore
                    caller.ValueList.Clear()
                    caller.ValueList.AddRange(lastRepr.ValueList)
                    this.RestoreVariables(called)
                | FplBlockType.Class ->
                    let inst = called.CreateInstance()
                    this.SaveResult rootCaller inst

                | _ -> ()
            else
                match caller.FplId with 
                | "iif" -> evaluateEquivalence caller
                | "impl" -> evaluateImplication caller
                | "not" -> evaluateNegation caller
                | "and" -> evaluateConjunction caller
                | "xor" -> evaluateExclusiveOr caller
                | "or" ->  evaluateDisjunction caller
                | _ when caller.FplId.StartsWith("del.") ->
                    emitID013Diagnostics caller rootCaller.StartPos rootCaller.EndPos |> ignore
                | _ -> ()
        | _ -> ()
        