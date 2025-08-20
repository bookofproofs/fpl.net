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


    /// Copy the ValueList of the variadic ar to the ValueList of the variadic p
    /// by removing the previous values (if any) and
    /// inserting the clones of the elements.
    member private this.ReplaceVariables (parameters:FplVariable list) (arguments:FplValue list) =
        let replaceValues (p: FplVariable) (ar: FplValue) =
            p.ValueList.Clear()
            ar.ValueList
            |> Seq.iter (fun fv ->
                let fvClone = fv.Clone()
                p.ValueList.Add(fvClone)
            )

        let rec replace (pars:FplVariable list) (args: FplValue list) = 
            match (pars, args) with
            | (p::ps, ar::ars) ->
                match p.IsVariadic, ar with
                // p is variadic, ar is variadic 
                | true, (:? FplVariable as arVar) when arVar.IsVariadic ->
                    replaceValues p ar
                    // continue replacing variables with the remaining lists
                    replace ps ars
                // p is variadic, ar is anything
                | true, _ ->
                    replaceValues p ar              
                    // continue replacing variables with the original pars and the remaining ars list
                    replace pars ars
                // p is not variadic, ar is variadic 
                | false, (:? FplVariable as arVar) when arVar.IsVariadic -> ()
                 // p is not variadic, ar is anything but variadic 
                | false, _ ->
                    // otherwise, simply assign the argument's representation to the parameter's representation
                    replaceValues p ar
                    // continue replacing variables with the remaining lists
                    replace ps ars
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
        |> Seq.filter (fun kvp -> kvp.Value.IsVariable()) 
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

    /// Restores the scope variables of an FplValue block from the stack.
    member private this.RestoreVariables(fvPar:FplValue) = 
        let blockVars = _stack.Pop()
        blockVars.Value
        |> Seq.iter (fun kvp -> 
            let orig = fvPar.Scope[kvp.Key] 
            orig.Copy(kvp.Value)
        )

    member this.Run (rootCaller:FplValue) (caller:FplValue) = 
        match caller with 
        | :? FplConjunction
        | :? FplExclusiveOr 
        | :? FplDisjunction 
        | :? FplNegation 
        | :? FplImplication 
        | :? FplEquivalence 
        | :? FplReference ->
            if caller.Scope.Count > 0 then 
                let called = 
                    caller.Scope 
                    |> Seq.map (fun kvp -> kvp.Value) 
                    |> Seq.toList 
                    |> List.head
                match called with 
                | :? FplPredicate -> 
                    let pars = this.SaveVariables(called) |> List.map (fun v -> v :?> FplVariable)
                    let args = caller.ArgList |> Seq.toList
                    this.ReplaceVariables pars args
                    let lastRepr = new FplRoot()
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
                | _ -> ()
            else
                match caller.FplId with 
                | FplGrammarCommons.literalIif -> caller.Run()
                | FplGrammarCommons.literalImpl -> caller.Run()
                | FplGrammarCommons.literalNot -> caller.Run()
                | FplGrammarCommons.literalAnd -> caller.Run()
                | FplGrammarCommons.literalXor -> caller.Run()
                | FplGrammarCommons.literalOr ->  caller.Run()
                | _ when caller.FplId.StartsWith("del.") ->
                    emitID013Diagnostics caller rootCaller.StartPos rootCaller.EndPos |> ignore
                | _ -> ()
        | _ -> ()
        