/// <summary>
/// This module contains types modeling the storage memory used to separate the
/// scope of called FPL nodes such as functions, constructors, predicates, etc.
/// </summary>

(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)

module Fpl.Interpreter.SymbolTable.Storage.RunState
open System.Collections.Generic
open Fpl.Primitives
open Fpl.Interpreter.BasicTypes

/// <summary>
/// Internal container storing clones of variable values for a single call frame.
/// </summary>
type private StateDict() = 
    let _vars = Dictionary<string,FplGenericNode option>()
    /// The dictionary of the variable values of the called node before it was called
    member this.VarValues = _vars

/// <summary>
/// Implements the runtime state stack used to "run" FPL statements step-by-step while
/// managing storage of variables and other evaluation-related information.
/// </summary>
/// <remarks>
/// FPL uses a call-by-value approach for parameter passing; this type saves and restores
/// caller variable states and assigns parameter values for called blocks.
/// </remarks>
type State() =
    let _stateStack = Stack<KeyValuePair<string, StateDict>>()

    /// <summary>
    /// Copy the values from argument nodes into parameter signature variables,
    /// handling variadic parameters and arguments appropriately.
    /// </summary>
    /// <param name="parameters">List of signature parameters of the called block.</param>
    /// <param name="arguments">List of actual argument nodes supplied by the caller.</param>
    member this.ReplaceVariables (parameters:FplGenericHasValue list) (arguments:FplGenericNode list) =
        let replaceValues (p:FplGenericHasValue) (ar:FplGenericNode) =
            match ar with
            | :? FplGenericHasValue as arWithValue ->
                // set the parameter's value to the value of the argument, if argument with value
                match arWithValue.Value with 
                | Some v -> p.SetValue v
                | None -> p.Value <- None
            | _ -> 
                // set the parameter's value to the argument itself, since it is a non-valued argument
                p.SetValue ar 

        let rec replace (pars:FplGenericHasValue list) (args: FplGenericNode list) = 
            match (pars, args) with
            | (p::ps, ar::ars) ->
                match p.Name , ar.Name with
                // p is variadic, ar is variadic 
                | PrimVariableArrayL, PrimVariableArrayL ->
                    replaceValues p ar
                    // continue replacing variables with the remaining lists
                    replace ps ars
                // p is variadic, ar is anything
                | PrimVariableArrayL, _ ->
                    replaceValues p ar              
                    // continue replacing variables with the original pars and the remaining ars list
                    replace pars ars
                // p is not variadic, ar is variadic 
                | PrimVariableL, PrimVariableArrayL -> ()
                 // p is not variadic, ar is anything but variadic 
                | PrimVariableL, _ ->
                    // otherwise, simply assign the argument's representation to the parameter's representation
                    replaceValues p ar
                    // continue replacing variables with the remaining lists
                    replace ps ars
                | _ , _ -> ()
            | (p::ps, []) -> ()
            | ([], ar::ars) -> ()
            | ([], []) -> ()
        replace parameters arguments

    /// <summary>
    /// Saves clones of the original scope variables of the called block onto the internal stack.
    /// </summary>
    /// <param name="called">The called block whose original scope must be preserved.</param>
    /// <returns>
    /// A list of parameters (signature variables) of the called block to be used for parameter initialization.
    /// </returns>
    member this.SaveState (called:FplGenericHasValue) = 
        // now process all scope variables and push by replacing them with their clones
        // and pushing the originals on the stack
        let toBeSavedState = new StateDict()
        let pars = List<FplGenericHasValue>()
        let vars = called.GetVariables()
        vars 
        |> List.map (fun parOriginal -> parOriginal :?> FplGenericHasValue)
        |> List.iter (fun parOriginal -> 
            toBeSavedState.VarValues.Add(parOriginal.FplId, parOriginal.Value)
            match box parOriginal with 
            | :? IVariable as parOrig when parOrig.IsSignatureVariable ->
                pars.Add(parOriginal)
            | _ -> ()
        )
        let kvp = KeyValuePair(called.FplId,toBeSavedState)
        
        _stateStack.Push(kvp)
        pars |> Seq.toList

    /// <summary>
    /// Restores the variable values of a called block to the state they had before the call.
    /// </summary>
    /// <param name="called">The called block whose state should be restored.</param>
    /// <exceptions>
    /// <exception cref="System.Collections.Generic.KeyNotFoundException">
    /// Thrown if a previously saved variable key is not present in the called block's scope.
    /// </exception>
    /// </exceptions>
    /// Restores the state of a called FplValue block it had before it was called.
    member this.RestoreState (called:FplGenericHasValue) =
        // TODO: restore also the IsInitialized flag of the variables
        let stateBeforeBeingCalled = _stateStack.Pop().Value
        stateBeforeBeingCalled.VarValues
        |> Seq.iter (fun kvp -> 
            let origVariable = (called.Scope:Dictionary<string, FplGenericNode>)[kvp.Key]
            let origVariableWithValue = origVariable :?> FplGenericHasValue
            let oldValue = kvp.Value
            origVariableWithValue.Value <- oldValue
        )

    /// <summary>
    /// Clears the internal saved state stack.
    /// </summary>
    member this.Clear() = _stateStack.Clear()
