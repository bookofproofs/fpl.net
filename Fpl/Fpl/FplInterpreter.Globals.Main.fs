/// This module contains classes used as Singletons during the interpretation
/// by the FplInterpreter

(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)
module FplInterpreter.Globals.Main
open System.Collections.Generic

open FplPrimitives
open FplInterpreterBasicTypes
open FplInterpreter.Globals.Helpers
open FplInterpreter.Globals.STEval
open FplInterpreter.Globals.Validity


type State() = 
    let _vars = Dictionary<string,FplGenericNode option>()
    /// The dictionary of the variable values of the called node before it was called
    member this.VarValues = _vars


/// This type implements the functionality needed to "run" FPL statements step-by-step
/// while managing the storage of variables and other evaluation-related information.
/// FPL uses a call-by-value approach when it comes to 
/// replacing parameters by a calling function with arguments.
type FplVariableStack() = 
    let _stateStack = Stack<KeyValuePair<string, State>>()
    let _validStmtStore = ValidStmtStore()
    let _recursionCounters = Dictionary<string, int>()
    let _assumedArguments = Stack<FplGenericNode>()
    let _evalStack = EvalStack()
    let _helper = Helper()
    
    // A stack memory storing the variables of all runing symbol table nodes
    member this.Stack = _stateStack

    /// A stack memory storing potential new nodes of the symbol table during its evaluation process
    member this.Eval = _evalStack

    /// A handle for helper variables storing some context during the creation process of the symbol table
    member this.Helper = _helper

    /// Copy the ValueList of the variadic ar to the ValueList of the variadic p
    /// by removing the previous values (if any) and
    /// inserting the clones of the elements.
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

    /// Saves the clones (!) of the original scope variables of an FplValue block as a KeyValuePair to a stack memory.
    /// where the key is the block's FplId and the value is a dictionary of all scope variables.
    /// Returns a list of parameters of the called FplValue, i.e. its signature variables.
    /// Since the block's FplId is unique in the scope, all variables are stored in a separate scope.
    member this.SaveState (called:FplGenericHasValue) = 
        // now process all scope variables and push by replacing them with their clones
        // and pushing the originals on the stack
        let toBeSavedState = new State()
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

    member this.ValidStmtStore = _validStmtStore

    member this.LastAssumedArgument =
        if _assumedArguments.Count > 0 then
            Some (_assumedArguments.Peek())
        else 
            None

    member this.AssumeArgument assumption = _assumedArguments.Push (assumption)
    
    member this.RevokeLastArgument() = if _assumedArguments.Count > 0 then _assumedArguments.Pop() |> ignore

    // Clears the globals.
    member this.ClearEvalStack() = 
        _evalStack.Clear()
        _assumedArguments.Clear()
        _stateStack.Clear()

let variableStack = FplVariableStack()



