/// This module contains classes used as Singletons during the interpretation
/// by the FplInterpreter

(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)
module FplInterpreterGlobals
open System
open System.Collections.Generic
open System.IO
open FParsec
open FplPrimitives
open FplInterpreterUtils
open FplInterpreterBasicTypes

type State() = 
    let _vars = Dictionary<string,FplGenericNode option>()
    /// The dictionary of the variable values of the called node before it was called
    member this.VarValues = _vars

/// This type implements the functionality needed to "run" FPL statements step-by-step
/// while managing the storage of variables and other evaluation-related information.
/// FPL uses a call-by-value approach when it comes to 
/// replacing parameters by a calling function with arguments.
type FplVariableStack() = 
    let mutable _inSignatureEvaluation = false
    let mutable _inReferenceToProofOrCorollary = false
    let _stateStack = Stack<KeyValuePair<string, State>>()
    let _valueStack = Stack<FplGenericNode>()
    let _recursionCounters = Dictionary<string, int>()
    let _assumedArguments = Stack<FplGenericNode>()
    // positions of the caller to prevent some diagnostics of being shown at the wrong position 
    let mutable _callerStartPos = Position("", 0,0,0)
    let mutable _callerEndPos = Position("", 0,0,0)
    let mutable _language = "tex" // the default language is tex, otherwise, it should be set in the FPL IDE extension
    let mutable _recursionLevel = 0

    let mutable _nextRunOrder = 0

    /// Current language choice of all localizations
    member this.CurrentLanguage
        with get () = _language
        and set (value) = _language <- value

    /// Starting position of the caller
    member this.CallerStartPos
        with get () = _callerStartPos
        and set (value) = _callerStartPos <- value

    /// End position of the caller 
    member this.CallerEndPos
        with get () = _callerEndPos
        and set (value) = _callerEndPos <- value

    /// Recursion level
    member this.RecursionLevel = _recursionLevel

    /// Increment recursion level
    member this.RecursionInc() = 
        _recursionLevel <- _recursionLevel + 1

    /// Decrement recursion level
    member this.RecursionDec() = 
        _recursionLevel <- _recursionLevel - 1

    /// Returns the next available RunOrder to be stored, when inserting an FplValue into its parent.
    /// The need for this functionality is that sometimes, the block is inserted into the parent's scope, which is a dictionary.
    /// When running the nodes in the dictionary, their run order will ensure that they are being run in the order they have bin inserted.
    /// This order is incremented and stored when specific FplValue when they are created.
    /// All FplValues can have either Some or None RunOrder.
    /// Those with Some RunOrder include e.g. the following building blocks: axioms, theorems, lemmas, propositions, proofs, corollaries, arguments in proofs.
    /// Those with None include all other types of FplValues. They do not run by their own. They are "called" by those with Some RunOrder.
    member this.GetNextAvailableFplBlockRunOrder = 
        _nextRunOrder <- _nextRunOrder + 1
        _nextRunOrder
    
    /// Indicates if this EvalStack is evaluating a signature on a FPL building block
    member this.InSignatureEvaluation
        with get () = _inSignatureEvaluation
        and set (value) = _inSignatureEvaluation <- value
        
    /// Indicates if this EvalStack is evaluating a ReferenceToProofOrCorollary
    member this.InReferenceToProofOrCorollary
        with get () = _inReferenceToProofOrCorollary
        and set (value) = _inReferenceToProofOrCorollary <- value

    // The stack memory of the runner to store the variables of all run programs
    member this.Stack = _stateStack

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


    member this.EvalStack = _valueStack

    // Pops an FplValue from stack without propagating it's name and signature to the next FplValue on the stack.
    member this.Pop() = _valueStack.Pop()

    // Pops an FplValue from stack and propagates it's name and signature to the next FplValue on the stack.
    member this.PopEvalStack() = 
        let fv = _valueStack.Pop()
        if _valueStack.Count > 0 then
            let next = _valueStack.Peek()
            fv.EmbedInSymbolTable (Some next) 

    // Pushes an FplValue to the stack.
    member this.PushEvalStack fv = _valueStack.Push fv

    // Peeks an FplValue from the stack.
    member this.PeekEvalStack() = _valueStack.Peek()

    member this.LastAssumedArgument =
        if _assumedArguments.Count > 0 then
            Some (_assumedArguments.Peek())
        else 
            None

    member this.AssumeArgument assumption = _assumedArguments.Push (assumption)
    
    member this.RevokeLastArgument() = if _assumedArguments.Count > 0 then _assumedArguments.Pop() |> ignore

    // Clears stack.
    member this.ClearEvalStack() = 
        _valueStack.Clear()
        _assumedArguments.Clear()
        _stateStack.Clear()

let variableStack = FplVariableStack()

type Debug =
    | Start
    | Stop

let debug (fv:FplGenericNode) (debugMode:Debug) =
    let bars n = String.replicate n "| "
    let rec getPath (fv1:FplGenericNode) =
        match fv1.Parent with 
        | Some parent -> $"{getPath parent} # {fv1.ShortName} {fv1.Type SignatureType.Name}"
        | None -> $"{fv1.ShortName}"
    let vars =
        fv.GetVariables()
        |> List.map (fun var -> $"{var.FplId}={var.Represent()}")
        |> String.concat ", "
    if TestSharedConfig.TestConfig.DebugMode then 
        let indent = bars (variableStack.RecursionLevel)
        let logLine =
            match debugMode with
            | Debug.Start ->
                variableStack.RecursionInc()
                $"Start:{indent}{getPath fv}:[{fv.Represent()}][{vars}]{Environment.NewLine}"
            | Debug.Stop ->
                variableStack.RecursionDec()
                $"Stop :{indent.Substring(2)}{getPath fv}:[{fv.Represent()}][{vars}]{Environment.NewLine}"
        let currDir = Directory.GetCurrentDirectory()
        File.AppendAllText(Path.Combine(currDir, "Debug.txt"), logLine)

// Returns the root node of any FplValue
let rec root (fv:FplGenericNode) =
    if fv.Name = PrimRoot then 
        fv 
    else root fv.Parent.Value

