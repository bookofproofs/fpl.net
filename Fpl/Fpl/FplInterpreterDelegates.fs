/// This module contains all types used by the FplInterpreter
/// to model / interpret delagates, including equality 

(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 
*)
module FplInterpreterDelegates
open FplPrimitives
open FplGrammarTypes
open FplInterpreterDiagnosticsEmitter
open FplInterpreterBasicTypes
open FplInterpreter.Globals.Main
open FplInterpreterChecks
open FplInterpreterSTEmbedding
open FplInterpreterReferences
open FplInterpreterIntrinsicTypes
open FplInterpreterVariables
open FplInterpreterExtensions

[<AbstractClass>]
type FplGenericDelegate(name, positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericHasValue(positions, parent)

    do 
        this.FplId <- name

    override this.RunOrder = None

/// Implements the semantics of an FPL equality.
type FplEquality(name, positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericDelegate(name, positions, parent)

    do 
        this.FplId <- $"{LiteralDel}{PrimDelegateEqual}"
        this.TypeId <- LiteralPred

    override this.Name = PrimDelegateEqualL
    override this.ShortName = PrimDelegateEqual

    override this.Clone () =
        let ret = new FplEquality(this.FplId, (this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Copy(other) =
        base.Copy(other)
        this.TypeId <- LiteralPred

    override this.Type signatureType = 
        let head = getFplHead this signatureType
        let propagate = propagateSignatureType signatureType
        let args = signatureSep ", " this.ArgList propagate
        sprintf "%s(%s)" head args

    override this.CheckConsistency (): unit = 
        if this.ArgList.Count <> 2 then 
            this.ErrorOccurred <- emitID013Diagnostics $"Predicate `=` takes 2 arguments, got {this.ArgList.Count}." variableStack.CallerStartPos variableStack.CallerEndPos 
        base.CheckConsistency()
    
    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        addExpressionToReference this

    override this.Run() = 
        debug this Debug.Start
        match this.ErrorOccurred with 
        | Some err ->
            this.SetDefaultValue()
        | _ ->
            if isInQuantor this then 
                this.SetDefaultValue()
            else

                let a = this.ArgList[0]
                let b = this.ArgList[1]
                let aType = a.Type SignatureType.Type
                let bType = b.Type SignatureType.Type
                let aRepr = a.Represent()
                let bRepr = b.Represent()

                let newPred = new FplIntrinsicPred((variableStack.CallerStartPos, variableStack.CallerEndPos), this.Parent.Value)
                match aRepr with
                | LiteralUndef -> 
                    this.ErrorOccurred <- emitID013Diagnostics "Predicate `=` cannot be evaluated because the left argument is undefined." variableStack.CallerStartPos variableStack.CallerEndPos 
                    this.SetDefaultValue()
                | _ -> 
                    match bRepr with
                    | LiteralUndef -> 
                        this.ErrorOccurred <- emitID013Diagnostics "Predicate `=` cannot be evaluated because the right argument is undefined." variableStack.CallerStartPos variableStack.CallerEndPos 
                        this.SetDefaultValue()
                    | _ when aType<>bType -> 
                        newPred.FplId <- LiteralFalse // if the compared arguments have different types, then unequal
                        this.SetValue newPred
                    | _ when aType = "tpl" && bType = "tpl" && aRepr = PrimUndetermined && bRepr = PrimUndetermined -> 
                        this.SetDefaultValue()
                    | _ -> 
                        match aRepr with
                        | PrimUndetermined -> 
                            this.ErrorOccurred <- emitID013Diagnostics "Predicate `=` cannot be evaluated because the left argument is undetermined." variableStack.CallerStartPos variableStack.CallerEndPos 
                            this.SetDefaultValue()
                        | _ -> 
                            match bRepr with
                            | PrimUndetermined -> 
                                this.ErrorOccurred <- emitID013Diagnostics "Predicate `=` cannot be evaluated because the right argument is undetermined." variableStack.CallerStartPos variableStack.CallerEndPos 
                                this.SetDefaultValue()
                            | _ -> 
                                newPred.FplId <- $"{(aRepr = bRepr)}".ToLower()
                                this.SetValue newPred
        debug this Debug.Stop

/// Implements the semantics of an FPL decrement delegate.
type FplDecrement(name, positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericDelegate(name, positions, parent)

    do 
        this.TypeId <- PrimDigits

    override this.Name = PrimDelegateDecrementL
    override this.ShortName = PrimDelegateDecrement

    override this.Clone () =
        let ret = new FplDecrement(this.FplId, (this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Copy(other) =
        base.Copy(other)

    override this.Type signatureType = 
        let head = getFplHead this signatureType
        match signatureType with
        | SignatureType.Type -> head
        | _ ->
            let propagate = propagateSignatureType signatureType
            let args = signatureSep ", " this.ArgList propagate
            sprintf "%s(%s)" head args


    override this.CheckConsistency() =
        if this.ArgList.Count <> 1 then 
            this.ErrorOccurred <- emitID013Diagnostics $"Decrement takes 1 arguments, got {this.ArgList.Count}." this.StartPos this.EndPos
        else
            let arg = this.ArgList[0]
            let argType = arg.Type SignatureType.Type 
            if argType <> PrimDigits then 
                this.ErrorOccurred <- emitID013Diagnostics $"Decrement's argument requires type `{PrimDigits}`, got `{argType}`." arg.StartPos arg.EndPos
        base.CheckConsistency()
    
    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        addExpressionToReference this

    override this.Run() = 
        debug this Debug.Start
        match this.ErrorOccurred with
        | Some err ->
            this.SetDefaultValue()
        | _ ->
            let newValue = FplExtensionObj((this.StartPos, this.EndPos), this.Parent.Value)
            newValue.TypeId <- PrimDigits
            let argPre = this.ArgList[0]
            argPre.Run()
            let numericValue = 
                match argPre with
                | :? FplGenericVariable -> 
                    argPre.Represent()
                | :? FplReference when argPre.RefersTo.IsSome ->
                    match argPre.RefersTo.Value with
                    | :? FplGenericVariable as argPreVar -> 
                        argPreVar.Represent()
                    | _ -> argPre.FplId
                | _ -> argPre.FplId

            let mutable n = 0
            System.Int32.TryParse(numericValue, &n) |> ignore
            let n' = n - 1
            if n' < 0 then 
                // TODO issue diagnostics overflow Decrement
                this.SetDefaultValue()
            else
                newValue.FplId <- string n'
                this.SetValue newValue
        debug this Debug.Stop
