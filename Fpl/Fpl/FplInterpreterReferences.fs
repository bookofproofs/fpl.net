/// This module contains all functions and types used by the FplInterpreter
/// that are referencing other nodes of the symbol table 

(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)
module FplInterpreterReferences
open System
open FplGrammarTypes
open FplPrimitives
open FplInterpreterDiagnosticsEmitter
open FplInterpreterBasicTypes
open FplInterpreter.Globals.Debug
open FplInterpreter.Globals.Main
open FplInterpreterChecks
open FplInterpreterSTEmbedding
open FplInterpreterVariables

/// Searches for a references in node symbol table. 
/// Will work properly only for nodes types that use their scope like FplReference, FplSelf, FplParent, FplForInStmtDomain, FplForInStmtEntity, FplVariable
let rec referencedNodeOpt (fv:FplGenericNode) = 
    
    let refNodeOpt = 
        match box fv with 
        | :? IHasDotted as dotted when dotted.DottedChild.IsSome -> referencedNodeOpt dotted.DottedChild.Value
        | _ when fv.Name = PrimInstanceL -> Some fv
        | _ when fv.Name = PrimIntrinsicPred -> Some fv
        | _ when fv.Name = PrimIntrinsicInd -> Some fv
        | _ when fv.Name = PrimVariableL -> fv.RefersTo
        | _ -> fv.RefersTo
    match refNodeOpt with
    | Some refNode when refNode.Name = LiteralSelf -> refNode.RefersTo
    | Some refNode when refNode.Name = LiteralParent -> refNode.RefersTo
    | _ -> refNodeOpt

[<AbstractClass>]
type FplGenericReference(positions: Positions, parent: FplGenericNode) =
    inherit FplGenericHasValue(positions, parent)
    
    override this.Clone () = this // do not clone references to prevent stack overflow 

    member private this.RunWithVariableReplacement (called:FplGenericHasValue) =
        match box called, this.NextBlockNode with
        | :? ICanBeCalledRecusively as calledRecursively, Some blockNodeOfThis when 
            Object.ReferenceEquals(blockNodeOfThis, called) && 
            calledRecursively.CallCounter > maxRecursion -> () // stop recursion
        | _ ->
            let mutable allArgumentsHaveTerminedValues = true
            let args = 
                this.ArgList 
                // run all arguments before replacing parameters with argument values
                |> Seq.map (fun arg -> 
                    arg.Run()
                    match arg with 
                    | :? FplGenericHasValue as argWithValue ->
                        match argWithValue.Value with
                        | None -> 
                            // set the value of the argument with undef in evaluation was unsuccessfull
                            argWithValue.SetValue (new FplUndetermined(arg.TypeId, (arg.StartPos, arg.EndPos), arg))
                            allArgumentsHaveTerminedValues <- false
                        | Some (:? FplUndetermined) -> 
                            allArgumentsHaveTerminedValues <- false
                        | Some v when v.FplId = PrimUndetermined -> 
                            allArgumentsHaveTerminedValues <- false
                        | _ -> ()
                    | _ -> ()
                    arg
                )
                |> Seq.toList

            // run subroutines only if all arguments have defined values
            if allArgumentsHaveTerminedValues then 
                let pars = variableStack.SaveState(called) 
                variableStack.ReplaceVariables pars args
                // store the position of the caller
                variableStack.Helper.CallerStartPos <- this.StartPos
                variableStack.Helper.CallerEndPos <- this.EndPos
                // run all statements of the called node
                called.Run()
                this.SetValueOf called
                variableStack.RestoreState(called)
            else
                called.SetDefaultValue()
                this.SetValueOf called

    member private this.RunExtensionWithVariableReplacement (extensionObj:FplGenericNode)=
        match extensionObj.UltimateBlockNode, extensionObj.RefersTo with
        | Some enclosingNode, Some (:? FplGenericHasValue as calledExtension) when not (Object.ReferenceEquals(enclosingNode, calledExtension)) ->
            // if the extension object is called outside its own extension
            // delegate its evaluation to this extension
            let pars = variableStack.SaveState(calledExtension) 
            let args = [extensionObj]
            variableStack.ReplaceVariables pars args
            variableStack.Helper.CallerStartPos <- extensionObj.StartPos
            variableStack.Helper.CallerEndPos <- extensionObj.EndPos
            calledExtension.Run()
            
            // and store the value of the extension to this reference
            this.SetValueOf calledExtension
            variableStack.RestoreState(calledExtension)
        | _ ->
            // otherwise (i.e., inside the extensionObj's extension), 
            // treat the extensionObj as a value and store this value to the reference
            this.SetValue extensionObj

    override this.Run() =
        debug this Debug.Start
        let calledOpt = referencedNodeOpt this
        match calledOpt with 
        | Some (:? FplGenericHasValue as called) when isCallableWithParams called ->
            this.RunWithVariableReplacement called 
        | Some (:? FplGenericHasValue as called) when isCallableWithoutParams called ->
            called.Run()
            this.SetValueOf called
        | Some (:? FplGenericHasValue as called) ->
            match called.Name with
            | PrimVariableL
            | PrimDelegateEqualL
            | PrimDelegateDecrementL ->
                called.Run()
                this.SetValueOf called
            | PrimVariableArrayL ->
                this.SetValue called
            | _ -> ()
        | Some (:? FplGenericIsValue as called) when called.Name = PrimExtensionObj ->
            this.RunExtensionWithVariableReplacement (called :> FplGenericNode)
        | Some (:? FplGenericIsValue as called) ->
            this.SetValue called
        | _ -> ()
        debug this Debug.Stop

    override this.RunOrder = None

type FplReference(positions: Positions, parent: FplGenericNode) =
    inherit FplGenericReference(positions, parent)
    let mutable _callCounter = 0 

    let mutable _dottedChild : FplGenericNode option = None

    override this.Name = PrimRefL
    override this.ShortName = PrimRef

    /// The optional dotted child set when parsing a dotted reference 
    member this.DottedChild
        with get() = _dottedChild
        and set (value:FplGenericNode option) = _dottedChild <- value

    interface IHasDotted with 
        member this.DottedChild 
            with get () = this.DottedChild
            and set (value) = this.DottedChild <- value

    override this.SetValue fv = 
        match this.RefersTo with
        | Some (:? FplGenericVariable as var) when var.Name = PrimVariableL ->
            var.SetValue fv
            base.SetValue fv
        | _ ->
            base.SetValue fv

    override this.Type signatureType =
        let headObj = 
            match this.RefersTo with
            | Some ret when ret.Name = LiteralSelf && ret.RefersTo.IsSome -> ret.RefersTo.Value
            | Some ret when ret.Name = LiteralParent && ret.RefersTo.IsSome -> ret.RefersTo.Value
            | Some ret when ret.Name = PrimDelegateDecrementL && ret.RefersTo.IsSome -> ret.RefersTo.Value
            | Some ret -> ret
            | None -> this

        let propagate = propagateSignatureType signatureType

        // The arguments are reserved for the arguments or the coordinates of the reference
        let args, argsCount =
            let ret = signatureSep ", " this.ArgList propagate
            ret, this.ArgList.Count

        let head = 
            let ret = 
                if headObj.Name = PrimExtensionL || headObj.Name = PrimDelegateDecrementL then 
                    headObj.Type signatureType
                elif headObj.ExpressionType.IsNoFix then
                    getFplHead headObj signatureType 
                else
                    headObj.ExpressionType.GetUserDefinedLiteral headObj.FplId
            match signatureType, ret, args with
            | SignatureType.Type, "", LiteralUndef -> ""
            | SignatureType.Type, "", "" -> LiteralUndef
            | SignatureType.Type, _, _ -> headObj.TypeId
            | _ -> ret

        let fallBackValueClosure =
            let varMappingOpt = getMapping headObj
            match varMappingOpt with 
            | Some varMapping ->
                match headObj.Name with 
                | PrimFunctionalTermL when signatureType = SignatureType.Type -> 
                    varMapping.Type propagate
                | PrimMandatoryFunctionalTermL when signatureType = SignatureType.Type -> 
                    varMapping.Type propagate
                | PrimExtensionL when signatureType = SignatureType.Type -> 
                    varMapping.Type propagate
                | _ -> 
                    $"{head}({args})"
            | None when signatureType = SignatureType.Type ->
                head
            | _ ->
                $"{head}({args})"

        match argsCount, this.ArgType, this.DottedChild with
            | 0, ArgType.Nothing, Some qualification when propagate = SignatureType.Type ->
                qualification.Type propagate
            | 0, ArgType.Nothing, Some qualification ->
                $"{head}.{qualification.Type propagate}"
            | 0, ArgType.Brackets, Some qualification ->
                $"{head}[].{qualification.Type propagate}"
            | 0, ArgType.Parentheses, Some qualification ->
                $"{head}().{qualification.Type propagate}"
            | 0, ArgType.Nothing, None -> 
                match headObj.Name with 
                | PrimVariableArrayL 
                | PrimPredicateL 
                | PrimMandatoryPredicateL 
                | PrimFunctionalTermL 
                | PrimExtensionObj 
                | PrimMandatoryFunctionalTermL -> $"{headObj.Type signatureType}"
                | _ -> head
            | 0, ArgType.Brackets, None ->
                $"{head}[]"
            | 0, ArgType.Parentheses, None ->
                fallBackValueClosure
            | 1, ArgType.Nothing, None -> 
                if head <> String.Empty then 
                    $"{head}({args})"
                else
                    args
            | _, ArgType.Nothing, Some qualification -> 
                $"{head}({args}).{qualification.Type propagate}"
            | _, ArgType.Brackets, Some qualification ->
                $"{head}[{args}].{qualification.Type propagate}"
            | _, ArgType.Parentheses, Some qualification ->
                $"{head}({args}).{qualification.Type propagate}"
            | _, ArgType.Nothing, None -> 
                $"{head}({args})"
            | _, ArgType.Brackets, None ->
                $"{head}[{args}]"
            | _, ArgType.Parentheses, None ->
                fallBackValueClosure

    override this.Represent() = // done
        if _callCounter > maxRecursion then
            this.ErrorOccurred <- emitLG002diagnostic (this.Type(SignatureType.Name)) _callCounter this.StartPos this.EndPos
            PrimUndetermined // fallback to undefined after infinite recursion (if any)
        else
            _callCounter <- _callCounter + 1
            let result = 
                match this.Value, this.DottedChild, this.RefersTo with 
                | _, Some dc, _ -> 
                    if not (Object.ReferenceEquals(dc, this)) then
                        // If the dotted child is not identical as "this",
                        // delegate the representation to dotted.
                        dc.Represent()
                    else
                        // Otherwise, fall back with dotted's "type representation" to prevent infinite loops
                        dc.Type SignatureType.Mixed
                | Some value, _, _ ->
                    if not (Object.ReferenceEquals(value,this)) then
                        // If the value is not identical as "this",
                        // delegate the representation to value.
                        value.Represent()
                    else
                        // Otherwise, fall back with "undef" to prevent infinite loops
                        PrimUndetermined
                | _, _, Some refTo when refTo.Name = LiteralSelf && refTo.ErrorOccurred.IsSome ->
                    // infinite loop or other error in self detected
                    // fallback to undefined
                    PrimUndetermined 
                | _, _, Some refTo ->
                    if not (Object.ReferenceEquals(refTo,this)) then
                        // If refTo is not identical as "this",
                        // delegate the representation to refTo.
                        refTo.Represent()
                    else
                        refTo.Type SignatureType.Mixed
                | _, _, _ ->
                    this.Type SignatureType.Mixed
            _callCounter <- _callCounter - 1
            result

    override this.CheckConsistency () = 
        base.CheckConsistency()
        checkCleanedUpFormula this

    override this.EmbedInSymbolTable nextOpt = 
        this.CheckConsistency()
        match nextOpt with 
        | Some next when next.IsBlock() ->
            addExpressionToParentArgList this 
        | Some next when next.Name = PrimForInStmtDomainL -> 
            next.RefersTo <- Some this
        | Some (:? FplReference as next) when next.DottedChild.IsSome -> 
            next.EndPos <- this.EndPos
        | Some next when (next.Name = PrimMapCaseElseL || next.Name = PrimMapCaseSingleL) -> 
            addExpressionToParentArgList this
            next.TypeId <- this.TypeId
            next.EndPos <- this.EndPos
        | Some next -> 
            addExpressionToParentArgList this
            next.EndPos <- this.EndPos
        | _ -> ()            


let findCandidatesByNameInDotted (fv: FplGenericNode) (name: string) =
    let rec findQualifiedEntity (fv1: FplGenericNode) =
        match fv1 with
        | :? FplReference as ref when ref.DottedChild.IsSome -> 
            ScopeSearchResult.Found(ref.DottedChild.Value)
        | :? FplReference -> 
            match fv1.Parent with
            | Some parent -> findQualifiedEntity parent
            | None -> ScopeSearchResult.NotFound
        | _ -> ScopeSearchResult.NotFound

    match findQualifiedEntity fv with
    | ScopeSearchResult.Found candidate ->
        match candidate with
        | :? FplVariable as var ->
            // prefer variable value over its referred type node
            Option.orElse var.Value var.RefersTo |> Option.toList
        | _ -> []
    | _ -> []

