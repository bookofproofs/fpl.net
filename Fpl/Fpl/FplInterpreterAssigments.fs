/// This module contains all symbol table nodes used by the FplInterpreter
/// to model assignments.

(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)
module FplInterpreterAssignments
open FplPrimitives
open FplGrammarTypes
open ErrDiagnostics
open FplInterpreterDiagnosticsEmitter
open FplInterpreterBasicTypes
open FplInterpreter.Globals.Debug
open FplInterpreterChecks
open FplInterpreter.Globals.Helpers
open FplInterpreterReferences
open FplInterpreterIntrinsicTypes
open FplInterpreterVariables
open FplInterpreterFplTypeMatching
open FplInterpreterReferencesSelfParent

/// Implements the assignment statement in FPL.
type FplAssignment(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericStmt(positions, parent)

    do
        this.FplId <- PrimAssignment
        this.TypeId <- LiteralUndef

    override this.Name = PrimAssignmentL

    override this.Clone () =
        let ret = new FplAssignment((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = 
        getFplHead this signatureType

    member private this.GetAssignmentArg no =
        if this.ArgList.Count > 1 then 
            let candidate = this.ArgList[no]
            match candidate with 
            | :? FplReference as ref ->
                match ref.DottedChild with 
                | Some dc -> dc.RefersTo
                | None when ref.RefersTo.IsSome -> ref.RefersTo
                | _ -> Some candidate
            | _ ->
                Some candidate
        else
            None

    member this.Assignee:FplGenericNode option = this.GetAssignmentArg 0

    member this.AssignedValue = 
        let assignedValueOpt = this.GetAssignmentArg 1
        match assignedValueOpt with 
        | Some (:? FplVariableArray as targetArray) when this.ArgList[1].ArgType = ArgType.Brackets ->
            let targetCoords = representationSep "|" (this.ArgList[1].ArgList) 
            let valueAtTargetCoordinates = targetArray.GetValueByCoordinates targetCoords
            Some valueAtTargetCoordinates
        | Some _ -> assignedValueOpt
        | None -> Some (new FplIntrinsicUndef((this.StartPos, this.EndPos), this))

    override this.CheckConsistency () = 
        base.CheckConsistency()
        let checkTypes (assignee:FplGenericNode) (assignedValue:FplGenericNode) =
            let nameAssignee = assignee.Type SignatureType.Name
            let nameAssignedValue = assignedValue.Type SignatureType.Name
            if nameAssignee = nameAssignedValue then
                this.ErrorOccurred <- emitLG005Diagnostics nameAssignedValue assignedValue.StartPos assignedValue.EndPos
            else
                // assignee is to be treated as parameter, the assignedValue as argument
                match mpwa [assignedValue] [assignee] with
                | Some errMsg ->
                    this.ErrorOccurred <- emitSIG05Diagnostics errMsg this.ArgList[1].StartPos this.ArgList[1].EndPos
                | _ -> ()
                
        let checkErrorOccuredInReference (fv:FplGenericNode) = 
            match fv with
            | :? FplReference as ref -> 
                this.ErrorOccurred <- ref.ErrorOccurred 
            | _ -> ()

        match this.ArgList[0], this.Assignee with
        | :? FplReference as ref, Some assignee when ref.ArgType = ArgType.Parentheses ->
            this.ErrorOccurred <- emitSIG07iagnostic (ref.Type SignatureType.Name) "an expression" assignee.Name (this.ArgList[0].StartPos) (this.ArgList[0].EndPos)
        | :? FplReference as ref, Some (:? FplGenericIsValue as assignee) ->
            this.ErrorOccurred <- emitSIG07iagnostic (ref.Type SignatureType.Name) "a value" assignee.Name (this.ArgList[0].StartPos) (this.ArgList[0].EndPos)
        | :? FplReference as ref, Some (:? FplReference as assignee) when assignee.RefersTo.IsNone ->
            this.ErrorOccurred <- emitSIG07iagnostic (ref.Type SignatureType.Name) "undefined" assignee.Name (this.ArgList[0].StartPos) (this.ArgList[0].EndPos)
        | _ -> ()

        let nameAssignee = this.ArgList[0].Type SignatureType.Name
        let nameAssignedValue = this.ArgList[1].Type SignatureType.Name
        if nameAssignee = nameAssignedValue then
            // something has been assigned to itself
            this.ErrorOccurred <- emitLG005Diagnostics nameAssignedValue this.ArgList[1].StartPos this.ArgList[1].EndPos

        // remember proceeding errors of references used in the assignment (if any)
        checkErrorOccuredInReference this.ArgList[0]
        checkErrorOccuredInReference this.ArgList[1]
        match this.ErrorOccurred, this.Assignee, this.AssignedValue with
        | None, Some (:? FplVariable as assignee), Some (assignedValue:FplGenericNode) when assignedValue.Name = PrimClassL ->
            assignee.IsInitialized <- true
            checkTypes assignee assignedValue
        | None, Some (:? FplVariable as assignee), Some (assignedValue:FplGenericNode) when (assignedValue.Name = PrimFunctionalTermL || assignedValue.Name = PrimMandatoryFunctionalTermL) && isCallByValue this.ArgList[1] ->
            let mapOpt = getMapping assignedValue
            match mapOpt with 
            | Some map -> checkTypes this.ArgList[0] map
            | _ -> checkTypes assignee assignedValue
        | None, Some (:? FplVariable as assignee), Some _ -> 
            checkTypes assignee this.ArgList[1] 
        | None, Some (:? FplVariableArray as assignee), Some assignedValue ->
           checkTypes this.ArgList[0] this.ArgList[1] 
        | None, Some (:? FplSelf as assignee), _ ->
            match assignee.RefersTo with 
            | Some ref -> 
                this.ErrorOccurred <- emitSIG07iagnostic (assignee.Type SignatureType.Name) (getEnglishName ref.Name false) assignee.Name (this.ArgList[0].StartPos) (this.ArgList[0].EndPos)
            | None ->
                this.ErrorOccurred <- emitSIG07iagnostic (assignee.Type SignatureType.Name) "the type of self could not be determined" assignee.Name (this.ArgList[0].StartPos) (this.ArgList[0].EndPos)
        | None, Some (:? FplParent as assignee), _ ->
            match assignee.RefersTo with 
            | Some ref -> 
                this.ErrorOccurred <- emitSIG07iagnostic (assignee.Type SignatureType.Name) (getEnglishName ref.Name false) assignee.Name (this.ArgList[0].StartPos) (this.ArgList[0].EndPos)
            | None ->
                this.ErrorOccurred <- emitSIG07iagnostic (assignee.Type SignatureType.Name) "the type of parent could not be determined" assignee.Name (this.ArgList[0].StartPos) (this.ArgList[0].EndPos)
        | None, Some (assignee), Some assignedValue ->
            this.ErrorOccurred <- emitSIG07iagnostic (assignee.Type SignatureType.Name) $"type `{assignee.Type SignatureType.Type}`" assignee.Name (this.ArgList[0].StartPos) (this.ArgList[0].EndPos)
        | _ -> ()

    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        addExpressionToParentArgList this

    member private this.SetAssignee (fv:FplGenericNode) = 
        match this.Assignee with
        | Some (:? FplVariable as assignee) ->
            assignee.SetValue fv
        | Some (:? FplVariableArray as assignee) ->
            let coordinatesKey = representationSep "|" (this.ArgList[0].ArgList) 
            assignee.AssignValueToCoordinates coordinatesKey fv // set value of array
        | _ -> ()

    override this.Run() =
        debug this Debug.Start

        match this.ErrorOccurred, this.ArgList[1], this.AssignedValue with 
        | Some _, _, _ ->
            () // skip assignment, if any proceeding errors occured
        | None, (:? FplGenericHasValue as ref), Some (:? FplVariableArray as assignedValue) ->
            this.SetAssignee assignedValue
        | None, (:? FplGenericHasValue as ref), Some (:? FplGenericIsValue as assignedValue) ->
            this.SetAssignee assignedValue
        | None, (:? FplGenericHasValue as ref), _ ->
            ref.Run()
            this.SetAssignee (ref.Value.Value)
        | None, (:? FplGenericIsValue as ref), _ ->
            this.SetAssignee ref
        | _ -> ()

        debug this Debug.Stop

