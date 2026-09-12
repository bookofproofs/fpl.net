(* Copyright (c) 2021+ bookofproofs See LICENSE in the project root for license terms. *)

/// <summary>
/// Module housing symbol-table nodes that model the FPL assignment statement.
/// </summary>
/// <remarks>
/// The assignment node validates target and source compatibility, records initialization
/// for variables and arrays, and performs runtime assignment semantics. Diagnostics for
/// signature/typing and illegal assignment forms are emitted via the emitter helpers
/// (SIG05, SIG07, LG005 and related diagnostics).
/// </remarks>
module Fpl.Interpreter.SymbolTable.Types3.Assignments
open Fpl.Primitives
open Fpl.Parser.Types
open Fpl.Errors.Diagnostics
open Fpl.Errors.Emitter
open Fpl.Interpreter.BasicTypes
open Fpl.Interpreter.Helpers.Checks
open Fpl.Interpreter.Helpers.Basic
open Fpl.Interpreter.Helpers.Debug
open Fpl.Interpreter.SymbolTable.Types2.Intrinsic
open Fpl.Interpreter.SymbolTable.Types2.Variables
open Fpl.Interpreter.SymbolTable.Types2.References
open Fpl.Interpreter.SymbolTable.TypeMatching
open Fpl.Interpreter.SymbolTable.Types3.SelfParent

/// <summary>
/// Symbol-table node that implements the FPL assignment statement.
/// </summary>
/// <param name="positions">Source start/end positions used for diagnostics.</param>
/// <param name="parent">Parent AST node in the symbol table.</param>
/// <remarks>
/// The node supports assigning to references, variables, array elements and handles
/// special forms such as call-by-value functional-term assignments. It runs consistency
/// checks to ensure types/signatures are compatible and emits diagnostics where appropriate.
/// </remarks>
type FplAssignment(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericStmt(positions, parent)

    do
        this.FplId <- PrimAssignment
        this.TypeId <- LiteralUndef

    override this.Name = PrimAssignmentL

    /// <summary>
    /// Create a shallow clone of this assignment node preserving parts and positions.
    /// </summary>
    /// <returns>New <c>FplAssignment</c> instance with parts copied.</returns>
    override this.Clone () =
        let ret = new FplAssignment((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    /// <summary>
    /// Resolve the textual type head for this node according to requested signature format.
    /// </summary>
    /// <param name="signatureType">Requested signature type used for formatting.</param>
    /// <returns>Type head as produced by <c>getFplHead</c>.</returns>
    override this.Type signatureType = 
        getFplHead this signatureType

    /// <summary>
    /// Internal helper to resolve the effective node used for assignment at argument index <paramref name="no"/>.
    /// </summary>
    /// <param name="no">Index into <c>ArgList</c> (0 for assignee, 1 for assigned value).</param>
    /// <returns>
    /// Optionally the resolved node to be treated as assignee/assigned value. When encountering
    /// dotted references the referenced child is preferred.
    /// </returns>
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

    /// <summary>
    /// The resolved assignee node, if present.
    /// </summary>
    /// <returns>Option containing the assignee node or <c>None</c> when unavailable.</returns>
    member this.Assignee:FplGenericNode option = this.GetAssignmentArg 0

    /// <summary>
    /// The resolved assigned value node. For array-index assignments the actual value at
    /// the target coordinates is returned.
    /// </summary>
    /// <returns>Option containing the assigned value node, or an <c>FplIntrinsicUndef</c> when missing.</returns>
    member this.AssignedValue = 
        let assignedValueOpt = this.GetAssignmentArg 1
        match assignedValueOpt with 
        | Some (:? FplVariableArray as targetArray) when this.ArgList[1].ArgType = ArgType.Brackets ->
            let targetCoords = representationSep "|" (this.ArgList[1].ArgList) 
            let valueAtTargetCoordinates = targetArray.GetValueByCoordinates targetCoords
            Some valueAtTargetCoordinates
        | Some _ -> assignedValueOpt
        | None -> Some (new FplIntrinsicUndef((this.StartPos, this.EndPos), this))

    /// <summary>
    /// Perform consistency checks for the assignment statement, including type/signature validation
    /// and legality of the assignment target.
    /// </summary>
    /// <remarks>
    /// Checks performed:
    /// - Self-assignment detection (LG005).
    /// - Parameter/argument matching using <c>FplTypeMatcher.MatchPwA</c> (SIG05).
    /// - Illegal assignment forms such as assigning to <c>self</c> or <c>parent</c> or attempting
    ///   to assign a reference used as a function call (SIG07).
    /// Diagnostics are attached to <c>this.ErrorOccurred</c> when violations are found.
    /// </remarks>
    /// <exceptions>
    /// <exception>Emits <c>LG005</c> when assigning a value to itself or assigning the same type.</exception>
    /// <exception>Emits <c>SIG05</c> when argument/parameter matching fails.</exception>
    /// <exception>Emits <c>SIG07</c> for illegal reference assignment forms.</exception>
    /// </exceptions>
    override this.CheckConsistency () = 
        base.CheckConsistency()
        let checkTypes (assignee:FplGenericNode) (assignedValue:FplGenericNode) =
            let nameAssignee = assignee.Type SignatureType.Name
            let nameAssignedValue = assignedValue.Type SignatureType.Name
            if nameAssignee = nameAssignedValue then
                this.ErrorOccurred <- emitLG005Diagnostics nameAssignedValue assignedValue.StartPos assignedValue.EndPos
            else
                // assignee is to be treated as parameter, the assignedValue as argument
                match FplTypeMatcher.MatchPwA [assignedValue] [assignee] with
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
            this.ErrorOccurred <- emitSIG07diagnostics (ref.Type SignatureType.Name) assignee.Name (this.ArgList[0].StartPos) (this.ArgList[0].EndPos)
        | :? FplReference as ref, Some (:? FplGenericIsValue as assignee) ->
            this.ErrorOccurred <- emitSIG07diagnostics (ref.Type SignatureType.Name) assignee.Name (this.ArgList[0].StartPos) (this.ArgList[0].EndPos)
        | :? FplReference as ref, Some (:? FplReference as assignee) when assignee.RefersTo.IsNone ->
            this.ErrorOccurred <- emitSIG07diagnostics (ref.Type SignatureType.Name) assignee.Name (this.ArgList[0].StartPos) (this.ArgList[0].EndPos)
        | _ -> ()

        let nameAssignee = this.ArgList[0].Type SignatureType.Name
        let nameAssignedValue = this.ArgList[1].Type SignatureType.Name
        if nameAssignee = nameAssignedValue then
            // something has been assigned to itself
            this.ErrorOccurred <- emitLG005Diagnostics nameAssignedValue this.ArgList[1].StartPos this.ArgList[1].EndPos

        // remember preceding errors of references used in the assignment (if any)
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
            this.ErrorOccurred <- emitSIG07diagnostics (assignee.Type SignatureType.Name) assignee.Name (this.ArgList[0].StartPos) (this.ArgList[0].EndPos)
        | None, Some (:? FplParent as assignee), _ ->
            this.ErrorOccurred <- emitSIG07diagnostics (assignee.Type SignatureType.Name) assignee.Name (this.ArgList[0].StartPos) (this.ArgList[0].EndPos)
        | None, Some (assignee), _ ->
            this.ErrorOccurred <- emitSIG07diagnostics (assignee.Type SignatureType.Name) assignee.Name (this.ArgList[0].StartPos) (this.ArgList[0].EndPos)
        | _ -> ()

    /// <summary>
    /// Embed the assignment expression into its parent's argument list after running checks.
    /// </summary>
    /// <param name="_">Unused conventional parameter.</param>
    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        addExpressionToParentArgList this

    /// <summary>
    /// Set the resolved assignee to the provided value at runtime.
    /// </summary>
    /// <param name="fv">Value node to set into the assignee.</param>
    /// <param name="assignedExpression">The original assigned expression node (keeps reference info).</param>
    /// <remarks>
    /// - For variables the value is set and the variable's reference is recorded.
    /// - For variable arrays the value is assigned at coordinates computed from the lhs index list.
    /// - For template targets the template usage is attempted (SIG12).
    /// </remarks>
    member private this.SetAssignee (fv:FplGenericNode) assignedExpression = 
        match this.Assignee with
        | Some (:? FplVariable as assignee) ->
            assignee.SetValue fv
            match assignee.RefersTo with
            | None ->
                assignee.RefersTo <- Some assignedExpression
                assignee.IsExpressionAssigned <- true
            | _ -> ()
        | Some (:? FplVariableArray as assignee) ->
            match assignee.RefersTo with 
            | Some (:? FplIntrinsicTpl as tpl) -> tpl.TrySetTemplateUsage fv (SIG12("", "", "", "").Code)
            | _ -> ()
            let coordinatesKey = representationSep "|" (this.ArgList[0].ArgList) 
            assignee.AssignValueToCoordinates coordinatesKey fv // set value of array
        | _ -> ()

    /// <summary>
    /// Execute the assignment at runtime: evaluate right-hand side as necessary and store
    /// the computed value into the assignee.
    /// </summary>
    /// <remarks>
    /// If pre-existing errors are present the assignment is skipped. The method handles direct
    /// value assignments, references that must be evaluated, and array-to-array assignments.
    /// </remarks>
    override this.Run() =
        StaticDebug.Debug(this,Debug.Start)

        match this.ErrorOccurred, this.ArgList[1], this.AssignedValue with 
        | Some _, _, _ ->
            () // skip assignment, if any preceding errors occurred
        | None, (:? FplGenericHasValue as ref), Some (:? FplVariableArray as assignedValue) ->
            this.SetAssignee assignedValue assignedValue
        | None, (:? FplGenericHasValue as ref), Some (:? FplGenericIsValue as assignedValue) ->
            this.SetAssignee assignedValue assignedValue
        | None, (:? FplGenericHasValue as ref), Some assignedValue ->
            ref.Run()
            this.SetAssignee (ref.Value.Value) assignedValue
        | None, (:? FplGenericIsValue as ref), Some assignedValue ->
            this.SetAssignee ref assignedValue
        | _ -> ()

        StaticDebug.Debug(this,Debug.Stop)

