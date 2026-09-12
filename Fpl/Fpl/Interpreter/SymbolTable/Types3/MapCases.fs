(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 
*)

/// <summary>
/// Module containing FPL symbol-table node types that model and interpret the
/// <c>mcases</c> statement (map/case expressions).
/// </summary>
/// <remarks>
/// The module defines three node kinds:
/// - <c>FplMapCaseSingle</c>: a single case with a condition and a result expression.
/// - <c>FplMapCaseElse</c>: the else branch for <c>mcases</c>.
/// - <c>FplMapCases</c>: the enclosing map/cases node that validates branch types,
///   checks for duplicate (unreachable) cases and evaluates the selected branch at runtime.
/// Diagnostics SIG13 and SIG14 are used for signature/type and reachability errors respectively.
/// </remarks>
module Fpl.Interpreter.SymbolTable.Types3.MapCases
open System.Collections.Generic
open Fpl.Primitives
open Fpl.Parser.Types
open Fpl.Errors.Diagnostics
open Fpl.Errors.Emitter
open Fpl.Interpreter.BasicTypes
open Fpl.Interpreter.Helpers.Checks
open Fpl.Interpreter.Helpers.Basic
open Fpl.Interpreter.Helpers.Debug
open Fpl.Interpreter.SymbolTable.Types2.Intrinsic

/// <summary>
/// Represents a single <c>mcases</c> branch: a predicate/condition and its result expression.
/// </summary>
/// <param name="positions">Source code start/end positions for diagnostic reporting.</param>
/// <param name="parent">Parent symbol-table node.</param>
/// <remarks>
/// This node inherits from <c>FplGenericHasValue</c> and stores the condition in its first
/// argument and the result expression in its second argument. It delegates its static type
/// determination to the usual <c>getFplHead</c> policy and validates that the condition is a predicate.
/// </remarks>
type FplMapCaseSingle(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericHasValue(positions, parent)
    do 
        this.FplId <- PrimMapCaseSingle

    override this.Name = PrimMapCaseSingleL
    override this.ShortName = PrimStmt

    /// <summary>
    /// Create a shallow clone of this branch node preserving parts and positions.
    /// </summary>
    /// <returns>A new <c>FplMapCaseSingle</c> instance with copied parts.</returns>
    override this.Clone () =
        let ret = new FplMapCaseSingle((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    /// <summary>
    /// Returns the static type head for this node according to the provided signature type.
    /// </summary>
    /// <param name="signatureType">The surrounding signature type used for inference.</param>
    /// <returns>Type head as resolved by <c>getFplHead</c>.</returns>
    override this.Type signatureType = 
        getFplHead this signatureType

    /// <summary>
    /// Get the condition (predicate) expression of this case.
    /// </summary>
    /// <returns>Node representing the case condition (first argument).</returns>
    member this.GetCondition() = this.ArgList[0]

    /// <summary>
    /// Get the result expression of this case.
    /// </summary>
    /// <returns><c>FplGenericHasValue</c> representing the result (second argument).</returns>
    member this.GetResult() = this.ArgList[1] :?> FplGenericHasValue

    /// <summary>
    /// Performs consistency checks for a single case branch.
    /// </summary>
    /// <remarks>
    /// Ensures baseline consistency defined in base class and that the first argument is a predicate.
    /// Diagnostic helpers are invoked via <c>checkArgPred</c>.
    /// </remarks>
    override this.CheckConsistency() = 
        base.CheckConsistency()
        checkArgPred this (this.GetCondition())

    /// <summary>
    /// Embeds this case into the parent expression list after running consistency checks.
    /// </summary>
    /// <param name="_">Unused parameter (conventional signature).</param>
    /// <remarks>
    /// Will call <c>addExpressionToParentArgList</c> to attach this branch to the parent map/cases node.
    /// </remarks>
    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        addExpressionToParentArgList this

    override this.RunOrder = None

    /// <summary>
    /// Evaluate the result expression of this branch and set this node's value accordingly.
    /// </summary>
    /// <remarks>
    /// The condition is evaluated by the enclosing <c>FplMapCases.Run</c>; this node's <c>Run</c>
    /// focuses on running the branch result and storing its value.
    /// </remarks>
    override this.Run() = 
        StaticDebug.Debug(this,Debug.Start)
        let result = this.GetResult()
        result.Run()
        this.SetValueOf result
        StaticDebug.Debug(this,Debug.Stop)

/// <summary>
/// Represents the else branch for an <c>mcases</c> construct.
/// </summary>
/// <param name="positions">Source code start/end positions for diagnostic reporting.</param>
/// <param name="parent">Parent symbol-table node.</param>
/// <remarks>
/// The else branch typically contains a single result expression. Its type delegates to its
/// contained expression when present, otherwise falls back to the standard head resolver.
/// </remarks>
type FplMapCaseElse(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericHasValue(positions, parent)
    do 
        this.FplId <- PrimMapCaseElse

    override this.Name = PrimMapCaseElseL
    override this.ShortName = PrimStmt

    /// <summary>
    /// Create a shallow clone of this else-branch node preserving parts and positions.
    /// </summary>
    /// <returns>A new <c>FplMapCaseElse</c> instance with copied parts.</returns>
    override this.Clone () =
        let ret = new FplMapCaseElse((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    /// <summary>
    /// Resolve the static type for the else branch by delegating to its first argument if present.
    /// </summary>
    /// <param name="signatureType">The surrounding signature type used for inference.</param>
    /// <returns>Type head resolved from the contained expression or the default head.</returns>
    override this.Type signatureType = 
        let argOpt = this.ArgList |> Seq.tryHead 
        match argOpt with 
        | Some arg -> arg.Type signatureType // delegate type to the argument of MapCaseElse case
        | _ -> getFplHead this signatureType // fallback (should never occur due to FPL syntax)

    /// <summary>
    /// Attach the else branch to its parent expression list.
    /// </summary>
    /// <param name="_">Unused parameter (conventional signature).</param>
    override this.EmbedInSymbolTable _ = addExpressionToParentArgList this

    override this.RunOrder = None

    /// <summary>
    /// Evaluate the else-branch content and set this node's value accordingly.
    /// </summary>
    /// <remarks>
    /// The else node expects at least one argument representing the content to return when no other
    /// case condition holds.
    /// </remarks>
    override this.Run() = 
        StaticDebug.Debug(this,Debug.Start)
        let first = this.ArgList |> Seq.head
        let contentOfElsResult = first :?> FplGenericHasValue
        contentOfElsResult.Run()
        this.SetValueOf contentOfElsResult
        StaticDebug.Debug(this,Debug.Stop)

/// <summary>
/// Represents the enclosing <c>mcases</c> construct containing multiple branches and an else branch.
/// </summary>
/// <param name="positions">Source code start/end positions for diagnostic reporting.</param>
/// <param name="parent">Parent symbol-table node.</param>
/// <remarks>
/// Responsibilities:
/// - Ensure that all branch results (including else) have the same effective type (SIG13 checks).
/// - Detect duplicate/unreachable case signatures (SIG14 diagnostics).
/// - Evaluate the first branch whose condition evaluates to <c>True</c>, or the else branch if none match.
/// </remarks>
type FplMapCases(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericHasValue(positions, parent)
    let _consistentCaseType = new FplIntrinsicTpl("", positions, parent)
    let _reachableCases = new HashSet<string>()

    do 
        this.FplId <- LiteralMapCases

    override this.Name = PrimMapCasesL
    override this.ShortName = PrimStmt

    /// <summary>
    /// Create a shallow clone of this <c>mcases</c> node preserving parts and positions.
    /// </summary>
    /// <returns>A new <c>FplMapCases</c> instance with copied parts.</returns>
    override this.Clone () =
        let ret = new FplMapCases((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    /// <summary>
    /// Resolve the static type head for the whole <c>mcases</c> node.
    /// </summary>
    /// <param name="signatureType">The surrounding signature type used for inference.</param>
    /// <returns>Type head as resolved by <c>getFplHead</c>.</returns>
    override this.Type signatureType = 
        getFplHead this signatureType

    /// <summary>
    /// Return all branch nodes that are single condition/result pairs (exclude else).
    /// </summary>
    /// <returns>List of <c>FplMapCaseSingle</c> nodes in source order.</returns>
    member this.GetConditionResultList() = 
        this.ArgList
        |> Seq.choose (fun item ->
            match item with
            | :? FplMapCaseSingle as condRes -> Some condRes
            | _ -> None)
        |> Seq.toList

    /// <summary>
    /// Return the else branch node for this <c>mcases</c>.
    /// </summary>
    /// <returns><c>FplGenericHasValue</c> representing the else branch.</returns>
    /// <remarks>
    /// The else branch is expected to be the last argument in the map/cases argument list.
    /// </remarks>
    member this.GetMapElse() = 
        let last = this.ArgList |> Seq.last
        last :?> FplGenericHasValue

    /// <summary>
    /// Validate that all branch results (including else) resolve to a consistent type.
    /// </summary>
    /// <remarks>
    /// Uses an internal template intrinsic (<c>FplIntrinsicTpl</c>) to try setting a single
    /// template usage across all branch result expressions. If inconsistencies are found,
    /// the node's <c>TypeId</c> is set to <c>LiteralUndef</c>. Otherwise the resolved consistent
    /// type or mapping type id is assigned.
    /// </remarks>
    /// <exceptions>
    /// <exception>Emits SIG13-style diagnostics when branch result types do not match.</exception>
    /// </exceptions>
    member private this.CheckAllResultsForEqualType() =
        // check if all results have the same type
        this.GetConditionResultList()
        |> Seq.map (fun conditionResultPair -> conditionResultPair.GetResult())
        |> Seq.iter (fun result -> _consistentCaseType.TrySetTemplateUsage result (SIG13("", "", "", "").Code))
        // check also else result
        _consistentCaseType.TrySetTemplateUsage (this.GetMapElse()) (SIG13("", "", "", "").Code)
        match _consistentCaseType.ErrorOccurred with
        | Some errMsg -> 
            // Since there were preceding errors regarding inconsistent Type Ids of some branches
            // set the TypeId of this FplMapCases to undefined
            this.TypeId <- LiteralUndef  
         | _ ->
            // Set the TypeId of this FplMapCases to the consistent TypeId found for all of its branches
            let typeOfAllBranches = _consistentCaseType.RefersTo.Value
            let mapOpt = getMapping typeOfAllBranches
            this.TypeId <- 
                match mapOpt with 
                | Some map -> map.TypeId // if the type of all branches is a mapping, use the mapping's TypeId
                | None -> typeOfAllBranches.TypeId

    /// <summary>
    /// Ensure case conditions are reachable and not duplicated.
    /// </summary>
    /// <remarks>
    /// Each condition's signature (<c>condition.Type SignatureType.Name</c>) is used as a key.
    /// If the same signature appears more than once, a SIG14 diagnostic is emitted and the
    /// duplicate is marked as an error source via <c>this.ErrorOccurred</c>.
    /// </remarks>
    /// <exceptions>
    /// <exception>Emits SIG14 diagnostics for duplicate/unreachable case signatures.</exception>
    /// </exceptions>
    member private this.CheckAllCasesForBeingReachable() =
        _reachableCases.Clear()
        this.GetConditionResultList()
        |> Seq.map (fun conditionResultPair -> conditionResultPair.GetCondition())
        |> Seq.iter (fun condition -> 
            let conditionSignature = condition.Type SignatureType.Name
            if _reachableCases.Add(conditionSignature) then 
                () // signature added
            else
                // signature was already added
                this.ErrorOccurred <- emitSIG14Diagnostics condition.StartPos condition.EndPos
                
        )

    /// <summary>
    /// Run consistency checks for the <c>mcases</c> node.
    /// </summary>
    /// <remarks>
    /// Calls base consistency checks then verifies result type consistency and reachability of cases.
    /// </remarks>
    override this.CheckConsistency() = 
        base.CheckConsistency()
        this.CheckAllResultsForEqualType()
        this.CheckAllCasesForBeingReachable()


    /// <summary>
    /// Embed the <c>mcases</c> expression into its parent after performing consistency checks.
    /// </summary>
    /// <param name="_">Unused parameter (conventional signature).</param>
    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        addExpressionToParentArgList this

    override this.RunOrder = None

    /// <summary>
    /// Evaluate the <c>mcases</c> node at runtime.
    /// </summary>
    /// <remarks>
    /// Evaluates each case condition in order. The first case whose condition evaluates to
    /// <c>True</c> has its result evaluated and returned. If no condition matches, the else
    /// branch is evaluated and returned.
    /// </remarks>
    override this.Run() = 
        StaticDebug.Debug(this,Debug.Start)
        let resultLst = this.GetConditionResultList()
        let mapElse = this.GetMapElse()
        let firstMapCaseWithTrueConditionOpt = 
            resultLst
            |> Seq.tryFind(fun mapCaseSingle -> 
                let condition = mapCaseSingle.GetCondition()
                condition.Run()
                condition.Represent() = LiteralTrue
            )
        match firstMapCaseWithTrueConditionOpt with
        | Some firstMapCaseWithTrueCondition -> 
            firstMapCaseWithTrueCondition.Run()
            let resOfFound = firstMapCaseWithTrueCondition.GetResult()
            this.SetValueOf resOfFound
        | None -> 
            mapElse.Run()
            this.SetValueOf mapElse
        StaticDebug.Debug(this,Debug.Stop)
