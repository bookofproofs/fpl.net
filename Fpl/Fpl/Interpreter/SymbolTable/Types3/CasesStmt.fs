(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)
/// <summary>
/// Module containing symbol-table nodes that model the FPL <c>cases</c> statement.
/// </summary>
/// <remarks>
/// The <c>cases</c> statement evaluates a sequence of condition/statement branches
/// and executes the first branch whose condition evaluates to <c>True</c>. The module
/// provides concrete nodes for single case branches, the else branch, and the enclosing
/// <c>cases</c> block which performs reachability checks and runtime dispatch.
/// Diagnostics SIG14 is emitted for duplicate/unreachable case signatures.
/// </remarks>
module Fpl.Interpreter.SymbolTable.Types3.CasesStmt
open System.Collections.Generic
open Fpl.Primitives
open Fpl.Parser.Types
open Fpl.Errors.Emitter
open Fpl.Interpreter.BasicTypes
open Fpl.Interpreter.Helpers.Checks
open Fpl.Interpreter.Helpers.Basic
open Fpl.Interpreter.Helpers.Debug
open Fpl.Interpreter.SymbolTable.Types2.Intrinsic



/// <summary>
/// Represents a single <c>cases</c> branch: a condition followed by statements to run
/// when the condition holds.
/// </summary>
/// <param name="positions">Source start/end positions used for diagnostics.</param>
/// <param name="parent">Parent AST node in the symbol table.</param>
/// <remarks>
/// The first argument is expected to be a predicate (condition). The remaining arguments
/// represent the statements to execute when the condition is true. Consistency checks
/// assert that the condition is a predicate and embed the branch into the parent's
/// argument list.
/// </remarks>
type FplCaseSingle(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericStmt(positions, parent)
    do 
        this.FplId <- PrimCaseSingle

    override this.Name = PrimCaseSingleL

    /// <summary>
    /// Create a shallow clone of the case branch preserving parts and positions.
    /// </summary>
    /// <returns>A new <c>FplCaseSingle</c> instance with copied parts.</returns>
    override this.Clone () =
        let ret = new FplCaseSingle((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    /// <summary>
    /// Resolve the static type head for the branch using the general head policy.
    /// </summary>
    /// <param name="signatureType">Requested signature type used for formatting.</param>
    /// <returns>Type head string as returned by <c>getFplHead</c>.</returns>
    override this.Type signatureType = 
        getFplHead this signatureType

    /// <summary>
    /// Return the condition (first argument) of the case branch.
    /// </summary>
    /// <returns>Node representing the branch condition.</returns>
    member this.GetCondition() = this.ArgList[0]

    /// <summary>
    /// Return the sequence of statements that follow the condition in this branch.
    /// </summary>
    /// <returns>Sequence of statement nodes to execute when the condition is true.</returns>
    member this.StmtsAfterCondition() = this.ArgList |> Seq.tail

    /// <summary>
    /// Perform consistency checks: base checks plus predicate validation for the condition.
    /// </summary>
    override this.CheckConsistency() = 
        base.CheckConsistency()
        checkArgPred this (this.GetCondition())

    /// <summary>
    /// Embed this branch into its parent's argument list after consistency checks.
    /// </summary>
    /// <param name="_">Unused parameter (conventional signature).</param>
    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        addExpressionToParentArgList this

    /// <summary>
    /// Execute all statements following the branch condition.
    /// </summary>
    /// <remarks>
    /// The enclosing <c>FplCases</c> is responsible for evaluating the condition; when a branch
    /// is selected its <c>Run</c> runs the statements defined after the condition.
    /// </remarks>
    override this.Run() = 
        StaticDebug.Debug(this,Debug.Start)
        this.StmtsAfterCondition()
        |> Seq.iter (fun stmt -> stmt.Run())
        StaticDebug.Debug(this,Debug.Stop)

/// <summary>
/// Represents the else branch of a <c>cases</c> construct.
/// </summary>
/// <param name="positions">Source start/end positions used for diagnostics.</param>
/// <param name="parent">Parent AST node in the symbol table.</param>
/// <remarks>
/// The else branch contains statements that run when no case conditions evaluate to <c>True</c>.
/// </remarks>
type FplCaseElse(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericStmt(positions, parent)
    do 
        this.FplId <- PrimCaseElse

    override this.Name = PrimCaseElseL

    /// <summary>
    /// Create a shallow clone of the else branch preserving parts and positions.
    /// </summary>
    /// <returns>A new <c>FplCaseElse</c> instance with copied parts.</returns>
    override this.Clone () =
        let ret = new FplCaseElse((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    /// <summary>
    /// Resolve the static type head for the else branch.
    /// </summary>
    /// <param name="signatureType">Requested signature type used for formatting.</param>
    /// <returns>Type head string as returned by <c>getFplHead</c>.</returns>
    override this.Type signatureType = 
        getFplHead this signatureType

    /// <summary>
    /// Execute all statements contained in the else branch.
    /// </summary>
    /// <remarks>
    /// The else branch runs when none of the case branches' conditions are true.
    /// </remarks>
    override this.Run() = 
        StaticDebug.Debug(this,Debug.Start)
        this.ArgList 
        |> Seq.iter (fun stmt -> stmt.Run())
        StaticDebug.Debug(this,Debug.Stop)

/// <summary>
/// Represents the enclosing <c>cases</c> statement that contains multiple branches and an else.
/// </summary>
/// <param name="positions">Source start/end positions used for diagnostics.</param>
/// <param name="parent">Parent AST node in the symbol table.</param>
/// <remarks>
/// Responsibilities:
/// - Collect single-case branches and the else branch.
/// - Detect duplicate/unreachable case condition signatures and emit SIG14 diagnostics.
/// - At runtime evaluate conditions in order and execute the first branch with a true condition,
///   otherwise execute the else branch.
/// </remarks>
type FplCases(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericStmt(positions, parent)
    let _reachableCases = new HashSet<string>()
    do 
        this.FplId <- LiteralCases

    override this.Name = PrimCasesL

    /// <summary>
    /// Create a shallow clone of the cases node preserving parts and positions.
    /// </summary>
    /// <returns>A new <c>FplCases</c> instance with copied parts.</returns>
    override this.Clone () =
        let ret = new FplCases((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    /// <summary>
    /// Resolve the static type head for the cases block.
    /// </summary>
    /// <param name="signatureType">Requested signature type used for formatting.</param>
    /// <returns>Type head string as returned by <c>getFplHead</c>.</returns>
    override this.Type signatureType = 
        getFplHead this signatureType

    /// <summary>
    /// Return all branch nodes that are single condition/statement pairs (exclude else).
    /// </summary>
    /// <returns>List of <c>FplCaseSingle</c> nodes in source order.</returns>
    member this.GetConditionResultList() = 
        this.ArgList
        |> Seq.choose (fun item ->
            match item with
            | :? FplCaseSingle as condRes -> Some condRes
            | _ -> None)
        |> Seq.toList

    /// <summary>
    /// Return the else statement node for this <c>cases</c> (expected as the last argument).
    /// </summary>
    /// <returns>Node representing the else branch (last argument).</returns>
    member this.GetElseStmt() = this.ArgList |> Seq.last

    /// <summary>
    /// Validate that case conditions are unique and reachable.
    /// </summary>
    /// <remarks>
    /// Uses each condition's signature (<c>condition.Type SignatureType.Name</c>) as a key.
    /// When a duplicate signature is found a SIG14 diagnostic is emitted and recorded
    /// on <c>this.ErrorOccurred</c>.
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
    /// Run consistency checks for the <c>cases</c> node.
    /// </summary>
    /// <remarks>
    /// Invokes base consistency checks and reachability validation for case conditions.
    /// </remarks>
    override this.CheckConsistency() = 
        base.CheckConsistency()
        this.CheckAllCasesForBeingReachable()

    /// <summary>
    /// Embed the <c>cases</c> expression into its parent after performing consistency checks.
    /// </summary>
    /// <param name="_">Unused parameter (conventional signature).</param>
    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        addExpressionToParentArgList this

    /// <summary>
    /// Evaluate the <c>cases</c> statement at runtime.
    /// </summary>
    /// <remarks>
    /// Conditions are evaluated in source order. The first case whose condition evaluates to
    /// <c>True</c> has its statements executed. If no condition matches, the else branch runs.
    /// </remarks>
    override this.Run() = 
        StaticDebug.Debug(this,Debug.Start)
        let resultLst = this.GetConditionResultList()
        let elseStmt = this.GetElseStmt()
        let firstCaseWithTrueConditionOpt = 
            resultLst
            |> Seq.tryFind(fun caseSingle -> 
                let condition = caseSingle.GetCondition()
                condition.Run()
                condition.Represent() = LiteralTrue
            )
        match firstCaseWithTrueConditionOpt with
        | Some firstCaseWithTrueCondition -> 
            firstCaseWithTrueCondition.Run()
        | None -> 
            elseStmt.Run()
        StaticDebug.Debug(this,Debug.Stop)
