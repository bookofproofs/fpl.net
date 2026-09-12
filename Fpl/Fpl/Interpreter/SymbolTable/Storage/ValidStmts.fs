(* Copyright (c) 2021+ bookofproofs See LICENSE in the project root for license terms. *)

/// <summary>
/// Module containing classes used to store valid statements globally in the Fpl.Interpreter namespace.
/// </summary>
/// <remarks>
/// The module provides an in-memory store of axioms, theorems, inference rules and derived arguments
/// and exposes helpers to register expressions, query the last assumed argument, and serialize the store.
/// </remarks>
module Fpl.Interpreter.SymbolTable.Storage.ValidStmts
open System.Collections.Generic
open System.Text.Json
open Fpl.Primitives
open Fpl.Interpreter.BasicTypes

/// <summary>
/// In-memory store for valid statements (axioms, theorems, inference rules, derived arguments).
/// </summary>
/// <remarks>
/// The store preserves derived-assumed arguments on an internal stack so derived/revoke operations
/// can be reconciled. Designed for use by the interpreter and tooling (language server).
/// </remarks>
type ValidStmtStore() =
    let _theoremStore = Dictionary<string, ValidStatement>()
    let _assumedArguments = Stack<FplGenericNode>()

    /// <summary>
    /// Registers an expression node in the validity store.
    /// </summary>
    /// <param name="st">The <see cref="FplGenericNode"/> representing the expression to register.</param>
    /// <returns>
    /// <c>true</c> if the expression was accepted and stored (or pushed as an assumed argument);
    /// <c>false</c> if the node is not a valid candidate for storage.
    /// </returns>
    member this.RegisterExpression (st:FplGenericNode) =
        match box st with
        | :? IValid as infer ->
            let validStmt = infer.ValidExpression
            match validStmt.ValidityReason with
            | ValidityReason.Error -> false // do nothing if error was flagged
            | ValidityReason.IsRuleOfInference(pre, con) ->
                _theoremStore.TryAdd($"{pre}|{con}", validStmt) |> ignore
                true
            | ValidityReason.IsDerivedAssumed assumption ->
                _assumedArguments.Push st
                _theoremStore.TryAdd(assumption, validStmt) |> ignore
                true
            | ValidityReason.IsAxiom expr 
            | ValidityReason.IsAxiomAssertion expr 
            | ValidityReason.IsTheorem expr 
            | ValidityReason.IsDerived expr ->
                _theoremStore.TryAdd(expr, validStmt) |> ignore
                true
            | ValidityReason.IsDerivedRevoke (assumedExpr,revokedExpr) ->
                if _assumedArguments.Count > 0 then
                    _assumedArguments.Pop() |> ignore
                    _theoremStore.Remove assumedExpr |> ignore 
                _theoremStore.TryAdd(revokedExpr, validStmt) |> ignore
                true
        | _ -> false

    /// <summary>
    /// Returns the most recently pushed assumed argument, if any.
    /// </summary>
    /// <returns>
    /// <c>Some(FplGenericNode)</c> containing the last assumed argument, or <c>None</c> when none exists.
    /// </returns>
    member this.LastAssumedArgument =
        if _assumedArguments.Count > 0 then
            Some (_assumedArguments.Peek())
        else 
            None

    /// <summary>
    /// Produces a JSON string representing all stored valid statements as a single merged array.
    /// </summary>
    /// <returns>A JSON string containing serialized entries for all stored valid statements.</returns>
    /// <remarks>
    /// The output flattens axioms, inference rules, and derived arguments into a single array
    /// and includes provenance information when available (theory name, file path, line/column).
    /// </remarks>
    member this.ToJson() =
        let all = ResizeArray<Dictionary<string,string>>()

        let getReason reason =
            match reason with
            | ValidityReason.IsAxiom _ -> LiteralAxL
            | ValidityReason.IsAxiomAssertion _ -> PrimAssertion
            | ValidityReason.IsRuleOfInference _ -> PrimRuleOfInference
            | ValidityReason.IsTheorem _ -> PrimTitleTheorems
            | ValidityReason.IsDerived _ -> PrimArgInfDerive
            | ValidityReason.IsDerivedAssumed _ -> PrimArgInfAssume
            | ValidityReason.IsDerivedRevoke _ -> PrimArgInfRevoke
            | ValidityReason.Error -> "Error"

        let getExpr reason =
            match reason with
            | ValidityReason.IsAxiom expr 
            | ValidityReason.IsAxiomAssertion expr
            | ValidityReason.IsTheorem expr
            | ValidityReason.IsDerived expr
            | ValidityReason.IsDerivedAssumed expr -> expr
            | ValidityReason.IsRuleOfInference (preExpr,conExpr) -> $"{preExpr}/{conExpr}"
            | ValidityReason.IsDerivedRevoke (_,revokedExpr) -> revokedExpr
            | ValidityReason.Error -> "Error"

        for kvp in _theoremStore do
            let stmt = kvp.Value
            let obj = Dictionary<string,string>()
            obj.Add("statementExpression", getExpr stmt.ValidityReason)
            obj.Add("reason", getReason stmt.ValidityReason)
            let ultimateNodeOpt = stmt.Node.UltimateBlockNode
            match ultimateNodeOpt with
            | Some ultimateNode when ultimateNode.Parent.IsSome ->
                match ultimateNode.Parent with
                | Some theory ->
                    obj.Add("blockName", ultimateNode.Type SignatureType.Mixed)
                    obj.Add("theoryName", theory.FplId)
                    match theory.FilePath with
                    | Some filePath ->
                        obj.Add("FilePath", filePath)
                        obj.Add("Line", $"{stmt.Node.StartPos.Line}")
                        obj.Add("Column", $"{stmt.Node.StartPos.Column}")
                    | _ -> ()
                | _ -> ()
            | _ -> ()
            all.Add(obj)

        JsonSerializer.Serialize(all)

    /// <summary>
    /// Clears the internal validity store and the assumed-arguments stack.
    /// </summary>
    member this.ClearValidityStore() =
        _theoremStore.Clear() 
        _assumedArguments.Clear()

    /// <summary>
    /// Gets the number of stored valid statements in the internal theorem dictionary.
    /// </summary>
    member this.Count = _theoremStore.Count

