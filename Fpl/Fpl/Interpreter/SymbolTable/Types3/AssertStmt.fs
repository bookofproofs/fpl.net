(* Copyright (c) 2021+ bookofproofs See LICENSE in the project root for license terms. *)

/// <summary>
/// Module containing symbol-table nodes that model the FPL <c>assert</c> statement.
/// </summary>
/// <remarks>
/// The assert statement registers an asserted expression with the runtime valid-statement
/// store so it can be used for proof / verification workflows. The module provides the
/// concrete <c>FplAssertion</c> node which implements the required validity reporting
/// contract.
/// </remarks>
module Fpl.Interpreter.SymbolTable.Types3.AssertStmt
open Fpl.Primitives
open Fpl.Parser.Types
open Fpl.Interpreter.BasicTypes
open Fpl.Interpreter.Helpers.Debug
open Fpl.Interpreter.SymbolTable.Storage.Heap
open Fpl.Interpreter.SymbolTable.Types2.Intrinsic


/// <summary>
/// Represents an <c>assert</c> statement node in the symbol table.
/// </summary>
/// <param name="positions">Source start/end positions for the node (used for diagnostics).</param>
/// <param name="parent">Parent AST node in the symbol table.</param>
/// <remarks>
/// The <c>FplAssertion</c> node implements <c>IValid</c> and provides a validity
/// descriptor that identifies the asserted expression as an axiom-style assertion.
/// At runtime the node registers itself with <c>heap.ValidStmtStore</c>.
/// </remarks>
type FplAssertion(positions: Positions, parent: FplGenericNode) =
    inherit FplGenericStmt(positions, parent)

    override this.Name = PrimAssertion
    override this.ShortName = LiteralAss

    /// <summary>
    /// Construct the validity descriptor for this assertion.
    /// </summary>
    /// <returns>
    /// A <c>ValidStatement</c> record containing the node and a
    /// <c>ValidityReason</c> derived from the asserted expression's signature.
    /// </returns>
    /// <remarks>
    /// If the assertion node contains no expression the validity reason falls back
    /// to <c>ValidityReason.Error</c> as a defensive default.
    /// </remarks>
    member this.ValidExpression =
        let validityReason = 
            let exprOpt = this.ArgList |> Seq.tryLast
            match exprOpt with
            | Some expr -> ValidityReason.IsAxiomAssertion (expr.Type SignatureType.Name)
            | _ -> ValidityReason.Error // fallback if axiom node is empty

        {
            ValidStatement.Node = this
            ValidStatement.ValidityReason = validityReason
        }

    interface IValid with
        /// <summary>
        /// Expose the computed validity descriptor for external consumers.
        /// </summary>
        member this.ValidExpression
            with get () = this.ValidExpression

    /// <summary>
    /// Create a shallow clone of this assertion node preserving parts and positions.
    /// </summary>
    /// <returns>A new <c>FplAssertion</c> instance.</returns>
    override this.Clone () =
        let ret = new FplAssertion((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    /// <summary>
    /// Execute the assertion at runtime by registering it in the valid-statement store.
    /// </summary>
    /// <remarks>
    /// Registration is performed by calling <c>heap.ValidStmtStore.RegisterExpression</c>.
    /// No other runtime effects are produced by an assertion node.
    /// </remarks>
    override this.Run() = 
        StaticDebug.Debug(this,Debug.Start)
        heap.ValidStmtStore.RegisterExpression this |> ignore
        StaticDebug.Debug(this,Debug.Stop)

    /// <summary>
    /// Assertions do not participate in ordered execution scheduling.
    /// </summary>
    override this.RunOrder = None

