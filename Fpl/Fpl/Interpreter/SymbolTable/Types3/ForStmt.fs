(* Copyright (c) 2021+ bookofproofs See LICENSE in the project root for license terms. *)

/// <summary>
/// Module containing symbol-table node types that model FPL `for` statements and their parts.
/// </summary>
/// <remarks>
/// Provides nodes for the `for ... in ... do` style construct including entity, domain and body handling.
/// The types implement symbol-table embedding, runtime iteration semantics and diagnostics emission.
/// </remarks>
module Fpl.Interpreter.SymbolTable.Types3.ForStmt
open Fpl.Primitives
open Fpl.Parser.Types
open Fpl.Errors.Emitter
open Fpl.Interpreter.BasicTypes
open Fpl.Interpreter.Helpers.Basic
open Fpl.Interpreter.Helpers.Debug
open Fpl.Interpreter.SymbolTable.Types2.Intrinsic
open Fpl.Interpreter.SymbolTable.Types2.Variables
open Fpl.Interpreter.SymbolTable.Types2.References

/// <summary>
/// Enumerates the supported kinds of iteration for a <c>for in</c> statement.
/// </summary>
type FplForEnumeratorType = 
    | ArrayElements
    | Predicative
    | Error

/// <summary>
/// Represents a <c>for</c>-in statement node in the symbol table.
/// </summary>
/// <param name="positions">Source start and end positions used for diagnostics.</param>
/// <param name="parent">Parent node in the symbol table.</param>
/// <remarks>
/// The node expects its first argument to be the iteration entity (variable reference),
/// the second argument to be the domain (e.g. an array) and the remaining arguments make up the loop body.
/// </remarks>
type FplForInStmt(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericStmt(positions, parent)
    do 
        this.FplId <- LiteralFor

    /// <summary>
    /// Name of the AST element used for diagnostics and pattern matching.
    /// </summary>
    override this.Name = PrimForInStmtL

    /// <summary>
    /// Clone this node preserving positions and parent.
    /// </summary>
    /// <returns>A cloned <c>FplForInStmt</c> instance.</returns>
    override this.Clone () =
        let ret = new FplForInStmt((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    /// <summary>
    /// Returns the type head for the for statement.
    /// </summary>
    /// <param name="signatureType">Requested signature rendering mode.</param>
    /// <returns>String representing the head of the node according to <paramref name="signatureType"/>.</returns>
    override this.Type signatureType = 
        getFplHead this signatureType

    /// <summary>
    /// The entity of the for statement (the loop variable/reference), if present.
    /// </summary>
    /// <returns>Optional referenced node for the entity argument.</returns>
    member this.Entity =
        if this.ArgList.Count > 0 then 
            this.ArgList[0].RefersTo
        else 
            None

    /// <summary>
    /// The domain expression (collection or predicate) for enumeration, if present.
    /// </summary>
    /// <returns>Optional referenced node for the domain argument.</returns>
    member this.Domain =
        if this.ArgList.Count > 1 then 
            this.ArgList[1].RefersTo 
        else 
            None

    /// <summary>
    /// The body statements of the for statement.
    /// </summary>
    /// <returns>List of statement nodes forming the loop body.</returns>
    member this.Body =
        // the body of the for statement starts after the entity and after the domain
        if this.ArgList.Count > 2 then 
            this.ArgList |> Seq.tail |> Seq.tail |> Seq.toList
        else
            []

    /// <summary>
    /// Determine the enumerator type and provide the list of elements for iteration.
    /// </summary>
    /// <returns>
    /// A tuple of <see cref="FplForEnumeratorType"/> and a list of nodes to enumerate.
    /// If an error occurs a diagnostic is emitted and <c>FplForEnumeratorType.Error</c> is returned.
    /// </returns>
    /// <remarks>
    /// Currently supports enumeration over <c>FplVariableArray</c> values (array elements).
    /// Other domain types produce ST005 diagnostics.
    /// </remarks>
    member this.GetEnumerator() =
        match this.Domain with
        | Some (:? FplVariableArray as domain) ->
            (FplForEnumeratorType.ArrayElements, domain.ValueList |> Seq.toList)
        | Some domain ->
            this.ErrorOccurred <- emitST005Diagnostics (domain.Type SignatureType.Name) domain.Name this.ArgList[1].StartPos this.ArgList[1].EndPos
            (FplForEnumeratorType.Error, [])
        | _ ->
            this.ErrorOccurred <- emitST005Diagnostics "missing" PrimNone this.StartPos this.StartPos
            (FplForEnumeratorType.Error, [])
            
    /// <summary>
    /// Execute the for-statement at runtime.
    /// </summary>
    /// <remarks>
    /// For array enumeration, sets the entity's value to each array element in turn and runs the body statements.
    /// Type compatibility checks between the entity and enumerated elements are TODO and currently not enforced here.
    /// </remarks>
    override this.Run() = 
        StaticDebug.Debug(this,Debug.Start)
        match this.Entity, this.GetEnumerator() with
        | Some (:? FplGenericHasValue as entity), (FplForEnumeratorType.ArrayElements, lst) ->
            lst
            |> List.iter (fun lstElement ->
                // TODO: check type compatibility of entity accepting lstElement
                entity.Value <- Some lstElement
                this.Body
                |> List.iter (fun stmt ->
                    stmt.Run()
                )
            )
        | _, _ -> ()
        StaticDebug.Debug(this,Debug.Stop)

/// <summary>
/// Node representing the entity position of a for-in statement (the iterated variable/reference).
/// </summary>
/// <param name="positions">Source positions used for diagnostics.</param>
/// <param name="parent">Parent node in the symbol table.</param>
type FplForInStmtEntity(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericStmt(positions, parent)
    do 
        this.FplId <- PrimForInStmtEntity

    override this.Name = PrimForInStmtEntityL

    override this.Clone () =
        let ret = new FplForInStmtEntity((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    /// <summary>
    /// Returns the effective type of the entity by resolving referenced node if available.
    /// </summary>
    /// <param name="signatureType">Requested rendering mode.</param>
    /// <returns>Resolved type/name or the node head when unresolved.</returns>
    override this.Type signatureType = 
        let entityOpt = referencedNodeOpt this
        match entityOpt with 
        | Some entity -> entity.Type signatureType
        | _ -> getFplHead this signatureType

    /// <summary>
    /// Embed this entity node into the symbol table under a for-in statement context.
    /// </summary>
    override this.EmbedInSymbolTable _ = tryAddToParentForInStmt this

    /// <summary>
    /// Runtime execution for the entity node is a no-op; entity evaluation occurs in the parent for-statement.
    /// </summary>
    override this.Run() = 
        // TODO implement run
        StaticDebug.Debug(this,Debug.Start)
        StaticDebug.Debug(this,Debug.Stop)

/// <summary>
/// Node representing the domain expression of a for-in statement (the collection or predicate to iterate over).
/// </summary>
/// <param name="positions">Source positions used for diagnostics.</param>
/// <param name="parent">Parent node in the symbol table.</param>
type FplForInStmtDomain(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericStmt(positions, parent)
    do 
        this.FplId <- PrimForInStmtDomain

    override this.Name = PrimForInStmtDomainL

    override this.Clone () =
        let ret = new FplForInStmtDomain((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    /// <summary>
    /// Returns the type of the domain by resolving the referenced node when possible.
    /// </summary>
    /// <param name="signatureType">Requested rendering mode.</param>
    /// <returns>Resolved domain type or node head when unresolved.</returns>
    override this.Type signatureType = 
        let domainOpt = referencedNodeOpt this
        match domainOpt with 
        | Some domain -> domain.Type signatureType
        | _ -> getFplHead this signatureType

    /// <summary>
    /// Embed this domain node into the parent's for-in statement context.
    /// </summary>
    override this.EmbedInSymbolTable _ = tryAddToParentForInStmt this

    /// <summary>
    /// Runtime execution for the domain node is a no-op; domain iteration is handled by the parent for-statement.
    /// </summary>
    override this.Run() = 
        // TODO implement run
        StaticDebug.Debug(this,Debug.Start)
        StaticDebug.Debug(this,Debug.Stop)
