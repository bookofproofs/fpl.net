(* Copyright (c) 2021+ bookofproofs See LICENSE in the project root for license terms. *)

/// <summary>
/// Module containing symbol-table node implementations for definition properties such as
/// mandatory predicates and mandatory functional terms used inside FPL definitions.
/// </summary>
/// <remarks>
/// Types in this module model properties that are part of definitions (classes, predicates,
/// functional terms) and provide symbol-table embedding, signature checks, runtime
/// evaluation semantics and diagnostic emission. They integrate with the valid-statement
/// store and other interpreter subsystems (heap, emitters, helpers).
/// </remarks>
module Fpl.Interpreter.SymbolTable.Types3.DefinitionProperties
open FParsec
open Fpl.Primitives
open Fpl.Parser.Types
open Fpl.Errors.Emitter
open Fpl.Interpreter.BasicTypes
open Fpl.Interpreter.Helpers.Checks
open Fpl.Interpreter.Helpers.Basic
open Fpl.Interpreter.Helpers.Debug
open Fpl.Interpreter.SymbolTable.Storage.Heap
open Fpl.Interpreter.SymbolTable.Types2.Variables
open Fpl.Interpreter.SymbolTable.Types2.Definitions
open Fpl.Interpreter.SymbolTable.Types3.PredicativeBlocks

/// <summary>
/// Mandatory predicate property used inside definitions.
/// </summary>
/// <param name="positions">Tuple of start and end source positions used for diagnostics.</param>
/// <param name="parent">Parent AST/symbol node.</param>
/// <remarks>
/// Mandatory predicates behave like predicative blocks that must be available to the parent definition.
/// They are callable and support recursion detection using a call counter. Consistency checks include
/// variable-usage diagnostics when the node is not intrinsic.
/// </remarks>
type FplMandatoryPredicate(positions: Positions, parent: FplGenericNode) =
    inherit FplGenericPredicateWithExpression(positions, parent)
    let mutable _isReady = false
    let mutable _callCounter = 0

    override this.Name = PrimMandatoryPredicateL
    override this.ShortName = PrimMandatoryPredicate

    /// <summary>
    /// Create a deep copy of this mandatory predicate node.
    /// </summary>
    /// <returns>A cloned <c>FplMandatoryPredicate</c> instance.</returns>
    override this.Clone () =
        let ret = new FplMandatoryPredicate((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    interface IReady with
        member _.IsReady = _isReady

    interface ICanBeCalledRecusively with
        member _.CallCounter = _callCounter

    /// <summary>
    /// Mandatory predicates are block-like nodes.
    /// </summary>
    override this.IsBlock () = true

    /// <summary>
    /// Return the textual/type signature of the mandatory predicate including its parameter tuple.
    /// </summary>
    /// <param name="signatureType">Rendering mode for signature.</param>
    /// <returns>Formatted signature string.</returns>
    override this.Type signatureType = 
        let head = getFplHead this signatureType

        let paramT = getParamTuple this signatureType
        sprintf "%s(%s)" head paramT

    /// <summary>
    /// Perform consistency checks for this mandatory predicate.
    /// </summary>
    /// <remarks>
    /// When the node is not intrinsic, unused-variable diagnostics (VAR04) are emitted.
    /// </remarks>
    override this.CheckConsistency () = 
        if not this.IsIntrinsic then // if not intrinsic, check variable usage
            checkVAR04Diagnostics this

    /// <summary>
    /// Embed the mandatory predicate as a sub-block of the containing FPL block.
    /// </summary>
    override this.EmbedInSymbolTable _ = 
        base.CheckConsistency()
        tryAddSubBlockToFplBlock this
    
    /// <summary>
    /// Execute the mandatory predicate: guard against recursion, run intrinsic handling or run-body semantics.
    /// </summary>
    /// <remarks>
    /// Uses a call-counter to detect recursion (emits LG002 when exceeded). If intrinsic, intrinsic predicate
    /// semantics are applied; otherwise arguments are run and the last value is used. The node becomes ready
    /// when its arity reaches zero.
    /// </remarks>
    override this.Run() = 
        StaticDebug.Debug(this,Debug.Start)
        if not _isReady then
            _callCounter <- _callCounter + 1
            if _callCounter > maxRecursion then
                this.SetDefaultValue()
                this.ErrorOccurred <- emitLG002Diagnostics (this.Type(SignatureType.Name)) _callCounter heap.Helper.CallerStartPos heap.Helper.CallerEndPos
            else
                if this.IsIntrinsic then 
                    runIntrinsicPredicate this
                else
                    runArgsAndSetWithLastValue this

            _callCounter <- _callCounter - 1
            _isReady <- this.Arity = 0
        StaticDebug.Debug(this,Debug.Stop)

/// <summary>
/// Mandatory functional term property used inside definitions (must be present).
/// </summary>
/// <param name="positions">Tuple of start and end source positions used for diagnostics.</param>
/// <param name="parent">Parent AST/symbol node.</param>
/// <remarks>
/// Represents a functional term that is required by the containing definition. Provides
/// signature tracking, optional constant naming, recursion detection and default value handling.
/// Consistency checks include signature/mapping checks (SIG11) and variable-usage diagnostics.
/// </remarks>
type FplMandatoryFunctionalTerm(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericHasValue(positions, parent)
    let mutable _signStartPos = Position("", 0L, 0L, 0L)
    let mutable _signEndPos = Position("", 0L, 0L, 0L)
    let mutable _isReady = false
    let mutable _callCounter = 0
    let mutable _constantName = ""

    do 
        this.FplId <- LiteralFunc
        this.TypeId <- LiteralFunc

    override this.Name = PrimMandatoryFunctionalTermL
    override this.ShortName = PrimMandatoryFunctionalTerm

    /// <summary>
    /// Signature start position used for diagnostics.
    /// </summary>
    member this.SignStartPos
        with get() = _signStartPos
        and set(value) = _signStartPos <- value

    /// <summary>
    /// Signature end position used for diagnostics.
    /// </summary>
    member this.SignEndPos
        with get() = _signEndPos
        and set(value) = _signEndPos <- value

    interface IHasSignature with
        member this.SignStartPos 
            with get () = this.SignStartPos
            and set (value) = this.SignStartPos <- value
        member this.SignEndPos 
            with get () = this.SignEndPos
            and set (value) = this.SignEndPos <- value

    interface IReady with
        member _.IsReady = _isReady

    /// <summary>
    /// Optional constant name used when the functional term is treated as a constant.
    /// </summary>
    member this.ConstantName = _constantName
    member this.SetConstantName() = _constantName <- signatureRepresent this

    interface IConstant with
        member this.ConstantName = this.ConstantName 
        member this.SetConstantName() = this.SetConstantName() 

    /// <summary>
    /// Create a deep copy of this mandatory functional term node.
    /// </summary>
    override this.Clone () =
        let ret = new FplMandatoryFunctionalTerm((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.IsBlock () = true

    /// <summary>
    /// Compute the textual signature for the mandatory functional term including mapping return type.
    /// </summary>
    /// <param name="signatureType">Requested signature rendering mode.</param>
    /// <returns>Formatted signature string or empty when mapping is missing.</returns>
    override this.Type signatureType = 
        let head = getFplHead this signatureType
        let propagate = propagateSignatureType signatureType

        match getMapping this with
        | Some map ->
            let paramT = getParamTuple this signatureType
            sprintf "%s(%s) -> %s" head paramT (map.Type(propagate))
        | _ -> ""

    /// <summary>
    /// Represent the functional term using its value or signature; guards against recursion.
    /// </summary>
    /// <returns>Representation string or <c>LiteralUndet</c> when recursion limit exceeded.</returns>
    override this.Represent() = // done
        if _callCounter > maxRecursion then
            this.ErrorOccurred <- emitLG002Diagnostics (this.Type(SignatureType.Name)) _callCounter this.StartPos this.EndPos
            LiteralUndet
        else
            _callCounter <- _callCounter + 1
            let result = getFunctionalTermRepresent this
            _callCounter <- _callCounter - 1
            result

    /// <summary>
    /// Perform consistency checks: variable usage, mapping checks (SIG11) and base consistency.
    /// </summary>
    override this.CheckConsistency () =
        if not this.IsIntrinsic then // if not intrinsic, check variable usage
            checkVAR04Diagnostics this
        checkSIG11Diagnostics this
        base.CheckConsistency()

    /// <summary>
    /// Embed this mandatory functional term as a sub-block of the containing FPL block.
    /// </summary>
    override this.EmbedInSymbolTable _ =
        this.CheckConsistency()
        tryAddSubBlockToFplBlock this

    override this.RunOrder = None

    /// <summary>
    /// Execute the mandatory functional term: handle recursion, intrinsic initialization or run-body semantics.
    /// </summary>
    /// <remarks>
    /// When intrinsic, a default instance is produced; otherwise the arguments are executed and the last value propagated.
    /// Emits LG002 when recursion limit is exceeded and sets a fallback default instance.
    /// </remarks>
    override this.Run() = 
        StaticDebug.Debug(this,Debug.Start)
        if not _isReady then
            _callCounter <- _callCounter + 1
            if _callCounter > maxRecursion then
                let instance = getDefaultValueOfFunction this
                this.SetValue instance
                this.ErrorOccurred <- emitLG002Diagnostics (this.Type(SignatureType.Name)) _callCounter heap.Helper.CallerStartPos heap.Helper.CallerEndPos
            else
                if this.IsIntrinsic then 
                    runIntrinsicFunction this 
                else
                    runArgsAndSetWithLastValue this
            _callCounter <- _callCounter - 1
            _isReady <- this.Arity = 0 
        StaticDebug.Debug(this,Debug.Stop)

/// <summary>
/// Looks for declared properties or constructors that match a specific name within a building block.
/// </summary>
/// <remarks>
/// The function header is present in the source to support property/constructor lookup logic used
/// elsewhere in the symbol-table handling. Implementation-specific details appear after this point in the file.
/// </remarks>
/// <exception>
/// No exceptions are thrown for semantic diagnostics; diagnostics are emitted via emitter helpers.
/// </exception>
/// Looks for all declared properties or constructors (if any) that equal 
/// the specific name within the building block, whose syntax tree the FplValue `fv` is part of.
let findPropertyCandidatesByNameInBlock (fv: FplGenericNode) (name: string) =
    let rec findDefinition (fv1: FplGenericNode) =
        if isTheory fv1 then
            ScopeSearchResult.NotFound
        elif isDefinition fv1 then 
            ScopeSearchResult.Found fv1
        else 
            match fv1.Parent with
            | Some parent -> findDefinition parent
            | None -> ScopeSearchResult.NotFound

    match findDefinition fv with
    | ScopeSearchResult.Found candidate ->
        candidate.Scope
        |> Seq.filter (fun kvp -> kvp.Value.FplId = name)
        |> Seq.map (fun kvp -> kvp.Value)
        |> Seq.toList
    | _ -> []
