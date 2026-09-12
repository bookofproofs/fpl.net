(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)
/// <summary>
/// Module providing symbol-table nodes for handling `self`, `parent` and base-constructor calls
/// within FPL class/definition contexts.
/// </summary>
/// <remarks>
/// Contains implementations for:
/// - `FplBaseConstructorCall` — representing explicit calls to parent class constructors,
/// - `FplParent` — the `parent` reference usable inside properties/constructors,
/// - `FplSelf` — the `self` reference usable inside extensions, predicates and functional terms.
///
/// These nodes perform consistency checks, integrate with symbol-table embedding helpers and
/// emit diagnostics via the emitter helpers. Runtime evaluation for these nodes is intentionally
/// lightweight because their main purpose is symbol-table semantics and diagnostics.
/// </remarks>
module Fpl.Interpreter.SymbolTable.Types3.SelfParent
open Fpl.Primitives
open Fpl.Parser.Types
open Fpl.Errors.Messages
open Fpl.Errors.Emitter
open Fpl.Interpreter.BasicTypes
open Fpl.Interpreter.Helpers.Basic
open Fpl.Interpreter.Helpers.Checks
open Fpl.Interpreter.SymbolTable.Storage.Util
open Fpl.Interpreter.SymbolTable.Types2.References
open Fpl.Interpreter.SymbolTable.Types2.Definitions
open Fpl.Interpreter.SymbolTable.TypeMatching

/// <summary>
/// Represents a call to a parent/base class constructor within a class constructor.
/// </summary>
/// <param name="positions">Source start/end positions for diagnostics.</param>
/// <param name="parent">Parent AST/symbol node (the constructor node).</param>
/// <remarks>
/// Validates that the referenced base class exists among the derived class bases,
/// tries to resolve an appropriate constructor of the parent class and registers the call
/// to avoid duplicate or missing parent-constructor diagnostics.
/// </remarks>
type FplBaseConstructorCall(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericReference(positions, parent)

    do 
        this.FplId <- LiteralObj
        this.TypeId <- LiteralObj

    /// <summary>
    /// Name used for pattern matching and diagnostics.
    /// </summary>
    override this.Name = PrimBaseConstructorCall
    override this.ShortName = PrimStmt

    /// <summary>
    /// Return the textual/type representation using function-like notation.
    /// </summary>
    /// <param name="signatureType">Requested signature rendering mode.</param>
    /// <returns>Representation string for the base constructor call.</returns>
    override this.Type signatureType = 
        let head = getFplHead this signatureType
        let propagate = propagateSignatureType signatureType
        let args = signatureSep ", " this.ArgList propagate
        sprintf "%s(%s)" head args

    /// <summary>
    /// Perform consistency checks for the base-constructor call and attempt to resolve the target constructor.
    /// </summary>
    /// <remarks>
    /// - Ensures the referenced base class name is present among the current class bases.
    /// - For intrinsic parent classes creates an implicit default constructor when the call uses no parameters.
    /// - Emits diagnostics:
    ///   - <c>ID021</c> for duplicate parent-constructor calls,
    ///   - <c>ID022</c> for invalid parameter usage for intrinsic classes,
    ///   - <c>ID017</c> when the referenced base class is missing or unresolved.
    /// </remarks>
    override this.CheckConsistency() = 
        base.CheckConsistency()

        // Check the base constructor call's id is the same as one of the classes this class is derived from,
        let outerClassOpt = this.UltimateBlockNode
        let enclosingConstructorOpt = this.NextBlockNode

        let registerParentConstructor() =
            match enclosingConstructorOpt with 
            | Some (:? FplConstructor as ctor) ->
                if ctor.ParentConstructorCalls.Contains(this.FplId) then 
                    // issue duplicate constructor call diagnostics
                    this.ErrorOccurred <- emitID021Diagnostics this.FplId this.StartPos this.EndPos
                else
                    ctor.ParentConstructorCalls.Add this.FplId |> ignore
            | _ -> ()

        match outerClassOpt with
        | Some (:? FplClass as outerClass) ->
            let baseClassObjectOpt = 
                outerClass.ArgList 
                |> Seq.filter (fun pc -> pc.FplId = this.FplId)
                |> Seq.tryHead
                |> Option.map (fun (pc:FplGenericNode) -> pc :?> FplBase)

            let classCandidates =
                outerClass.ArgList
                |> Seq.map (fun fv ->
                    match fv.RefersTo with
                    | Some cl -> qualifiedNameSimple cl
                    | None -> $"unknown class `{fv.FplId}`")
                |> Seq.sort |> numbered

            match baseClassObjectOpt with 
            | Some baseClassObject ->
                match baseClassObject.RefersTo with
                | Some baseClass ->
                    // now, try to match a constructor of the parentClass based on the signature of this base constructor call
                    match baseClass.IsIntrinsic, this.ArgList.Count with
                    | true, 0 ->
                        // call of a constructor of an intrinsic class (i.e., that is missing any constructor) with 0 parameters
                        // add "default constructor reference"
                        let defaultConstructor = new FplDefaultConstructor(baseClass.FplId, (this.StartPos, this.EndPos), this)
                        defaultConstructor.EmbedInSymbolTable defaultConstructor.Parent
                        defaultConstructor.ToBeConstructedClass <- Some baseClass
                        registerParentConstructor()
                    | true, _ ->
                        // the call uses parameters that are not possible for calling a non-existing constructor 
                        // obj() or an intrinsic class
                        this.ErrorOccurred <- emitID022Diagnostics baseClass.FplId this.StartPos this.EndPos
                    | false, _ ->
                        let parentClass = baseClass :?> FplClass
                        let constructors = parentClass.GetConstructors()
                        match checkSIG04Diagnostics this constructors with
                        | Some ctor ->
                            let name = ctor.Type SignatureType.Mixed
                            this.Scope.TryAdd(name, ctor) |> ignore
                        | None -> ()
                        registerParentConstructor()
                | None ->
                    // The base class is syntactically present, but the declaration is unresolved.
                    // Still count the constructor invocation so ID020 is not raised incorrectly.
                    registerParentConstructor()
                    this.ErrorOccurred <- emitID017Diagnostics this.FplId classCandidates true this.StartPos this.EndPos
            | _ ->
                    this.ErrorOccurred <- emitID017Diagnostics this.FplId classCandidates true this.StartPos this.EndPos
                    registerParentConstructor()
        | _ ->
            // this case never happens, 
            // if so the bug will become apparent by failing to call the parent class constructor
            () 


    /// <summary>
    /// Embed the base-constructor-call into its parent (constructor) argument list after consistency checks.
    /// </summary>
    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        addExpressionToParentArgList this

/// <summary>
/// Reference node representing the FPL `parent` keyword.
/// </summary>
/// <param name="positions">Source positions for diagnostics.</param>
/// <param name="parent">Parent AST/symbol node.</param>
/// <remarks>
/// `parent` is only valid within specific property/constructor contexts. This node resolves
/// to the referenced parent node when available and emits diagnostics for incorrect use.
/// Cloning is intentionally a no-op to avoid recursion/stack overflow.
/// </remarks>
type FplParent(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericNode(positions, Some parent)
    let mutable _callCounter = 0

    do 
        this.FplId <- LiteralParent
        this.TypeId <- LiteralUndef

    override this.Name = LiteralParent
    override this.ShortName = LiteralParent

    /// <summary>
    /// Prevent cloning of parent reference nodes to avoid recursion issues.
    /// </summary>
    override this.Clone() = this // do not clone FplParent to prevent stack overflow 

    /// <summary>
    /// Return the effective type of the referenced parent node when resolved.
    /// </summary>
    /// <param name="signatureType">Requested signature rendering mode.</param>
    /// <returns>Type or literal parent when unresolved.</returns>
    override this.Type signatureType = 
        match this.RefersTo with 
        | Some ref -> ref.Type signatureType
        | _ -> LiteralParent

    /// <summary>
    /// Represent the referenced parent as its referent's representation.
    /// </summary>
    /// <returns>Representation string or <c>LiteralUndet</c> when unresolved or recursive.</returns>
    override this.Represent() = // done
        match this.RefersTo with 
        | Some ref -> 
            if _callCounter > maxRecursion then
                this.ErrorOccurred <- emitLG002Diagnostics (this.Type(SignatureType.Name)) _callCounter this.StartPos this.EndPos
                LiteralUndet
            else
                _callCounter <- _callCounter + 1
                let result = ref.Represent()
                _callCounter <- _callCounter - 1
                result
        | _ -> LiteralUndet

    /// <summary>
    /// `parent` has no runtime action by itself; value semantics follow the resolved referent.
    /// </summary>
    override this.Run() = 
        // FplParent has no value, unless it has a representable RefersTo
        ()

    /// <summary>
    /// Determine the enclosing block that `parent` should point to, and the expected context.
    /// </summary>
    /// <returns>
    /// <see cref="ScopeSearchResult.Found"/> with the block when `parent` is used inside a supported property/constructor context,
    /// <see cref="ScopeSearchResult.FoundIncorrectBlock"/> when used in an incorrect block, or <see cref="ScopeSearchResult.NotFound"/>.
    /// </returns>
    member this.ParentBlock =
        match this.UltimateBlockNode, this.NextBlockNode with
        | Some block, Some nextBlock ->
            match block.Name, nextBlock.Name with 
            | PrimClassL, LiteralCtorL 
            | PrimClassL, PrimMandatoryFunctionalTermL
            | PrimClassL, PrimMandatoryPredicateL
            | PrimPredicateL, PrimMandatoryFunctionalTermL
            | PrimPredicateL, PrimMandatoryPredicateL
            | PrimFunctionalTermL, PrimMandatoryFunctionalTermL
            | PrimFunctionalTermL, PrimMandatoryPredicateL ->
                ScopeSearchResult.Found block
            | _ ->
                ScopeSearchResult.FoundIncorrectBlock block
        | _ ->
            ScopeSearchResult.NotFound

    /// <summary>
    /// Validate correct usage of `parent` (emits ID015 for incorrect contexts).
    /// </summary>
    override this.CheckConsistency (): unit =
        match this.ParentBlock with
        | ScopeSearchResult.FoundIncorrectBlock block ->
            this.ErrorOccurred <- emitID015Diagnostics $"{block.Name} `{block.Type(SignatureType.Name)}`" this.StartPos this.EndPos
        | _ -> ()
        base.CheckConsistency()

    /// <summary>
    /// Embed this reference into the parent as an expression reference after checks.
    /// </summary>
    override this.EmbedInSymbolTable _ =
        this.CheckConsistency()
        addExpressionToReference this

    override this.RunOrder = None


/// <summary>
/// Reference node representing the FPL `self` keyword.
/// </summary>
/// <param name="positions">Source start/end positions for diagnostics.</param>
/// <param name="parent">Parent AST/symbol node.</param>
/// <remarks>
/// `self` resolves to the enclosing block (extension, predicate, functional term, class) when used
/// in supported contexts. The node performs consistency checks and returns the referent's representation
/// when requested. Cloning is avoided to prevent recursion/stack overflow.
/// </remarks>
type FplSelf(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericNode(positions, Some parent)
    let mutable _callCounter = 0

    do 
        this.FplId <- LiteralSelf
        this.TypeId <- LiteralUndef

    override this.Name = LiteralSelf
    override this.ShortName = LiteralSelf

    /// <summary>
    /// Prevent cloning of `self` to avoid recursion issues.
    /// </summary>
    override this.Clone() = this // do not clone FplSelf to prevent stack overflow 

    /// <summary>
    /// Return the effective type of `self` by delegating to the resolved referent.
    /// </summary>
    /// <param name="signatureType">Requested signature rendering mode.</param>
    /// <returns>Resolved type or <c>LiteralSelf</c> when unresolved.</returns>
    override this.Type signatureType = 
        match this.RefersTo with 
        | Some ref -> ref.Type signatureType
        | _ -> LiteralSelf

    /// <summary>
    /// Represent the referenced `self` as the representation of its referent.
    /// </summary>
    /// <returns>Representation string or <c>LiteralUndet</c> on recursion/unresolved referent.</returns>
    override this.Represent() = // done
        match this.RefersTo with 
        | Some ref -> 
            if _callCounter > maxRecursion then
                this.ErrorOccurred <- emitLG002Diagnostics (this.Type(SignatureType.Name)) _callCounter this.StartPos this.EndPos
                LiteralUndet
            else
                _callCounter <- _callCounter + 1
                let result = ref.Represent()
                _callCounter <- _callCounter - 1
                result
        | _ -> LiteralUndet

    /// <summary>
    /// `self` has no runtime action; its semantics are provided by its referent when resolved.
    /// </summary>
    override this.Run() = 
        // FplSelf has no value, unless it has a representable RefersTo
        ()

    /// <summary>
    /// Determine the block `self` should refer to based on the following node context.
    /// </summary>
    /// <returns>
    /// <see cref="ScopeSearchResult.Found"/> with the block when `self` is used correctly,
    /// <see cref="ScopeSearchResult.FoundIncorrectBlock"/> when used in an incorrect block,
    /// or <see cref="ScopeSearchResult.NotFound"/>.
    /// </returns>
    member this.SelfBlock = 
        match this.NextBlockNode with
        | Some block ->
            match block.Name with 
            | PrimExtensionL
            | PrimMandatoryFunctionalTermL
            | PrimMandatoryPredicateL
            | PrimClassL
            | PrimPredicateL
            | PrimFunctionalTermL -> ScopeSearchResult.Found block
            | _ -> ScopeSearchResult.FoundIncorrectBlock block
        | _ -> ScopeSearchResult.NotFound

    /// <summary>
    /// Validate correct usage of `self` and emit diagnostic ID016 when used in an invalid block.
    /// </summary>
    override this.CheckConsistency () =
        match this.SelfBlock with
        | ScopeSearchResult.FoundIncorrectBlock block ->
            this.ErrorOccurred <- emitID016Diagnostics $"{block.Name} `{block.Type(SignatureType.Name)}`" this.StartPos this.EndPos
        | _ -> ()
        base.CheckConsistency()

    /// <summary>
    /// Embed this reference into the parent as an expression reference after checks.
    /// </summary>
    override this.EmbedInSymbolTable _ =
        this.CheckConsistency()
        addExpressionToReference this

    override this.RunOrder = None

