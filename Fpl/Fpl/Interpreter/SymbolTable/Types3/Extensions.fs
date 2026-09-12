(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 
*)
/// <summary>
/// Module containing FPL symbol-table node types and helpers that model and interpret
/// extensions, return statements and related utilities.
/// </summary>
/// <remarks>
/// Key types:
/// - <c>FplExtensionObj</c>: runtime representation of an extension object reference.
/// - <c>FplReturn</c>: models the <c>return</c> statement inside functional terms/extensions.
/// - <c>FplExtension</c>: definition node for user-defined extensions (single-parameter functional terms).
/// Utility functions help with locating extension definitions and mappings in the symbol table.
/// Diagnostics emitted from this module include ID018 (unknown extension), SIG03, SIG11 and LG002.
/// </remarks>
module Fpl.Interpreter.SymbolTable.Types3.Extensions
open System
open System.Text.RegularExpressions
open FParsec
open Fpl.Primitives
open Fpl.Parser.Types
open Fpl.Errors.Emitter
open Fpl.Interpreter.BasicTypes
open Fpl.Interpreter.Helpers.Checks
open Fpl.Interpreter.Helpers.Basic
open Fpl.Interpreter.Helpers.Debug
open Fpl.Interpreter.SymbolTable.Storage.Heap
open Fpl.Interpreter.SymbolTable.Storage.Util
open Fpl.Interpreter.SymbolTable.Types2.Intrinsic
open Fpl.Interpreter.SymbolTable.Types2.Variables
open Fpl.Interpreter.SymbolTable.Types2.References
open Fpl.Interpreter.SymbolTable.Types2.Definitions
open Fpl.Interpreter.SymbolTable.TypeMatching
open Fpl.Interpreter.SymbolTable.Types3.MapCases



/// <summary>
/// Represents an extension object reference used in FPL source to denote a concrete
/// extension pattern instance or a reference to a named extension.
/// </summary>
/// <param name="positions">Tuple of start/end source positions for diagnostics.</param>
/// <param name="parent">Parent node in the symbol-table AST.</param>
/// <remarks>
/// The node resolves its type based on context:
/// - When used inside the defining extension, its TypeId is set to the extension's name.
/// - When used outside, it attempts to resolve the mapping for the matched extension and uses
///   the mapping's type id if available.
/// If no matching extension exists, an ID018 diagnostic is emitted.
/// </remarks>
type FplExtensionObj(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericIsValue(positions, parent)

    do 
        this.TypeId <- LiteralObj


    override this.Name = PrimExtensionObj
    override this.ShortName = LiteralObj

    /// <summary>
    /// Create a shallow clone of this extension object node preserving parts and positions.
    /// </summary>
    /// <returns>A new <c>FplExtensionObj</c> instance with copied parts.</returns>
    override this.Clone () =
        let ret = new FplExtensionObj((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    /// <summary>
    /// Resolve the static type representation for this extension object under a requested signature type.
    /// </summary>
    /// <param name="signatureType">The requested signature format (Name, Type, Mixed, etc.).</param>
    /// <returns>
    /// - When <c>SignatureType.Type</c> and a mapping exists, delegates to the mapping type.
    /// - Otherwise returns the node's configured <c>TypeId</c> or string representation of the head.
    /// </returns>
    override this.Type signatureType =
        match signatureType with 
        | SignatureType.Type ->
            match this.RefersTo, this.NextBlockNode with
            | Some ext, Some enclosingExtension when not (Object.ReferenceEquals(ext, enclosingExtension)) ->
                // if this FplExtensionObj is being used outside the extension defining its pattern
                this.RefersTo <- Some ext
                // use mapping's type of the extension defining its pattern
                let mappingOpt = getMapping ext
                match mappingOpt with 
                | Some mapping ->
                    mapping.Type SignatureType.Type
                | None ->
                    this.TypeId
            | _, _ ->
                this.TypeId
        | _ ->    
            let head = getFplHead this signatureType
            sprintf "%s" head

    /// <summary>
    /// Return the textual representation used in source for this extension object.
    /// </summary>
    /// <returns>FPL identifier string stored in <c>FplId</c>.</returns>
    override this.Represent() = 
        this.FplId // return FplId

    /// <summary>
    /// Execution does not produce side-effects for an extension object; this is a no-op.
    /// </summary>
    override this.Run() = 
        ()

    /// <summary>
    /// Validate the extension object reference by resolving candidate extension definitions.
    /// </summary>
    /// <remarks>
    /// The routine collects extension definitions visible in the current heap scope and
    /// attempts to match the object's representation against the pattern (regular expression)
    /// stored in extension definitions. If no match is found an ID018 diagnostic is emitted.
    /// When the object is used inside its defining extension, it is bound to that enclosing block.
    /// </remarks>
    /// <exceptions>
    /// <exception>Emits ID018 diagnostic when no matching extension definition is found.</exception>
    /// </exceptions>
    override this.CheckConsistency () = 
        base.CheckConsistency()
        let matchReprId (fv1:FplGenericNode) (identifier:string) = 
            let regex = Regex(fv1.TypeId)
            regex.IsMatch(identifier)
        
        let extensionCandidates =
            heap.Root.Scope
            |> Seq.map (fun theory ->
                theory.Value.Scope
                |> Seq.filter (fun kvp -> kvp.Value.Name = PrimExtensionL)
                |> Seq.map (fun kvp -> kvp.Value)
            )
            |> Seq.concat 
            |> Seq.toList

        let enclosingNode = this.NextBlockNode
        // if this FplExtensionObj happens to be used inside an FplExtension definition, 
        // we add this one to the collection of candidates
        let extensionCandidatesIncludingEnclosing = 
            let parentExtension = enclosingNode
            match parentExtension with 
            | Some ext when ext.Name = PrimExtensionL -> 
                [ext] @ extensionCandidates
            | _ -> 
                extensionCandidates

        let extOpt = 
            extensionCandidatesIncludingEnclosing 
            |> Seq.filter (fun ext -> 
                if matchReprId ext this.FplId then 
                    true
                else
                    false
            )
            // find the first match even, if there are multiple extensions that would match it 
            |> Seq.tryHead

        match extOpt, enclosingNode with
        | None, _ ->
            this.ErrorOccurred <- emitID018Diagnostics this.FplId this.StartPos this.EndPos
        | Some ext, Some enclosingExtension when Object.ReferenceEquals(ext, enclosingExtension) ->
            // if this FplExtensionObj is being used inside the enclosingExtension (i.e., extension defining its pattern)
            this.RefersTo <- Some enclosingExtension
            // we set the type if this FplExtensionObj to the name (FplId) of the enclosing extension
            this.TypeId <- enclosingExtension.FplId
        | Some ext, _ ->
            // if this FplExtensionObj is being used outside the extension defining its pattern
            this.RefersTo <- Some ext
            // we set the type if this FplExtensionObj to the mapping's type of the extension defining its pattern
            let mappingOpt = getMapping ext
            match mappingOpt with
            | Some mapping -> this.TypeId <- mapping.TypeId
            | _ -> ()

    /// <summary>
    /// Embed this expression as a reference in the parent symbol-table context after checks.
    /// </summary>
    /// <param name="_">Unused parameter (conventional signature).</param>
    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()    
        addExpressionToReference this

    override this.RunOrder = None


/// <summary>
/// Represents the <c>return</c> statement inside functional terms or extensions.
/// </summary>
/// <param name="positions">Tuple of start/end source positions for diagnostics.</param>
/// <param name="parent">Parent AST node.</param>
/// <remarks>
/// The return node validates the returned expression against the enclosing functional term's
/// mapping type (SIG03 diagnostics). Supported returned forms include intrinsic literals,
/// references, map-cases expressions and others; when no explicit match occurs a default value
/// is returned.
/// </remarks>
type FplReturn(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericHasValue(positions, parent)

    do
        this.FplId <- LiteralRet
        this.TypeId <- LiteralUndef

    override this.Name = PrimReturn
    override this.ShortName = PrimStmt

    /// <summary>
    /// Create a shallow clone of this return node preserving parts and positions.
    /// </summary>
    /// <returns>A new <c>FplReturn</c> instance with copied parts.</returns>
    override this.Clone () =
        let ret = new FplReturn((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    /// <summary>
    /// Return the textual identity used for the return node type resolution.
    /// </summary>
    /// <param name="signatureType">Requested signature type (unused).</param>
    /// <returns>Identifier string for the return node.</returns>
    override this.Type signatureType = this.FplId

    /// <summary>
    /// Attach the return expression to the parent's argument list.
    /// </summary>
    /// <param name="_">Unused parameter (conventional signature).</param>
    override this.EmbedInSymbolTable _ = addExpressionToParentArgList this

    override this.RunOrder = None

    /// <summary>
    /// Execute the return statement: validate and evaluate the returned expression and
    /// set this node's value accordingly.
    /// </summary>
    /// <remarks>
    /// The method checks the mapping of the enclosing functional term and validates the returned
    /// expression via <c>FplTypeMatcher.MatchPwA</c>. Depending on the concrete returned node
    /// type it may run nested nodes (references, map-cases) or set a default value.
    /// </remarks>
    /// <exceptions>
    /// <exception>
    /// Emits SIG03 diagnostics when the returned expression does not match the functional term's mapping.
    /// </exception>
    /// </exceptions>
    override this.Run() =
        StaticDebug.Debug(this,Debug.Start)
        let returnedReference = this.ArgList[0]
        let blockOpt = this.NextBlockNode
        match blockOpt with 
        | Some funTerm ->
            let mapTypeOpt = getMapping funTerm
            match mapTypeOpt with 
            | Some mapType ->
                match FplTypeMatcher.MatchPwA [ returnedReference ] [ mapType ] with
                | Some errMsg -> returnedReference.ErrorOccurred <- emitSIG03Diagnostics errMsg (returnedReference.StartPos) (returnedReference.EndPos)
                | _ -> 
                    match returnedReference with
                    | :? FplIntrinsicTrue 
                    | :? FplIntrinsicFalse
                    | :? FplIntrinsicTpl 
                    | :? FplIntrinsicInd 
                    | :? FplIntrinsicUndef 
                    | :? FplUndetermined ->
                        this.SetValue returnedReference
                    | :? FplReference as ref ->
                        ref.Run()
                        this.SetValueOf ref
                    | :? FplMapCases as mapCases -> 
                        mapCases.Run()
                        this.SetValueOf mapCases
                    | _ ->
                        this.SetDefaultValue()
            | _ -> 
                // should syntactically not occur that a functional term has no mapping
                // in this case return default value
                this.SetDefaultValue()
        | _ -> 
            // should syntactically not occur that a return statement occurs in something else
            // than a functional term
            // in this case return default value
            this.SetDefaultValue()
        StaticDebug.Debug(this,Debug.Stop)

/// <summary>
/// Represents an extension definition (single-parameter functional term) in the symbol table.
/// </summary>
/// <param name="positions">Tuple of start/end source positions for diagnostics.</param>
/// <param name="parent">Parent AST node.</param>
/// <param name="runOrder">Execution order used when running the extension.</param>
/// <remarks>
/// Extensions behave like single-parameter functions. They maintain a call counter to detect
/// excessive recursion and emit LG002 when recursion limits are exceeded. Consistency checks
/// include SIG11 validation for mixed signatures.
/// </remarks>
type FplExtension(positions: Positions, parent: FplGenericNode, runOrder) =
    inherit FplGenericHasValue(positions, parent)
    let _runOrder = runOrder
    let mutable _callCounter = 0
    let mutable _signStartPos = Position("", 0L, 0L, 0L)
    let mutable _signEndPos = Position("", 0L, 0L, 0L)
    
    interface IHasSignature with
        member _.SignStartPos 
            with get (): Position = _signStartPos
            and set (value) = _signStartPos <- value
        member _.SignEndPos 
            with get (): Position = _signEndPos
            and set (value) = _signEndPos <- value

    override this.Name = PrimExtensionL 
    override this.ShortName = PrimExtension

    /// <summary>
    /// Create a shallow clone of this extension definition preserving parts and positions.
    /// </summary>
    /// <returns>A new <c>FplExtension</c> instance with copied parts.</returns>
    override this.Clone () =
        let ret = new FplExtension((this.StartPos, this.EndPos), this.Parent.Value, _runOrder)
        this.AssignParts(ret)
        ret

    /// <summary>
    /// Returns the mapping node associated with this extension, creating a placeholder mapping
    /// when none exists.
    /// </summary>
    /// <returns>
    /// The <c>FplMapping</c> that represents the extension's parameter-to-result mapping.
    /// When missing, a default mapping with undefined ids is returned.
    /// </returns>
    /// <remarks>
    /// Callers can query the mapping for its type signatures and parameter structure.
    /// </remarks>
    member this.Mapping =
        let mapOpt = getMapping this
        match mapOpt with 
        | Some map -> map :?> FplMapping
        | None -> 
            let defaultMap = new FplMapping((this.StartPos, this.EndPos), this)
            defaultMap.FplId <- LiteralUndef
            defaultMap.TypeId <- LiteralUndef
            defaultMap


    /// <summary>
    /// Resolve the requested type representation of this extension.
    /// </summary>
    /// <param name="signatureType">Requested signature format (Name, Type, Mixed, etc.).</param>
    /// <returns>String describing the extension signature or mapping type.</returns>
    override this.Type signatureType = 
        match signatureType with 
        | SignatureType.Name
        | SignatureType.Mixed -> $"{this.FplId} -> {this.Mapping.Type signatureType}" 
        | SignatureType.Type -> $"{this.Mapping.Type signatureType}"

    /// <summary>
    /// Mark this node as an AST block (contains statements/parameters).
    /// </summary>
    /// <returns>True indicating the node is a block.</returns>
    override this.IsBlock () = true

    /// <summary>
    /// Return the extension's single parameter variable.
    /// </summary>
    /// <returns>The <c>FplVariable</c> representing the extension parameter.</returns>
    member this.ExtensionVar = 
        let extensionVar = 
            getParameters this
            |> List.head
        (extensionVar :?> FplVariable)

    /// <summary>
    /// Return the return statement node from this extension body (expected to be the last argument).
    /// </summary>
    /// <returns><c>FplReturn</c> node.</returns>
    member private this.ReturnStmt =
        let last = this.ArgList |> Seq.last
        last :?> FplReturn

    /// <summary>
    /// Execute the extension with its current argument list. Handles recursion limiting and
    /// provides default values when no arguments are supplied.
    /// </summary>
    /// <remarks>
    /// - Uses <c>maxRecursion</c> to detect excessive recursion and emits LG002 diagnostic.
    /// - When arguments are present, uses helpers to run arguments and set the result to the last value.
    /// </remarks>
    override this.Run() = 
        StaticDebug.Debug(this,Debug.Start)
        // run only if the extension variable was initialized
        _callCounter <- _callCounter + 1
        if _callCounter > maxRecursion then
            let instance = getDefaultValueOfFunction this
            this.SetValue instance
            this.ErrorOccurred <- emitLG002Diagnostics (this.Type(SignatureType.Name)) _callCounter heap.Helper.CallerStartPos heap.Helper.CallerEndPos
        else
            if this.ArgList.Count = 0 then 
                let instance = getDefaultValueOfFunction this
                this.SetValue instance
            else
                runArgsAndSetWithLastValue this
        _callCounter <- _callCounter - 1
        StaticDebug.Debug(this,Debug.Stop)

    /// <summary>
    /// Perform signature checks for this extension (SIG11) and base consistency checks.
    /// </summary>
    override this.CheckConsistency () = 
        checkSIG11Diagnostics this
        base.CheckConsistency()

    /// <summary>
    /// Embed this extension in the parent scope, using mixed-signature insertion helpers.
    /// </summary>
    /// <param name="_">Unused parameter (conventional signature).</param>
    override this.EmbedInSymbolTable _ =
        this.CheckConsistency()
        tryAddToParentUsingMixedSignature this

    override this.RunOrder = Some _runOrder


/// <summary>
/// Walk up the parent chain to find the nearest enclosing extension definition.
/// </summary>
/// <param name="leaf">Starting AST node to search from.</param>
/// <returns>
/// Option containing the nearest <c>FplExtension</c> ancestor or <c>None</c> if none exists.
/// </returns>
let rec getParentExtension (leaf: FplGenericNode) =
    match leaf with
    | :? FplExtension ->
        Some leaf
    | _ -> 
        match leaf.Parent with
        | Some parent -> getParentExtension parent 
        | _ -> None

/// <summary>
/// Search for an extension by its identifier across the provided root's visible scope.
/// </summary>
/// <param name="root">Root node whose scope is searched (usually the heap root).</param>
/// <param name="identifier">The extension name to search for.</param>
/// <returns>
/// <c>ScopeSearchResult.Found</c> with the first matching node when present; otherwise <c>ScopeSearchResult.NotFound</c>.
/// </returns>
let searchExtensionByName (root: FplGenericNode) identifier =
    let candidates =
        root.Scope
        |> Seq.map (fun theory ->
            theory.Value.Scope
            |> Seq.filter (fun kvp -> isExtension kvp.Value)
            |> Seq.map (fun kvp -> kvp.Value)
            |> Seq.filter (fun ext -> ext.FplId = identifier))
        |> Seq.concat
        |> Seq.toList

    if candidates.Length = 0 then
        ScopeSearchResult.NotFound
    else
        ScopeSearchResult.Found candidates.Head

/// <summary>
/// Given a candidate node and an extension name, determine if the candidate is the mapping
/// node belonging to an extension with the given name.
/// </summary>
/// <param name="fv">Candidate node to inspect (expected to be a mapping node).</param>
/// <param name="name">Extension name to match against the candidate's parent extension.</param>
/// <returns>
/// A list containing the extension parent node when the mapping belongs to the named extension;
/// otherwise an empty list.
/// </returns>
let findCandidateOfExtensionMapping (fv: FplGenericNode) (name: string) =
    match fv with 
    | :? FplMapping -> 
        match fv.Parent with 
        | Some (:? FplExtension as ext) when ext.FplId = name -> [fv.Parent.Value]
        | _ -> []
    | _ -> []
