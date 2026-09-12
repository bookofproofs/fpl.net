(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)

/// <summary>
/// Module containing variable-related symbol-table node types used by the FPL interpreter.
/// </summary>
/// <remarks>
/// This module implements generic and concrete variable nodes, mapping nodes and helper
/// checks used during symbol-table construction, type rendering and runtime evaluation.
/// Diagnostic emission is performed via helper emitters rather than throwing exceptions.
/// </remarks>
module Fpl.Interpreter.SymbolTable.Types2.Variables
open System.Collections.Generic
open FParsec
open Fpl.Primitives
open Fpl.Parser.Types
open Fpl.Errors.Diagnostics
open Fpl.Errors.Emitter
open Fpl.Interpreter.BasicTypes
open Fpl.Interpreter.Helpers.Basic
open Fpl.Interpreter.Helpers.Debug
open Fpl.Interpreter.SymbolTable.Types2.Intrinsic

[<AbstractClass>]
/// <summary>
/// Abstract base type for variables that may carry values and participate in scoping rules.
/// </summary>
/// <param name="fplId">The FPL identifier for the variable (name).</param>
/// <param name="positions">Start and end positions in the source for diagnostics.</param>
/// <param name="parent">Parent node in the AST/symbol table.</param>
/// <remarks>
/// Tracks variable properties such as whether the variable is a signature variable,
/// initialized, bound or used. Provides embedding logic for a variety of parent node types
/// and integrates with diagnostic emitters for scope conflicts.
/// </remarks>
type FplGenericVariable(fplId, positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericHasValue(positions, parent)
    let mutable _isSignatureVariable = false
    let mutable _isInitialized = false
    let mutable _isBound = false
    let mutable _isUsed = false

    do 
        this.FplId <- fplId
        this.TypeId <- LiteralUndef

    /// <summary>
    /// Indicates whether this variable was used after declaration.
    /// </summary>
    member this.IsUsed
        with get () = _isUsed

    /// <summary>
    /// Marks this variable and any nested variables as used.
    /// </summary>
    /// <remarks>
    /// Recursively marks variables obtained from <c>GetVariables</c> so that nested variables
    /// become used as well.
    /// </remarks>
    member this.SetIsUsed() =
        let rec setIsUsed (fv:FplGenericNode) =
            fv.GetVariables()
            |> List.map (fun var -> var :?> FplGenericVariable)
            |> List.iter (fun var -> var.SetIsUsed())
        _isUsed <- true
        setIsUsed this        

    /// <summary>
    /// Indicates whether this variable is bound (for example by a quantifier).
    /// </summary>
    member this.IsBound
        with get () = _isBound

    /// <summary>
    /// Marks this variable and nested variables as bound.
    /// </summary>
    /// <remarks>
    /// Recursively marks variables obtained from <c>GetVariables</c> so that nested variables
    /// become bound as well.
    /// </remarks>
    member this.SetIsBound() =
        let rec setIsBound (fv:FplGenericNode) =
            fv.GetVariables()
            |> List.map (fun var -> var :?> FplGenericVariable)
            |> List.iter (fun var -> var.SetIsBound())
        _isBound <- true
        setIsBound this        

    /// <summary>
    /// Indicates whether this variable is declared in the signature (true) or in the block (false).
    /// </summary>
    member this.IsSignatureVariable
        with get () = _isSignatureVariable
        and set (value) = 
            _isSignatureVariable <- value

    /// <summary>
    /// Indicates whether this variable has been initialized with a value.
    /// </summary>
    /// <remarks>
    /// Setting this property also sets <c>IsBound</c> to true because initialized variables
    /// are considered bound by the interpreter semantics.
    /// </remarks>
    member this.IsInitialized
        with get () = _isInitialized
        and set (value) = 
            _isInitialized <- value
            if _isInitialized then 
                _isBound <- value // all initialized variables are also bound

    interface IVariable with
        member this.IsSignatureVariable 
            with get () = this.IsSignatureVariable
            and set (value) = this.IsSignatureVariable <- value
        member this.IsInitialized 
            with get () = this.IsInitialized
            and set (value) = this.IsInitialized <- value
        member this.IsBound 
            with get () = this.IsBound

    /// <summary>
    /// Embeds the variable into the symbol table depending on the provided context node.
    /// </summary>
    /// <param name="nextOpt">Optional next node determining the embedding context.</param>
    /// <remarks>
    /// Embedding checks for scope conflicts and emits diagnostics using emitter helpers
    /// (e.g. <c>emitVAR03Diagnostics</c>, <c>emitVAR02Diagnostics</c> etc.). This method
    /// does not throw exceptions for those diagnostics; it records diagnostic state on the node.
    /// </remarks>
    override this.EmbedInSymbolTable nextOpt =
        this.CheckConsistency()
        let addToRuleOfInference (block:FplGenericNode) = 
            if block.Scope.ContainsKey(this.FplId) then
                this.ErrorOccurred <- emitVAR03Diagnostics this.FplId block.Scope[this.FplId].QualifiedStartPos this.StartPos this.EndPos
            else
                block.Scope.Add(this.FplId, this)

        let addToSimpleFplBlocksScope (block:FplGenericNode) = 
            if block.Scope.ContainsKey(this.FplId) then
                this.ErrorOccurred <- emitVAR03Diagnostics this.FplId block.Scope[this.FplId].QualifiedStartPos this.StartPos this.EndPos
            else
                block.Scope.Add(this.FplId, this)
        
        let addToPropertyOrConstructor (property:FplGenericNode) = 
            let parentOfProperty = property.Parent.Value
            if property.Scope.ContainsKey(this.FplId) then
                this.ErrorOccurred <- emitVAR03Diagnostics this.FplId property.Scope[this.FplId].QualifiedStartPos this.StartPos this.EndPos
            elif parentOfProperty.Scope.ContainsKey(this.FplId) then
                // check also the scope of the property's parent block
                this.ErrorOccurred <- emitVAR03Diagnostics this.FplId parentOfProperty.Scope[this.FplId].QualifiedStartPos this.StartPos this.EndPos
            else
                property.Scope.Add(this.FplId, this)

        let addToProofOrCorolllary (proofOrCorollary:FplGenericNode) = 
            let rec conflictInScope (node:FplGenericNode) formulaConflict =
                if node.Scope.ContainsKey(this.FplId) then
                    this.ErrorOccurred <- emitVAR03Diagnostics this.FplId node.Scope[this.FplId].QualifiedStartPos this.StartPos this.EndPos
                    true
                else 
                    let parent = node.Parent.Value
                    match parent.Name with
                    | LiteralCorL
                    | LiteralThmL
                    | LiteralLemL
                    | LiteralPropL
                    | LiteralConjL
                    | LiteralAxL ->
                        conflictInScope parent formulaConflict
                    | _ -> false

            if not (conflictInScope proofOrCorollary false) then
                proofOrCorollary.Scope.Add(this.FplId, this)

        let addToQuantifier (quantifier:FplGenericNode) =
            // issue VAR03, if the variable to be bound by the quantifier was declared 
            // in the scope the quantifier is placed in.
            let rec checkConfictInScope (node:FplGenericNode) =
                if node.Scope.ContainsKey(this.FplId) then
                    this.ErrorOccurred <- emitVAR03Diagnostics this.FplId node.Scope[this.FplId].QualifiedStartPos this.StartPos this.EndPos
                else 
                    let parent = node.Parent.Value
                    match parent.Name with
                    | PrimRoot 
                    | PrimTheoryL -> ()
                    | _ ->
                        checkConfictInScope parent 

            checkConfictInScope quantifier
            quantifier.Scope.TryAdd(this.FplId, this) |> ignore        
        
        let addToVariableOrMapping (variableOrMapping:FplGenericNode) =
            let rec conflictInScope (node:FplGenericNode) =
                if node.Scope.ContainsKey(this.FplId) then
                    this.ErrorOccurred <- emitVAR03Diagnostics this.FplId node.Scope[this.FplId].QualifiedStartPos this.StartPos this.EndPos 
                    true
                else 
                    let parent = node.Parent.Value
                    match parent.Name with
                    | PrimRoot 
                    | PrimTheoryL -> false
                    | _ ->
                        conflictInScope parent
            
            if not (conflictInScope variableOrMapping) then
                variableOrMapping.Scope.Add(this.FplId, this)
                let blockOpt = variableOrMapping.UltimateBlockNode
                match blockOpt with
                | Some block -> block.Scope.Add(this.FplId, this)
                | None -> ()
            else
                variableOrMapping.Scope.TryAdd(this.FplId, this) |> ignore

        match nextOpt with 
        | Some next when next.Name = PrimRefL ->
            next.FplId <- this.FplId
            next.TypeId <- this.TypeId
            next.RefersTo <- Some this
        | Some next when ( next.Name = LiteralAxL 
                        || next.Name = LiteralThmL 
                        || next.Name = LiteralLemL 
                        || next.Name = LiteralPropL 
                        || next.Name = LiteralConjL 
                        || next.Name = PrimClassL 
                        || next.Name = PrimFunctionalTermL
                        || next.Name = PrimPredicateL
                        || next.Name = PrimExtensionL
                        || next.Name = PrimTranslationL
                        ) ->
            addToSimpleFplBlocksScope next
        | Some next when next.Name = PrimRuleOfInference ->
            addToRuleOfInference next
        | Some next when ( next.Name = LiteralCtorL
                        || next.Name = PrimMandatoryFunctionalTermL 
                        || next.Name = PrimMandatoryPredicateL) ->
            addToPropertyOrConstructor next
        | Some next when (next.Name = LiteralPrfL 
                        || next.Name = LiteralCorL) ->
            addToProofOrCorolllary next
        | Some next when (next.Name = PrimVariableL
                        || next.Name = PrimVariableArrayL) ->
            addToVariableOrMapping next
        | Some next when next.Name = PrimMappingL ->
            this.SetIsBound() // mapping-Variables are bound
            addToVariableOrMapping next
        | Some next when next.Name = PrimQuantifierAll || next.Name = PrimQuantifierExists || next.Name = PrimQuantifierExistsN ->  
            this.SetIsBound() // quantifier variables are bound
            if next.Scope.ContainsKey(this.FplId) then
                this.ErrorOccurred <- emitVAR02Diagnostics this.FplId this.StartPos this.EndPos
            elif next.Name = PrimQuantifierExistsN && next.Scope.Count>0 then 
                this.ErrorOccurred <- emitVAR07Diagnostics this.FplId this.StartPos this.EndPos
            elif this.Name = PrimVariableArrayL then 
                this.ErrorOccurred <- emitVAR08Diagnostics this.FplId this.StartPos this.EndPos
            else
                addToQuantifier next
                
        | _ -> addExpressionToParentArgList this

    /// <summary>
    /// Returns the type head and parameter rendering for this variable.
    /// </summary>
    /// <param name="signatureType">Determines whether to render name, type or other signature forms.</param>
    /// <returns>Rendered type/name representation string according to the requested signature type.</returns>
    override this.Type signatureType =
        let head = getFplHead this signatureType

        let pars = getParamTuple this signatureType
        let propagate = propagateSignatureType signatureType

        match this.ArgType, pars, getMapping this with
        | ArgType.Parentheses, "", None -> 
            if signatureType = SignatureType.Name then 
                head
            else
                $"{head}({pars})"
        | ArgType.Parentheses, "", Some map -> 
            if signatureType = SignatureType.Name then 
                head
            else
                $"{head}({pars}) -> {map.Type propagate}"
        | ArgType.Parentheses, _, None -> $"{head}({pars})"
        | ArgType.Parentheses, _, Some map -> $"{head}({pars}) -> {map.Type propagate}"
        | ArgType.Nothing, "", None -> head
        | ArgType.Nothing, "", Some map -> $"{head}() -> {map.Type propagate}" 
        | _, _, None -> sprintf "%s(%s)" head pars
        | _, _, Some map -> sprintf "%s(%s) -> %s" head pars (map.Type propagate)

    override this.RunOrder = None

    /// <summary>
    /// Copies state from another variable node into this one.
    /// </summary>
    /// <param name="other">Source node to copy from (must be a <c>FplGenericVariable</c>).</param>
    /// <remarks>
    /// Copies binding/usage/initialization state and references; used when cloning or duplicating
    /// parts of the symbol tree.
    /// </remarks>
    override this.Copy(other: FplGenericNode) = 
        base.Copy(other)
        let otherVar = other :?> FplGenericVariable
        if otherVar.IsBound then 
            this.SetIsBound()
        if otherVar.IsUsed then 
            this.SetIsUsed()
        this.IsSignatureVariable <- otherVar.IsSignatureVariable 
        this.IsInitialized <- otherVar.IsInitialized
        this.RefersTo <- otherVar.RefersTo

    /// <summary>
    /// Sets the runtime value of this variable node.
    /// </summary>
    /// <param name="fv">The value node to set.</param>
    /// <remarks>
    /// If the assigned value is not undetermined, this sets <c>IsInitialized</c>.
    /// </remarks>
    override this.SetValue fv =
        base.SetValue fv
        if fv.FplId <> LiteralUndet then
            this.IsInitialized <- true

    /// <summary>
    /// Copies the value of another node into this variable (aliasing semantics).
    /// </summary>
    /// <param name="fv">Source node whose value should be used.</param>
    /// <remarks>
    /// If the assigned value is not undetermined, this sets <c>IsInitialized</c>.
    /// </remarks>
    override this.SetValueOf fv =
        base.SetValueOf fv
        if fv.FplId <> LiteralUndet then
            this.IsInitialized <- true

/// <summary>
/// Emits VAR04 diagnostics for any variables that were declared but never used.
/// </summary>
/// <param name="fv">Node from which to collect declared variables.</param>
/// <remarks>
/// The function sets the <c>ErrorOccurred</c> property on each variable and uses
/// diagnostic emitters to record the issue. This does not throw exceptions.
/// </remarks>
let checkVAR04Diagnostics (fv:FplGenericNode) = 
    fv.GetVariables()
    |> List.map (fun var -> var :?> FplGenericVariable)
    |> List.filter(fun var -> not var.IsUsed)
    |> List.iter (fun var -> 
        var.ErrorOccurred <- emitVAR04Diagnostics var.FplId var.StartPos var.EndPos
    )

/// <summary>
/// Concrete variable node used for ordinary variables.
/// </summary>
/// <param name="fplId">Variable name.</param>
/// <param name="positions">Source positions for diagnostics.</param>
/// <param name="parent">Parent AST/symbol node.</param>
type FplVariable(fplId, positions: Positions, parent: FplGenericNode) =
    inherit FplGenericVariable(fplId, positions, parent)
    let mutable _isExpressionAssigned = false

    override this.Name = PrimVariableL

    override this.ShortName = PrimVariable


    /// <summary>
    /// Indicates whether this variable has been assigned an expression in the symbol table.
    /// </summary>
    /// <remarks>
    /// If true, <c>RefersTo</c> points to the expression node assigned to this variable.
    /// </remarks>
    member this.IsExpressionAssigned
        with get () = _isExpressionAssigned
        and set (value) = 
            _isExpressionAssigned <- value

    /// <summary>
    /// Creates a deep copy of this variable preserving binding, usage and initialization state.
    /// </summary>
    /// <returns>A cloned <c>FplVariable</c>.</returns>
    override this.Clone () =
        let ret = new FplVariable(this.FplId, (this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        if this.IsBound then 
            ret.SetIsBound()
        if this.IsUsed then 
            ret.SetIsUsed()
        ret.IsSignatureVariable <- this.IsSignatureVariable 
        ret.IsInitialized <- this.IsInitialized
        ret

    /// <summary>
    /// Sets the runtime value for this variable and applies template usage checks when relevant.
    /// </summary>
    /// <param name="fv">Value node to assign.</param>
    /// <remarks>
    /// If the variable refers to a template (<c>FplIntrinsicTpl</c>), this triggers
    /// template usage consistency checks (SIG12).
    /// </remarks>
    override this.SetValue fv =
        base.SetValue fv
        // if the type of a variable is a template, check for SIG12 consistency of all assigned values (if any)
        match this.RefersTo with 
        | Some (:? FplIntrinsicTpl as tpl) -> tpl.TrySetTemplateUsage fv (SIG12("", "", "", "").Code)
        | _ -> ()

    /// <summary>
    /// Returns a textual representation of the variable's current value or its undecided/declared state.
    /// </summary>
    /// <returns>
    /// - <c>LiteralUndef</c> if the variable type is undefined
    /// - <c>LiteralUndet</c> if a value exists but the variable is not initialized/bound
    /// - the representation of the assigned value when initialized or bound
    /// </returns>
    override this.Represent() = // done
        let unsetRepresentation =
            match this.TypeId with
            | LiteralUndef -> LiteralUndef
            | _ -> LiteralUndet 
        match this.Value with 
        | None -> unsetRepresentation
        | Some ref ->
            let subRepr = ref.Represent()
            if this.IsInitialized || this.IsBound then 
                subRepr
            else
                unsetRepresentation

    /// <summary>
    /// Runtime evaluation for the variable node: sets default value if no value is assigned.
    /// </summary>
    override this.Run() =
        StaticDebug.Debug(this,Debug.Start)
        match this.Value with 
        | None -> this.SetDefaultValue()
        | _ -> ()
        StaticDebug.Debug(this,Debug.Stop)

/// <summary>
/// Interface for nodes that expose dimensionality and index/type-setting helpers.
/// </summary>
type IHasDimensions =
    /// <summary>
    /// Number of index dimensions.
    /// </summary>
    abstract member Dimensionality : int
    /// <summary>
    /// Allowed types for each dimension (index types).
    /// </summary>
    abstract member DimensionTypes : List<FplGenericNode>
    /// <summary>
    /// Helper to set main type and index allowed-types during symbol table construction.
    /// </summary>
    /// <param name="typeId">Type identifier for the main or index type.</param>
    /// <param name="typeNodeOpt">Optional node the type refers to.</param>
    /// <param name="pos1">Start position used for the created index-type node.</param>
    /// <param name="pos2">End position used for the created index-type node.</param>
    abstract member SetType : string -> FplGenericNode option -> Position -> Position -> unit

/// <summary>
/// Represents mapping/index types used as function/mapping signatures or index types.
/// </summary>
/// <param name="positions">Source positions for diagnostics.</param>
/// <param name="parent">Parent AST/symbol node.</param>
/// <remarks>
/// Mapping nodes may be used as both main types and index-allowed types. They support
/// array-style mappings when designated via <c>SetIsArray</c>.
/// </remarks>
type FplMapping(positions: Positions, parent: FplGenericNode) =
    inherit FplGenericNode(positions, Some parent)
    let _dimensionTypes = new List<FplGenericNode>()
    let mutable _dimensionTypesBeingSet = false
    let mutable _isArrayMapping = false

    /// <summary>
    /// Mark this mapping as an array mapping (affects Type rendering).
    /// </summary>
    member this.SetIsArray() = _isArrayMapping <- true

    /// <summary>
    /// Number of index dimensions declared for this mapping.
    /// </summary>
    member this.Dimensionality = _dimensionTypes.Count

    /// <summary>
    /// List of nodes describing allowed types for each dimension.
    /// </summary>
    member this.DimensionTypes = _dimensionTypes

    /// <summary>
    /// Sets the main type for the mapping or adds an index allowed-type depending on internal state.
    /// </summary>
    /// <param name="typeId">Type identifier to set.</param>
    /// <param name="typeNodeOpt">Optional node the type refers to.</param>
    /// <param name="pos1">Start position for created index type node if needed.</param>
    /// <param name="pos2">End position for created index type node if needed.</param>
    /// <remarks>
    /// The first call sets the main type and any subsequent calls add index-allowed types.
    /// </remarks>
    member this.SetType (typeId:string) (typeNodeOpt:FplGenericNode option) pos1 pos2 = 
        if not _dimensionTypesBeingSet then 
            this.TypeId <-
                if _isArrayMapping then 
                    $"*{typeId}"
                else
                    typeId
            this.RefersTo <- typeNodeOpt 
            _dimensionTypesBeingSet <- true
        else
            let indexAllowedType = FplMapping((pos1,pos2), this) 
            indexAllowedType.TypeId <- typeId
            indexAllowedType.RefersTo <- typeNodeOpt 
            this.DimensionTypes.Add indexAllowedType

    interface IHasDimensions with
        member this.Dimensionality = _dimensionTypes.Count
        member this.DimensionTypes = _dimensionTypes
        member this.SetType typeId typeNodeOpt pos1 pos2 = this.SetType typeId typeNodeOpt pos1 pos2

    override this.Name = PrimMappingL
    override this.ShortName = PrimMapping

    /// <summary>
    /// Clone this mapping node (shallow copy of parts).
    /// </summary>
    override this.Clone () =
        let ret = new FplMapping((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.IsMapping () = true

    /// <summary>
    /// Returns the textual/type representation for this mapping node.
    /// </summary>
    /// <param name="signatureType">Specifies how to render nested mapping types.</param>
    /// <returns>Formatted mapping type string; includes array dimension information if applicable.</returns>
    override this.Type signatureType = 
        let pars = getParamTuple this signatureType
        let propagate = propagateSignatureType signatureType

        let myMapping = 
            if this.ArgList.Count > 0 then 
                let arg = this.ArgList[0]
                match arg with 
                | :? FplMapping ->
                    Some(arg)
                | _ -> None
            else
                None
        let mainType = 
            match this.ArgType, myMapping with
            | ArgType.Parentheses, None -> $"{this.TypeId}({pars})"
            | _, None -> this.TypeId
            | _, Some map -> $"{this.TypeId}({pars}) -> {map.Type(propagate)}" 

        if not _isArrayMapping then
            mainType
        else
            let dimensionTypes = signatureSep "," this.DimensionTypes signatureType
            $"{mainType}[{dimensionTypes}]"

    /// <summary>
    /// Returns a fallback representation used for functional term inference.
    /// </summary>
    override this.Represent() = // done
        // a fall back value representation for intrinsic functional terms
        $"dec {this.Type(SignatureType.Type)}"

    override this.Run() = 
        // FplMapping has nothing to do in run
        ()

    override this.EmbedInSymbolTable _ = addExpressionToParentArgList this 

    override this.RunOrder = None

/// <summary>
/// Variable array type supporting indexed values and multi-dimensional index types.
/// </summary>
/// <param name="fplId">Identifier of the variable array.</param>
/// <param name="positions">Source positions for diagnostics.</param>
/// <param name="parent">Parent AST/symbol node.</param>
/// <remarks>
/// Variable arrays maintain a mapping from coordinate keys to values and support type
/// declaration for dimension index types.
/// </remarks>
type FplVariableArray(fplId, positions: Positions, parent: FplGenericNode) =
    inherit FplGenericVariable(fplId, positions, parent)
    let _dimensionTypes = new List<FplGenericNode>()
    let mutable _dimensionTypesBeingSet = false
    let _valueKeys = new Dictionary<string,int>() // used to store the keys of all values
    let _valueList = List<FplGenericNode>()

    member this.Dimensionality = _dimensionTypes.Count

    member this.DimensionTypes = _dimensionTypes

    member this.ValueKeys = _valueKeys

    /// <summary>
    /// Sets the array main type on first call; subsequent calls register index allowed-types.
    /// </summary>
    /// <param name="typeId">Main or index type identifier.</param>
    /// <param name="typeNodeOpt">Optional node the type refers to.</param>
    /// <param name="pos1">Position start for index-type node creation.</param>
    /// <param name="pos2">Position end for index-type node creation.</param>
    /// <remarks>
    /// The main type is stored as a pointer via Scope for index types; main type is prefixed
    /// to indicate an array (leading '*').
    /// </remarks>
    member this.SetType (typeId:string) (typeNodeOpt:FplGenericNode option) pos1 pos2 = 
        if not _dimensionTypesBeingSet then 
            this.TypeId <- $"*{typeId}"
            // TODO prefer RefersTo over Scope when storing the type node of the variable array 
            match typeNodeOpt with 
            | Some typeNode -> this.Scope.TryAdd(typeId, typeNode) |> ignore
            | _ -> ()
            _dimensionTypesBeingSet <- true
        else
            let indexAllowedType = FplMapping((pos1,pos2), this) 
            indexAllowedType.TypeId <- typeId
            indexAllowedType.RefersTo <- typeNodeOpt
            this.DimensionTypes.Add indexAllowedType

    interface IHasDimensions with
        member this.Dimensionality = _dimensionTypes.Count
        member this.DimensionTypes = _dimensionTypes
        member this.SetType typeId typeNodeOpt pos1 pos2 = this.SetType typeId typeNodeOpt pos1 pos2

    override this.Name = PrimVariableArrayL

    override this.ShortName = PrimVariableArray

    /// <summary>
    /// Copies the internal ValueKeys dictionary into the provided target array node.
    /// </summary>
    /// <param name="target">Target <c>FplVariableArray</c> to receive a copy of keys.</param>
    member private this.CopyValueKeys (target:FplVariableArray) = 
        target.ValueKeys.Clear()
        this.ValueKeys
        |> Seq.iter (fun kvp ->
            target.ValueKeys.Add(kvp.Key, kvp.Value)
        )

    /// <summary>
    /// Clones the variable array including dimension type nodes and keys.
    /// </summary>
    override this.Clone () =
        let ret = new FplVariableArray(this.FplId, (this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        if this.IsBound then 
            ret.SetIsBound()
        if this.IsUsed then 
            ret.SetIsUsed()
        ret.IsSignatureVariable <- this.IsSignatureVariable 
        ret.IsInitialized <- this.IsInitialized

        this.DimensionTypes
        |> Seq.iter (fun (fv1:FplGenericNode) ->
            let value = fv1.Clone()
            ret.DimensionTypes.Add(value))

        this.CopyValueKeys ret
        ret

    /// <summary>
    /// Value list of the variable array in insertion order.
    /// </summary>
    member this.ValueList = _valueList

    /// <summary>
    /// Retrieves the value node for the given coordinate key or returns an undefined intrinsic node if none exists.
    /// </summary>
    /// <param name="coordinatesKey">Coordinate key identifying the value.</param>
    /// <returns>Stored value node or a new <c>FplIntrinsicUndef</c> when the key is missing.</returns>
    member this.GetValueByCoordinates coordinatesKey =
        if this.ValueKeys.ContainsKey coordinatesKey then 
           let index = this.ValueKeys[coordinatesKey]
           // return a value based on coordinates 
           this.ValueList[index] 
        else
           // otherwise, spawn an undefined value
           new FplIntrinsicUndef((this.StartPos, this.EndPos), this)

    /// <summary>
    /// Assigns a value node to the given coordinates key, replacing existing values when necessary.
    /// </summary>
    /// <param name="coordinatesKey">Coordinate key for the assignment.</param>
    /// <param name="value">Node to assign at the given coordinates.</param>
    /// <remarks>
    /// Marks the variable array as initialized and maintains an index mapping for fast lookup.
    /// </remarks>
    member this.AssignValueToCoordinates coordinatesKey (value:FplGenericNode) =
        this.IsInitialized <- true

        if this.ValueKeys.ContainsKey coordinatesKey then 
           let index = this.ValueKeys[coordinatesKey]
           // a value with this coordinates already exists, and we replace it by the new one
           this.ValueList[index] <- value
        else
            // a value with this coordinates does not exist yet. We ann the value 
            this.ValueList.Add value
            // and store the index of the new coordinatesKey
            this.ValueKeys.Add (coordinatesKey, this.ValueList.Count-1)

    /// <summary>
    /// Returns the variable-array type representation including dimension types.
    /// </summary>
    /// <param name="signatureType">Signature rendering mode.</param>
    /// <returns>Type/name representation string for the variable array.</returns>
    override this.Type signatureType =
        let mainType = base.Type signatureType
        let dimensionTypes = signatureSep "," this.DimensionTypes signatureType

        match signatureType with
        | SignatureType.Name -> this.FplId
        | _ -> $"{mainType}[{dimensionTypes}]"

    /// <summary>
    /// Returns a readable representation of the variable array's contents or declared/undetermined form.
    /// </summary>
    override this.Represent() = // done
        if this.ValueList.Count = 0 then
            if this.IsInitialized then 
                // this case should never happen, because isInitializesVariable is a contradiction to ValueList.Count 0
                LiteralUndef
            else
                match this.TypeId with
                | LiteralUndef -> LiteralUndef
                | _ -> $"dec {this.Type SignatureType.Type}"
        else
            // ensure canonical order of keys
            let sortedKeys = 
                let sortByCoordinates (items: string seq) =
                    let parseCoord (coord: string) =
                        if coord.StartsWith "$" then
                            // Numeric coordinate: return Left(int)
                            let n = coord.Substring(1) |> int
                            Choice1Of2 n
                        else
                            // Alphabetic coordinate: return Right(string)
                            Choice2Of2 coord

                    let keyOf (s: string) =
                        s.Split('|')
                        |> Array.map parseCoord
                        |> Array.toList

                    items
                    |> Seq.sortBy keyOf
                sortByCoordinates this.ValueKeys.Keys 
            
            let subRepr = 
                sortedKeys
                |> Seq.map (fun coordinatesKey -> 
                    let index = this.ValueKeys[coordinatesKey]
                    let valueRepr = this.ValueList[index].Represent()
                    $"[{coordinatesKey}]->{valueRepr}"
                )
                |> String.concat ", "
            if this.IsInitialized then 
                subRepr
            else
                match this.TypeId with
                | LiteralUndef -> LiteralUndef
                | _ -> $"dec {this.Type(SignatureType.Type)}" 

    /// <summary>
    /// Runtime execution is not required for variable arrays; operation is a no-op.
    /// </summary>
    override this.Run() =
        () // running not necessary for arrays
