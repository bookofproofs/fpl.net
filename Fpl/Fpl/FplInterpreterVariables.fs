/// This module contains all types of the FplInterpreter related to variables

(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)
module FplInterpreterVariables
open System.Collections.Generic
open FParsec
open FplPrimitives
open FplGrammarTypes
open ErrDiagnostics
open FplInterpreterDiagnosticsEmitter
open FplInterpreterBasicTypes
open FplInterpreterUtils
open FplInterpreterGlobals
open FplInterpreterSTEmbedding
open FplInterpreterIntrinsicTypes

[<AbstractClass>]
type FplGenericVariable(fplId, positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericHasValue(positions, parent)
    let mutable _isSignatureVariable = false
    let mutable _isInitialized = false
    let mutable _isBound = false
    let mutable _isUsed = false

    do 
        this.FplId <- fplId
        this.TypeId <- LiteralUndef

    /// Getter if this variable was used after its declaration.
    member this.IsUsed
        with get () = _isUsed

    /// Sets this variable to a used one .
    member this.SetIsUsed() =
        let rec setIsUsed (fv:FplGenericNode) =
            fv.GetVariables()
            |> List.map (fun var -> var :?> FplGenericVariable)
            |> List.iter (fun var -> var.SetIsUsed())
        _isUsed <- true
        setIsUsed this        

    /// Getter if this variable is bound (by a quantor of otherwise).
    member this.IsBound
        with get () = _isBound

    /// Sets this variable to a bound one.
    member this.SetIsBound() =
        let rec setIsBound (fv:FplGenericNode) =
            fv.GetVariables()
            |> List.map (fun var -> var :?> FplGenericVariable)
            |> List.iter (fun var -> var.SetIsBound())
        _isBound <- true
        setIsBound this        

    /// Indicates if this Variable is declared in the signature (true) or in the block (false).
    member this.IsSignatureVariable
        with get () = _isSignatureVariable
        and set (value) = 
            _isSignatureVariable <- value
            _isBound <- value // all signature variables are also bound

    /// Indicates if this FplValue is an initialized variable
    member this.IsInitialized
        with get () = _isInitialized
        and set (value) = 
            _isInitialized <- value
            _isBound <- value // all initialized variables are also bound

    interface IVariable with
        member this.IsSignatureVariable 
            with get () = this.IsSignatureVariable
            and set (value) = this.IsSignatureVariable <- value
        member this.IsInitialized 
            with get () = this.IsInitialized
            and set (value) = this.IsInitialized <- value

    override this.EmbedInSymbolTable nextOpt =
        this.CheckConsistency()
        let addToRuleOfInference (block:FplGenericNode) = 
            if block.Scope.ContainsKey(this.FplId) then
                this.ErrorOccurred <- emitVAR03diagnostics this.FplId block.Scope[this.FplId].QualifiedStartPos this.StartPos this.EndPos
            else
                block.Scope.Add(this.FplId, this)

        let addToSimpleFplBlocksScope (block:FplGenericNode) = 
            if block.Scope.ContainsKey(this.FplId) then
                this.ErrorOccurred <- emitVAR03diagnostics this.FplId block.Scope[this.FplId].QualifiedStartPos this.StartPos this.EndPos
            else
                block.Scope.Add(this.FplId, this)
        
        let addToPropertyOrConstructor (property:FplGenericNode) = 
            let parentOfProperty = property.Parent.Value
            if property.Scope.ContainsKey(this.FplId) then
                this.ErrorOccurred <- emitVAR03diagnostics this.FplId property.Scope[this.FplId].QualifiedStartPos this.StartPos this.EndPos
            elif parentOfProperty.Scope.ContainsKey(this.FplId) then
                // check also the scope of the property's parent block
                this.ErrorOccurred <- emitVAR03diagnostics this.FplId parentOfProperty.Scope[this.FplId].QualifiedStartPos this.StartPos this.EndPos
            else
                property.Scope.Add(this.FplId, this)

        let addToProofOrCorolllary (proofOrCorollary:FplGenericNode) = 
            let rec conflictInScope (node:FplGenericNode) formulaConflict =
                if node.Scope.ContainsKey(this.FplId) then
                    this.ErrorOccurred <- emitVAR03diagnostics this.FplId node.Scope[this.FplId].QualifiedStartPos this.StartPos this.EndPos
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

        let addToQuantor (quantor:FplGenericNode) =
            // issue VAR03, if the variable to be bound by the quantor was declared 
            // in the scope the quantor is placed in.
            let rec checkConfictInScope (node:FplGenericNode) =
                if node.Scope.ContainsKey(this.FplId) then
                    this.ErrorOccurred <- emitVAR03diagnostics this.FplId node.Scope[this.FplId].QualifiedStartPos this.StartPos this.EndPos
                else 
                    let parent = node.Parent.Value
                    match parent.Name with
                    | PrimRoot 
                    | PrimTheoryL -> ()
                    | _ ->
                        checkConfictInScope parent 

            checkConfictInScope quantor
            quantor.Scope.TryAdd(this.FplId, this) |> ignore        
        
        let addToVariableOrMapping (variableOrMapping:FplGenericNode) =
            let rec conflictInScope (node:FplGenericNode) =
                if node.Scope.ContainsKey(this.FplId) then
                    this.ErrorOccurred <- emitVAR03diagnostics this.FplId node.Scope[this.FplId].QualifiedStartPos this.StartPos this.EndPos 
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
        | Some next when next.Name = PrimQuantorAll || next.Name = PrimQuantorExists || next.Name = PrimQuantorExistsN ->  
            this.SetIsBound() // quantor-Variables are bound
            if next.Scope.ContainsKey(this.FplId) then
                this.ErrorOccurred <- emitVAR02diagnostics this.FplId this.StartPos this.EndPos
            elif next.Name = PrimQuantorExistsN && next.Scope.Count>0 then 
                this.ErrorOccurred <- emitVAR07diagnostics this.FplId this.StartPos this.EndPos
            elif this.Name = PrimVariableArrayL then 
                this.ErrorOccurred <- emitVAR08diagnostics this.StartPos this.EndPos
            else
                addToQuantor next
                
        | _ -> addExpressionToParentArgList this

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

    override this.SetValue fv =
        base.SetValue fv
        if fv.FplId <> PrimUndetermined then
            this.IsInitialized <- true

    override this.SetValueOf fv =
        base.SetValueOf fv
        if fv.FplId <> PrimUndetermined then
            this.IsInitialized <- true

let checkVAR04Diagnostics (fv:FplGenericNode) = 
    fv.GetVariables()
    |> List.map (fun var -> var :?> FplGenericVariable)
    |> List.filter(fun var -> not var.IsUsed)
    |> List.iter (fun var -> 
        var.ErrorOccurred <- emitVAR04diagnostics var.FplId var.StartPos var.EndPos
    )

type FplVariable(fplId, positions: Positions, parent: FplGenericNode) =
    inherit FplGenericVariable(fplId, positions, parent)

    override this.Name = PrimVariableL

    override this.ShortName = PrimVariable

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

    override this.SetValue fv =
        base.SetValue fv
        match this.RefersTo with 
        | Some (:? FplIntrinsicTpl as tpl) -> tpl.TrySetTemplateUsage fv (SIG12("", "", "", "").Code)
        | _ -> ()

    override this.Represent() = // done
        let unsetRepresentation =
            match this.TypeId with
            | LiteralUndef -> LiteralUndef
            | _ -> PrimUndetermined 
        match this.Value with 
        | None -> unsetRepresentation
        | Some ref ->
            let subRepr = ref.Represent()
            if this.IsInitialized || this.IsBound then 
                subRepr
            else
                unsetRepresentation

    override this.Run() =
        debug this Debug.Start
        match this.Value with 
        | None -> this.SetDefaultValue()
        | _ -> ()
        debug this Debug.Stop

type IHasDimensions =
    abstract member Dimensionality : int
    abstract member DimensionTypes : List<FplGenericNode>
    abstract member SetType : string -> FplGenericNode option -> Position -> Position -> unit

type FplMapping(positions: Positions, parent: FplGenericNode) =
    inherit FplGenericNode(positions, Some parent)
    let _dimensionTypes = new List<FplGenericNode>()
    let mutable _dimensionTypesBeingSet = false
    let mutable _isArrayMapping = false

    /// Sets this mapping to an array-typed mapping.
    member this.SetIsArray() = _isArrayMapping <- true

    member this.Dimensionality = _dimensionTypes.Count

    member this.DimensionTypes = _dimensionTypes

    /// Sets the during the symbol table construction.
    /// Because the type consists of a main type and index allowed-types, we use "Dimension being set" as a flag
    /// to decide which one to be set.
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

    override this.Clone () =
        let ret = new FplMapping((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.IsMapping () = true

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

    override this.Represent() = // done
        // a fall back value representation for intrinsic functional terms
        $"dec {this.Type(SignatureType.Type)}"

    override this.Run() = 
        // FplMapping has nothing to do in run
        ()

    override this.EmbedInSymbolTable _ = addExpressionToParentArgList this 

    override this.RunOrder = None

type FplVariableArray(fplId, positions: Positions, parent: FplGenericNode) =
    inherit FplGenericVariable(fplId, positions, parent)
    let _dimensionTypes = new List<FplGenericNode>()
    let mutable _dimensionTypesBeingSet = false
    let _valueKeys = new Dictionary<string,int>() // used to store the keys of all values
    let _valueList = List<FplGenericNode>()

    member this.Dimensionality = _dimensionTypes.Count

    member this.DimensionTypes = _dimensionTypes

    member this.ValueKeys = _valueKeys

    /// Sets the during the symbol table construction.
    /// Because the type consists of a main type and index allowed-types, we use "Dimension being set" as a flag
    /// to decide which one to be set.
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

    /// Copies the ValueKeys field from this to target.
    member private this.CopyValueKeys (target:FplVariableArray) = 
        target.ValueKeys.Clear()
        this.ValueKeys
        |> Seq.iter (fun kvp ->
            target.ValueKeys.Add(kvp.Key, kvp.Value)
        )

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

    /// ValueList of the FplVariableArray.
    member this.ValueList = _valueList

    member this.GetValueByCoordinates coordinatesKey =
        if this.ValueKeys.ContainsKey coordinatesKey then 
           let index = this.ValueKeys[coordinatesKey]
           // return a value based on coordinates 
           this.ValueList[index] 
        else
           // otherwise, spawn an undefined value
           new FplIntrinsicUndef((this.StartPos, this.EndPos), this)

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

    override this.Type signatureType =
        let mainType = base.Type signatureType
        let dimensionTypes = signatureSep "," this.DimensionTypes signatureType

        match signatureType with
        | SignatureType.Name -> this.FplId
        | _ -> $"{mainType}[{dimensionTypes}]"

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

    override this.Run() =
        () // running not necessary for arrays
