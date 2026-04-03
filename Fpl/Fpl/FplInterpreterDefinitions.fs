/// This module contains all classed used by the FplInterpreter
/// to store and interpret definitions in the symbol table

(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)
module FplInterpreterDefinitions
open System.Collections.Generic
open FParsec
open FplGrammarTypes
open FplPrimitives
open FplInterpreterDiagnosticsEmitter
open FplInterpreterBasicTypes
open FplInterpreter.Globals.Debug
open FplInterpreter.Globals.Root
open FplInterpreter.Globals.Heap
open FplInterpreterChecks
open FplInterpreterSTEmbedding
open FplInterpreterIntrinsicTypes
open FplInterpreterVariables
open FplInterpreterReferences

[<AbstractClass>]
type FplGenericPredicate(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericHasValue(positions, parent)
    do 
        this.FplId <- LiteralTrue
        this.TypeId <- LiteralPred

    override this.RunOrder = None

    override this.Run() = 
        debug this Debug.Start 
        // the default value of predicates is an undetermined predicate
        this.SetDefaultValue()
        debug this Debug.Stop

[<AbstractClass>]
type FplGenericInheriting(positions: Positions, parent: FplGenericNode) =
    inherit FplGenericHasValue(positions, parent)
    // used to ensure that every clone of FplGenericInheriting will preserve reference identity of inherited variables 
    let _inheritedVariables = Dictionary<string, List<FplGenericNode>>()
    // used to ensure that every clone of FplGenericInheriting will preserve reference identity of inherited properties
    let _inheritedProperties = Dictionary<string, List<FplGenericNode>>()

    /// Wraps an inherited object in a tuple together with the newFromNode it was from and stores this tuple with the keyOfInheritedObject in mapOfInheritedObjects. 
    /// Returns (Some oldFromNode, Some newFromNode) if some other tuple existed in the map, where oldFromNode will be the old base node that was overridden by newFromNode
    /// Returns (None, None) if no other tuple yet existed in the map
    member private this.OverrideInheritedObject keyOfInheritedObject (mapOfInheritedObjects:Dictionary<string, List<FplGenericNode>>) (inheritedObject:FplGenericNode) (newFromNode:FplGenericNode) withCloning =
        let clone = 
            if withCloning then
                inheritedObject.Clone() // create a clone of new object to override the old one
            else
                inheritedObject
        // create a tuple (clone, fromBaseNode)
        let tuple = List<FplGenericNode>()
        tuple.Add clone 
        tuple.Add newFromNode // and where it was from
        if mapOfInheritedObjects.ContainsKey keyOfInheritedObject then
            // replace the old reference
            let oldFromNode = mapOfInheritedObjects[keyOfInheritedObject][1]
            mapOfInheritedObjects[keyOfInheritedObject] <- tuple
            (Some oldFromNode.Name, Some (oldFromNode.Type SignatureType.Mixed), Some (newFromNode.Type SignatureType.Mixed))
        else
            // add a new reference
            mapOfInheritedObjects.Add(keyOfInheritedObject, tuple)
            (None, None, None)
            
    member this.InheritVariables (fromBaseNode:FplGenericNode) = 
        fromBaseNode.GetVariables()
        |> List.iter (fun var ->
            match this.OverrideInheritedObject var.FplId _inheritedVariables var fromBaseNode true with
            | (Some typeName, Some oldFromNode, Some newFromNode) ->
                fromBaseNode.ErrorOccurred <- emitVAR06iagnostic var.FplId oldFromNode newFromNode typeName fromBaseNode.StartPos fromBaseNode.EndPos
            | _ ->
                ()
        )

    member this.InheritProperties (fromBaseNode:FplGenericNode) = 
        fromBaseNode.GetProperties()
        |> List.iter (fun prty ->
            let prtyName = prty.Type SignatureType.Mixed
            match this.OverrideInheritedObject prtyName _inheritedProperties prty fromBaseNode true with
            | (Some typeName, Some oldFromNode, Some newFromNode) ->
                fromBaseNode.ErrorOccurred <- emitSIG06iagnostic prtyName oldFromNode newFromNode typeName fromBaseNode.StartPos fromBaseNode.EndPos
            | _ ->
                ()
        )

    override this.CheckConsistency() = 
        base.CheckConsistency()
        // check if own declared variables override the inherited ones
        this.GetVariables()
        |> Seq.iter (fun var -> 
            if _inheritedVariables.ContainsKey var.FplId then
                let oldFrom = _inheritedVariables[var.FplId][1]
                let oldFromNode = oldFrom.Type SignatureType.Mixed
                let newFromNode = this.Type SignatureType.Mixed
                let typeName = oldFrom.Name
                // override the old node
                let tuple = List<FplGenericNode>()
                tuple.Add var // own scope variable
                tuple.Add this // the var is from this
                _inheritedVariables[var.FplId] <- tuple
                // emit VAR06, since the inner variable overrides some inherited var
                var.ErrorOccurred <- emitVAR06iagnostic var.FplId oldFromNode newFromNode typeName var.StartPos var.EndPos
        )
        // check if own declared properties override the inherited ones
        this.GetProperties()
        |> Seq.iter (fun prty -> 
            let prtyName = prty.Type SignatureType.Mixed
            if _inheritedProperties.ContainsKey prtyName then
                let oldFrom = _inheritedProperties[prtyName][1]
                let oldFromNode = oldFrom.Type SignatureType.Mixed
                let newFromNode = this.Type SignatureType.Mixed
                let typeName = oldFrom.Name
                // override the old node
                let tuple = List<FplGenericNode>()
                tuple.Add prty // own scope property
                tuple.Add this // the property is from this
                _inheritedProperties[prtyName] <- tuple
                // emit SIG06, since the inner property overrides some inherited property
                prty.ErrorOccurred <- emitSIG06iagnostic prtyName oldFromNode newFromNode typeName prty.StartPos prty.EndPos
        )
        // add inherited variables, if they still do not exist in scope
        _inheritedVariables
        |> Seq.iter (fun kvp ->
            if this.Scope.ContainsKey(kvp.Key) then 
                () // VAR06 was already emitted
            else
                let var = kvp.Value[0]
                this.Scope.Add (kvp.Key, var)
        )
        // add inherited properties, if they still do not exist in scope
        _inheritedProperties
        |> Seq.iter (fun kvp ->
            if this.Scope.ContainsKey(kvp.Key) then 
                () // SIG06 was already emitted
            else
                let prty = kvp.Value[0]
                this.Scope.Add (kvp.Key, prty)
        )

let runIntrinsicPredicate (fv:FplGenericHasValue) = 
    match box fv with
    | :? IConstant as fvConstant ->
        fvConstant.SetConstantName()
        let instance = new FplInstance(fv.TypeId, (fv.StartPos, fv.EndPos), fv)
        instance.FplId <- fvConstant.ConstantName
        fv.SetValue instance
    | _ ->
        fv.SetDefaultValue()

let runArgsAndSetWithLastValue (fv:FplGenericHasValue) =
    // run all statements and the last predicate in the FplPredicate
    fv.ArgList |> Seq.iter (fun fv1 -> fv1.Run()) 
    // Assign the value of the FplPredicate using the last predicate
    let lastOpt = fv.ArgList |> Seq.tryLast
    match lastOpt with 
    | Some (:? FplGenericHasValue as last) -> fv.SetValueOf last
    | _ -> fv.SetDefaultValue()

type FplPredicate(positions: Positions, parent: FplGenericNode, runOrder) as this =
    inherit FplGenericInheriting(positions, parent)
    let _runOrder = runOrder
    let mutable _signStartPos = Position("", 0L, 0L, 0L)
    let mutable _signEndPos = Position("", 0L, 0L, 0L)
    let mutable _isReady = false
    let mutable _callCounter = 0
    let mutable _constantName = ""

    do 
        this.FplId <- LiteralTrue
        this.TypeId <- LiteralPred

    member this.SignStartPos
        with get() = _signStartPos
        and set(value) = _signStartPos <- value

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

    member this.ConstantName = _constantName
    member this.SetConstantName() = _constantName <- signatureRepresent this

    interface IConstant with
        member this.ConstantName = this.ConstantName 
        member this.SetConstantName() = this.SetConstantName() 


    interface ICanBeCalledRecusively with
        member _.CallCounter = _callCounter

    override this.Name = PrimPredicateL
    override this.ShortName = PrimPredicate

    override this.Clone () =
        let ret = new FplPredicate((this.StartPos, this.EndPos), this.Parent.Value, _runOrder)
        this.AssignParts(ret)
        ret

    override this.IsFplBlock () = true

    override this.IsBlock () = true

    override this.CheckConsistency() = 
        base.CheckConsistency()
        match this.ExpressionType with
        | FixType.Infix _ when this.Arity <> 2 -> this.ErrorOccurred <- emitSIG00Diagnostics this.ExpressionType.Type 2 this.Arity this.SignStartPos this.SignEndPos
        | FixType.Prefix _ when this.Arity <> 1 -> this.ErrorOccurred <- emitSIG00Diagnostics this.ExpressionType.Type 1 this.Arity this.SignStartPos this.SignEndPos
        | FixType.Postfix _ when this.Arity <> 1 -> this.ErrorOccurred <- emitSIG00Diagnostics this.ExpressionType.Type 1 this.Arity this.SignStartPos this.SignEndPos
        | _ -> ()
        match this.ExpressionType with
        | FixType.Infix (symbol, precedence) -> checkSIG02Diagnostics this symbol precedence this.SignStartPos this.SignEndPos
        | _ -> ()
        checkPredicateExpressionReturnsPredicate this

    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        if not this.IsIntrinsic then // if not intrinsic, check variable usage
            checkVAR04Diagnostics this
        tryAddToParentUsingMixedSignature this
        
    override this.Type signatureType = 
        let head = getFplHead this signatureType

        let paramT = getParamTuple this signatureType
        sprintf "%s(%s)" head paramT

    override this.Run() = 
        debug this Debug.Start
        if not _isReady then
            _callCounter <- _callCounter + 1
            if _callCounter > maxRecursion then
                this.SetDefaultValue()
                this.ErrorOccurred <- emitLG002diagnostic (this.Type(SignatureType.Name)) _callCounter heap.Helper.CallerStartPos heap.Helper.CallerEndPos
            else
                if this.IsIntrinsic then 
                    runIntrinsicPredicate this
                else
                    runArgsAndSetWithLastValue this

            _callCounter <- _callCounter - 1
            _isReady <- this.Arity = 0        
            this.GetProperties() |> List.iter (fun fv -> fv.Run())
        debug this Debug.Stop

    override this.RunOrder = Some _runOrder

type FplBase(positions: Positions, parent: FplGenericNode) =
    inherit FplGenericNode(positions, Some parent)

    override this.Name = LiteralBase
    override this.ShortName = LiteralBase

    override this.Clone () =
        let ret = new FplBase((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type _ = this.FplId

    override this.EmbedInSymbolTable _ = addExpressionToParentArgList this

    override this.Run() = 
        // FplBase no value on its own
        ()

    override this.RunOrder = None

[<AbstractClass>]
type FplGenericConstructor(name, positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericHasValue(positions, parent)
    let mutable (_toBeConstructedClass:FplGenericNode option) = None 
    let mutable _constantName = ""

    do
        this.FplId <- name
        this.TypeId <- name

    override this.Name = PrimDefaultConstructor
    override this.ShortName = LiteralCtor

    member this.ConstantName = _constantName
    member this.SetConstantName() = _constantName <- signatureRepresent this

    interface IConstant with
        member this.ConstantName = this.ConstantName 
        member this.SetConstantName() = this.SetConstantName() 

    override this.Type signatureType =
        let head = getFplHead this signatureType
        let paramT = getParamTuple this signatureType
        match signatureType with
        | SignatureType.Name
        | SignatureType.Mixed -> $"{head}({paramT})" 
        | SignatureType.Type -> head
            
    member this.ToBeConstructedClass  
        with get () = _toBeConstructedClass
        and set (value) = _toBeConstructedClass <- value

    override this.Run() = 
        debug this Debug.Start

        let rec createSubInstance (classDef:FplGenericNode) (instance:FplGenericNode) (baseInstance:FplGenericNode)=
            classDef.ArgList
            |> Seq.filter (fun fv -> fv.Name = LiteralBase)
            |> Seq.map (fun fv -> fv :?> FplBase)
            |> Seq.map (fun fv -> fv.RefersTo)
            |> Seq.iter (fun baseClassOpt ->
                match baseClassOpt with
                | Some baseClass ->
                    let subInstance = new FplInstance(baseClass.TypeId, (this.StartPos, this.EndPos), this)
                    subInstance.FplId <- baseClass.FplId
                    subInstance.TypeId <- subInstance.FplId
                    createSubInstance baseClass subInstance baseInstance
                    instance.ArgList.Add subInstance
                | _ -> ()
            )
        this.SetConstantName()

        let instance = new FplInstance(this.TypeId, (this.StartPos, this.EndPos), this)
        match this.ToBeConstructedClass with
        | Some classDef -> 
            instance.FplId <- this.ConstantName
            instance.TypeId <- classDef.FplId
            this.ArgList 
            |> Seq.iter (fun fv ->
                fv.Run()
            )
            createSubInstance classDef instance instance
        | None ->
            instance.FplId <- LiteralUndef
            instance.TypeId <- LiteralUndef
        // the value of FplGenericConstructor is the created instance
        this.SetValue instance 
        debug this Debug.Stop


    member this.Instance =
        match this.Value with 
        | Some ref -> Some (ref :?> FplInstance)
        | _ -> None

    override this.RunOrder = None

/// This constructor is only used for creating instances of classes that have no declared constructors.
/// In FPL, such classes are "intrinsic". When the default constructor calls the constructor
/// of some base classes, it is only possible if those classes are also intrinsic or have declared constructors
/// without parameters. 
type FplDefaultConstructor(name, positions: Positions, parent: FplGenericNode) =
    inherit FplGenericConstructor(name, positions, parent)

    override this.Name = PrimDefaultConstructor
    override this.ShortName = LiteralCtor

    override this.Clone () =
        let ret = new FplDefaultConstructor(this.FplId, (this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.EmbedInSymbolTable nextOpt = 
        this.CheckConsistency()
        match nextOpt with 
        | Some next ->
            next.Scope.TryAdd(this.FplId, this) |> ignore
        | _ -> ()

type FplConstructor(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericConstructor(parent.FplId, positions, parent)
    let mutable _signStartPos = Position("", 0L, 0L, 0L)
    let mutable _signEndPos = Position("", 0L, 0L, 0L)
    let mutable _parentConstructorCalls = HashSet<string>()

    do 
        this.ToBeConstructedClass <- Some parent

    member this.SignStartPos
        with get() = _signStartPos
        and set(value) = _signStartPos <- value

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

    override this.Name = LiteralCtorL
    override this.ShortName = LiteralCtor

    override this.Clone () =
        let ret = new FplConstructor((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    member this.ParentConstructorCalls = _parentConstructorCalls

    override this.IsBlock () = true

    override this.CheckConsistency () = 
        base.CheckConsistency()
        // check if the constructor calls all necessary parent classes
        let parentClassOpt = this.UltimateBlockNode
        match parentClassOpt with
        | Some (:? FplClass as parentClass) ->
            parentClass.ArgList 
            |> Seq.iter (fun fv -> 
                if not (this.ParentConstructorCalls.Contains fv.FplId) then
                    fv.ErrorOccurred <- emitID020Diagnostics fv.FplId fv.StartPos
            )
        | _ -> ()
        checkVAR04Diagnostics this

    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        tryAddSubBlockToFplBlock this

    member this.ParentClass = this.Parent.Value :?> FplClass

and FplClass(positions: Positions, parent: FplGenericNode, runOrder) as this =
    inherit FplGenericInheriting(positions, parent)
    let _runOrder = runOrder
    let mutable _signStartPos = Position("", 0L, 0L, 0L)
    let mutable _signEndPos = Position("", 0L, 0L, 0L)

    do
        this.FplId <- LiteralObj
        this.TypeId <- LiteralObj

    member this.SignStartPos
        with get() = _signStartPos
        and set(value) = _signStartPos <- value

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

    override this.Name = PrimClassL
    override this.ShortName = PrimClass

    override this.Clone () =
        let ret = new FplClass((this.StartPos, this.EndPos), this.Parent.Value, _runOrder)
        this.AssignParts(ret)
        ret

    override this.IsFplBlock () = true
    override this.IsBlock () = true
    override this.IsClass () = true
    
    /// If this is a class definition, the function will return a list (possibly empty) list of all of its constructors.
    member this.GetConstructors() =
        this.Scope
        |> Seq.map (fun kvp -> kvp.Value)
        |> Seq.filter (fun fv -> fv.Name = LiteralCtorL)
        |> Seq.toList
    
    override this.Type signatureType = getFplHead this signatureType

    override this.Run() = 
        // initialization of the stmts in the class and/or its constructors and/or properties not needed since
        // it will be done inside instances
        // FplClass has no value on their own
        ()

    override this.CheckConsistency () = 
        base.CheckConsistency()
        checkVAR04Diagnostics this

    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        tryAddToParentUsingFplId this 

    override this.RunOrder = Some _runOrder

    member this.AddDefaultConstructor () = 
        let defaultConstructor = new FplDefaultConstructor(this.FplId, (this.StartPos, this.EndPos), this)
        defaultConstructor.EmbedInSymbolTable defaultConstructor.Parent
        defaultConstructor.ToBeConstructedClass <- Some this

let getDefaultValueOfFunction (fv:FplGenericHasValue) =
    let mapOpt = getMapping fv
    match mapOpt with
    | Some (:? FplMapping as map) ->
        let instance =
            match map.RefersTo with 
            | Some cl when cl.Name = PrimClassL && map.Dimensionality = 0 ->
                // delegate instance building to a default class constructor if the mapping refers to a class
                let defaultCtor = cl.Scope.Values |> Seq.head :?> FplGenericConstructor
                defaultCtor.Run()
                match defaultCtor.Instance with 
                | Some inst -> 
                    inst.Parent <- Some fv
                    inst 
                | None -> 
                    new FplInstance(map.TypeId, (fv.StartPos, fv.EndPos), fv)
            | _ ->
                new FplInstance(map.TypeId, (fv.StartPos, fv.EndPos), fv)
        instance 
    | _ ->
        (new FplInstance(fv.TypeId, (fv.StartPos, fv.EndPos), fv))

let runIntrinsicFunction (fv:FplGenericHasValue) =
    match box fv with
    | :? IConstant as fvConstant ->
        let instance = getDefaultValueOfFunction fv
        fvConstant.SetConstantName()
        instance.FplId <- fvConstant.ConstantName
        fv.SetValue instance
    | _ ->
        fv.SetValue (getDefaultValueOfFunction fv)

let getFunctionalTermRepresent (fv:FplGenericHasValue) =
    let defaultReprsentation (fv1:FplGenericHasValue)= 
        match fv1.Value with 
        | None ->
            // since the function term has no value, it has no return statement
            // And the FPL syntax ensures that this can only be the case
            // if the Functional Term is intrinsic.
            // In this case, the "representation" of the function is
            // its declared mapping type
            let mapping = fv1.ArgList[0]
            $"dec {mapping.Type(SignatureType.Mixed)}"
        | Some v -> v.Represent()
    if fv.IsIntrinsic then
        match box fv with
        | :? IConstant as fvConstant ->
            fvConstant.ConstantName
        | _ -> defaultReprsentation fv
    else
        defaultReprsentation fv

type FplFunctionalTerm(positions: Positions, parent: FplGenericNode, runOrder) as this =
    inherit FplGenericInheriting(positions, parent)
    let mutable _signStartPos = Position("", 0L, 0L, 0L)
    let mutable _signEndPos = Position("", 0L, 0L, 0L)
    let _runOrder = runOrder
    let mutable _isReady = false
    let mutable _callCounter = 0
    let mutable _constantName = ""

    do 
        this.FplId <- LiteralFunc
        this.TypeId <- LiteralFunc

    member this.SignStartPos
        with get() = _signStartPos
        and set(value) = _signStartPos <- value

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

    member this.ConstantName = _constantName
    member this.SetConstantName() = _constantName <- signatureRepresent this

    interface IConstant with
        member this.ConstantName = this.ConstantName 
        member this.SetConstantName() = this.SetConstantName() 

    override this.Name = PrimFunctionalTermL
    override this.ShortName = PrimFunctionalTerm

    override this.Clone () =
        let ret = new FplFunctionalTerm((this.StartPos, this.EndPos), this.Parent.Value, _runOrder)
        this.AssignParts(ret)
        ret

    override this.IsFplBlock () = true
    override this.IsBlock () = true

    override this.Type signatureType = 
        let head = getFplHead this signatureType
        let propagate = propagateSignatureType signatureType

        match getMapping this with
        | Some map ->
            let paramT = getParamTuple this signatureType
            sprintf "%s(%s) -> %s" head paramT (map.Type(propagate))
        | _ -> ""

    override this.CheckConsistency (): unit = 
        base.CheckConsistency()
        if not this.IsIntrinsic then // if not intrinsic, check variable usage
            checkVAR04Diagnostics this
        match this.ExpressionType with
        | FixType.Infix _ when this.Arity <> 2 -> this.ErrorOccurred <- emitSIG00Diagnostics this.ExpressionType.Type 2 this.Arity this.SignStartPos this.SignEndPos
        | FixType.Prefix _ when this.Arity <> 1 -> this.ErrorOccurred <- emitSIG00Diagnostics this.ExpressionType.Type 1 this.Arity this.SignStartPos this.SignEndPos
        | FixType.Postfix _ when this.Arity <> 1 -> this.ErrorOccurred <- emitSIG00Diagnostics this.ExpressionType.Type 1 this.Arity this.SignStartPos this.SignEndPos
        | _ -> ()
        match this.ExpressionType with
        | FixType.Infix (symbol, precedence) -> checkSIG02Diagnostics heap.Root symbol precedence this.SignStartPos this.SignEndPos
        | _ -> ()
        checkSIG11Diagnostics this

    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        tryAddToParentUsingMixedSignature this

    override this.RunOrder = Some _runOrder

    override this.Represent() = // done
        if _callCounter > maxRecursion then
            this.ErrorOccurred <- emitLG002diagnostic (this.Type(SignatureType.Name)) _callCounter this.StartPos this.EndPos
            PrimUndetermined
        else
            _callCounter <- _callCounter + 1
            let result = getFunctionalTermRepresent this
            _callCounter <- _callCounter - 1
            result

    override this.Run() = 
        debug this Debug.Start
        if not _isReady then
            _callCounter <- _callCounter + 1
            if _callCounter > maxRecursion then
                let instance = getDefaultValueOfFunction this
                this.SetValue instance
                this.ErrorOccurred <- emitLG002diagnostic (this.Type(SignatureType.Name)) _callCounter heap.Helper.CallerStartPos heap.Helper.CallerEndPos
            else
                if this.IsIntrinsic then 
                    runIntrinsicFunction this 
                else
                    runArgsAndSetWithLastValue this

                this.GetProperties()
                |> List.iter (fun fv -> fv.Run())
            _callCounter <- _callCounter - 1
            _isReady <- this.Arity = 0 
        debug this Debug.Stop

