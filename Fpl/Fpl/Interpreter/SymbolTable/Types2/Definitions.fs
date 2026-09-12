(* Copyright (c) 2021+ bookofproofs See LICENSE in the project root for license terms. *)

/// <summary>
/// Module containing symbol-table node implementations for definitions, classes,
/// constructors, predicates and functional terms used by the FPL interpreter.
/// </summary>
/// <remarks>
/// This module implements inheritance handling, constructor/instance creation,
/// predicate and functional term evaluation, default value creation for functions,
/// and diagnostic emission during symbol-table construction. Diagnostics are emitted
/// via helper emitter functions; methods generally record diagnostics on nodes rather
/// than throwing exceptions.
/// </remarks>
module Fpl.Interpreter.SymbolTable.Types2.Definitions
open System.Collections.Generic
open FParsec
open Fpl.Parser.Types
open Fpl.Primitives
open Fpl.Errors.Emitter
open Fpl.Interpreter.BasicTypes
open Fpl.Interpreter.Helpers.Checks
open Fpl.Interpreter.Helpers.Basic
open Fpl.Interpreter.Helpers.Debug
open Fpl.Interpreter.SymbolTable.Storage.Heap
open Fpl.Interpreter.SymbolTable.Storage.Util
open Fpl.Interpreter.SymbolTable.Types2.Intrinsic
open Fpl.Interpreter.SymbolTable.Types2.Variables

/// <summary>
/// Abstract base for nodes that inherit variables and properties from other nodes.
/// </summary>
/// <param name="positions">Source positions used for diagnostics.</param>
/// <param name="parent">Parent node in the symbol table.</param>
/// <remarks>
/// Provides helpers to track inherited variables and properties so clones preserve
/// reference identity. Emits diagnostics when inherited members are overridden or
/// conflict with local declarations.
/// </remarks>
[<AbstractClass>]
type FplGenericInheriting(positions: Positions, parent: FplGenericNode) =
    inherit FplGenericHasValue(positions, parent)
    // used to ensure that every clone of FplGenericInheriting will preserve reference identity of inherited variables 
    let _inheritedVariables = Dictionary<string, List<FplGenericNode>>()
    // used to ensure that every clone of FplGenericInheriting will preserve reference identity of inherited properties
    let _inheritedProperties = Dictionary<string, List<FplGenericNode>>()

    /// <summary>
    /// Wraps an inherited object together with the node it was inherited from and stores it
    /// in a dictionary keyed by the inherited object's identifier.
    /// </summary>
    /// <param name="keyOfInheritedObject">Key under which the inherited object is stored.</param>
    /// <param name="mapOfInheritedObjects">Dictionary that records inherited objects.</param>
    /// <param name="inheritedObject">The inherited node being wrapped.</param>
    /// <param name="newFromNode">The node that provides the inherited object.</param>
    /// <param name="withCloning">If true, clones the inherited object before storing.</param>
    /// <returns>
    /// A tuple with (Some oldFromName, Some oldFromType, Some newFromType) when an override occurred,
    /// or (None, None, None) when the inherited key was newly added.
    /// </returns>
    /// <remarks>
    /// The function optionally clones the inherited object to preserve identity semantics
    /// when creating overrides in derived nodes.
    /// </remarks>
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
            
    /// <summary>
    /// Import variables from the given base node into this node's inherited variables map.
    /// </summary>
    /// <param name="fromBaseNode">Node to inherit variables from.</param>
    /// <remarks>
    /// Emits VAR06 diagnostics when an inherited variable conflicts with an existing one.
    /// </remarks>
    member this.InheritVariables (fromBaseNode:FplGenericNode) = 
        fromBaseNode.GetVariables()
        |> List.iter (fun var ->
            match this.OverrideInheritedObject var.FplId _inheritedVariables var fromBaseNode true with
            | (Some typeName, Some oldFromNode, Some newFromNode) ->
                fromBaseNode.ErrorOccurred <- emitVAR06iagnostic var.FplId oldFromNode newFromNode typeName fromBaseNode.StartPos fromBaseNode.EndPos
            | _ ->
                ()
        )

    /// <summary>
    /// Import properties from the given base node into this node's inherited properties map.
    /// </summary>
    /// <param name="fromBaseNode">Node to inherit properties from.</param>
    /// <remarks>
    /// Emits SIG06 diagnostics on property signature conflicts.
    /// </remarks>
    member this.InheritProperties (fromBaseNode:FplGenericNode) = 
        fromBaseNode.GetProperties()
        |> List.iter (fun prty ->
            let prtyName = prty.Type SignatureType.Mixed
            let prtyQ = box prty :?> IHasSignature 
            match this.OverrideInheritedObject prtyName _inheritedProperties prty fromBaseNode true with
            | (Some _, Some oldFromNode, Some newFromNode) ->
                fromBaseNode.ErrorOccurred <- emitSIG06Diagnostics prtyName oldFromNode newFromNode fromBaseNode.Name prtyQ.SignStartPos prtyQ.SignEndPos
            | _ ->
                ()
        )

    /// <summary>
    /// Checks inheritance-related consistency: overridden variables/properties and scope population
    /// with inherited members that are not shadowed by local declarations.
    /// </summary>
    /// <remarks>
    /// Emits diagnostics using emitter helpers; diagnostics are recorded on nodes and not thrown.
    /// </remarks>
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
                // override the old node
                let tuple = List<FplGenericNode>()
                tuple.Add prty // own scope property
                tuple.Add this // the property is from this
                _inheritedProperties[prtyName] <- tuple
                let prtyQ = box prty :?> IHasSignature 
                // emit SIG06, since the inner property overrides some inherited property
                prty.ErrorOccurred <- emitSIG06Diagnostics prtyName oldFromNode newFromNode oldFrom.Name prtyQ.SignStartPos prtyQ.SignEndPos
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

/// <summary>
/// Evaluate an intrinsic predicate value for the given node.
/// </summary>
/// <param name="fv">Predicate node to evaluate.</param>
/// <remarks>
/// If the node implements <c>IConstant</c> its constant name is set and an instance node
/// is created as the predicate value. Otherwise the default undetermined value is set.
/// </remarks>
let runIntrinsicPredicate (fv:FplGenericHasValue) = 
    match box fv with
    | :? IConstant as fvConstant ->
        fvConstant.SetConstantName()
        let instance = new FplInstance(fv.TypeId, (fv.StartPos, fv.EndPos), fv)
        instance.FplId <- fvConstant.ConstantName
        fv.SetValue instance
    | _ ->
        fv.SetDefaultValue()

/// <summary>
/// Execute all arguments of <paramref name="fv"/> and set its value to the last argument's value.
/// </summary>
/// <param name="fv">Node whose arguments are run and whose value will be set.</param>
/// <remarks>
/// If the last argument is missing or not a value-carrying node, the node's value is set to the default.
/// </remarks>
let runArgsAndSetWithLastValue (fv:FplGenericHasValue) =
    // run all statements and the last predicate in the FplPredicate
    fv.ArgList |> Seq.iter (fun fv1 -> fv1.Run()) 
    // Assign the value of the FplPredicate using the last predicate
    let lastOpt = fv.ArgList |> Seq.tryLast
    match lastOpt with 
    | Some (:? FplGenericHasValue as last) -> fv.SetValueOf last
    | _ -> fv.SetDefaultValue()

/// <summary>
/// Predicate block node that can inherit variables/properties and be evaluated.
/// </summary>
/// <param name="positions">Source positions used for diagnostics.</param>
/// <param name="parent">Parent node in the symbol table.</param>
/// <param name="runOrder">Execution ordering index for this predicate block.</param>
/// <remarks>
/// Supports recursion detection, intrinsic predicate handling and signature validation.
/// </remarks>
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

    /// <summary>
    /// Clone the predicate block preserving its parts.
    /// </summary>
    override this.Clone () =
        let ret = new FplPredicate((this.StartPos, this.EndPos), this.Parent.Value, _runOrder)
        this.AssignParts(ret)
        ret

    override this.IsFplBlock () = true

    override this.IsBlock () = true

    /// <summary>
    /// Perform signature/arity checks and predicate return type validation.
    /// </summary>
    /// <remarks>
    /// Emits SIG00/SIG02 diagnostics and uses <c>checkPredicateExpressionReturnsPredicate</c> to validate the body.
    /// </remarks>
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

    /// <summary>
    /// Embed the predicate into the parent's symbol table and perform variable-usage diagnostics.
    /// </summary>
    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        if not this.IsIntrinsic then // if not intrinsic, check variable usage
            checkVAR04Diagnostics this
        tryAddToParentUsingMixedSignature this
        
    override this.Type signatureType = 
        let head = getFplHead this signatureType

        let paramT = getParamTuple this signatureType
        sprintf "%s(%s)" head paramT

    /// <summary>
    /// Evaluate the predicate block; intrinsic predicates are handled specially.
    /// </summary>
    /// <remarks>
    /// Detects recursion, runs intrinsic handler or runs all args and sets the last value.
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
            this.GetProperties() |> List.iter (fun fv -> fv.Run())
        StaticDebug.Debug(this,Debug.Stop)

    override this.RunOrder = Some _runOrder

/// <summary>
/// Represents a base declaration inside class definitions used for inheritance processing.
/// </summary>
/// <param name="positions">Source positions for diagnostics.</param>
/// <param name="parent">Parent node in the symbol table.</param>
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

/// <summary>
/// Generic constructor abstraction that can create instances of classes and generate instances
/// as node values.
/// </summary>
/// <param name="name">Constructor name (typically derived from the class).</param>
/// <param name="positions">Source positions for diagnostics.</param>
/// <param name="parent">Parent node representing the owning class.</param>
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

    /// <summary>
    /// Execute the constructor producing an instance value and assigning it to this node.
    /// </summary>
    /// <remarks>
    /// Creates nested instances for base classes and records constant name. If no class
    /// definition is available, sets the instance to undef.
    /// </remarks>
    override this.Run() = 
        StaticDebug.Debug(this,Debug.Start)

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
        StaticDebug.Debug(this,Debug.Stop)


    member this.Instance =
        match this.Value with 
        | Some ref -> Some (ref :?> FplInstance)
        | _ -> None

    override this.RunOrder = None

/// <summary>
/// Default constructor used for classes that declare no constructors explicitly.
/// </summary>
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

/// <summary>
/// Concrete constructor node referenced by classes.
/// </summary>
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

    /// <summary>
    /// Validate constructor consistency, ensuring required parent constructor calls exist
    /// and that variables are checked for unused declarations.
    /// </summary>
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

/// <summary>
/// Class definition node supporting inheritance, constructors and properties.
/// </summary>
/// <param name="positions">Source positions for diagnostics.</param>
/// <param name="parent">Parent node in the symbol table.</param>
/// <param name="runOrder">Execution ordering index for members of this class.</param>
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
    
    /// <summary>
    /// Return the list of constructors declared in this class.
    /// </summary>
    /// <returns>List of constructor nodes (possibly empty).</returns>
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

    /// <summary>
    /// Embed the class into the parent scope using its FPL identifier.
    /// </summary>
    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        tryAddToParentUsingFplId this 

    override this.RunOrder = Some _runOrder

    /// <summary>
    /// Add a default constructor to this class when no constructors are declared.
    /// </summary>
    member this.AddDefaultConstructor () = 
        let defaultConstructor = new FplDefaultConstructor(this.FplId, (this.StartPos, this.EndPos), this)
        defaultConstructor.EmbedInSymbolTable defaultConstructor.Parent
        defaultConstructor.ToBeConstructedClass <- Some this

/// <summary>
/// Return a default instance node for the given function-like node based on its mapping type.
/// </summary>
/// <param name="fv">Function-like node to produce a default instance for.</param>
/// <returns>An instance node representing the default value for the function's return type.</returns>
/// <remarks>
/// If the mapping refers to a class with a default constructor, that constructor is used to construct the instance.
/// </remarks>
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

/// <summary>
/// If <paramref name="fv"/> is intrinsic and constant, create and set a default instance value,
/// otherwise set a function default instance for the node.
/// </summary>
/// <param name="fv">Function-like node to initialize.</param>
let runIntrinsicFunction (fv:FplGenericHasValue) =
    match box fv with
    | :? IConstant as fvConstant ->
        let instance = getDefaultValueOfFunction fv
        fvConstant.SetConstantName()
        instance.FplId <- fvConstant.ConstantName
        fv.SetValue instance
    | _ ->
        fv.SetValue (getDefaultValueOfFunction fv)

/// <summary>
/// Compute a representation string for a functional term.
/// </summary>
/// <param name="fv">Functional term node to represent.</param>
/// <returns>Representation string for the functional term or its value.</returns>
/// <remarks>
/// For intrinsic functional terms without a value, the representation is derived from the declared mapping type.
/// Otherwise the value's representation is used.
/// </remarks>
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

/// <summary>
/// Functional term block node that can be called and returns values according to its mapping.
/// </summary>
/// <param name="positions">Source positions for diagnostics.</param>
/// <param name="parent">Parent node in the symbol table.</param>
/// <param name="runOrder">Execution ordering index for this functional term block.</param>
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

    /// <summary>
    /// Perform consistency checks for the functional term, including signature and mapping checks.
    /// </summary>
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

    /// <summary>
    /// Return a string representation for the functional term or its current value.
    /// </summary>
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
    /// Evaluate the functional term, creating a default instance for the return type when needed.
    /// </summary>
    /// <remarks>
    /// Handles recursion detection, intrinsic function initialization and runs the block body when not intrinsic.
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

                this.GetProperties()
                |> List.iter (fun fv -> fv.Run())
            _callCounter <- _callCounter - 1
            _isReady <- this.Arity = 0 
        StaticDebug.Debug(this,Debug.Stop)

