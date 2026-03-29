/// This module contains all types necessary to interpret FPL code (semantics)
(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)
module FplInterpreterTypes

open System
open System.Text.RegularExpressions
open System.Collections.Generic
open System.Text
open FParsec
open FplPrimitives
open FplGrammarTypes
open ErrDiagnostics
open FplInterpreterDiagnosticsEmitter
open FplInterpreterAstPreprocessing
open FplInterpreterBasicTypes
open FplInterpreterGlobals
open FplInterpreterChecks
open FplInterpreterSTEmbedding
open FplInterpreterReferences
open FplInterpreterIntrinsicTypes
open FplInterpreterVariables
open FplInterpreterDefinitions
open FplInterpreterFplTypeMatching
open FplInterpreterPredicativeBlocks
open FplInterpreterProofs




type FplLocalization(positions: Positions, parent: FplGenericNode, runOrder) =
    inherit FplGenericNode(positions, Some parent)
    let _runOrder = runOrder
    let mutable _currentLanguage = ""

    override this.Name = LiteralLocL
    override this.ShortName = LiteralLoc

    override this.Clone () =
        let ret = new FplLocalization((this.StartPos, this.EndPos), this.Parent.Value, _runOrder)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = 
        let head = getFplHead this signatureType
        let paramT =
            this.Scope
            |> Seq.filter (fun (kvp: KeyValuePair<string, FplGenericNode>) -> isVar kvp.Value)
            |> Seq.map (fun kvp -> kvp.Value)
            |> Seq.map (fun fv -> fv.Type signatureType)
            |> String.concat ", "

        match paramT with
        | "" -> head
        | _ -> sprintf "%s(%s)" head paramT

    override this.Represent() = // done
        if this.Scope.ContainsKey(_currentLanguage) then
            let language = this.Scope[_currentLanguage]
            language.Represent() // represent the current language
        else
            this.Type(SignatureType.Name) 
        
    override this.IsBlock() = true

    override this.Run() = 
        debug this Debug.Start
        _currentLanguage <- variableStack.CurrentLanguage // remember current language for Represent()
        if not (this.Scope.ContainsKey(_currentLanguage)) then
            let expression = this.ArgList[0]
            this.ErrorOccurred <- emitST004diagnostics _currentLanguage expression.StartPos expression.EndPos
        debug this Debug.Stop

    override this.RunOrder = Some _runOrder

    override this.EmbedInSymbolTable _ = tryAddToParentUsingTypedSignature this

type FplTranslation(positions: Positions, parent: FplGenericNode) =
    inherit FplGenericNode(positions, Some parent)

    override this.Name = PrimTranslationL
    override this.ShortName = PrimTranslation

    override this.Clone () =
        let ret = new FplTranslation((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = 
        let head = getFplHead this signatureType
        let args = signatureSep " " this.ArgList SignatureType.Name
        sprintf "%s%s" head args

    override this.Represent() = // done
        this.FplId // represent according to string in the FplId of the translation term

    override this.Run() = 
        // no run necessary 
        ()

    override this.EmbedInSymbolTable _ = addExpressionToParentArgList this 

    override this.RunOrder = None

type FplLanguage(positions: Positions, parent: FplGenericNode) =
    inherit FplGenericNode(positions, Some parent)

    override this.Name = PrimLanguageL
    override this.ShortName = PrimLanguage

    override this.Clone () =
        let ret = new FplLanguage((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType =
        let head = getFplHead this signatureType
        head

    override this.Represent() = // done
        // concatenate all translations of the language
        representationSep " " this.ArgList 

    override this.Run() = 
        // no run necessary 
        ()

    override this.EmbedInSymbolTable _ = 
        let parent = this.Parent.Value
        if parent.Scope.ContainsKey(this.FplId) then 
            let conflict = parent.Scope[this.FplId]
            this.ErrorOccurred <- emitID014Diagnostics this.FplId conflict.QualifiedStartPos this.StartPos this.EndPos 
        else
            parent.Scope.Add(this.FplId, this)

    override this.RunOrder = None



[<AbstractClass>]
type FplGenericStmt(positions: Positions, parent: FplGenericNode) =
    inherit FplGenericIsAction(positions, parent)

    override this.ShortName = PrimStmt

    override this.Type signatureType = this.FplId

    override this.EmbedInSymbolTable _ = addExpressionToParentArgList this

type FplAssertion(positions: Positions, parent: FplGenericNode) =
    inherit FplGenericStmt(positions, parent)

    override this.Name = PrimAssertion
    override this.ShortName = LiteralAss

    override this.Clone () =
        let ret = new FplAssertion((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Run() = 
        // TODO implement run
        debug this Debug.Start
        debug this Debug.Stop

    override this.RunOrder = None




/// Implements an object that is used to provide a representation of extensions in FPL.
type FplExtensionObj(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericIsValue(positions, parent)

    do 
        this.TypeId <- LiteralObj


    override this.Name = PrimExtensionObj
    override this.ShortName = LiteralObj

    override this.Clone () =
        let ret = new FplExtensionObj((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

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

    override this.Represent() = 
        this.FplId // return FplId

    override this.Run() = 
        ()

    override this.CheckConsistency () = 
        base.CheckConsistency()
        let matchReprId (fv1:FplGenericNode) (identifier:string) = 
            let regex = Regex(fv1.TypeId)
            regex.IsMatch(identifier)
        
        let extensionCandidates =
            let root = getRoot this
            root.Scope
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

    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()    
        addExpressionToReference this

    override this.RunOrder = None


type FplMapCaseSingle(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericHasValue(positions, parent)
    do 
        this.FplId <- PrimMapCaseSingle

    override this.Name = PrimMapCaseSingleL
    override this.ShortName = PrimStmt

    override this.Clone () =
        let ret = new FplMapCaseSingle((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = 
        getFplHead this signatureType

    member this.GetCondition() = this.ArgList[0]
    member this.GetResult() = this.ArgList[1] :?> FplGenericHasValue

    override this.CheckConsistency() = 
        base.CheckConsistency()
        checkArgPred this (this.GetCondition())

    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        addExpressionToParentArgList this

    override this.RunOrder = None

    override this.Run() = 
        debug this Debug.Start
        let result = this.GetResult()
        result.Run()
        this.SetValueOf result
        debug this Debug.Stop

type FplMapCaseElse(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericHasValue(positions, parent)
    do 
        this.FplId <- PrimMapCaseElse

    override this.Name = PrimMapCaseElseL
    override this.ShortName = PrimStmt

    override this.Clone () =
        let ret = new FplMapCaseElse((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = 
        let argOpt = this.ArgList |> Seq.tryHead 
        match argOpt with 
        | Some arg -> arg.Type signatureType // delegate type to the argument of MapCaseElse case
        | _ -> getFplHead this signatureType // fallback (should never occur due to FPL syntax)

    override this.EmbedInSymbolTable _ = addExpressionToParentArgList this

    override this.RunOrder = None

    override this.Run() = 
        debug this Debug.Start
        let first = this.ArgList |> Seq.head
        let contentOfElsResult = first :?> FplGenericHasValue
        contentOfElsResult.Run()
        this.SetValueOf contentOfElsResult
        debug this Debug.Stop

type FplMapCases(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericHasValue(positions, parent)
    let _consistentCaseType = new FplIntrinsicTpl("", positions, parent)
    let _reachableCases = new HashSet<string>()

    do 
        this.FplId <- LiteralMapCases

    override this.Name = PrimMapCasesL
    override this.ShortName = PrimStmt

    override this.Clone () =
        let ret = new FplMapCases((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = 
        getFplHead this signatureType

    member this.GetConditionResultList() = 
        this.ArgList
        |> Seq.choose (fun item ->
            match item with
            | :? FplMapCaseSingle as condRes -> Some condRes
            | _ -> None)
        |> Seq.toList

    member this.GetMapElse() = 
        let last = this.ArgList |> Seq.last
        last :?> FplGenericHasValue

    member private this.CheckAllResultsForEqualType() =
        // check if all results have the same type
        this.GetConditionResultList()
        |> Seq.map (fun conditionResultPair -> conditionResultPair.GetResult())
        |> Seq.iter (fun result -> _consistentCaseType.TrySetTemplateUsage result (SIG13("", "", "", "").Code))
        // check also else result
        _consistentCaseType.TrySetTemplateUsage (this.GetMapElse()) (SIG13("", "", "", "").Code)
        match _consistentCaseType.ErrorOccurred with
        | Some errMsg -> 
            // Since there were proceeding errors regarding inconsistent Type Ids of some branches
            // set the TypeId of this FplMapCases to undefined
            this.TypeId <- LiteralUndef  
         | _ ->
            // Set the TypeId of this FplMapCases to the consistent TypeId found for all of its branches
            let typeOfAllBranches = _consistentCaseType.RefersTo.Value
            let mapOpt = getMapping typeOfAllBranches
            this.TypeId <- 
                match mapOpt with 
                | Some map -> map.TypeId // if the type of all branches is a mapping, use the mapping's TypeId
                | None -> typeOfAllBranches.TypeId

    member private this.CheckAllCasesForBeingReachable() =
        _reachableCases.Clear()
        this.GetConditionResultList()
        |> Seq.map (fun conditionResultPair -> conditionResultPair.GetCondition())
        |> Seq.iter (fun condition -> 
            let conditionSignature = condition.Type SignatureType.Name
            if _reachableCases.Add(conditionSignature) then 
                () // signature added
            else
                // signature was already added
                this.ErrorOccurred <- emitSIG14diagnostics condition.StartPos condition.EndPos
                
        )

    override this.CheckConsistency() = 
        base.CheckConsistency()
        this.CheckAllResultsForEqualType()
        this.CheckAllCasesForBeingReachable()


    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        addExpressionToParentArgList this

    override this.RunOrder = None

    override this.Run() = 
        debug this Debug.Start
        let resultLst = this.GetConditionResultList()
        let mapElse = this.GetMapElse()
        let firstMapCaseWithTrueConditionOpt = 
            resultLst
            |> Seq.tryFind(fun mapCaseSingle -> 
                let condition = mapCaseSingle.GetCondition()
                condition.Run()
                condition.Represent() = LiteralTrue
            )
        match firstMapCaseWithTrueConditionOpt with
        | Some firstMapCaseWithTrueCondition -> 
            firstMapCaseWithTrueCondition.Run()
            let resOfFound = firstMapCaseWithTrueCondition.GetResult()
            this.SetValueOf resOfFound
        | None -> 
            mapElse.Run()
            this.SetValueOf mapElse
        debug this Debug.Stop

/// Implements the return statement in FPL.
type FplReturn(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericHasValue(positions, parent)

    do
        this.FplId <- LiteralRet
        this.TypeId <- LiteralUndef

    override this.Name = PrimReturn
    override this.ShortName = PrimStmt

    override this.Clone () =
        let ret = new FplReturn((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = this.FplId

    override this.EmbedInSymbolTable _ = addExpressionToParentArgList this

    override this.RunOrder = None

    override this.Run() =
        debug this Debug.Start
        let returnedReference = this.ArgList[0]
        let blockOpt = this.NextBlockNode
        match blockOpt with 
        | Some funTerm ->
            let mapTypeOpt = getMapping funTerm
            match mapTypeOpt with 
            | Some mapType ->
                match mpwa [ returnedReference ] [ mapType ] with
                | Some errMsg -> returnedReference.ErrorOccurred <- emitSIG03Diagnostics errMsg (returnedReference.StartPos) (returnedReference.EndPos)
                | _ -> 
                    match returnedReference with
                    | :? FplIntrinsicPred 
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
            // then a functional term
            // in this case return default value
            this.SetDefaultValue()
        debug this Debug.Stop



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

    override this.Clone () =
        let ret = new FplExtension((this.StartPos, this.EndPos), this.Parent.Value, _runOrder)
        this.AssignParts(ret)
        ret

    // Returns a reference to the mapping of this extension
    member this.Mapping =
        let mapOpt = getMapping this
        match mapOpt with 
        | Some map -> map :?> FplMapping
        | None -> 
            let defaultMap = new FplMapping((this.StartPos, this.EndPos), this)
            defaultMap.FplId <- LiteralUndef
            defaultMap.TypeId <- LiteralUndef
            defaultMap


    override this.Type signatureType = 
        match signatureType with 
        | SignatureType.Name
        | SignatureType.Mixed -> $"{this.FplId} -> {this.Mapping.Type signatureType}" 
        | SignatureType.Type -> $"{this.Mapping.Type signatureType}"

    override this.IsBlock () = true

    /// Returns the (only one parameter) variable of this extension 
    member this.ExtensionVar = 
        let extensionVar = 
            getParameters this
            |> List.head
        (extensionVar :?> FplVariable)

    member private this.ReturnStmt =
        let last = this.ArgList |> Seq.last
        last :?> FplReturn

    override this.Run() = 
        debug this Debug.Start
        // run only if the extension variable was initialized
        _callCounter <- _callCounter + 1
        if _callCounter > maxRecursion then
            let instance = getDefaultValueOfFunction this
            this.SetValue instance
            this.ErrorOccurred <- emitLG002diagnostic (this.Type(SignatureType.Name)) _callCounter variableStack.CallerStartPos variableStack.CallerEndPos
        else
            if this.ArgList.Count = 0 then 
                let instance = getDefaultValueOfFunction this
                this.SetValue instance
            else
                runArgsAndSetWithLastValue this
        _callCounter <- _callCounter - 1
        debug this Debug.Stop

    override this.CheckConsistency () = 
        checkSIG11Diagnostics this
        base.CheckConsistency()

    override this.EmbedInSymbolTable _ =
        this.CheckConsistency()
        tryAddToParentUsingMixedSignature this

    override this.RunOrder = Some _runOrder




type FplBaseConstructorCall(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericReference(positions, parent)

    do 
        this.FplId <- LiteralObj
        this.TypeId <- LiteralObj

    override this.Name = PrimBaseConstructorCall
    override this.ShortName = PrimStmt

    override this.Type signatureType = 
        let head = getFplHead this signatureType
        let propagate = propagateSignatureType signatureType
        let args = signatureSep ", " this.ArgList propagate
        sprintf "%s(%s)" head args

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
                    this.ErrorOccurred <- emitID021Diagnostics this.FplId this.StartPos
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
                    // the base constructor call's id is not among the base classes this class is derived from
                    let candidates = outerClass.ArgList |> Seq.map (fun fv -> fv.FplId) |> Seq.sort |> String.concat ", "
                    this.ErrorOccurred <- emitID017Diagnostics this.FplId candidates this.StartPos this.EndPos
            | _ ->
                    this.ErrorOccurred <- emitID017Diagnostics this.FplId "" this.StartPos this.EndPos
                    registerParentConstructor()
        | _ ->
            // this case never happens, 
            // if so the bug will become apparent by failing to call the parent class constructor
            () 


    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        addExpressionToParentArgList this


/// Reference to "parent" using the FPL parent keyword. 
// It will point to a parent only inside FPL properties. Otherwise, it is undefined
type FplParent(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericNode(positions, Some parent)
    let mutable _callCounter = 0

    do 
        this.FplId <- LiteralParent
        this.TypeId <- LiteralUndef

    override this.Name = LiteralParent
    override this.ShortName = LiteralParent

    override this.Clone() = this // do not clone FplParent to prevent stack overflow 

    override this.Type signatureType = 
        match this.RefersTo with 
        | Some ref -> ref.Type signatureType
        | _ -> LiteralParent

    override this.Represent() = // done
        match this.RefersTo with 
        | Some ref -> 
            if _callCounter > maxRecursion then
                this.ErrorOccurred <- emitLG002diagnostic (this.Type(SignatureType.Name)) _callCounter this.StartPos this.EndPos
                PrimUndetermined
            else
                _callCounter <- _callCounter + 1
                let result = ref.Represent()
                _callCounter <- _callCounter - 1
                result
        | _ -> PrimUndetermined

    override this.Run() = 
        // FplParent has no value, unless it has a representable RefersTo
        ()

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

    override this.CheckConsistency (): unit =
        match this.ParentBlock with
        | ScopeSearchResult.FoundIncorrectBlock block ->
            this.ErrorOccurred <- emitID015diagnostics $"{getEnglishName block.Name true} '{block.Type(SignatureType.Name)}'" this.StartPos this.EndPos
        | _ -> ()
        base.CheckConsistency()

    override this.EmbedInSymbolTable _ =
        this.CheckConsistency()
        addExpressionToReference this

    override this.RunOrder = None



/// Reference to "self" using the FPL self keyword. 
// It will point to the enclosing block inside FPL predicate definitions, functional terms, and properties. Otherwise, it is undefined.
type FplSelf(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericNode(positions, Some parent)
    let mutable _callCounter = 0

    do 
        this.FplId <- LiteralSelf
        this.TypeId <- LiteralUndef

    override this.Name = LiteralSelf
    override this.ShortName = LiteralSelf

    override this.Clone() = this // do not clone FplSelf to prevent stack overflow 

    override this.Type signatureType = 
        match this.RefersTo with 
        | Some ref -> ref.Type signatureType
        | _ -> LiteralSelf

    override this.Represent() = // done
        match this.RefersTo with 
        | Some ref -> 
            if _callCounter > maxRecursion then
                this.ErrorOccurred <- emitLG002diagnostic (this.Type(SignatureType.Name)) _callCounter this.StartPos this.EndPos
                PrimUndetermined
            else
                _callCounter <- _callCounter + 1
                let result = ref.Represent()
                _callCounter <- _callCounter - 1
                result
        | _ -> PrimUndetermined

    override this.Run() = 
        // FplSelf has no value, unless it has a representable RefersTo
        ()

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

    override this.CheckConsistency () =
        match this.SelfBlock with
        | ScopeSearchResult.FoundIncorrectBlock block ->
            this.ErrorOccurred <- emitID016diagnostics $"{getEnglishName block.Name true} '{block.Type(SignatureType.Name)}'" this.StartPos this.EndPos
        | _ -> ()
        base.CheckConsistency()

    override this.EmbedInSymbolTable _ =
        this.CheckConsistency()
        addExpressionToReference this

    override this.RunOrder = None




[<AbstractClass>]
type FplGenericDelegate(name, positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericHasValue(positions, parent)

    do 
        this.FplId <- name

    override this.RunOrder = None


/// Implements the semantics of an FPL equality.
type FplEquality(name, positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericDelegate(name, positions, parent)

    do 
        this.FplId <- $"{LiteralDel}{PrimDelegateEqual}"
        this.TypeId <- LiteralPred

    override this.Name = PrimDelegateEqualL
    override this.ShortName = PrimDelegateEqual

    override this.Clone () =
        let ret = new FplEquality(this.FplId, (this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Copy(other) =
        base.Copy(other)
        this.TypeId <- LiteralPred

    override this.Type signatureType = 
        let head = getFplHead this signatureType
        let propagate = propagateSignatureType signatureType
        let args = signatureSep ", " this.ArgList propagate
        sprintf "%s(%s)" head args

    override this.CheckConsistency (): unit = 
        if this.ArgList.Count <> 2 then 
            this.ErrorOccurred <- emitID013Diagnostics $"Predicate `=` takes 2 arguments, got {this.ArgList.Count}." variableStack.CallerStartPos variableStack.CallerEndPos 
        base.CheckConsistency()
    
    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        addExpressionToReference this

    override this.Run() = 
        debug this Debug.Start
        match this.ErrorOccurred with 
        | Some err ->
            this.SetDefaultValue()
        | _ ->
            if isInQuantor this then 
                this.SetDefaultValue()
            else

                let a = this.ArgList[0]
                let b = this.ArgList[1]
                let aType = a.Type SignatureType.Type
                let bType = b.Type SignatureType.Type
                let aRepr = a.Represent()
                let bRepr = b.Represent()

                let newPred = new FplIntrinsicPred((variableStack.CallerStartPos, variableStack.CallerEndPos), this.Parent.Value)
                match aRepr with
                | LiteralUndef -> 
                    this.ErrorOccurred <- emitID013Diagnostics "Predicate `=` cannot be evaluated because the left argument is undefined." variableStack.CallerStartPos variableStack.CallerEndPos 
                    this.SetDefaultValue()
                | _ -> 
                    match bRepr with
                    | LiteralUndef -> 
                        this.ErrorOccurred <- emitID013Diagnostics "Predicate `=` cannot be evaluated because the right argument is undefined." variableStack.CallerStartPos variableStack.CallerEndPos 
                        this.SetDefaultValue()
                    | _ when aType<>bType -> 
                        newPred.FplId <- LiteralFalse // if the compared arguments have different types, then unequal
                        this.SetValue newPred
                    | _ when aType = "tpl" && bType = "tpl" && aRepr = PrimUndetermined && bRepr = PrimUndetermined -> 
                        this.SetDefaultValue()
                    | _ -> 
                        match aRepr with
                        | PrimUndetermined -> 
                            this.ErrorOccurred <- emitID013Diagnostics "Predicate `=` cannot be evaluated because the left argument is undetermined." variableStack.CallerStartPos variableStack.CallerEndPos 
                            this.SetDefaultValue()
                        | _ -> 
                            match bRepr with
                            | PrimUndetermined -> 
                                this.ErrorOccurred <- emitID013Diagnostics "Predicate `=` cannot be evaluated because the right argument is undetermined." variableStack.CallerStartPos variableStack.CallerEndPos 
                                this.SetDefaultValue()
                            | _ -> 
                                newPred.FplId <- $"{(aRepr = bRepr)}".ToLower()
                                this.SetValue newPred
        debug this Debug.Stop

/// Implements the semantics of an FPL decrement delegate.
type FplDecrement(name, positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericDelegate(name, positions, parent)

    do 
        this.TypeId <- PrimDigits

    override this.Name = PrimDelegateDecrementL
    override this.ShortName = PrimDelegateDecrement

    override this.Clone () =
        let ret = new FplDecrement(this.FplId, (this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Copy(other) =
        base.Copy(other)

    override this.Type signatureType = 
        let head = getFplHead this signatureType
        match signatureType with
        | SignatureType.Type -> head
        | _ ->
            let propagate = propagateSignatureType signatureType
            let args = signatureSep ", " this.ArgList propagate
            sprintf "%s(%s)" head args


    override this.CheckConsistency() =
        if this.ArgList.Count <> 1 then 
            this.ErrorOccurred <- emitID013Diagnostics $"Decrement takes 1 arguments, got {this.ArgList.Count}." this.StartPos this.EndPos
        else
            let arg = this.ArgList[0]
            let argType = arg.Type SignatureType.Type 
            if argType <> PrimDigits then 
                this.ErrorOccurred <- emitID013Diagnostics $"Decrement's argument requires type `{PrimDigits}`, got `{argType}`." arg.StartPos arg.EndPos
        base.CheckConsistency()
    
    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        addExpressionToReference this

    override this.Run() = 
        debug this Debug.Start
        match this.ErrorOccurred with
        | Some err ->
            this.SetDefaultValue()
        | _ ->
            let newValue = FplExtensionObj((this.StartPos, this.EndPos), this.Parent.Value)
            newValue.TypeId <- PrimDigits
            let argPre = this.ArgList[0]
            argPre.Run()
            let numericValue = 
                match argPre with
                | :? FplGenericVariable -> 
                    argPre.Represent()
                | :? FplReference when argPre.RefersTo.IsSome ->
                    match argPre.RefersTo.Value with
                    | :? FplGenericVariable as argPreVar -> 
                        argPreVar.Represent()
                    | _ -> argPre.FplId
                | _ -> argPre.FplId

            let mutable n = 0
            System.Int32.TryParse(numericValue, &n) |> ignore
            let n' = n - 1
            if n' < 0 then 
                // TODO issue diagnostics overflow Decrement
                this.SetDefaultValue()
            else
                newValue.FplId <- string n'
                this.SetValue newValue
        debug this Debug.Stop

/// Implements the semantics of the FPL is operator.
type FplIsOperator(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericPredicate(positions, parent)

    do 
        this.FplId <- LiteralIs

    override this.Name = PrimIsOperator
    override this.ShortName = LiteralIs

    override this.Clone () =
        let ret = new FplIsOperator((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = 
        let head = getFplHead this signatureType
        let args = signatureSep ", " this.ArgList signatureType
        sprintf "%s(%s)" head args
        
    override this.Run() = 
        debug this Debug.Start
        let operand = this.ArgList[0]
        let typeOfOperand = this.ArgList[1]
        // FPL truth-table
        match operand with 
        | :? FplReference as op ->
            match mpwa [operand] [typeOfOperand] with
            | Some errMsg -> 
                let newValue =  new FplIntrinsicPred((this.StartPos, this.EndPos), this)
                newValue.FplId <- LiteralFalse
                this.SetValue newValue
            | None -> 
                let newValue =  new FplIntrinsicPred((this.StartPos, this.EndPos), this)
                newValue.FplId <- LiteralTrue
                this.SetValue newValue
        | _ -> 
            let newValue =  new FplIntrinsicPred((this.StartPos, this.EndPos), this)
            newValue.FplId <- LiteralFalse
            this.SetValue newValue
        
        debug this Debug.Stop

    override this.EmbedInSymbolTable _ = addExpressionToParentArgList this


type FplCaseSingle(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericStmt(positions, parent)
    do 
        this.FplId <- PrimCaseSingle

    override this.Name = PrimCaseSingleL

    override this.Clone () =
        let ret = new FplCaseSingle((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = 
        getFplHead this signatureType

    member this.GetCondition() = this.ArgList[0]
    member this.StmtsAfterCondition() = this.ArgList |> Seq.tail

    override this.CheckConsistency() = 
        base.CheckConsistency()
        checkArgPred this (this.GetCondition())

    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        addExpressionToParentArgList this

    override this.Run() = 
        debug this Debug.Start
        this.StmtsAfterCondition()
        |> Seq.iter (fun stmt -> stmt.Run())
        debug this Debug.Stop

type FplCaseElse(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericStmt(positions, parent)
    do 
        this.FplId <- PrimCaseElse

    override this.Name = PrimCaseElseL

    override this.Clone () =
        let ret = new FplCaseElse((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = 
        getFplHead this signatureType

    override this.Run() = 
        debug this Debug.Start
        this.ArgList 
        |> Seq.iter (fun stmt -> stmt.Run())
        debug this Debug.Stop

type FplCases(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericStmt(positions, parent)
    let _reachableCases = new HashSet<string>()
    do 
        this.FplId <- LiteralCases

    override this.Name = PrimCasesL

    override this.Clone () =
        let ret = new FplCases((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = 
        getFplHead this signatureType

    member this.GetConditionResultList() = 
        this.ArgList
        |> Seq.choose (fun item ->
            match item with
            | :? FplCaseSingle as condRes -> Some condRes
            | _ -> None)
        |> Seq.toList

    member this.GetElseStmt() = this.ArgList |> Seq.last

    member private this.CheckAllCasesForBeingReachable() =
        _reachableCases.Clear()
        this.GetConditionResultList()
        |> Seq.map (fun conditionResultPair -> conditionResultPair.GetCondition())
        |> Seq.iter (fun condition -> 
            let conditionSignature = condition.Type SignatureType.Name
            if _reachableCases.Add(conditionSignature) then 
                () // signature added
            else
                // signature was already added
                this.ErrorOccurred <- emitSIG14diagnostics condition.StartPos condition.EndPos
                
        )

    override this.CheckConsistency() = 
        base.CheckConsistency()
        this.CheckAllCasesForBeingReachable()

    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        addExpressionToParentArgList this

    override this.Run() = 
        debug this Debug.Start
        let resultLst = this.GetConditionResultList()
        let elseStmt = this.GetElseStmt()
        let firstCaseWithTrueConditionOpt = 
            resultLst
            |> Seq.tryFind(fun caseSingle -> 
                let condition = caseSingle.GetCondition()
                condition.Run()
                condition.Represent() = LiteralTrue
            )
        match firstCaseWithTrueConditionOpt with
        | Some firstCaseWithTrueCondition -> 
            firstCaseWithTrueCondition.Run()
        | None -> 
            elseStmt.Run()
        debug this Debug.Stop

type FplForEnumeratorType = 
    | ArrayElements
    | Predicative
    | Error

type FplForInStmt(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericStmt(positions, parent)
    do 
        this.FplId <- LiteralFor

    override this.Name = PrimForInStmtL

    override this.Clone () =
        let ret = new FplForInStmt((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = 
        getFplHead this signatureType

    member this.Entity =
        if this.ArgList.Count > 0 then 
            this.ArgList[0].RefersTo
        else 
            None

    member this.Domain =
        if this.ArgList.Count > 1 then 
            this.ArgList[1].RefersTo 
        else 
            None

    member this.Body =
        // the body of the for statement starts after the entity and after the domain
        if this.ArgList.Count > 2 then 
            this.ArgList |> Seq.tail |> Seq.tail |> Seq.toList
        else
            []

    member this.GetEnumerator() =
        match this.Domain with
        | Some (:? FplVariableArray as domain) ->
            (FplForEnumeratorType.ArrayElements, domain.ValueList |> Seq.toList)
        | Some domain ->
            this.ErrorOccurred <- emitST005diagnostics (domain.Type SignatureType.Name) domain.Name this.ArgList[1].StartPos this.ArgList[1].EndPos
            (FplForEnumeratorType.Error, [])
        | _ ->
            this.ErrorOccurred <- emitST005diagnostics "missing" PrimNone this.StartPos this.StartPos
            (FplForEnumeratorType.Error, [])
            
    override this.Run() = 
        debug this Debug.Start
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
        debug this Debug.Stop

type FplForInStmtEntity(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericStmt(positions, parent)
    do 
        this.FplId <- PrimForInStmtEntity

    override this.Name = PrimForInStmtEntityL

    override this.Clone () =
        let ret = new FplForInStmtEntity((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = 
        let entityOpt = referencedNodeOpt this
        match entityOpt with 
        | Some entity -> entity.Type signatureType
        | _ -> getFplHead this signatureType

    override this.EmbedInSymbolTable _ = tryAddToParentForInStmt this

    override this.Run() = 
        // TODO implement run
        debug this Debug.Start
        debug this Debug.Stop

type FplForInStmtDomain(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericStmt(positions, parent)
    do 
        this.FplId <- PrimForInStmtDomain

    override this.Name = PrimForInStmtDomainL

    override this.Clone () =
        let ret = new FplForInStmtDomain((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = 
        let domainOpt = referencedNodeOpt this
        match domainOpt with 
        | Some domain -> domain.Type signatureType
        | _ -> getFplHead this signatureType
    override this.EmbedInSymbolTable _ = tryAddToParentForInStmt this

    override this.Run() = 
        // TODO implement run
        debug this Debug.Start
        debug this Debug.Stop

/// Implements the assignment statement in FPL.
type FplAssignment(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericStmt(positions, parent)

    do
        this.FplId <- PrimAssignment
        this.TypeId <- LiteralUndef

    override this.Name = PrimAssignmentL

    override this.Clone () =
        let ret = new FplAssignment((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = 
        getFplHead this signatureType

    member private this.GetAssignmentArg no =
        if this.ArgList.Count > 1 then 
            let candidate = this.ArgList[no]
            match candidate with 
            | :? FplReference as ref ->
                match ref.DottedChild with 
                | Some dc -> dc.RefersTo
                | None when ref.RefersTo.IsSome -> ref.RefersTo
                | _ -> Some candidate
            | _ ->
                Some candidate
        else
            None

    member this.Assignee:FplGenericNode option = this.GetAssignmentArg 0

    member this.AssignedValue = 
        let assignedValueOpt = this.GetAssignmentArg 1
        match assignedValueOpt with 
        | Some (:? FplVariableArray as targetArray) when this.ArgList[1].ArgType = ArgType.Brackets ->
            let targetCoords = representationSep "|" (this.ArgList[1].ArgList) 
            let valueAtTargetCoordinates = targetArray.GetValueByCoordinates targetCoords
            Some valueAtTargetCoordinates
        | Some _ -> assignedValueOpt
        | None -> Some (new FplIntrinsicUndef((this.StartPos, this.EndPos), this))

    override this.CheckConsistency () = 
        base.CheckConsistency()
        let checkTypes (assignee:FplGenericNode) (assignedValue:FplGenericNode) =
            let nameAssignee = assignee.Type SignatureType.Name
            let nameAssignedValue = assignedValue.Type SignatureType.Name
            if nameAssignee = nameAssignedValue then
                this.ErrorOccurred <- emitLG005Diagnostics nameAssignedValue assignedValue.StartPos assignedValue.EndPos
            else
                // assignee is to be treated as parameter, the assignedValue as argument
                match mpwa [assignedValue] [assignee] with
                | Some errMsg ->
                    this.ErrorOccurred <- emitSIG05Diagnostics errMsg this.ArgList[1].StartPos this.ArgList[1].EndPos
                | _ -> ()
                
        let checkErrorOccuredInReference (fv:FplGenericNode) = 
            match fv with
            | :? FplReference as ref -> 
                this.ErrorOccurred <- ref.ErrorOccurred 
            | _ -> ()

        match this.ArgList[0], this.Assignee with
        | :? FplReference as ref, Some assignee when ref.ArgType = ArgType.Parentheses ->
            this.ErrorOccurred <- emitSIG07iagnostic (ref.Type SignatureType.Name) "an expression" assignee.Name (this.ArgList[0].StartPos) (this.ArgList[0].EndPos)
        | :? FplReference as ref, Some (:? FplGenericIsValue as assignee) ->
            this.ErrorOccurred <- emitSIG07iagnostic (ref.Type SignatureType.Name) "a value" assignee.Name (this.ArgList[0].StartPos) (this.ArgList[0].EndPos)
        | :? FplReference as ref, Some (:? FplReference as assignee) when assignee.RefersTo.IsNone ->
            this.ErrorOccurred <- emitSIG07iagnostic (ref.Type SignatureType.Name) "undefined" assignee.Name (this.ArgList[0].StartPos) (this.ArgList[0].EndPos)
        | _ -> ()

        let nameAssignee = this.ArgList[0].Type SignatureType.Name
        let nameAssignedValue = this.ArgList[1].Type SignatureType.Name
        if nameAssignee = nameAssignedValue then
            // something has been assigned to itself
            this.ErrorOccurred <- emitLG005Diagnostics nameAssignedValue this.ArgList[1].StartPos this.ArgList[1].EndPos

        // remember proceeding errors of references used in the assignment (if any)
        checkErrorOccuredInReference this.ArgList[0]
        checkErrorOccuredInReference this.ArgList[1]
        match this.ErrorOccurred, this.Assignee, this.AssignedValue with
        | None, Some (:? FplVariable as assignee), Some (assignedValue:FplGenericNode) when assignedValue.Name = PrimClassL ->
            assignee.IsInitialized <- true
            checkTypes assignee assignedValue
        | None, Some (:? FplVariable as assignee), Some (assignedValue:FplGenericNode) when (assignedValue.Name = PrimFunctionalTermL || assignedValue.Name = PrimMandatoryFunctionalTermL) && isCallByValue this.ArgList[1] ->
            let mapOpt = getMapping assignedValue
            match mapOpt with 
            | Some map -> checkTypes this.ArgList[0] map
            | _ -> checkTypes assignee assignedValue
        | None, Some (:? FplVariable as assignee), Some _ -> 
            checkTypes assignee this.ArgList[1] 
        | None, Some (:? FplVariableArray as assignee), Some assignedValue ->
           checkTypes this.ArgList[0] this.ArgList[1] 
        | None, Some (:? FplSelf as assignee), _ ->
            match assignee.RefersTo with 
            | Some ref -> 
                this.ErrorOccurred <- emitSIG07iagnostic (assignee.Type SignatureType.Name) (getEnglishName ref.Name false) assignee.Name (this.ArgList[0].StartPos) (this.ArgList[0].EndPos)
            | None ->
                this.ErrorOccurred <- emitSIG07iagnostic (assignee.Type SignatureType.Name) "the type of self could not be determined" assignee.Name (this.ArgList[0].StartPos) (this.ArgList[0].EndPos)
        | None, Some (:? FplParent as assignee), _ ->
            match assignee.RefersTo with 
            | Some ref -> 
                this.ErrorOccurred <- emitSIG07iagnostic (assignee.Type SignatureType.Name) (getEnglishName ref.Name false) assignee.Name (this.ArgList[0].StartPos) (this.ArgList[0].EndPos)
            | None ->
                this.ErrorOccurred <- emitSIG07iagnostic (assignee.Type SignatureType.Name) "the type of parent could not be determined" assignee.Name (this.ArgList[0].StartPos) (this.ArgList[0].EndPos)
        | None, Some (assignee), Some assignedValue ->
            this.ErrorOccurred <- emitSIG07iagnostic (assignee.Type SignatureType.Name) $"type `{assignee.Type SignatureType.Type}`" assignee.Name (this.ArgList[0].StartPos) (this.ArgList[0].EndPos)
        | _ -> ()

    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        addExpressionToParentArgList this

    member private this.SetAssignee (fv:FplGenericNode) = 
        match this.Assignee with
        | Some (:? FplVariable as assignee) ->
            assignee.SetValue fv
        | Some (:? FplVariableArray as assignee) ->
            let coordinatesKey = representationSep "|" (this.ArgList[0].ArgList) 
            assignee.AssignValueToCoordinates coordinatesKey fv // set value of array
        | _ -> ()

    override this.Run() =
        debug this Debug.Start

        match this.ErrorOccurred, this.ArgList[1], this.AssignedValue with 
        | Some _, _, _ ->
            () // skip assignment, if any proceeding errors occured
        | None, (:? FplGenericHasValue as ref), Some (:? FplVariableArray as assignedValue) ->
            this.SetAssignee assignedValue
        | None, (:? FplGenericHasValue as ref), Some (:? FplGenericIsValue as assignedValue) ->
            this.SetAssignee assignedValue
        | None, (:? FplGenericHasValue as ref), _ ->
            ref.Run()
            this.SetAssignee (ref.Value.Value)
        | None, (:? FplGenericIsValue as ref), _ ->
            this.SetAssignee ref
        | _ -> ()

        debug this Debug.Stop

/// A string representation of an FplValue
let toString (fplValue:FplGenericNode) = $"{fplValue.ShortName} {fplValue.Type(SignatureType.Name)}"


type SymbolTable(parsedAsts: ParsedAstList, debug: bool, offlineMode: bool) =
    let _parsedAsts = parsedAsts
    let mutable _mainTheory = ""
    let _evalLog = List<string>()
    let _root = new FplRoot()
    let _debug = debug
    let _offlineMode = offlineMode

    /// Returns the current OfflineMode, with which the SymbolTable was created. 
    /// OfflineMode=True should not be used in production. If true, the unit tests will try to 
    /// get a local copy of Fpl libraries instead of trying to download them from the Internet.
    member this.OfflineMode
        with get () = _offlineMode

    /// Returns the current main theory.
    member this.MainTheory
        with get () = _mainTheory
        and set (value) = _mainTheory <- value

    /// Returns the evaluation root node of the symbol table.
    member this.Root = _root

    /// Returns the list of parsed asts
    member this.ParsedAsts = _parsedAsts

    /// Returns the string representation of all asts .
    member this.AstsToString =
        let res =
            _parsedAsts
            |> Seq.map (fun pa -> pa.Parsing.Ast.ToString())
            |> String.concat Environment.NewLine

        res

    /// If there is a valid topological sorting, order the list descending by this ordering.
    member this.OrderAsts() =
        _parsedAsts.Sort(
            Comparer<ParsedAst>.Create(fun b a -> compare a.Sorting.TopologicalSorting b.Sorting.TopologicalSorting)
        )

    /// Serializes the symbol table as json
    member this.ToJson() =
        let sb = StringBuilder()
        let mutable currentPath = ""

        let rec createJson (root: FplGenericNode) (sb: StringBuilder) level isLast preventInfinite =
            match root.FilePath with
            | Some path -> currentPath <- path
            | _ -> ()

            let indent, indentMinusOne =
                if _debug then
                    String(' ', level), String(' ', level - 1)
                else
                    String.Empty, String.Empty

            sb.AppendLine(indentMinusOne + "{") |> ignore
            let name = $"{root.Type(SignatureType.Name)}".Replace(@"\", @"\\")
            let fplTypeName = $"{root.Type(SignatureType.Type)}".Replace(@"\", @"\\")
            let mutable fplValueRepr = $"{root.Represent()}".Replace("\\", "\\\\")   // escape backslashes first
                                                            .Replace("\"", "\\\"")   // then escape double quotes

            if name = this.MainTheory then
                sb.AppendLine($"{indent}\"Name\": \"(Main) {name}\",") |> ignore
            else
                sb.AppendLine($"{indent}\"Name\": \"{name}\",") |> ignore

            sb.AppendLine($"{indent}\"Type\": \"{root.ShortName}\",") |> ignore
            sb.AppendLine($"{indent}\"FplValueType\": \"{fplTypeName}\",") |> ignore
            sb.AppendLine($"{indent}\"FplValueRepr\": \"{fplValueRepr}\",") |> ignore
            sb.AppendLine($"{indent}\"Line\": \"{root.StartPos.Line}\",") |> ignore
            sb.AppendLine($"{indent}\"Column\": \"{root.StartPos.Column}\",") |> ignore
            sb.AppendLine($"{indent}\"FilePath\": \"{currentPath}\",") |> ignore

            if preventInfinite then
                sb.AppendLine($"{indent}\"Scope\": [],") |> ignore
                sb.AppendLine($"{indent}\"ArgList\": [],") |> ignore
                sb.AppendLine($"{indent}\"ValueList\": []") |> ignore
            else
                sb.AppendLine($"{indent}\"Scope\": [") |> ignore
                let mutable counterScope = 0
                root.Scope
                |> Seq.iter (fun child ->
                    counterScope <- counterScope + 1
                    createJson
                        child.Value
                        sb
                        (level + 1)
                        (counterScope = root.Scope.Count)
                        (root.FplId = LiteralSelf || root.FplId = LiteralParent))
                sb.AppendLine($"{indent}],") |> ignore

                sb.AppendLine($"{indent}\"ArgList\": [") |> ignore
                let mutable argList = 0
                root.ArgList
                |> Seq.iter (fun child ->
                    argList <- argList + 1
                    createJson child sb (level + 1) (argList = root.ArgList.Count) false)
                sb.AppendLine($"{indent}],") |> ignore

                sb.AppendLine($"{indent}\"ValueList\": [") |> ignore
                let mutable valueList = 0
                match root with 
                | :? FplGenericHasValue as rootWithValue ->
                    match rootWithValue.Value with
                    | Some ref -> createJson ref sb (level + 1) true false
                    | None -> ()
                | _ -> ()
                sb.AppendLine($"{indent}]") |> ignore

            if isLast then
                sb.AppendLine(indentMinusOne + "}") |> ignore
            else
                sb.AppendLine(indentMinusOne + "},") |> ignore

        createJson this.Root sb 1 false false
        let res = sb.ToString().TrimEnd()

        if res.EndsWith(',') then
            res.Substring(0, res.Length - 1)
        else
            res

    /// Returns the uses dependencies of this symbol table needed e.g. for debugging purposes in the FPL language server.
    member this.UsesDependencies() =
        let sb = StringBuilder()
        sb.AppendLine() |> ignore
        sb.AppendLine("SymbolTable: ") |> ignore

        this.Root.Scope
        |> Seq.map (fun theory -> $"{theory.Value.Type(SignatureType.Mixed)} ({theory.Value.Scope.Count})")
        |> String.concat Environment.NewLine
        |> sb.AppendLine
        |> ignore

        sb.AppendLine("ParsedAsts: ") |> ignore

        this.ParsedAsts
        |> Seq.map (fun pa ->
            $"[{pa.Id}, {pa.Sorting.TopologicalSorting}, {pa.Sorting.ReferencedAsts}, {pa.Sorting.ReferencingAsts}]")
        |> String.concat Environment.NewLine
        |> sb.AppendLine
        |> ignore

        sb.ToString()

    /// Creates trace statistics needed e.g. for debugging purposes in the FPL language server.
    member this.TraceStatistics =
        let sb = StringBuilder()

        this.ParsedAsts
        |> Seq.iter (fun pa ->
            let paDiagnostics = ad.GetStreamDiagnostics(pa.Parsing.Uri)

            let statsDiags =
                paDiagnostics.Values
                |> Seq.groupBy (fun d -> $"{d.Emitter}({d.Code.Code})")
                |> Seq.map (fun (groupId, group) -> $"{groupId}:{Seq.length group}")
                |> String.concat ", "

            sb.AppendLine $"{pa.Id}(chksm {pa.Parsing.Checksum}): #total diags {paDiagnostics.Count}, {statsDiags}"
            |> ignore)

        sb.ToString()


/// Looks for all declared building blocks with a specific name.
let findCandidatesByName (st: SymbolTable) (name: string) withClassConstructors withCorollariesOrProofs =
    let pm = List<FplGenericNode>()

    let rec flattenCorollariesAndProofs (tls:FplGenericNode) =
        tls.Scope.Values
        |> Seq.iter (fun fv -> 
            match fv with
            | :? FplProof -> pm.Add(fv)
            | :? FplCorollary -> 
                pm.Add(fv)
                flattenCorollariesAndProofs fv
            | _ -> ()
        )
    let nameWithoutProofOrCorRef = 
        if withCorollariesOrProofs && name.Contains("$") then 
            let parts = name.Split('$')
            parts.[0] 
        else
            name
    let nameWithProofOrCorRef = 
        if withCorollariesOrProofs && not (name.Contains("$")) then 
            $"{name}$"
        else
            name

    if isUpper name then 
        st.Root.Scope // iterate all theories
        |> Seq.iter (fun theory ->
            theory.Value.Scope
            // filter only blocks starting with the same FplId as the reference
            |> Seq.map (fun kvp -> kvp.Value)
            |> Seq.filter (fun fv -> 
                fv.FplId = name 
                || fv.FplId = nameWithoutProofOrCorRef 
                || $"{fv.FplId}$".StartsWith nameWithProofOrCorRef)
            |> Seq.iter (fun (block: FplGenericNode) ->
                pm.Add(block)

                if withClassConstructors && block.IsClass() then
                    block.Scope
                    |> Seq.map (fun kvp -> kvp.Value)
                    |> Seq.filter (fun (fv: FplGenericNode) -> (fv.Name = LiteralCtorL || fv.Name = PrimDefaultConstructor))
                    |> Seq.iter (fun (fv: FplGenericNode) -> pm.Add(fv))

                if withCorollariesOrProofs && (block :? FplGenericTheoremLikeStmt) then 
                    flattenCorollariesAndProofs block
            )
        )
        |> ignore

    pm |> Seq.toList

/// Looks for all declared properties or constructors (if any) that equal 
/// the specific name within the building block, whose syntax tree the FplValue `fv` is part of.
let findCandidatesByNameInBlock (fv: FplGenericNode) (name: string) =
    let rec findDefinition (fv1: FplGenericNode) =
        if isTheory fv1 then
            ScopeSearchResult.NotFound
        else
            match fv1 with
            | :? FplPredicate 
            | :? FplClass
            | :? FplFunctionalTerm -> ScopeSearchResult.Found(fv1)
            | _ ->
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

let findCandidateOfExtensionMapping (fv: FplGenericNode) (name: string) =
    match fv with 
    | :? FplMapping -> 
        match fv.Parent with 
        | Some (:? FplExtension as ext) when ext.FplId = name -> [fv.Parent.Value]
        | _ -> []
    | _ -> []

let findCandidatesByNameInDotted (fv: FplGenericNode) (name: string) =
    let rec findQualifiedEntity (fv1: FplGenericNode) =
        match fv1 with
        | :? FplReference as ref when ref.DottedChild.IsSome -> 
            ScopeSearchResult.Found(ref.DottedChild.Value)
        | :? FplReference -> 
            match fv1.Parent with
            | Some parent -> findQualifiedEntity parent
            | None -> ScopeSearchResult.NotFound
        | _ -> ScopeSearchResult.NotFound

    match findQualifiedEntity fv with
    | ScopeSearchResult.Found candidate ->
        match candidate with
        | :? FplVariable as var ->
            // prefer variable value over its referred type node
            Option.orElse var.Value var.RefersTo |> Option.toList
        | _ -> []
    | _ -> []

let rec getParentExtension (leaf: FplGenericNode) =
    match leaf with
    | :? FplExtension ->
        Some leaf
    | _ -> 
        match leaf.Parent with
        | Some parent -> getParentExtension parent 
        | _ -> None

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

