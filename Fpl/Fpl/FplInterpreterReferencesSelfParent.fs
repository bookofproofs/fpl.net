module FplInterpreterReferencesSelfParent
open FplPrimitives
open FplGrammarTypes
open ErrDiagnostics
open FplInterpreterDiagnosticsEmitter
open FplInterpreterBasicTypes
open FplInterpreter.Globals.Helpers
open FplInterpreterReferences
open FplInterpreterDefinitions
open FplInterpreterFplTypeMatching

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

