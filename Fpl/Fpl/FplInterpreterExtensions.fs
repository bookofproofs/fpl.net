/// This module contains all types used by the FplInterpreter
/// to model / interpret extensions

(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 
*)
module FplInterpreterExtensions
open System
open System.Text.RegularExpressions
open FParsec
open FplPrimitives
open FplGrammarTypes
open FplInterpreterDiagnosticsEmitter
open FplInterpreterBasicTypes
open FplInterpreterChecks
open FplInterpreter.Globals.Debug
open FplInterpreter.Globals.HelpersBasic
open FplInterpreter.Globals.Heap
open FplInterpreter.Globals.HelpersComplex
open FplInterpreterReferences
open FplInterpreterIntrinsicTypes
open FplInterpreterVariables
open FplInterpreterDefinitions
open FplInterpreterFplTypeMatching
open FplInterpreterMapCases



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

    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()    
        addExpressionToReference this

    override this.RunOrder = None


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
            this.ErrorOccurred <- emitLG002diagnostic (this.Type(SignatureType.Name)) _callCounter heap.Helper.CallerStartPos heap.Helper.CallerEndPos
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

let findCandidateOfExtensionMapping (fv: FplGenericNode) (name: string) =
    match fv with 
    | :? FplMapping -> 
        match fv.Parent with 
        | Some (:? FplExtension as ext) when ext.FplId = name -> [fv.Parent.Value]
        | _ -> []
    | _ -> []
