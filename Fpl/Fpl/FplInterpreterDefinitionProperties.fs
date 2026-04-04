/// This module contains all symbol table nodes used by the FplInterpreter
/// to model properties of definitions

(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)
module FplInterpreterDefinitionProperties
open FParsec
open FplPrimitives
open FplGrammarTypes
open FplInterpreterDiagnosticsEmitter
open FplInterpreterBasicTypes
open FplInterpreter.Globals.Debug
open FplInterpreter.Globals.HelpersBasic
open FplInterpreter.Globals.Heap
open FplInterpreterChecks
open FplInterpreterVariables
open FplInterpreterDefinitions
open FplInterpreterPredicativeBlocks

type FplMandatoryPredicate(positions: Positions, parent: FplGenericNode) =
    inherit FplGenericPredicateWithExpression(positions, parent)
    let mutable _isReady = false
    let mutable _callCounter = 0

    override this.Name = PrimMandatoryPredicateL
    override this.ShortName = PrimMandatoryPredicate

    override this.Clone () =
        let ret = new FplMandatoryPredicate((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret


    interface IReady with
        member _.IsReady = _isReady

    interface ICanBeCalledRecusively with
        member _.CallCounter = _callCounter

    override this.IsBlock () = true

    override this.Type signatureType = 
        let head = getFplHead this signatureType

        let paramT = getParamTuple this signatureType
        sprintf "%s(%s)" head paramT

    override this.CheckConsistency () = 
        if not this.IsIntrinsic then // if not intrinsic, check variable usage
            checkVAR04Diagnostics this

    override this.EmbedInSymbolTable _ = 
        base.CheckConsistency()
        tryAddSubBlockToFplBlock this
    
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
        debug this Debug.Stop

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

    override this.Clone () =
        let ret = new FplMandatoryFunctionalTerm((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.IsBlock () = true

    override this.Type signatureType = 
        let head = getFplHead this signatureType
        let propagate = propagateSignatureType signatureType

        match getMapping this with
        | Some map ->
            let paramT = getParamTuple this signatureType
            sprintf "%s(%s) -> %s" head paramT (map.Type(propagate))
        | _ -> ""

    override this.Represent() = // done
        if _callCounter > maxRecursion then
            this.ErrorOccurred <- emitLG002diagnostic (this.Type(SignatureType.Name)) _callCounter this.StartPos this.EndPos
            PrimUndetermined
        else
            _callCounter <- _callCounter + 1
            let result = getFunctionalTermRepresent this
            _callCounter <- _callCounter - 1
            result

    override this.CheckConsistency () =
        if not this.IsIntrinsic then // if not intrinsic, check variable usage
            checkVAR04Diagnostics this
        checkSIG11Diagnostics this
        base.CheckConsistency()

    override this.EmbedInSymbolTable _ =
        this.CheckConsistency()
        // set all signature variables of this block to bound ones
        this.GetVariables()
        |> List.map (fun var -> var :?> FplGenericVariable)
        |> List.filter (fun var -> var.IsSignatureVariable)
        |> List.iter (fun var -> var.SetIsBound())
        tryAddSubBlockToFplBlock this

    override this.RunOrder = None

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
            _callCounter <- _callCounter - 1
            _isReady <- this.Arity = 0 
        debug this Debug.Stop


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
