/// This module contains all classes used by the FplInterpreter
/// to model predicative nodes in the symbol table, like axioms, and theorem-like statments

(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)
module FplInterpreterPredicativeBlocks
open FParsec
open FplPrimitives
open FplGrammarTypes
open FplInterpreterDiagnosticsEmitter
open FplInterpreterBasicTypes
open FplInterpreterUtils
open FplInterpreterGlobals
open FplInterpreterChecks
open FplInterpreterSTEmbedding
open FplInterpreterVariables
open FplInterpreterDefinitions

let runArgumentsOfGenericPredicateWithExpression (fv:FplGenericHasValue) = 
    fv.ArgList
    |> Seq.iter (fun fv1 -> 
        fv1.Run()
        match fv1 with 
        | :? FplGenericHasValue as fv1WithValue -> 
            fv.SetValueOf fv1WithValue
        | _ ->
            // warning stmt inside predicate might cause side-effects
            fv.ErrorOccurred <- emitLG004diagnostic fv.Name fv1.StartPos fv1.EndPos
            
    )

[<AbstractClass>]
type FplGenericPredicateWithExpression(positions: Positions, parent: FplGenericNode) =
    inherit FplGenericPredicate(positions, parent)
    let mutable _signStartPos = Position("", 0L, 0L, 0L)
    let mutable _signEndPos = Position("", 0L, 0L, 0L)

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

    override this.Type signatureType = getFplHead this signatureType

    override this.CheckConsistency () = 
        base.CheckConsistency()
        if not this.IsIntrinsic then
            checkVAR04Diagnostics this
        checkPredicateExpressionReturnsPredicate this


type FplAxiom(positions: Positions, parent: FplGenericNode, runOrder) =
    inherit FplGenericPredicateWithExpression(positions, parent)
    let _runOrder = runOrder
    let mutable _isReady = false

    interface IReady with
        member _.IsReady = _isReady

    override this.Name = LiteralAxL
    override this.ShortName = LiteralAx

    override this.Clone () =
        let ret = new FplAxiom((this.StartPos, this.EndPos), this.Parent.Value, _runOrder)
        this.AssignParts(ret)
        ret

    override this.IsFplBlock () = true
    override this.IsBlock () = true

    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        tryAddToParentUsingFplId this

    override this.Run() = 
        debug this Debug.Start
        if not _isReady then
            runArgumentsOfGenericPredicateWithExpression this 
            _isReady <- true
            checkLG003Diagnostics this

            // evaluate all corollaries of the axiom
            this.Scope.Values
            |> Seq.filter (fun fv -> fv.Name = LiteralCorL) 
            |> Seq.sortBy (fun block -> block.RunOrder.Value) 
            |> Seq.iter (fun fv -> 
                fv.Run()
            )

        debug this Debug.Stop

    override this.RunOrder = Some _runOrder

[<AbstractClass>]
type FplGenericTheoremLikeStmt(positions: Positions, parent: FplGenericNode, runOrder) =
    inherit FplGenericPredicateWithExpression(positions, parent)
    let _runOrder = runOrder
    let mutable _isReady = false
    let mutable _hasProof = false

    override this.Name = LiteralThmL
    override this.ShortName = LiteralThm

    interface IReady with
        member _.IsReady = _isReady

    interface IHasProof with
        member this.HasProof
            with get (): bool = _hasProof
            and set (value) = _hasProof <- value
    
    override this.IsFplBlock () = true
    override this.IsBlock () = true

    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        tryAddToParentUsingFplId this 

    override this.Run() = 
        debug this Debug.Start
        if not _isReady then
            runArgumentsOfGenericPredicateWithExpression this 
            _isReady <- true
            checkLG003Diagnostics this

            // evaluate all corollaries and proofs of the theorem-like statement
            this.Scope.Values
            |> Seq.filter (fun fv -> fv.Name = LiteralPrfL || fv.Name = LiteralCorL) 
            |> Seq.sortBy (fun block -> block.RunOrder.Value) 
            |> Seq.iter (fun fv -> 
                fv.Run()
            )

        if not _hasProof then 
           this.ErrorOccurred <- emitPR007Diagnostics (this.Type(SignatureType.Name)) this.Name this.SignStartPos this.SignEndPos
        debug this Debug.Stop

    override this.RunOrder = Some _runOrder

type FplTheorem(positions: Positions, parent: FplGenericNode, runOrder) =
    inherit FplGenericTheoremLikeStmt(positions, parent, runOrder)

    override this.Name = LiteralThmL
    override this.ShortName = LiteralThm

    override this.Clone () =
        let ret = new FplTheorem((this.StartPos, this.EndPos), this.Parent.Value, this.RunOrder.Value)
        this.AssignParts(ret)
        ret

type FplLemma(positions: Positions, parent: FplGenericNode, runOrder) =
    inherit FplGenericTheoremLikeStmt(positions, parent, runOrder)

    override this.Name = LiteralLemL
    override this.ShortName = LiteralLem

    override this.Clone () =
        let ret = new FplLemma((this.StartPos, this.EndPos), this.Parent.Value, this.RunOrder.Value)
        this.AssignParts(ret)
        ret

type FplProposition(positions: Positions, parent: FplGenericNode, runOrder) =
    inherit FplGenericTheoremLikeStmt(positions, parent, runOrder)

    override this.Name = LiteralPropL
    override this.ShortName = LiteralProp

    override this.Clone () =
        let ret = new FplProposition((this.StartPos, this.EndPos), this.Parent.Value, this.RunOrder.Value)
        this.AssignParts(ret)
        ret

type FplCorollary(positions: Positions, parent: FplGenericNode, runOrder) =
    inherit FplGenericTheoremLikeStmt(positions, parent, runOrder)

    override this.Name = LiteralCorL
    override this.ShortName = LiteralCor

    override this.Clone () =
        let ret = new FplCorollary((this.StartPos, this.EndPos), this.Parent.Value, this.RunOrder.Value)
        this.AssignParts(ret)
        ret

    override this.EmbedInSymbolTable _ =
        match tryFindAssociatedBlockForCorollary this with
        | ScopeSearchResult.FoundAssociate potentialParent -> 
            // everything is OK, change the parent of the provable from theory to the found parent 
            this.Parent <- Some potentialParent
        | ScopeSearchResult.FoundIncorrectBlock incorrectBlock ->
            this.ErrorOccurred <- emitID005diagnostics this.FplId (qualifiedName incorrectBlock false) this.StartPos this.EndPos
        | ScopeSearchResult.NotFound ->
            this.ErrorOccurred <- emitID006diagnostics this.FplId this.SignStartPos this.SignEndPos
        | _ -> ()
        tryAddToParentUsingFplId this


type FplConjecture(positions: Positions, parent: FplGenericNode, runOrder) =
    inherit FplGenericPredicateWithExpression(positions, parent)
    let _runOrder = runOrder
    let mutable _isReady = false

    interface IReady with
        member _.IsReady = _isReady

    override this.Name = LiteralConjL
    override this.ShortName = LiteralConj

    override this.Clone () =
        let ret = new FplConjecture((this.StartPos, this.EndPos), this.Parent.Value, _runOrder)
        this.AssignParts(ret)
        ret

    override this.IsFplBlock () = true
    override this.IsBlock () = true

    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        tryAddToParentUsingFplId this 

    override this.Run() = 
        debug this Debug.Start
        if not _isReady then
            runArgumentsOfGenericPredicateWithExpression this 
            _isReady <- true
            checkLG003Diagnostics this

            // evaluate all corollaries of the conjecture
            this.Scope.Values
            |> Seq.filter (fun fv -> fv.Name = LiteralCorL) 
            |> Seq.sortBy (fun block -> block.RunOrder.Value) 
            |> Seq.iter (fun fv -> 
                fv.Run()
            )
        debug this Debug.Stop


    override this.RunOrder = Some _runOrder
