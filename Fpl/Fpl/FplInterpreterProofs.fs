/// This module contains all symbol table nodes used by the FplInterpreter
/// to interpret proofs.

(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)
module FplInterpreterProofs
open System
open FplPrimitives
open FplGrammarTypes
open FplInterpreterDiagnosticsEmitter
open FplInterpreterBasicTypes
open FplInterpreter.Globals.Debug
open FplInterpreter.Globals.HelpersBasic
open FplInterpreterChecks
open FplInterpreter.Globals.HelpersComplex
open FplInterpreter.Globals.Heap
open FplInterpreterIntrinsicTypes
open FplInterpreterCompoundPredicates
open FplInterpreterPredicativeBlocks

[<AbstractClass>]
type FplGenericArgInference(positions: Positions, parent: FplGenericNode) =
    inherit FplGenericPredicate(positions, parent)

    override this.Type signatureType =
        let head = getFplHead this signatureType
        head

    override this.EmbedInSymbolTable _ = addExpressionToParentArgList this 

[<AbstractClass>]
type FplGenericJustificationItem(positions: Positions, parent: FplGenericNode) =
    inherit FplGenericPredicate(positions, parent)

    override this.ShortName = PrimJustification

    override this.Type signatureType =
        let head = getFplHead this signatureType
        head

    override this.EmbedInSymbolTable _ = 
        let thisJustificationItemId = this.Type(SignatureType.Mixed)

        let alreadyAddedIdOpt = 
            this.Parent.Value.ArgList
            |> Seq.map (fun argJi -> argJi.Type(SignatureType.Mixed))
            |> Seq.tryFind (fun otherId -> otherId = thisJustificationItemId)
        match alreadyAddedIdOpt with
        | Some otherId ->
            this.ErrorOccurred <- emitPR004Diagnostics thisJustificationItemId otherId this.StartPos this.EndPos 
        | _ -> ()
        addExpressionToParentArgList this

    member this.ParentJustification = this.Parent.Value :?> FplJustification

    /// Returns a) Some expression inferred by the proceeding JustificationItem in the same proof argument.
    /// b) If there is no proceeding JustificationItem in the same proof argument, but there is a proceeding argument proof,
    ///    the function will return Some expression parsed from that proceeding proof argument. 
    ///    Note that this parsed expression might differ from the InferredExpression of the proceeding proof arguments last JustificationItem.
    /// c) If there is no proceeding JustificationItem and even no proceeding proof argument, the function will return None.
    member this.PreviousExpression = 
        if this.ArgList.Count > 0 then 
            Some this.ArgList[0]
        else
            None

    /// Returns Some expression that could be inferred by this JustificationItem based on its PreviousExpression and its ReferencedJustification.
    /// None will be returned if Such an InferredExpression could not be inferred
    member this.InferredExpression = 
        if this.ArgList.Count > 1 then 
            Some this.ArgList[1]
        else
            None

    override this.Run() = 
        debug this Debug.Start
        match this.RefersTo with 
        | Some (:? FplGenericHasValue as ref) when ref.Value.IsSome ->
            let refType, isRefPred = isArgPred ref
            if isRefPred then 
                // because the ref must proceed "this" in FPL Code, it was already run
                // so we only need to copy its value into this
                this.SetValueOf ref
            else
                // if there is a value but ref is not a predicate, 
                // set the value of "this" to undetermined
                this.SetDefaultValue()
                // and issue diagnostics saying that this requires a predicate
                let refName = ref.Type SignatureType.Name
                this.ErrorOccurred <- emitLG001Diagnostics refType refName this.Name ref.StartPos ref.StartPos
        | Some (:? FplGenericHasValue as ref) when ref.Value.IsNone ->
            // set the value of "this" to undetermined
            ref.SetDefaultValue()
            this.SetValueOf ref
            // TODO issue diagnostics saying that a predicate expression was expected but has No value
        | _ -> 
            this.SetDefaultValue()
        debug this Debug.Stop

and FplJustificationItemByAx(positions: Positions, parent: FplGenericNode) =
    inherit FplGenericJustificationItem(positions, parent)

    override this.Name = PrimJIByAx

    override this.Clone () =
        let ret = new FplJustificationItemByAx((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

and FplJustificationItemByDef(positions: Positions, parent: FplGenericNode) =
    inherit FplGenericJustificationItem(positions, parent)

    override this.Name = PrimJIByDef

    override this.Clone () =
        let ret = new FplJustificationItemByDef((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

and FplJustificationItemByDefVar(positions: Positions, parent: FplGenericNode) =
    inherit FplGenericJustificationItem(positions, parent)

    override this.Name = PrimJIByDefVar

    override this.Clone () =
        let ret = new FplJustificationItemByDefVar((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

and FplJustificationItemByConj(positions: Positions, parent: FplGenericNode) =
    inherit FplGenericJustificationItem(positions, parent)

    override this.Name = PrimJIByConj

    override this.Clone () =
        let ret = new FplJustificationItemByConj((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

and FplJustificationItemByCor(positions: Positions, parent: FplGenericNode) =
    inherit FplGenericJustificationItem(positions, parent)

    override this.Name = PrimJIByCor

    override this.Clone () =
        let ret = new FplJustificationItemByCor((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

and FplJustificationItemByInf(positions: Positions, parent: FplGenericNode) =
    inherit FplGenericJustificationItem(positions, parent)

    override this.Name = PrimJIByInf

    override this.Clone () =
        let ret = new FplJustificationItemByInf((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

and FplJustificationItemByRefArgument(positions: Positions, parent: FplGenericNode) =
    inherit FplGenericJustificationItem(positions, parent)

    override this.Name = PrimJIByRefArgument

    override this.Clone () =
        let ret = new FplJustificationItemByRefArgument((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

and FplJustificationItemByProofArgument(positions: Positions, parent: FplGenericNode) =
    inherit FplGenericJustificationItem(positions, parent)

    override this.Name = PrimJIByProofArgument

    override this.Clone () =
        let ret = new FplJustificationItemByProofArgument((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

and FplJustificationItemByTheoremLikeStmt(positions: Positions, parent: FplGenericNode) =
    inherit FplGenericJustificationItem(positions, parent)

    override this.Name = PrimJIByTheoremLikeStmt

    override this.Clone () =
        let ret = new FplJustificationItemByTheoremLikeStmt((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

and FplJustification(positions: Positions, parent: FplGenericNode) =
    inherit FplGenericPredicate(positions, parent)

    override this.Name = PrimJustificationL
    override this.ShortName = PrimJustification

    override this.Clone () =
        let ret = new FplJustification((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType =
        let head = getFplHead this signatureType
        head

    override this.Run() = 
        // TODO implement Run
        debug this Debug.Start
        let v = new FplIntrinsicPred((this.StartPos, this.EndPos), this)
        this.Value <- Some v
        debug this Debug.Stop


    member this.GetOrderedJustificationItems =
        this.ArgList
            |> Seq.map (fun fv -> fv :?> FplGenericJustificationItem)
            |> Seq.toList

    override this.EmbedInSymbolTable _ = addExpressionToParentArgList this

    member this.ParentArgument = this.Parent.Value :?> FplArgument

and FplArgument(positions: Positions, parent: FplGenericNode, runOrder) =
    inherit FplGenericPredicate(positions, parent)
    let _runOrder = runOrder

    override this.Name = PrimArgL
    override this.ShortName = PrimArg

    override this.Clone () =
        let ret = new FplArgument((this.StartPos, this.EndPos), this.Parent.Value, _runOrder)
        this.AssignParts(ret)
        ret
    
    override this.Type signatureType =
        let head = getFplHead this signatureType
        head

    member this.Justification = 
        if this.ArgList.Count>0 then 
            let justification = this.ArgList[0]
            Some (justification :?> FplJustification)
        else
            None

    member this.ArgumentInference = 
        if this.ArgList.Count>1 then 
            let argInference = this.ArgList[1]
            Some (argInference :?> FplGenericArgInference)
        else
            None

    override this.Run() =
        debug this Debug.Start
        // the argument has two elements, the justification and an argument inference
        let justificationOpt = this.Justification
        let argInferenceOpt = this.ArgumentInference

        match justificationOpt, argInferenceOpt with
        | Some justification, Some argInference -> 
            let orderdListJustifications = justification.GetOrderedJustificationItems
            let mutable allEvaluateToTrue = (orderdListJustifications.Length > 0) // if the proof is empty, it will evaluate into undetermined
            orderdListJustifications
            |> List.iter (fun fv ->
                fv.Run()
                let fvRepr = fv.Represent()
                allEvaluateToTrue <- allEvaluateToTrue && fvRepr = LiteralTrue
            )
            if not allEvaluateToTrue then
                this.ErrorOccurred <- emitPR009Diagnostics this.StartPos this.StartPos
                this.SetDefaultValue()
            else
                let v = new FplIntrinsicPred((this.StartPos, this.StartPos), this)
                v.FplId <- LiteralTrue
                this.SetValue v
        | _ -> 
            this.SetDefaultValue()
        (* TODO: Enhance variableStack by the context in which this argument is being evaluated
            Here are some preliminary considerations: 
            1) The context should include 
                a) the argumentInference of the previous argument (if such exists) - the first argument doesn't have such a predecessor
                b) if the argument has more than justification, variableStack should store a list of applying the previous argumentInference from a) each justification sorted by their RunOrder 
                   so then next justification from the list can be applied to the last result from variableStack. 
                   The idea is that a list of justification could be "unzipped" in the FPL code by writing a sequence
                   of arguments, each having only a single justification from the original list. This unzipped FPL code should be semantically
                   the same as "zipping/hiding" the arguments by listing multiply justifications and only inferring to the last argumentInference.
                c) possibly the structure of the to-be-proven predicate of the original theorem
            2) The evaluation of the argument should then handle the following cases
                a) whether or not the argumentInference of this argument is an FplAssume or FplRevoke 
                b) whether or not the justification is an axiom
                c) whether or not the justification is a rule of inference
                d) whether or not the justification is a by definition
        *)
        debug this Debug.Stop



    override this.EmbedInSymbolTable _ = 
        let (proof:FplProof) = this.ParentProof
        if proof.HasArgument (this.FplId) then 
            let conflict = proof.Scope[this.FplId]
            this.ErrorOccurred <- emitPR003Diagnostics this.FplId conflict.QualifiedStartPos this.StartPos this.EndPos 
        else 
            proof.Scope.Add(this.FplId, this)

    override this.RunOrder = Some _runOrder

    member this.ParentProof = this.Parent.Value :?> FplProof

and FplArgInferenceAssume(positions: Positions, parent: FplGenericNode) =
    inherit FplGenericArgInference(positions, parent)

    override this.Name = PrimArgInfAssume
    override this.ShortName = PrimArgInf

    member this.InferrableExpression =
        let validityReason = 
            let exprOpt = this.ArgList |> Seq.tryLast
            match exprOpt with
            | Some expr -> ValidityReason.IsDerivedAssumed (expr.Type SignatureType.Name)
            | _ -> ValidityReason.Error // fallback if axiom node is empty

        {
            ValidStatement.Node = this
            ValidStatement.ValidityReason = validityReason
        }

    interface IInferrable with
        member this.InferrableExpression
            with get () = this.InferrableExpression

    override this.Clone () =
        let ret = new FplArgInferenceAssume((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Run() = 
        debug this Debug.Start
        if heap.ValidStmtStore.RegisterExpression this then
            let v = new FplIntrinsicPred((this.StartPos, this.EndPos), this)
            v.FplId <- LiteralTrue
            this.SetValue v
        else
            this.SetDefaultValue()
        debug this Debug.Stop

    member this.ParentArgument = this.Parent.Value :?> FplArgument

and FplArgInferenceRevoke(positions: Positions, parent: FplGenericNode) =
    inherit FplGenericArgInference(positions, parent)

    override this.Name = PrimArgInfRevoke
    override this.ShortName = PrimArgInf

    member this.InferrableExpression =
        match heap.ValidStmtStore.LastAssumedArgument with
        | Some assumption -> 
            let assumptionId = assumption.Type SignatureType.Mixed
            // and replace it with its negated version
            let negatedAssumption = new FplNegation((this.StartPos, this.EndPos), this.Parent.Value)
            negatedAssumption.ArgList.Add assumption
            let revokedExpr = negatedAssumption.Type SignatureType.Name
            {
                ValidStatement.Node = this
                ValidStatement.ValidityReason = ValidityReason.IsDerivedRevoke(assumptionId, revokedExpr)
            }
        | _ ->
            {
                ValidStatement.Node = this
                ValidStatement.ValidityReason = ValidityReason.Error
            }


    interface IInferrable with
        member this.InferrableExpression
            with get () = this.InferrableExpression



    override this.Clone () =
        let ret = new FplArgInferenceRevoke((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.CheckConsistency () = 
        let fvAi = this
        let argumentId = fvAi.FplId
        let (arg:FplArgument) = fvAi.ParentArgument
        let proof = arg.ParentProof
        if argumentId = arg.FplId then 
            // revokes its own argument
            this.ErrorOccurred <- emitPR015Diagnostics argumentId this.StartPos this.EndPos
        elif proof.HasArgument argumentId then 
            let refArg = proof.Scope[argumentId] :?> FplArgument
            let aiOpt = refArg.ArgumentInference
            match aiOpt with
            | Some (:? FplArgInferenceAssume as toBeRevoked) -> 
                match heap.ValidStmtStore.LastAssumedArgument with 
                | Some (:? FplArgInferenceAssume as last) when last = toBeRevoked -> 
                    ()
                | Some (:? FplArgInferenceAssume as last) when last <> toBeRevoked -> 
                    let lastArg = last.ParentArgument
                    this.ErrorOccurred <- emitPR016Diagnostics argumentId lastArg.FplId this.StartPos this.EndPos
                | _ ->    
                    // the referenced argument is not an assumption in the proof
                    this.ErrorOccurred <- emitPR015Diagnostics argumentId this.StartPos this.EndPos
            | _ -> 
                // the referenced argument is not an assumption in the proof
                this.ErrorOccurred <- emitPR015Diagnostics argumentId this.StartPos this.EndPos
        else
            this.ErrorOccurred <- emitPR005Diagnostics argumentId this.StartPos this.EndPos

        base.CheckConsistency()

    override this.EmbedInSymbolTable _ =
        this.CheckConsistency()
        addExpressionToParentArgList this 

    override this.Run() = 
        debug this Debug.Start
        match this.ErrorOccurred with
        | Some err -> this.SetDefaultValue()
        | _ ->
            if heap.ValidStmtStore.RegisterExpression this then
                let v = new FplIntrinsicPred((this.StartPos, this.EndPos), this)
                v.FplId <- LiteralTrue
                this.SetValue v
            else
                this.SetDefaultValue()
        debug this Debug.Stop

    member this.ParentArgument = this.Parent.Value :?> FplArgument

and FplArgInferenceTrivial(positions: Positions, parent: FplGenericNode) =
    inherit FplGenericArgInference(positions, parent)

    override this.Name = PrimArgInfTrivial
    override this.ShortName = PrimArgInf

    override this.Clone () =
        let ret = new FplArgInferenceTrivial((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Run() = 
        debug this Debug.Start
        // TODO - check if trivial is possible and spawn FplIntrisincPred true if possible
        this.SetDefaultValue()
        debug this Debug.Stop

    member this.ParentArgument = this.Parent.Value :?> FplArgument

and FplArgInferenceDerived(positions: Positions, parent: FplGenericNode) =
    inherit FplGenericArgInference(positions, parent)

    override this.Name = PrimArgInfDerive
    override this.ShortName = PrimArgInf

    override this.Clone () =
        let ret = new FplArgInferenceDerived((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Run() = 
        // TODO implement run
        debug this Debug.Start
        let v = new FplIntrinsicPred((this.StartPos, this.EndPos), this)
        this.Value <- Some v
        debug this Debug.Stop

    member this.ParentArgument = this.Parent.Value :?> FplArgument


and FplProof(positions: Positions, parent: FplGenericNode, runOrder) =
    inherit FplGenericPredicateWithExpression(positions, parent)
    let _runOrder = runOrder
            
    override this.Name = LiteralPrfL
    override this.ShortName = LiteralPrf

    override this.Clone () =
        let ret = new FplProof((this.StartPos, this.EndPos), this.Parent.Value, _runOrder)
        this.AssignParts(ret)
        ret

    override this.IsFplBlock () = true
    override this.IsBlock () = true
    override this.IsProof () = true

    override this.Type signatureType =
        let head = getFplHead this signatureType
        head

    member this.OrderedArguments =
        this.Scope.Values
        |> Seq.filter (fun fv -> fv.Name = PrimArgL)
        |> Seq.map (fun fv -> fv :?> FplArgument)
        |> Seq.sortBy (fun fv -> fv.RunOrder)
        |> Seq.toList

    member this.HasArgument argumentId = this.Scope.ContainsKey(argumentId)

    override this.Run() = 
        debug this Debug.Start
        // tell the parent theorem-like statement that it has a proof
        let parent = this.Parent.Value 
        match box parent with 
        | :? IHasProof as parentWithProof ->
            parentWithProof.HasProof <- true
        | _ -> ()
        // evaluate the proof by evaluating all arguments according to their order in the FPL code
        let orderedProofArguments = this.OrderedArguments 
        let mutable allEvaluateToTrue = (orderedProofArguments.Length > 0) // if the proof is empty, it will evaluate into undetermined
        this.OrderedArguments
        |> Seq.iter (fun fv -> 
            fv.Run()
            let fvRepr = fv.Represent()
            allEvaluateToTrue <- allEvaluateToTrue && fvRepr = LiteralTrue 
        )
        if not allEvaluateToTrue then
            this.ErrorOccurred <- emitPR009Diagnostics this.StartPos this.StartPos
            this.SetDefaultValue()
        else
            let v = new FplIntrinsicPred((this.SignStartPos, this.SignEndPos), this)
            v.FplId <- LiteralTrue
            this.SetValue v
        debug this Debug.Stop

    /// Tries to find a theorem-like statement for a proof
    /// and returns different cases of ScopeSearchResult, depending on different semantical error situations.
    member private this.TryFindAssociatedBlockForProof (fplValue: FplGenericNode) =
        match fplValue.Parent with
        | Some theory ->

            let flattenedScopes = flattenScopes theory.Parent.Value

            let potentialProvableName = stripLastDollarDigit (fplValue.FplId)

            // The parent node of the proof is the theory. In its scope
            // we should find the theorem we are looking for.
            let buildingBlocksMatchingDollarDigitNameList =
                // the potential block name of the proof is the
                // concatenated type signature of the name of the proof
                // without the last dollar digit
                flattenedScopes |> List.filter (fun fv -> fv.FplId = potentialProvableName)

            let provableBlocklist =
                buildingBlocksMatchingDollarDigitNameList
                |> List.filter (fun fv -> isProvable fv)

            let notProvableBlocklist =
                buildingBlocksMatchingDollarDigitNameList
                |> List.filter (fun fv -> not (isProvable fv ))

            if provableBlocklist.Length > 1 then
                ScopeSearchResult.FoundMultiple(
                    provableBlocklist
                    |> List.map (fun fv -> sprintf "'%s' %s" fv.Name (fv.Type(SignatureType.Mixed)))
                    |> String.concat ", "
                )
            elif provableBlocklist.Length > 0 then
                let potentialTheorem = provableBlocklist.Head
                ScopeSearchResult.FoundAssociate potentialTheorem
            elif notProvableBlocklist.Length > 0 then
                let potentialOther = notProvableBlocklist.Head
                ScopeSearchResult.FoundIncorrectBlock potentialOther
            else
                ScopeSearchResult.NotFound
        | None -> ScopeSearchResult.NotApplicable

    /// Issue PR017 for all "trivial" arguments that are not the last one in the proof 
    member private this.CheckTrivialArgumentsPR017 (trivialArgs:FplArgument list) lastArg =
        trivialArgs
        |> List.filter (fun trivialArg -> not (Object.ReferenceEquals(trivialArg, lastArg)))
        |> List.iter (fun trivialArg ->
            this.ErrorOccurred <- emitPR017Diagnostics trivialArg.StartPos trivialArg.EndPos
        )

    /// Issue PR018 for all "trivial" arguments that have not exactly one justification
    member private this.CheckTrivialArgumentsPR018 (trivialArgs:FplArgument list) =
        trivialArgs
        |> List.filter (fun trivialArg -> trivialArg.Justification.IsSome)
        |> List.map (fun trivialArg -> trivialArg.Justification.Value)
        |> List.filter (fun justification -> justification.GetOrderedJustificationItems.Length <> 1)
        |> List.iter (fun justification ->
            this.ErrorOccurred <- emitPR018Diagnostics justification.StartPos justification.EndPos
        )

    member private this.CheckTrivialArguments() =
        let orderedArgs = this.OrderedArguments
        if orderedArgs.Length > 0 then
            let lastArg = orderedArgs |> List.last
            let trivialArgs = 
                this.OrderedArguments
                |> List.map (fun arg -> arg.ArgumentInference)
                |> List.filter (fun argInf -> argInf.IsSome)
                |> List.map (fun argInf -> argInf.Value)
                |> List.filter (fun argInf -> argInf.Parent.IsSome && argInf.Name = PrimArgInfTrivial)
                |> List.map (fun argInf -> argInf.Parent.Value)
                |> List.map (fun arg -> arg :?> FplArgument)

            this.CheckTrivialArgumentsPR017 trivialArgs lastArg
            this.CheckTrivialArgumentsPR018 trivialArgs

    override this.CheckConsistency () = 
        match this.TryFindAssociatedBlockForProof this with
        | ScopeSearchResult.FoundAssociate potentialParent -> 
            // everything is OK, change the parent of the provable from theory to the found parent 
            this.Parent <- Some potentialParent
        | ScopeSearchResult.FoundIncorrectBlock incorrectBlock ->
            this.ErrorOccurred <- emitID002Diagnostics this.FplId (qualifiedName incorrectBlock false) this.StartPos this.EndPos
        | ScopeSearchResult.NotFound ->
            this.ErrorOccurred <- emitID003diagnostics this.FplId this.SignStartPos this.SignEndPos
        | _ -> ()
        this.CheckTrivialArguments()
        base.CheckConsistency()

    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        tryAddToParentUsingFplId this

    override this.RunOrder = Some _runOrder

let getArgumentInProof (fv1:FplGenericJustificationItem) argName =
    let proof = 
        match fv1 with 
        | :? FplJustificationItemByProofArgument when fv1.RefersTo.IsSome ->
            fv1.RefersTo.Value :?> FplProof
        | _ ->
            let parent = fv1.ParentJustification
            let arg = parent.ParentArgument
            arg.ParentProof
    if proof.HasArgument argName then 
        Some proof.Scope[argName]
    else 
        None
