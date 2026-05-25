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
open System.Collections.Generic
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
open FplInterpreterExpressionMatching
open FplInterpreterRulesOfInferences

type FplJustificationItemByAx(positions: Positions, parent: FplGenericNode) =
    inherit FplGenericJustificationItem(positions, parent)

    override this.Name = PrimJIByAx

    override this.Clone () =
        let ret = new FplJustificationItemByAx((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    member this.ParentJustification = this.Parent.Value :?> FplJustification

    override this.ProceedingExprCandidates
        // identify the expression contained in the axiom
        // referred by this "byax" justification in a proof
        with get (): FplGenericNode list =
            match this.RefersTo with
            | Some ax ->
                if ax.ArgList.Count > 0 then
                    [ax.ArgList |> Seq.last]
                else
                    [FplUndetermined(LiteralPred, (this.StartPos, this.EndPos), this)]
            | None ->
                [FplUndetermined(LiteralPred, (this.StartPos, this.EndPos), this)]

and FplJustificationItemByDef(positions: Positions, parent: FplGenericNode) =
    inherit FplGenericJustificationItem(positions, parent)

    override this.Name = PrimJIByDef

    override this.Clone () =
        let ret = new FplJustificationItemByDef((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    member this.ParentJustification = this.Parent.Value :?> FplJustification

    override this.ProceedingExprCandidates
        // identify the expressions contained in the definition
        with get (): FplGenericNode list =
            match this.RefersTo with
            | Some def ->
                let predicateDefIfAny = extractPredicateDefinitionExpressions def
                let assertions = extractAssertionExpressions def
                let predicativeProperties = extractPredicativePropertiesExpressions def
                let total =
                    predicateDefIfAny @ assertions @ predicativeProperties
                if total.Length > 0 then
                    total
                else
                    [FplUndetermined(LiteralPred, (this.StartPos, this.EndPos), this)]
            | None ->
                [FplUndetermined(LiteralPred, (this.StartPos, this.EndPos), this)]

and FplJustificationItemByDefVar(positions: Positions, parent: FplGenericNode) =
    inherit FplGenericJustificationItem(positions, parent)

    override this.Name = PrimJIByDefVar

    override this.Clone () =
        let ret = new FplJustificationItemByDefVar((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    member this.ParentJustification = this.Parent.Value :?> FplJustification

    override this.ProceedingExprCandidates 
        // identify the expressions contained in the variable definition
        with get (): FplGenericNode list =
            match this.RefersTo with
            | Some var ->
                match var.RefersTo with 
                | Some def ->
                    let predicateDefIfAny = extractPredicateDefinitionExpressions def
                    let assertions = extractAssertionExpressions def
                    let predicativeProperties = extractPredicativePropertiesExpressions def
                    let total =
                        predicateDefIfAny @ assertions @ predicativeProperties
                    if total.Length > 0 then
                        total
                    else
                        [FplUndetermined(LiteralPred, (this.StartPos, this.EndPos), this)]
                | None ->
                    [FplUndetermined(LiteralPred, (this.StartPos, this.EndPos), this)]
            | _ ->
                [FplUndetermined(LiteralPred, (this.StartPos, this.EndPos), this)]

and FplJustificationItemByConj(positions: Positions, parent: FplGenericNode) =
    inherit FplGenericJustificationItem(positions, parent)

    override this.Name = PrimJIByConj

    override this.Clone () =
        let ret = new FplJustificationItemByConj((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    member this.ParentJustification = this.Parent.Value :?> FplJustification

    override this.ProceedingExprCandidates
        // identify the expression contained in the conjecture
        // referred by this "byconj" justification in a proof
        with get (): FplGenericNode list =
            match this.RefersTo with
            | Some ax ->
                if ax.ArgList.Count > 0 then
                    [ax.ArgList |> Seq.last]
                else
                    [FplUndetermined(LiteralPred, (this.StartPos, this.EndPos), this)]
            | None ->
                [FplUndetermined(LiteralPred, (this.StartPos, this.EndPos), this)]

and FplJustificationItemByCor(positions: Positions, parent: FplGenericNode) =
    inherit FplGenericJustificationItem(positions, parent)

    override this.Name = PrimJIByCor

    override this.Clone () =
        let ret = new FplJustificationItemByCor((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    member this.ParentJustification = this.Parent.Value :?> FplJustification

    override this.ProceedingExprCandidates
        // identify the expression contained in the corollary
        // referred by this "bycor" justification in a proof
        with get (): FplGenericNode list =
            match this.RefersTo with
            | Some cor ->
                if cor.ArgList.Count > 0 then
                    [cor.ArgList |> Seq.last]
                else
                    [FplUndetermined(LiteralPred, (this.StartPos, this.EndPos), this)]
            | None ->
                [FplUndetermined(LiteralPred, (this.StartPos, this.EndPos), this)]

and FplJustificationItemByInf(positions: Positions, parent: FplGenericNode) =
    inherit FplGenericJustificationItem(positions, parent)

    override this.Name = PrimJIByInf

    override this.Clone () =
        let ret = new FplJustificationItemByInf((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    member this.ParentJustification = this.Parent.Value :?> FplJustification

    /// Replaces the variables in the conclusion of the rule of inference
    /// by expressions matched to these variables from the premise of the rule of inference
    /// when it was structurally matched to some expression.
    member private this.ReplaceVarsByVarUsages (conclusionExpression:FplGenericNode) (varUsageDict:Dictionary<string, FplGenericNode>) =
        let isVariableWithMatchedExpression (arg:FplGenericNode) =
            match arg.Name with
            | PrimRefL when arg.RefersTo.IsSome ->
                match arg.RefersTo with
                | Some var when var.Name = PrimVariableL ->
                    varUsageDict.ContainsKey(var.FplId) 
                | _ ->  false
            | _ ->  false

        let rec replaceVarsByUsages (expr:FplGenericNode) =
            let newArgList = List<FplGenericNode>()
            expr.ArgList
            |> Seq.iter (fun arg ->
                if isVariableWithMatchedExpression arg then
                    newArgList.Add varUsageDict[arg.FplId]
                else 
                    newArgList.Add (replaceVarsByUsages arg)
            )
            let exprVarList = expr.GetVariables()
            exprVarList
            |> Seq.iter (fun var ->
                if varUsageDict.ContainsKey(var.FplId) then
                    // correct the TypeId and FplId of any cloned conclusion variables (like those of cloned quantors in the conclusion of the rule of reference)
                    // to the TypeId of the mached variable of the matched premise of the rule of reference
                    var.TypeId <- varUsageDict[var.FplId].TypeId
                    // Note: The corrected FplId stems from the matched premise of the rule of reference, while the original FplId stems from its cloned conclusion
                    // The corrected FplId might, therefore, differ from the cloned Key (being the original FplId), where this variable resides in expr.Scope dictionary
                    // This difference is OK, since we only need the dictionaries value of the variable. This is used here a dummy for expression matching based on expression syntax (.Type SignatureType.Name)
                    // of the expression. The correct Key-Value correspondence was only needed when embedding the variable into the SymbolTable
                    // which at this point is only the input, not the output of the FplInterpreter.
                    var.FplId <- varUsageDict[var.FplId].FplId 
            )
            
            // replace expression arguments by new expressions where variables were replaced by their usages
            newArgList
            |> Seq.iteri (fun i arg -> expr.ArgList[i] <- arg)
            expr
        if isVariableWithMatchedExpression conclusionExpression then
            // If the conclusion of the rule of reference is a single variable
            // and this variable was matched with some expression,
            // we replace the whole conclusion with this matched expression
            varUsageDict[conclusionExpression.FplId]
        else
            // otherwise we replace it with the conclusionExpression in which
            // we recursively replace all variables by matched expressions
            replaceVarsByUsages conclusionExpression

    override this.ProceedingExprCandidates
        with get (): FplGenericNode list =
            match this.RefersTo, this.Parent with
            | Some (:? FplRuleOfInference as ruleOfInference), Some (:? FplJustification as just) ->
                match ruleOfInference.Premise, ruleOfInference.Conclusion with
                | Some premisePredicateListNode, Some conclusion ->
                    let premisePredicateList = premisePredicateListNode.ArgList |> Seq.toList
                    // (all justification items but the first one, which is the "byinf" one)
                    let (proceedingJustificationItems: FplGenericNode list) = allBefore this just.GetOrderedJustificationItems
                    let (proceedingExpressionLists : (FplGenericJustificationItem * FplGenericNode list) list) = 
                        proceedingJustificationItems
                        |> List.filter (fun fv -> fv :? FplGenericJustificationItem)
                        |> List.map (fun fv -> fv :?> FplGenericJustificationItem)
                        |> List.map (fun fv -> fv, fv.ProceedingExprCandidates)
                    // Here, we have for each element of premisePredicateList a whole list of proceeding expressions.
                    // We have to pair each premise (from the list of premises of the rule of inference) with the expressions that match it (if any).
                    // The resulting data structure is a dictionary of key-Value pairs where key = premise expression, value = list of expressions that matched the premise expression.
                    let listOfPairs = matchJustItemsExpressionsAgainstPremiseList proceedingExpressionLists premisePredicateList this
                    match this.ErrorOccurred with
                    | Some _ ->
                        // error occured while matching input justificationItems with premise list
                        [FplUndetermined(LiteralPred, (this.StartPos, this.EndPos), this)]
                    | None ->
                        if listOfPairs.Length > 0 then
                            let varUsageDict = snd listOfPairs.Head
                            let expr = conclusion.Clone()
                            [this.ReplaceVarsByVarUsages expr varUsageDict]
                        else
                            []
                | _ ->
                    [FplUndetermined(LiteralPred, (this.StartPos, this.EndPos), this)]
            | _ ->
                [FplUndetermined(LiteralPred, (this.StartPos, this.EndPos), this)]

    override this.Run() =
        debug this Debug.Start
        match this.ErrorOccurred with
        | Some _ -> this.SetDefaultValue()
        | None ->
            if this.ProceedingExprCandidates.Length <> 1 then
                this.SetDefaultValue()
            else
                match this.ProceedingExprCandidates.Head with
                | :? FplUndetermined -> this.SetDefaultValue()
                | candidate -> this.SetValue candidate
        debug this Debug.Stop


            //        if premisePredicateList.Length <> proceedingJustificationItems.Length then
            //            just.ErrorOccurred <- emitPR020Diagnostics (premisePredicateListNode.ArgList.Count) (proceedingJustificationItems.Length) just.StartPos just.EndPos
            //            [FplUndetermined(LiteralPred, (this.StartPos, this.EndPos), this)]
            //        else
            //            let dictPremise2MatchingExpressionListPairs = getDictOfPremise2MatchingExpressionListPairs proceedingExpressionLists premisePredicateList
            //            if checkExpressions proceedingExpressionLists (premisePredicateListNode.ArgList |> Seq.toList) this then
            //                // premisePredicateList matches proceedingJustificationItems
            //                // now check, if the argument inference corresponds to the conclusion
            //                let argInferenceExpr = argInferenceExprCandidates.Head
            //                if checkExpressions [argInferenceExpr] [conclusion] argInferenceExpr then
            //                    let v = new FplIntrinsicPred((this.StartPos, this.EndPos), this)
            //                    v.FplId <- LiteralTrue
            //                    this.SetValue v
            //                else
            //                    this.SetDefaultValue()
            //            else
            //                this.SetDefaultValue()
            //    | _, _ -> this.SetDefaultValue()
            //    let (arg:FplArgument) = just.ParentArgument

            //    match arg.ArgumentInference with
            //    | Some (argInference: FplGenericArgInference) ->
            //        let argInferenceExprCandidates = argInference.ProceedingExprCandidates
            //    | _ -> this.SetDefaultValue()
            //| _, _ -> this.SetDefaultValue()

and FplJustificationItemByRefArgument(positions: Positions, parent: FplGenericNode) =
    inherit FplGenericJustificationItem(positions, parent)

    override this.Name = PrimJIByRefArgument

    override this.Clone () =
        let ret = new FplJustificationItemByRefArgument((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    member this.ParentJustification = this.Parent.Value :?> FplJustification

    override this.ProceedingExprCandidates 
        // identify the expression referred by this "argument reference" justification in a proof
        with get (): FplGenericNode list =
            match this.RefersTo with
            | Some (:? FplArgument as argument) ->
                match argument.ArgumentInference with
                | Some (argInference: FplGenericArgInference) ->
                    // delegate to the argInference of the referenced argument
                    argInference.ProceedingExprCandidates
                | _ -> [FplUndetermined(LiteralPred, (this.StartPos, this.EndPos), this)]
            | _ ->
                [FplUndetermined(LiteralPred, (this.StartPos, this.EndPos), this)]

and FplJustificationItemByProofArgument(positions: Positions, parent: FplGenericNode) =
    inherit FplGenericJustificationItem(positions, parent)

    override this.Name = PrimJIByProofArgument

    override this.Clone () =
        let ret = new FplJustificationItemByProofArgument((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    member this.ParentJustification = this.Parent.Value :?> FplJustification

    override this.ProceedingExprCandidates 
        // identify the expression referred by this "argument reference" justification in a proof
        with get (): FplGenericNode list =
            match this.RefersTo with
            | Some (:? FplArgument as argument) ->
                match argument.ArgumentInference with
                | Some argInference ->
                    // delegate to the argInference of the referenced argument
                    argInference.ProceedingExprCandidates
                | _ -> [FplUndetermined(LiteralPred, (this.StartPos, this.EndPos), this)]
            | _ ->
                [FplUndetermined(LiteralPred, (this.StartPos, this.EndPos), this)]

and FplJustificationItemByTheoremLikeStmt(positions: Positions, parent: FplGenericNode) =
    inherit FplGenericJustificationItem(positions, parent)

    override this.Name = PrimJIByTheoremLikeStmt

    override this.Clone () =
        let ret = new FplJustificationItemByTheoremLikeStmt((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    member this.ParentJustification = this.Parent.Value :?> FplJustification

    override this.ProceedingExprCandidates
        // identify the expression contained in the theorem-like stmt
        // referred by this justification in a proof
        with get (): FplGenericNode list =
            match this.RefersTo with
            | Some ax ->
                if ax.ArgList.Count > 0 then
                    [ax.ArgList |> Seq.last]
                else
                    [FplUndetermined(LiteralPred, (this.StartPos, this.EndPos), this)]
            | None ->
                [FplUndetermined(LiteralPred, (this.StartPos, this.EndPos), this)]

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
        this.SetDefaultValue()
        debug this Debug.Stop


    member this.GetOrderedJustificationItems =
        this.ArgList |> Seq.toList

    override this.CheckConsistency () =
        let justItems = (this.ArgList |> Seq.toList)
        match justItems |> List.tryLast with
        | Some lastJustItem when lastJustItem.Name = PrimJIByInf ->
            () // only when the last justification item in a row is a "byinf" item, users can mex different justification types proceeding it
        | _ ->
            // else (i.e. last just item is different from a "byinf" item)
            match findTwoDifferentNames justItems with
            | Choice1Of2 _ -> ()
                // all justification item types are identical → OK
            | Choice2Of2 (justificationType1Pre, justificationType2Pre) ->
                let justificationType1 = justificationType1Pre.Replace("justification ", "")
                let justificationType2 = justificationType2Pre.Replace("justification ", "")
                // issue diagnostics if ordered justification items mix types of justification items
                this.ErrorOccurred <- emitPR019Diagnostics justificationType1 justificationType2 this.StartPos this.EndPos
        base.CheckConsistency()

    override this.EmbedInSymbolTable _ =
        this.CheckConsistency()
        addExpressionToParentArgList this

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
        this.ArgList
        |> Seq.tryFind (fun fv -> fv :? FplJustification)

    member this.ArgumentInference = 
            this.ArgList
            |> Seq.tryFind (fun fv -> fv :? FplGenericArgInference)
            |> Option.map (fun fv -> fv :?> FplGenericArgInference)

    override this.CheckConsistency () = 
        let (proof:FplProof) = this.ParentProof
        if proof.HasArgument (this.FplId) then 
            let conflict = proof.Scope[this.FplId]
            this.ErrorOccurred <- emitPR003Diagnostics this.FplId conflict.QualifiedStartPos this.StartPos this.StartPos 
        base.CheckConsistency()

    override this.EmbedInSymbolTable _ =
        this.CheckConsistency()
        match this.ErrorOccurred with
        | Some err -> ()
        | _ ->
            let proof = this.ParentProof
            proof.Scope.Add(this.FplId, this)

    override this.Run() =
        debug this Debug.Start
        // the argument has two elements, the justification and an argument inference
        let justificationOpt = this.Justification
        let argInferenceOpt = this.ArgumentInference

        match justificationOpt, argInferenceOpt with
        | Some justification, Some argInference -> 
            let orderdListJustifications = justification.ArgList |> Seq.toList
            if orderdListJustifications.Length = 0 then
                argInference.Run()
                this.SetValueOf argInference
            else
                let allEvaluateToTrue = 
                    orderdListJustifications
                    |> List.forall (fun fv ->
                        fv.Run()
                        let fvRepr = fv.Represent()
                        fvRepr = LiteralTrue
                    )
                if not allEvaluateToTrue then
                    this.SetDefaultValue()
                else
                    let v = new FplIntrinsicTrue((this.StartPos, this.StartPos), this)
                    this.SetValue v
        | Some justification, None -> 
            this.SetDefaultValue()
        | None, Some argInference -> 
            this.SetDefaultValue()
        | None, None -> 
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

    override this.ProceedingExprCandidates =
        raise (NotImplementedException())

    override this.Clone () =
        let ret = new FplArgInferenceAssume((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Run() = 
        debug this Debug.Start
        if heap.ValidStmtStore.RegisterExpression this then
            let v = new FplIntrinsicTrue((this.StartPos, this.EndPos), this)
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

    override this.EmbedInSymbolTable _ =
        addExpressionToParentArgList this 

    member private this.IsRevokable() =
        let fvAi = this
        let argumentId = fvAi.FplId
        let (arg:FplArgument) = fvAi.ParentArgument
        let proof = arg.ParentProof
        if argumentId = arg.FplId then 
            // revokes itself (circular)
            this.ErrorOccurred <- emitPR015Diagnostics argumentId this.StartPos this.EndPos
            false
        elif proof.HasArgument argumentId then 
            let refArg = proof.Scope[argumentId] :?> FplArgument
            let aiOpt = refArg.ArgumentInference
            match aiOpt with
            | Some (:? FplArgInferenceAssume as toBeRevoked) -> 
                match heap.ValidStmtStore.LastAssumedArgument with 
                | Some (:? FplArgInferenceAssume as last) when last = toBeRevoked -> 
                    true // is revocable
                | Some (:? FplArgInferenceAssume as last) when last <> toBeRevoked -> 
                    let lastArg = last.ParentArgument
                    this.ErrorOccurred <- emitPR016Diagnostics argumentId lastArg.FplId this.StartPos this.EndPos
                    false // not revocable
                | _ ->    
                    // the revoked argument was not assumed in the proof
                    this.ErrorOccurred <- emitPR015Diagnostics argumentId this.StartPos this.EndPos
                    false
            | _ -> 
                // the revoked argument was not assumed in the proof
                this.ErrorOccurred <- emitPR015Diagnostics argumentId this.StartPos this.EndPos
                false
        else
            this.ErrorOccurred <- emitPR005Diagnostics argumentId this.StartPos this.EndPos
            false

    override this.ProceedingExprCandidates =
        raise (NotImplementedException())


    override this.Run() = 
        debug this Debug.Start
        if this.IsRevokable() && heap.ValidStmtStore.RegisterExpression this then
            let v = new FplIntrinsicTrue((this.StartPos, this.EndPos), this)
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
        let v = new FplIntrinsicTrue((this.StartPos, this.EndPos), this)
        this.SetValue v
        debug this Debug.Stop

    override this.ProceedingExprCandidates 
        // identify the expression contained in the theorem-like statement belonging to the proof,
        // in which this "trivial" argument reference occurs
        with get (): FplGenericNode list =
            let themLikeStmtOpt = this.UltimateBlockNode
            match themLikeStmtOpt with
            | Some thmLikeStmt ->
                if thmLikeStmt.ArgList.Count > 0 then
                    [thmLikeStmt.ArgList |> Seq.last]
                else
                    [FplUndetermined(LiteralPred, (this.StartPos, this.EndPos), this)]
            | None ->
                [FplUndetermined(LiteralPred, (this.StartPos, this.EndPos), this)]

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
        let v = FplIntrinsicTrue((this.StartPos, this.EndPos), this.Parent.Value)
        this.SetValue v
        debug this Debug.Stop

    member this.ParentArgument = this.Parent.Value :?> FplArgument

    override this.ProceedingExprCandidates
        // the argument of this FplArgInferenceDerived is the expression we need as a candidate
        with get (): FplGenericNode list =
            let exprOpt = this.ArgList |> Seq.tryHead
            match exprOpt with
            | Some expr -> [expr]
            | _ -> [FplUndetermined(LiteralPred, (this.StartPos, this.EndPos), this)]

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
        let allEvaluateToTrue =
            orderedProofArguments
            |> Seq.forall (fun fv -> 
                fv.Run()
                let fvRepr = fv.Represent()
                fvRepr = LiteralTrue 
            )
        if not allEvaluateToTrue then
            this.ErrorOccurred <- emitPR009Diagnostics this.StartPos this.StartPos
            this.SetDefaultValue()
        else
            let v = new FplIntrinsicTrue((this.SignStartPos, this.SignEndPos), this)
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
        |> List.filter (fun justification -> justification.ArgList.Count <> 1)
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
            let parent = fv1.Parent.Value
            let arg = parent.Parent.Value
            arg.Parent.Value :?> FplProof
    if proof.HasArgument argName then 
        Some proof.Scope[argName]
    else 
        None
