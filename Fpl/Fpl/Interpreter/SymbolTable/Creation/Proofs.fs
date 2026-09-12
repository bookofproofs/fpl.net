(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)

/// <summary>
/// Provides specialized evaluators for AST nodes that represent proofs and related
/// constructs in FPL (proof signatures, proof blocks, arguments, justifications, and
/// justification items).
/// </summary>
/// <remarks>
/// The evaluator updates the interpreter symbol-table heap state, creates and manipulates
/// FPL node frames (pushing and popping them on the evaluation stack), delegates nested
/// evaluation to <c>evalRef.Value</c>, and emits diagnostics via the emitter helpers.
/// Side effects include mutations of <c>heap</c>, setting helper flags (for signature or
/// reference evaluation), and creating/attaching proof-related objects declared in
/// <c>Fpl.Interpreter.SymbolTable.Types4.Proofs</c>.
/// </remarks>
/// <exception cref="System.Exception">Thrown when an unsupported AST node is supplied to the top-level evaluator.</exception>
module Fpl.Interpreter.SymbolTable.Creation.Proofs
open Fpl.Primitives
open Fpl.Parser.Types
open Fpl.Errors.Emitter
open Fpl.Interpreter.BasicTypes
open Fpl.Interpreter.Helpers.Basic
open Fpl.Interpreter.Helpers.Checks
open Fpl.Interpreter.SymbolTable.Storage.Heap
open Fpl.Interpreter.SymbolTable.Storage.Util
open Fpl.Interpreter.SymbolTable.Types4.Proofs
open Fpl.Interpreter.SymbolTable.Creation.Forward


/// <summary>
/// Evaluate AST nodes that belong to proofs and their substructures.
/// </summary>
/// <param name="ast">An AST node representing a proof or a node related to proofs
/// (e.g. <c>Ast.Proof</c>, <c>Ast.ProofSignature</c>, <c>Ast.Argument</c>,
/// <c>Ast.Justification</c>, justification items, inference forms, <c>Ast.Qed</c>, etc.).</param>
/// <returns>Unit. The function performs side effects on the global evaluation <c>heap</c>
/// and may emit diagnostics.</returns>
/// <remarks>
/// - For a top-level <c>Ast.Proof</c> node, a new <c>FplProof</c> frame is created,
///   pushed to the evaluation stack to register the signature, popped to attach the
///   proof to any enclosing theorem-like statement, pushed again to process the proof
///   body, checked for consistency via <c>CheckConsistency</c>, and finally popped.
/// - For signature nodes <c>Ast.ProofSignature</c> the function sets
///   <c>heap.Helper.InSignatureEvaluation</c> while evaluating nested signature nodes and
///   records signature positions with <c>setSignaturePositions</c>.
/// - For arguments, justification items and inference forms the evaluator creates the
///   appropriate <c>Fpl*</c> nodes, pushes them on the evaluation stack, delegates
///   nested evaluation to <c>evalRef.Value</c>, and then pops the frames.
/// - The function relies on helper methods that search symbol tables and emit diagnostics
///   (e.g. <c>findCandidatesByName</c>, <c>emitPR...Diagnostics</c>), and thus can set
///   error flags on created frames (such as <c>ErrorOccurred</c> and <c>RefersTo</c>).
/// </remarks>
/// <exception cref="System.Exception">Thrown when <paramref name="ast"/> is not a recognized proof or related node.</exception>
let evalProofs ast =
    match ast with
    | Ast.Proof((pos1, pos2), (proofSignatureAst, proofBlockAst)) ->
        let parent = heap.Eval.PeekEvalStack()
        let fv = new FplProof((pos1, pos2), parent, heap.Helper.GetNextAvailableFplBlockRunOrder)
        heap.Eval.PushEvalStack(fv)
        evalRef.Value proofSignatureAst
        heap.Eval.PopEvalStack() // add to parent theorem (if any)
        heap.Eval.PushEvalStack(fv) // push again
        evalRef.Value proofBlockAst
        fv.CheckConsistency()
        heap.Eval.Pop() |> ignore // pop without embedding in theorem (already done)
    | Ast.ProofSignature((pos1, pos2), (simpleSignatureAst, dollarDigitListAsts)) ->
        heap.Helper.InSignatureEvaluation <- true
        evalRef.Value simpleSignatureAst
        dollarDigitListAsts |> List.map evalRef.Value |> ignore
        setSignaturePositions pos1 pos2
        heap.Helper.InSignatureEvaluation <- false
    | Ast.ProofBlock proofContent ->
        evalRef.Value proofContent
    | Ast.ProofContent ((varDeclBlock, proofArgumentListAst), optQedAst) ->
        evalRef.Value varDeclBlock
        proofArgumentListAst |> List.map evalRef.Value |> ignore
        optQedAst |> Option.map evalRef.Value |> Option.defaultValue ()
    | Ast.Argument((pos1, pos2), (justifiedArgumentAst)) ->
        let fv = heap.Eval.PeekEvalStack()
        let arg = new FplArgument((pos1, pos2), fv, heap.Helper.GetNextAvailableFplBlockRunOrder) 
        heap.Eval.PushEvalStack(arg)
        evalRef.Value justifiedArgumentAst
        heap.Eval.PopEvalStack()
    | Ast.JustArgInf((pos1, pos2), (justificationAst, argumentInferenceAst)) ->
        evalRef.Value justificationAst
        evalRef.Value argumentInferenceAst
    | Ast.StartArgument argumentIdentifier ->
        evalRef.Value argumentIdentifier
    | Ast.StartArgumentStictly (argumentIdentifier, justificationItemListAsts) ->
        evalRef.Value argumentIdentifier
        justificationItemListAsts |> List.map evalRef.Value |> ignore
    | Ast.Justification((pos1, pos2), justificationItemAst) ->
        match justificationItemAst with
        | Ast.StartArgument _ ->
            evalRef.Value justificationItemAst // symbol table will be missing justification because it was omitted in FPL code
        | Ast.StartArgumentStictly _ 
        | _ ->
            // otherwise, we create a justification node
            let fv = heap.Eval.PeekEvalStack()
            let just = new FplJustification((pos1, pos2), fv)
            heap.Eval.PushEvalStack(just)
            evalRef.Value justificationItemAst
            heap.Eval.PopEvalStack()
    | Ast.JustificationItem((pos1, pos2), justificationReferenceAst) ->
        evalRef.Value justificationReferenceAst 
    | Ast.ReferenceToProofOrCorollary((pos1, pos2), (referencingIdentifierAst)) ->
        heap.Helper.InReferenceToProofOrCorollary <- true
        evalRef.Value referencingIdentifierAst
        heap.Helper.InReferenceToProofOrCorollary <- false
    | Ast.ByDef((pos1, pos2), variableAst) ->
        let parent = heap.Eval.PeekEvalStack()
        let fvJi = new FplJustificationItemByDefVar((pos1, pos2), parent)
        heap.Eval.PushEvalStack(fvJi)
        evalRef.Value variableAst
        heap.Eval.PopEvalStack()
    | Ast.JustificationIdentifier((pos1, pos2), (((byModifierOption, predicateIdentifierAst), dollarDigitListAsts), refArgumentIdentifierAst)) ->
        let parent = heap.Eval.PeekEvalStack()

        /// <summary>
        /// Validate and resolve candidate bindings for a justification item and emit appropriate diagnostics.
        /// </summary>
        /// <param name="fvJi">The justification item frame to be resolved. Side effects: may set <c>RefersTo</c>
        /// and <c>ErrorOccurred</c> on <c>fvJi</c>.</param>
        /// <param name="candidates">A list of candidate frames returned by name lookup to be inspected.</param>
        /// <returns>Unit. Side effects: updates <c>fvJi</c> and emits diagnostics through emitter helpers.</returns>
        /// <remarks>
        /// This helper inspects the scope search results returned by
        /// <c>tryFindAssociatedBlockForJustificationItem</c>, refines references for proof-argument
        /// cases (PR006), and issues diagnostics for incorrect block types (PR001) or multiple matches (ID023).
        /// It is intended to be a local, side-effecting helper used during evaluation of
        /// <c>Ast.JustificationIdentifier</c> nodes.
        /// </remarks>
        let checkPR001_PR006Diagnostics (fvJi:FplGenericNode) candidates = 
            match tryFindAssociatedBlockForJustificationItem fvJi candidates with
            | ScopeSearchResult.FoundAssociate potentialCandidate ->
                match fvJi with 
                | :? FplJustificationItemByProofArgument as fvJi1 ->
                    let split = fvJi.FplId.Split(":")
                    if split.Length > 1 then 
                        // here, argName is the argument identifier of the other proof
                        let proofName = $"{split.[0]}"
                        let argName = $"{split.[1]}"
                        fvJi.RefersTo <- Some potentialCandidate // first stage - set the potential other proof to refers to 
                        match getArgumentInProof fvJi1 argName with
                        | Some argument ->
                            fvJi.RefersTo <- Some argument // second stage - refine the found argument 
                        | _ ->
                            fvJi.ErrorOccurred <- emitPR006Diagnostics proofName argName fvJi.StartPos fvJi.EndPos
                            // remove the first stage, since the no argument was found in other proof
                            fvJi.RefersTo <- None
                | _ -> fvJi.RefersTo <- Some potentialCandidate
            | ScopeSearchResult.FoundIncorrectBlock otherBlock ->
                let alternative = 
                    match fvJi.Name with 
                    | PrimJIByAx ->
                        "A reference to an axiom was required."
                    | PrimJIByConj ->
                        "A reference to a conjecture was required."
                    | PrimJIByCor ->
                        "A reference to a corollary was required."
                    | PrimJIByDef ->
                        "A reference to a definition (of a class, a predicate, or a functional term) was required."
                    | PrimJIByDefVar ->
                        "A reference to a variable was required."
                    | PrimJIByInf ->
                        "A reference to a rule of inference was required."
                    | PrimJIByProofArgument ->
                        "A reference to an argument in another proof was required."
                    | PrimJIByRefArgument ->
                        "A reference to a previous argument in this proof was required."
                    | PrimJIByTheoremLikeStmt ->
                        "A reference to a theorem, a lemma, or a proposition was required."
                    | _ -> "Expected another reference."
                fvJi.ErrorOccurred <- emitPR001Diagnostics (qualifiedNameSimple otherBlock) fvJi.Name fvJi.StartPos fvJi.EndPos alternative
            | ScopeSearchResult.FoundMultiple listOfKandidates ->
                fvJi.ErrorOccurred <- emitID023Diagnostics listOfKandidates fvJi.StartPos fvJi.EndPos
            | _ -> ()


        match byModifierOption, dollarDigitListAsts, refArgumentIdentifierAst with
        | Some LiteralByAx, Some _, None -> 
            // byax justification cannot be used together with a proof or corollary reference
            parent.ErrorOccurred <- emitPR010Diagnostics LiteralByAx LiteralAxL pos1 pos2 
        | Some LiteralByAx, Some _, Some _ -> 
            // byax justification cannot be used together with a proof argument reference 
            parent.ErrorOccurred <- emitPR011Diagnostics LiteralByAx LiteralAxL pos1 pos2 
        | Some LiteralByAx, None, None -> 
            let fvJi = new FplJustificationItemByAx((pos1, pos2), parent)
            heap.Eval.PushEvalStack(fvJi)
            evalRef.Value predicateIdentifierAst
            // check, if indeed the predicateId points to an axiom, if not issue diagnostics
            let candidates = findCandidatesByName fvJi.FplId false false
            checkPR001_PR006Diagnostics fvJi candidates
            heap.Eval.PopEvalStack()
        | Some LiteralByConj, Some _, None -> 
            // byconj justification cannot be used together with a proof reference
            parent.ErrorOccurred <- emitPR010Diagnostics LiteralByConj LiteralConjL pos1 pos2 
        | Some LiteralByConj, Some _, Some _ -> 
            // byconj justification cannot be used together with a proof argument reference 
            parent.ErrorOccurred <- emitPR011Diagnostics LiteralByConj LiteralConjL pos1 pos2 
        | Some LiteralByConj, None, None -> 
            let fvJi = new FplJustificationItemByConj((pos1, pos2), parent)
            heap.Eval.PushEvalStack(fvJi)
            evalRef.Value predicateIdentifierAst
            // check, if indeed the predicateId points to a conjecture, if not issue diagnostics
            let candidates = findCandidatesByName fvJi.FplId false false
            checkPR001_PR006Diagnostics fvJi candidates
            heap.Eval.PopEvalStack()
        | Some LiteralByCor, Some _, _ -> 
            let fvJi = new FplJustificationItemByCor((pos1, pos2), parent)
            heap.Eval.PushEvalStack(fvJi)
            evalRef.Value predicateIdentifierAst
            dollarDigitListAsts.Value |> List.map evalRef.Value |> ignore
            let candidatesPre = findCandidatesByName fvJi.FplId false true
            let candidates =
                if candidatesPre.Length > 1 then 
                    candidatesPre |> List.filter (fun fv -> fv.FplId = fvJi.FplId)
                else
                    candidatesPre
            // check, if indeed the predicateId points to a corollary, if not issue diagnostics
            checkPR001_PR006Diagnostics fvJi candidates
            heap.Eval.PopEvalStack()
        | Some LiteralByCor, None, _ -> 
            // byCor justification a reference to a corollary
            match predicateIdentifierAst with
            | Ast.PredicateIdentifier(_, providedIdentifier) ->
                parent.ErrorOccurred <- emitPR012Diagnostics providedIdentifier pos1 pos2
            | _ -> ()
        | Some LiteralByDef, Some _, None -> 
            // byDef justification cannot be used together with a proof reference
            parent.ErrorOccurred <- emitPR010Diagnostics LiteralByDef LiteralDefL pos1 pos2 
        | Some LiteralByDef, Some _, Some _ -> 
            // byDef justification cannot be used together with a proof argument reference 
            parent.ErrorOccurred <- emitPR011Diagnostics LiteralByDef LiteralDefL pos1 pos2 
        | Some LiteralByDef, None, None -> 
            let fvJi = new FplJustificationItemByDef((pos1, pos2), parent)
            heap.Eval.PushEvalStack(fvJi)
            evalRef.Value predicateIdentifierAst
            // check, if indeed the predicateId points to a definition, if not issue diagnostics
            let candidates = findCandidatesByName fvJi.FplId false false
            checkPR001_PR006Diagnostics fvJi candidates
            heap.Eval.PopEvalStack()
        | Some LiteralByInf, Some _, None -> 
            // byInf justification cannot be used together with a proof reference
            parent.ErrorOccurred <- emitPR010Diagnostics LiteralByInf PrimRuleOfInference pos1 pos2 
        | Some LiteralByInf, Some _, Some _ -> 
            // byInf justification cannot be used together with a proof argument reference 
            parent.ErrorOccurred <- emitPR011Diagnostics LiteralByInf PrimRuleOfInference pos1 pos2 
        | Some LiteralByInf, None, None -> 
            let fvJi = new FplJustificationItemByInf((pos1, pos2), parent)
            heap.Eval.PushEvalStack(fvJi)
            evalRef.Value predicateIdentifierAst
            // check, if indeed the predicateId points to a rule of inference, if not issue diagnostics
            let candidates = findCandidatesByName fvJi.FplId false false
            checkPR001_PR006Diagnostics fvJi candidates
            heap.Eval.PopEvalStack()
        | Some _, _, _ -> () // does not occur, because the parser byModifier choices between only two keywords LiteralByAx or LiteralByDef
        | None, Some _, None -> 
            let fvJi = new FplJustificationItemByCor((pos1, pos2), parent)
            heap.Eval.PushEvalStack(fvJi)
            evalRef.Value predicateIdentifierAst
            dollarDigitListAsts.Value |> List.map evalRef.Value |> ignore
            let candidates = findCandidatesByName fvJi.FplId false true
            // check, if indeed the predicateId points to a corollary, if not issue diagnostics
            checkPR001_PR006Diagnostics fvJi candidates
            // issue info diagnostics that references to a corollary need the keyword byCor to increase readability
            parent.ErrorOccurred <- emitPR013Diagnostics pos1 pos2
            heap.Eval.PopEvalStack()
        | None, Some _, Some _ -> 
            let fvJi = new FplJustificationItemByProofArgument((pos1, pos2), parent)
            heap.Eval.PushEvalStack(fvJi)
            evalRef.Value predicateIdentifierAst
            dollarDigitListAsts.Value |> List.map evalRef.Value |> ignore
            evalRef.Value refArgumentIdentifierAst.Value 
            let splitOffAnyArgumentId (input: string) =
                let parts = input.Split(':')
                if parts.Length > 0 then parts.[0] else ""
            let name = splitOffAnyArgumentId fvJi.FplId
            let candidates = findCandidatesByName name false true 
            let candidatesFiltered = 
                if candidates.Length > 1 then 
                    candidates |> List.filter (fun fv -> fv.FplId = name)
                else
                    candidates
            // check, if indeed the predicateId points to another proof, if not issue diagnostics, 
            // also check if arg exists, if not issue diagnostics
            checkPR001_PR006Diagnostics fvJi candidatesFiltered
            heap.Eval.PopEvalStack()
        | None, None, Some _ ->  
            // issue diagnostics a theorem-like statement justification cannot be used together with a proof argument reference 
            parent.ErrorOccurred <- emitPR014Diagnostics pos1 pos2 
        | None, None, None -> 
            let fvJi = new FplJustificationItemByTheoremLikeStmt((pos1, pos2), parent)
            heap.Eval.PushEvalStack(fvJi)
            evalRef.Value predicateIdentifierAst
            let candidates = findCandidatesByName fvJi.FplId false false
            // check if indeed the predicateId points to a theorem-like statement except a corollary, if not issue diagnostics
            checkPR001_PR006Diagnostics fvJi candidates
            heap.Eval.PopEvalStack()
    | Ast.TrivialArgument((pos1, pos2), _) -> 
        let fv = heap.Eval.PeekEvalStack()
        let refBlock = new FplArgInferenceTrivial((pos1, pos2), fv) 
        heap.Eval.PushEvalStack(refBlock)
        heap.Eval.PopEvalStack()
    | Ast.DeriveArgument ((pos1, pos2),predicateAst) -> 
        let fv = heap.Eval.PeekEvalStack()
        let argInf = new FplArgInferenceDerived((pos1, pos2), fv) 
        heap.Eval.PushEvalStack(argInf)
        evalRef.Value predicateAst
        heap.Eval.PopEvalStack()
    | Ast.AssumeArgument((pos1, pos2), predicateAst) ->
        let fv = heap.Eval.PeekEvalStack()
        let fvNew = new FplArgInferenceAssume((pos1, pos2), fv) 
        heap.Eval.PushEvalStack(fvNew)
        evalRef.Value predicateAst
        heap.Eval.PopEvalStack()
    | Ast.RevokeArgument((pos1, pos2), predicateAst) ->
        let fv = heap.Eval.PeekEvalStack()
        let argInf = new FplArgInferenceRevoke((pos1, pos2), fv) 
        heap.Eval.PushEvalStack(argInf)
        evalRef.Value predicateAst
        heap.Eval.PopEvalStack()
    | Ast.Qed((pos1, pos2), _) -> ()
    | _ ->
        failwith (sprintf "{%O} is not a proof or related node" ast)
