/// This module evaluates the abstract syntax tree (AST) and interprets its semantics./// This module evaluates the abstract syntax tree (AST) and interprets its semantics.
/// It produces a SymbolTable object containing a current semantical representation of the AST.

(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)
module FplInterpreterBuildingBlocks

open System
open System.Collections.Generic
open ErrDiagnostics
open FplPrimitives
open FplGrammarTypes
open FplInterpreterDiagnosticsEmitter
open FplInterpreterAstPreprocessing
open FplInterpreterBasicTypes
open FplInterpreter.Globals.Root
open FplInterpreter.Globals.ST
open FplInterpreter.Globals.Heap
open FplInterpreterChecks
open FplInterpreterSTEmbedding
open FplInterpreterIntrinsicTypes
open FplInterpreterVariables
open FplInterpreterReferences
open FplInterpreterDefinitions
open FplInterpreterFplTypeMatching
open FplInterpreterPredicativeBlocks
open FplInterpreterDefinitionProperties
open FplInterpreterProofs
open FplInterpreterQuantors
open FplInterpreterRulesOfInferences
open FplInterpreterCompoundPredicates
open FplInterpreterExtensions
open FplInterpreterDelegates
open FplInterpreterMapCases
open FplInterpreterLocalization
open FplInterpreterReferencesSelfParent
open FplInterpreterAssertStmt
open FplInterpreterAssignments
open FplInterpreterIsOperator
open FplInterpreterForStmt
open FplInterpreterCasesStmt

/// Simplify trivially nested expressions by removing from the stack FplValue nodes that were created due to too long parsing tree and replacing them by their sub nodes 
let rec simplifyTriviallyNestedExpressions (rb1:FplGenericNode) = 
    match rb1 with 
    | :? FplReference as rb when rb.ArgList.Count = 1 && rb.FplId = "" ->
        // removable reference blocks are those with only a single argument and unset FplId 
        let subNode = rb.ArgList[0] 
        heap.Eval.Pop() |> ignore // pop the removable reference block and ignored it
        heap.Eval.PushEvalStack(subNode) // push its subNode instead
        // adjust subNode's Parent, EndPos, Scope
        subNode.Parent <- rb.Parent 
        subNode.EndPos <- rb.EndPos
        // prevent recursive loops
        rb.ArgList.Clear() 
        rb.Value <- None
        rb.Scope.Clear()
        simplifyTriviallyNestedExpressions subNode
    | _ -> ()


let setKeywordType keywordType pos1 pos2 = 
    let fv = heap.Eval.PeekEvalStack()
    match fv with
    | :? FplVariableArray as arr -> arr.SetType keywordType None pos1 pos2 
    | :? FplMapping as map -> map.SetType keywordType None pos1 pos2
    | _ ->  fv.TypeId <- keywordType

/// A recursive function evaluating an AST and returning a list of EvalAliasedNamespaceIdentifier records
/// for each occurrence of the uses clause in the FPL code.
let rec eval (st: SymbolTable) ast =
    let evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst =
        match optVarDeclOrSpecList with
        | Some astList -> astList |> List.map (eval st) |> ignore
        | None -> ()
        eval st predicateAst

    let setSignaturePositions pos1 pos2 = 
        let fv = heap.Eval.PeekEvalStack()
        match box fv with 
        | :? IHasSignature as withSignature -> 
            withSignature.SignStartPos <- pos1  
            withSignature.SignEndPos <- pos2
        | _ -> ()

    match ast with
    // lexical / leaf tokens
    | Ast.Alias((pos1, pos2), s) -> ()
    | Ast.Dot((pos1, pos2),()) -> ()
    | Ast.Star((pos1, pos2),()) -> ()

    | Ast.Digits s -> 
        let fv = heap.Eval.PeekEvalStack()
        fv.FplId <- s
        fv.TypeId <- s

    | Ast.DollarDigits((pos1, pos2), s) -> 
        let fv = heap.Eval.PeekEvalStack()
        let sid = $"${s.ToString()}"
        match fv with 
        | :? FplReference when fv.FplId = String.Empty && not heap.Helper.InReferenceToProofOrCorollary ->
            let value = new FplIntrinsicInd((pos1, pos2), fv)
            value.FplId <- sid
            heap.Eval.PushEvalStack(value)
            heap.Eval.PopEvalStack()
        | _  ->
            fv.FplId <- fv.FplId + sid
            match fv.TypeId with 
            | "" when not heap.Helper.InReferenceToProofOrCorollary -> fv.TypeId <- LiteralInd
            | LiteralPred -> ()
            | _ -> fv.TypeId <- fv.TypeId + sid

    | Ast.ExtensionRegex s -> 
        let fv = heap.Eval.PeekEvalStack()
        fv.TypeId <- s

    | Ast.ExtensionName((pos1, pos2), s) ->
        let fv = heap.Eval.PeekEvalStack()
        let extensionName = s
        match fv with 
        | :? FplExtension ->
            fv.FplId <- extensionName
            fv.TypeId <- extensionName
        | _ -> ()

    | Ast.LanguageCode((pos1, pos2), s) -> 
        let fv = heap.Eval.PeekEvalStack()
        fv.FplId <- s
        fv.TypeId <- s
        fv.StartPos <- pos1
        fv.EndPos <- pos2
    | Ast.LocalizationString((pos1, pos2), s) -> 
        let fv = heap.Eval.PeekEvalStack()
        fv.FplId <- s
        fv.TypeId <- s

    | Ast.PascalCaseId ((pos1, pos2), pascalCaseId) -> 
        let fv = heap.Eval.PeekEvalStack()
        match fv.Name with
        | LiteralAxL
        | LiteralThmL
        | LiteralPropL
        | LiteralLemL
        | LiteralConjL
        | LiteralCorL
        | PrimFunctionalTermL
        | PrimPredicateL
        | LiteralPrfL
        | PrimMandatoryFunctionalTermL
        | PrimMandatoryPredicateL
        | PrimPredicateL
        | PrimFunctionalTermL
        | PrimRuleOfInference -> 
            fv.FplId <- pascalCaseId
        | LiteralCtorL ->
            fv.FplId <- pascalCaseId
            fv.TypeId <- pascalCaseId
            fv.ErrorOccurred <- emitID008Diagnostics pascalCaseId fv.Parent.Value.FplId pos1 pos2
        | PrimClassL ->
            fv.FplId <- pascalCaseId
            fv.TypeId <- pascalCaseId
        | _ -> ()

    | Ast.PredicateIdentifier((pos1, pos2), identifier) ->
        let fv = heap.Eval.PeekEvalStack()
        let searchIdentifier = 
            if heap.Helper.InReferenceToProofOrCorollary then 
                $"{identifier}{fv.FplId}"
            else
                identifier
            
        let candidatesFromTheory = findCandidatesByName fv searchIdentifier false heap.Helper.InReferenceToProofOrCorollary
        let candidatesLocal = findPropertyCandidatesByNameInBlock fv searchIdentifier
        let candidatesOfMapping = findCandidateOfExtensionMapping fv searchIdentifier
        let candidates, candidatesNames =  filterCandidates (candidatesFromTheory @ candidatesLocal @ candidatesOfMapping) searchIdentifier true
        let correctIds (fv1:FplGenericNode) = 
            match fv with 
            | :? FplBase 
            | :? FplBaseConstructorCall 
            | :? FplForInStmtDomain -> 
                fv1.FplId <- searchIdentifier
                fv1.TypeId <- searchIdentifier
            | :? FplReference -> 
                fv1.FplId <- searchIdentifier
                fv1.TypeId <- searchIdentifier
            | :? FplGenericJustificationItem as fvJi -> 
                fvJi.FplId <- searchIdentifier
            | _ -> ()

        match candidates.Length with
        | 0 -> 
            match fv.Parent with
            | Some (:? FplReference as parent) when parent.DottedChild.IsSome && Object.ReferenceEquals(fv, parent.DottedChild.Value) ->
                // do not emit ID010 diagnostics, if fv is a dotted child, whose identifier we are still being evaluated
                // only with this identifier, it will be possible in AST.PredicateWithOptSpecification to search for correct candidates 
                () 
            | _ -> 
                // otherwise, issue ID010 diagnostics
                fv.ErrorOccurred <- emitID010Diagnostics identifier pos1 pos2
            match fv with 
            | :? FplVariableArray as arr -> arr.SetType identifier None pos1 pos2
            | :? FplMapping as map -> map.SetType identifier None pos1 pos2
            | :? FplVariable ->
                let fvWithValue = fv :?> FplGenericHasValue
                fvWithValue.TypeId <- identifier
                fvWithValue.SetDefaultValue()
            | _ -> correctIds fv 
        | 1 ->
            let candidate = candidates.Head
            match fv with 
            | :? FplVariableArray as arr ->  arr.SetType identifier (Some candidate) pos1 pos2
            | :? FplMapping as map -> 
                let candidate = candidates.Head
                // mappings can point to classes 
                map.SetType identifier (Some candidate) pos1 pos2
            | :? FplVariable -> 
                fv.TypeId <- identifier
                fv.RefersTo <- Some candidate
            | _ -> correctIds fv
        | _ ->
            match fv with 
            | :? FplMapping 
            | :? FplVariable -> 
                fv.ErrorOccurred <- emitID017Diagnostics identifier candidatesNames pos1 pos2
            | _ -> correctIds fv

    // types and type-related constructs
    | Ast.IndexType((pos1, pos2),()) -> 
        setKeywordType LiteralInd pos1 pos2
    | Ast.FunctionalTermType((pos1, pos2),()) -> 
        setKeywordType LiteralFunc pos1 pos2
    | Ast.ObjectType((pos1, pos2),()) -> 
        setKeywordType LiteralObj pos1 pos2
    | Ast.PredicateType((pos1, pos2),()) -> 
        setKeywordType LiteralPred pos1 pos2
    | Ast.TemplateType((pos1, pos2), s) -> 
        let fv = heap.Eval.PeekEvalStack()
        setKeywordType s pos1 pos2
        let templateNode = new FplIntrinsicTpl(s, (pos1, pos2), fv)
        match fv with 
        | :? FplGenericVariable as var -> 
            // attach template type to declared variable 
            var.RefersTo <- Some templateNode
        | _ -> () // RefersTo's semantics in other FplValues is different, do not interfere with it
        heap.Eval.PushEvalStack(templateNode)
        heap.Eval.PopEvalStack()

    | Ast.ArrayType((pos1, pos2), (mainTypeAst, indexAllowedTypeListAst)) ->
        let fv = heap.Eval.PeekEvalStack()
        match fv with 
        | :? FplMapping as mapping -> mapping.SetIsArray()
        | _ -> ()
        eval st mainTypeAst
        indexAllowedTypeListAst |> List.map (eval st) |> ignore
    | Ast.SimpleVariableType((pos1, pos2), simpleVariableTypeAst) ->
        eval st simpleVariableTypeAst
    | Ast.IndexAllowedType((pos1, pos2), indexAllowedTypeAst) ->
        eval st indexAllowedTypeAst


    | Ast.Intrinsic((pos1, pos2),()) -> 
        let fv = heap.Eval.PeekEvalStack()
        fv.IsIntrinsic <- true // flag that this block is intrinsic
        match fv.Name with 
        | PrimClassL ->
            let cl = fv :?> FplClass
            cl.AddDefaultConstructor()
        | _ -> ()
    | Ast.Error  -> ()


    | Ast.Var((pos1, pos2), name) ->
        let searchVarByName (fv:FplGenericNode) = 
            match (searchInUpperScopeByName fv name) with
            | ScopeSearchResult.Found foundVar -> 
                // variable was declared in the scope
                match fv.Name with 
                | PrimJIByDefVar 
                | PrimRefL 
                | PrimForInStmtEntityL 
                | PrimForInStmtDomainL ->
                    fv.FplId <- name
                    fv.RefersTo <- Some foundVar
                | PrimTranslationL ->
                    // for translations, use the name of the variable
                    fv.FplId <- foundVar.FplId
                    fv.TypeId <- foundVar.TypeId 
                | _ -> ()
                match foundVar with
                | :? FplGenericVariable as var -> var .SetIsUsed()
                | _ -> ()
            | _ ->
                match fv.UltimateBlockNode with 
                | Some (:? FplLocalization as loc) when heap.Helper.InSignatureEvaluation -> 
                    () // localizations during 
                | _ ->
                    // otherwise emit variable not declared 
                    fv.ErrorOccurred <- emitVAR01diagnostics name pos1 pos2
                
                // if no variable in scope was found, spawn an undefined variable
                let undefVar = new FplVariable(name, (pos1, pos2), fv)
                undefVar.TypeId <- LiteralUndef
                undefVar.SetDefaultValue()
                heap.Eval.PushEvalStack(undefVar)
                heap.Eval.PopEvalStack()
            
        let fv = heap.Eval.PeekEvalStack()
        let parentFv = fv.Parent.Value
        match fv.Name with 
        | PrimVariableL
        | PrimVariableArrayL -> 
            // in the context of variable declarations, we set the name and positions of the variables
            fv.FplId <- name
            fv.TypeId <- LiteralUndef 
            fv.StartPos <- pos1
            fv.EndPos <- pos2
        | PrimExtensionL -> 
            let newVar = new FplVariable(name, (pos1, pos2), fv)
            newVar.TypeId <- fv.FplId
            newVar.IsSignatureVariable <- heap.Helper.InSignatureEvaluation
            heap.Eval.PushEvalStack(newVar)
            heap.Eval.PopEvalStack()
        | PrimRefL ->
            match box parentFv with 
            | :? IHasDotted as dotted when dotted.DottedChild.IsSome ->
                fv.FplId <- name
                fv.TypeId <- LiteralUndef
            | _ -> searchVarByName fv
        | _ -> 
            // in all other contexts, check by name, if this variable was declared in some scope
            searchVarByName fv

        match fv.UltimateBlockNode with 
        | Some (:? FplLocalization as loc) when loc.ArgList.Count = 0 && fv.RefersTo.IsSome -> 
            let variable = fv.RefersTo.Value
            if loc.Scope.ContainsKey(name) then 
                let other = loc.Scope[name]
                variable.ErrorOccurred <- emitVAR11diagnostics name other.QualifiedStartPos pos1 pos2 
            else 
                loc.Scope.Add(name, variable)
                variable.Parent <- Some loc
        | _ -> ()

    | Ast.ObjectSymbol((pos1, pos2), symbol) -> 
        let fv = heap.Eval.PeekEvalStack()
        fv.FplId <- symbol
        fv.TypeId <- symbol
        fv.StartPos <- pos1
        fv.EndPos <- pos2
        checkSIG01Diagnostics fv
    | Ast.ArgumentIdentifier((pos1, pos2), argumentId) -> 
        let fv = heap.Eval.PeekEvalStack()
        fv.FplId <- argumentId.Substring(0,argumentId.Length-1) // argument id without the "." at the end
    | Ast.RefArgumentIdentifier((pos1, pos2), argumentId) -> 
        let fv = heap.Eval.PeekEvalStack()
        match fv.Name with 
        | PrimJIByProofArgument -> fv.FplId <- $"{fv.FplId}:{argumentId}"
        | PrimArgInfRevoke -> 
            let fvAi = fv :?> FplArgInferenceRevoke
            let arg = fvAi.ParentArgument
            let proof = arg.ParentProof
            if argumentId = arg.FplId then 
                // revokes its own argument
                fv.ErrorOccurred <- emitPR015Diagnostics argumentId pos1 pos2
            elif proof.HasArgument argumentId then 
                let refArg = proof.Scope[argumentId] :?> FplArgument
                let aiOpt = refArg.ArgumentInference
                match aiOpt with
                | Some (:? FplArgInferenceAssume as toBeRevoked) -> 
                    match heap.ValidStmtStore.LastAssumedArgument with 
                    | Some (:? FplArgInferenceAssume as last) when last = toBeRevoked -> 
                        heap.ValidStmtStore.RevokeLastArgument() 
                    | Some (:? FplArgInferenceAssume as last) when last <> toBeRevoked -> 
                        let lastArg = last.ParentArgument
                        fv.ErrorOccurred <- emitPR016Diagnostics argumentId lastArg.FplId pos1 pos2
                    | _ ->    
                        // the referenced argument is not an assumption in the proof
                        fv.ErrorOccurred <- emitPR015Diagnostics argumentId pos1 pos2
                | _ -> 
                    // the referenced argument is not an assumption in the proof
                    fv.ErrorOccurred <- emitPR015Diagnostics argumentId pos1 pos2
            else
                fv.ErrorOccurred <- emitPR005Diagnostics argumentId pos1 pos2
            fvAi.FplId <- argumentId
        | PrimJustificationL -> 
            let fvAi = new FplJustificationItemByRefArgument((pos1, pos2), fv)
            fvAi.FplId <- argumentId
            let just = fvAi.ParentJustification
            let arg = just.ParentArgument
            let proof = arg.ParentProof
            if not (proof.HasArgument argumentId) then
                fv.ErrorOccurred <- emitPR005Diagnostics argumentId pos1 pos2
            heap.Eval.PushEvalStack(fvAi)
            heap.Eval.PopEvalStack()
        | _ -> ()
    | Ast.Prefix((pos1, pos2), symbol) -> 
        let fv = heap.Eval.PeekEvalStack()
        fv.ExpressionType <- FixType.Prefix symbol
    | Ast.Infix((pos1, pos2), (symbol, precedenceAsts)) -> 
        let fv = heap.Eval.PeekEvalStack()
        eval st precedenceAsts
        fv.ExpressionType <- FixType.Infix (symbol, fv.AuxiliaryInfo)
    | Ast.Postfix((pos1, pos2), symbol) -> 
        let fv = heap.Eval.PeekEvalStack()
        fv.ExpressionType <- FixType.Postfix symbol
    | Ast.Symbol((pos1, pos2), symbol) -> 
        let fv = heap.Eval.PeekEvalStack()
        fv.ExpressionType <- FixType.Symbol symbol
    | Ast.InfixOperator((pos1, pos2), symbol) -> 
        let fv = heap.Eval.PeekEvalStack()
        fv.FplId <- symbol
        fv.TypeId <- symbol
        fv.StartPos <- pos1
        fv.EndPos <- pos2
        checkSIG01Diagnostics fv
    | Ast.PostfixOperator((pos1, pos2), symbol) -> 
        let fv = heap.Eval.PeekEvalStack()
        fv.FplId <- symbol
        fv.TypeId <- symbol
        fv.StartPos <- pos1
        fv.EndPos <- pos2
        checkSIG01Diagnostics fv
    | Ast.PrefixOperator((pos1, pos2), symbol) -> 
        let fv = heap.Eval.PeekEvalStack()
        fv.FplId <- symbol
        fv.TypeId <- symbol
        fv.StartPos <- pos1
        fv.EndPos <- pos2
        checkSIG01Diagnostics fv
    | Ast.Self((pos1, pos2), _) -> 
        let parent = heap.Eval.PeekEvalStack()
        let fv = new FplSelf((pos1, pos2), parent)
        match fv.SelfBlock with
        | ScopeSearchResult.Found block ->
            fv.RefersTo <- Some block
        | _ -> ()
        heap.Eval.PushEvalStack(fv)
        heap.Eval.PopEvalStack()
    | Ast.Parent((pos1, pos2), _) -> 
        let parent = heap.Eval.PeekEvalStack()
        let fv = new FplParent((pos1, pos2), parent)
        match fv.ParentBlock with
        | ScopeSearchResult.Found block ->
            fv.RefersTo <- Some block
        | _ -> ()
        heap.Eval.PushEvalStack(fv)
        heap.Eval.PopEvalStack()
    | Ast.True((pos1, pos2), _) -> 
        let fv = heap.Eval.PeekEvalStack()
        let value = new FplIntrinsicPred((pos1, pos2), fv)
        value.FplId <- LiteralTrue
        heap.Eval.PushEvalStack(value)
        heap.Eval.PopEvalStack()
    | Ast.False((pos1, pos2), _) -> 
        let fv = heap.Eval.PeekEvalStack()
        let value = new FplIntrinsicPred((pos1, pos2), fv)
        value.StartPos <- pos1
        value.EndPos <- pos2
        value.FplId <- LiteralFalse
        value.TypeId <- LiteralPred
        heap.Eval.PushEvalStack(value)
        heap.Eval.PopEvalStack()
    | Ast.Undefined((pos1, pos2), _) -> 
        let fv = heap.Eval.PeekEvalStack()
        let fvNew = new FplIntrinsicUndef((pos1, pos2), fv)
        heap.Eval.PushEvalStack(fvNew)
        heap.Eval.PopEvalStack()
    | Ast.Trivial((pos1, pos2), _) -> 
        let fv = heap.Eval.PeekEvalStack()
        let refBlock = new FplArgInferenceTrivial((pos1, pos2), fv) 
        heap.Eval.PushEvalStack(refBlock)
        heap.Eval.PopEvalStack()
    | Ast.Qed((pos1, pos2), _) -> ()
    | Ast.RuleOfInferenceSignature((pos1, pos2), simpleSignatureAst) ->
        heap.Helper.InSignatureEvaluation <- true
        eval st simpleSignatureAst
        setSignaturePositions pos1 pos2
        heap.Helper.InSignatureEvaluation <- false
    | Ast.RuleOfInference((pos1, pos2), (signatureAst, premiseConclusionBlockAst)) ->
        let parent = heap.Eval.PeekEvalStack()
        let fv = new FplRuleOfInference((pos1, pos2), parent, heap.Helper.GetNextAvailableFplBlockRunOrder)
        heap.Eval.PushEvalStack(fv)
        eval st signatureAst
        eval st premiseConclusionBlockAst
        heap.Eval.PopEvalStack() 
    | Ast.Mapping((pos1, pos2), variableTypeAst) ->
        let fv = heap.Eval.PeekEvalStack()
        let map = new FplMapping((pos1, pos2), fv)
        heap.Eval.PushEvalStack(map)
        eval st variableTypeAst
        heap.Eval.PopEvalStack()
    | Ast.ClassIdentifier((pos1, pos2), ast1) ->
        eval st ast1
        let fv = heap.Eval.PeekEvalStack()
        fv.EndPos <- pos2
    | Ast.Extension((pos1, pos2), extensionString) ->
        let fv = heap.Eval.PeekEvalStack()
        let fplNew = new FplExtensionObj((pos1,pos2), fv)
        heap.Eval.PushEvalStack(fplNew)
        fplNew.FplId <- extensionString
        heap.Eval.PopEvalStack()
    | Ast.UsesClause((pos1, pos2), ast1) ->
        eval st ast1
    | Ast.Not((pos1, pos2), predicateAst) ->
        let fv = heap.Eval.PeekEvalStack()
        let fvNew = new FplNegation((pos1, pos2), fv)
        heap.Eval.PushEvalStack(fvNew)
        eval st predicateAst
        heap.Eval.PopEvalStack()
    | Ast.InEntity((pos1, pos2), inDomainAst) ->
        let forStmt = heap.Eval.PeekEvalStack()
        let inDomain = new FplForInStmtDomain((pos1,pos2), forStmt)
        heap.Eval.PushEvalStack(inDomain) // add ForInStmtDomain
        eval st inDomainAst
        heap.Eval.PopEvalStack() // remove ForInStmtDomain
    | Ast.Assertion((pos1, pos2), predicateAst) ->
        let fv = heap.Eval.PeekEvalStack()
        let fvNew = new FplAssertion((pos1, pos2), fv)
        heap.Eval.PushEvalStack(fvNew)
        eval st predicateAst
        heap.Eval.PopEvalStack()
    | Ast.DottedPredicate((pos1, pos2), predicateWithOptSpecificationAst) ->
        let fv = heap.Eval.PeekEvalStack()
        let refBlock = new FplReference((pos1, pos2), fv) 
        match fv with 
        | :? FplReference as ref ->
            ref.DottedChild <- Some refBlock
        | _ -> ()
        heap.Eval.PushEvalStack(refBlock)
        eval st predicateWithOptSpecificationAst
        heap.Eval.PopEvalStack()
    | Ast.Return((pos1, pos2), returneeAst) ->
        let fv = heap.Eval.PeekEvalStack()
        let stmt = new FplReturn((pos1,pos2), fv)
        heap.Eval.PushEvalStack(stmt)
        eval st returneeAst
        heap.Eval.PopEvalStack() 
    | Ast.AssumeArgument((pos1, pos2), predicateAst) ->
        let fv = heap.Eval.PeekEvalStack()
        let fvNew = new FplArgInferenceAssume((pos1, pos2), fv) 
        heap.Eval.PushEvalStack(fvNew)
        eval st predicateAst
        heap.Eval.PopEvalStack()
        heap.ValidStmtStore.AssumeArgument fvNew
    | Ast.RevokeArgument((pos1, pos2), predicateAst) ->
        let fv = heap.Eval.PeekEvalStack()
        let argInf = new FplArgInferenceRevoke((pos1, pos2), fv) 
        heap.Eval.PushEvalStack(argInf)
        eval st predicateAst
        heap.Eval.PopEvalStack()
    | Ast.ByDef((pos1, pos2), variableAst) ->
        let parent = heap.Eval.PeekEvalStack()
        let fvJi = new FplJustificationItemByDefVar((pos1, pos2), parent)
        heap.Eval.PushEvalStack(fvJi)
        eval st variableAst
        heap.Eval.PopEvalStack()
    | Ast.AST((pos1, pos2), ast1) ->
        eval st ast1

    | Ast.ParamTuple((pos1, pos2), namedVariableDeclarationListAsts) ->
        let fv = heap.Eval.PeekEvalStack()
        fv.ArgType <- ArgType.Parentheses
        namedVariableDeclarationListAsts |> List.map (fun child ->
            match child with 
            | Ast.NamedVarDecl(_,(varList,_)) -> fv.Arity <- fv.Arity + varList.Length
            | _ -> ()
            eval st child
        ) |> ignore
    | Ast.NamespaceIdentifier((pos1, pos2), asts) ->
        asts |> List.map (eval st) |> ignore
    | Ast.TranslationTerm((pos1, pos2), asts) ->
        let fv = heap.Eval.PeekEvalStack()
        asts |> List.map (fun ebnfTerm ->
            let trsl = new FplTranslation((pos1, pos2), fv)
            heap.Eval.PushEvalStack(trsl)
            eval st ebnfTerm
            heap.Eval.PopEvalStack()
        ) |> ignore
    | Ast.TranslationTermList((pos1, pos2), ebnfTermAsts) ->
        let chooseRandomMember (lst: Ast list) =
            let rnd = Random()
            let index = rnd.Next(lst.Length)
            lst.[index]
        eval st (chooseRandomMember ebnfTermAsts)
    | Ast.BrackedCoordList((pos1, pos2), coordListAst) ->
        let getProceedingReference = heap.Eval.GetProceedingReference()

        match getProceedingReference with 
        | Some ref -> 
            ref.ArgType <- ArgType.Brackets
            if coordListAst.Length > 0 then 
                coordListAst 
                |> List.iter (fun pred -> 
                    let ref = new FplReference((pos1, pos2), ref)
                    heap.Eval.PushEvalStack(ref)
                    eval st pred
                    heap.Eval.PopEvalStack()
                ) 
        | _ -> ()
    | Ast.And((pos1, pos2), (predicateAst1, predicateAst2)) ->
        let fv = heap.Eval.PeekEvalStack()
        let fvNew = new FplConjunction((pos1, pos2), fv)
        heap.Eval.PushEvalStack(fvNew)
        eval st predicateAst1
        eval st predicateAst2
        heap.Eval.PopEvalStack()
    | Ast.Or((pos1, pos2), (predicateAst1, predicateAst2)) ->
        let fv = heap.Eval.PeekEvalStack()
        let fvNew = new FplDisjunction((pos1, pos2), fv)
        heap.Eval.PushEvalStack(fvNew)
        eval st predicateAst1
        eval st predicateAst2
        heap.Eval.PopEvalStack()
    | Ast.Xor((pos1, pos2), (predicateAst1, predicateAst2)) ->
        let fv = heap.Eval.PeekEvalStack()
        let fvNew = new FplExclusiveOr((pos1, pos2), fv)
        heap.Eval.PushEvalStack(fvNew)
        eval st predicateAst1
        eval st predicateAst2
        heap.Eval.PopEvalStack()
    | Ast.VarDeclBlock((pos1, pos2), varDeclOrStmtAstList) ->
        varDeclOrStmtAstList 
        |> List.map (fun subAst -> eval st subAst) |> ignore
    | Ast.StatementList((pos1, pos2), asts) ->
        asts |> List.map (eval st) |> ignore
    | Ast.PremiseList((pos1, pos2), predicateListAsts) ->
        let parent = heap.Eval.PeekEvalStack()
        let fv = new FplPredicateList((pos1, pos2), parent, heap.Helper.GetNextAvailableFplBlockRunOrder) 
        heap.Eval.PushEvalStack(fv)
        predicateListAsts |> List.map (eval st) |> ignore
        heap.Eval.PopEvalStack()
    | Ast.JustificationItem((pos1, pos2), justificationReferenceAst) ->
        eval st justificationReferenceAst 
    | Ast.Justification((pos1, pos2), justificationItemAsts) ->
        let fv = heap.Eval.PeekEvalStack()
        let just = new FplJustification((pos1, pos2), fv) 
        heap.Eval.PushEvalStack(just)
        justificationItemAsts |> List.map (eval st) |> ignore
        heap.Eval.PopEvalStack()
    | Ast.ArgumentTuple((pos1, pos2), predicateListAst) ->
        let next = heap.Eval.PeekEvalStack()
        let consumeArgumentsWithParent (parent:FplGenericNode) =
            if predicateListAst.Length > 0 then 
                predicateListAst 
                |> List.iter (fun pred -> 
                    let ref = new FplReference((pos1, pos2), parent)
                    heap.Eval.PushEvalStack(ref)
                    eval st pred
                    heap.Eval.PopEvalStack()
                )
        
        let getProceedingReference = heap.Eval.GetProceedingReference()
            
        match next with 
        | :? FplEquality 
        | :? FplDecrement
        | :? FplBaseConstructorCall -> 
            consumeArgumentsWithParent next
        | _ -> 
            match getProceedingReference with 
            | Some ref ->
                ref.ArgType <- ArgType.Parentheses
                consumeArgumentsWithParent ref
            | _ -> ()
    | Ast.QualificationList((pos1, pos2), asts) ->
        asts |> List.map (eval st) |> ignore
    | Ast.Namespace(asts) ->
        asts |> List.map (eval st) |> ignore
    | Ast.CompoundFunctionalTermType((pos1, pos2), (ast1, astTupleOption)) ->
        eval st ast1
        match astTupleOption with 
        | Some (ast2, _) -> eval st ast2 |> ignore
        | _ -> ()
        match astTupleOption with 
        | Some (_, ast3) -> eval st ast3 |> ignore
        | _ -> ()
    | Ast.AliasedNamespaceIdentifier((pos1, pos2), (ast1, optAst)) ->
        eval st ast1
        optAst |> Option.map (eval st) |> ignore
    | Ast.CompoundPredicateType((pos1, pos2), (ast1, optAst)) ->
        eval st ast1
        optAst |> Option.map (eval st) |> ignore
    | Ast.ReferenceToProofOrCorollary((pos1, pos2), (referencingIdentifierAst)) ->
        heap.Helper.InReferenceToProofOrCorollary <- true
        eval st referencingIdentifierAst
        heap.Helper.InReferenceToProofOrCorollary <- false
    | Ast.PredicateWithOptSpecification((pos1, pos2), (fplIdentifierAst, optionalSpecificationAst)) ->
        let fv = heap.Eval.PeekEvalStack()
        let searchForCandidatesOfReferenceBlock (refBlock:FplGenericNode) = 
            let candidatesFromTheory = findCandidatesByName fv refBlock.FplId true false
            let candidatesFromPropertyScope = findPropertyCandidatesByNameInBlock refBlock refBlock.FplId
            let candidatesFromDottedQualification = findCandidatesByNameInDotted refBlock refBlock.FplId
            candidatesFromTheory  
            @ candidatesFromPropertyScope 
            @ candidatesFromDottedQualification

        /// parentFv is a dotted reference 
        let getCandidatesBasedOnDottedParent (parentFv: FplGenericNode) = 
            let referencedNodeOpt, typeRefNode, typeNameRefNode =
                match parentFv.RefersTo with 
                | Some parentFvRefersTo ->
                    match parentFvRefersTo with 
                    | :? FplFunctionalTerm 
                    | :? FplPredicate 
                    | :? FplClass -> Some parentFvRefersTo, parentFvRefersTo.Type SignatureType.Mixed, parentFvRefersTo.Name
                    | _ -> 
                        let refNodeOpt = parentFvRefersTo.RefersTo
                        match refNodeOpt with 
                        | Some refNode -> refNodeOpt, refNode.Type SignatureType.Mixed, refNode.Name
                        | None -> None, $"{parentFv.FplId}:{LiteralUndef}", parentFv.Name
                | None ->
                    None, $"{parentFv.FplId}:{LiteralUndef}", parentFv.Name
            let candidatesPre = 
                match referencedNodeOpt with 
                | Some referencedNode ->
                    referencedNode.GetVariables() @ referencedNode.GetProperties() 
                | _ -> []
            match box parentFv with
            | :? IHasDotted as pDotted when pDotted.DottedChild.IsSome -> 
                let dottedChild = pDotted.DottedChild.Value
                typeRefNode, typeNameRefNode, filterCandidates candidatesPre dottedChild.FplId false
            | _ -> typeRefNode, typeNameRefNode, ([], "") // empty candidates list and name

        let parentFv = fv.Parent.Value
        match optionalSpecificationAst, box parentFv with
        | Some specificationAst, (:? IHasDotted as pDotted) when pDotted.DottedChild.IsSome -> 
            eval st fplIdentifierAst
            eval st specificationAst |> ignore
            let typeRefNode, typeNameRefNode, (candidates, candidatesNames) = getCandidatesBasedOnDottedParent parentFv 
            if candidates.Length = 0 then 
                fv.ErrorOccurred <- emitID012Diagnostics (fv.Type SignatureType.Mixed) typeNameRefNode typeRefNode candidatesNames pos1 pos2
            else
                match checkSIG04Diagnostics fv candidates with
                | Some matchedCandidate -> fv.RefersTo <- Some matchedCandidate
                | _ -> ()

        | Some specificationAst, _ -> 
            let node = new FplReference((pos1, pos2), fv) 
            heap.Eval.PushEvalStack(node)
            eval st fplIdentifierAst
            eval st specificationAst |> ignore
            
            let candidates = 
                if checkStartsWithLowerCase node.FplId then
                    // match the signatures of small-letter entities (like the self or parent entity, or variables with arguments) 
                    // with their declared types 
                    match node.RefersTo with
                    | Some ref ->
                        match ref.Name, ref.RefersTo with
                        // the candidate from FplSelf is the block it points to (if any)
                        | LiteralSelf, Some fplBlock -> [fplBlock]
                        | LiteralSelf, None -> []
                        // the candidate from FplParent is the block it points to (if any)
                        | LiteralParent, Some fplBlock -> [fplBlock]
                        | LiteralParent, None -> []
                        // the candidate from FplVariable is the block it points to (if any)
                        | PrimVariableL, Some fplBlock -> [fplBlock]
                        | _, _ -> [ref]
                    | None -> []
                else
                    searchForCandidatesOfReferenceBlock node
            if candidates.Length = 1 && candidates.Head.Name = PrimVariableArrayL then
                let candidate = candidates.Head
                node.RefersTo <- Some candidate 
                checkSIG08_SIG10Diagnostics node
            else
                match checkSIG04Diagnostics node candidates with
                | Some matchedCandidate -> 
                    match node.RefersTo with
                    | Some self when self.Name = LiteralSelf && self.RefersTo.IsSome && Object.ReferenceEquals(self.RefersTo.Value, matchedCandidate) ->
                        () // omit replacing node.RefersTo if it refers to FplSelf and FplSelf already refers to the matchedCandidate
                    | Some parent when parent.Name = LiteralParent && parent.RefersTo.IsSome && Object.ReferenceEquals(parent.RefersTo.Value, matchedCandidate) ->
                        () // omit replacing node.RefersTo if it refers to FplParent and FplParent already refers to the matchedCandidate
                    | _ ->
                        node.RefersTo <- Some matchedCandidate
                | _ -> ()

            heap.Eval.PopEvalStack()
        | None, (:? IHasDotted as pDotted) when pDotted.DottedChild.IsSome -> 
            eval st fplIdentifierAst
            let typeRefNode, typeNameRefNode, (candidates, candidatesNames) = getCandidatesBasedOnDottedParent parentFv
            if candidates.Length = 0 then 
                fv.ErrorOccurred <- emitID012Diagnostics (fv.Type SignatureType.Mixed) typeNameRefNode typeRefNode candidatesNames pos1 pos2
            else
                fv.RefersTo <- Some candidates.Head 
        | None, _ -> 
            // if no specification was found then simply continue in the same context
            eval st fplIdentifierAst
            let node = fv.UltimateBlockNode.Value
            // make sure, we still add a referenced node candidate to the scope of a reference
            let candidates = searchForCandidatesOfReferenceBlock fv
            let classes = candidates |> List.filter (fun c -> c.Name = PrimClassL)
            let constructors = candidates |> List.filter (fun c -> c.Name = LiteralCtorL) 
            if constructors.Length > 0 then
                // if among the candidates are class constructors (that due to the FPL syntax always have a signature with 0 or more parameters)
                // we check if to issue a SIG04 diagnostic. At this AST case, a class was referred with a PascalCaseIdentifier 
                // without parentheses. This will only be accepted by the interpreter (without SIG04), if there is
                // a parameterless constructor. In other words, referring a class without parentheses is only allowed
                // if the class is intrinsic (has no constructors) or has a parameterless constructor.
                match checkSIG04Diagnostics fv constructors with
                | Some matchedCandidate -> 
                    // add a parameterless constructor (if such exists)
                     fv.RefersTo <- Some matchedCandidate 
                | _ -> ()
            elif classes.Length > 0 && constructors.Length = 0 then
                // add the class (intrinsic case, no constructors at all)
                let candidate = classes.Head
                fv.RefersTo <- Some candidate
                fv.ErrorOccurred <- checkID025Diagnostics (qualifiedName candidate false) node.Name fv.StartPos fv.EndPos
            elif candidates.Length > 0 then
                // not a class was referred, add the candidate (e.g., referenced variable)
                let candidate = candidates.Head
                fv.FplId <- candidate.FplId 
                fv.RefersTo <- Some candidate
                fv.ErrorOccurred <- checkID025Diagnostics (qualifiedName candidate false) node.Name fv.StartPos fv.EndPos
            else
                ()
        simplifyTriviallyNestedExpressions fv
    | Ast.SelfOrParent((pos1, pos2), selforParentAst) -> 
        eval st selforParentAst
    | Ast.Language((pos1, pos2),(langCode, ebnfAst)) ->
        let fv = heap.Eval.PeekEvalStack()
        let lang = new FplLanguage((pos1, pos2), fv) 
        heap.Eval.PushEvalStack(lang)
        eval st langCode
        eval st ebnfAst
        heap.Eval.PopEvalStack() // remove language
    | Ast.InheritedPredicateTypeList inheritedTypeAsts 
    | Ast.InheritedFunctionalTypeList inheritedTypeAsts 
    | Ast.InheritedClassTypeList inheritedTypeAsts -> 
        let beingCreatedNode = heap.Eval.PeekEvalStack()
        let addVariablesAndPropertiesOfBaseNode (bNode:FplGenericNode) = 
            match box beingCreatedNode with
            | :? FplGenericInheriting as inheritingNode -> 
                inheritingNode.InheritVariables bNode
                inheritingNode.InheritProperties bNode
            | _ -> ()

        inheritedTypeAsts
        |> List.iter (fun baseAst ->
            match baseAst with
            | Ast.PredicateIdentifier((pos1, pos2), _) ->
                // retrieve the name of the class and the class (if it exists)
                let baseNode = new FplBase((pos1, pos2), beingCreatedNode)
                heap.Eval.PushEvalStack(baseNode)            
                eval st baseAst
                heap.Eval.PopEvalStack() |> ignore
                let candidates = findCandidatesByName baseNode baseNode.FplId false true
                if candidates.Length > 0 then 
                    let foundBase = candidates.Head
                    match beingCreatedNode, foundBase with
                    | :? FplPredicate, :? FplPredicate 
                    | :? FplFunctionalTerm, :? FplFunctionalTerm ->
                        let nodeType = beingCreatedNode.Type SignatureType.Type
                        let baseType = foundBase.Type SignatureType.Type
                        if nodeType <> baseType then 
                            baseNode.ErrorOccurred <- emitID007diagnostics beingCreatedNode.Name nodeType foundBase.Name baseType pos1 pos2
                        else 
                            baseNode.RefersTo <- Some foundBase // add found base class to base
                            addVariablesAndPropertiesOfBaseNode foundBase
                    | :? FplClass, :? FplClass -> 
                        baseNode.RefersTo <- Some foundBase // add found base class to base
                        addVariablesAndPropertiesOfBaseNode foundBase
                    | :? FplPredicate, _
                    | :? FplFunctionalTerm, _
                    | :? FplClass, _ ->
                        let nodeType = beingCreatedNode.Type SignatureType.Type
                        let baseType = foundBase.Type SignatureType.Type
                        baseNode.ErrorOccurred <- emitID007diagnostics beingCreatedNode.Name nodeType foundBase.Name baseType pos1 pos2
                    | _ -> () // does not occur, since syntax of inherited base is not supported from non-classes, non-functional terms, and non-predicates
                else
                    baseNode.ErrorOccurred <- emitID010Diagnostics baseNode.FplId pos1 pos2
                if baseNode.FplId = beingCreatedNode.FplId then 
                    baseNode.ErrorOccurred <- emitID009Diagnostics baseNode.FplId pos1 pos2
            | _ -> ()
        )
        let classInheritanceChains = findInheritanceChains beingCreatedNode 
        classInheritanceChains
        |> Seq.filter (fun kvp -> kvp.Value <> "ok")
        |> Seq.iter (fun kvp -> 
            beingCreatedNode.ErrorOccurred <- emitID011Diagnostics kvp.Key kvp.Value beingCreatedNode.StartPos beingCreatedNode.EndPos
        )
    | Ast.ExtensionAssignment((pos1, pos2), (varAst, extensionRegexAst)) ->
        heap.Helper.InSignatureEvaluation <- true
        eval st varAst
        eval st extensionRegexAst
        heap.Helper.InSignatureEvaluation <- false
    | Ast.ExtensionSignature((pos1, pos2), (extensionAssignmentAst, extensionMappingAst)) ->
        eval st extensionAssignmentAst
        eval st extensionMappingAst
    | Ast.DefinitionExtension((pos1, pos2), ((extensionNameAst,extensionSignatureAst), extensionTermAst)) ->
        let parent = heap.Eval.PeekEvalStack()
        let fv = new FplExtension((pos1,pos2), parent, heap.Helper.GetNextAvailableFplBlockRunOrder)
        heap.Eval.PushEvalStack(fv)
        eval st extensionNameAst
        eval st extensionSignatureAst
        eval st extensionTermAst
        heap.Eval.PopEvalStack()
    | Ast.Impl((pos1, pos2), (predicateAst1, predicateAst2)) ->
        let fv = heap.Eval.PeekEvalStack()
        let fvNew = new FplImplication((pos1, pos2), fv)
        heap.Eval.PushEvalStack(fvNew)
        eval st predicateAst1
        eval st predicateAst2
        heap.Eval.PopEvalStack()
        
    | Ast.Iif((pos1, pos2), (predicateAst1, predicateAst2)) ->
        let fv = heap.Eval.PeekEvalStack()
        let fvNew = new FplEquivalence((pos1, pos2), fv)
        heap.Eval.PushEvalStack(fvNew)
        eval st predicateAst1
        eval st predicateAst2
        heap.Eval.PopEvalStack()
    | Ast.IsOperator((pos1, pos2), (isOpArgAst, variableTypeAst)) ->
        let fv = heap.Eval.PeekEvalStack()
        let fvNew = new FplIsOperator((pos1, pos2), fv)
        heap.Eval.PushEvalStack(fvNew)
        let operand = new FplReference((pos1, pos2), fvNew) 
        heap.Eval.PushEvalStack(operand)
        eval st isOpArgAst
        heap.Eval.PopEvalStack()
        let typeOfOperand = new FplMapping((pos1, pos2), fvNew) 
        heap.Eval.PushEvalStack(typeOfOperand)
        eval st variableTypeAst
        heap.Eval.PopEvalStack()
        heap.Eval.PopEvalStack()
    | Ast.Delegate((pos1, pos2), (delegateId, argumentTupleAst)) ->
        let fv = heap.Eval.PeekEvalStack()
        match delegateId with 
        | PrimDelegateEqualL -> 
            let deleg = new FplEquality(delegateId, (pos1, pos2), fv)
            heap.Eval.PushEvalStack(deleg)
            eval st argumentTupleAst
            heap.Eval.PopEvalStack()
        | PrimDelegateDecrementL -> 
            let deleg = new FplDecrement(delegateId, (pos1, pos2), fv)
            heap.Eval.PushEvalStack(deleg)
            eval st argumentTupleAst
            heap.Eval.PopEvalStack()
        | _ -> 
            let deleg = new FplReference((pos1, pos2), fv)
            deleg.FplId <- delegateId
            deleg.TypeId <- delegateId
            heap.Eval.PushEvalStack(deleg)
            eval st argumentTupleAst
            heap.Eval.PopEvalStack()
            deleg.ErrorOccurred <- emitID013Diagnostics $"Unknown delegate `{delegateId}`" pos1 pos2
    | Ast.PredicateSignature(((pos1, pos2), ((simpleSignatureAst, inhPredicateTypeListAstsOpt), paramTupleAst)), optUserDefinedSymbolAst) -> 
        ()
        // empty since the pattern will be matched in DefinitionPredicagte 
        // we list it her to remove FS0025 incomplete pattern warnings
    | Ast.ReferencingIdentifier((pos1, pos2), (predicateIdentifierAst, dollarDigitListAsts)) ->
        dollarDigitListAsts |> List.map (eval st) |> ignore
        eval st predicateIdentifierAst
        let fv = heap.Eval.PeekEvalStack()
        match fv with 
        | :? FplReference ->
            let candidates = findCandidatesByName fv fv.FplId false true
            if candidates.Length > 0 then 
                let candidate = candidates.Head
                fv.RefersTo <- Some candidate
                match fv.UltimateBlockNode with
                | Some block ->
                    fv.ErrorOccurred <- checkID025Diagnostics (qualifiedName candidate false) block.Name fv.StartPos fv.EndPos
                | _ -> ()
        | _ -> ()
    | ProofSignature((pos1, pos2), (simpleSignatureAst, dollarDigitListAsts)) ->
        heap.Helper.InSignatureEvaluation <- true
        eval st simpleSignatureAst
        dollarDigitListAsts |> List.map (eval st) |> ignore
        setSignaturePositions pos1 pos2
        heap.Helper.InSignatureEvaluation <- false
    | Ast.Localization(((pos1, pos2), predicateAst), translationListAsts) ->
        let parent = heap.Eval.PeekEvalStack()
        let fv = new FplLocalization((pos1, pos2), parent, heap.Helper.GetNextAvailableFplBlockRunOrder)
        let var04List = List<KeyValuePair<string, Positions>>()
        heap.Eval.PushEvalStack(fv)
        heap.Helper.InSignatureEvaluation <- true
        eval st predicateAst
        heap.Helper.InSignatureEvaluation <- false
        translationListAsts |> List.map (fun subAst -> 
            eval st subAst
            let vars = fv.GetVariables()
            vars
            |> List.map (fun var -> var :?> FplGenericVariable)
            |> List.filter (fun var -> not var.IsUsed)
            |> List.map (fun var ->
                let loc = heap.Eval.PeekEvalStack()
                let languageList = 
                    loc.Scope 
                    |> Seq.filter (fun kvp -> isLanguage kvp.Value) 
                    |> Seq.map (fun kvp -> kvp.Value) 
                    |> Seq.toList 
                    |> List.rev
                if not languageList.IsEmpty then
                    let lan = languageList.Head
                    let kvp = KeyValuePair(var.FplId,(lan.StartPos, lan.EndPos))
                    var04List.Add kvp
            )
        ) |> ignore
        heap.Eval.PopEvalStack()
        var04List
        |> Seq.iter (fun kvp -> 
            fv.ErrorOccurred <- emitVAR04diagnostics kvp.Key (fst kvp.Value) (snd kvp.Value)
        )
    | Ast.FunctionalTermInstance((pos1, pos2), (functionalTermInstanceSignatureAst, functionalTermInstanceBlockOptAst)) ->
        let parent = heap.Eval.PeekEvalStack()
        let fvNew = new FplMandatoryFunctionalTerm((pos1, pos2), parent)
        heap.Eval.PushEvalStack(fvNew)
        eval st functionalTermInstanceSignatureAst
        match functionalTermInstanceBlockOptAst with 
        | Some functionalTermInstanceBlockAst ->
            eval st functionalTermInstanceBlockAst
        | None -> fvNew.IsIntrinsic <- true
        heap.Eval.PopEvalStack()
    | Ast.All((pos1, pos2), (namedVarDeclAstList, predicateAst)) ->
        let parent = heap.Eval.PeekEvalStack()
        let fv = new FplQuantorAll((pos1, pos2), parent)
        heap.Eval.PushEvalStack(fv) // add all quantor
        fv.Arity <- fv.Arity + (namedVarDeclAstList |> List.length)
        namedVarDeclAstList
        |> List.map (fun namedVarDeclAst ->
            eval st namedVarDeclAst
        )
        |> ignore
        eval st predicateAst
        heap.Eval.PopEvalStack() // remove all quantor
    | Ast.Exists((pos1, pos2), (namedVarDeclAstList, predicateAst)) ->
        let parent = heap.Eval.PeekEvalStack()
        let fv = new FplQuantorExists((pos1, pos2), parent)
        heap.Eval.PushEvalStack(fv) // add exists quantor
        fv.Arity <- fv.Arity + (namedVarDeclAstList |> List.length)
        namedVarDeclAstList
        |> List.map (fun namedVarDeclAst ->
            eval st namedVarDeclAst
        )
        |> ignore
        eval st predicateAst
        heap.Eval.PopEvalStack() // remove exists quantor
    | Ast.ExistsN((pos1, pos2), ((dollarDigitsAst, namedVarDeclListAst), predicateAst)) ->
        let parent = heap.Eval.PeekEvalStack()
        let fv = new FplQuantorExistsN((pos1, pos2), parent)
        heap.Eval.PushEvalStack(fv) // add exists n quantor
        eval st dollarDigitsAst
        namedVarDeclListAst
        |> List.map (fun namedVarDeclAst ->
            eval st namedVarDeclAst
        )
        |> ignore
        eval st predicateAst
        heap.Eval.PopEvalStack() // remove exists n quantor
    | Ast.FunctionalTermSignature(((pos1, pos2), (((simpleSignatureAst, inhFunctionalTypeListAstsOpt), paramTupleAst), mappingAst)), optUserDefinedSymbolAst) -> 
        ()
        // empty since the pattern will be matched in DefinitionFunctionalTerm 
        // we list it her to remove FS0025 incomplete pattern warnings
    | Ast.PredicateWithQualification(predicateWithOptSpecificationAst, qualificationListAst) ->
        eval st predicateWithOptSpecificationAst
        eval st qualificationListAst
    | Ast.InfixOperation((pos1, pos2), separatedPredicateListAst) ->
        let fv = heap.Eval.PeekEvalStack()
        separatedPredicateListAst
        |> List.map (fun (predAst, optOperandAst) -> 
            // evaluate the operand
            let pred = new FplReference((pos1,pos2), fv)
            heap.Eval.PushEvalStack(pred)
            eval st predAst
            fv.ArgList.Add(heap.Eval.Pop()) // pop the stack element (same reference as pred) and store it in a list
            // followed by the operator
            match optOperandAst with
            | Some opAst -> 
                let infixOperator = new FplReference((pos1,pos2), fv)
                heap.Eval.PushEvalStack(infixOperator)
                // evaluate the operator by trying to find a definition for the operator
                eval st opAst
                // store the index of the infix operator, so we still know it after sorting the list by precedence later
                fv.ArgList.Add(heap.Eval.Pop()) // pop the stack element (same reference as infixOperator) and store it in a list
            | None -> () // in this case, we consumed and evaluated all operators in the infix operation (due to FPL parser Ast structure)
        )
        |> ignore

        // If the parsed infix operation ended with an operator (incomplete expression like "(1 =)"),
        // fv.ArgList will have an even count (pred, op) and no following predicate. Emit SY000 and drop the trailing operator
        // so the interpreter doesn't crash and can continue on a best-effort basis.
        if fv.ArgList.Count % 2 = 0 then
            let trailingOp = fv.ArgList.[fv.ArgList.Count - 1]
            // record diagnostic on the trailing operator
            trailingOp.ErrorOccurred <- emitSY000diagnostics trailingOp.FplId fv.EndPos fv.EndPos
            // remove the trailing operator so further processing won't index out of range
            fv.ArgList.RemoveAt(fv.ArgList.Count - 1)

        /// Returns the precedence of fv1 if its ExpressionType is Infix
        /// or Int32.MaxValue otherwise
        let getPrecedence (fv1:FplGenericNode) =
            match fv1.RefersTo with
            | None -> Int32.MaxValue
            | Some x -> 
                match x.ExpressionType with
                |  FixType.Infix (symb, prec) -> prec
                | _ -> Int32.MaxValue

        // This while loop will evaluate multiple non-parenthesized infix operations
        // according to their precedence by grouping them into binary operations and leave fv with only one binary operation
        while fv.ArgList.Count > 1 do
            let mutable currentMinimalPrecedence = Int32.MaxValue
            let mutable currMinIndex = 1
            for i in 1 .. 2 .. fv.ArgList.Count - 1 do
                let currPrecedence = getPrecedence fv.ArgList[i]
                if currentMinimalPrecedence > currPrecedence then
                    currentMinimalPrecedence <- currPrecedence
                    currMinIndex <- i
            let currentOp = fv.ArgList[currMinIndex]
            let firstOp = fv.ArgList[currMinIndex-1]
            let secondOp = fv.ArgList[currMinIndex+1]
            currentOp.ArgList.Add(firstOp)
            currentOp.ArgList.Add(secondOp)
            let refNodeOpt = referencedNodeOpt currentOp
            match refNodeOpt with 
            | Some refNode when refNode.Arity = 2 ->
                let pars = 
                    refNode.GetVariables() 
                    |> List.map (fun var -> var :?> FplGenericVariable)
                    |> List.filter (fun var -> var.IsSignatureVariable)
                // try to issue SIG04 diagnostics per argument of the binary operator
                if pars.Length = 2 then 
                    match mpwa [firstOp] [pars[0]] with
                    | Some errMsg -> 
                        let extendedErrMsg = $"{errMsg} in {qualifiedName refNode true}"
                        firstOp.ErrorOccurred <- emitSIG04Diagnostics (currentOp.Type SignatureType.Mixed) 1 extendedErrMsg firstOp.StartPos firstOp.EndPos
                    | _ -> ()
                    match mpwa [secondOp] [pars[1]] with
                    | Some errMsg -> 
                        let extendedErrMsg = $"{errMsg} in {qualifiedName refNode true}"
                        secondOp.ErrorOccurred <- emitSIG04Diagnostics (currentOp.Type SignatureType.Mixed) 1 extendedErrMsg secondOp.StartPos secondOp.EndPos
                    | _ -> ()
                else
                    // if something went wrong (for instance, wrong arity), issue SIG04 with fallback using the operand 
                    // together with its referenced node
                    checkSIG04Diagnostics currentOp [refNode] |> ignore
            | _ -> ()
            fv.ArgList.RemoveAt(currMinIndex+1) 
            fv.ArgList.RemoveAt(currMinIndex-1) 
        simplifyTriviallyNestedExpressions fv 
    | Ast.Expression((pos1, pos2), ((((prefixOpAst, predicateAst), postfixOpAst), optionalSpecificationAst), qualificationListAst)) ->
        let fv = heap.Eval.PeekEvalStack()
        let refBlock = new FplReference((pos1, pos2), fv) 
        heap.Eval.PushEvalStack(refBlock)
        let ensureReversedPolishNotation = 
            if prefixOpAst.IsSome && postfixOpAst.IsSome then 
                // for heuristic reasons, we choose a precedence of postfix ...
                postfixOpAst |> Option.map (eval st) |> Option.defaultValue () 
                let postfixedInnerPred = new FplReference((pos1,pos2), heap.Eval.PeekEvalStack())
                heap.Eval.PushEvalStack(postfixedInnerPred)
                // ... over prefix notation in mathematics
                prefixOpAst |> Option.map (eval st) |> Option.defaultValue ()
                let prefixedInnerPred = new FplReference((pos1,pos2), heap.Eval.PeekEvalStack())
                heap.Eval.PushEvalStack(prefixedInnerPred)
                eval st predicateAst
                heap.Eval.PopEvalStack()
                heap.Eval.PopEvalStack()
            elif prefixOpAst.IsSome then 
                prefixOpAst |> Option.map (eval st) |> Option.defaultValue ()
                let innerPred = new FplReference((pos1,pos2), heap.Eval.PeekEvalStack())
                heap.Eval.PushEvalStack(innerPred)
                eval st predicateAst
                heap.Eval.PopEvalStack()
            elif postfixOpAst.IsSome then 
                postfixOpAst |> Option.map (eval st) |> Option.defaultValue ()
                let innerPred = new FplReference((pos1,pos2), heap.Eval.PeekEvalStack())
                heap.Eval.PushEvalStack(innerPred)
                eval st predicateAst
                heap.Eval.PopEvalStack()
            else
                eval st predicateAst
        ensureReversedPolishNotation
        optionalSpecificationAst |> Option.map (eval st) |> Option.defaultValue ()
        eval st qualificationListAst
        let refBlock = heap.Eval.PeekEvalStack() // if the reference was replaced, take this one
        refBlock.EndPos <- pos2
        simplifyTriviallyNestedExpressions refBlock 
        heap.Eval.PopEvalStack()
        match fv with 
        | :? FplReference ->
            simplifyTriviallyNestedExpressions fv 
        | _ -> ()
    | Ast.Cases((pos1, pos2), (caseSingleListAsts, caseElseAst)) ->
        let parent = heap.Eval.PeekEvalStack()
        let casesStmt = new FplCases((pos1, pos2), parent)
        heap.Eval.PushEvalStack(casesStmt) // add cases 
        caseSingleListAsts |> List.map (fun caseAst -> eval st caseAst) |> ignore
        eval st caseElseAst
        heap.Eval.PopEvalStack() // remove cases
    | Ast.CaseSingle((pos1, pos2), (predicateAst, statementListAsts)) ->
        let parent = heap.Eval.PeekEvalStack()
        let singleCase = new FplCaseSingle((pos1,pos2), parent)
        heap.Eval.PushEvalStack(singleCase) // add single case
        eval st predicateAst
        statementListAsts |> List.map (eval st) |> ignore
        heap.Eval.PopEvalStack() // remove single case 
    | Ast.CaseElse((pos1, pos2), statementListAsts) ->
        let parent = heap.Eval.PeekEvalStack()
        let elseCase = new FplCaseElse((pos1,pos2), parent)
        heap.Eval.PushEvalStack(elseCase) // add else 
        statementListAsts |> List.map (eval st) |> ignore
        heap.Eval.PopEvalStack() // remove else 
    | Ast.MapCases((pos1, pos2), (mapCaseSingleAstList, elseStatementAst)) ->
        let parent = heap.Eval.PeekEvalStack()
        let fvNew = new FplMapCases((pos1, pos2), parent)
        heap.Eval.PushEvalStack(fvNew) // add mcases
        mapCaseSingleAstList |> List.map (fun caseAst -> eval st caseAst) |> ignore
        eval st elseStatementAst
        heap.Eval.PopEvalStack() // remove mcases
    | Ast.MapCaseSingle((pos1, pos2), (predicateFirstAst, predicateSecondAst)) ->
        let parent = heap.Eval.PeekEvalStack()
        let mapCaseSingle = new FplMapCaseSingle((pos1,pos2), parent)
        heap.Eval.PushEvalStack(mapCaseSingle) // add mcases single
        eval st predicateFirstAst
        eval st predicateSecondAst 
        heap.Eval.PopEvalStack() // remove mcases single
    | Ast.MapCaseElse((pos1, pos2), predicateAst) ->
        let parent = heap.Eval.PeekEvalStack()
        let elseCase = new FplMapCaseElse((pos1,pos2), parent)
        heap.Eval.PushEvalStack(elseCase) // add mcases else
        eval st predicateAst 
        heap.Eval.PopEvalStack() // remove mcases else
    | Ast.FunctionalTermInstanceSignature((pos1, pos2), ((simpleSignatureAst, paramTupleAst), mappingAst)) ->
        heap.Helper.InSignatureEvaluation <- true
        eval st simpleSignatureAst
        eval st paramTupleAst
        heap.Helper.InSignatureEvaluation <- false
        eval st mappingAst
        setSignaturePositions pos1 pos2
    | Ast.PredicateInstanceSignature((pos1, pos2), (simpleSignatureAst, paramTupleAst)) ->
        heap.Helper.InSignatureEvaluation <- true
        eval st simpleSignatureAst
        eval st paramTupleAst
        setSignaturePositions pos1 pos2
        heap.Helper.InSignatureEvaluation <- false
    | Ast.ConstructorSignature((pos1, pos2), (simpleSignatureAst, paramTupleAst)) ->
        heap.Helper.InSignatureEvaluation <- true
        eval st simpleSignatureAst
        eval st paramTupleAst
        setSignaturePositions pos1 pos2
        heap.Helper.InSignatureEvaluation <- false
    | Ast.Assignment((pos1, pos2), (predicateWithQualificationAst, predicateAst)) ->
        let parent = heap.Eval.PeekEvalStack()
        let fvNew = new FplAssignment((pos1, pos2), parent)
        heap.Eval.PushEvalStack(fvNew) // add assignment
        let assigneeReference = 
            match predicateWithQualificationAst with 
            | Ast.PredicateWithQualification(predicateWithOptSpecificationAst, _) ->
                match predicateWithOptSpecificationAst with 
                | Ast.PredicateWithOptSpecification ((assigneePos1,assigneePos2),(_,_)) ->
                    // create assigneeReference with correct positioning of the assignee (to improve related diagnostics positions)
                    new FplReference((assigneePos1,assigneePos2), fvNew)
                | _ ->
                    new FplReference((pos1,pos2), fvNew)
            | _ ->
                new FplReference((pos1,pos2), fvNew)
        heap.Eval.PushEvalStack(assigneeReference) // add assignee
        eval st predicateWithQualificationAst
        heap.Eval.PopEvalStack() // remove assignee
        eval st predicateAst
        heap.Eval.PopEvalStack() // remove Assignment
    | Ast.PredicateInstance((pos1, pos2), (signatureAst, predInstanceBlockAstOpt)) ->
        let parent = heap.Eval.PeekEvalStack()
        let fvNew = new FplMandatoryPredicate((pos1, pos2), parent)
        heap.Eval.PushEvalStack(fvNew)
        eval st signatureAst
        match predInstanceBlockAstOpt with 
        | Some predInstanceBlockAst ->
            eval st predInstanceBlockAst
        | None -> fvNew.IsIntrinsic <- true
        heap.Eval.PopEvalStack()
    | Ast.BaseConstructorCall((pos1, pos2), (inheritedClassTypeAst, argumentTupleAst)) ->
        let parent = heap.Eval.PeekEvalStack()
        let fvNew = new FplBaseConstructorCall((pos1, pos2), parent) 
        heap.Eval.PushEvalStack(fvNew)
        eval st inheritedClassTypeAst
        eval st argumentTupleAst
        heap.Eval.PopEvalStack()
    | Ast.JustArgInf((pos1, pos2), (justificationAst, argumentInferenceAst)) ->
        eval st justificationAst
        eval st argumentInferenceAst
    | Ast.Argument((pos1, pos2), (argIdAst, argAst)) ->
        let fv = heap.Eval.PeekEvalStack()
        let arg = new FplArgument((pos1, pos2), fv, heap.Helper.GetNextAvailableFplBlockRunOrder) 
        heap.Eval.PushEvalStack(arg)
        eval st argIdAst
        eval st argAst
        heap.Eval.PopEvalStack()
    | Ast.ForIn((pos1, pos2), ((entityAst, inDomainAst), statementListAst)) ->
        let parent = heap.Eval.PeekEvalStack()
        let forStmt = new FplForInStmt((pos1, pos2), parent)
        heap.Eval.PushEvalStack(forStmt) // add ForInStmt
        let entity = new FplForInStmtEntity((pos1,pos2), forStmt)
        heap.Eval.PushEvalStack(entity) // add ForInStmtEntity
        eval st entityAst
        heap.Eval.PopEvalStack() // remove ForInStmtEntity
        eval st inDomainAst
        statementListAst |> List.map (fun stmtAst -> eval st stmtAst) |> ignore
        heap.Eval.PopEvalStack() // remove ForInStmt
    | Ast.PremiseConclusionBlock((pos1, pos2), ((optVarDeclOrSpecList, premiseAst), conclusionAst)) ->
        optVarDeclOrSpecList |> Option.map (List.map (eval st) >> ignore) |> Option.defaultValue ()
        eval st premiseAst
        eval st conclusionAst
    | Ast.TheoremSignature((pos1, pos2), simpleSignatureAst) ->
        heap.Helper.InSignatureEvaluation <- true
        eval st simpleSignatureAst
        setSignaturePositions pos1 pos2
        heap.Helper.InSignatureEvaluation <- false
    | Ast.Theorem((pos1, pos2), (signatureAst, (optVarDeclOrSpecList, predicateAst))) ->
        let parent = heap.Eval.PeekEvalStack()
        let fv = new FplTheorem((pos1, pos2), parent, heap.Helper.GetNextAvailableFplBlockRunOrder)
        heap.Eval.PushEvalStack(fv)
        eval st signatureAst
        evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst
        heap.Eval.PopEvalStack()
    | Ast.LemmaSignature((pos1, pos2), simpleSignatureAst) ->
        heap.Helper.InSignatureEvaluation <- true
        eval st simpleSignatureAst
        setSignaturePositions pos1 pos2
        heap.Helper.InSignatureEvaluation <- false
    | Ast.Lemma((pos1, pos2), (signatureAst, (optVarDeclOrSpecList, predicateAst))) ->
        let parent = heap.Eval.PeekEvalStack()
        let fv = new FplLemma((pos1, pos2), parent, heap.Helper.GetNextAvailableFplBlockRunOrder)
        heap.Eval.PushEvalStack(fv)
        eval st signatureAst
        evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst
        heap.Eval.PopEvalStack()
    | Ast.PropositionSignature((pos1, pos2), simpleSignatureAst) ->
        heap.Helper.InSignatureEvaluation <- true
        eval st simpleSignatureAst
        setSignaturePositions pos1 pos2
        heap.Helper.InSignatureEvaluation <- false
    | Ast.Proposition((pos1, pos2), (signatureAst, (optVarDeclOrSpecList, predicateAst))) ->
        let parent = heap.Eval.PeekEvalStack()
        let fv = new FplProposition((pos1, pos2), parent, heap.Helper.GetNextAvailableFplBlockRunOrder)
        heap.Eval.PushEvalStack(fv)
        eval st signatureAst
        evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst
        heap.Eval.PopEvalStack()
    | Ast.ConjectureSignature((pos1, pos2), simpleSignatureAst) ->
        heap.Helper.InSignatureEvaluation <- true
        eval st simpleSignatureAst
        setSignaturePositions pos1 pos2
        heap.Helper.InSignatureEvaluation <- false
    | Ast.Conjecture((pos1, pos2), (signatureAst, (optVarDeclOrSpecList, predicateAst))) ->
        let parent = heap.Eval.PeekEvalStack()
        let fv = new FplConjecture((pos1, pos2), parent, heap.Helper.GetNextAvailableFplBlockRunOrder)
        heap.Eval.PushEvalStack(fv)
        eval st signatureAst
        evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst
        heap.Eval.PopEvalStack()
    | Ast.AxiomSignature((pos1, pos2), simpleSignatureAst) ->
        heap.Helper.InSignatureEvaluation <- true
        eval st simpleSignatureAst
        setSignaturePositions pos1 pos2
        heap.Helper.InSignatureEvaluation <- false
    | Ast.Axiom((pos1, pos2), (signatureAst, (optVarDeclOrSpecList, predicateAst))) ->
        let parent = heap.Eval.PeekEvalStack()
        let fv = new FplAxiom((pos1, pos2), parent, heap.Helper.GetNextAvailableFplBlockRunOrder)
        heap.Eval.PushEvalStack(fv)
        eval st signatureAst
        evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst
        heap.Eval.PopEvalStack()
    | Ast.CorollarySignature((pos1, pos2), (simpleSignatureAst, dollarDigitListAsts)) ->
        heap.Helper.InSignatureEvaluation <- true
        eval st simpleSignatureAst
        dollarDigitListAsts |> List.map (eval st) |> ignore
        setSignaturePositions pos1 pos2
        heap.Helper.InSignatureEvaluation <- false
    | Ast.Corollary((pos1, pos2), (corollarySignatureAst, (optVarDeclOrSpecList, predicateAst))) ->
        let parent = heap.Eval.PeekEvalStack()
        let fv = new FplCorollary((pos1, pos2), parent, heap.Helper.GetNextAvailableFplBlockRunOrder)
        heap.Eval.PushEvalStack(fv)
        eval st corollarySignatureAst
        heap.Eval.PopEvalStack() // add to parent theorem (if any) 
        heap.Eval.PushEvalStack(fv) // push again to have the current corollary on stack
        evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst
        // now, we are ready to emit VAR04 diagnostics for all variables declared in the signature of the corollary.
        fv.CheckConsistency()
        heap.Eval.Pop() |> ignore // pop without 
    | Ast.NamedVarDecl((pos1, pos2), (variableListAst, variableTypeAst)) ->
        let parent = heap.Eval.PeekEvalStack()
        parent.AuxiliaryInfo <- variableListAst |> List.length // remember how many variables to create
        // create all variables of the named variable declaration in the current scope
        variableListAst |> List.iter (fun varAst ->
            match variableTypeAst with 
            | Ast.ArrayType((posMan1, posMan2),(mainTypeAst, indexAllowedTypeListAst)) ->
                parent.ErrorOccurred <- emitVAR00Diagnostics parent.AuxiliaryInfo posMan1 posMan2        
                match varAst with 
                | Ast.Var((varPos1, varPos2), varName) ->
                    let newVar = new FplVariableArray(varName, (varPos1, varPos2), parent)
                    newVar.IsSignatureVariable <- (heap.Helper.InSignatureEvaluation && hasSignature parent)
                    heap.Eval.PushEvalStack(newVar)
                    eval st mainTypeAst
                    indexAllowedTypeListAst |> List.map (eval st) |> ignore
                    heap.Eval.PopEvalStack()
                | _ -> ()
            | Ast.SimpleVariableType((_, _),simplVariableTypeAst) ->
                match varAst with 
                | Ast.Var((varPos1, varPos2), varName) ->
                    let newVar = new FplVariable(varName, (varPos1, varPos2), parent)
                    newVar.IsSignatureVariable <- (heap.Helper.InSignatureEvaluation && hasSignature parent)
                    heap.Eval.PushEvalStack(newVar)
                    eval st simplVariableTypeAst
                    heap.Eval.PopEvalStack()
                | _ -> ()
            | _ -> ()
        ) |> ignore 
    | Ast.ConstructorBlock((pos1, pos2), optVarDeclOrSpecListAst) ->
        let parent = heap.Eval.PeekEvalStack()
        // evaluate the construction block 
        match optVarDeclOrSpecListAst with
        | Some astList -> 
            astList |> List.map (eval st) |> ignore
        | None -> ()
        if parent.ArgList.Count = 0 then
            parent.ErrorOccurred <- emitST002diagnostics parent.Name parent.StartPos parent.EndPos
    | Ast.Constructor((pos1, pos2), (signatureAst, constructorBlockAst)) ->
        let parent = heap.Eval.PeekEvalStack()
        let fv = new FplConstructor((pos1, pos2), parent)
        heap.Eval.PushEvalStack(fv)
        eval st signatureAst
        eval st constructorBlockAst
        heap.Eval.PopEvalStack()
    | Ast.DefPredicateContent(optAsts, ast1) ->
        optAsts
        |> Option.map (List.map (eval st) >> ignore)
        |> Option.defaultValue ()
        |> ignore
        eval st ast1
    | Ast.DefFunctionContent(optAsts, ast1) ->
        optAsts
        |> Option.map (List.map (eval st) >> ignore)
        |> Option.defaultValue ()
        |> ignore
        eval st ast1
    | Ast.DefClassCompleteContent(optVarDeclOrSpecListAsts, constructorListAsts) ->
        optVarDeclOrSpecListAsts |> Option.map (List.map (eval st) >> ignore) |> Option.defaultValue ()
        constructorListAsts |> List.map (eval st) |> ignore
    | Ast.DefinitionPredicate((pos1, pos2), (predicateSignatureAst, optDefBlock)) ->
        let parent = heap.Eval.PeekEvalStack()
        let fv = new FplPredicate((pos1, pos2), parent, heap.Helper.GetNextAvailableFplBlockRunOrder)
        heap.Eval.PushEvalStack(fv)
        match predicateSignatureAst with
        | Ast.PredicateSignature(((pos1, pos2), ((simpleSignatureAst, inhPredicateTypeListAstsOpt), paramTupleAst)), optUserDefinedSymbolAst) ->
            heap.Helper.InSignatureEvaluation <- true
            eval st simpleSignatureAst
            eval st paramTupleAst
            heap.Helper.InSignatureEvaluation <- false
            optUserDefinedSymbolAst |> Option.map (eval st) |> Option.defaultValue () |> ignore
            match optDefBlock with 
            | Some (predicateContentAst, optPropertyListAsts) ->
                eval st predicateContentAst
                optPropertyListAsts |> Option.map (List.map (eval st) >> ignore) |> Option.defaultValue ()
            | None -> fv.IsIntrinsic <- true
            inhPredicateTypeListAstsOpt |> Option.map (eval st) |> Option.defaultValue ()
            setSignaturePositions pos1 pos2
        | _ -> ()
        heap.Eval.PopEvalStack()
    | Ast.FunctionalTermDefinitionBlock((pos1, pos2), optDefBlock) ->
        let functionaTermBlock = heap.Eval.PeekEvalStack()
        match optDefBlock with 
        | Some (funcContentAst, optPropertyListAsts) ->
            eval st funcContentAst
            optPropertyListAsts |> Option.map (List.map (eval st) >> ignore) |> Option.defaultValue ()
            let properties = functionaTermBlock.GetProperties()
            if properties.IsEmpty && functionaTermBlock.ArgList.Count = 1 then
                functionaTermBlock.ErrorOccurred <- emitST001diagnostics functionaTermBlock.Name pos1 pos2
        | None -> functionaTermBlock.IsIntrinsic <- true
    | Ast.DefinitionFunctionalTerm((pos1, pos2), (functionalTermSignatureAst, functionalTermDefBlockAst)) ->
        let parent = heap.Eval.PeekEvalStack()
        let fv = new FplFunctionalTerm((pos1, pos2), parent, heap.Helper.GetNextAvailableFplBlockRunOrder)
        heap.Eval.PushEvalStack(fv)
        match functionalTermSignatureAst with
        | Ast.FunctionalTermSignature(((pos1, pos2), (((simpleSignatureAst, inhFunctionalTypeListAstsOpt), paramTupleAst), mappingAst)), optUserDefinedSymbolAst) -> 
            eval st mappingAst
            heap.Helper.InSignatureEvaluation <- true
            eval st simpleSignatureAst
            eval st paramTupleAst
            heap.Helper.InSignatureEvaluation <- false
            optUserDefinedSymbolAst |> Option.map (eval st) |> Option.defaultValue () 
            eval st functionalTermDefBlockAst
            inhFunctionalTypeListAstsOpt |> Option.map (eval st) |> Option.defaultValue () 
            setSignaturePositions pos1 pos2
        | _ -> ()
        heap.Eval.PopEvalStack()
    | Ast.ClassSignature((pos1, pos2), simpleSignatureAst) ->
        heap.Helper.InSignatureEvaluation <- true
        eval st simpleSignatureAst
        setSignaturePositions pos1 pos2
        heap.Helper.InSignatureEvaluation <- false
    | Ast.ClassDefinitionBlock((pos1, pos2), optDefBlock) ->
        let classBlock = heap.Eval.PeekEvalStack()
        let cl = classBlock :?> FplClass
        match optDefBlock with 
        | Some (classContentAst, optPropertyListAsts) ->
            eval st classContentAst
            optPropertyListAsts |> Option.map (List.map (eval st) >> ignore) |> Option.defaultValue ()
            let properties = cl.GetProperties()
            let constructors = cl.GetConstructors()
            let classContent =  cl.ArgList |> Seq.filter (fun node -> node.Name <> LiteralBase) |> Seq.toList
            if properties.IsEmpty && classContent.Length = 0 && constructors.IsEmpty then
                classBlock.ErrorOccurred <- emitST001diagnostics classBlock.Name pos1 pos2
        | None -> 
            cl.IsIntrinsic <- true
            cl.AddDefaultConstructor()
    | Ast.DefinitionClass((pos1, pos2),(((classSignatureAst, optInheritedClassTypeListAst), optUserDefinedObjSymAst), classBlockAst)) ->
        let parent = heap.Eval.PeekEvalStack()
        let fv = new FplClass((pos1, pos2), parent, heap.Helper.GetNextAvailableFplBlockRunOrder)
        heap.Eval.PushEvalStack(fv)
        eval st classSignatureAst
        optInheritedClassTypeListAst |> Option.map (eval st) |> Option.defaultValue ()
        optUserDefinedObjSymAst |> Option.map (eval st) |> Option.defaultValue ()
        eval st classBlockAst
        heap.Eval.PopEvalStack()
    | Ast.DerivedPredicate ((pos1, pos2),predicateAst) -> 
        let fv = heap.Eval.PeekEvalStack()
        let argInf = new FplArgInferenceDerived((pos1, pos2), fv) 
        heap.Eval.PushEvalStack(argInf)
        eval st predicateAst
        heap.Eval.PopEvalStack()
    | Ast.Proof((pos1, pos2), (referencingIdentifierAst, (proofArgumentListAst, optQedAst))) ->
        let parent = heap.Eval.PeekEvalStack()
        let fv = new FplProof((pos1, pos2), parent, heap.Helper.GetNextAvailableFplBlockRunOrder)
        heap.Eval.PushEvalStack(fv)
        eval st referencingIdentifierAst
        heap.Eval.PopEvalStack() // add to parent theorem (if any)
        heap.Eval.PushEvalStack(fv) // push again
        proofArgumentListAst |> List.map (eval st) |> ignore
        optQedAst |> Option.map (eval st) |> Option.defaultValue ()
        fv.CheckConsistency()
        let value = new FplIntrinsicPred((pos1,pos1), fv)
        value.FplId <- LiteralTrue
        // check if all arguments could be correctly inferred
        fv.OrderedArguments
        |> Seq.iter (fun fv1 -> 
            let argInferenceOpt = fv1.ArgumentInference
            match argInferenceOpt with
            | Some argInference ->
                let argInferenceResult = argInference.Represent()
                match argInferenceResult with
                | LiteralTrue -> ()
                | _ -> value.FplId <- LiteralFalse // TODO all other arguments that are either undetermined or false should issue an error
            | _ -> () // TODO argumentInference not found
        )
        fv.Value <- Some value
        heap.Eval.Pop() |> ignore // pop without embedding in theorem (already done)
    | Ast.Precedence((pos1, pos2), precedence) ->
        let fv = heap.Eval.PeekEvalStack()
        fv.AuxiliaryInfo <- precedence
    | Ast.JustificationIdentifier((pos1, pos2), (((byModifierOption, predicateIdentifierAst), dollarDigitListAsts), refArgumentIdentifierAst)) ->
        let parent = heap.Eval.PeekEvalStack()

        let checkPR001_PR006Diagnostics (fvJi:FplGenericNode) candidates = 
            match tryFindAssociatedBlockForJustificationItem fvJi candidates with
            | ScopeSearchResult.FoundAssociate potentialCandidate -> 
                fvJi.RefersTo <- Some potentialCandidate
                match fvJi with 
                | :? FplJustificationItemByProofArgument as fvJi1 ->
                    let split = fvJi.FplId.Split(":")
                    if split.Length > 1 then 
                        // here, argName is the argument identifier of the other proof
                        let argName = $"{split.[1]}"
                        match getArgumentInProof fvJi1 argName with
                        | Some argument -> fvJi.ArgList.Add(argument) 
                        | _ -> fvJi.ErrorOccurred <- emitPR006Diagnostics fvJi.FplId argName fvJi.StartPos fvJi.EndPos 
                | _ -> ()
            | ScopeSearchResult.FoundIncorrectBlock otherBlock ->
                let alternative = 
                    match fvJi.Name with 
                    | PrimJIByAx ->
                        "Expected a reference to an axiom."
                    | PrimJIByConj ->
                        "Expected a reference to a conjecture."
                    | PrimJIByCor ->
                        "Expected a reference to a corollary."
                    | PrimJIByDef ->
                        "Expected a reference to a definition (of a class, a predicate, or a functional term)."
                    | PrimJIByDefVar ->
                        "Expected a reference to a variable."
                    | PrimJIByInf ->
                        "Expected a reference to a rule of inference."
                    | PrimJIByProofArgument ->
                        "Expected a reference to an argument in another proof."
                    | PrimJIByRefArgument ->
                        "Expected a reference to a previous argument in this proof."
                    | PrimJIByTheoremLikeStmt ->
                        "Expected a reference to a theorem, a lemma, or a proposition."
                    | _ -> "Expected another reference."
                fvJi.ErrorOccurred <- emitPR001Diagnostics (qualifiedName otherBlock false) fvJi.Name fvJi.StartPos fvJi.EndPos alternative
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
            eval st predicateIdentifierAst
            // check, if indeed the predicateId points to an axiom, if not issue diagnostics
            let candidates = findCandidatesByName fvJi fvJi.FplId false false
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
            eval st predicateIdentifierAst
            // check, if indeed the predicateId points to a conjecture, if not issue diagnostics
            let candidates = findCandidatesByName fvJi fvJi.FplId false false
            checkPR001_PR006Diagnostics fvJi candidates
            heap.Eval.PopEvalStack()
        | Some LiteralByCor, Some _, _ -> 
            let fvJi = new FplJustificationItemByCor((pos1, pos2), parent)
            heap.Eval.PushEvalStack(fvJi)
            eval st predicateIdentifierAst
            dollarDigitListAsts.Value |> List.map (eval st) |> ignore
            let candidates = findCandidatesByName fvJi fvJi.FplId false true
            // check, if indeed the predicateId points to a corollary, if not issue diagnostics
            checkPR001_PR006Diagnostics fvJi candidates
            heap.Eval.PopEvalStack()
        | Some LiteralByCor, None, _ -> 
            // byCor justification a reference to a corollary
            parent.ErrorOccurred <- emitPR012Diagnostics pos1 pos2 
        | Some LiteralByDef, Some _, None -> 
            // byDef justification cannot be used together with a proof reference
            parent.ErrorOccurred <- emitPR010Diagnostics LiteralByDef LiteralDefL pos1 pos2 
        | Some LiteralByDef, Some _, Some _ -> 
            // byDef justification cannot be used together with a proof argument reference 
            parent.ErrorOccurred <- emitPR011Diagnostics LiteralByDef LiteralDefL pos1 pos2 
        | Some LiteralByDef, None, None -> 
            let fvJi = new FplJustificationItemByDef((pos1, pos2), parent)
            heap.Eval.PushEvalStack(fvJi)
            eval st predicateIdentifierAst
            // check, if indeed the predicateId points to a definition, if not issue diagnostics
            let candidates = findCandidatesByName fvJi fvJi.FplId false false
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
            eval st predicateIdentifierAst
            // check, if indeed the predicateId points to a rule of inference, if not issue diagnostics
            let candidates = findCandidatesByName fvJi fvJi.FplId false false
            checkPR001_PR006Diagnostics fvJi candidates
            heap.Eval.PopEvalStack()
        | Some _, _, _ -> () // does not occur, because the parser byModifier choices between only two keywords LiteralByAx or LiteralByDef
        | None, Some _, None -> 
            let fvJi = new FplJustificationItemByCor((pos1, pos2), parent)
            heap.Eval.PushEvalStack(fvJi)
            eval st predicateIdentifierAst
            dollarDigitListAsts.Value |> List.map (eval st) |> ignore
            let candidates = findCandidatesByName fvJi fvJi.FplId false true
            // check, if indeed the predicateId points to a corollary, if not issue diagnostics
            checkPR001_PR006Diagnostics fvJi candidates
            // issue info diagnostics that references to a corollary need the keyword byCor to increase readability
            parent.ErrorOccurred <- emitPR013Diagnostics pos1 pos2
            heap.Eval.PopEvalStack()
        | None, Some _, Some _ -> 
            let fvJi = new FplJustificationItemByProofArgument((pos1, pos2), parent)
            heap.Eval.PushEvalStack(fvJi)
            eval st predicateIdentifierAst
            dollarDigitListAsts.Value |> List.map (eval st) |> ignore
            eval st refArgumentIdentifierAst.Value 
            let splitOffAnyArgumentId (input: string) =
                let parts = input.Split(':')
                if parts.Length > 0 then parts.[0] else ""
            let name = splitOffAnyArgumentId fvJi.FplId
            let candidates = findCandidatesByName fvJi name false true 
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
            eval st predicateIdentifierAst
            let candidates = findCandidatesByName fvJi fvJi.FplId false false
            // check if indeed the predicateId points to a theorem-like statement except a corollary, if not issue diagnostics
            checkPR001_PR006Diagnostics fvJi candidates
            heap.Eval.PopEvalStack()


let tryFindParsedAstUsesClausesEvaluated (parsedAsts: List<ParsedAst>) =
    if parsedAsts.Exists(fun pa -> pa.Status = ParsedAstStatus.UsesClausesEvaluated) then
        Some(parsedAsts.Find(fun pa -> pa.Status = ParsedAstStatus.UsesClausesEvaluated))
    else
        None

let evaluateSymbolTable (st: SymbolTable) =
    st.ParsedAsts.OrderAsts()

    let mutable found = true

    while found do
        let usesClausesEvaluatedParsedAst =
            tryFindParsedAstUsesClausesEvaluated st.ParsedAsts

        match usesClausesEvaluatedParsedAst with
        | Some pa ->
            heap.ClearEvalStack()
            // evaluate the ParsedAst of a theory
            let theoryValue = new FplTheory(pa.Id, st.Root, pa.Parsing.Uri.AbsolutePath, heap.Helper.GetNextAvailableFplBlockRunOrder);
            if not (st.Root.Scope.ContainsKey(pa.Id)) then
                st.Root.Scope.Add(pa.Id, theoryValue)
            else
                st.Root.Scope[pa.Id] <- theoryValue
            heap.Eval.PushEvalStack(theoryValue)
            ad.CurrentUri <- pa.Parsing.Uri
            eval st pa.Parsing.Ast
            pa.Status <- ParsedAstStatus.Evaluated
            heap.Eval.PopEvalStack()
            theoryValue.Run()
        | None -> found <- false

