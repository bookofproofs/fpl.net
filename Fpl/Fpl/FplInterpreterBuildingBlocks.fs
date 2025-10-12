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
open FParsec
open ErrDiagnostics
open FplPrimitives
open FplGrammarTypes
open FplInterpreterTypes
open FplInterpreterDiagnosticsEmitterPre
open FplInterpreterDiagnosticsEmitter

let variableStack = FplVariableStack()

let eval_pos_char_list (st: SymbolTable) (startpos: Position) (endpos: Position) charlist =
    charlist |> List.map string |> String.concat "" |> ignore

/// Simplify trivially nested expressions by removing from the stack FplValue nodes that were created due to too long parsing tree and replacing them by their subnodes 
let simplifyTriviallyNestedExpressions (rb:FplValue) = 
    if rb.ArgList.Count = 1 && rb.FplId = "" then
        // removable reference blocks are those with only a single argument and unset FplId 
        let subNode = rb.ArgList[0]
        match subNode with
        | :? FplConjunction
        | :? FplExclusiveOr 
        | :? FplDisjunction 
        | :? FplNegation 
        | :? FplImplication 
        | :? FplEquivalence 
        | :? FplIsOperator 
        | :? FplEquality 
        | :? FplDecrement 
        | :? FplExtensionObj 
        | :? FplIntrinsicUndef 
        | :? FplReference
        | :? FplGenericQuantor
        | :? FplIntrinsicInd
        | :? FplIntrinsicPred ->
            variableStack.Pop() |> ignore // pop the removable reference block and ignored it
            variableStack.PushEvalStack(subNode) // push its subNode instead
            // adjust subNode's Parent, EndPos, Scope
            subNode.Parent <- rb.Parent 
            subNode.EndPos <- rb.EndPos
            if rb.Scope.ContainsKey(".") then 
                subNode.Scope.Add(".",rb.Scope["."])
            // adjust Parent's scope
            match rb.Parent with 
            | Some parent -> 
                if parent.Scope.ContainsKey(".") then
                   parent.Scope["."] <- subNode
            | _ -> ()
            // prevent recursive loops
            rb.ArgList.Clear() 
            rb.ValueList.Clear()
            rb.Scope.Clear()
        | _ -> ()

/// A recursive function evaluating an AST and returning a list of EvalAliasedNamespaceIdentifier records
/// for each occurrence of the uses clause in the FPL code.
let rec eval (st: SymbolTable) ast =
    let evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst =
        match optVarDeclOrSpecList with
        | Some astList -> astList |> List.map (eval st) |> ignore
        | None -> ()
        eval st predicateAst

    let setUnitType (fv:FplValue) (value:FplValue) (tplName:string)=
        match value with
        | :? FplIntrinsicPred
        | :? FplIntrinsicInd 
        | :? FplIntrinsicObj 
        | :? FplIntrinsicFunc ->
            match fv with
            | :? FplClass -> () // do not override class's type with base obj
            | :? FplReference ->
                fv.TypeId <- $"{value.ShortName}"
                fv.SetValue value
            | _ ->  fv.TypeId <- $"{value.ShortName}"
        | :? FplIntrinsicTpl ->
            match fv with
            | :? FplClass -> () // do not override class's type with base obj
            | :? FplReference ->
                fv.TypeId <- $"{tplName}"
                value.TypeId <- $"{tplName}"
                value.FplId <- $"{tplName}"
                fv.SetValue value
            | _ ->  
                fv.TypeId <- $"{tplName}"
        | _ ->
            fv.TypeId <- value.ShortName
            fv.SetValue value

        match fv with
        | :? FplVariableMany -> fv.TypeId <- $"*{fv.TypeId}"
        | :? FplVariableMany1 -> fv.TypeId <- $"+{fv.TypeId}"
        | _ -> ()
    
    let setSignaturePositions pos1 pos2 = 
        let fv = variableStack.PeekEvalStack()
        match box fv with 
        | :? IHasSignature as withSignature -> 
            withSignature.SignStartPos <- pos1  
            withSignature.SignEndPos <- pos2
        | _ -> ()

    match ast with
    | Ast.IndexType((pos1, pos2),()) -> 
        st.EvalPush("IndexType")
        let fv = variableStack.PeekEvalStack()
        let value = new FplIntrinsicInd((pos1, pos2), fv)
        setUnitType fv value ""
        st.EvalPop() |> ignore
    | Ast.ObjectType((pos1, pos2),()) -> 
        st.EvalPush("ObjectType")
        let fv = variableStack.PeekEvalStack()
        let value = new FplIntrinsicObj((pos1, pos2), fv)
        setUnitType fv value ""
        st.EvalPop()
    | Ast.PredicateType((pos1, pos2),()) -> 
        st.EvalPush("PredicateType")
        let fv = variableStack.PeekEvalStack()
        let value = new FplIntrinsicPred((pos1, pos2), fv)
        setUnitType fv value ""
        st.EvalPop()
    | Ast.FunctionalTermType((pos1, pos2),()) -> 
        st.EvalPush("FunctionalTermType")
        let fv = variableStack.PeekEvalStack()
        let value = new FplIntrinsicFunc((pos1, pos2), fv)
        setUnitType fv value ""
        st.EvalPop()
    | Ast.Many((pos1, pos2),()) ->
        st.EvalPush("Many")
        st.EvalPop()
    | Ast.Many1((pos1, pos2),()) ->
        st.EvalPush("Many1")
        st.EvalPop()
    | Ast.One((pos1, pos2),()) ->
        st.EvalPush("One")
        st.EvalPop()
    | Ast.Star((pos1, pos2),()) ->
        st.EvalPush("Star")
        st.EvalPop()
    | Ast.Dot((pos1, pos2),()) ->
        st.EvalPush("Dot")
        st.EvalPop()
    | Ast.Intrinsic((pos1, pos2),()) -> 
        st.EvalPush("Intrinsic")
        let fv = variableStack.PeekEvalStack()
        fv.IsIntrinsic <- true // flag that this block is intrinsic
        st.EvalPop()
    | Ast.Error  ->   
        st.EvalPush("Error")
        st.EvalPop()
    // strings: | Digits of string
    | Ast.Digits s -> 
        st.EvalPush("Digits")
        let fv = variableStack.PeekEvalStack()
        fv.FplId <- s
        fv.TypeId <- s
        st.EvalPop()
    | Ast.PascalCaseId ((pos1, pos2), pascalCaseId) -> 
        st.EvalPush(PrimPascalCaseId)
        let fv = variableStack.PeekEvalStack()
        match fv.Name with
        | LiteralAxL
        | LiteralThmL
        | LiteralPropL
        | LiteralLemL
        | LiteralConjL
        | LiteralCorL
        | PrimFuncionalTermL
        | PrimPredicateL
        | LiteralPrfL
        | PrimMandatoryFunctionalTermL
        | PrimMandatoryPredicateL
        | PrimPredicateL
        | PrimFuncionalTermL
        | PrimRuleOfInference -> 
            fv.FplId <- pascalCaseId
        | LiteralCtorL ->
            fv.FplId <- pascalCaseId
            fv.TypeId <- pascalCaseId
            emitID008Diagnostics pascalCaseId fv.Parent.Value.FplId pos1 pos2
        | PrimClassL ->
            fv.FplId <- pascalCaseId
            fv.TypeId <- pascalCaseId
        | _ -> ()
        st.EvalPop() 
    | Ast.ExtensionRegex s -> 
        st.EvalPush("ExtensionRegex")
        let fv = variableStack.PeekEvalStack()
        let vars = fv.GetVariables()
        if vars.Length> 0 then
            let mainVar = vars.Head
            mainVar.TypeId <- s // set the extensions's main variable's type to the pattern
        st.EvalPop() 
    // | DollarDigits of Positions * int
    | Ast.DollarDigits((pos1, pos2), s) -> 
        st.EvalPush("DollarDigits")
        let path = st.EvalPath()
        let fv = variableStack.PeekEvalStack()
        let sid = $"${s.ToString()}"
        if path.Contains("Expression.DollarDigits") then
            let value = new FplIntrinsicInd((pos1, pos2), fv)
            value.FplId <- sid
            variableStack.PushEvalStack(value)
            variableStack.PopEvalStack()
        else
            fv.FplId <- fv.FplId + sid
            match fv.TypeId with 
            | "" -> fv.TypeId <- LiteralInd
            | LiteralPred -> ()
            | _ -> fv.TypeId <- fv.TypeId + sid

        st.EvalPop() 
    | Ast.ExtensionName((pos1, pos2), s) ->
        st.EvalPush("ExtensionName")
        let fv = variableStack.PeekEvalStack()
        let extensionName = $"@{s}"
        match fv with 
        | :? FplExtension ->
            fv.FplId <- extensionName
            fv.TypeId <- extensionName
        | :? FplVariableMany -> 
            let sid = $"*{extensionName}"
            fv.TypeId <- sid
        | :? FplVariableMany1 -> 
            let sid = $"+{extensionName}"
            fv.TypeId <- sid
        | _ -> 
            fv.TypeId <- extensionName
            checkID019Diagnostics st extensionName pos1 pos2
        st.EvalPop() 
    | Ast.TemplateType((pos1, pos2), s) -> 
        st.EvalPush("TemplateType")
        let fv = variableStack.PeekEvalStack()
        let value = new FplIntrinsicTpl((pos1, pos2), fv)
        setUnitType fv value s
        st.EvalPop() 
    | Ast.Var((pos1, pos2), name) ->
        st.EvalPush("Var")
        let evalPath = st.EvalPath()
        let isLocalizationDeclaration = evalPath.StartsWith("AST.Namespace.Localization.Expression.")
        let fv = variableStack.PeekEvalStack()
        match fv.Name with 
        | PrimVariableL
        | PrimVariableManyL
        | PrimVariableMany1L -> 
            // in the context of variable declarations, we set the name and positions of the variables
            fv.FplId <- name
            fv.TypeId <- LiteralUndef 
            fv.StartPos <- pos1
            fv.EndPos <- pos2
        | PrimExtensionL -> 
            let newVar = new FplVariable(name, (pos1, pos2), fv)
            variableStack.PushEvalStack(newVar)
            variableStack.PopEvalStack()
        | _ -> 
            // in all other contexts, check by name, if this variable was declared in some scope
            let rec IsInUpperScope (fv1: FplValue): FplGenericVariable option =
                if fv1.Name = PrimTheoryL then 
                    None
                elif fv1.Scope.ContainsKey(name) then
                    Some (fv1.Scope[name] :?> FplGenericVariable)
                else
                    IsInUpperScope fv1.Parent.Value
            match IsInUpperScope fv with
            | Some foundVar -> 
                // it was declared in the scope
                match fv.Name with 
                | PrimRefL ->
                    // for references, add to the reference's scope
                    fv.Scope.Add(name, foundVar)
                    fv.FplId <- name
                | PrimTranslationL ->
                    // for translations, use the name of the variable
                    fv.FplId <- foundVar.Type SignatureType.Name
                | _ -> ()
                foundVar.AuxiliaryInfo <- foundVar.AuxiliaryInfo + 1
            | _ ->
                // otherwise emit variable not declared 
                emitVAR01diagnostics name pos1 pos2
                let undefVar = new FplVariable(name, (pos1, pos2), fv)
                let undefined = new FplIntrinsicUndef((pos1, pos2), undefVar)
                undefVar.SetValue(undefined)
                variableStack.PushEvalStack(undefVar)
                variableStack.PopEvalStack()

        if isLocalizationDeclaration && fv.Scope.ContainsKey(name) then 
            let variable = fv.Scope[name] 
            let rec getLocalization (fValue:FplValue) = 
                match fValue with
                | :? FplLocalization -> fValue
                | _ ->
                    match fValue.Parent with
                    | Some parent -> getLocalization parent
                    | None -> fValue
            let loc = getLocalization fv
            if loc.Scope.ContainsKey(name) then 
                let other = loc.Scope[name]
                emitVAR03diagnostics name other.QualifiedStartPos pos1 pos2 true
            else 
                loc.Scope.Add(name, variable)
                variable.Parent <- Some loc
        st.EvalPop() 
    | Ast.Alias((pos1, pos2), s) -> 
        st.EvalPush("Alias")
        st.EvalPop() 
    | Ast.LanguageCode((pos1, pos2), s) -> 
        st.EvalPush("LanguageCode")
        let fv = variableStack.PeekEvalStack()
        fv.FplId <- s
        fv.TypeId <- s
        fv.StartPos <- pos1
        fv.EndPos <- pos2
        st.EvalPop() 
    | Ast.LocalizationString((pos1, pos2), s) -> 
        st.EvalPush("LocalizationString")
        let fv = variableStack.PeekEvalStack()
        fv.FplId <- s
        fv.TypeId <- s
        st.EvalPop() 
    | Ast.ObjectSymbol((pos1, pos2), symbol) -> 
        st.EvalPush("ObjectSymbol")
        let fv = variableStack.PeekEvalStack()
        fv.FplId <- symbol
        fv.TypeId <- symbol
        fv.StartPos <- pos1
        fv.EndPos <- pos2
        emitSIG01Diagnostics st fv pos1 pos2 
        st.EvalPop()
    | Ast.ArgumentIdentifier((pos1, pos2), argumentId) -> 
        st.EvalPush("ArgumentIdentifier")
        let fv = variableStack.PeekEvalStack()
        fv.FplId <- argumentId.Substring(0,argumentId.Length-1) // argument id without the "." at the end
        st.EvalPop() 
    | Ast.RefArgumentIdentifier((pos1, pos2), argumentId) -> 
        st.EvalPush("RefArgumentIdentifier")
        let fv = variableStack.PeekEvalStack()
        match fv.Name with 
        | PrimJIByProofArgument -> fv.FplId <- $"{fv.FplId}:{argumentId}"
        | PrimArgInfRevoke -> 
            let fvAi = fv :?> FplArgInferenceRevoke
            let arg = fvAi.ParentArgument
            let proof = arg.ParentProof
            if proof.HasArgument argumentId then 
                let refArg = proof.Scope[argumentId] :?> FplArgument
                let aiOpt = refArg.ArgumentInference
                match aiOpt with
                | Some (:? FplArgInferenceAssume as toBeRevoked) -> 
                    match variableStack.LastAssumedArgument with 
                    | Some (:? FplArgInferenceAssume as last) when last = toBeRevoked -> 
                        variableStack.RevokeLastArgument() 
                    | Some (:? FplArgInferenceAssume as last) when last <> toBeRevoked -> 
                        let lastArg = last.ParentArgument
                        emitPR016Diagnostics argumentId lastArg.FplId pos1 pos2
                    | _ ->    
                        // the referenced argument is not an assumption in the proof
                        emitPR015Diagnostics argumentId pos1 pos2
                | _ -> 
                    // the referenced argument is not an assumption in the proof
                    emitPR015Diagnostics argumentId pos1 pos2
            else
                emitPR005Diagnostics argumentId pos1 pos2
            fvAi.FplId <- argumentId
        | PrimJustificationL -> 
            let fvAi = new FplJustificationItemByRefArgument((pos1, pos2), fv)
            fvAi.FplId <- argumentId
            let just = fvAi.ParentJustification
            let arg = just.ParentArgument
            let proof = arg.ParentProof
            if not (proof.HasArgument argumentId) then
                emitPR005Diagnostics argumentId pos1 pos2
            variableStack.PushEvalStack(fvAi)
            variableStack.PopEvalStack()
        | _ -> ()
        st.EvalPop() 
    | Ast.Prefix((pos1, pos2), symbol) -> 
        st.EvalPush("Prefix")
        let fv = variableStack.PeekEvalStack()
        fv.ExpressionType <- FixType.Prefix symbol
        st.EvalPop() 
    | Ast.Infix((pos1, pos2), (symbol, precedenceAsts)) -> 
        st.EvalPush("Infix")
        let fv = variableStack.PeekEvalStack()
        eval st precedenceAsts
        fv.ExpressionType <- FixType.Infix (symbol, fv.AuxiliaryInfo)
        emitSIG02Diagnostics st fv pos1 pos2 
        st.EvalPop() 
    | Ast.Postfix((pos1, pos2), symbol) -> 
        st.EvalPush("Postfix")
        let fv = variableStack.PeekEvalStack()
        fv.ExpressionType <- FixType.Postfix symbol
        st.EvalPop() 
    | Ast.Symbol((pos1, pos2), symbol) -> 
        st.EvalPush("Symbol")
        let fv = variableStack.PeekEvalStack()
        fv.ExpressionType <- FixType.Symbol symbol
        st.EvalPop() 
    | Ast.InfixOperator((pos1, pos2), symbol) -> 
        st.EvalPush("InfixOperator")
        let fv = variableStack.PeekEvalStack()
        fv.FplId <- symbol
        fv.TypeId <- symbol
        fv.StartPos <- pos1
        fv.EndPos <- pos2
        emitSIG01Diagnostics st fv pos1 pos2 
        st.EvalPop() 
    | Ast.PostfixOperator((pos1, pos2), symbol) -> 
        st.EvalPush("PostfixOperator")
        let fv = variableStack.PeekEvalStack()
        fv.FplId <- symbol
        fv.TypeId <- symbol
        fv.StartPos <- pos1
        fv.EndPos <- pos2
        emitSIG01Diagnostics st fv pos1 pos2 
        st.EvalPop() 
    | Ast.PrefixOperator((pos1, pos2), symbol) -> 
        st.EvalPush("PrefixOperator")
        let fv = variableStack.PeekEvalStack()
        fv.FplId <- symbol
        fv.TypeId <- symbol
        fv.StartPos <- pos1
        fv.EndPos <- pos2
        emitSIG01Diagnostics st fv pos1 pos2 
        st.EvalPop() 
    // | Self of Positions * unit
    | Ast.Self((pos1, pos2), _) -> 
        st.EvalPush("Self")
        let parent = variableStack.PeekEvalStack()
        let fv = new FplSelf((pos1, pos2), parent)
        match parent with 
        | :? FplReference -> 
            parent.FplId <- fv.FplId
        | _ -> ()
        variableStack.PushEvalStack(fv)
        variableStack.PopEvalStack()
        let oldDiagnosticsStopped = ad.DiagnosticsStopped
        ad.DiagnosticsStopped <- false
        match fv.NextBlockNode with
        | Some block ->
            match block.Name with 
            | PrimMandatoryFunctionalTermL
            | PrimMandatoryPredicateL
            | PrimClassL
            | PrimPredicateL
            | PrimFuncionalTermL ->
                fv.Scope.Add(block.FplId, block)
            | _ ->
                emitID016diagnostics $"'{block.Name}' {block.Type(SignatureType.Name)}" pos1 pos2
        | _ -> ()
        ad.DiagnosticsStopped <- oldDiagnosticsStopped
        st.EvalPop() 
    | Ast.Parent((pos1, pos2), _) -> 
        st.EvalPush("Parent")
        let parent = variableStack.PeekEvalStack()
        let fv = new FplParent((pos1, pos2), parent)
        match parent with 
        | :? FplReference -> 
            parent.FplId <- fv.FplId
        | _ -> ()
        variableStack.PushEvalStack(fv)
        variableStack.PopEvalStack()
        let oldDiagnosticsStopped = ad.DiagnosticsStopped
        ad.DiagnosticsStopped <- false
        match fv.UltimateBlockNode, fv.NextBlockNode with
        | Some block, Some nextBlock ->
            match block.Name, nextBlock.Name with 
            | PrimClassL, LiteralCtorL 
            | PrimClassL, PrimMandatoryFunctionalTermL
            | PrimClassL, PrimMandatoryPredicateL
            | PrimPredicateL, PrimMandatoryFunctionalTermL
            | PrimPredicateL, PrimMandatoryPredicateL
            | PrimFuncionalTermL, PrimMandatoryFunctionalTermL
            | PrimFuncionalTermL, PrimMandatoryPredicateL ->
                fv.Scope.Add(block.FplId, block)
            | PrimClassL, PrimClassL ->
                let alternative = Some "However, the reference was made inside the class block and not inside its constructor or its property."
                emitID015diagnostics $"'{block.Name}' {block.Type(SignatureType.Name)}" pos1 pos2 alternative
            | _ ->
                emitID015diagnostics $"'{block.Name}' {block.Type(SignatureType.Name)}" pos1 pos2 None
        | _ -> ()
        ad.DiagnosticsStopped <- oldDiagnosticsStopped

        st.EvalPop() 
    | Ast.True((pos1, pos2), _) -> 
        st.EvalPush("True")
        let fv = variableStack.PeekEvalStack()
        let value = new FplIntrinsicPred((pos1, pos2), fv)
        value.FplId <- LiteralTrue
        variableStack.PushEvalStack(value)
        variableStack.PopEvalStack()
        st.EvalPop() 
    | Ast.False((pos1, pos2), _) -> 
        st.EvalPush("False")
        let fv = variableStack.PeekEvalStack()
        let value = new FplIntrinsicPred((pos1, pos2), fv)
        value.StartPos <- pos1
        value.EndPos <- pos2
        value.FplId <- LiteralFalse
        value.TypeId <- LiteralPred
        variableStack.PushEvalStack(value)
        variableStack.PopEvalStack()
        st.EvalPop() 
    | Ast.Undefined((pos1, pos2), _) -> 
        st.EvalPush("Undefined")
        let fv = variableStack.PeekEvalStack()
        let fvNew = new FplIntrinsicUndef((pos1, pos2), fv)
        variableStack.PushEvalStack(fvNew)
        variableStack.PopEvalStack()
        st.EvalPop() 
    | Ast.Trivial((pos1, pos2), _) -> 
        st.EvalPush("Trivial")
        let fv = variableStack.PeekEvalStack()
        let refBlock = new FplArgInferenceTrivial((pos1, pos2), fv) 
        variableStack.PushEvalStack(refBlock)
        variableStack.PopEvalStack()
        st.EvalPop() 
    | Ast.Qed((pos1, pos2), _) -> 
        st.EvalPush("Qed")
        st.EvalPop() 
    | Ast.RuleOfInferenceSignature((pos1, pos2), simpleSignatureAst) ->
        st.EvalPush("RuleOfInferenceSignature")
        variableStack.InSignatureEvaluation <- true
        eval st simpleSignatureAst
        setSignaturePositions pos1 pos2
        variableStack.InSignatureEvaluation <- false
        st.EvalPop()
    | Ast.RuleOfInference((pos1, pos2), (signatureAst, premiseConclusionBlockAst)) ->
        st.EvalPush("RuleOfInference")
        let parent = variableStack.PeekEvalStack()
        let fv = new FplRuleOfInference((pos1, pos2), parent, variableStack.GetNextAvailableFplBlockRunOrder)
        ad.DiagnosticsStopped <- true // stop all diagnostics during rule of inference
        variableStack.PushEvalStack(fv)
        eval st signatureAst
        eval st premiseConclusionBlockAst
        ad.DiagnosticsStopped <- false // enable all diagnostics after rule of inference
        variableStack.PopEvalStack() 
        st.EvalPop() 
    | Ast.Mapping((pos1, pos2), variableTypeAst) ->
        st.EvalPush("Mapping")
        let fv = variableStack.PeekEvalStack()
        let map = new FplMapping((pos1, pos2), fv)
        variableStack.PushEvalStack(map)
        eval st variableTypeAst
        variableStack.PopEvalStack()
        st.EvalPop()
    | Ast.ClassIdentifier((pos1, pos2), ast1) ->
        st.EvalPush("ClassIdentifier")
        eval st ast1
        let fv = variableStack.PeekEvalStack()
        fv.EndPos <- pos2
        st.EvalPop()
    | Ast.Extension((pos1, pos2), extensionString) ->
        st.EvalPush("Extension")
        let fv = variableStack.Pop()
        let fplNew = new FplExtensionObj((pos1,pos2), fv.Parent.Value)
        variableStack.PushEvalStack(fplNew)
        fplNew.FplId <- extensionString
        checkID018Diagnostics st fplNew extensionString pos1 pos2
        st.EvalPop()
    | Ast.ExtensionType((pos1, pos2), extensionNameAst) ->
        st.EvalPush("ExtensionType")
        eval st extensionNameAst
        st.EvalPop()
    | Ast.UsesClause((pos1, pos2), ast1) ->
        st.EvalPush("UsesClause")
        eval st ast1
        st.EvalPop()
    | Ast.Not((pos1, pos2), predicateAst) ->
        st.EvalPush("Not")
        let fv = variableStack.PeekEvalStack()
        let fvNew = new FplNegation((pos1, pos2), fv)
        variableStack.PushEvalStack(fvNew)
        eval st predicateAst
        emitLG000orLG001Diagnostics fvNew PrimNegation
        variableStack.PopEvalStack()
        st.EvalPop()
    | Ast.InEntity((pos1, pos2), ast1) ->
        st.EvalPush("InEntity")
        eval st ast1
        st.EvalPop()
    | Ast.Assertion((pos1, pos2), predicateAst) ->
        st.EvalPush("Assertion")
        let fv = variableStack.PeekEvalStack()
        let fvNew = new FplAssertion((pos1, pos2), fv)
        variableStack.PushEvalStack(fvNew)
        eval st predicateAst
        variableStack.PopEvalStack()
        st.EvalPop()
    | Ast.DottedPredicate((pos1, pos2), predicateWithOptSpecificationAst) ->
        st.EvalPush("DottedPredicate")
        let fv = variableStack.PeekEvalStack()
        let refBlock = new FplReference((pos1, pos2), fv) 
        fv.Scope.Add(".",refBlock)
        variableStack.PushEvalStack(refBlock)
        eval st predicateWithOptSpecificationAst
        variableStack.PopEvalStack()
        st.EvalPop()
    | Ast.Return((pos1, pos2), returneeAst) ->
        st.EvalPush("Return")
        let fv = variableStack.PeekEvalStack()
        let stmt = new FplReturn((pos1,pos2), fv)
        variableStack.PushEvalStack(stmt)
        eval st returneeAst
        variableStack.PopEvalStack() 
        st.EvalPop()
    | Ast.AssumeArgument((pos1, pos2), predicateAst) ->
        st.EvalPush("AssumeArgument")
        let fv = variableStack.PeekEvalStack()
        let fvNew = new FplArgInferenceAssume((pos1, pos2), fv) 
        variableStack.PushEvalStack(fvNew)
        eval st predicateAst
        variableStack.PopEvalStack()
        variableStack.AssumeArgument fvNew
        st.EvalPop()
    | Ast.RevokeArgument((pos1, pos2), predicateAst) ->
        st.EvalPush("RevokeArgument")
        let fv = variableStack.PeekEvalStack()
        let argInf = new FplArgInferenceRevoke((pos1, pos2), fv) 
        variableStack.PushEvalStack(argInf)
        eval st predicateAst
        variableStack.PopEvalStack()
        st.EvalPop()
    | Ast.ByDef((pos1, pos2), variableAst) ->
        st.EvalPush("ByDef")
        let parent = variableStack.PeekEvalStack()
        let fvJi = new FplJustificationItemByDefVar((pos1, pos2), parent)
        variableStack.PushEvalStack(fvJi)
        eval st variableAst
        variableStack.PopEvalStack()
        st.EvalPop()
    | Ast.VariableType((pos1, pos2), compoundVariableTypeAst) ->
        st.EvalPush("VariableType")
        eval st compoundVariableTypeAst
        st.EvalPop()
    | Ast.AST((pos1, pos2), ast1) ->
        st.EvalPush("AST")
        eval st ast1
        st.EvalPop()
    // | NamespaceIdentifier of Positions * Ast list
    | Ast.PredicateIdentifier((pos1, pos2), dottedIdListAst) ->
        st.EvalPush("PredicateIdentifier")

        let pascalCaseIdList = dottedIdListAst |> List.collect (function Ast.PascalCaseId (_,s) -> [s] | _ -> [])
        let identifier = String.concat "." pascalCaseIdList
        let evalPath = st.EvalPath()
        let fv = variableStack.PeekEvalStack()

        match fv with 
        | :? FplVariableMany -> 
            fv.TypeId <- $"*{identifier}"
        | :? FplVariableMany1 -> 
            fv.TypeId <- $"+{identifier}"
        | :? FplVariable -> 
            fv.TypeId <- identifier
        | :? FplMapping -> 
            fv.TypeId <- fv.TypeId + identifier
        | :? FplBase 
        | :? FplBaseConstructorCall -> 
            fv.FplId <- identifier
            fv.TypeId <- identifier
        | :? FplReference -> 
            fv.FplId <- fv.FplId + identifier
            fv.TypeId <- fv.TypeId + identifier
        | :? FplGenericJustificationItem as fvJi -> 
            fvJi.FplId <- identifier
        | _ -> ()
        if evalPath.Contains(".NamedVarDecl.") || evalPath.Contains(".VariableType.ClassType.") then 
            let candidates = findCandidatesByName st identifier false false
            match (fv, candidates.Length) with
            | (:? FplVariable, 0) -> 
                emitSIG04DiagnosticsForTypes identifier pos1 pos2
                let undefValue = new FplIntrinsicUndef((fv.StartPos, fv.EndPos), fv)
                fv.ValueList.Add(undefValue)
               
            | (:? FplVariable, 1) -> 
                fv.Scope.TryAdd(fv.FplId, candidates.Head) |> ignore
            | (:? FplVariable, _) -> 
                let candidatesNames =
                    candidates
                    |> Seq.map (fun fv -> qualifiedName fv)
                    |> String.concat ", "
                emitID017Diagnostics identifier candidatesNames pos1 pos2
            | _ -> ()

        
        st.EvalPop()
    | Ast.ParamTuple((pos1, pos2), namedVariableDeclarationListAsts) ->
        st.EvalPush("ParamTuple")
        let fv = variableStack.PeekEvalStack()
        namedVariableDeclarationListAsts |> List.map (
            fun child ->
            match child with 
            | Ast.NamedVarDecl(_,((varList,_),_)) -> fv.Arity <- fv.Arity + varList.Length
            | _ -> ()
            eval st child
        ) |> ignore
        st.EvalPop()
    | Ast.NamespaceIdentifier((pos1, pos2), asts) ->
        st.EvalPush("NamespaceIdentifier")
        asts |> List.map (eval st) |> ignore
        st.EvalPop()
    | Ast.TranslationTerm((pos1, pos2), asts) ->
        st.EvalPush("TranslationTerm")
        let fv = variableStack.PeekEvalStack()
        asts |> List.map (fun ebnfTerm ->
            let trsl = new FplTranslation((pos1, pos2), fv)
            variableStack.PushEvalStack(trsl)
            eval st ebnfTerm
            variableStack.PopEvalStack()
        ) |> ignore
        st.EvalPop()
    | Ast.TranslationTermList((pos1, pos2), ebnfTermAsts) ->
        st.EvalPush("TranslationTermList")
        let chooseRandomMember (lst: Ast list) =
            let rnd = Random()
            let index = rnd.Next(lst.Length)
            lst.[index]
        eval st (chooseRandomMember ebnfTermAsts)
        st.EvalPop()
    | Ast.BrackedCoordList((pos1, pos2), coordListAst) ->
        st.EvalPush("BrackedCoordList")
        let getProceedingReference =
            let getFirstRefFromStack =
                variableStack.EvalStack
                |> Seq.tryFind (fun fv -> fv :? FplReference )
            match getFirstRefFromStack with 
            | Some ref -> Some (ref :?> FplReference)
            | _ -> None

        match getProceedingReference with 
        | Some ref -> 
            ref.ArgType <- ArgType.Brackets
            if coordListAst.Length > 0 then 
                coordListAst 
                |> List.iter (fun pred -> 
                    let ref = new FplReference((pos1, pos2), ref)
                    variableStack.PushEvalStack(ref)
                    eval st pred
                    variableStack.PopEvalStack()
                ) 
        | _ -> ()
        st.EvalPop()
    | Ast.And((pos1, pos2), (predicateAst1, predicateAst2)) ->
        st.EvalPush("And")
        let fv = variableStack.PeekEvalStack()
        let fvNew = new FplConjunction((pos1, pos2), fv)
        variableStack.PushEvalStack(fvNew)
        eval st predicateAst1
        eval st predicateAst2
        emitLG000orLG001Diagnostics fvNew PrimConjunction
        variableStack.PopEvalStack()
        st.EvalPop()
    | Ast.Or((pos1, pos2), (predicateAst1, predicateAst2)) ->
        st.EvalPush("Or")
        let fv = variableStack.PeekEvalStack()
        let fvNew = new FplDisjunction((pos1, pos2), fv)
        variableStack.PushEvalStack(fvNew)
        eval st predicateAst1
        eval st predicateAst2
        emitLG000orLG001Diagnostics fvNew PrimDisjunction
        variableStack.PopEvalStack()
        st.EvalPop()
    | Ast.Xor((pos1, pos2), (predicateAst1, predicateAst2)) ->
        st.EvalPush("Xor")
        let fv = variableStack.PeekEvalStack()
        let fvNew = new FplExclusiveOr((pos1, pos2), fv)
        variableStack.PushEvalStack(fvNew)
        eval st predicateAst1
        eval st predicateAst2
        emitLG000orLG001Diagnostics fvNew "exclusive-or"
        variableStack.PopEvalStack()
        st.EvalPop()
    | Ast.VarDeclBlock((pos1, pos2), varDeclOrStmtAstList) ->
        st.EvalPush("VarDeclBlock")
        varDeclOrStmtAstList 
        |> List.map (fun subAst -> eval st subAst) |> ignore
        st.EvalPop()
    | Ast.StatementList((pos1, pos2), asts) ->
        st.EvalPush("StatementList")
        asts |> List.map (eval st) |> ignore
        st.EvalPop()
    | Ast.PremiseList((pos1, pos2), predicateListAsts) ->
        st.EvalPush("PremiseList")
        let parent = variableStack.PeekEvalStack()
        let fv = new FplPredicateList((pos1, pos2), parent, variableStack.GetNextAvailableFplBlockRunOrder) 
        variableStack.PushEvalStack(fv)
        predicateListAsts |> List.map (eval st) |> ignore
        variableStack.PopEvalStack()
        st.EvalPop()
    | Ast.JustificationItem((pos1, pos2), justificationReferenceAst) ->
        st.EvalPush("JustificationItem")
        eval st justificationReferenceAst 
        st.EvalPop()
    | Ast.Justification((pos1, pos2), justificationItemAsts) ->
        st.EvalPush("Justification")
        let fv = variableStack.PeekEvalStack()
        let just = new FplJustification((pos1, pos2), fv) 
        variableStack.PushEvalStack(just)
        justificationItemAsts |> List.map (eval st) |> ignore
        variableStack.PopEvalStack()
        st.EvalPop()
    | Ast.ArgumentTuple((pos1, pos2), predicateListAst) ->
        st.EvalPush("ArgumentTuple")
        let next = variableStack.PeekEvalStack()
        let consumeArgumentsWithParent (parent:FplValue) =
            if predicateListAst.Length > 0 then 
                predicateListAst 
                |> List.iter (fun pred -> 
                    let ref = new FplReference((pos1, pos2), parent)
                    variableStack.PushEvalStack(ref)
                    eval st pred
                    variableStack.PopEvalStack()
                )
        
        let getProceedingReference =
            let getFirstRefFromStack =
                variableStack.EvalStack
                |> Seq.tryFind (fun fv -> fv :? FplReference )
            match getFirstRefFromStack with 
            | Some ref -> Some (ref :?> FplReference)
            | _ -> None
            
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
        st.EvalPop()
    | Ast.QualificationList((pos1, pos2), asts) ->
        st.EvalPush("QualificationList")
        if asts.Length > 0 then
            let fv = variableStack.PeekEvalStack()
            asts |> List.map (eval st) |> ignore
        st.EvalPop()
    // | Namespace of Ast option * Ast list
    | Ast.Namespace(asts) ->
        st.EvalPush("Namespace")
        asts |> List.map (eval st) |> ignore
        st.EvalPop()
    // CompoundFunctionalTermType of Positions * ((Ast * Ast) option)
    | Ast.CompoundFunctionalTermType((pos1, pos2), (ast1, astTupleOption)) ->
        st.EvalPush("CompoundFunctionalTermType")
        eval st ast1
        match astTupleOption with 
        | Some (ast2, _) -> eval st ast2 |> ignore
        | _ -> ()
        match astTupleOption with 
        | Some (_, ast3) -> 
            let fv = variableStack.PeekEvalStack()
            fv.EndPos <- pos2
            eval st ast3 |> ignore
        | _ -> ()
        st.EvalPop()
    // AliasedNamespaceIdentifier of Positions * (Ast * Ast option)
    | Ast.AliasedNamespaceIdentifier((pos1, pos2), (ast1, optAst)) ->
        st.EvalPush("AliasedNamespaceIdentifier")
        eval st ast1
        optAst |> Option.map (eval st) |> ignore
        st.EvalPop()
    | Ast.ClassType((pos1, pos2), specificClassTypeAst) ->
        st.EvalPush("ClassType")
        eval st specificClassTypeAst
        st.EvalPop()
    | Ast.CompoundPredicateType((pos1, pos2), (ast1, optAst)) ->
        st.EvalPush("CompoundPredicateType")
        eval st ast1
        optAst |> Option.map (eval st) |> ignore
        st.EvalPop()
    | Ast.ReferenceToProofOrCorollary((pos1, pos2), (referencingIdentifierAst)) ->
        st.EvalPush("ReferenceToProofOrCorollary")
        eval st referencingIdentifierAst
        st.EvalPop()
    | Ast.PredicateWithOptSpecification((pos1, pos2), (fplIdentifierAst, optionalSpecificationAst)) ->
        st.EvalPush("PredicateWithOptSpecification")
        let fv = variableStack.PeekEvalStack()
        let searchForCandidatesOfReferenceBlock (refBlock:FplValue) = 
            let candidatesFromTheory = findCandidatesByName st refBlock.FplId true false
            let candidatesFromPropertyScope = findCandidatesByNameInBlock refBlock refBlock.FplId
            let candidatesFromDottedQualification = findCandidatesByNameInDotted refBlock refBlock.FplId
            candidatesFromTheory  
            @ candidatesFromPropertyScope 
            @ candidatesFromDottedQualification

        match optionalSpecificationAst with
        | Some specificationAst -> 
            let refBlock = new FplReference((pos1, pos2), fv) 
            variableStack.PushEvalStack(refBlock)
            eval st fplIdentifierAst
            eval st specificationAst |> ignore
            let refBlock = variableStack.PeekEvalStack()
            if System.Char.IsLower(refBlock.FplId[0]) then
                // match the signatures of small-letter entities (like the self or parent entity, or variables with arguments) 
                // with their declared types 
                let candidatesOfSelfOrParentEntity = 
                    refBlock.Scope
                    |> Seq.filter (fun kvp -> kvp.Key = LiteralSelf || kvp.Key = LiteralParent)
                    |> Seq.map (fun kvp -> kvp.Value)
                    |> Seq.toList


                let candidatesOfVariables = 
                    refBlock.Scope
                    |> Seq.filter (fun kvp -> kvp.Key = refBlock.FplId)
                    |> Seq.map (fun kvp -> kvp.Value)
                    |> Seq.toList

                let candidatesOfVariableTypes = 
                    candidatesOfVariables
                    |> Seq.filter (fun fv1 -> fv1.ArgList.Count = 1)
                    |> Seq.map (fun fv1 -> fv1.ArgList[0])
                    |> Seq.toList

                let candidates = candidatesOfSelfOrParentEntity 
                                 @ candidatesOfVariables 
                                 @ candidatesOfVariableTypes 
                checkSIG04Diagnostics refBlock candidates |> ignore
            else
                let candidates = searchForCandidatesOfReferenceBlock refBlock
                match checkSIG04Diagnostics refBlock candidates with
                | Some matchedCandidate -> 
                    if matchedCandidate.IsIntrinsic then 
                        let defaultConstructor = new FplDefaultConstructor(matchedCandidate.FplId, (refBlock.StartPos, refBlock.EndPos), refBlock)
                        defaultConstructor.EmbedInSymbolTable defaultConstructor.Parent
                        defaultConstructor.ToBeConstructedClass <- Some matchedCandidate
                    else
                        refBlock.Scope.TryAdd(refBlock.FplId, matchedCandidate) |> ignore
                | _ -> ()

            variableStack.PopEvalStack()
        | None -> 
            // if no specification was found then simply continue in the same context
            eval st fplIdentifierAst
            let block = fv.UltimateBlockNode.Value
            // make sure, we still add a referenced node candidate to the scope of a reference
            let candidates = searchForCandidatesOfReferenceBlock fv
            let classes = candidates |> List.filter (fun c -> c.IsClass())
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
                    fv.Scope.TryAdd(fv.FplId,matchedCandidate) |> ignore
                | _ -> ()
            elif classes.Length > 0 && constructors.Length = 0 then
                // add the class (intrinsic case, no constructors at all)

                fv.Scope.TryAdd(fv.FplId, classes.Head) |> ignore
                // let candidate = classes.Head
                // emitID025Diagnostics (qualifiedName candidate) candidate.EnglishName block.EnglishName block.Name fv.StartPos fv.EndPos
            elif candidates.Length > 0 then
                // not a class was referred, add the candidate (e.g., referenced variable)
                let candidate = candidates.Head
                fv.FplId <- candidate.FplId 
                fv.Scope.TryAdd(fv.FplId, candidate) |> ignore
                emitID025Diagnostics (qualifiedName candidate) candidate.EnglishName block.EnglishName block.Name fv.StartPos fv.EndPos
            else
                ()

        simplifyTriviallyNestedExpressions fv |> ignore
        st.EvalPop()
    // | SelfAts of Positions * char list
    | Ast.SelfOrParent((pos1, pos2), selforParentAst) -> 
        st.EvalPush("SelfAts")
        eval st selforParentAst
        st.EvalPop()
    | Ast.Language((pos1, pos2),(langCode, ebnfAst)) ->
        st.EvalPush("Language")
        let fv = variableStack.PeekEvalStack()
        let lang = new FplLanguage((pos1, pos2), fv) 
        variableStack.PushEvalStack(lang)
        eval st langCode
        eval st ebnfAst
        variableStack.PopEvalStack() // remove language
        st.EvalPop()
    | Ast.InheritedFunctionalTypeList inheritedTypeAsts 
    | Ast.InheritedClassTypeList inheritedTypeAsts -> 
        st.EvalPush("InheritedFunctionalOrClassTypeList")
        let beingCreatedNode = variableStack.PeekEvalStack()
        // a dictionary to prevent shadowed variables
        let distinctVariables = Dictionary<string, FplValue>()
        beingCreatedNode.GetVariables()
        |> List.iter (fun fv ->
            distinctVariables.Add(fv.FplId, beingCreatedNode)
        )
        // a dictionary to prevent shadowed properties
        let distinctProperties = Dictionary<string, FplValue>()
        beingCreatedNode.GetProperties()
        |> List.iter (fun fv ->
            distinctVariables.Add(fv.Type SignatureType.Mixed, beingCreatedNode)
        )
        let addVariablesAndPropertiesOfBaseNode (bNode:FplValue) = 
            bNode.GetVariables()
            |> List.iter (fun var ->
                if distinctVariables.ContainsKey var.FplId then
                    emitVAR06iagnostic var.FplId bNode.FplId (distinctVariables[var.FplId].FplId) true bNode.StartPos bNode.EndPos
                else
                    // store the variable name and the class, it is from 
                    distinctVariables.Add (var.FplId, bNode)
                    beingCreatedNode.Scope.Add (var.FplId, var.Clone())
            )
            bNode.GetProperties()
            |> List.iter (fun prty ->
                let prtyName = prty.Type SignatureType.Mixed
                if distinctProperties.ContainsKey prtyName then
                    emitSIG06iagnostic prtyName bNode.FplId (distinctProperties[prtyName].FplId) true bNode.StartPos bNode.EndPos
                else
                    // store the property name and the class, it is from 
                    distinctProperties.Add (prtyName, bNode)
                    beingCreatedNode.Scope.Add (prtyName, prty.Clone())
            )

        inheritedTypeAsts
        |> List.iter (fun baseAst ->
            match baseAst with
            | Ast.PredicateIdentifier((pos1, pos2), _) ->
                // retrieve the name of the class and the class (if it exists)
                let baseNode = new FplBase((pos1, pos2), beingCreatedNode)
                variableStack.PushEvalStack(baseNode)            
                eval st baseAst
                variableStack.PopEvalStack() |> ignore
                let candidates = findCandidatesByName st baseNode.FplId false true
                if candidates.Length > 0 then 
                    let foundBase = candidates.Head
                    match beingCreatedNode, foundBase with
                    | :? FplFunctionalTerm, :? FplFunctionalTerm ->
                        let nodeType = beingCreatedNode.Type SignatureType.Type
                        let baseType = foundBase.Type SignatureType.Type
                        if nodeType <> baseType then 
                            emitID007diagnostics beingCreatedNode.Name nodeType foundBase.Name baseType pos1 pos2
                        else 
                            baseNode.Scope.Add (foundBase.FplId, foundBase) // add found functional term to base
                            addVariablesAndPropertiesOfBaseNode foundBase
                    | :? FplClass, :? FplClass -> 
                        baseNode.Scope.Add (foundBase.FplId, foundBase) // add found base class to base
                        addVariablesAndPropertiesOfBaseNode foundBase
                    | :? FplFunctionalTerm, _
                    | :? FplClass, _ ->
                        let nodeType = beingCreatedNode.Type SignatureType.Type
                        let baseType = foundBase.Type SignatureType.Type
                        emitID007diagnostics beingCreatedNode.Name nodeType foundBase.Name baseType pos1 pos2
                    | _ -> () // does not occur, since syntax of inherited base from non-classes and non-functional terms is not supported 
                else
                    emitID010Diagnostics baseNode.FplId pos1 pos2
                if baseNode.FplId = beingCreatedNode.FplId then 
                    emitID009Diagnostics baseNode.FplId pos1 pos2
            | _ -> ()
        )
        let classInheritanceChains = findInheritanceChains beingCreatedNode 
        classInheritanceChains
        |> Seq.filter (fun kvp -> kvp.Value <> "ok")
        |> Seq.iter (fun kvp -> 
            emitID011Diagnostics kvp.Key kvp.Value beingCreatedNode.StartPos beingCreatedNode.EndPos
        )
        st.EvalPop()
    | Ast.ExtensionAssignment((pos1, pos2), (varAst, extensionRegexAst)) ->
        st.EvalPush("ExtensionAssignment")
        eval st varAst
        eval st extensionRegexAst
        st.EvalPop()
    | Ast.ExtensionSignature((pos1, pos2), (extensionAssignmentAst, extensionMappingAst)) ->
        st.EvalPush("ExtensionSignature")
        eval st extensionAssignmentAst
        eval st extensionMappingAst
        st.EvalPop()
    | Ast.DefinitionExtension((pos1, pos2), ((extensionNameAst,extensionSignatureAst), extensionTermAst)) ->
        st.EvalPush("DefinitionExtension")
        let parent = variableStack.PeekEvalStack()
        let fv = new FplExtension((pos1,pos2), parent)
        variableStack.PushEvalStack(fv)
        eval st extensionNameAst
        eval st extensionSignatureAst
        eval st extensionTermAst
        variableStack.PopEvalStack()
        st.EvalPop()
    | Ast.Impl((pos1, pos2), (predicateAst1, predicateAst2)) ->
        st.EvalPush("Impl")
        let fv = variableStack.PeekEvalStack()
        let fvNew = new FplImplication((pos1, pos2), fv)
        variableStack.PushEvalStack(fvNew)
        eval st predicateAst1
        eval st predicateAst2
        emitLG000orLG001Diagnostics fvNew PrimImplication
        variableStack.PopEvalStack()
        st.EvalPop()
    | Ast.Iif((pos1, pos2), (predicateAst1, predicateAst2)) ->
        st.EvalPush("Iif")
        let fv = variableStack.PeekEvalStack()
        let fvNew = new FplEquivalence((pos1, pos2), fv)
        variableStack.PushEvalStack(fvNew)
        eval st predicateAst1
        eval st predicateAst2
        emitLG000orLG001Diagnostics fvNew PrimEquivalence
        variableStack.PopEvalStack()
        st.EvalPop()
    | Ast.IsOperator((pos1, pos2), (isOpArgAst, variableTypeAst)) ->
        st.EvalPush("IsOperator")
        let fv = variableStack.PeekEvalStack()
        let fvNew = new FplIsOperator((pos1, pos2), fv)
        variableStack.PushEvalStack(fvNew)
        let operand = new FplReference((pos1, pos2), fvNew) 
        variableStack.PushEvalStack(operand)
        eval st isOpArgAst
        variableStack.PopEvalStack()
        let typeOfOperand = new FplMapping((pos1, pos2), fvNew) 
        variableStack.PushEvalStack(typeOfOperand)
        eval st variableTypeAst
        variableStack.PopEvalStack()
        variableStack.PopEvalStack()
        st.EvalPop()
    | Ast.Delegate((pos1, pos2), (delegateId, argumentTupleAst)) ->
        st.EvalPush("Delegate")
        let fv = variableStack.PeekEvalStack()
        match delegateId with 
        | PrimDelegateEqualL -> 
            let deleg = new FplEquality(delegateId, (pos1, pos2), fv)
            variableStack.PushEvalStack(deleg)
            eval st argumentTupleAst
            variableStack.PopEvalStack()
        | PrimDelegateDecrementL -> 
            let deleg = new FplDecrement(delegateId, (pos1, pos2), fv)
            variableStack.PushEvalStack(deleg)
            eval st argumentTupleAst
            variableStack.PopEvalStack()
        | _ -> 
            let deleg = new FplReference((pos1, pos2), fv)
            deleg.FplId <- delegateId
            variableStack.PushEvalStack(deleg)
            eval st argumentTupleAst
            variableStack.PopEvalStack()
            emitID013Diagnostics pos1 pos2 $"Unknown delegate `{delegateId}`"  
        st.EvalPop()
    // | ClosedOrOpenRange of Positions * ((Ast * Ast option) * Ast)
    | Ast.PredicateSignature(((pos1, pos2), (simpleSignatureAst, paramTupleAst)), optUserDefinedSymbolAst) ->
        st.EvalPush("PredicateSignature")
        variableStack.InSignatureEvaluation <- true
        eval st simpleSignatureAst
        eval st paramTupleAst
        optUserDefinedSymbolAst |> Option.map (eval st) |> Option.defaultValue () |> ignore
        let fv = variableStack.PeekEvalStack()
        emitSIG00Diagnostics fv pos1 pos2
        setSignaturePositions pos1 pos2
        variableStack.InSignatureEvaluation <- false
        st.EvalPop()
    // | ReferencingIdentifier of Positions * (Ast * Ast list)
    | ReferencingIdentifier((pos1, pos2), (predicateIdentifierAst, dollarDigitListAsts)) ->
        st.EvalPush("ReferencingIdentifier")
        eval st predicateIdentifierAst
        dollarDigitListAsts |> List.map (eval st) |> ignore
        let fv = variableStack.PeekEvalStack()
        match fv with 
        | :? FplReference ->
            let candidates = findCandidatesByName st fv.FplId false true
            if candidates.Length > 0 then 
                let candidate = candidates.Head
                fv.Scope.TryAdd(fv.FplId, candidate) |> ignore
                match fv.UltimateBlockNode with
                | Some block ->
                    emitID025Diagnostics (qualifiedName candidate) candidate.EnglishName block.EnglishName block.Name fv.StartPos fv.EndPos
                | _ -> ()
        | _ -> ()
        st.EvalPop()
    | ProofSignature((pos1, pos2), (simpleSignatureAst, dollarDigitListAsts)) ->
        st.EvalPush("ProofSignature")
        variableStack.InSignatureEvaluation <- true
        eval st simpleSignatureAst
        dollarDigitListAsts |> List.map (eval st) |> ignore
        setSignaturePositions pos1 pos2
        variableStack.InSignatureEvaluation <- false
        st.EvalPop()
    | Ast.Localization(((pos1, pos2), predicateAst), translationListAsts) ->
        st.EvalPush("Localization")
        let parent = variableStack.PeekEvalStack()
        let fv = new FplLocalization((pos1, pos2), parent)
        let var04List = Dictionary<string, Positions>()
        ad.DiagnosticsStopped <- true // stop all diagnostics during localization
        variableStack.PushEvalStack(fv)
        eval st predicateAst
        translationListAsts |> List.map (fun subAst -> 
            eval st subAst
            let vars = fv.GetVariables()
            vars
            |> List.filter (fun (var:FplValue) -> var.AuxiliaryInfo = 0)
            |> List.map (fun var ->
                let loc = variableStack.PeekEvalStack()
                let languageList = 
                    loc.Scope 
                    |> Seq.filter (fun kvp -> isLanguage kvp.Value) 
                    |> Seq.map (fun kvp -> kvp.Value) 
                    |> Seq.toList 
                    |> List.rev
                if not languageList.IsEmpty then
                    let lan = languageList.Head
                    var04List.Add (var.FplId,(lan.StartPos, lan.EndPos))
            )
        ) |> ignore
        variableStack.PopEvalStack()
        ad.DiagnosticsStopped <- false // enable all diagnostics during localization
        var04List
        |> Seq.iter (fun kvp -> 
            emitVAR04diagnostics kvp.Key (fst kvp.Value) (snd kvp.Value)
        )
        st.EvalPop()
    | Ast.FunctionalTermInstance((pos1, pos2), (functionalTermInstanceSignatureAst, functionalTermInstanceBlockAst)) ->
        st.EvalPush("FunctionalTermInstance")
        let parent = variableStack.PeekEvalStack()
        let fvNew = new FplMandatoryFunctionalTerm((pos1, pos2), parent)
        variableStack.PushEvalStack(fvNew)
        eval st functionalTermInstanceSignatureAst
        eval st functionalTermInstanceBlockAst
        variableStack.PopEvalStack()
        st.EvalPop()
    // | All of Positions * ((Ast list * Ast option) list * Ast)
    | Ast.All((pos1, pos2), (namedVarDeclAstList, predicateAst)) ->
        st.EvalPush("All")
        let parent = variableStack.PeekEvalStack()
        let fv = new FplQuantorAll((pos1, pos2), parent)
        variableStack.PushEvalStack(fv) // add all quantor
        fv.Arity <- fv.Arity + (namedVarDeclAstList |> List.length)
        namedVarDeclAstList
        |> List.map (fun namedVarDeclAst ->
            eval st namedVarDeclAst
        )
        |> ignore
        eval st predicateAst
        emitVAR05diagnostics fv
        variableStack.PopEvalStack() // remove all quantor
        emitLG000orLG001Diagnostics fv PrimQuantorAll
        st.EvalPop()
    | Ast.Exists((pos1, pos2), (namedVarDeclAstList, predicateAst)) ->
        st.EvalPush("Exists")
        let parent = variableStack.PeekEvalStack()
        let fv = new FplQuantorExists((pos1, pos2), parent)
        variableStack.PushEvalStack(fv) // add exists quantor
        fv.Arity <- fv.Arity + (namedVarDeclAstList |> List.length)
        namedVarDeclAstList
        |> List.map (fun namedVarDeclAst ->
            eval st namedVarDeclAst
        )
        |> ignore
        eval st predicateAst
        emitVAR05diagnostics fv
        variableStack.PopEvalStack() // remove exists quantor
        emitLG000orLG001Diagnostics fv PrimQuantorExists
        st.EvalPop()
    | Ast.ExistsN((pos1, pos2), ((dollarDigitsAst, namedVarDeclListAst), predicateAst)) ->
        st.EvalPush("ExistsN")
        let parent = variableStack.PeekEvalStack()
        let fv = new FplQuantorExistsN((pos1, pos2), parent)
        variableStack.PushEvalStack(fv) // add exists n quantor
        eval st dollarDigitsAst
        namedVarDeclListAst
        |> List.map (fun namedVarDeclAst ->
            eval st namedVarDeclAst
        )
        |> ignore
        eval st predicateAst
        emitVAR05diagnostics fv
        variableStack.PopEvalStack() // remove exists n quantor
        emitLG000orLG001Diagnostics fv PrimQuantorExistsN
        st.EvalPop()
    // | FunctionalTermSignature of Positions * (Ast * Ast)
    | Ast.FunctionalTermSignature(((pos1, pos2), (((simpleSignatureAst, inhFunctionalTypeListAstsOpt), paramTupleAst), mappingAst)), optUserDefinedSymbolAst) -> 
        eval st mappingAst
        variableStack.InSignatureEvaluation <- true
        st.EvalPush("FunctionalTermSignature")
        eval st simpleSignatureAst
        eval st paramTupleAst
        inhFunctionalTypeListAstsOpt |> Option.map (eval st) |> Option.defaultValue () 
        variableStack.InSignatureEvaluation <- false
        optUserDefinedSymbolAst |> Option.map (eval st) |> Option.defaultValue () 
        setSignaturePositions pos1 pos2
        st.EvalPop()
    | Ast.PredicateWithQualification(predicateWithOptSpecificationAst, qualificationListAst) ->
        st.EvalPush("PredicateWithQualification")
        eval st predicateWithOptSpecificationAst
        eval st qualificationListAst
        st.EvalPop()
    // | InfixOperation of Positions * (Ast * Ast option) list
    | Ast.InfixOperation((pos1, pos2), separatedPredicateListAst) ->
        st.EvalPush("InfixOperation")
        let fv = variableStack.PeekEvalStack()
        separatedPredicateListAst
        |> List.map (fun (predAst, optOperandAst) -> 
            // evaluate the operand
            let pred = new FplReference((pos1,pos2), fv)
            variableStack.PushEvalStack(pred)
            eval st predAst
            fv.ArgList.Add(variableStack.Pop()) // pop the stack element (same reference as pred) and store it in a list
            // followed by the operator
            match optOperandAst with
            | Some opAst -> 
                let infixOperator = new FplReference((pos1,pos2), fv)
                variableStack.PushEvalStack(infixOperator)
                // evaluate the operator by trying to find a definition for the operator
                eval st opAst
                // store the index of the infix operator, so we still know it after sorting the list by precedence later
                fv.ArgList.Add(variableStack.Pop()) // pop the stack element (same reference as infixOperator) and store it in a list
            | None -> () // in this case, we consumed and evaluated all operators in the infix operation (due to FPL parser Ast structure)
        )
        |> ignore
        
        let precNodeList (fv1:FplValue) = 
            fv1.Scope.Values 
            |> Seq.toList 

        /// Returns the precedence of fv1 if its ExpressionType is Infix
        /// or Int32.MaxValue otherwise
        let getPrecedence (fv1:FplValue) =
            match precNodeList fv1 with
            | [] -> Int32.MaxValue
            | x::xs -> 
                match x.ExpressionType with
                |  FixType.Infix (symb, prec) -> prec
                | _ -> Int32.MaxValue

        // This while loop will evaluate multiple unparenthesized infix operations
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
            match precNodeList currentOp with
            | x::xs -> 
                match checkSIG04Diagnostics currentOp [x] with 
                | Some candidate -> 
                    currentOp.Run variableStack // execute the matched binary operator
                | _ -> ()
            | _ -> ()
            fv.ArgList.RemoveAt(currMinIndex+1) 
            fv.ArgList.RemoveAt(currMinIndex-1) 
        simplifyTriviallyNestedExpressions fv
        st.EvalPop()
    // | Expression of Positions * ((((Ast option * Ast) * Ast option) * Ast option) * Ast)
    | Ast.Expression((pos1, pos2), ((((prefixOpAst, predicateAst), postfixOpAst), optionalSpecificationAst), qualificationListAst)) ->
        st.EvalPush("Expression")
        let fv = variableStack.PeekEvalStack()
        let refBlock = new FplReference((pos1, pos2), fv) 
        variableStack.PushEvalStack(refBlock)
        let ensureReversedPolishNotation = 
            if prefixOpAst.IsSome && postfixOpAst.IsSome then 
                // for heuristic reasons, we choose a precedence of postfix ...
                postfixOpAst |> Option.map (eval st) |> Option.defaultValue () 
                let postfixedInnerPred = new FplReference((pos1,pos2), variableStack.PeekEvalStack())
                variableStack.PushEvalStack(postfixedInnerPred)
                // ... over prefix notation in mathematics
                prefixOpAst |> Option.map (eval st) |> Option.defaultValue ()
                let prefixedInnerPred = new FplReference((pos1,pos2), variableStack.PeekEvalStack())
                variableStack.PushEvalStack(prefixedInnerPred)
                eval st predicateAst
                variableStack.PopEvalStack()
                variableStack.PopEvalStack()
            elif prefixOpAst.IsSome then 
                prefixOpAst |> Option.map (eval st) |> Option.defaultValue ()
                let innerPred = new FplReference((pos1,pos2), variableStack.PeekEvalStack())
                variableStack.PushEvalStack(innerPred)
                eval st predicateAst
                variableStack.PopEvalStack()
            elif postfixOpAst.IsSome then 
                postfixOpAst |> Option.map (eval st) |> Option.defaultValue ()
                let innerPred = new FplReference((pos1,pos2), variableStack.PeekEvalStack())
                variableStack.PushEvalStack(innerPred)
                eval st predicateAst
                variableStack.PopEvalStack()
            else
                eval st predicateAst
        ensureReversedPolishNotation
        optionalSpecificationAst |> Option.map (eval st) |> Option.defaultValue ()
        eval st qualificationListAst
        let refBlock = variableStack.PeekEvalStack() // if the reference was replaced, take this one
        refBlock.EndPos <- pos2
        simplifyTriviallyNestedExpressions refBlock
        let last = variableStack.PeekEvalStack()
        variableStack.PopEvalStack()
        match fv with
        | :? FplTheorem  
        | :? FplLemma  
        | :? FplProposition  
        | :? FplCorollary  
        | :? FplConjecture  
        | :? FplPredicate  
        | :? FplAxiom 
        | :? FplMandatoryPredicate ->
            fv.SetValue(last)
        | :? FplReference ->
            // simplify references created due to superfluous parentheses of expressions
            // by replacing them with their single value
            if prefixOpAst.IsNone && 
                postfixOpAst.IsNone &&
                fv.FplId = "" && 
                fv.ArgList.Count = 1 then
                    let subNode = fv.ArgList[0]
                    match subNode with
                    | :? FplReference ->
                        variableStack.Pop() |> ignore
                        variableStack.PushEvalStack(subNode)
                        subNode.Parent <- fv.Parent
                        fv.ArgList.Clear()
                    | _ -> ()
        | _ -> ()
        st.EvalPop()
    // | Cases of Positions * (Ast list * Ast)
    | Ast.Cases((pos1, pos2), (caseSingleListAsts, caseElseAst)) ->
        st.EvalPush("Cases")
        let parent = variableStack.PeekEvalStack()
        let casesStmt = new FplCases((pos1, pos2), parent)
        variableStack.PushEvalStack(casesStmt) // add cases 
        caseSingleListAsts |> List.map (fun caseAst -> eval st caseAst) |> ignore
        eval st caseElseAst
        variableStack.PopEvalStack() // remove cases
        st.EvalPop()
    | Ast.CaseSingle((pos1, pos2), (predicateAst, statementListAsts)) ->
        st.EvalPush("CaseSingle")
        let parent = variableStack.PeekEvalStack()
        let singleCase = new FplCaseSingle((pos1,pos2), parent)
        variableStack.PushEvalStack(singleCase) // add single case
        eval st predicateAst
        statementListAsts |> List.map (eval st) |> ignore
        variableStack.PopEvalStack() // remove single case 
        st.EvalPop()
    | Ast.CaseElse((pos1, pos2), statementListAsts) ->
        st.EvalPush("CaseElse")
        let parent = variableStack.PeekEvalStack()
        let elseCase = new FplCaseElse((pos1,pos2), parent)
        variableStack.PushEvalStack(elseCase) // add else 
        statementListAsts |> List.map (eval st) |> ignore
        variableStack.PopEvalStack() // remove else 
        st.EvalPop()
    | Ast.MapCases((pos1, pos2), (mapCaseSingleAstList, elseStatementAst)) ->
        st.EvalPush("MapCases")
        let parent = variableStack.PeekEvalStack()
        let fvNew = new FplMapCases((pos1, pos2), parent)
        variableStack.PushEvalStack(fvNew) // add mapcases
        mapCaseSingleAstList |> List.map (fun caseAst -> eval st caseAst) |> ignore
        eval st elseStatementAst
        variableStack.PopEvalStack() // remove mapcases
        st.EvalPop()
    | Ast.MapCaseSingle((pos1, pos2), (predicateFirstAst, predicateSecondAst)) ->
        st.EvalPush("MapCaseSingle")
        let parent = variableStack.PeekEvalStack()
        let mapCaseSingle = new FplMapCaseSingle((pos1,pos2), parent)
        variableStack.PushEvalStack(mapCaseSingle) // add mapcase single
        eval st predicateFirstAst
        eval st predicateSecondAst 
        variableStack.PopEvalStack() // remove mapcase single
        st.EvalPop()
    | Ast.MapCaseElse((pos1, pos2), predicateAst) ->
        st.EvalPush("MapCaseElse")
        let parent = variableStack.PeekEvalStack()
        let elseCase = new FplMapCaseElse((pos1,pos2), parent)
        variableStack.PushEvalStack(elseCase) // add mapcase else
        eval st predicateAst 
        variableStack.PopEvalStack() // remove mapcase else
        st.EvalPop()
    | Ast.FunctionalTermInstanceSignature((pos1, pos2), ((simpleSignatureAst, paramTupleAst), mappingAst)) ->
        variableStack.InSignatureEvaluation <- true
        st.EvalPush("FunctionalTermInstanceSignature")
        eval st simpleSignatureAst
        eval st paramTupleAst
        variableStack.InSignatureEvaluation <- false
        eval st mappingAst
        setSignaturePositions pos1 pos2
        st.EvalPop()
    | Ast.PredicateInstanceSignature((pos1, pos2), (simpleSignatureAst, paramTupleAst)) ->
        variableStack.InSignatureEvaluation <- true
        st.EvalPush("PredicateInstanceSignature")
        eval st simpleSignatureAst
        eval st paramTupleAst
        setSignaturePositions pos1 pos2
        st.EvalPop()
        variableStack.InSignatureEvaluation <- false
    | Ast.ConstructorSignature((pos1, pos2), (simpleSignatureAst, paramTupleAst)) ->
        variableStack.InSignatureEvaluation <- true
        st.EvalPush("ConstructorSignature")
        eval st simpleSignatureAst
        eval st paramTupleAst
        setSignaturePositions pos1 pos2
        st.EvalPop()
        variableStack.InSignatureEvaluation <- false
    | Ast.Assignment((pos1, pos2), (predicateWithQualificationAst, predicateAst)) ->
        st.EvalPush("Assignment")
        let parent = variableStack.PeekEvalStack()
        let fvNew = new FplAssignment((pos1, pos2), parent)
        variableStack.PushEvalStack(fvNew) // add assignment
        let assigneeReference = new FplReference((pos1,pos2), fvNew)
        variableStack.PushEvalStack(assigneeReference) // add assignee
        eval st predicateWithQualificationAst
        variableStack.PopEvalStack() // remove assignee
        let assignedValue = new FplReference((pos1,pos2), fvNew)
        variableStack.PushEvalStack(assignedValue) // add value
        eval st predicateAst
        variableStack.PopEvalStack() // remove value
        variableStack.PopEvalStack() // remove Assignment
        st.EvalPop()
    | Ast.PredicateInstance((pos1, pos2), (signatureAst, predInstanceBlockAst)) ->
        st.EvalPush("PredicateInstance")
        let parent = variableStack.PeekEvalStack()
        let fvNew = new FplMandatoryPredicate((pos1, pos2), parent)
        variableStack.PushEvalStack(fvNew)
        eval st signatureAst
        eval st predInstanceBlockAst
        if not fvNew.IsIntrinsic then // if not intrinsic, check variable usage
            emitVAR04diagnosticsOld fvNew
        variableStack.PopEvalStack()
        st.EvalPop()
    | Ast.BaseConstructorCall((pos1, pos2), (inheritedClassTypeAst, argumentTupleAst)) ->
        st.EvalPush("BaseConstructorCall")
        let parent = variableStack.PeekEvalStack()
        let fvNew = new FplBaseConstructorCall((pos1, pos2), parent) 
        variableStack.PushEvalStack(fvNew)
        eval st inheritedClassTypeAst
        eval st argumentTupleAst
        variableStack.PopEvalStack()
        st.EvalPop()
    | Ast.JustArgInf((pos1, pos2), (justificationAst, argumentInferenceAst)) ->
        st.EvalPush("JustArgInf")
        eval st justificationAst
        eval st argumentInferenceAst
        st.EvalPop()
    | Ast.Argument((pos1, pos2), (argIdAst, argAst)) ->
        st.EvalPush("Argument")
        let fv = variableStack.PeekEvalStack()
        let arg = new FplArgument((pos1, pos2), fv, variableStack.GetNextAvailableFplBlockRunOrder) 
        variableStack.PushEvalStack(arg)
        eval st argIdAst
        eval st argAst
        variableStack.PopEvalStack()
        st.EvalPop()
    // | ForIn of Positions * ((Ast * Ast) * Ast list)
    | Ast.ForIn((pos1, pos2), ((entityAst, inDomainAst), statementListAst)) ->
        st.EvalPush("ForIn")
        let parent = variableStack.PeekEvalStack()
        let forStmt = new FplForInStmt((pos1, pos2), parent)
        variableStack.PushEvalStack(forStmt) // add ForInStmt
        let entity = new FplForInStmtEntity((pos1,pos2), forStmt)
        variableStack.PushEvalStack(entity) // add ForInStmtEntity
        eval st entityAst
        variableStack.PopEvalStack() // remove ForInStmtEntity
        let inDomain = new FplForInStmtDomain((pos1,pos2), forStmt)
        variableStack.PushEvalStack(inDomain) // add ForInStmtDomain
        eval st inDomainAst
        variableStack.PopEvalStack() // remove ForInStmtDomain
        statementListAst |> List.map (fun stmtAst -> eval st stmtAst) |> ignore
        variableStack.PopEvalStack() // remove ForInStmt
        st.EvalPop()
    // | SignatureWithPreConBlock of Ast * ((Ast list option * Ast) * Ast)
    | Ast.PremiseConclusionBlock((pos1, pos2), ((optVarDeclOrSpecList, premiseAst), conclusionAst)) ->
        st.EvalPush("PremiseConclusionBlock")
        optVarDeclOrSpecList |> Option.map (List.map (eval st) >> ignore) |> Option.defaultValue ()
        eval st premiseAst
        eval st conclusionAst
        st.EvalPop()
    | Ast.TheoremSignature((pos1, pos2), simpleSignatureAst) ->
        st.EvalPush("TheoremSignature")
        variableStack.InSignatureEvaluation <- true
        eval st simpleSignatureAst
        setSignaturePositions pos1 pos2
        variableStack.InSignatureEvaluation <- false
        st.EvalPop()
    | Ast.Theorem((pos1, pos2), (signatureAst, (optVarDeclOrSpecList, predicateAst))) ->
        st.EvalPush("Theorem")
        let parent = variableStack.PeekEvalStack()
        let fv = new FplTheorem((pos1, pos2), parent, variableStack.GetNextAvailableFplBlockRunOrder)
        variableStack.PushEvalStack(fv)
        eval st signatureAst
        evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst
        emitVAR04diagnosticsOld fv
        variableStack.PopEvalStack()
        st.EvalPop()
    | Ast.LemmaSignature((pos1, pos2), simpleSignatureAst) ->
        st.EvalPush("LemmaSignature")
        variableStack.InSignatureEvaluation <- true
        eval st simpleSignatureAst
        setSignaturePositions pos1 pos2
        variableStack.InSignatureEvaluation <- false
        st.EvalPop()
    | Ast.Lemma((pos1, pos2), (signatureAst, (optVarDeclOrSpecList, predicateAst))) ->
        st.EvalPush("Lemma")
        let parent = variableStack.PeekEvalStack()
        let fv = new FplLemma((pos1, pos2), parent, variableStack.GetNextAvailableFplBlockRunOrder)
        variableStack.PushEvalStack(fv)
        eval st signatureAst
        evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst
        emitVAR04diagnosticsOld fv
        variableStack.PopEvalStack()
        st.EvalPop()
    | Ast.PropositionSignature((pos1, pos2), simpleSignatureAst) ->
        st.EvalPush("PropositionSignature")
        variableStack.InSignatureEvaluation <- true
        eval st simpleSignatureAst
        setSignaturePositions pos1 pos2
        variableStack.InSignatureEvaluation <- false
        st.EvalPop()
    | Ast.Proposition((pos1, pos2), (signatureAst, (optVarDeclOrSpecList, predicateAst))) ->
        st.EvalPush("Proposition")
        let parent = variableStack.PeekEvalStack()
        let fv = new FplProposition((pos1, pos2), parent, variableStack.GetNextAvailableFplBlockRunOrder)
        variableStack.PushEvalStack(fv)
        eval st signatureAst
        evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst
        emitVAR04diagnosticsOld fv
        variableStack.PopEvalStack()
        st.EvalPop()
    | Ast.ConjectureSignature((pos1, pos2), simpleSignatureAst) ->
        st.EvalPush("ConjectureSignature")
        variableStack.InSignatureEvaluation <- true
        eval st simpleSignatureAst
        setSignaturePositions pos1 pos2
        variableStack.InSignatureEvaluation <- false
        st.EvalPop()
    | Ast.Conjecture((pos1, pos2), (signatureAst, (optVarDeclOrSpecList, predicateAst))) ->
        st.EvalPush("Conjecture")
        let parent = variableStack.PeekEvalStack()
        let fv = new FplConjecture((pos1, pos2), parent, variableStack.GetNextAvailableFplBlockRunOrder)
        variableStack.PushEvalStack(fv)
        eval st signatureAst
        evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst
        emitVAR04diagnosticsOld fv
        variableStack.PopEvalStack()
        st.EvalPop()
    | Ast.AxiomSignature((pos1, pos2), simpleSignatureAst) ->
        st.EvalPush("AxiomSignature")
        variableStack.InSignatureEvaluation <- true
        eval st simpleSignatureAst
        setSignaturePositions pos1 pos2
        variableStack.InSignatureEvaluation <- false
        st.EvalPop()
    | Ast.Axiom((pos1, pos2), (signatureAst, (optVarDeclOrSpecList, predicateAst))) ->
        st.EvalPush("Axiom")
        let parent = variableStack.PeekEvalStack()
        let fv = new FplAxiom((pos1, pos2), parent, variableStack.GetNextAvailableFplBlockRunOrder)
        variableStack.PushEvalStack(fv)
        eval st signatureAst
        evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst
        emitVAR04diagnosticsOld fv
        variableStack.PopEvalStack()
        st.EvalPop()
    | Ast.CorollarySignature((pos1, pos2), (simpleSignatureAst, dollarDigitListAsts)) ->
        st.EvalPush("CorollarySignature")
        variableStack.InSignatureEvaluation <- true
        eval st simpleSignatureAst
        dollarDigitListAsts |> List.map (eval st) |> ignore
        setSignaturePositions pos1 pos2
        variableStack.InSignatureEvaluation <- false
        st.EvalPop()
    | Ast.Corollary((pos1, pos2), (corollarySignatureAst, (optVarDeclOrSpecList, predicateAst))) ->
        st.EvalPush("Corollary")
        let parent = variableStack.PeekEvalStack()
        let fv = new FplCorollary((pos1, pos2), parent, variableStack.GetNextAvailableFplBlockRunOrder)
        variableStack.PushEvalStack(fv)
        eval st corollarySignatureAst
        variableStack.PopEvalStack() // add to parent theorem (if any) 
        variableStack.PushEvalStack(fv) // push again to have the current corollary on stack
        evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst
        // now, we are ready to emit VAR03 diagnostics for all variables declared in the signature of the corollary.
        emitVAR04diagnosticsOld fv
        variableStack.Pop() |> ignore // pop without 
        st.EvalPop()
    // | NamedVarDecl of Positions * ((Ast list * Ast) * Ast)
    | Ast.NamedVarDecl((pos1, pos2), ((variableListAst, varDeclModifierAst), variableTypeAst)) ->
        st.EvalPush("NamedVarDecl")
        let parent = variableStack.PeekEvalStack()
        parent.AuxiliaryInfo <- variableListAst |> List.length // remember how many variables to create
        // create all variables of the named variable declaration in the current scope
        variableListAst |> List.iter (fun varAst ->
            match varDeclModifierAst with 
            | Ast.Many((posMan1, posMan2),()) ->
                checkVAR00Diagnostics parent.AuxiliaryInfo posMan1 posMan2        
                match varAst with 
                | Ast.Var((varPos1, varPos2), varName) ->
                    let newVar = new FplVariableMany(varName, (varPos1, varPos2), parent)
                    newVar.IsSignatureVariable <- (variableStack.InSignatureEvaluation && not (isVar parent))
                    variableStack.PushEvalStack(newVar)
                    eval st variableTypeAst
                    variableStack.PopEvalStack()
                | _ -> ()
            | Ast.Many1((posMan1, posMan2),()) ->
                checkVAR00Diagnostics parent.AuxiliaryInfo posMan1 posMan2        
                match varAst with 
                | Ast.Var((varPos1, varPos2), varName) ->
                    let newVar = new FplVariableMany1(varName, (varPos1, varPos2), parent)
                    newVar.IsSignatureVariable <- (variableStack.InSignatureEvaluation && not (isVar parent))
                    variableStack.PushEvalStack(newVar)
                    eval st variableTypeAst
                    variableStack.PopEvalStack()
                | _ -> ()
            | Ast.One((_, _),()) ->
                match varAst with 
                | Ast.Var((varPos1, varPos2), varName) ->
                    let newVar = new FplVariable(varName, (varPos1, varPos2), parent)
                    newVar.IsSignatureVariable <- (variableStack.InSignatureEvaluation && not (isVar parent))
                    variableStack.PushEvalStack(newVar)
                    eval st variableTypeAst
                    variableStack.PopEvalStack()
                | _ -> ()
            | _ -> ()
        ) |> ignore 
        st.EvalPop()
    // | Axiom of Constructor * (Ast * (Ast list option * Ast))
    | Ast.Constructor((pos1, pos2), (signatureAst, optVarDeclOrSpecListAst)) ->
        st.EvalPush("Constructor")
        let parent = variableStack.PeekEvalStack()
        let fv = new FplConstructor((pos1, pos2), parent)
        variableStack.PushEvalStack(fv)
        eval st signatureAst
        // evaluate the declaration block
        match optVarDeclOrSpecListAst with
        | Some astList -> astList |> List.map (eval st) |> ignore
        | None -> ()
        variableStack.PopEvalStack()
        st.EvalPop()
    // | DefPredicateContent of Ast list option * Ast
    | Ast.DefPredicateContent(optAsts, ast1) ->
        st.EvalPush("DefPredicateContent")
        optAsts
        |> Option.map (List.map (eval st) >> ignore)
        |> Option.defaultValue ()
        |> ignore
        eval st ast1
        st.EvalPop()
    | Ast.DefFunctionContent(optAsts, ast1) ->
        st.EvalPush("DefFunctionContent")
        optAsts
        |> Option.map (List.map (eval st) >> ignore)
        |> Option.defaultValue ()
        |> ignore
        eval st ast1
        st.EvalPop()
    // | DefClassCompleteContent of Ast list option * Ast list
    | Ast.DefClassCompleteContent(optVarDeclOrSpecListAsts, constructorListAsts) ->
        st.EvalPush("DefClassCompleteContent")
        optVarDeclOrSpecListAsts |> Option.map (List.map (eval st) >> ignore) |> Option.defaultValue ()
        constructorListAsts |> List.map (eval st) |> ignore
        st.EvalPop()
    // | DefinitionPredicate of Positions * (Ast * (Ast * Ast list option))
    | Ast.DefinitionPredicate((pos1, pos2), (predicateSignature, optDefBlock)) ->
        st.EvalPush("DefinitionPredicate")
        let parent = variableStack.PeekEvalStack()
        let fv = new FplPredicate((pos1, pos2), parent, variableStack.GetNextAvailableFplBlockRunOrder)
        variableStack.PushEvalStack(fv)
        eval st predicateSignature
        match optDefBlock with 
        | Some (predicateContentAst, optPropertyListAsts) ->
            eval st predicateContentAst
            optPropertyListAsts |> Option.map (List.map (eval st) >> ignore) |> Option.defaultValue ()
        | None -> fv.IsIntrinsic <- true
        variableStack.PopEvalStack()
        st.EvalPop()
    // | DefinitionFunctionalTerm of Positions * ((Ast * Ast) * (Ast * Ast list option))
    | Ast.DefinitionFunctionalTerm((pos1, pos2), (functionalTermSignatureAst, optDefBlock)) ->
        st.EvalPush("DefinitionFunctionalTerm")
        let parent = variableStack.PeekEvalStack()
        let fv = new FplFunctionalTerm((pos1, pos2), parent, variableStack.GetNextAvailableFplBlockRunOrder)
        variableStack.PushEvalStack(fv)
        match optDefBlock with 
        | Some (funcContentAst, optPropertyListAsts) ->
            eval st funcContentAst
            optPropertyListAsts |> Option.map (List.map (eval st) >> ignore) |> Option.defaultValue ()
        | None -> fv.IsIntrinsic <- true
        eval st functionalTermSignatureAst 
        if not fv.IsIntrinsic then // if not intrinsic, check variable usage
            emitVAR04diagnosticsOld fv
        variableStack.PopEvalStack()
        st.EvalPop()
    | Ast.ClassSignature((pos1, pos2), simpleSignatureAst) ->
        st.EvalPush("ClassSignature")
        variableStack.InSignatureEvaluation <- true
        eval st simpleSignatureAst
        setSignaturePositions pos1 pos2
        variableStack.InSignatureEvaluation <- false
        st.EvalPop()
    | Ast.DefinitionClass((pos1, pos2),(((classSignatureAst, optInheritedClassTypeListAst), optUserDefinedObjSymAst), optDefBlock)) ->
        st.EvalPush("DefinitionClass")
        let parent = variableStack.PeekEvalStack()
        let fv = new FplClass((pos1, pos2), parent)
        variableStack.PushEvalStack(fv)
        eval st classSignatureAst
        optUserDefinedObjSymAst |> Option.map (eval st) |> Option.defaultValue ()
        match optDefBlock with 
        | Some (classContentAst, optPropertyListAsts) ->
            eval st classContentAst
            optPropertyListAsts |> Option.map (List.map (eval st) >> ignore) |> Option.defaultValue ()
        | None -> fv.IsIntrinsic <- true
        optInheritedClassTypeListAst |> Option.map (eval st) |> Option.defaultValue ()
        emitVAR04diagnosticsOld fv
        variableStack.PopEvalStack()
        st.EvalPop()
    // | DerivedPredicate of Ast
    | Ast.DerivedPredicate ((pos1, pos2),predicateAst) -> 
        st.EvalPush("DerivedPredicate")
        let fv = variableStack.PeekEvalStack()
        let argInf = new FplArgInferenceDerived((pos1, pos2), fv) 
        variableStack.PushEvalStack(argInf)
        eval st predicateAst
        variableStack.PopEvalStack()
        st.EvalPop()
    // | Proof of Positions * (Ast * (Ast list * Ast option))
    | Ast.Proof((pos1, pos2), (referencingIdentifierAst, (proofArgumentListAst, optQedAst))) ->
        st.EvalPush("Proof")
        let parent = variableStack.PeekEvalStack()
        let fv = new FplProof((pos1, pos2), parent, variableStack.GetNextAvailableFplBlockRunOrder)
        variableStack.PushEvalStack(fv)
        eval st referencingIdentifierAst
        variableStack.PopEvalStack() // add to parent theorem (if any)
        variableStack.PushEvalStack(fv) // push again
        proofArgumentListAst |> List.map (eval st) |> ignore
        optQedAst |> Option.map (eval st) |> Option.defaultValue ()
        emitVAR04diagnosticsOld fv
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
                | _ -> value.FplId <- LiteralFalse // todo all other arguments that are either undetermined or false should issue an error
            | _ -> () // todo argumentinference not found
        )
        fv.ValueList.Add(value)
        variableStack.Pop() |> ignore // pop without embedding in theorem (already done)
        st.EvalPop()
    | Ast.Precedence((pos1, pos2), precedence) ->
        st.EvalPush("Precedence")
        let fv = variableStack.PeekEvalStack()
        fv.AuxiliaryInfo <- precedence
        st.EvalPop()
    // Positions * ((Ast * Ast list) * Ast) 
    | Ast.JustificationIdentifier((pos1, pos2), (((byModifierOption, predicateIdentifierAst), dollarDigitListAsts), refArgumentIdentifierAst)) ->
        st.EvalPush("JustificationIdentifier")
        let parent = variableStack.PeekEvalStack()

        let checkDiagnostics (fvJi:FplGenericJustificationItem) candidates = 
            match tryFindAssociatedBlockForJustificationItem fvJi candidates with
            | ScopeSearchResult.FoundAssociate potentialCandidate -> 
                fvJi.Scope.TryAdd(fvJi.FplId, potentialCandidate) |> ignore
                match fvJi with 
                | :? FplJustificationItemByProofArgument ->
                    let split = fvJi.FplId.Split(":")
                    if split.Length > 1 then 
                        // here, argName is the argument identifier of the other proof
                        let argName = $"{split.[1]}"
                        match getArgumentInProof fvJi argName with
                        | Some argument -> fvJi.ArgList.Add(argument) 
                        | _ -> emitPR006Diagnostics fvJi.FplId argName fvJi.StartPos fvJi.EndPos 
                | _ -> ()
            | ScopeSearchResult.FoundIncorrectBlock otherBlock ->
                let alternative = 
                    match fvJi with 
                    | :? FplJustificationItemByAx ->
                        "Expected a reference to an axiom."
                    | :? FplJustificationItemByConj ->
                        "Expected a reference to a conjecture."
                    | :? FplJustificationItemByCor ->
                        "Expected a reference to a corollary ."
                    | :? FplJustificationItemByDef ->
                        "Expected a reference to a definition (of a class, a predicate, or a functional term)."
                    | :? FplJustificationItemByDefVar ->
                        "Expected a reference to a variable."
                    | :? FplJustificationItemByInf ->
                        "Expected a reference to a rule of inference."
                    | :? FplJustificationItemByProofArgument ->
                        "Expected a reference to an argument in onother proof."
                    | :? FplJustificationItemByRefArgument ->
                        "Expected a reference to a previous argument in this proof."
                    | :? FplJustificationItemByTheoremLikeStmt ->
                        "Expected a reference to a theorem, a lemma, or a proposition."
                    | _ -> "Expected another reference."
                emitPR001Diagnostics otherBlock fvJi.Name fvJi.StartPos fvJi.EndPos alternative
            | ScopeSearchResult.NotFound ->
                emitID010Diagnostics fvJi.FplId fvJi.StartPos fvJi.EndPos
            | ScopeSearchResult.FoundMultiple listOfKandidates ->
                emitID023Diagnostics listOfKandidates fvJi.StartPos fvJi.EndPos
            | _ -> ()


        match byModifierOption, dollarDigitListAsts, refArgumentIdentifierAst with
        | Some LiteralByAx, Some _, None -> 
            // byax justification cannot be used together with a proof or corollary reference
            emitPR010Diagnostics LiteralByAx LiteralAxL pos1 pos2 
        | Some LiteralByAx, Some _, Some _ -> 
            // byax justification cannot be used together with a proof argument reference 
            emitPR011Diagnostics LiteralByAx LiteralAxL pos1 pos2 
        | Some LiteralByAx, None, None -> 
            let fvJi = new FplJustificationItemByAx((pos1, pos2), parent)
            variableStack.PushEvalStack(fvJi)
            eval st predicateIdentifierAst
            // check, if indeed the predicateId points to an axiom, if not issue diagnostics
            let candidates = findCandidatesByName st fvJi.FplId false false
            checkDiagnostics fvJi candidates
            variableStack.PopEvalStack()
        | Some LiteralByConj, Some _, None -> 
            // byconj justification cannot be used together with a proof reference
            emitPR010Diagnostics LiteralByConj LiteralConjL pos1 pos2 
        | Some LiteralByConj, Some _, Some _ -> 
            // byconj justification cannot be used together with a proof argument reference 
            emitPR011Diagnostics LiteralByConj LiteralConjL pos1 pos2 
        | Some LiteralByConj, None, None -> 
            let fvJi = new FplJustificationItemByConj((pos1, pos2), parent)
            variableStack.PushEvalStack(fvJi)
            eval st predicateIdentifierAst
            // check, if indeed the predicateId points to a conjecture, if not issue diagnostics
            let candidates = findCandidatesByName st fvJi.FplId false false
            checkDiagnostics fvJi candidates
            variableStack.PopEvalStack()
        | Some LiteralByCor, Some _, _ -> 
            let fvJi = new FplJustificationItemByCor((pos1, pos2), parent)
            variableStack.PushEvalStack(fvJi)
            eval st predicateIdentifierAst
            dollarDigitListAsts.Value |> List.map (eval st) |> ignore
            let candidates = findCandidatesByName st fvJi.FplId false true
            // check, if indeed the predicateId points to a corollary, if not issue diagnostics
            checkDiagnostics fvJi candidates
            variableStack.PopEvalStack()
        | Some LiteralByCor, None, _ -> 
            // byCor justification a reference to a corollary
            emitPR012Diagnostics pos1 pos2 
        | Some LiteralByDef, Some _, None -> 
            // byDef justification cannot be used together with a proof reference
            emitPR010Diagnostics LiteralByDef LiteralDefL pos1 pos2 
        | Some LiteralByDef, Some _, Some _ -> 
            // byDef justification cannot be used together with a proof argument reference 
            emitPR011Diagnostics LiteralByDef LiteralDefL pos1 pos2 
        | Some LiteralByDef, None, None -> 
            let fvJi = new FplJustificationItemByDef((pos1, pos2), parent)
            variableStack.PushEvalStack(fvJi)
            eval st predicateIdentifierAst
            // check, if indeed the predicateId points to a definition, if not issue diagnostics
            let candidates = findCandidatesByName st fvJi.FplId true false
            checkDiagnostics fvJi candidates
            variableStack.PopEvalStack()
        | Some LiteralByInf, Some _, None -> 
            // byInf justification cannot be used together with a proof reference
            emitPR010Diagnostics LiteralByInf PrimRuleOfInference pos1 pos2 
        | Some LiteralByInf, Some _, Some _ -> 
            // byInf justification cannot be used together with a proof argument reference 
            emitPR011Diagnostics LiteralByInf PrimRuleOfInference pos1 pos2 
        | Some LiteralByInf, None, None -> 
            let fvJi = new FplJustificationItemByInf((pos1, pos2), parent)
            variableStack.PushEvalStack(fvJi)
            eval st predicateIdentifierAst
            // check, if indeed the predicateId points to a rule of inference, if not issue diagnostics
            let candidates = findCandidatesByName st fvJi.FplId false false
            checkDiagnostics fvJi candidates
            variableStack.PopEvalStack()
        | Some _, _, _ -> () // does not occur, because the parser byModifier choices between only two keywords LiteralByAx or LiteralByDef
        | None, Some _, None -> 
            let fvJi = new FplJustificationItemByCor((pos1, pos2), parent)
            variableStack.PushEvalStack(fvJi)
            eval st predicateIdentifierAst
            dollarDigitListAsts.Value |> List.map (eval st) |> ignore
            let candidates = findCandidatesByName st fvJi.FplId false true
            // check, if indeed the predicateId points to a corollary, if not issue diagnostics
            checkDiagnostics fvJi candidates
            // issue info diagnostics that references to a corollary need the keyword byCor to increase readability
            emitPR013Diagnostics pos1 pos2
            variableStack.PopEvalStack()
        | None, Some _, Some _ -> 
            let fvJi = new FplJustificationItemByProofArgument((pos1, pos2), parent)
            variableStack.PushEvalStack(fvJi)
            eval st predicateIdentifierAst
            dollarDigitListAsts.Value |> List.map (eval st) |> ignore
            eval st refArgumentIdentifierAst.Value 
            let splitOffAnyArgumentId (input: string) =
                let parts = input.Split(':')
                if parts.Length > 0 then parts.[0] else ""
            let name = splitOffAnyArgumentId fvJi.FplId
            let candidates = findCandidatesByName st name false true 
            let candidatesFiltered = 
                if candidates.Length > 1 then 
                    candidates |> List.filter (fun fv -> fv.FplId = name)
                else
                    candidates
            // check, if indeed the predicateId points to another proof, if not issue diagnostics, 
            // also check if arg exists, if not issue diagnostics
            checkDiagnostics fvJi candidatesFiltered
            variableStack.PopEvalStack()
        | None, None, Some _ ->  
            // issue diagnostics a theorem-like statement justification cannot be used together with a proof argument reference 
            emitPR014Diagnostics pos1 pos2 
        | None, None, None -> 
            let fvJi = new FplJustificationItemByTheoremLikeStmt((pos1, pos2), parent)
            variableStack.PushEvalStack(fvJi)
            eval st predicateIdentifierAst
            let candidates = findCandidatesByName st fvJi.FplId false false
            // check if indeed the predicateId points to a theorem-like statement except a corollary, if not issue diagnostics
            checkDiagnostics fvJi candidates
            variableStack.PopEvalStack()
        st.EvalPop()


let tryFindParsedAstUsesClausesEvaluated (parsedAsts: List<ParsedAst>) =
    if parsedAsts.Exists(fun pa -> pa.Status = ParsedAstStatus.UsesClausesEvaluated) then
        Some(parsedAsts.Find(fun pa -> pa.Status = ParsedAstStatus.UsesClausesEvaluated))
    else
        None

let evaluateSymbolTable (st: SymbolTable) =
    st.OrderAsts()

    let mutable found = true

    while found do
        let usesClausesEvaluatedParsedAst =
            tryFindParsedAstUsesClausesEvaluated st.ParsedAsts

        match usesClausesEvaluatedParsedAst with
        | Some pa ->
            variableStack.ClearEvalStack()
            // evaluate the ParsedAst of a theory
            let theoryValue = new FplTheory(pa.Id, st.Root, pa.Parsing.Uri.AbsolutePath, variableStack.GetNextAvailableFplBlockRunOrder);
            if not (st.Root.Scope.ContainsKey(pa.Id)) then
                st.Root.Scope.Add(pa.Id, theoryValue)
            else
                st.Root.Scope[pa.Id] <- theoryValue
            variableStack.PushEvalStack(theoryValue)
            ad.CurrentUri <- pa.Parsing.Uri
            eval st pa.Parsing.Ast
            pa.Status <- ParsedAstStatus.Evaluated
            variableStack.PopEvalStack()
        | None -> found <- false

