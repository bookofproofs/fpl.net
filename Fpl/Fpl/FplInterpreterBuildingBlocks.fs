/// This module evaluates the abstract syntax tree (AST) and interprets its semantics.
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

let eval_string (st: SymbolTable) s = ()

let eval_pos_string (st: SymbolTable) (startpos: Position) (endpos: Position) ast = ()

let eval_pos_ast_ast_opt (st: SymbolTable) (startpos: Position) (endpos: Position) = ()

let eval_pos_char_list (st: SymbolTable) (startpos: Position) (endpos: Position) charlist =
    charlist |> List.map string |> String.concat "" |> ignore

let eval_pos_string_ast (st: SymbolTable) str = ()

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
        | :? FplVariable as v -> 
            if v.IsMany then
                fv.TypeId <- $"*{fv.TypeId}"
            elif v.IsMany1 then
                fv.TypeId <- $"+{fv.TypeId}"
            else 
                ()
        | _ -> ()
    
    let setSignaturePositions pos1 pos2 = 
        let fv = variableStack.PeekEvalStack()
        match box fv with 
        | :? IHasSignature as withSignature -> 
            withSignature.SignStartPos <- pos1  
            withSignature.SignEndPos <- pos2
        | _ -> ()

    match ast with
    // units: | Star
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
        match checkID009_ID010_ID011_Diagnostics st fv LiteralObj pos1 pos2 with
        | Some classNode -> 
            fv.ArgList.Add classNode
        | None -> ()
        checkID012Diagnostics st fv LiteralObj pos1 pos2 
        // add potential parent class call for this identifier (if it is one) 
        let path = st.EvalPath()
        if path.Contains("DefinitionClass.InheritedClassType") then 
            variableStack.ParentClassCalls.TryAdd(LiteralObj, None) |> ignore
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
        let fv = variableStack.PeekEvalStack()
        match fv.Parent with 
        | Some parent -> 
            checkVAR00Diagnostics parent.AuxiliaryInfo pos1 pos2
        | _ -> ()
        match fv with 
        | :? FplVariable as var -> var.SetToMany()
        | _ -> ()
        st.EvalPop()
    | Ast.Many1((pos1, pos2),()) ->
        st.EvalPush("Many1")
        let fv = variableStack.PeekEvalStack()
        match fv.Parent with 
        | Some parent -> 
            checkVAR00Diagnostics parent.AuxiliaryInfo pos1 pos2
        | _ -> ()
        match fv with 
        | :? FplVariable as var -> var.SetToMany1()
        | _ -> ()
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
    | Ast.PascalCaseId s -> 
        st.EvalPush(PrimPascalCaseId)
        let fv = variableStack.PeekEvalStack()
        eval_string st s
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
        | PrimOptionalFunctionalTermL
        | PrimMandatoryPredicateL
        | PrimOptionalPredicateL
        | PrimPredicateL
        | PrimFuncionalTermL
        | PrimRuleOfInference -> 
            fv.FplId <- s
        | LiteralCtorL
        | PrimClassL ->
            fv.FplId <- s
            fv.TypeId <- s
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
            if fv.TypeId <> "" then
                fv.TypeId <- fv.TypeId + sid
            else
                fv.TypeId <- LiteralInd
                    
            fv.EndPos <- pos2
        st.EvalPop() 
    | Ast.ExtensionName((pos1, pos2), s) ->
        st.EvalPush("ExtensionName")
        let fv = variableStack.PeekEvalStack()
        let extensionName = $"@{s}"
        match fv with 
        | :? FplExtension ->
            fv.FplId <- extensionName
            fv.TypeId <- extensionName
            fv.StartPos <- pos1
            fv.EndPos <- pos2
        | :? FplVariable as var when var.IsMany -> 
            let sid = $"*{extensionName}"
            fv.TypeId <- sid
        | :? FplVariable as var when var.IsMany1 -> 
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
        let isDeclaration = evalPath.Contains("NamedVarDecl.")  
        let isLocalizationDeclaration = evalPath.StartsWith("AST.Namespace.Localization.Expression.")
        let isExtensionDeclaration = evalPath.Contains("ExtensionAssignment.Var")
        let diagnosticsStopFlag = ad.DiagnosticsStopped
        ad.DiagnosticsStopped <- false // enable var-related diagnostics in AST.Var, even if it was stopped (e.g. in Ast.Localization)
        let fv = variableStack.PeekEvalStack()
        let varValue = new FplVariable((pos1, pos2), fv) 
        varValue.FplId <- name
        varValue.TypeId <- LiteralUndef
        let undefined = new FplIntrinsicUndef((pos1, pos2), varValue)  
        varValue.SetValue(undefined)
        varValue.IsSignatureVariable <- variableStack.InSignatureEvaluation 
        if isDeclaration then 
            // check for VAR03 diagnostics
            match variableInBlockScopeByName fv name true with 
            | ScopeSearchResult.Found other ->
                // replace the variable by other on stack
                emitVAR03diagnostics varValue other 
            | _ -> ()

            match variableInBlockScopeByName fv name false with 
            | ScopeSearchResult.Found other ->
                // replace the variable by other on stack
                variableStack.PushEvalStack(other)
            | _ -> 
                variableStack.PushEvalStack(varValue)

        elif isExtensionDeclaration then 
            fv.Scope.Add(name, varValue)
        elif isLocalizationDeclaration then 
            match variableInBlockScopeByName fv name false with 
            | ScopeSearchResult.Found other ->
                emitVAR03diagnostics varValue other 
            | _ -> 
                let rec getLocalization (fValue:FplValue) = 
                    match fValue with
                    | :? FplLocalization -> fValue
                    | _ ->
                        match fValue.Parent with
                        | Some parent -> getLocalization parent
                        | None -> fValue
                let loc = getLocalization fv
                loc.Scope.Add(name, varValue)
                // Add the variable to the reference in the localization
                variableStack.PushEvalStack(varValue)
                variableStack.PopEvalStack()
        else
            match variableInBlockScopeByName fv name true with 
            | ScopeSearchResult.Found other -> 
                match fv with
                | :? FplReference ->
                    if not (fv.Scope.ContainsKey(name)) then
                        fv.Scope.Add(name, other)
                | _ -> ()
                // count usages of the variable in scope
                other.AuxiliaryInfo <- other.AuxiliaryInfo + 1
            | _ -> 
                // otherwise emit variable not declared if this is not a declaration 
                emitVAR01diagnostics name pos1 pos2
                if fv.Name = PrimRefL then 
                    // for references, still add the variable to the scope of the reference. 
                    // It will then be treated as a "variable" that is undefined
                    fv.Scope.Add(name, varValue)
            fv.FplId <- name
            fv.TypeId <- LiteralUndef
        ad.DiagnosticsStopped <- diagnosticsStopFlag
        st.EvalPop() 
    | Ast.DelegateId((pos1, pos2), s) -> 
        st.EvalPush("DelegateId")
        let fv = variableStack.PeekEvalStack()
        fv.FplId <- s
        st.EvalPop() 
    | Ast.Alias((pos1, pos2), s) -> 
        st.EvalPush("Alias")
        eval_pos_string st pos1 pos2 s
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
    | Ast.ArgumentIdentifier((pos1, pos2), s) -> 
        st.EvalPush("ArgumentIdentifier")
        let fv = variableStack.PeekEvalStack()
        fv.FplId <- s.Substring(0,s.Length-1) // argument id without the "." at the end
        st.EvalPop() 
    | Ast.RefArgumentIdentifier((pos1, pos2), s) -> 
        st.EvalPush("RefArgumentIdentifier")
        let fv = variableStack.PeekEvalStack()
        match fv.Name with 
        | PrimJIByProofArgument -> fv.FplId <- $"{fv.FplId}:{s}"
        | PrimArgInfRevoke -> 
            let fvAi = fv :?> FplArgInferenceRevoke
            let arg = fvAi.ParentArgument
            let proof = arg.ParentProof
            proof.RevokeArgument s pos1 pos2 
            fvAi.FplId <- s
        | PrimJustificationL -> 
            let fvAi = new FplJustificationItemByRefArgument((pos1, pos2), fv)
            fvAi.FplId <- s
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
        let rb = variableStack.PeekEvalStack()
        rb.StartPos <- pos1
        rb.EndPos <- pos2
        rb.FplId <- LiteralSelf
        rb.TypeId <- LiteralSelf
        let oldDiagnosticsStopped = ad.DiagnosticsStopped
        ad.DiagnosticsStopped <- false
        let referencedBlock = nextDefinition rb 0
        match referencedBlock with
        | ScopeSearchResult.FoundIncorrectBlock name -> 
            emitID015diagnostics name rb
        | ScopeSearchResult.Found block -> 
            rb.Scope.Add(rb.FplId, block)
        | ScopeSearchResult.FoundMultiple name -> 
            emitID016diagnostics name rb
        | ScopeSearchResult.NotFound -> 
            emitID016diagnostics "(no block found)" rb
        | _ -> ()
        ad.DiagnosticsStopped <- oldDiagnosticsStopped
        st.EvalPop() 
    | Ast.Parent((pos1, pos2), _) -> 
        st.EvalPush("Parent")
        let rb = variableStack.PeekEvalStack()
        rb.StartPos <- pos1
        rb.EndPos <- pos2
        rb.FplId <- LiteralParent
        rb.TypeId <- LiteralParent
        let oldDiagnosticsStopped = ad.DiagnosticsStopped
        ad.DiagnosticsStopped <- false
        let referencedBlock = nextDefinition rb 1
        match referencedBlock with
        | ScopeSearchResult.FoundIncorrectBlock name -> 
            emitID015diagnostics name rb
        | ScopeSearchResult.Found block -> 
            rb.Scope.Add(rb.FplId, block)
        | ScopeSearchResult.FoundMultiple name -> 
            emitID016diagnostics name rb
        | ScopeSearchResult.NotFound -> 
            emitID016diagnostics "(no block found)" rb
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
        emitVAR04diagnostics fv
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
        let proof = fvNew.ParentArgument.ParentProof
        proof.AssumeArgument (fvNew)
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
    | Ast.PredicateIdentifier((pos1, pos2), asts) ->
        st.EvalPush("PredicateIdentifier")

        let pascalCaseIdList = asts |> List.collect (function Ast.PascalCaseId s -> [s] | _ -> [])
        let identifier = String.concat "." pascalCaseIdList
        let evalPath = st.EvalPath()
        let fv = variableStack.PeekEvalStack()

        match fv with 
        | :? FplTheorem
        | :? FplLemma
        | :? FplProposition
        | :? FplCorollary
        | :? FplConjecture
        | :? FplPredicate
        | :? FplAxiom
        | :? FplRuleOfInference ->
            fv.FplId <- identifier
            fv.TypeId <- LiteralPred
        | :? FplClass -> 
            if evalPath.EndsWith("InheritedClassType.PredicateIdentifier") then 
                match checkID009_ID010_ID011_Diagnostics st fv identifier pos1 pos2 with
                | Some classNode -> 
                    // add known class
                    fv.ArgList.Add classNode
                | None -> ()
                // add potential parent class call for this identifier
                let path = st.EvalPath()
                if path.Contains("DefinitionClass.InheritedClassType") then 
                    variableStack.ParentClassCalls.TryAdd(identifier, None) |> ignore
            else
                fv.FplId <- identifier
                fv.TypeId <- identifier
                match checkID009_ID010_ID011_Diagnostics st fv identifier pos1 pos2 with
                | Some classNode -> 
                    fv.ArgList.Add classNode
                | None -> ()

        | :? FplProof 
        | :? FplMandatoryPredicate
        | :? FplOptionalPredicate ->
            fv.FplId <- identifier
            fv.TypeId <- LiteralPred
        | :? FplMandatoryFunctionalTerm
        | :? FplOptionalFunctionalTerm
        | :? FplFunctionalTerm ->
            fv.FplId <- identifier
            fv.TypeId <- LiteralFunc
        | :? FplConstructor -> 
            fv.FplId <- identifier
            fv.TypeId <- identifier
            checkID008Diagnostics fv pos1 pos2
        | :? FplVariable as var when var.IsMany -> 
            fv.TypeId <- $"*{identifier}"
        | :? FplVariable as var when var.IsMany1 -> 
            fv.TypeId <- $"+{identifier}"
        | :? FplVariable as var when not (var.IsVariadic()) -> 
            fv.TypeId <- identifier
        | :? FplMapping -> 
            fv.TypeId <- fv.TypeId + identifier
        | :? FplReference -> 
            fv.FplId <- fv.FplId + identifier
            fv.TypeId <- fv.TypeId + identifier
            checkID012Diagnostics st fv identifier pos1 pos2
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
            | _ -> 
                match checkSIG04Diagnostics fv candidates with
                | Some candidate -> 
                    match fv with
                    | :? FplReference -> fv.Scope.Add(identifier, candidate)
                    | _ -> fv.ArgList.Add(candidate)
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
    | Ast.BracketedCoordsInType((pos1, pos2), asts) ->
        st.EvalPush("BracketedCoordsInType")
        let fv = variableStack.PeekEvalStack()
        fv.HasBrackets <- true
        asts 
        |> List.map (fun ast1 ->
            eval st ast1
        ) |> ignore
        fv.EndPos <- pos2
        st.EvalPop()
    | Ast.NamespaceIdentifier((pos1, pos2), asts) ->
        st.EvalPush("NamespaceIdentifier")
        asts |> List.map (eval st) |> ignore
        st.EvalPop()
    | Ast.LocalizationTerm((pos1, pos2), asts) ->
        st.EvalPush("LocalizationTerm")
        let fv = variableStack.PeekEvalStack()
        asts |> List.map (fun ebnfTerm ->
            let trsl = new FplTranslation((pos1, pos2), fv)
            variableStack.PushEvalStack(trsl)
            eval st ebnfTerm
            variableStack.PopEvalStack()
        ) |> ignore
        st.EvalPop()
    | Ast.LocalizationTermList((pos1, pos2), ebnfTermAsts) ->
        st.EvalPush("LocalizationTermList")
        let chooseRandomMember (lst: Ast list) =
            let rnd = Random()
            let index = rnd.Next(lst.Length)
            lst.[index]
        eval st (chooseRandomMember ebnfTermAsts)
        st.EvalPop()
    | Ast.BrackedCoordList((pos1, pos2), coordListAst) ->
        st.EvalPush("BrackedCoordList")
        let fv = variableStack.PeekEvalStack()
        fv.HasBrackets <- true
        if coordListAst.Length > 0 then 
            coordListAst 
            |> List.iter (fun pred -> 
                let ref = new FplReference((pos1, pos2), fv)
                variableStack.PushEvalStack(ref)
                eval st pred
                variableStack.PopEvalStack()
            ) 
        else
            let ref = new FplReference((pos1, pos2), fv)
            ref.FplId <- "???"
            ref.TypeId <- "???"
            variableStack.PushEvalStack(ref)
            variableStack.PopEvalStack()
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
        let fv = variableStack.PeekEvalStack()
        if predicateListAst.Length > 0 then 
            predicateListAst 
            |> List.iter (fun pred -> 
                eval st pred
            ) 
        else
            let ref = new FplReference((pos1, pos2), fv)
            ref.FplId <- "???"
            ref.TypeId <- "???"
            variableStack.PushEvalStack(ref)
            variableStack.PopEvalStack()
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
        eval_pos_ast_ast_opt st pos1 pos2
        st.EvalPop()
    | Ast.ClassType((pos1, pos2), (specificClassTypeAst, optbracketModifierAst)) ->
        st.EvalPush("ClassType")
        eval st specificClassTypeAst
        optbracketModifierAst |> Option.map (eval st) |> ignore
        eval_pos_ast_ast_opt st pos1 pos2
        st.EvalPop()
    | Ast.CompoundPredicateType((pos1, pos2), (ast1, optAst)) ->
        st.EvalPush("CompoundPredicateType")
        eval st ast1
        optAst |> Option.map (eval st) |> ignore
        eval_pos_ast_ast_opt st pos1 pos2
        st.EvalPop()
    | Ast.ReferenceToProofOrCorollary((pos1, pos2), (referencingIdentifierAst)) ->
        st.EvalPush("ReferenceToProofOrCorollary")
        eval st referencingIdentifierAst
        eval_pos_ast_ast_opt st pos1 pos2
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
                    refBlock.Scope.TryAdd(refBlock.FplId,matchedCandidate) |> ignore
                | _ -> ()

            variableStack.PopEvalStack()
        | None -> 
            // if no specification was found then simply continue in the same context
            eval st fplIdentifierAst
            // make sure, we still add a referenced node candidate to the scope of a reference
            let candidates = searchForCandidatesOfReferenceBlock fv
            let classes = candidates |> List.filter (fun c -> c.IsClass())
            let constructors = candidates |> List.filter (fun c -> isConstructor c) 
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
            elif candidates.Length > 0 then
                // not a class was referred, add the candidate (e.g., referenced variable)
                fv.Scope.TryAdd(fv.FplId, candidates.Head) |> ignore
            else
                ()

        simplifyTriviallyNestedExpressions fv |> ignore
        st.EvalPop()
    // | SelfAts of Positions * char list
    | Ast.SelfOrParent((pos1, pos2), selforParentAst) -> 
        st.EvalPush("SelfAts")
        eval st selforParentAst
        st.EvalPop()
    // | Translation of string * Ast
    | Ast.Translation((pos1, pos2),(langCode, ebnfAst)) ->
        st.EvalPush("Translation")
        let fv = variableStack.PeekEvalStack()
        let lang = new FplLanguage((pos1, pos2), fv) 
        variableStack.PushEvalStack(lang)
        eval st langCode
        let trsl = new FplTranslation((pos1, pos2), lang) 
        variableStack.PushEvalStack(trsl)
        eval st ebnfAst
        variableStack.PopEvalStack()
        variableStack.PopEvalStack()
        st.EvalPop()
    // | ExtensionBlock of Positions * (Ast * Ast)
    | Ast.InheritedClassType((pos1, pos2), ast1) -> 
        st.EvalPush("InheritedClassType")
        eval st ast1
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
    | Ast.Delegate((pos1, pos2), (fplDelegateIdentifierAst, argumentTupleAst)) ->
        st.EvalPush("Delegate")
        let fv = variableStack.PeekEvalStack()
        let refBlock = new FplReference((pos1, pos2), fv) 
        refBlock.FplId <- "del."
        refBlock.TypeId <- "del."
        variableStack.PushEvalStack(refBlock)
        eval st fplDelegateIdentifierAst
        eval st argumentTupleAst
        match refBlock.FplId with 
        | "Equal" -> 
            let deleg = new FplEquality((pos1, pos2), fv)
            deleg.Copy refBlock
            variableStack.Pop() |> ignore
            variableStack.PushEvalStack(deleg)
        | "Decrement" -> 
            let deleg = new FplDecrement((pos1, pos2), fv)
            deleg.Copy refBlock
            variableStack.Pop() |> ignore
            variableStack.PushEvalStack(deleg)
        | _ -> 
            refBlock.TypeId <- LiteralUndef
            emitID013Diagnostics pos1 pos2 $"Unknown delegate `{refBlock.FplId}`"  
        variableStack.PopEvalStack()
        st.EvalPop()
    // | ClosedOrOpenRange of Positions * ((Ast * Ast option) * Ast)
    | Ast.PredicateSignature((pos1, pos2), ((simpleSignatureAst, paramTupleAst), optUserDefinedSymbolAst)) ->
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
        st.EvalPop()
    | ProofSignature((pos1, pos2), (simpleSignatureAst, dollarDigitListAsts)) ->
        st.EvalPush("ProofSignature")
        variableStack.InSignatureEvaluation <- true
        eval st simpleSignatureAst
        dollarDigitListAsts |> List.map (eval st) |> ignore
        setSignaturePositions pos1 pos2
        variableStack.InSignatureEvaluation <- false
        st.EvalPop()
    | Ast.Localization((pos1, pos2), (predicateAst, translationListAsts)) ->
        st.EvalPush("Localization")
        let parent = variableStack.PeekEvalStack()
        let fv = new FplLocalization((pos1, pos2), parent)
        let diagList = List<Diagnostic>()
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
                let lanList = 
                    loc.Scope 
                    |> Seq.filter (fun kvp -> isLanguage kvp.Value) 
                    |> Seq.map (fun kvp -> kvp.Value) 
                    |> Seq.toList 
                    |> List.rev
                if not lanList.IsEmpty then
                    let lan = lanList.Head
                    diagList.Add(getVAR04diagnostic lan var.FplId)
            )
        ) |> ignore
        variableStack.PopEvalStack()
        ad.DiagnosticsStopped <- false // enable all diagnostics during localization
        diagList
        |> Seq.iter (fun diag -> ad.AddDiagnostic diag)
        st.EvalPop()
    | Ast.FunctionalTermInstance((pos1, pos2), ((optionalPropAst, functionalTerMInstanceSignatureAst), functionalTermInstanceBlockAst)) ->
        st.EvalPush("FunctionalTermInstance")
        let parent = variableStack.PeekEvalStack()
        match optionalPropAst with
        | Some _ -> 
                let fvNew = new FplOptionalFunctionalTerm((pos1, pos2), parent)
                variableStack.PushEvalStack(fvNew)
                eval st functionalTerMInstanceSignatureAst
                eval st functionalTermInstanceBlockAst
                variableStack.PopEvalStack()
        | None -> 
                let fvNew = new FplMandatoryFunctionalTerm((pos1, pos2), parent)
                variableStack.PushEvalStack(fvNew)
                eval st functionalTerMInstanceSignatureAst
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
    // | ExistsN of Positions * ((Ast * (Ast * Ast option)) * Ast)
    | Ast.ExistsN((pos1, pos2), ((dollarDigitsAst, namedVarDeclAst), predicateAst)) ->
        st.EvalPush("ExistsN")
        let parent = variableStack.PeekEvalStack()
        let fv = new FplQuantorExistsN((pos1, pos2), parent)
        variableStack.PushEvalStack(fv) // add exists n quantor
        eval st dollarDigitsAst
        eval st namedVarDeclAst
        eval st predicateAst
        emitVAR05diagnostics fv
        variableStack.PopEvalStack() // remove exists n quantor
        emitLG000orLG001Diagnostics fv PrimQuantorExistsN
        st.EvalPop()
    // | FunctionalTermSignature of Positions * (Ast * Ast)
    | Ast.FunctionalTermSignature((pos1, pos2), (((simpleSignatureAst, paramTupleAst), mappingAst), optUserDefinedSymbolAst)) -> 
        variableStack.InSignatureEvaluation <- true
        st.EvalPush("FunctionalTermSignature")
        eval st simpleSignatureAst
        eval st paramTupleAst
        eval st mappingAst
        optUserDefinedSymbolAst |> Option.map (eval st) |> Option.defaultValue () 
        setSignaturePositions pos1 pos2
        st.EvalPop()
        variableStack.InSignatureEvaluation <- false
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
        | :? FplMandatoryPredicate 
        | :? FplOptionalPredicate ->
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
        | :? FplLocalization -> 
            fv.FplId <- last.FplId
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
        eval st mappingAst
        setSignaturePositions pos1 pos2
        st.EvalPop()
        variableStack.InSignatureEvaluation <- false
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
        let dummyValue = new FplReference((pos1,pos2), fvNew)
        variableStack.PushEvalStack(dummyValue) // add value
        eval st predicateAst
        variableStack.PopEvalStack() // remove value
        variableStack.PopEvalStack() // remove Assignment
        st.EvalPop()
    | Ast.PredicateInstance((pos1, pos2), (keywordOptionalAst, (signatureAst, predInstanceBlockAst))) ->
        st.EvalPush("PredicateInstance")
        let parent = variableStack.PeekEvalStack()
        match keywordOptionalAst with
        | Some _ -> 
            let fvNew = new FplOptionalPredicate((pos1, pos2), parent)
            variableStack.PushEvalStack(fvNew)
            eval st signatureAst
            eval st predInstanceBlockAst
            if not fvNew.IsIntrinsic then // if not intrinsic, check variable usage
                emitVAR04diagnostics fvNew
            variableStack.PopEvalStack()
        | None -> 
            let fvNew = new FplMandatoryPredicate((pos1, pos2), parent)
            variableStack.PushEvalStack(fvNew)
            eval st signatureAst
            eval st predInstanceBlockAst
            if not fvNew.IsIntrinsic then // if not intrinsic, check variable usage
                emitVAR04diagnostics fvNew
            let d = fvNew.Type(SignatureType.Mixed)
            printf "%O" d
            variableStack.PopEvalStack()
        st.EvalPop()
    | Ast.ParentConstructorCall((pos1, pos2), (inheritedClassTypeAst, argumentTupleAst)) ->
        st.EvalPush("ParentConstructorCall")
        let fv = variableStack.PeekEvalStack()
        fv.StartPos <- pos1
        fv.EndPos <- pos2
        fv.FplId <- "bas"
        let refBlock = new FplReference((pos1, pos2), fv) 
        variableStack.PushEvalStack(refBlock)
        eval st inheritedClassTypeAst
        eval st argumentTupleAst
        variableStack.PopEvalStack()
        if fv.ArgList.Count>0 then
            let parentConstructorCallReference = fv.ArgList[0]
            let parentConstructorCallRefValue = getArgument parentConstructorCallReference
            match parentConstructorCallRefValue with
            | Some refVal -> 
                if variableStack.ParentClassCalls.ContainsKey(refVal.FplId) then
                    // Since the reference's id is the same as one of the classes this class is derived from,
                    let derivedClassOpt = getClassBlock parentConstructorCallReference
                    match derivedClassOpt with
                    | Some derivedClass ->
                        let parentClassFilterList = 
                            derivedClass.ArgList 
                            |> Seq.filter (fun pc -> pc.FplId = refVal.FplId)
                            |> Seq.toList
                        if parentClassFilterList.Length > 0 then
                            let parentClass = parentClassFilterList.Head
                            // add the found parent class to the parentClassCalls 
                            if Option.isNone variableStack.ParentClassCalls[refVal.FplId] then 
                                variableStack.ParentClassCalls[parentClass.FplId] <- Some parentClass
                                let (shadowedVars, shadowedProperties) = copyParentToDerivedClass parentClass derivedClass
                                shadowedVars
                                |> Seq.iter (fun name -> 
                                    emitVAR06iagnostic name derivedClass.FplId pos1
                                )
                            else
                                emitID021Diagnostics parentClass.FplId pos1
                    | None ->
                        // this case never happens, 
                        // if so the bug will become apparent by failing to call the parent class constructor
                        () 
                else 
                    () 
            | None -> 
                ()
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
        emitVAR04diagnostics fv
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
        emitVAR04diagnostics fv
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
        emitVAR04diagnostics fv
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
        emitVAR04diagnostics fv
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
        emitVAR04diagnostics fv
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
        match tryFindAssociatedBlockForCorollary fv with
        | ScopeSearchResult.FoundAssociate potentialParent -> 
            // everything is ok, change the parent of the provable from theory to the found parent 
            fv.Parent <- Some potentialParent
        | ScopeSearchResult.FoundIncorrectBlock block ->
            emitID005diagnostics fv block  
        | ScopeSearchResult.NotFound ->
            emitID006diagnostics fv  
        | ScopeSearchResult.FoundMultiple listOfKandidates ->
            emitID007Diagnostics fv.StartPos fv.EndPos (fv.Type(SignatureType.Type)) listOfKandidates  
        | _ -> ()
        evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst
        // now, we are ready to emit VAR03 diagnostics for all variables declared in the signature of the corollary.
        emitVAR03diagnosticsForCorollaryOrProofVariable fv  
        emitVAR04diagnostics fv
        variableStack.PopEvalStack()
        st.EvalPop()
    // | NamedVarDecl of Positions * ((Ast list * Ast) * Ast)
    | Ast.NamedVarDecl((pos1, pos2), ((variableListAst, varDeclModifierAst), variableTypeAst)) ->
        st.EvalPush("NamedVarDecl")
        let fv = variableStack.PeekEvalStack()
        fv.AuxiliaryInfo <- variableListAst |> List.length // remember how many variables to create
        // create all variables of the named variable declaration in the current scope
        variableListAst |> List.iter (fun varAst ->
            eval st varAst // here, the var is created and put on stack, but not popped
            eval st varDeclModifierAst
            eval st variableTypeAst
            variableStack.PopEvalStack() // take the var from stack 
        ) |> ignore 
        st.EvalPop()
    // | Axiom of Constructor * (Ast * (Ast list option * Ast))
    | Ast.Constructor((pos1, pos2), (signatureAst, optVarDeclOrSpecListAst)) ->
        st.EvalPush("Constructor")
        let parent = variableStack.PeekEvalStack()
        let fv = new FplConstructor((pos1, pos2), parent)
        variableStack.PushEvalStack(fv)
        eval st signatureAst
        
        // Initialize the counters of parent classes before evaluating the declaration block
        // of the constructor in which we want to count the calls to parent classes.
        // (we need to reset the counters for every constructor of the same class to avoid 
        // ID020 false positives for the wrong constructors)
        variableStack.ParentClassCountersInitialize()  

        // evaluate the declaration block
        match optVarDeclOrSpecListAst with
        | Some astList -> astList |> List.map (eval st) |> ignore
        | None -> ()

        // check if the constructor calls all necessary parent classes
        variableStack.ParentClassCalls 
        |> Seq.iter (fun kvp -> 
            match kvp.Value with
            | Some calledClassNode -> ()
            | None ->
                // for this class no parent class was called 
                emitID020Diagnostics kvp.Key pos1
        )

        let rb = new FplReference((pos1, pos2), fv)
        variableStack.PushEvalStack(rb)
        variableStack.PopEvalStack()
        emitVAR04diagnostics fv
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
    | Ast.DefinitionPredicate((pos1, pos2), (predicateSignature, (predicateContentAst, optPropertyListAsts))) ->
        st.EvalPush("DefinitionPredicate")
        let parent = variableStack.PeekEvalStack()
        let fv = new FplPredicate((pos1, pos2), parent, variableStack.GetNextAvailableFplBlockRunOrder)
        variableStack.PushEvalStack(fv)
        eval st predicateSignature
        eval st predicateContentAst
        optPropertyListAsts |> Option.map (List.map (eval st) >> ignore) |> Option.defaultValue ()
        if not fv.IsIntrinsic then // if not intrinsic, check variable usage
            emitVAR04diagnostics fv
        variableStack.PopEvalStack()
        st.EvalPop()
    // | DefinitionFunctionalTerm of Positions * ((Ast * Ast) * (Ast * Ast list option))
    | Ast.DefinitionFunctionalTerm((pos1, pos2), (functionalTermSignatureAst, (funcContentAst, optPropertyListAsts))) ->
        st.EvalPush("DefinitionFunctionalTerm")
        let parent = variableStack.PeekEvalStack()
        let fvNew = new FplFunctionalTerm((pos1, pos2), parent, variableStack.GetNextAvailableFplBlockRunOrder)
        variableStack.PushEvalStack(fvNew)
        eval st functionalTermSignatureAst 
        eval st funcContentAst
        optPropertyListAsts |> Option.map (List.map (eval st) >> ignore) |> Option.defaultValue ()
        if not fvNew.IsIntrinsic then // if not intrinsic, check variable usage
            emitVAR04diagnostics fvNew
        variableStack.PopEvalStack()
        st.EvalPop()
    | Ast.ClassSignature((pos1, pos2),((simpleSignatureAst, classTypeListAsts), optUserDefinedObjSymAst)) ->
        st.EvalPush("ClassSignature")
        variableStack.InSignatureEvaluation <- true
        eval st simpleSignatureAst
        // clear the storage of parent class counters before evaluating the list of parent classes
        variableStack.ParentClassCalls.Clear() 
        classTypeListAsts |> List.map (eval st) |> ignore
        optUserDefinedObjSymAst |> Option.map (eval st) |> Option.defaultValue ()
        setSignaturePositions pos1 pos2
        variableStack.InSignatureEvaluation <- false
        st.EvalPop()
    | Ast.DefinitionClass((pos1, pos2),(classSignatureAst, (classContentAst, optPropertyListAsts))) ->
        st.EvalPush("DefinitionClass")
        let parent = variableStack.PeekEvalStack()
        let fv = new FplClass((pos1, pos2), parent)
        variableStack.PushEvalStack(fv)
        eval st classSignatureAst
        eval st classContentAst
        optPropertyListAsts |> Option.map (List.map (eval st) >> ignore) |> Option.defaultValue ()
        emitVAR04diagnostics fv
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
        match tryFindAssociatedBlockForProof fv with
        | ScopeSearchResult.FoundAssociate potentialParent -> 
            // everything is ok, change the parent of the provable from theory to the found parent 
            fv.Parent <- Some potentialParent
        | ScopeSearchResult.FoundIncorrectBlock block ->
            emitID002Diagnostics (fv.Type(SignatureType.Type)) block fv.StartPos fv.EndPos
        | ScopeSearchResult.NotFound ->
            emitID003diagnostics fv  
        | ScopeSearchResult.FoundMultiple listOfKandidates ->
            emitID004Diagnostics (fv.Type(SignatureType.Type)) listOfKandidates fv.StartPos fv.EndPos
        | _ -> ()
        proofArgumentListAst |> List.map (eval st) |> ignore
        // now, we are ready to emit VAR03 diagnostics for all variables declared in the signature of the proof.
        emitVAR03diagnosticsForCorollaryOrProofVariable fv  
        optQedAst |> Option.map (eval st) |> Option.defaultValue ()
        emitVAR04diagnostics fv
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
        variableStack.PopEvalStack()
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
                        let argName = $"{split.[1]}."
                        match getArgumentInProof fvJi argName with
                        | Some argument -> fvJi.ArgList.Add(argument) 
                        | _ -> emitPR006Diagnostics fvJi.FplId argName fvJi.StartPos fvJi.EndPos 
                | _ -> ()
            | ScopeSearchResult.FoundIncorrectBlock otherBlock ->
                match fvJi with 
                | :? FplJustificationItemByDef ->
                    emitPR000Diagnostics otherBlock fvJi.StartPos fvJi.EndPos
                | :? FplJustificationItemByProofArgument ->
                    emitPR001Diagnostics otherBlock fvJi.StartPos fvJi.EndPos
                | _ ->
                    emitPR002Diagnostics otherBlock fvJi.StartPos fvJi.EndPos
            | ScopeSearchResult.NotFound ->
                emitID010Diagnostics fvJi.FplId fvJi.StartPos fvJi.EndPos
            | ScopeSearchResult.FoundMultiple listOfKandidates ->
                emitID023Diagnostics listOfKandidates fvJi.StartPos fvJi.EndPos
            | _ -> ()


        match byModifierOption, dollarDigitListAsts, refArgumentIdentifierAst with
        | Some LiteralByAx, Some _, _ -> 
            // byAx justification cannot be used together with a proof or corollary reference
            emitPR010Diagnostics LiteralByAx LiteralAxL pos1 pos2 
        | Some LiteralByAx, None, Some _ -> 
            // byAx justification cannot be used together with a proof argument reference 
            emitPR011Diagnostics LiteralByAx LiteralAxL pos1 pos2 
        | Some LiteralByAx, None, None -> 
            let fvJi = new FplJustificationItemByAx((pos1, pos2), parent)
            variableStack.PushEvalStack(fvJi)
            eval st predicateIdentifierAst
            // check, if indeed the predicateId points to an axiom, if not issue diagnostics
            let candidates = findCandidatesByName st fvJi.FplId false false
            checkDiagnostics fvJi candidates
            variableStack.PopEvalStack()
        | Some LiteralByInf, Some _, _ -> 
            // byInf justification cannot be used together with a proof reference
            emitPR010Diagnostics LiteralByInf PrimRuleOfInference pos1 pos2 
        | Some LiteralByInf, None, Some _ -> 
            // byInf justification cannot be used together with a proof argument reference 
            emitPR011Diagnostics LiteralByInf PrimRuleOfInference pos1 pos2 
        | Some LiteralByInf, None, None -> 
            let fvJi = new FplJustificationItemByAx((pos1, pos2), parent)
            variableStack.PushEvalStack(fvJi)
            eval st predicateIdentifierAst
            // check, if indeed the predicateId points to a rule of inference, if not issue diagnostics
            let candidates = findCandidatesByName st fvJi.FplId false false
            checkDiagnostics fvJi candidates
            variableStack.PopEvalStack()
        | Some LiteralByDef, Some _, _ -> 
            // byDef justification cannot be used together with a proof reference
            emitPR010Diagnostics LiteralByDef LiteralDefL pos1 pos2 
        | Some LiteralByDef, None, Some _ -> 
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
            let candidates = findCandidatesByName st fvJi.FplId false true |> List.filter (fun fv -> fv.FplId = name)
            // check, if indeed the predicateId points to another proof, if not issue diagnostics, 
            // also check if arg exists, if not issue diagnostics
            checkDiagnostics fvJi candidates
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
    let mutable order = 0

    while found do
        let usesClausesEvaluatedParsedAst =
            tryFindParsedAstUsesClausesEvaluated st.ParsedAsts

        match usesClausesEvaluatedParsedAst with
        | Some pa ->
            variableStack.ClearEvalStack()
            // evaluate the ParsedAst of a theory
            let theoryValue = new FplTheory((Position("",0,1,1), Position("",0,1,1)), st.Root, pa.Parsing.Uri.AbsolutePath, order);
            if not (st.Root.Scope.ContainsKey(pa.Id)) then
                st.Root.Scope.Add(pa.Id, theoryValue)
                order <- order + 1 // after adding a new theory to the symbol table, increase the order counter
            else
                st.Root.Scope[pa.Id] <- theoryValue
            theoryValue.FplId <- pa.Id
            theoryValue.TypeId <- pa.Id
            variableStack.PushEvalStack(theoryValue)
            ad.CurrentUri <- pa.Parsing.Uri
            eval st pa.Parsing.Ast
            pa.Status <- ParsedAstStatus.Evaluated
            variableStack.PopEvalStack()
        | None -> found <- false

