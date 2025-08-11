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
open FplGrammarCommons
open FplGrammarTypes
open FplInterpreterTypes
open FplInterpreterDiagnosticsEmitter
open FplInterpreterPredicateEvaluator
open FplInterpreterRunner
open EvalStackHandler
open System.Runtime.InteropServices

let es = EvalStack()
let runner = FplRunner()

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
        if subNode.FplBlockType = FplBlockType.Reference 
            || subNode.FplBlockType = FplBlockType.Quantor 
            || subNode.FplBlockType = FplBlockType.IntrinsicInd 
            || subNode.FplBlockType = FplBlockType.IntrinsicPred then 
            es.Pop() |> ignore // pop the removable reference block and ignored it
            es.PushEvalStack(subNode) // push its subNode instead
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

/// A recursive function evaluating an AST and returning a list of EvalAliasedNamespaceIdentifier records
/// for each occurrence of the uses clause in the FPL code.
let rec eval (st: SymbolTable) ast =
    let evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst =
        match optVarDeclOrSpecList with
        | Some astList -> astList |> List.map (eval st) |> ignore
        | None -> ()
        eval st predicateAst

    let evalMany blockType pos1 pos2 = 
        let fv = es.PeekEvalStack()
        match fv.Parent with 
        | Some parent -> 
            checkVAR00Diagnostics parent.AuxiliaryInfo pos1 pos2
        | _ -> ()
        fv.FplBlockType <- blockType

    let setUnitType (fv:FplValue) (value:FplValue) (tplName:string)=
        match value.FplBlockType with
        | FplBlockType.IntrinsicPred
        | FplBlockType.IntrinsicInd 
        | FplBlockType.IntrinsicObj 
        | FplBlockType.IntrinsicFunc ->
            match fv.FplBlockType with
            | FplBlockType.Class -> () // do not override class's type with base obj
            | FplBlockType.Reference ->
                fv.TypeId <- $"{value.ShortName}"
                fv.ValueList.Clear()
                fv.ValueList.Add(value)
            | _ ->  fv.TypeId <- $"{value.ShortName}"
        | FplBlockType.IntrinsicTpl ->
            match fv.FplBlockType with
            | FplBlockType.Class -> () // do not override class's type with base obj
            | FplBlockType.Reference ->
                fv.TypeId <- $"{tplName}"
                value.TypeId <- $"{tplName}"
                value.FplId <- $"{tplName}"
                fv.ValueList.Clear()
                fv.ValueList.Add(value)
            | _ ->  
                fv.TypeId <- $"{tplName}"
        | _ ->
            fv.TypeId <- value.ShortName
            fv.ValueList.Clear()
            fv.ValueList.Add(value)

        
        match fv.FplBlockType with 
        | FplBlockType.VariadicVariableMany -> 
            fv.TypeId <- $"*{fv.TypeId}"
        | FplBlockType.VariadicVariableMany1 -> 
            fv.TypeId <- $"+{fv.TypeId}"
        | _ -> ()

    match ast with
    // units: | Star
    | Ast.IndexType((pos1, pos2),()) -> 
        st.EvalPush("IndexType")
        let fv = es.PeekEvalStack()
        let value = new FplIntrinsicInd((pos1, pos2), fv)
        setUnitType fv value ""
        st.EvalPop() |> ignore
    | Ast.ObjectType((pos1, pos2),()) -> 
        st.EvalPush("ObjectType")
        let fv = es.PeekEvalStack()
        let value = new FplIntrinsicObj((pos1, pos2), fv)
        setUnitType fv value ""
        match checkID009_ID010_ID011_Diagnostics st fv literalObj pos1 pos2 with
        | Some classNode -> 
            fv.ArgList.Add classNode
        | None -> ()
        checkID012Diagnostics st fv literalObj pos1 pos2 
        // add potential parent class call for this identifier (if it is one) 
        let path = st.EvalPath()
        if path.Contains("DefinitionClass.InheritedClassType") then 
            es.ParentClassCalls.TryAdd(literalObj, None) |> ignore
        st.EvalPop()
    | Ast.PredicateType((pos1, pos2),()) -> 
        st.EvalPush("PredicateType")
        let fv = es.PeekEvalStack()
        let value = new FplIntrinsicPred((pos1, pos2), fv)
        setUnitType fv value ""
        st.EvalPop()
    | Ast.FunctionalTermType((pos1, pos2),()) -> 
        st.EvalPush("FunctionalTermType")
        let fv = es.PeekEvalStack()
        let value = new FplIntrinsicFunc((pos1, pos2), fv)
        setUnitType fv value ""
        st.EvalPop()
    | Ast.Many((pos1, pos2),()) ->
        st.EvalPush("Many")
        evalMany FplBlockType.VariadicVariableMany pos1 pos2
        st.EvalPop()
    | Ast.Many1((pos1, pos2),()) ->
        st.EvalPush("Many1")
        evalMany FplBlockType.VariadicVariableMany1 pos1 pos2
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
        let fv = es.PeekEvalStack()
        fv.IsIntrinsic <- true // flag that this block is intrinsic
        st.EvalPop()
    | Ast.Property((pos1, pos2),()) -> 
        st.EvalPush("Property")
        st.EvalPop()
    | Ast.Optional((pos1, pos2),()) -> 
        st.EvalPush("Optional")
        st.EvalPop()
    | Ast.Error  ->   
        st.EvalPush("Error")
        st.EvalPop()
    // strings: | Digits of string
    | Ast.Digits s -> 
        st.EvalPush("Digits")
        let fv = es.PeekEvalStack()
        fv.FplId <- s
        fv.TypeId <- s
        st.EvalPop()
    | Ast.PascalCaseId s -> 
        st.EvalPush("PascalCaseId")
        eval_string st s
        st.EvalPop() 
    | Ast.ExtensionRegex s -> 
        st.EvalPush("ExtensionRegex")
        let fv = es.PeekEvalStack()
        let vars = fv.GetVariables()
        if vars.Length> 0 then
            let mainVar = vars.Head
            mainVar.TypeId <- s // set the extensions's main variable's type to the pattern
        st.EvalPop() 
    // | DollarDigits of Positions * int
    | Ast.DollarDigits((pos1, pos2), s) -> 
        st.EvalPush("DollarDigits")
        let path = st.EvalPath()
        let fv = es.PeekEvalStack()
        let sid = $"${s.ToString()}"
        if path.Contains("Expression.DollarDigits") then
            let value = new FplIntrinsicInd((pos1, pos2), fv)
            value.FplId <- sid
            es.PushEvalStack(value)
            es.PopEvalStack()
        else
            fv.FplId <- fv.FplId + sid
            if fv.TypeId <> "" then
                fv.TypeId <- fv.TypeId + sid
            else
                fv.TypeId <- literalInd
                    
            fv.EndPos <- pos2
        st.EvalPop() 
    | Ast.ExtensionName((pos1, pos2), s) ->
        st.EvalPush("ExtensionName")
        let fv = es.PeekEvalStack()
        let extensionName = $"@{s}"
        match fv.FplBlockType with 
        | FplBlockType.Extension ->
            fv.FplId <- extensionName
            fv.TypeId <- extensionName
            fv.StartPos <- pos1
            fv.EndPos <- pos2
        | FplBlockType.VariadicVariableMany -> 
            let sid = $"*{extensionName}"
            fv.TypeId <- sid
        | FplBlockType.VariadicVariableMany1 -> 
            let sid = $"+{extensionName}"
            fv.TypeId <- sid
        | _ -> 
            fv.TypeId <- extensionName
            checkID019Diagnostics st extensionName pos1 pos2
        st.EvalPop() 
    | Ast.TemplateType((pos1, pos2), s) -> 
        st.EvalPush("TemplateType")
        let fv = es.PeekEvalStack()
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
        let fv = es.PeekEvalStack()
        let varValue = new FplVariable((pos1, pos2), fv) 
        varValue.FplId <- name
        varValue.TypeId <- literalUndef
        let undefined = new FplIntrinsicUndef((pos1, pos2), varValue)  
        varValue.ValueList.Add(undefined)
        varValue.IsSignatureVariable <- es.InSignatureEvaluation 
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
                es.PushEvalStack(other)
            | _ -> 
                es.PushEvalStack(varValue)

        elif isExtensionDeclaration then 
            fv.Scope.Add(name, varValue)
        elif isLocalizationDeclaration then 
            match variableInBlockScopeByName fv name false with 
            | ScopeSearchResult.Found other ->
                emitVAR03diagnostics varValue other 
            | _ -> 
                let rec getLocalization (fValue:FplValue) = 
                    if fValue.FplBlockType = FplBlockType.Localization then
                        fValue
                    else
                        match fValue.Parent with
                        | Some parent -> getLocalization parent
                        | None -> fValue
                let loc = getLocalization fv
                loc.Scope.Add(name, varValue)
                // Add the variable to the reference in the localization
                es.PushEvalStack(varValue)
                es.PopEvalStack()
        else
            match variableInBlockScopeByName fv name true with 
            | ScopeSearchResult.Found other -> 
                match fv.FplBlockType with
                | FplBlockType.Reference ->
                    if not (fv.Scope.ContainsKey(name)) then
                        fv.Scope.Add(name, other)
                | _ -> ()
                // count usages of the variable in scope
                other.AuxiliaryInfo <- other.AuxiliaryInfo + 1
            | _ -> 
                // otherwise emit variable not declared if this is not a declaration 
                emitVAR01diagnostics name pos1 pos2
            fv.FplId <- name
            fv.TypeId <- literalUndef
        ad.DiagnosticsStopped <- diagnosticsStopFlag
        st.EvalPop() 
    | Ast.DelegateId((pos1, pos2), s) -> 
        st.EvalPush("DelegateId")
        let fv = es.PeekEvalStack()
        fv.FplId <- fv.FplId + s
        fv.TypeId <- fv.TypeId + s
        st.EvalPop() 
    | Ast.Alias((pos1, pos2), s) -> 
        st.EvalPush("Alias")
        eval_pos_string st pos1 pos2 s
        st.EvalPop() 
    | Ast.LanguageCode((pos1, pos2), s) -> 
        st.EvalPush("LanguageCode")
        let fv = es.PeekEvalStack()
        fv.FplId <- s
        fv.TypeId <- s
        fv.StartPos <- pos1
        fv.EndPos <- pos2
        st.EvalPop() 
    | Ast.LocalizationString((pos1, pos2), s) -> 
        st.EvalPush("LocalizationString")
        let fv = es.PeekEvalStack()
        fv.FplId <- s
        fv.TypeId <- s
        st.EvalPop() 
    | Ast.ObjectSymbol((pos1, pos2), symbol) -> 
        st.EvalPush("ObjectSymbol")
        let fv = es.PeekEvalStack()
        fv.FplId <- symbol
        fv.TypeId <- symbol
        fv.StartPos <- pos1
        fv.EndPos <- pos2
        emitSIG01Diagnostics st fv pos1 pos2 
        st.EvalPop()
    | Ast.ArgumentIdentifier((pos1, pos2), s) -> 
        st.EvalPush("ArgumentIdentifier")
        let setId (fValue:FplValue) = 
            fValue.FplId <- s
            fValue.TypeId <- literalPred
            fValue.StartPos <- pos1
            fValue.EndPos <- pos2
        let fv = es.PeekEvalStack()
        setId fv
        let parent = fv.Parent.Value
        match parent.FplBlockType with
        | FplBlockType.ArgInference 
        | FplBlockType.Justification ->
            let arg = parent.Parent.Value
            let proof = arg.Parent.Value
            if not (proof.Scope.ContainsKey(s)) then 
                emitPR005Diagnostics fv 
        | FplBlockType.Argument -> ()
        | _ -> 
            emitPR000Diagnostics fv 

        st.EvalPop() 
    | Ast.Prefix((pos1, pos2), symbol) -> 
        st.EvalPush("Prefix")
        let fv = es.PeekEvalStack()
        fv.ExpressionType <- FixType.Prefix symbol
        st.EvalPop() 
    | Ast.Infix((pos1, pos2), (symbol, precedenceAsts)) -> 
        st.EvalPush("Infix")
        let fv = es.PeekEvalStack()
        eval st precedenceAsts
        fv.ExpressionType <- FixType.Infix (symbol, fv.AuxiliaryInfo)
        emitSIG02Diagnostics st fv pos1 pos2 
        st.EvalPop() 
    | Ast.Postfix((pos1, pos2), symbol) -> 
        st.EvalPush("Postfix")
        let fv = es.PeekEvalStack()
        fv.ExpressionType <- FixType.Postfix symbol
        st.EvalPop() 
    | Ast.Symbol((pos1, pos2), symbol) -> 
        st.EvalPush("Symbol")
        let fv = es.PeekEvalStack()
        fv.ExpressionType <- FixType.Symbol symbol
        st.EvalPop() 
    | Ast.InfixOperator((pos1, pos2), symbol) -> 
        st.EvalPush("InfixOperator")
        let fv = es.PeekEvalStack()
        fv.FplId <- symbol
        fv.TypeId <- symbol
        fv.StartPos <- pos1
        fv.EndPos <- pos2
        emitSIG01Diagnostics st fv pos1 pos2 
        st.EvalPop() 
    | Ast.PostfixOperator((pos1, pos2), symbol) -> 
        st.EvalPush("PostfixOperator")
        let fv = es.PeekEvalStack()
        fv.FplId <- symbol
        fv.TypeId <- symbol
        fv.StartPos <- pos1
        fv.EndPos <- pos2
        emitSIG01Diagnostics st fv pos1 pos2 
        st.EvalPop() 
    | Ast.PrefixOperator((pos1, pos2), symbol) -> 
        st.EvalPush("PrefixOperator")
        let fv = es.PeekEvalStack()
        fv.FplId <- symbol
        fv.TypeId <- symbol
        fv.StartPos <- pos1
        fv.EndPos <- pos2
        emitSIG01Diagnostics st fv pos1 pos2 
        st.EvalPop() 
    // | Self of Positions * unit
    | Ast.Self((pos1, pos2), _) -> 
        st.EvalPush("Self")
        let rb = es.PeekEvalStack()
        rb.StartPos <- pos1
        rb.EndPos <- pos2
        rb.FplId <- literalSelf
        rb.TypeId <- literalSelf
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
        let rb = es.PeekEvalStack()
        rb.StartPos <- pos1
        rb.EndPos <- pos2
        rb.FplId <- literalParent
        rb.TypeId <- literalParent
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
        let fv = es.PeekEvalStack()
        let value = new FplIntrinsicPred((pos1, pos2), fv)
        value.StartPos <- pos1
        value.EndPos <- pos2
        value.FplId <- literalTrue
        value.TypeId <- literalPred
        es.PushEvalStack(value)
        es.PopEvalStack()
        st.EvalPop() 
    | Ast.False((pos1, pos2), _) -> 
        st.EvalPush("False")
        let fv = es.PeekEvalStack()
        let value = new FplIntrinsicPred((pos1, pos2), fv)
        value.StartPos <- pos1
        value.EndPos <- pos2
        value.FplId <- literalFalse
        value.TypeId <- literalPred
        es.PushEvalStack(value)
        es.PopEvalStack()
        st.EvalPop() 
    | Ast.Undefined((pos1, pos2), _) -> 
        st.EvalPush("Undefined")
        let fv = es.PeekEvalStack()
        fv.StartPos <- pos1
        fv.EndPos <- pos2
        fv.FplId <- literalUndef
        fv.TypeId <- literalUndef
        st.EvalPop() 
    | Ast.Trivial((pos1, pos2), _) -> 
        st.EvalPush("Trivial")
        let fv = es.PeekEvalStack()
        let refBlock = new FplReference((pos1, pos2), fv) 
        es.PushEvalStack(refBlock)
        refBlock.FplId <- literalTrivial
        refBlock.TypeId <- literalPred
        let value = new FplIntrinsicPred((pos1, pos2), refBlock) 
        value.FplId <- literalTrue
        refBlock.ValueList.Add(value)
        es.PopEvalStack()
        st.EvalPop() 
    | Ast.Qed((pos1, pos2), _) -> 
        st.EvalPush("Qed")
        st.EvalPop() 
    | Ast.RuleOfInference((pos1, pos2), (signatureAst, premiseConclusionBlockAst)) ->
        st.EvalPush("RuleOfInference")
        let parent = es.PeekEvalStack()
        let fv = new FplRuleOfInference((pos1, pos2), parent)
        ad.DiagnosticsStopped <- true // stop all diagnostics during rule of inference
        es.PushEvalStack(fv)
        eval st signatureAst
        eval st premiseConclusionBlockAst
        ad.DiagnosticsStopped <- false // enable all diagnostics after rule of inference
        emitVAR04diagnostics fv
        es.PopEvalStack() 
        st.EvalPop() 
    | Ast.Mapping((pos1, pos2), variableTypeAst) ->
        st.EvalPush("Mapping")
        let fv = es.PeekEvalStack()
        let map = new FplMapping((pos1, pos2), fv)
        es.PushEvalStack(map)
        eval st variableTypeAst
        es.PopEvalStack()
        st.EvalPop()
    | Ast.ClassIdentifier((pos1, pos2), ast1) ->
        st.EvalPush("ClassIdentifier")
        eval st ast1
        let fv = es.PeekEvalStack()
        fv.EndPos <- pos2
        st.EvalPop()
    | Ast.Extension((pos1, pos2), extensionString) ->
        st.EvalPush("Extension")
        let fv = es.PeekEvalStack()
        fv.FplId <- extensionString
        fv.TypeId <- extensionString
        checkID018Diagnostics st fv extensionString pos1 pos2
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
        let fv = es.PeekEvalStack()
        fv.FplId <- literalNot
        fv.TypeId <- literalPred
        eval st predicateAst
        fv.EndPos <- pos2
        evaluateNegation fv
        emitLG000orLG001Diagnostics fv "negation"
        st.EvalPop()
    | Ast.InEntity((pos1, pos2), ast1) ->
        st.EvalPush("InEntity")
        eval st ast1
        st.EvalPop()
    | Ast.Assertion((pos1, pos2), predicateAst) ->
        st.EvalPush("Assertion")
        let fv = es.PeekEvalStack()
        fv.StartPos <- pos1
        fv.EndPos <- pos2
        fv.FplId <- literalAssL
        let rb = new FplReference((pos1,pos2), fv)
        es.PushEvalStack(rb)
        eval st predicateAst
        es.PopEvalStack()
        st.EvalPop()
    | Ast.ByDef((pos1, pos2), predicateWithQualificationAst) ->
        st.EvalPush("ByDef")
        let fv = es.PeekEvalStack()
        fv.FplId <- "bydef."
        fv.TypeId <- "bydef."
        eval st predicateWithQualificationAst
        emitPR001Diagnostics fv pos1 pos2
        st.EvalPop()
    | Ast.DottedPredicate((pos1, pos2), predicateWithOptSpecificationAst) ->
        st.EvalPush("DottedPredicate")
        let fv = es.PeekEvalStack()
        let refBlock = new FplReference((pos1, pos2), fv) 
        fv.Scope.Add(".",refBlock)
        es.PushEvalStack(refBlock)
        eval st predicateWithOptSpecificationAst
        es.PopEvalStack()
        st.EvalPop()
    | Ast.Return((pos1, pos2), returneeAst) ->
        st.EvalPush("Return")
        let fv = es.PeekEvalStack()
        let stmt = new FplStmt((pos1,pos2), fv)
        stmt.FplId <- literalRetL
        es.PushEvalStack(stmt)
        eval st returneeAst
        let returnedReference = stmt.ArgList[0]
        emitSIG03Diagnostics returnedReference fv
        let returnedValueOpt = returnedReference.GetArgument
        match returnedValueOpt with
        | Some returnedValue -> 
            if returnedValue.ValueList.Count > 0 then
                fv.ValueList.AddRange(returnedValue.ValueList)
            else
                // todo diagnostics returns uninitialized value
                let value = new FplIntrinsicUndef((pos1, pos2), fv)
                fv.ValueList.Add(value)
                fv.ValueList.Add(value)
        | _ -> 
            // add an undefined value since there was no argument of the 
            let value = new FplIntrinsicUndef((pos1, pos2), fv)
            fv.ValueList.Add(value)
        es.PopEvalStack() 
        st.EvalPop()
    | Ast.AssumeArgument((pos1, pos2), predicateAst) ->
        st.EvalPush("AssumeArgument")
        let fv = es.PeekEvalStack()
        let argInf = new FplArgInference((pos1, pos2), fv) 
        argInf.FplId <- literalAssume
        es.PushEvalStack(argInf)
        eval st predicateAst
        es.PopEvalStack()
        st.EvalPop()
    | Ast.RevokeArgument((pos1, pos2), predicateAst) ->
        st.EvalPush("RevokeArgument")
        let fv = es.PeekEvalStack()
        let argInf = new FplArgInference((pos1, pos2), fv) 
        argInf.FplId <- literalRevL
        es.PushEvalStack(argInf)
        eval st predicateAst
        es.PopEvalStack()
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
        let fv = es.PeekEvalStack()
        match fv.FplBlockType with 
        | FplBlockType.Class -> 
            if evalPath.EndsWith("InheritedClassType.PredicateIdentifier") then 
                match checkID009_ID010_ID011_Diagnostics st fv identifier pos1 pos2 with
                | Some classNode -> 
                    // add known class
                    fv.ArgList.Add classNode
                | None -> ()
                // add potential parent class call for this identifier
                let path = st.EvalPath()
                if path.Contains("DefinitionClass.InheritedClassType") then 
                    es.ParentClassCalls.TryAdd(identifier, None) |> ignore
            else
                fv.FplId <- identifier
                fv.TypeId <- identifier
                match checkID009_ID010_ID011_Diagnostics st fv identifier pos1 pos2 with
                | Some classNode -> 
                    fv.ArgList.Add classNode
                | None -> ()

        | FplBlockType.Axiom
        | FplBlockType.Theorem 
        | FplBlockType.Lemma 
        | FplBlockType.Proposition 
        | FplBlockType.Corollary 
        | FplBlockType.Conjecture 
        | FplBlockType.Proof 
        | FplBlockType.RuleOfInference 
        | FplBlockType.MandatoryPredicate
        | FplBlockType.OptionalPredicate
        | FplBlockType.Predicate ->
            fv.FplId <- identifier
            fv.TypeId <- literalPred
        | FplBlockType.MandatoryFunctionalTerm
        | FplBlockType.OptionalFunctionalTerm
        | FplBlockType.FunctionalTerm ->
            fv.FplId <- identifier
            fv.TypeId <- literalFunc
        | FplBlockType.Constructor -> 
            fv.FplId <- identifier
            fv.TypeId <- identifier
            checkID008Diagnostics fv pos1 pos2
        | FplBlockType.VariadicVariableMany -> 
            fv.TypeId <- $"*{identifier}"
        | FplBlockType.VariadicVariableMany1 -> 
            fv.TypeId <- $"+{identifier}"
        | FplBlockType.Variable -> 
            fv.TypeId <- identifier
        | FplBlockType.Mapping -> 
            fv.TypeId <- fv.TypeId + identifier
        | FplBlockType.Reference -> 
            fv.FplId <- fv.FplId + identifier
            fv.TypeId <- fv.TypeId + identifier
            checkID012Diagnostics st fv identifier pos1 pos2
        | _ -> ()
        if evalPath.Contains(".NamedVarDecl.") || evalPath.Contains(".VariableType.ClassType.") then 
            let candidates = findCandidatesByName st identifier false
            match (fv.FplBlockType, candidates.Length) with
            | (FplBlockType.Variable, 0)
            | (FplBlockType.VariadicVariableMany, 0)
            | (FplBlockType.VariadicVariableMany1, 0) -> 
                emitSIG04DiagnosticsForTypes identifier pos1 pos2
                let undefValue = new FplIntrinsicUndef((fv.StartPos, fv.EndPos), fv)
                fv.ValueList.Add(undefValue)
               
            | (FplBlockType.Variable, 1)
            | (FplBlockType.VariadicVariableMany, 1)
            | (FplBlockType.VariadicVariableMany1, 1) -> 
                fv.Scope.TryAdd(fv.FplId, candidates.Head) |> ignore
            | (FplBlockType.Variable, _)
            | (FplBlockType.VariadicVariableMany, _)
            | (FplBlockType.VariadicVariableMany1, _) -> 
                emitID017Diagnostics identifier candidates pos1 pos2
            | _ -> 
                match checkSIG04Diagnostics fv candidates with
                | Some candidate -> 
                    match fv.FplBlockType with
                    | FplBlockType.Reference -> fv.Scope.Add(identifier, candidate)
                    | _ -> fv.ArgList.Add(candidate)
                | _ -> ()
        
        st.EvalPop()
    | Ast.ParamTuple((pos1, pos2), namedVariableDeclarationListAsts) ->
        st.EvalPush("ParamTuple")
        let fv = es.PeekEvalStack()
        namedVariableDeclarationListAsts |> List.map (
            fun child ->
            match child with 
            | Ast.NamedVarDecl(_,((varList,_),_)) -> fv.Arity <- fv.Arity + varList.Length
            | _ -> ()
            eval st child
        ) |> ignore
        fv.EndPos <- pos2
        st.EvalPop()
    | Ast.BracketedCoordsInType((pos1, pos2), asts) ->
        st.EvalPush("BracketedCoordsInType")
        let fv = es.PeekEvalStack()
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
        let fv = es.PeekEvalStack()
        asts |> List.map (fun ebnfTerm ->
            let trsl = new FplTranslation((pos1, pos2), fv)
            es.PushEvalStack(trsl)
            eval st ebnfTerm
            es.PopEvalStack()
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
        let fv = es.PeekEvalStack()
        fv.HasBrackets <- true
        if coordListAst.Length > 0 then 
            coordListAst 
            |> List.iter (fun pred -> 
                let ref = new FplReference((pos1, pos2), fv)
                es.PushEvalStack(ref)
                eval st pred
                es.PopEvalStack()
            ) 
        else
            let ref = new FplReference((pos1, pos2), fv)
            ref.FplId <- "???"
            ref.TypeId <- "???"
            es.PushEvalStack(ref)
            es.PopEvalStack()
        st.EvalPop()
    | Ast.And((pos1, pos2), (predicateAst1, predicateAst2)) ->
        st.EvalPush("And")
        let fv = es.PeekEvalStack()
        fv.FplId <- literalAnd
        fv.TypeId <- literalPred
        eval st predicateAst1
        eval st predicateAst2
        fv.EndPos <- pos2
        evaluateConjunction fv
        emitLG000orLG001Diagnostics fv "conjunction"
        st.EvalPop()
    | Ast.Or((pos1, pos2), (predicateAst1, predicateAst2)) ->
        st.EvalPush("Or")
        let fv = es.PeekEvalStack()
        fv.FplId <- literalOr
        fv.TypeId <- literalPred
        eval st predicateAst1
        eval st predicateAst2
        fv.EndPos <- pos2
        evaluateDisjunction fv
        emitLG000orLG001Diagnostics fv "disjunction"
        st.EvalPop()
    | Ast.Xor((pos1, pos2), (predicateAst1, predicateAst2)) ->
        st.EvalPush("Xor")
        let fv = es.PeekEvalStack()
        fv.FplId <- literalXor
        fv.TypeId <- literalPred
        eval st predicateAst1
        eval st predicateAst2
        fv.EndPos <- pos2
        evaluateExclusiveOr fv
        emitLG000orLG001Diagnostics fv "exclusive-or"
        st.EvalPop()
    | Ast.VarDeclBlock((pos1, pos2), varDeclOrStmtAstList) ->
        st.EvalPush("VarDeclBlock")
        let fv = es.PeekEvalStack()
        let stmtList = List<FplValue>()
        varDeclOrStmtAstList 
        |> List.map(fun ast -> 
            match ast with 
            | Ast.NamedVarDecl _ -> eval st ast
            | _ -> 
                let stmt = new FplStmt((pos1,pos2), fv)
                es.PushEvalStack(stmt)
                eval st ast
                stmtList.Add(es.Pop())
        ) |> ignore
        fv.ArgList.AddRange(stmtList)
        st.EvalPop()
    | Ast.StatementList((pos1, pos2), asts) ->
        st.EvalPush("StatementList")
        asts |> List.map (eval st) |> ignore
        st.EvalPop()
    | Ast.DefaultResult((pos1, pos2), asts) ->
        st.EvalPush("DefaultResult")
        asts |> List.map (eval st) |> ignore
        st.EvalPop()
    | Ast.Justification((pos1, pos2), predicateList) ->
        st.EvalPush("Justification")
        let fv = es.PeekEvalStack()
        let just = new FplJustification((pos1, pos2), fv) 
        es.PushEvalStack(just)
        predicateList |> List.map (eval st) |> ignore
        es.PopEvalStack()
        st.EvalPop()
    | Ast.ArgumentTuple((pos1, pos2), predicateListAst) ->
        st.EvalPush("ArgumentTuple")
        let fv = es.PeekEvalStack()
        if predicateListAst.Length > 0 then 
            predicateListAst 
            |> List.iter (fun pred -> 
                eval st pred
            ) 
        else
            let ref = new FplReference((pos1, pos2), fv)
            ref.FplId <- "???"
            ref.TypeId <- "???"
            es.PushEvalStack(ref)
            es.PopEvalStack()
        st.EvalPop()
    | Ast.QualificationList((pos1, pos2), asts) ->
        st.EvalPush("QualificationList")
        if asts.Length > 0 then
            let fv = es.PeekEvalStack()
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
            let fv = es.PeekEvalStack()
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
    | Ast.ReferenceToProofOrCorollary((pos1, pos2), (referencingIdentifierAst, optArgumentTuple)) ->
        if optArgumentTuple.IsNone then
            st.EvalPush("ReferenceToProof")
        else
            st.EvalPush("ReferenceToCorollary")
        eval st referencingIdentifierAst
        optArgumentTuple |> Option.map (eval st) |> ignore
        eval_pos_ast_ast_opt st pos1 pos2
        st.EvalPop()
    | Ast.PredicateWithOptSpecification((pos1, pos2), (fplIdentifierAst, optionalSpecificationAst)) ->
        st.EvalPush("PredicateWithOptSpecification")
        let fv = es.PeekEvalStack()
        let searchForCandidatesOfReferenceBlock (refBlock:FplValue) = 
            let candidatesFromTheory = findCandidatesByName st refBlock.FplId true
            let candidatesFromPropertyScope = findCandidatesByNameInBlock refBlock refBlock.FplId
            let candidatesFromDottedQualification = findCandidatesByNameInDotted refBlock refBlock.FplId
            candidatesFromTheory  
            @ candidatesFromPropertyScope 
            @ candidatesFromDottedQualification

        match optionalSpecificationAst with
        | Some specificationAst -> 
            let refBlock = new FplReference((pos1, pos2), fv) 
            es.PushEvalStack(refBlock)
            eval st fplIdentifierAst
            eval st specificationAst |> ignore
            if System.Char.IsLower(refBlock.FplId[0]) then
                // match the signatures of small-letter entities (like the self or parent entity, or variables with arguments) 
                // with their declared types 
                let candidatesOfSelfOrParentEntity = 
                    refBlock.Scope
                    |> Seq.filter (fun kvp -> kvp.Key = literalSelf || kvp.Key = literalParent)
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

            es.PopEvalStack()
        | None -> 
            // if no specification was found then simply continue in the same context
            eval st fplIdentifierAst
            // make sure, we still add a referenced node candidate to the scope of a reference
            let candidates = searchForCandidatesOfReferenceBlock fv
            let classes = candidates |> List.filter (fun c -> c.FplBlockType = FplBlockType.Class)
            let constructors = candidates |> List.filter (fun c -> c.FplBlockType = FplBlockType.Constructor)
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
        let fv = es.PeekEvalStack()
        let lang = new FplLanguage((pos1, pos2), fv) 
        es.PushEvalStack(lang)
        eval st langCode
        let trsl = new FplTranslation((pos1, pos2), lang) 
        es.PushEvalStack(trsl)
        eval st ebnfAst
        es.PopEvalStack()
        es.PopEvalStack()
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
        let parent = es.PeekEvalStack()
        let fv = new FplExtension((pos1,pos2), parent)
        es.PushEvalStack(fv)
        eval st extensionNameAst
        eval st extensionSignatureAst
        eval st extensionTermAst
        es.PopEvalStack()
        st.EvalPop()
    | Ast.Impl((pos1, pos2), (predicateAst1, predicateAst2)) ->
        st.EvalPush("Impl")
        let fv = es.PeekEvalStack()
        fv.FplId <- literalImpl
        fv.TypeId <- literalPred
        eval st predicateAst1
        eval st predicateAst2
        fv.EndPos <- pos2
        evaluateImplication fv
        emitLG000orLG001Diagnostics fv "implication"
        st.EvalPop()
    | Ast.Iif((pos1, pos2), (predicateAst1, predicateAst2)) ->
        st.EvalPush("Iif")
        let fv = es.PeekEvalStack()
        fv.FplId <- literalIif
        fv.TypeId <- literalPred
        eval st predicateAst1
        eval st predicateAst2
        fv.EndPos <- pos2
        evaluateEquivalence fv
        emitLG000orLG001Diagnostics fv "equivalence"
        st.EvalPop()
    | Ast.IsOperator((pos1, pos2), (isOpArgAst, variableTypeAst)) ->
        st.EvalPush("IsOperator")
        let fv = es.PeekEvalStack()
        fv.FplId <- literalIs
        fv.TypeId <- literalPred
        let operand = new FplReference((pos1, pos2), fv) 
        es.PushEvalStack(operand)
        eval st isOpArgAst
        es.PopEvalStack()
        let typeOfOperand = new FplMapping((pos1, pos2), fv) 
        es.PushEvalStack(typeOfOperand)
        eval st variableTypeAst
        es.PopEvalStack()
        evaluateIsOperator fv operand typeOfOperand
        
        st.EvalPop()
    | Ast.Delegate((pos1, pos2), (fplDelegateIdentifierAst, argumentTupleAst)) ->
        st.EvalPush("Delegate")
        let fv = es.PeekEvalStack()
        let refBlock = new FplReference((pos1, pos2), fv) 
        refBlock.FplId <- "del."
        refBlock.TypeId <- "del."
        es.PushEvalStack(refBlock)
        eval st fplDelegateIdentifierAst
        eval st argumentTupleAst
        emitID013Diagnostics refBlock pos1 pos2 |> ignore
        es.PopEvalStack()
        st.EvalPop()
    // | ClosedOrOpenRange of Positions * ((Ast * Ast option) * Ast)
    | Ast.SignatureWithUserDefinedString((pos1, pos2),
                                         ((predicateIdentifierAst, optUserDefinedSymbolAst), paramTupleAst)) ->
        st.EvalPush("SignatureWithUserDefinedString")
        eval st predicateIdentifierAst
        optUserDefinedSymbolAst
        |> Option.map (eval st)
        |> Option.defaultValue ()
        |> ignore
        eval st paramTupleAst
        let fv = es.PeekEvalStack()
        emitSIG00Diagnostics fv pos1 pos2
        st.EvalPop()
    | Ast.PropertyBlock((pos1, pos2), (keywordPropertyAst, definitionPropertyAst)) ->
        st.EvalPush("PropertyBlock")
        eval st keywordPropertyAst
        let parent = es.PeekEvalStack()
        let fv = new FplMandatoryPredicate((pos1, pos2), parent) // todo, is this correct? properties might be also functional terms
        es.PushEvalStack(fv)
        eval st definitionPropertyAst
        if not fv.IsIntrinsic then // if not intrinsic, check variable usage
            emitVAR04diagnostics fv
        es.PopEvalStack()
        st.EvalPop()
    // | ReferencingIdentifier of Positions * (Ast * Ast list)
    | ReferencingIdentifier((pos1, pos2), (ast1, asts)) ->
        let parentEvalPath = st.EvalPath()
        st.EvalPush("ReferencingIdentifier")
        eval st ast1
        asts |> List.map (eval st) |> ignore
        if parentEvalPath.EndsWith(".ReferenceToProof") then
            emitPR002Diagnostics pos1 pos2 // avoid referencing to proofs in general considering it not best practice in mathematics
        st.EvalPop()
    | Ast.ConditionFollowedByResult((pos1, pos2), (ast1, asts)) ->
        st.EvalPush("ConditionFollowedByResult")
        eval st ast1
        asts |> List.map (eval st) |> ignore
        st.EvalPop()
    | Ast.Localization((pos1, pos2), (predicateAst, translationListAsts)) ->
        st.EvalPush("Localization")
        let parent = es.PeekEvalStack()
        let fv = new FplLocalization((pos1, pos2), parent)
        let diagList = List<Diagnostic>()
        ad.DiagnosticsStopped <- true // stop all diagnostics during localization
        es.PushEvalStack(fv)
        eval st predicateAst
        translationListAsts |> List.map (fun ast -> 
            eval st ast
            let vars = fv.GetVariables()
            vars
            |> List.filter (fun (var:FplValue) -> var.AuxiliaryInfo = 0)
            |> List.map (fun var ->
                let loc = es.PeekEvalStack()
                let lanList = 
                    loc.Scope 
                    |> Seq.filter (fun kvp -> kvp.Value.FplBlockType = FplBlockType.Language) 
                    |> Seq.map (fun kvp -> kvp.Value) 
                    |> Seq.toList 
                    |> List.rev
                if not lanList.IsEmpty then
                    let lan = lanList.Head
                    diagList.Add(getVAR04diagnostic lan var.FplId)
            )
        ) |> ignore
        es.PopEvalStack()
        ad.DiagnosticsStopped <- false // enable all diagnostics during localization
        diagList
        |> Seq.iter (fun diag -> ad.AddDiagnostic diag)
        st.EvalPop()
    | Ast.FunctionalTermInstance((pos1, pos2), (functionalTermSignatureAst, functionalTermInstanceBlockAst)) ->
        st.EvalPush("FunctionalTermInstance")
        eval st functionalTermSignatureAst
        eval st functionalTermInstanceBlockAst
        st.EvalPop()
    // | All of Positions * ((Ast list * Ast option) list * Ast)
    | Ast.All((pos1, pos2), (namedVarDeclAstList, predicateAst)) ->
        st.EvalPush("All")
        let parent = es.PeekEvalStack()
        let fv = new FplQuantor((pos1, pos2), parent)
        fv.FplId <- literalAll
        fv.TypeId <- literalPred
        es.PushEvalStack(fv)
        fv.Arity <- fv.Arity + (namedVarDeclAstList |> List.length)
        namedVarDeclAstList
        |> List.map (fun namedVarDeclAst ->
            eval st namedVarDeclAst
        )
        |> ignore
        let pred = new FplReference((pos1, pos2), fv)
        es.PushEvalStack(pred)
        eval st predicateAst
        es.PopEvalStack()
        emitVAR05diagnostics fv
        es.PopEvalStack()
        emitLG000orLG001Diagnostics fv "all quantor"
        st.EvalPop()
    | Ast.Exists((pos1, pos2), (namedVarDeclAstList, predicateAst)) ->
        st.EvalPush("Exists")
        let parent = es.PeekEvalStack()
        let fv = new FplQuantor((pos1, pos2), parent)
        fv.FplId <- literalEx
        fv.TypeId <- literalPred
        es.PushEvalStack(fv)
        fv.Arity <- fv.Arity + (namedVarDeclAstList |> List.length)
        namedVarDeclAstList
        |> List.map (fun namedVarDeclAst ->
            eval st namedVarDeclAst
        )
        |> ignore
        let pred = new FplReference((pos1, pos2), fv)
        es.PushEvalStack(pred)
        eval st predicateAst
        es.PopEvalStack()
        emitVAR05diagnostics fv
        es.PopEvalStack()
        emitLG000orLG001Diagnostics fv "exists quantor"
        st.EvalPop()
    // | ExistsN of Positions * ((Ast * (Ast * Ast option)) * Ast)
    | Ast.ExistsN((pos1, pos2), ((dollarDigitsAst, namedVarDeclAst), predicateAst)) ->
        st.EvalPush("ExistsN")
        let parent = es.PeekEvalStack()
        let fv = new FplQuantor((pos1, pos2), parent)
        fv.FplId <- literalExN
        fv.TypeId <- literalPred
        fv.Arity <- 1
        es.PushEvalStack(fv)
        eval st dollarDigitsAst
        eval st namedVarDeclAst
        let pred = new FplReference((pos1, pos2), fv)
        es.PushEvalStack(pred)
        eval st predicateAst
        es.PopEvalStack()
        emitVAR05diagnostics fv
        es.PopEvalStack()
        emitLG000orLG001Diagnostics fv "exists n times quantor"
        st.EvalPop()
    // | FunctionalTermSignature of Positions * (Ast * Ast)
    | Ast.FunctionalTermSignature((pos1, pos2), ((optAst, signatureWithUserDefinedStringAst), mappingAst)) -> 
        es.InSignatureEvaluation <- true
        st.EvalPush("FunctionalTermSignature")
        eval st signatureWithUserDefinedStringAst
        let fv = es.PeekEvalStack()
        match optAst with
        | Some ast1 -> 
            eval st ast1
            if fv.IsFplBlock() then
                fv.FplBlockType <- FplBlockType.FunctionalTerm
            else
                fv.FplBlockType <- FplBlockType.OptionalFunctionalTerm
                fv.TypeId <- literalFunc
        | None -> 
            if fv.IsFplBlock() then
                fv.FplBlockType <- FplBlockType.FunctionalTerm
            else
                fv.FplBlockType <- FplBlockType.MandatoryFunctionalTerm
                fv.TypeId <- literalFunc
        fv.EndPos <- pos2
        eval st mappingAst
        st.EvalPop()
        es.InSignatureEvaluation <- false
    | Ast.PredicateWithQualification(predicateWithOptSpecificationAst, qualificationListAst) ->
        st.EvalPush("PredicateWithQualification")
        eval st predicateWithOptSpecificationAst
        eval st qualificationListAst
        st.EvalPop()
    // | InfixOperation of Positions * (Ast * Ast option) list
    | Ast.InfixOperation((pos1, pos2), separatedPredicateListAst) ->
        st.EvalPush("InfixOperation")
        let fv = es.PeekEvalStack()
        separatedPredicateListAst
        |> List.map (fun (predAst, optOperandAst) -> 
            // evaluate the operand
            let pred = new FplReference((pos1,pos2), fv)
            es.PushEvalStack(pred)
            eval st predAst
            fv.ArgList.Add(es.Pop()) // pop the stack element (same reference as pred) and store it in a list
            // followed by the operator
            match optOperandAst with
            | Some opAst -> 
                let infixOperator = new FplReference((pos1,pos2), fv)
                es.PushEvalStack(infixOperator)
                // evaluate the operator by trying to find a definition for the operator
                eval st opAst
                // store the index of the infix operator, so we still know it after sorting the list by precedence later
                fv.ArgList.Add(es.Pop()) // pop the stack element (same reference as infixOperator) and store it in a list
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
                    runner.Run currentOp currentOp // execute the matched binary operator
                | _ -> ()
            | _ -> ()
            fv.ArgList.RemoveAt(currMinIndex+1) 
            fv.ArgList.RemoveAt(currMinIndex-1) 
        simplifyTriviallyNestedExpressions fv
        let last = es.PeekEvalStack()
        runner.Run last last // execute the last matched binary operator
        st.EvalPop()
    // | Expression of Positions * ((((Ast option * Ast) * Ast option) * Ast option) * Ast)
    | Ast.Expression((pos1, pos2), ((((prefixOpAst, predicateAst), postfixOpAst), optionalSpecificationAst), qualificationListAst)) ->
        st.EvalPush("Expression")
        let fv = es.PeekEvalStack()
        let refBlock = new FplReference((pos1, pos2), fv) 
        es.PushEvalStack(refBlock)
        let ensureReversedPolishNotation = 
            if prefixOpAst.IsSome && postfixOpAst.IsSome then 
                // for heuristic reasons, we choose a precedence of postfix ...
                postfixOpAst |> Option.map (eval st) |> Option.defaultValue () 
                let postfixedInnerPred = new FplReference((pos1,pos2), es.PeekEvalStack())
                es.PushEvalStack(postfixedInnerPred)
                // ... over prefix notation in mathematics
                prefixOpAst |> Option.map (eval st) |> Option.defaultValue ()
                let prefixedInnerPred = new FplReference((pos1,pos2), es.PeekEvalStack())
                es.PushEvalStack(prefixedInnerPred)
                eval st predicateAst
                es.PopEvalStack()
                es.PopEvalStack()
            elif prefixOpAst.IsSome then 
                prefixOpAst |> Option.map (eval st) |> Option.defaultValue ()
                let innerPred = new FplReference((pos1,pos2), es.PeekEvalStack())
                es.PushEvalStack(innerPred)
                eval st predicateAst
                es.PopEvalStack()
            elif postfixOpAst.IsSome then 
                postfixOpAst |> Option.map (eval st) |> Option.defaultValue ()
                let innerPred = new FplReference((pos1,pos2), es.PeekEvalStack())
                es.PushEvalStack(innerPred)
                eval st predicateAst
                es.PopEvalStack()
            else
                eval st predicateAst
        ensureReversedPolishNotation
        optionalSpecificationAst |> Option.map (eval st) |> Option.defaultValue ()
        eval st qualificationListAst
        let refBlock = es.PeekEvalStack() // if the reference was replaced, take this one
        refBlock.EndPos <- pos2
        simplifyTriviallyNestedExpressions refBlock
        let last = es.PeekEvalStack()
        es.PopEvalStack()
        match fv.FplBlockType with
        | FplBlockType.Axiom 
        | FplBlockType.Corollary 
        | FplBlockType.Proposition 
        | FplBlockType.Theorem 
        | FplBlockType.Lemma
        | FplBlockType.Conjecture 
        | FplBlockType.Predicate 
        | FplBlockType.MandatoryPredicate 
        | FplBlockType.OptionalPredicate ->
            fv.ValueList.Add(last)
        | FplBlockType.Reference ->
            // simplify references created due to superfluous parentheses of expressions
            // by replacing them with their single value
            if prefixOpAst.IsNone && 
                postfixOpAst.IsNone &&
                fv.FplId = "" && 
                fv.ArgList.Count = 1 then
                    let subNode = fv.ArgList[0]
                    if subNode.FplBlockType = FplBlockType.Reference then 
                        es.Pop() |> ignore
                        es.PushEvalStack(subNode)
                        subNode.Parent <- fv.Parent
                        fv.ArgList.Clear()
        | FplBlockType.Localization -> 
            fv.FplId <- last.FplId
        | _ -> ()
        st.EvalPop()
    // | Cases of Positions * (Ast list * Ast)
    | Ast.Cases((pos1, pos2), (conditionFollowedByResultListAsts, elseStatementAst)) ->
        st.EvalPush("Cases")
        let fv = es.PeekEvalStack()
        fv.StartPos <- pos1
        fv.EndPos <- pos2
        fv.FplId <- literalCases
        conditionFollowedByResultListAsts 
        |> List.map (fun caseAst ->
            let cas = new FplStmt((pos1,pos2), fv)
            cas.FplId <- "case"
            es.PushEvalStack(cas)
            eval st caseAst
            es.PopEvalStack()
        ) |> ignore
        let cas = new FplStmt((pos1,pos2), fv)
        cas.FplId <- "else"
        es.PushEvalStack(cas)
        eval st elseStatementAst
        es.PopEvalStack()
        st.EvalPop()
    // | Signature of Positions * (Ast * Ast)
    | Ast.Signature((pos1, pos2), (predicateIdentifierAst, paramTupleAst)) ->
        es.InSignatureEvaluation <- true
        st.EvalPush("Signature")
        eval st predicateIdentifierAst
        eval st paramTupleAst
        let fv = es.PeekEvalStack()
        st.EvalPop()
        es.InSignatureEvaluation <- false
    | Ast.Assignment((pos1, pos2), (predicateWithQualificationAst, predicateAst)) ->
        st.EvalPush("Assignment")
        let fv = es.PeekEvalStack()
        fv.StartPos <- pos1
        fv.EndPos <- pos2
        fv.FplId <- $"assign (ln {pos1.Line})"
        let assigneeReference = new FplReference((pos1,pos2), fv)
        es.PushEvalStack(assigneeReference)
        eval st predicateWithQualificationAst
        es.PopEvalStack() 
        let dummyValue = new FplReference((pos1,pos2), fv)
        es.PushEvalStack(dummyValue)
        eval st predicateAst
        es.PopEvalStack() 
        let assignedValueList = fv.ArgList |> Seq.toList |> List.rev 
        let assignedValueOpt = 
            if assignedValueList.Length > 0 then
                Some assignedValueList.Head
            else
                None
        let assigneeOpt = assigneeReference.GetArgument
        match (assigneeOpt, assignedValueOpt) with
        | (Some assignee, Some assignedValue)  ->
            checkSIG05Diagnostics assignee assignedValue
            runner.Run assignedValue assignedValue
            // the scope of the assigned value (which has the FplBlockType.Reference) 
            // either already contains a matching candidate 
            // or not. We will now match the assignee with the 
            let candidateOpt = 
                assignedValue.Scope 
                |> Seq.filter (fun kvp -> kvp.Key = assignedValue.FplId)
                |> Seq.map (fun kvp -> kvp.Value)
                |> Seq.toList
                |> List.tryLast
            let valueOpt = 
                match candidateOpt with
                | Some candidate -> candidate.Instantiate()
                | None -> assignedValue.GetArgument
            match valueOpt with
            | Some value -> assignee.SetValue(value) |> ignore
            | None -> ()
        | _ -> ()
        st.EvalPop()
    | Ast.PredicateInstance((pos1, pos2), ((optAst, signatureAst), predInstanceBlockAst)) ->
        st.EvalPush("PredicateInstance")
        eval st signatureAst
        let fv = es.PeekEvalStack()
        match optAst with
        | Some ast1 -> 
            eval st ast1
            fv.FplBlockType <- FplBlockType.OptionalPredicate
        | None -> 
            fv.FplBlockType <- FplBlockType.MandatoryPredicate
        eval st predInstanceBlockAst
        st.EvalPop()
    | Ast.ParentConstructorCall((pos1, pos2), (inheritedClassTypeAst, argumentTupleAst)) ->
        st.EvalPush("ParentConstructorCall")
        let fv = es.PeekEvalStack()
        fv.StartPos <- pos1
        fv.EndPos <- pos2
        fv.FplId <- "bas"
        let refBlock = new FplReference((pos1, pos2), fv) 
        es.PushEvalStack(refBlock)
        eval st inheritedClassTypeAst
        eval st argumentTupleAst
        es.PopEvalStack()
        if fv.ArgList.Count>0 then
            let parentConstructorCallReference = fv.ArgList[0]
            let parentConstructorCallRefValue = parentConstructorCallReference.GetArgument
            match parentConstructorCallRefValue with
            | Some refVal -> 
                if es.ParentClassCalls.ContainsKey(refVal.FplId) then
                    // Since the reference's id is the same as one of the classes this class is derived from,
                    let derivedClassOpt = parentConstructorCallReference.GetClassBlock()
                    match derivedClassOpt with
                    | Some derivedClass ->
                        let parentClassFilterList = 
                            derivedClass.ArgList 
                            |> Seq.filter (fun pc -> pc.FplId = refVal.FplId)
                            |> Seq.toList
                        if parentClassFilterList.Length > 0 then
                            let parentClass = parentClassFilterList.Head
                            // add the found parent class to the parentClassCalls 
                            if Option.isNone es.ParentClassCalls[refVal.FplId] then 
                                es.ParentClassCalls[parentClass.FplId] <- Some parentClass
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
        let fv = es.PeekEvalStack()
        let arg = new FplArgument((pos1, pos2), fv) 
        es.PushEvalStack(arg)
        eval st argIdAst
        eval st argAst
        es.PopEvalStack()
        st.EvalPop()
    // | ForIn of Positions * ((Ast * Ast) * Ast list)
    | Ast.ForIn((pos1, pos2), ((entityAst, inDomainAst), statementListAst)) ->
        st.EvalPush("ForIn")
        let fv = es.PeekEvalStack()
        fv.StartPos <- pos1
        fv.EndPos <- pos2
        fv.FplId <- literalFor
        let entity = new FplReference((pos1,pos2), fv)
        es.PushEvalStack(entity)
        eval st entityAst
        es.PopEvalStack()
        let inDomain = new FplReference((pos1,pos2), fv)
        es.PushEvalStack(inDomain)
        eval st inDomainAst
        es.PopEvalStack()
        statementListAst 
        |> List.map (fun stmtAst ->
            let stmt = new FplStmt((pos1,pos2), fv)
            es.PushEvalStack(stmt)
            eval st stmtAst
            es.PopEvalStack()
        ) |> ignore
        st.EvalPop()
    // | SignatureWithPreConBlock of Ast * ((Ast list option * Ast) * Ast)
    | Ast.PremiseConclusionBlock((pos1, pos2), ((optVarDeclOrSpecList, premiseAst), conclusionAst)) ->
        st.EvalPush("PremiseConclusionBlock")
        optVarDeclOrSpecList |> Option.map (List.map (eval st) >> ignore) |> Option.defaultValue ()
        eval st premiseAst
        eval st conclusionAst
        st.EvalPop()
    // | Theorem of Positions * (Ast * (Ast list option * Ast))
    | Ast.Theorem((pos1, pos2), (signatureAst, (optVarDeclOrSpecList, predicateAst))) ->
        st.EvalPush("Theorem")
        let parent = es.PeekEvalStack()
        let fv = new FplTheorem((pos1, pos2), parent)
        es.PushEvalStack(fv)
        eval st signatureAst
        evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst
        emitVAR04diagnostics fv
        es.PopEvalStack()
        st.EvalPop()
    | Ast.Lemma((pos1, pos2), (signatureAst, (optVarDeclOrSpecList, predicateAst))) ->
        st.EvalPush("Lemma")
        let parent = es.PeekEvalStack()
        let fv = new FplLemma((pos1, pos2), parent)
        es.PushEvalStack(fv)
        eval st signatureAst
        evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst
        emitVAR04diagnostics fv
        es.PopEvalStack()
        st.EvalPop()
    | Ast.Proposition((pos1, pos2), (signatureAst, (optVarDeclOrSpecList, predicateAst))) ->
        st.EvalPush("Proposition")
        let parent = es.PeekEvalStack()
        let fv = new FplProposition((pos1, pos2), parent)
        es.PushEvalStack(fv)
        eval st signatureAst
        evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst
        emitVAR04diagnostics fv
        es.PopEvalStack()
        st.EvalPop()
    | Ast.Conjecture((pos1, pos2), (signatureAst, (optVarDeclOrSpecList, predicateAst))) ->
        st.EvalPush("Conjecture")
        let parent = es.PeekEvalStack()
        let fv = new FplConjecture((pos1, pos2), parent)
        es.PushEvalStack(fv)
        eval st signatureAst
        evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst
        emitVAR04diagnostics fv
        es.PopEvalStack()
        st.EvalPop()
    | Ast.Axiom((pos1, pos2), (signatureAst, (optVarDeclOrSpecList, predicateAst))) ->
        st.EvalPush("Axiom")
        let parent = es.PeekEvalStack()
        let fv = new FplAxiom((pos1, pos2), parent)
        es.PushEvalStack(fv)
        eval st signatureAst
        evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst
        emitVAR04diagnostics fv
        es.PopEvalStack()
        st.EvalPop()
    // | Corollary of Positions * ((Ast * Ast) * (Ast list option * Ast))
    | Ast.CorollarySignature(referencingIdentifierAst, paramTupleAst) ->
        st.EvalPush("CorollarySignature")
        es.InSignatureEvaluation <- true
        eval st referencingIdentifierAst
        eval st paramTupleAst
        es.InSignatureEvaluation <- false
        st.EvalPop()
    | Ast.Corollary((pos1, pos2), (corollarySignatureAst, (optVarDeclOrSpecList, predicateAst))) ->
        st.EvalPush("Corollary")
        let parent = es.PeekEvalStack()
        let fv = new FplCorollary((pos1, pos2), parent)
        es.PushEvalStack(fv)
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
            emitID007diagnostics fv listOfKandidates  
        | _ -> ()
        evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst
        // now, we are ready to emit VAR03 diagnostics for all variables declared in the signature of the corollary.
        emitVAR03diagnosticsForCorollaryOrProofVariable fv  
        emitVAR04diagnostics fv
        es.PopEvalStack()
        st.EvalPop()
    // | NamedVarDecl of Positions * ((Ast list * Ast) * Ast)
    | Ast.NamedVarDecl((pos1, pos2), ((variableListAst, varDeclModifierAst), variableTypeAst)) ->
        st.EvalPush("NamedVarDecl")
        let fv = es.PeekEvalStack()
        fv.AuxiliaryInfo <- variableListAst |> List.length // remember how many variables to create
        // create all variables of the named variable declaration in the current scope
        variableListAst |> List.iter (fun varAst ->
            eval st varAst // here, the var is created and put on stack, but not popped
            eval st varDeclModifierAst
            eval st variableTypeAst
            es.PopEvalStack() // take the var from stack 
        ) |> ignore 
        st.EvalPop()
    // | Axiom of Constructor * (Ast * (Ast list option * Ast))
    | Ast.Constructor((pos1, pos2), (signatureAst, (optVarDeclOrSpecListAst, keywordSelfAst))) ->
        st.EvalPush("Constructor")
        let parent = es.PeekEvalStack()
        let fv = new FplConstructor((pos1, pos2), parent)
        es.PushEvalStack(fv)
        eval st signatureAst
        
        // Initialize the counters of parent classes before evaluating the declaration block
        // of the constructor in which we want to count the calls to parent classes.
        // (we need to reset the counters for every constructor of the same class to avoid 
        // ID020 false positives for the wrong constructors)
        es.ParentClassCountersInitialize()  

        // evaluate the declaration block
        match optVarDeclOrSpecListAst with
        | Some astList -> astList |> List.map (eval st) |> ignore
        | None -> ()

        // check if the constructor calls all necessary parent classes
        es.ParentClassCalls 
        |> Seq.iter (fun kvp -> 
            match kvp.Value with
            | Some calledClassNode -> ()
            | None ->
                // for this class no parent class was called 
                emitID020Diagnostics kvp.Key pos1
        )

        let rb = new FplReference((pos1, pos2), fv)
        es.PushEvalStack(rb)
        eval st keywordSelfAst
        es.PopEvalStack()
        emitVAR04diagnostics fv
        es.PopEvalStack()
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
    | Ast.DefinitionPredicate((pos1, pos2), (signatureWithUserDefinedStringAst, (predicateContentAst, optPropertyListAsts))) ->
        st.EvalPush("DefinitionPredicate")
        let fplTheory = es.PeekEvalStack()
        let fv = new FplPredicate((pos1, pos2), fplTheory)
        es.PushEvalStack(fv)
        es.InSignatureEvaluation <- true
        eval st signatureWithUserDefinedStringAst
        es.InSignatureEvaluation <- false
        eval st predicateContentAst
        optPropertyListAsts |> Option.map (List.map (eval st) >> ignore) |> Option.defaultValue ()
        if not fv.IsIntrinsic then // if not intrinsic, check variable usage
            emitVAR04diagnostics fv
        else    
            let value = new FplIntrinsicPred((pos1, pos2), fv)
            fv.ValueList.Add(value)
        es.PopEvalStack()
        st.EvalPop()
    // | DefinitionFunctionalTerm of Positions * ((Ast * Ast) * (Ast * Ast list option))
    | Ast.DefinitionFunctionalTerm((pos1, pos2), (functionalTermSignatureAst, (funcContentAst, optPropertyListAsts))) ->
        st.EvalPush("DefinitionFunctionalTerm")
        let parent = es.PeekEvalStack()
        let fv = new FplFunctionalTerm((pos1, pos2), parent)
        es.PushEvalStack(fv)
        eval st functionalTermSignatureAst
        eval st funcContentAst
        optPropertyListAsts |> Option.map (List.map (eval st) >> ignore) |> Option.defaultValue ()
        if not fv.IsIntrinsic then // if not intrinsic, check variable usage
            emitVAR04diagnostics fv
        es.PopEvalStack()
        st.EvalPop()
    // | DefinitionClass of Positions * (((Ast * Ast option) * Ast list) * (Ast * Ast list option))
    | Ast.DefinitionClass((pos1, pos2),
                          (((predicateIdentifierAst, optUserDefinedObjSymAst), classTypeListAsts),
                           (classContentAst, optPropertyListAsts))) ->
        st.EvalPush("DefinitionClass")
        let parent = es.PeekEvalStack()
        let fv = new FplClass((pos1, pos2), parent)
        es.PushEvalStack(fv)
        es.InSignatureEvaluation <- true

        eval st predicateIdentifierAst
        es.InSignatureEvaluation <- false
        optUserDefinedObjSymAst |> Option.map (eval st) |> Option.defaultValue ()

        // clear the storage of parent class counters before evaluating the list of parent classes
        es.ParentClassCalls.Clear() 
        // now evaluate the list of parent classes while adding the identified classes to the storage
        classTypeListAsts |> List.map (eval st) |> ignore

        eval st classContentAst
        optPropertyListAsts
        |> Option.map (List.map (eval st) >> ignore)
        |> Option.defaultValue ()
        emitVAR04diagnostics fv
        es.PopEvalStack()
        st.EvalPop()
    // | DerivedPredicate of Ast
    | Ast.DerivedPredicate ((pos1, pos2),predicateAst) -> 
        st.EvalPush("DerivedPredicate")
        let fv = es.PeekEvalStack()
        let argInf = new FplArgInference((pos1, pos2), fv) 
        argInf.FplId <- "derive"
        es.PushEvalStack(argInf)
        eval st predicateAst
        es.PopEvalStack()
        st.EvalPop()
    // | Proof of Positions * (Ast * (Ast list * Ast option))
    | Ast.Proof((pos1, pos2), (referencingIdentifierAst, (proofArgumentListAst, optQedAst))) ->
        st.EvalPush("Proof")
        let parent = es.PeekEvalStack()
        let fv = new FplProof((pos1, pos2), parent)
        es.PushEvalStack(fv)
        eval st referencingIdentifierAst
        match tryFindAssociatedBlockForProof fv with
        | ScopeSearchResult.FoundAssociate potentialParent -> 
            // everything is ok, change the parent of the provable from theory to the found parent 
            fv.Parent <- Some potentialParent
        | ScopeSearchResult.FoundIncorrectBlock block ->
            emitID002diagnostics fv block  
        | ScopeSearchResult.NotFound ->
            emitID003diagnostics fv  
        | ScopeSearchResult.FoundMultiple listOfKandidates ->
            emitID004diagnostics fv listOfKandidates  
        | _ -> ()
        proofArgumentListAst |> List.map (eval st) |> ignore
        // now, we are ready to emit VAR03 diagnostics for all variables declared in the signature of the proof.
        emitVAR03diagnosticsForCorollaryOrProofVariable fv  
        optQedAst |> Option.map (eval st) |> Option.defaultValue ()
        emitVAR04diagnostics fv
        let value = new FplIntrinsicPred((pos1,pos1), fv)
        value.FplId <- literalTrue
        // check if all arguments could be correctly inferred
        fv.Scope
        |> Seq.filter (fun kvp -> kvp.Value.FplBlockType = FplBlockType.Argument)
        |> Seq.iter (fun kvp -> 
            let argInference = kvp.Value.ArgList[1]
            let argInferenceResult = argInference.Represent()
            match argInferenceResult with
            | FplGrammarCommons.literalTrue -> ()
            | _ -> value.FplId <- literalFalse // todo all other arguments that are either undetermined or false should issue an error

        )
        fv.ValueList.Add(value)
        es.PopEvalStack()
        st.EvalPop()
    | Ast.Precedence((pos1, pos2), precedence) ->
        st.EvalPush("Precedence")
        let fv = es.PeekEvalStack()
        fv.AuxiliaryInfo <- precedence
        st.EvalPop()
    | ast1 ->
        let astType = ast1.GetType().Name
        emitID000Diagnostics astType


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
            es.ClearEvalStack()
            // evaluate the ParsedAst of a theory
            let theoryValue = new FplTheory((Position("",0,1,1), Position("",0,1,1)), st.Root, pa.Parsing.Uri.AbsolutePath);
            if not (st.Root.Scope.ContainsKey(pa.Id)) then
                st.Root.Scope.Add(pa.Id, theoryValue)
            else
                st.Root.Scope[pa.Id] <- theoryValue
            theoryValue.FplId <- pa.Id
            theoryValue.TypeId <- pa.Id
            es.PushEvalStack(theoryValue)
            ad.CurrentUri <- pa.Parsing.Uri
            eval st pa.Parsing.Ast
            pa.Status <- ParsedAstStatus.Evaluated
            es.PopEvalStack()
        | None -> found <- false
