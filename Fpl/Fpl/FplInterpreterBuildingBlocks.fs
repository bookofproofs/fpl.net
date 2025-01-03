module FplInterpreterBuildingBlocks

open System
open System.Collections.Generic
open FParsec
open ErrDiagnostics
open FplGrammarTypes
open FplInterpreterTypes
open FplInterpreterDiagnosticsEmitter
open FplInterpreterPredicateEvaluator
open FplInterpreterRunner


type EvalStack() = 
    let _valueStack = Stack<FplValue>()
    let mutable _inSignatureEvaluation = false

    /// Indicates if this EvalStack is evaluating a signature on a FPL building block
    member this.InSignatureEvaluation
        with get () = _inSignatureEvaluation
        and set (value) = _inSignatureEvaluation <- value

    /// Adds the FplValue to it's parent's Scope.
    static member tryAddToScope (fv:FplValue) = 
        let next = fv.Parent.Value
        let identifier = 
            match fv.BlockType with
            |  FplValueType.Constructor -> 
                fv.Type(SignatureType.Mixed)
            | _ -> 
                if FplValue.IsBlock(fv) then 
                    fv.Type(SignatureType.Mixed)
                elif FplValue.IsVariable(fv) then 
                    fv.FplId
                else
                    fv.Type(SignatureType.Name)
        match FplValue.InScopeOfParent(fv) identifier with
        | ScopeSearchResult.Found conflict -> 
            match next.BlockType with
            | FplValueType.Justification -> 
                emitPR004Diagnostics fv conflict 
            | _ -> 
                match fv.BlockType with
                | FplValueType.Language -> 
                    let oldDiagnosticsStopped = ad.DiagnosticsStopped
                    ad.DiagnosticsStopped <- false
                    emitID014diagnostics fv conflict 
                    ad.DiagnosticsStopped <- oldDiagnosticsStopped
                | FplValueType.Argument -> 
                    emitPR003diagnostics fv conflict 
                | FplValueType.Variable -> 
                    ()
                | _ ->
                    emitID001diagnostics fv conflict 
        | _ -> 
            next.Scope.Add(identifier,fv)

    /// adds the FplValue to it's parent's ValueList
    static member tryAddToValueList (fv:FplValue) = 
        let next = fv.Parent.Value
        next.ValueList.Add(fv)

    // Pops an FplValue from stack without propagating it's name and signature to the next FplValue on the stack.
    member this.Pop() = _valueStack.Pop()

    // Pops an FplValue from stack and propagates it's name and signature to the next FplValue on the stack.
    member this.PopEvalStack() = 
        let fv = _valueStack.Pop()
        if _valueStack.Count > 0 then
            let next = _valueStack.Peek()

            match fv.BlockType with
            | FplValueType.Proof 
            | FplValueType.Corollary ->
                EvalStack.tryAddToScope fv
            | FplValueType.Class 
            | FplValueType.Theorem
            | FplValueType.Localization
            | FplValueType.Lemma
            | FplValueType.Proposition
            | FplValueType.Conjecture
            | FplValueType.RuleOfInference
            | FplValueType.Constructor
            | FplValueType.MandatoryPredicate
            | FplValueType.OptionalPredicate
            | FplValueType.MandatoryFunctionalTerm
            | FplValueType.OptionalFunctionalTerm
            | FplValueType.Axiom
            | FplValueType.Predicate
            | FplValueType.Extension
            | FplValueType.Argument 
            | FplValueType.Language 
            | FplValueType.FunctionalTerm ->
                EvalStack.tryAddToScope fv
            | FplValueType.Reference ->
                match next.BlockType with
                | FplValueType.Localization -> 
                    next.FplId <- fv.FplId
                    next.TypeId <- fv.TypeId
                    next.NameEndPos <- fv.NameEndPos
                | FplValueType.Justification -> 
                    EvalStack.tryAddToScope fv
                | FplValueType.Argument ->
                    EvalStack.tryAddToValueList fv 
                | FplValueType.Axiom
                | FplValueType.Theorem 
                | FplValueType.Lemma 
                | FplValueType.Proposition 
                | FplValueType.Corollary 
                | FplValueType.Conjecture 
                | FplValueType.Proof 
                | FplValueType.RuleOfInference 
                | FplValueType.Predicate 
                | FplValueType.FunctionalTerm 
                | FplValueType.Class 
                | FplValueType.Constructor
                | FplValueType.MandatoryFunctionalTerm
                | FplValueType.OptionalFunctionalTerm
                | FplValueType.MandatoryPredicate
                | FplValueType.OptionalPredicate ->
                    EvalStack.tryAddToValueList fv 
                | FplValueType.Quantor ->
                    EvalStack.tryAddToValueList fv 
                    next.NameEndPos <- fv.NameEndPos
                | _ -> 
                    if next.Scope.ContainsKey(".") then 
                        ()
                    else
                        EvalStack.tryAddToValueList fv
                    next.NameEndPos <- fv.NameEndPos
            | FplValueType.Variable
            | FplValueType.VariadicVariableMany
            | FplValueType.VariadicVariableMany1 ->
                match next.BlockType with 
                | FplValueType.Theorem
                | FplValueType.Lemma
                | FplValueType.Proposition
                | FplValueType.Conjecture
                | FplValueType.RuleOfInference
                | FplValueType.Constructor
                | FplValueType.Corollary
                | FplValueType.Proof
                | FplValueType.MandatoryPredicate
                | FplValueType.OptionalPredicate
                | FplValueType.MandatoryFunctionalTerm
                | FplValueType.OptionalFunctionalTerm
                | FplValueType.Axiom
                | FplValueType.Predicate
                | FplValueType.Class
                | FplValueType.Mapping 
                | FplValueType.Variable 
                | FplValueType.VariadicVariableMany
                | FplValueType.VariadicVariableMany1 
                | FplValueType.FunctionalTerm ->
                    EvalStack.tryAddToScope fv
                | FplValueType.Quantor  
                | FplValueType.Localization -> 
                    EvalStack.tryAddToScope fv
                | FplValueType.Reference ->
                    EvalStack.tryAddToValueList fv
                | _ -> ()
            | FplValueType.Object
            | FplValueType.Quantor
            | FplValueType.Theory
            | FplValueType.Justification 
            | FplValueType.ArgInference 
            | FplValueType.Mapping 
            | FplValueType.Translation 
            | FplValueType.Stmt
            | FplValueType.Assertion
            | FplValueType.Root -> 
                EvalStack.tryAddToValueList fv 


    // Pushes an FplValue to the stack.
    member this.PushEvalStack fv = _valueStack.Push fv

    // Peeks an FplValue from the stack.
    member this.PeekEvalStack() = _valueStack.Peek()

    // Clears stack.
    member this.ClearEvalStack() = _valueStack.Clear()

let es = EvalStack()
let run = FplRunner()

let eval_string (st: SymbolTable) s = ()

let eval_pos_string (st: SymbolTable) (startpos: Position) (endpos: Position) ast = ()

let eval_pos_ast_ast_opt (st: SymbolTable) (startpos: Position) (endpos: Position) = ()

let eval_pos_char_list (st: SymbolTable) (startpos: Position) (endpos: Position) charlist =
    charlist |> List.map string |> String.concat "" |> ignore

let eval_pos_string_ast (st: SymbolTable) str = ()

/// Simplify trivially nested expressions 
let simplifyTriviallyNestedExpressions (rb:FplValue) = 
    if rb.ValueList.Count = 1 && rb.FplId = "" then
        let subNode = rb.ValueList[0]
        if subNode.BlockType = FplValueType.Reference || subNode.BlockType = FplValueType.Quantor then 
            es.Pop() |> ignore
            es.PushEvalStack(subNode)
            subNode.Parent <- rb.Parent
            subNode.NameEndPos <- rb.NameEndPos
            if rb.Scope.ContainsKey(".") then 
                subNode.Scope.Add(".",rb.Scope["."])
            match rb.Parent with 
            | Some parent -> 
                if parent.Scope.ContainsKey(".") then
                   parent.Scope["."] <- subNode
            | _ -> ()
            // prevent recursive clearing of the subNode
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
        fv.BlockType <- blockType

    let setUnitType (fv:FplValue) typeName typeRepr =
        match fv.BlockType with 
        | FplValueType.VariadicVariableMany -> 
            fv.TypeId <- $"*{typeName}"
            fv.ReprId <- $"intr *{typeRepr}"
        | FplValueType.VariadicVariableMany1 -> 
            fv.TypeId <- $"+{typeName}"
            fv.ReprId <- $"intr +{typeRepr}"
        | FplValueType.Variable -> 
            fv.TypeId <- typeName
            fv.ReprId <- typeRepr
        | FplValueType.Mapping -> 
            fv.TypeId <- typeName
            fv.ReprId <- typeRepr
        | _ -> ()

    match ast with
    // units: | Star
    | Ast.IndexType((pos1, pos2),()) -> 
        st.EvalPush("IndexType")
        let fv = es.PeekEvalStack()
        setUnitType fv "ind" "0"
        st.EvalPop() |> ignore
    | Ast.ObjectType((pos1, pos2),()) -> 
        st.EvalPush("ObjectType")
        let fv = es.PeekEvalStack()
        setUnitType fv "obj" "intr obj"
        checkID009_ID010_ID011_Diagnostics st fv "obj" pos1 pos2
        checkID012Diagnostics st fv "obj" pos1 pos2 
        // we need an extra FplValue for objects to enable class inheritance from them
        let fv1 = FplValue.CreateFplValue((pos1,pos2),FplValueType.Object, es.PeekEvalStack())
        es.PushEvalStack(fv1)
        es.PopEvalStack()
        st.EvalPop()
    | Ast.PredicateType((pos1, pos2),()) -> 
        st.EvalPush("PredicateType")
        let fv = es.PeekEvalStack()
        setUnitType fv "pred" "undetermined"
        st.EvalPop()
    | Ast.FunctionalTermType((pos1, pos2),()) -> 
        st.EvalPush("FunctionalTermType")
        let fv = es.PeekEvalStack()
        setUnitType fv "func" "func"
        st.EvalPop()
    | Ast.Many((pos1, pos2),()) ->
        st.EvalPush("Many")
        evalMany FplValueType.VariadicVariableMany pos1 pos2
        st.EvalPop()
    | Ast.Many1((pos1, pos2),()) ->
        st.EvalPush("Many1")
        evalMany FplValueType.VariadicVariableMany1 pos1 pos2
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
        fv.ReprId <- s
        st.EvalPop() 
    // | DollarDigits of Positions * int
    | Ast.DollarDigits((pos1, pos2), s) -> 
        st.EvalPush("DollarDigits")
        let fv = es.PeekEvalStack()
        let sid = $"${s.ToString()}"
        fv.FplId <- fv.FplId + sid
        fv.TypeId <- 
            if fv.TypeId<>"" then 
                fv.TypeId + sid
            else
                "ind"
        fv.NameEndPos <- pos2
        st.EvalPop() 
    | Ast.ExtensionName((pos1, pos2), s) ->
        st.EvalPush("ExtensionName")
        let fv = es.PeekEvalStack()
        let extensionName = $"@{s}"
        match fv.BlockType with 
        | FplValueType.Extension ->
            fv.FplId <- extensionName
            fv.TypeId <- extensionName
            fv.NameStartPos <- pos1
            fv.NameEndPos <- pos2
        | FplValueType.VariadicVariableMany -> 
            let sid = $"*{extensionName}"
            fv.TypeId <- sid
            fv.ReprId <- $"intr {sid}"
        | FplValueType.VariadicVariableMany1 -> 
            let sid = $"+{extensionName}"
            fv.TypeId <- sid
            fv.ReprId <- $"intr {sid}"
        | _ -> 
            fv.TypeId <- extensionName
            fv.ReprId <- $"intr {extensionName}"
            checkID019Diagnostics st extensionName pos1 pos2
        st.EvalPop() 
    | Ast.TemplateType((pos1, pos2), s) -> 
        st.EvalPush("TemplateType")
        let fv = es.PeekEvalStack()
        setUnitType fv s s
        st.EvalPop() 
    | Ast.Var((pos1, pos2), name) ->
        st.EvalPush("Var")
        let evalPath = st.EvalPath()
        let isDeclaration = evalPath.Contains("NamedVarDecl.") 
        let isLocalizationDeclaration = evalPath.StartsWith("AST.Namespace.Localization.Expression.")
        let diagnosticsStopFlag = ad.DiagnosticsStopped
        ad.DiagnosticsStopped <- false // enable var-related diagnostics in AST.Var, even if it was stopped (e.g. in Ast.Localization)
        let fv = es.PeekEvalStack()
        let varValue = FplValue.CreateFplValue((pos1,pos2), FplValueType.Variable, fv)
        varValue.FplId <- name
        varValue.TypeId <- "undef"
        varValue.ReprId <- "undef"
        varValue.IsSignatureVariable <- es.InSignatureEvaluation 
        if isDeclaration then 
            match FplValue.VariableInBlockScopeByName fv name false with 
            | ScopeSearchResult.Found other ->
                // replace the variable by other on stack
                es.PushEvalStack(other)
                emitVAR03diagnostics varValue other 
            | _ -> 
                es.PushEvalStack(varValue)
        elif isLocalizationDeclaration then 
            match FplValue.VariableInBlockScopeByName fv name false with 
            | ScopeSearchResult.Found other ->
                emitVAR03diagnostics varValue other 
            | _ -> 
                let rec getLocalization (fValue:FplValue) = 
                    if fValue.BlockType = FplValueType.Localization then
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
            match FplValue.VariableInBlockScopeByName fv name true with 
            | ScopeSearchResult.Found other -> 
                match fv.BlockType with
                | FplValueType.Reference ->
                    if not (fv.Scope.ContainsKey(name)) then
                        fv.Scope.Add(name, other)
                | _ -> ()
                // count usages of the variable in scope
                other.AuxiliaryInfo <- other.AuxiliaryInfo + 1
            | _ -> 
                // otherwise emit variable not declared if this is not a declaration 
                emitVAR01diagnostics name pos1 pos2
            fv.FplId <- name
            fv.TypeId <- "undef"
            fv.ReprId <- "undef"
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
        fv.NameStartPos <- pos1
        fv.NameEndPos <- pos2
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
        fv.NameStartPos <- pos1
        fv.NameEndPos <- pos2
        emitSIG01Diagnostics st fv pos1 pos2 
        st.EvalPop()
    | Ast.ArgumentIdentifier((pos1, pos2), s) -> 
        st.EvalPush("ArgumentIdentifier")
        let setId (fValue:FplValue) = 
            fValue.FplId <- s
            fValue.TypeId <- "pred"
            fValue.NameStartPos <- pos1
            fValue.NameEndPos <- pos2
        let fv = es.PeekEvalStack()
        setId fv
        let parent = fv.Parent.Value
        match parent.BlockType with
        | FplValueType.ArgInference 
        | FplValueType.Justification ->
            let arg = parent.Parent.Value
            let proof = arg.Parent.Value
            if not (proof.Scope.ContainsKey(s)) then 
                emitPR005Diagnostics fv 
        | FplValueType.Argument -> ()
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
        fv.NameStartPos <- pos1
        fv.NameEndPos <- pos2
        emitSIG01Diagnostics st fv pos1 pos2 
        st.EvalPop() 
    | Ast.PostfixOperator((pos1, pos2), symbol) -> 
        st.EvalPush("PostfixOperator")
        let fv = es.PeekEvalStack()
        fv.FplId <- symbol
        fv.TypeId <- symbol
        fv.NameStartPos <- pos1
        fv.NameEndPos <- pos2
        emitSIG01Diagnostics st fv pos1 pos2 
        st.EvalPop() 
    | Ast.PrefixOperator((pos1, pos2), symbol) -> 
        st.EvalPush("PrefixOperator")
        let fv = es.PeekEvalStack()
        fv.FplId <- symbol
        fv.TypeId <- symbol
        fv.NameStartPos <- pos1
        fv.NameEndPos <- pos2
        emitSIG01Diagnostics st fv pos1 pos2 
        st.EvalPop() 
    // | Self of Positions * unit
    | Ast.Self((pos1, pos2), _) -> 
        st.EvalPush("Self")
        let rb = es.PeekEvalStack()
        rb.NameStartPos <- pos1
        rb.NameEndPos <- pos2
        rb.FplId <- "self"
        rb.TypeId <- "self"
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
        rb.NameStartPos <- pos1
        rb.NameEndPos <- pos2
        rb.FplId <- "parent"
        rb.TypeId <- "parent"
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
        fv.NameStartPos <- pos1
        fv.NameEndPos <- pos2
        fv.FplId <- "true"
        fv.ReprId <- "true"
        fv.TypeId <- "pred"
        st.EvalPop() 
    | Ast.False((pos1, pos2), _) -> 
        st.EvalPush("False")
        let fv = es.PeekEvalStack()
        fv.NameStartPos <- pos1
        fv.NameEndPos <- pos2
        fv.FplId <- "false"
        fv.ReprId <- "false"
        fv.TypeId <- "pred"
        st.EvalPop() 
    | Ast.Undefined((pos1, pos2), _) -> 
        st.EvalPush("Undefined")
        let fv = es.PeekEvalStack()
        fv.NameStartPos <- pos1
        fv.NameEndPos <- pos2
        fv.FplId <- "undef"
        fv.TypeId <- "undef"
        st.EvalPop() 
    | Ast.Trivial((pos1, pos2), _) -> 
        st.EvalPush("Trivial")
        let refBlock = FplValue.CreateFplValue((pos1, pos2), FplValueType.Reference, es.PeekEvalStack()) 
        es.PushEvalStack(refBlock)
        refBlock.FplId <- "trivial"
        refBlock.TypeId <- "trivial"
        es.PopEvalStack()
        st.EvalPop() 
    | Ast.Qed((pos1, pos2), _) -> 
        st.EvalPush("Qed")
        st.EvalPop() 
    | Ast.RuleOfInference((pos1, pos2), (signatureAst, premiseConclusionBlockAst)) ->
        st.EvalPush("RuleOfInference")
        let fv = FplValue.CreateFplValue((pos1, pos2), FplValueType.RuleOfInference, es.PeekEvalStack())
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
        let map = FplValue.CreateFplValue((pos1, pos2),FplValueType.Mapping,fv)
        map.ReprId <- ""
        es.PushEvalStack(map)
        eval st variableTypeAst
        fv.ReprId <- map.ReprId
        es.PopEvalStack()
        st.EvalPop()
    | Ast.ClassIdentifier((pos1, pos2), ast1) ->
        st.EvalPush("ClassIdentifier")
        eval st ast1
        let fv = es.PeekEvalStack()
        fv.NameEndPos <- pos2
        st.EvalPop()
    | Ast.Extension((pos1, pos2), extensionString) ->
        st.EvalPush("Extension")
        let fv = es.PeekEvalStack()
        fv.FplId <- extensionString
        fv.TypeId <- extensionString
        fv.ReprId <- extensionString
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
        fv.FplId <- "not"
        fv.TypeId <- "pred"
        eval st predicateAst
        fv.NameEndPos <- pos2
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
        fv.NameStartPos <- pos1
        fv.NameEndPos <- pos2
        fv.FplId <- "assert"
        let rb = FplValue.CreateFplValue((pos1,pos2), FplValueType.Reference, fv)
        es.PushEvalStack(rb)
        eval st predicateAst
        es.PopEvalStack()
        st.EvalPop()
    | Ast.ByDef((pos1, pos2), predicateWithQualificationAst) ->
        st.EvalPush("ByDef")
        let fv = es.PeekEvalStack()
        fv.FplId <- "bydef."
        fv.ReprId <- "undetermined"
        fv.TypeId <- "bydef."
        eval st predicateWithQualificationAst
        emitPR001Diagnostics fv pos1 pos2
        st.EvalPop()
    | Ast.DottedPredicate((pos1, pos2), predicateWithOptSpecificationAst) ->
        st.EvalPush("DottedPredicate")
        let fv = es.PeekEvalStack()
        let refBlock = FplValue.CreateFplValue((pos1, pos2), FplValueType.Reference, fv) 
        fv.Scope.Add(".",refBlock)
        es.PushEvalStack(refBlock)
        eval st predicateWithOptSpecificationAst
        es.PopEvalStack()
        st.EvalPop()
    | Ast.Return((pos1, pos2), returneeAst) ->
        st.EvalPush("Return")
        let fv = es.PeekEvalStack()
        let stmt = FplValue.CreateFplValue((pos1,pos2), FplValueType.Stmt, fv)
        stmt.FplId <- "return"
        es.PushEvalStack(stmt)
        eval st returneeAst
        let refBlock = stmt.ValueList[0]
        emitSIG03Diagnostics refBlock fv
        es.PopEvalStack() 
        st.EvalPop()
    | Ast.AssumeArgument((pos1, pos2), predicateAst) ->
        st.EvalPush("AssumeArgument")
        let fv = es.PeekEvalStack()
        let argInf = FplValue.CreateFplValue((pos1, pos2), FplValueType.ArgInference, fv) 
        argInf.FplId <- "assume"
        es.PushEvalStack(argInf)
        eval st predicateAst
        es.PopEvalStack()
        st.EvalPop()
    | Ast.RevokeArgument((pos1, pos2), predicateAst) ->
        st.EvalPush("RevokeArgument")
        let fv = es.PeekEvalStack()
        let argInf = FplValue.CreateFplValue((pos1, pos2), FplValueType.ArgInference, fv) 
        argInf.FplId <- "revoke"
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
        match fv.BlockType with 
        | FplValueType.Class -> 
            if evalPath.EndsWith("InheritedClassType.PredicateIdentifier") then 
                checkID009_ID010_ID011_Diagnostics st fv identifier pos1 pos2
            else
                fv.FplId <- identifier
                fv.TypeId <- identifier
                fv.ReprId <- $"class {identifier}"
                checkID008Diagnostics fv pos1 pos2
                checkID009_ID010_ID011_Diagnostics st fv identifier pos1 pos2
        | FplValueType.Axiom
        | FplValueType.Theorem 
        | FplValueType.Lemma 
        | FplValueType.Proposition 
        | FplValueType.Corollary 
        | FplValueType.Conjecture 
        | FplValueType.Proof 
        | FplValueType.RuleOfInference 
        | FplValueType.MandatoryPredicate
        | FplValueType.OptionalPredicate
        | FplValueType.Predicate ->
                fv.FplId <- identifier
                fv.TypeId <- "pred"
                fv.ReprId <- "undetermined"
                checkID008Diagnostics fv pos1 pos2
                checkID009_ID010_ID011_Diagnostics st fv identifier pos1 pos2
        | FplValueType.MandatoryFunctionalTerm
        | FplValueType.OptionalFunctionalTerm
        | FplValueType.FunctionalTerm ->
                fv.FplId <- identifier
                fv.TypeId <- "func"
                checkID008Diagnostics fv pos1 pos2
                checkID009_ID010_ID011_Diagnostics st fv identifier pos1 pos2
        | FplValueType.Constructor -> 
                fv.FplId <- identifier
                fv.TypeId <- identifier
                fv.ReprId <- "obj"
                checkID008Diagnostics fv pos1 pos2
                checkID009_ID010_ID011_Diagnostics st fv identifier pos1 pos2
        | FplValueType.VariadicVariableMany -> 
            fv.TypeId <- $"*{identifier}"
        | FplValueType.VariadicVariableMany1 -> 
            fv.TypeId <- $"+{identifier}"
        | FplValueType.Variable -> 
            fv.TypeId <- identifier
        | FplValueType.Mapping -> 
            fv.TypeId <- fv.TypeId + identifier
        | FplValueType.Reference -> 
            fv.FplId <- fv.FplId + identifier
            fv.TypeId <- fv.TypeId + identifier
            checkID012Diagnostics st fv identifier pos1 pos2
            
        | _ -> ()
        if evalPath.Contains(".NamedVarDecl.") || evalPath.Contains(".VariableType.ClassType.") then 
            let candidates = findCandidatesByName st identifier false
            match (fv.BlockType, candidates.Length) with
            | (FplValueType.Variable, 0)
            | (FplValueType.VariadicVariableMany, 0)
            | (FplValueType.VariadicVariableMany1, 0) -> 
                emitSIG04DiagnosticsForTypes identifier pos1 pos2
            | (FplValueType.Variable, 1)
            | (FplValueType.VariadicVariableMany, 1)
            | (FplValueType.VariadicVariableMany1, 1) -> 
                fv.ValueList.Add(candidates.Head)
            | (FplValueType.Variable, _)
            | (FplValueType.VariadicVariableMany, _)
            | (FplValueType.VariadicVariableMany1, _) -> 
                emitID017Diagnostics identifier candidates pos1 pos2
            | _ -> 
                match emitSIG04Diagnostics fv candidates with
                | Some candidate -> 
                    match fv.BlockType with
                    | FplValueType.Reference -> fv.Scope.Add(identifier, candidate)
                    | _ -> fv.ValueList.Add(candidate)
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
        fv.NameEndPos <- pos2
        st.EvalPop()
    | Ast.BracketedCoordsInType((pos1, pos2), asts) ->
        st.EvalPush("BracketedCoordsInType")
        let fv = es.PeekEvalStack()
        fv.HasBrackets <- true
        asts 
        |> List.map (fun ast1 ->
            eval st ast1
        ) |> ignore
        fv.NameEndPos <- pos2
        st.EvalPop()
    | Ast.NamespaceIdentifier((pos1, pos2), asts) ->
        st.EvalPush("NamespaceIdentifier")
        asts |> List.map (eval st) |> ignore
        st.EvalPop()
    | Ast.LocalizationTerm((pos1, pos2), asts) ->
        st.EvalPush("LocalizationTerm")
        asts |> List.map (fun ebnfTerm ->
            let trsl = FplValue.CreateFplValue((pos1, pos2), FplValueType.Translation, es.PeekEvalStack())
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
                let ref = FplValue.CreateFplValue((pos1, pos2),FplValueType.Reference,fv)
                es.PushEvalStack(ref)
                eval st pred
                es.PopEvalStack()
            ) 
        else
            let ref = FplValue.CreateFplValue((pos1, pos2),FplValueType.Reference,fv)
            ref.FplId <- "???"
            ref.TypeId <- "???"
            es.PushEvalStack(ref)
            es.PopEvalStack()
        st.EvalPop()
    | Ast.And((pos1, pos2), (predicateAst1, predicateAst2)) ->
        st.EvalPush("And")
        let fv = es.PeekEvalStack()
        fv.FplId <- "and"
        fv.ReprId <- "undetermined"
        fv.TypeId <- "pred"
        eval st predicateAst1
        eval st predicateAst2
        fv.NameEndPos <- pos2
        evaluateConjunction fv
        emitLG000orLG001Diagnostics fv "conjunction"
        st.EvalPop()
    | Ast.Or((pos1, pos2), (predicateAst1, predicateAst2)) ->
        st.EvalPush("Or")
        let fv = es.PeekEvalStack()
        fv.FplId <- "or"
        fv.ReprId <- "undetermined"
        fv.TypeId <- "pred"
        eval st predicateAst1
        eval st predicateAst2
        fv.NameEndPos <- pos2
        evaluateDisjunction fv
        emitLG000orLG001Diagnostics fv "disjunction"
        st.EvalPop()
    | Ast.Xor((pos1, pos2), (predicateAst1, predicateAst2)) ->
        st.EvalPush("Xor")
        let fv = es.PeekEvalStack()
        fv.FplId <- "xor"
        fv.ReprId <- "undetermined"
        fv.TypeId <- "pred"
        eval st predicateAst1
        eval st predicateAst2
        fv.NameEndPos <- pos2
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
                let stmt = FplValue.CreateFplValue((pos1,pos2), FplValueType.Stmt, fv)
                es.PushEvalStack(stmt)
                eval st ast
                stmtList.Add(es.Pop())
        ) |> ignore
        fv.ValueList.AddRange(stmtList)
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
        let just = FplValue.CreateFplValue((pos1, pos2), FplValueType.Justification, fv) 
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
            let ref = FplValue.CreateFplValue((pos1, pos2),FplValueType.Reference,fv)
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
            fv.NameEndPos <- pos2
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
        match optionalSpecificationAst with
        | Some specificationAst -> 
            let refBlock = FplValue.CreateFplValue((pos1, pos2), FplValueType.Reference, fv) 
            es.PushEvalStack(refBlock)
            eval st fplIdentifierAst
            eval st specificationAst |> ignore
            if System.Char.IsLower(refBlock.FplId[0]) then
                // match the signatures of small-letter entities (like the self or parent entity, or variables with arguments) 
                // with their declared types 
                let candidates = 
                    refBlock.Scope
                    |> Seq.filter (fun kvp -> kvp.Key = refBlock.FplId)
                    |> Seq.map (fun kvp -> kvp.Value)
                    |> Seq.toList
                emitSIG04Diagnostics refBlock candidates |> ignore
            else
                let candidatesFromTheory = findCandidatesByName st refBlock.FplId true
                let candidatesFromPropertyScope = findCandidatesByNameInBlock refBlock refBlock.FplId
                let candidatesFromDottedQualification = findCandidatesByNameInDotted refBlock refBlock.FplId
                let candidates = candidatesFromTheory  
                                 @ candidatesFromPropertyScope 
                                 @ candidatesFromDottedQualification
                match emitSIG04Diagnostics refBlock candidates with
                | Some matchedCandidate -> 
                    refBlock.Scope.Add(refBlock.FplId,matchedCandidate)
                | _ -> ()
            es.PopEvalStack()
        | None -> 
            // if no specification was found then simply continue in the same context
            eval st fplIdentifierAst

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
        let lang = FplValue.CreateFplValue((pos1, pos2), FplValueType.Language, fv) 
        es.PushEvalStack(lang)
        eval st langCode
        let trsl = FplValue.CreateFplValue((pos1, pos2), FplValueType.Translation, lang) 
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
    | Ast.ExtensionBlock((pos1, pos2), (extensionNameAst, extensionRegexAst)) ->
        st.EvalPush("ExtensionBlock")
        let fv = FplValue.CreateFplValue((pos1,pos2),FplValueType.Extension, es.PeekEvalStack())
        es.PushEvalStack(fv)
        eval st extensionNameAst
        eval st extensionRegexAst
        es.PopEvalStack()
        st.EvalPop()
    | Ast.Impl((pos1, pos2), (predicateAst1, predicateAst2)) ->
        st.EvalPush("Impl")
        let fv = es.PeekEvalStack()
        fv.FplId <- "impl"
        es.PeekEvalStack().ReprId <- "undetermined"
        fv.TypeId <- "pred"
        eval st predicateAst1
        eval st predicateAst2
        fv.NameEndPos <- pos2
        evaluateImplication fv
        emitLG000orLG001Diagnostics fv "implication"
        st.EvalPop()
    | Ast.Iif((pos1, pos2), (predicateAst1, predicateAst2)) ->
        st.EvalPush("Iif")
        let fv = es.PeekEvalStack()
        fv.FplId <- "iif"
        es.PeekEvalStack().ReprId <- "undetermined"
        fv.TypeId <- "pred"
        eval st predicateAst1
        eval st predicateAst2
        fv.NameEndPos <- pos2
        evaluateEquivalence fv
        emitLG000orLG001Diagnostics fv "equivalence"
        st.EvalPop()
    | Ast.IsOperator((pos1, pos2), (isOpArgAst, variableTypeAst)) ->
        st.EvalPush("IsOperator")
        let fv = es.PeekEvalStack()
        fv.FplId <- "is"
        fv.ReprId <- "undetermined"
        fv.TypeId <- "pred"
        let operand = FplValue.CreateFplValue((pos1, pos2), FplValueType.Reference, fv) 
        es.PushEvalStack(operand)
        eval st isOpArgAst
        es.PopEvalStack()
        let typeOfOperand = FplValue.CreateFplValue((pos1, pos2), FplValueType.Mapping, fv) 
        es.PushEvalStack(typeOfOperand)
        eval st variableTypeAst
        let t = typeOfOperand.Type(SignatureType.Type)
        es.PopEvalStack()
        evaluateIsOperator fv operand typeOfOperand
        st.EvalPop()
    | Ast.Delegate((pos1, pos2), (fplDelegateIdentifierAst, argumentTupleAst)) ->
        st.EvalPush("Delegate")
        let fv = es.PeekEvalStack()
        let refBlock = FplValue.CreateFplValue((pos1, pos2), FplValueType.Reference, fv) 
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
        let fv = FplValue.CreateFplValue((pos1, pos2), FplValueType.MandatoryPredicate, es.PeekEvalStack())
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
        let fv = FplValue.CreateFplValue((pos1, pos2),FplValueType.Localization,es.PeekEvalStack())
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
                    |> Seq.filter (fun kvp -> kvp.Value.BlockType = FplValueType.Language) 
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
        let fv = FplValue.CreateFplValue((pos1, pos2),FplValueType.Quantor,es.PeekEvalStack())
        fv.FplId <- "all"
        fv.TypeId <- "pred"
        es.PushEvalStack(fv)
        fv.Arity <- fv.Arity + (namedVarDeclAstList |> List.length)
        namedVarDeclAstList
        |> List.map (fun namedVarDeclAst ->
            eval st namedVarDeclAst
        )
        |> ignore
        let pred = FplValue.CreateFplValue((pos1, pos2),FplValueType.Reference,fv)
        es.PushEvalStack(pred)
        eval st predicateAst
        es.PopEvalStack()
        emitVAR05diagnostics fv
        es.PopEvalStack()
        emitLG000orLG001Diagnostics fv "all quantor"
        st.EvalPop()
    | Ast.Exists((pos1, pos2), (namedVarDeclAstList, predicateAst)) ->
        st.EvalPush("Exists")
        let fv = FplValue.CreateFplValue((pos1, pos2),FplValueType.Quantor,es.PeekEvalStack())
        fv.FplId <- "ex"
        fv.TypeId <- "pred"
        es.PushEvalStack(fv)
        fv.Arity <- fv.Arity + (namedVarDeclAstList |> List.length)
        namedVarDeclAstList
        |> List.map (fun namedVarDeclAst ->
            eval st namedVarDeclAst
        )
        |> ignore
        let pred = FplValue.CreateFplValue((pos1, pos2),FplValueType.Reference,fv)
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
        let fv = FplValue.CreateFplValue((pos1, pos2),FplValueType.Quantor, es.PeekEvalStack())
        fv.FplId <- "exn"
        fv.TypeId <- "pred"
        fv.Arity <- 1
        es.PushEvalStack(fv)
        eval st dollarDigitsAst
        eval st namedVarDeclAst
        let pred = FplValue.CreateFplValue((pos1, pos2),FplValueType.Reference,fv)
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
            if FplValue.IsFplBlock(fv) then
                fv.BlockType <- FplValueType.FunctionalTerm
            else
                fv.BlockType <- FplValueType.OptionalFunctionalTerm
                fv.TypeId <- "func"
        | None -> 
            if FplValue.IsFplBlock(fv) then
                fv.BlockType <- FplValueType.FunctionalTerm
            else
                fv.BlockType <- FplValueType.MandatoryFunctionalTerm
                fv.TypeId <- "func"
        fv.NameEndPos <- pos2
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
            let pred = FplValue.CreateFplValue((pos1,pos2),FplValueType.Reference,fv)
            es.PushEvalStack(pred)
            eval st predAst
            fv.ValueList.Add(es.Pop()) // pop the stack element (same reference as pred) and store it in a list
            // followed by the operator
            match optOperandAst with
            | Some opAst -> 
                let infixOperator = FplValue.CreateFplValue((pos1,pos2),FplValueType.Reference,fv)
                es.PushEvalStack(infixOperator)
                // evaluate the operator by trying to find a definition for the operator
                eval st opAst
                // store the index of the infix operator, so we still know it after sorting the list by precedence later
                fv.ValueList.Add(es.Pop()) // pop the stack element (same reference as infixOperator) and store it in a list
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
        while fv.ValueList.Count > 1 do
            let mutable currentMinimalPrecedence = Int32.MaxValue
            let mutable currMinIndex = 1
            for i in 1 .. 2 .. fv.ValueList.Count - 1 do
                let currPrecedence = getPrecedence fv.ValueList[i]
                if currentMinimalPrecedence > currPrecedence then
                    currentMinimalPrecedence <- currPrecedence
                    currMinIndex <- i
            let currentOp = fv.ValueList[currMinIndex]
            let firstOp = fv.ValueList[currMinIndex-1]
            let secondOp = fv.ValueList[currMinIndex+1]
            currentOp.ValueList.Add(firstOp)
            currentOp.ValueList.Add(secondOp)
            match precNodeList currentOp with
            | x::xs -> 
                match emitSIG04Diagnostics currentOp [x] with 
                | Some candidate -> 
                    run.Run currentOp // execute the matched binary operator
                | _ -> ()
            | _ -> ()
            fv.ValueList.RemoveAt(currMinIndex+1) 
            fv.ValueList.RemoveAt(currMinIndex-1) 
        simplifyTriviallyNestedExpressions fv
        let last = es.PeekEvalStack()
        run.Run last // execute the last matched binary operator
        st.EvalPop()
    // | Expression of Positions * ((((Ast option * Ast) * Ast option) * Ast option) * Ast)
    | Ast.Expression((pos1, pos2), ((((prefixOpAst, predicateAst), postfixOpAst), optionalSpecificationAst), qualificationListAst)) ->
        st.EvalPush("Expression")
        let fv = es.PeekEvalStack()
        let refBlock = FplValue.CreateFplValue((pos1, pos2), FplValueType.Reference, fv) 
        es.PushEvalStack(refBlock)
        let ensureReversedPolishNotation = 
            if prefixOpAst.IsSome && postfixOpAst.IsSome then 
                // for heuristic reasons, we choose a precedence of postfix ...
                postfixOpAst |> Option.map (eval st) |> Option.defaultValue () 
                let postfixedInnerPred = FplValue.CreateFplValue((pos1,pos2),FplValueType.Reference,es.PeekEvalStack())
                es.PushEvalStack(postfixedInnerPred)
                // ... over prefix notation in mathematics
                prefixOpAst |> Option.map (eval st) |> Option.defaultValue ()
                let prefixedInnerPred = FplValue.CreateFplValue((pos1,pos2),FplValueType.Reference,es.PeekEvalStack())
                es.PushEvalStack(prefixedInnerPred)
                eval st predicateAst
                es.PopEvalStack()
                es.PopEvalStack()
            elif prefixOpAst.IsSome then 
                prefixOpAst |> Option.map (eval st) |> Option.defaultValue ()
                let innerPred = FplValue.CreateFplValue((pos1,pos2),FplValueType.Reference,es.PeekEvalStack())
                es.PushEvalStack(innerPred)
                eval st predicateAst
                es.PopEvalStack()
            elif postfixOpAst.IsSome then 
                postfixOpAst |> Option.map (eval st) |> Option.defaultValue ()
                let innerPred = FplValue.CreateFplValue((pos1,pos2),FplValueType.Reference,es.PeekEvalStack())
                es.PushEvalStack(innerPred)
                eval st predicateAst
                es.PopEvalStack()
            else
                eval st predicateAst
        ensureReversedPolishNotation
        optionalSpecificationAst |> Option.map (eval st) |> Option.defaultValue ()
        eval st qualificationListAst
        let refBlock = es.PeekEvalStack() // if the reference was replaced, take this one
        refBlock.NameEndPos <- pos2
        simplifyTriviallyNestedExpressions refBlock
        let last = es.PeekEvalStack()
        es.PopEvalStack()
        match fv.BlockType with
        | FplValueType.Axiom 
        | FplValueType.Corollary 
        | FplValueType.Proposition 
        | FplValueType.Theorem 
        | FplValueType.Lemma
        | FplValueType.Conjecture 
        | FplValueType.Predicate 
        | FplValueType.MandatoryPredicate 
        | FplValueType.OptionalPredicate ->
            fv.ReprId <- last.ReprId
        | FplValueType.Reference ->
            // simplify references created due to superfluous parentheses of expressions
            // by replacing them with their only value
            if prefixOpAst.IsNone && 
                postfixOpAst.IsNone &&
                fv.FplId = "" && 
                fv.ValueList.Count = 1 then
                    let subNode = fv.ValueList[0]
                    if subNode.BlockType = FplValueType.Reference then 
                        es.Pop() |> ignore
                        es.PushEvalStack(subNode)
                        subNode.Parent <- fv.Parent
                        fv.ValueList.Clear()
        | _ -> ()
        st.EvalPop()
    // | Cases of Positions * (Ast list * Ast)
    | Ast.Cases((pos1, pos2), (conditionFollowedByResultListAsts, elseStatementAst)) ->
        st.EvalPush("Cases")
        let fv = es.PeekEvalStack()
        fv.NameStartPos <- pos1
        fv.NameEndPos <- pos2
        fv.FplId <- "cases"
        conditionFollowedByResultListAsts 
        |> List.map (fun caseAst ->
            let cas = FplValue.CreateFplValue((pos1,pos2), FplValueType.Stmt, fv)
            cas.FplId <- "case"
            es.PushEvalStack(cas)
            eval st caseAst
            es.PopEvalStack()
        ) |> ignore
        let cas = FplValue.CreateFplValue((pos1,pos2), FplValueType.Stmt, fv)
        cas.FplId <- "else"
        es.PushEvalStack(cas)
        eval st elseStatementAst
        es.PopEvalStack()
        st.EvalPop()
    // | Assignment of Positions * (Ast * Ast)
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
        fv.NameStartPos <- pos1
        fv.NameEndPos <- pos2
        fv.FplId <- "assign"
        let assignee = FplValue.CreateFplValue((pos1,pos2),FplValueType.Reference,es.PeekEvalStack())
        es.PushEvalStack(assignee)
        eval st predicateWithQualificationAst
        es.PopEvalStack() 
        let assignedValue = FplValue.CreateFplValue((pos1,pos2),FplValueType.Reference,es.PeekEvalStack())
        es.PushEvalStack(assignedValue)
        eval st predicateAst
        es.PopEvalStack() 
        // todo assign value to the assignee by checking the type consistency first and finding an appropriate way to embed the result in the symbol table.
        st.EvalPop()
    | Ast.PredicateInstance((pos1, pos2), ((optAst, signatureAst), predInstanceBlockAst)) ->
        st.EvalPush("PredicateInstance")
        eval st signatureAst
        let fv = es.PeekEvalStack()
        es.PeekEvalStack().ReprId <- "undetermined"
        match optAst with
        | Some ast1 -> 
            eval st ast1
            fv.BlockType <- FplValueType.OptionalPredicate
        | None -> 
            fv.BlockType <- FplValueType.MandatoryPredicate
        eval st predInstanceBlockAst
        st.EvalPop()
    | Ast.ParentConstructorCall((pos1, pos2), (inheritedClassTypeAst, argumentTupleAst)) ->
        st.EvalPush("ParentConstructorCall")
        let fv = es.PeekEvalStack()
        fv.NameStartPos <- pos1
        fv.NameEndPos <- pos2
        fv.FplId <- "bas"
        let refBlock = FplValue.CreateFplValue((pos1, pos2), FplValueType.Reference, fv) 
        es.PushEvalStack(refBlock)
        eval st inheritedClassTypeAst
        eval st argumentTupleAst
        es.PopEvalStack()
        st.EvalPop()
    | Ast.JustArgInf((pos1, pos2), (justificationAst, argumentInferenceAst)) ->
        st.EvalPush("JustArgInf")
        eval st justificationAst
        eval st argumentInferenceAst
        st.EvalPop()
    | Ast.Argument((pos1, pos2), (argIdAst, argAst)) ->
        st.EvalPush("Argument")
        let fv = es.PeekEvalStack()
        let arg = FplValue.CreateFplValue((pos1, pos2), FplValueType.Argument, fv) 
        es.PushEvalStack(arg)
        eval st argIdAst
        eval st argAst
        es.PopEvalStack()
        st.EvalPop()
    // | ForIn of Positions * ((Ast * Ast) * Ast list)
    | Ast.ForIn((pos1, pos2), ((entityAst, inDomainAst), statementListAst)) ->
        st.EvalPush("ForIn")
        let fv = es.PeekEvalStack()
        fv.NameStartPos <- pos1
        fv.NameEndPos <- pos2
        fv.FplId <- "for"
        let entity = FplValue.CreateFplValue((pos1,pos2), FplValueType.Reference, fv)
        es.PushEvalStack(entity)
        eval st entityAst
        es.PopEvalStack()
        let inDomain = FplValue.CreateFplValue((pos1,pos2), FplValueType.Reference, fv)
        es.PushEvalStack(inDomain)
        eval st inDomainAst
        es.PopEvalStack()
        statementListAst 
        |> List.map (fun stmtAst ->
            let stmt = FplValue.CreateFplValue((pos1,pos2), FplValueType.Stmt, fv)
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
        let fv = FplValue.CreateFplValue((pos1, pos2), FplValueType.Theorem, es.PeekEvalStack())
        es.PushEvalStack(fv)
        eval st signatureAst
        evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst
        emitVAR04diagnostics fv
        es.PopEvalStack()
        st.EvalPop()
    | Ast.Lemma((pos1, pos2), (signatureAst, (optVarDeclOrSpecList, predicateAst))) ->
        st.EvalPush("Lemma")
        let fv = FplValue.CreateFplValue((pos1, pos2), FplValueType.Lemma, es.PeekEvalStack())
        es.PushEvalStack(fv)
        eval st signatureAst
        evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst
        emitVAR04diagnostics fv
        es.PopEvalStack()
        st.EvalPop()
    | Ast.Proposition((pos1, pos2), (signatureAst, (optVarDeclOrSpecList, predicateAst))) ->
        st.EvalPush("Proposition")
        let fv = FplValue.CreateFplValue((pos1, pos2), FplValueType.Proposition, es.PeekEvalStack())
        es.PushEvalStack(fv)
        eval st signatureAst
        evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst
        emitVAR04diagnostics fv
        es.PopEvalStack()
        st.EvalPop()
    | Ast.Conjecture((pos1, pos2), (signatureAst, (optVarDeclOrSpecList, predicateAst))) ->
        st.EvalPush("Conjecture")
        let fv = FplValue.CreateFplValue((pos1, pos2), FplValueType.Conjecture, es.PeekEvalStack())
        es.PushEvalStack(fv)
        eval st signatureAst
        evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst
        emitVAR04diagnostics fv
        es.PopEvalStack()
        st.EvalPop()
    | Ast.Axiom((pos1, pos2), (signatureAst, (optVarDeclOrSpecList, predicateAst))) ->
        st.EvalPush("Axiom")
        let fv = FplValue.CreateFplValue((pos1, pos2), FplValueType.Axiom, es.PeekEvalStack())
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
        let fv = FplValue.CreateFplValue((pos1, pos2), FplValueType.Corollary, es.PeekEvalStack())
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
        let fv = FplValue.CreateFplValue((pos1, pos2), FplValueType.Constructor, es.PeekEvalStack())
        es.PushEvalStack(fv)
        eval st signatureAst
        match optVarDeclOrSpecListAst with
        | Some astList -> astList |> List.map (eval st) |> ignore
        | None -> ()
        let rb = FplValue.CreateFplValue((pos1, pos2), FplValueType.Reference, fv)
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
        let fv = FplValue.CreateFplValue((pos1, pos2), FplValueType.Predicate, fplTheory)
        es.PushEvalStack(fv)
        es.InSignatureEvaluation <- true
        eval st signatureWithUserDefinedStringAst
        es.InSignatureEvaluation <- false
        eval st predicateContentAst
        optPropertyListAsts |> Option.map (List.map (eval st) >> ignore) |> Option.defaultValue ()
        if not fv.IsIntrinsic then // if not intrinsic, check variable usage
            emitVAR04diagnostics fv
        es.PopEvalStack()
        st.EvalPop()
    // | DefinitionFunctionalTerm of Positions * ((Ast * Ast) * (Ast * Ast list option))
    | Ast.DefinitionFunctionalTerm((pos1, pos2), (functionalTermSignatureAst, (funcContentAst, optPropertyListAsts))) ->
        st.EvalPush("DefinitionFunctionalTerm")
        let fv = FplValue.CreateFplValue((pos1, pos2), FplValueType.FunctionalTerm, es.PeekEvalStack())
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
        let fv = FplValue.CreateFplValue((pos1, pos2), FplValueType.Class, es.PeekEvalStack())
        es.PushEvalStack(fv)
        es.InSignatureEvaluation <- true
        eval st predicateIdentifierAst
        es.InSignatureEvaluation <- false
        optUserDefinedObjSymAst |> Option.map (eval st) |> Option.defaultValue ()
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
        let argInf = FplValue.CreateFplValue((pos1, pos2), FplValueType.ArgInference, fv) 
        argInf.FplId <- "derive"
        es.PushEvalStack(argInf)
        eval st predicateAst
        es.PopEvalStack()
        st.EvalPop()
    // | Proof of Positions * (Ast * (Ast list * Ast option))
    | Ast.Proof((pos1, pos2), (referencingIdentifierAst, (proofArgumentListAst, optQedAst))) ->
        st.EvalPush("Proof")
        let fv = FplValue.CreateFplValue((pos1, pos2), FplValueType.Proof, es.PeekEvalStack())
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
            let theoryValue = FplValue.CreateTheory((Position("",0,1,1), Position("",0,1,1)), st.Root, pa.Parsing.Uri.AbsolutePath);
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
