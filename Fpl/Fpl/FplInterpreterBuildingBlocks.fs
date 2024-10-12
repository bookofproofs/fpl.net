﻿module FplInterpreterBuildingBlocks

open System
open System.Linq
open System.Collections.Generic
open FParsec
open ErrDiagnostics
open FplGrammarTypes
open FplInterpreterTypes
open FplInterpreterDiagnosticsEmitter
open FplInterpreterPredicateEvaluator


type EvalStack() = 
    let _valueStack = Stack<FplValue>()

    /// Adjusts the signature and name of an FplValue.
    static member adjustSignatureAndName (fplValue:FplValue) name (typeSignature:string list) = 
            fplValue.Name <- addWithComma fplValue.Name name
            fplValue.TypeSignature <- fplValue.TypeSignature @ typeSignature

    /// Adds the FplValue to it's parent's Scope.
    static member tryAddToScope fv = 
            match FplValue.InScopeOfParent(fv) fv.Name with
            | ScopeSearchResult.Found conflict -> 
                emitID001diagnostics fv conflict 
            | _ -> 
                fv.Parent.Value.Scope.Add(fv.Name,fv)

    /// adds the FplValue to it's parent's ValueList
    static member tryAddToValueList fv = 
            match FplValue.InScopeOfParent(fv) fv.Name with
            | ScopeSearchResult.Found conflict -> 
                emitID001diagnostics fv conflict 
            | _ -> 
                fv.Parent.Value.Scope.Add(fv.Name,fv)

    // Pops an FplValue from stack and propagates it's name and signature to the next FplValue on the stack.
    member this.PopEvalStack() = 
        let fv = _valueStack.Pop()
        let next = _valueStack.Peek()

        match fv.BlockType with
        | FplValueType.Proof -> 
            match FplValue.TryFindAssociatedBlockForProof fv with
            | ScopeSearchResult.FoundAssociate parentsName -> 
                // everything is ok, change the parent of the provable from theory to the found parent 
                fv.Parent <- Some fv.Parent.Value.Scope[parentsName]
            | ScopeSearchResult.FoundIncorrectBlock block ->
                emitID002diagnostics fv block  
            | ScopeSearchResult.NotFound ->
                emitID003diagnostics fv  
            | ScopeSearchResult.FoundMultiple listOfKandidates ->
                emitID004diagnostics fv listOfKandidates  
            | _ -> ()
            EvalStack.tryAddToScope fv
        | FplValueType.Corollary ->
            match FplValue.TryFindAssociatedBlockForCorollary fv with
            | ScopeSearchResult.FoundAssociate parentsName -> 
                // everything is ok, change the parent of the provable from theory to the found parent 
                fv.Parent <- Some fv.Parent.Value.Scope[parentsName]
                // now, we are ready to emit VAR03 diagnostics for all variables declared in the signature of the corollary.
                emitVAR03diagnosticsForCorollarysSignatureVariable fv  
            | ScopeSearchResult.FoundIncorrectBlock block ->
                emitID005diagnostics fv block  
            | ScopeSearchResult.NotFound ->
                emitID006diagnostics fv  
            | ScopeSearchResult.FoundMultiple listOfKandidates ->
                emitID007diagnostics fv listOfKandidates  
            | _ -> ()
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
        | FplValueType.FunctionalTerm ->
            EvalStack.tryAddToScope fv
        | FplValueType.Reference ->
            if fv.AuxiliaryInfo = 0 then
                // propagate references only if refblock has all opened brackets closed
                // and the name of its reference-typed parent is not yet ready
                if not (next.ValueList.Contains(fv)) then 
                    next.ValueList.Add(fv)
                    match fv.FplRepresentation with
                    | FplRepresentation.Pointer variable ->
                        EvalStack.adjustSignatureAndName next variable.Name variable.TypeSignature
                    | _ -> 
                        EvalStack.adjustSignatureAndName next fv.Name fv.TypeSignature
        | FplValueType.Variable
        | FplValueType.VariadicVariableMany
        | FplValueType.VariadicVariableMany1
        | FplValueType.Object
        | FplValueType.Premise
        | FplValueType.Conclusion
        | FplValueType.Theory
        | FplValueType.Translation
        | FplValueType.Root -> 
            EvalStack.tryAddToValueList fv

    // Pushes an FplValue to the stack.
    member this.PushEvalStack fv = _valueStack.Push fv

    // Peeks an FplValue from the stack.
    member this.PeekEvalStack() = _valueStack.Peek()

    // Clears stack.
    member this.ClearEvalStack() = _valueStack.Clear()

let es = EvalStack()

let adjustSignature (fplValue:FplValue) name = 
    
    if name <> "" then
        fplValue.Name <- addWithComma fplValue.Name name
        // note: the manipulation of the TypeSignature is necessary for all kinds of fplValue
        if name.StartsWith("*") && name <> "*" then
            fplValue.TypeSignature <- fplValue.TypeSignature @ ["*"; name.Substring(1)]
        elif name.StartsWith("+") && name <> "+" then
            fplValue.TypeSignature <- fplValue.TypeSignature @ ["+"; name.Substring(1)]
        elif name.StartsWith("$") && name <> "$" then
            fplValue.TypeSignature <- fplValue.TypeSignature @ ["ind"]
        elif name = "true" || name = "false" then
            fplValue.TypeSignature <- fplValue.TypeSignature @ ["pred"]
        else
            fplValue.TypeSignature <- fplValue.TypeSignature @ [name]

    if name ="(" || name = "[" then 
        fplValue.AuxiliaryInfo <- fplValue.AuxiliaryInfo + 1
    elif name =")" || name = "]" then 
        fplValue.AuxiliaryInfo <- fplValue.AuxiliaryInfo - 1

let setRepresentation (st: SymbolTable) representation = 
    let fv = es.PeekEvalStack()
    fv.FplRepresentation <- representation

let eval_units (st: SymbolTable) unitType pos1 pos2 = 
    if unitType <> "" then 
        let fv = es.PeekEvalStack()
        if FplValue.IsClass(fv) then
            ()
        elif FplValue.HasSignature(fv) then
            if (FplValue.IsVariadicVariableMany(fv)) then 
                adjustSignature fv $"*{unitType}"
            elif (FplValue.IsVariadicVariableMany1(fv)) then 
                adjustSignature fv $"+{unitType}"
            else
                adjustSignature fv unitType
                checkID009_ID010_ID011_Diagnostics st fv unitType pos1 pos2
        elif (FplValue.IsVariadicVariableMany(fv)) then 
            adjustSignature fv $"*{unitType}"
        elif (FplValue.IsVariadicVariableMany1(fv)) then 
            adjustSignature fv $"+{unitType}"
        elif (FplValue.IsReference(fv)) then 
            checkID012Diagnostics st fv unitType pos1 pos2
        else
            adjustSignature fv unitType
            checkID009_ID010_ID011_Diagnostics st fv unitType pos1 pos2


let eval_string (st: SymbolTable) s = ()

let eval_pos_string (st: SymbolTable) (startpos: Position) (endpos: Position) ast = ()

let eval_pos_unit (st: SymbolTable) (startpos: Position) (endpos: Position) = ()

let eval_pos_ast (st: SymbolTable) (startpos: Position) (endpos: Position) = ()

let eval_pos_ast_ast_opt (st: SymbolTable) (startpos: Position) (endpos: Position) = ()

let eval_pos_char_list (st: SymbolTable) (startpos: Position) (endpos: Position) charlist =
    charlist |> List.map string |> String.concat "" |> ignore

let eval_pos_string_ast (st: SymbolTable) str = ()

/// A recursive function evaluating an AST and returning a list of EvalAliasedNamespaceIdentifier records
/// for each occurrence of the uses clause in the FPL code.
let rec eval (st: SymbolTable) ast =
    let evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst =
        match optVarDeclOrSpecList with
        | Some astList -> astList |> List.map (eval st) |> ignore
        | None -> ()
        eval st predicateAst

    let evalMany (st:SymbolTable) blockType pos1 pos2 = 
        let fv = es.PeekEvalStack()
        checkVAR00Diagnostics fv.AuxiliaryInfo pos1 pos2
        fv.BlockType <- blockType

    match ast with
    // units: | Star
    | Ast.IndexType((pos1, pos2),()) -> 
        st.EvalPush("IndexType")
        eval_units st "ind" pos1 pos2 
        setRepresentation st (FplRepresentation.Index ((uint)0))
        st.EvalPop() |> ignore
    | Ast.ObjectType((pos1, pos2),()) -> 
        st.EvalPush("ObjectType")
        eval_units st "obj" pos1 pos2 
        setRepresentation st (FplRepresentation.ObjRepr "obj")
        st.EvalPop()
    | Ast.PredicateType((pos1, pos2),()) -> 
        st.EvalPush("PredicateType")
        eval_units st "pred" pos1 pos2 
        setRepresentation st (FplRepresentation.PredRepr FplPredicate.Undetermined)
        st.EvalPop()
    | Ast.FunctionalTermType((pos1, pos2),()) -> 
        st.EvalPush("FunctionalTermType")
        eval_units st "func" pos1 pos2  
        setRepresentation st (FplRepresentation.LangRepr FplLanguageConstruct.Function)
        st.EvalPop()
    | Ast.Many((pos1, pos2),()) ->
        st.EvalPush("Many")
        evalMany st FplValueType.VariadicVariableMany pos1 pos2
        st.EvalPop()
    | Ast.Many1((pos1, pos2),()) ->
        st.EvalPush("Many1")
        evalMany st FplValueType.VariadicVariableMany1 pos1 pos2
        st.EvalPop()
    | Ast.One((pos1, pos2),()) ->
        st.EvalPush("One")
        eval_units st "" pos1 pos2  
        st.EvalPop()
    | Ast.Star((pos1, pos2),()) ->
        st.EvalPush("Star")
        eval_units st "" pos1 pos2  
        st.EvalPop()
    | Ast.Dot((pos1, pos2),()) ->
        st.EvalPush("Dot")
        eval_units st "" pos1 pos2  
        st.EvalPop()
    | Ast.Intrinsic((pos1, pos2),()) -> 
        st.EvalPush("Intrinsic")
        eval_units st "" pos1 pos2  
        st.EvalPop()
    | Ast.Property((pos1, pos2),()) -> 
        st.EvalPush("Property")
        eval_units st "" pos1 pos2  
        st.EvalPop()
    | Ast.Optional((pos1, pos2),()) -> 
        st.EvalPush("Optional")
        eval_units st "" pos1 pos2  
        st.EvalPop()
    | Ast.Error  ->   
        st.EvalPush("Error")
        let pos = Position("",0,1,1)
        eval_units st "" pos pos
        st.EvalPop()
    // strings: | Digits of string
    | Ast.Digits s -> 
        st.EvalPush("Digits")
        let fv = es.PeekEvalStack()
        adjustSignature fv s
        st.EvalPop()
    | Ast.PascalCaseId s -> 
        st.EvalPush("PascalCaseId")
        eval_string st s
        st.EvalPop() 
    | Ast.ExtensionRegex s -> 
        st.EvalPush("ExtensionRegex")
        eval_string st s
        st.EvalPop() 
    // | DollarDigits of Positions * int
    | Ast.DollarDigits((pos1, pos2), s) -> 
        st.EvalPush("DollarDigits")
        let fv = es.PeekEvalStack()
        adjustSignature fv ("$"+s.ToString())
        fv.NameEndPos <- pos2
        st.EvalPop() 
    | Ast.Extensionname((pos1, pos2), s) ->
        st.EvalPush("Extensionname")
        let fv = es.PeekEvalStack()
        if (FplValue.IsVariadicVariableMany(fv)) then 
            adjustSignature fv ("*@" + s)
        elif (FplValue.IsVariadicVariableMany1(fv)) then 
            adjustSignature fv ("+@" + s)
        else
            adjustSignature fv ("@" + s)
        st.EvalPop() 
    | Ast.TemplateType((pos1, pos2), s) -> 
        st.EvalPush("TemplateType")
        let fv = es.PeekEvalStack()
        if (FplValue.IsVariadicVariableMany(fv)) then 
            adjustSignature fv ("*" + s)
        elif (FplValue.IsVariadicVariableMany1(fv)) then 
            adjustSignature fv ("+" + s)
        else
            adjustSignature fv s
        st.EvalPop() 
    | Ast.Var((pos1, pos2), name) ->
        st.EvalPush("Var")
        let fv = es.PeekEvalStack()
        let varValue = FplValue.CreateFplValue((pos1,pos2), FplValueType.Variable, fv)
        EvalStack.adjustSignatureAndName varValue name ["undef"] 
        es.PushEvalStack(varValue)
        if FplValue.IsReference(fv) then
            match FplValue.VariableInBlockScopeByName fv name with 
            | ScopeSearchResult.Found variableInScope ->
                // replace the reference by a pointer to an existing declared variable
                fv.FplRepresentation <- FplRepresentation.Pointer variableInScope
            | _ -> 
                emitVAR01diagnostics name pos1 pos2
        else
            match FplValue.VariableInBlockScopeByName(fv) name with
            | ScopeSearchResult.Found other ->
                // if found, the emit error that the variable was already declared.
                emitVAR03diagnostics fv other 
            | _ -> ()
        let evalPath = st.EvalPath()
        if not (evalPath.Contains("NamedVarDecl.")) then 
            es.PopEvalStack() // postpone popping all variables from stack that are being declared (they will be removed in Ast.NamedVarDecl(..))
        st.EvalPop() 
    | Ast.DelegateId((pos1, pos2), s) -> 
        st.EvalPush("DelegateId")
        let fv = es.PeekEvalStack()
        adjustSignature fv s
        st.EvalPop() 
    | Ast.Alias((pos1, pos2), s) -> 
        st.EvalPush("Alias")
        eval_pos_string st pos1 pos2 s
        st.EvalPop() 
    | Ast.LocalizationString((pos1, pos2), s) -> 
        st.EvalPush("LocalizationString")
        eval_pos_string st pos1 pos2 s
        st.EvalPop() 
    | Ast.ObjectSymbol((pos1, pos2), s) -> 
        st.EvalPush("ObjectSymbol")
        let fv = es.PeekEvalStack()
        adjustSignature fv s
        st.EvalPop() 
    | Ast.ArgumentIdentifier((pos1, pos2), s) -> 
        st.EvalPush("ArgumentIdentifier")
        let fv = es.PeekEvalStack()
        adjustSignature fv s
        emitPR000Diagnostics fv s pos1 pos2
        st.EvalPop() 
    | Ast.Prefix((pos1, pos2), symbol) -> 
        st.EvalPush("Prefix")
        let fv = es.PeekEvalStack()
        fv.ExpressionType <- FixType.Prefix symbol
        st.EvalPop() 
    | Ast.Infix((pos1, pos2), (symbol, precedenceAsts)) -> 
        st.EvalPush("Infix")
        let fv = es.PeekEvalStack()
        fv.ExpressionType <- FixType.Infix (symbol, fv.AuxiliaryInfo)
        emitSIG02Diagnostics st fv pos1 pos2 
        eval st precedenceAsts
        st.EvalPop() 
    | Ast.Postfix((pos1, pos2), symbol) -> 
        st.EvalPush("Postfix")
        let fv = es.PeekEvalStack()
        fv.ExpressionType <- FixType.Postfix symbol
        st.EvalPop() 
    | Ast.Symbol((pos1, pos2), s) -> 
        st.EvalPush("Symbol")
        eval_pos_string st pos1 pos2 s
        st.EvalPop() 
    | Ast.InfixOperator((pos1, pos2), symbol) -> 
        st.EvalPush("InfixOperator")
        let fv = es.PeekEvalStack()
        adjustSignature fv symbol
        emitSIG01Diagnostics st fv pos1 pos2 
        st.EvalPop() 
    | Ast.PostfixOperator((pos1, pos2), symbol) -> 
        st.EvalPush("PostfixOperator")
        let fv = es.PeekEvalStack()
        adjustSignature fv symbol
        emitSIG01Diagnostics st fv pos1 pos2 
        st.EvalPop() 
    | Ast.PrefixOperator((pos1, pos2), symbol) -> 
        st.EvalPush("PrefixOperator")
        let fv = es.PeekEvalStack()
        adjustSignature fv symbol
        emitSIG01Diagnostics st fv pos1 pos2 
        st.EvalPop() 
    // | Self of Positions * unit
    | Ast.Self((pos1, pos2), _) -> 
        st.EvalPush("Self")
        eval_pos_unit st pos1 pos2
        st.EvalPop() 
    | Ast.True((pos1, pos2), _) -> 
        st.EvalPush("True")
        let fv = es.PeekEvalStack()
        if FplValue.IsReference(fv) then
            fv.FplRepresentation <- FplRepresentation.PredRepr FplPredicate.True
        adjustSignature fv "true"
        st.EvalPop() 
    | Ast.False((pos1, pos2), _) -> 
        st.EvalPush("False")
        let fv = es.PeekEvalStack()
        if FplValue.IsReference(fv) then
            fv.FplRepresentation <- FplRepresentation.PredRepr FplPredicate.False
        adjustSignature fv "false"
        st.EvalPop() 
    | Ast.Undefined((pos1, pos2), _) -> 
        st.EvalPush("Undefined")
        let fv = es.PeekEvalStack()
        adjustSignature fv "undef"
        st.EvalPop() 
    | Ast.Trivial((pos1, pos2), _) -> 
        st.EvalPush("Trivial")
        eval_pos_unit st pos1 pos2
        st.EvalPop() 
    | Ast.Qed((pos1, pos2), _) -> 
        st.EvalPush("Qed")
        eval_pos_unit st pos1 pos2
        st.EvalPop() 
    // | ExtDigits of Positions * Ast
    | Ast.RuleOfInference((pos1, pos2), (signatureAst, premiseConclusionBlockAst)) ->
        st.EvalPush("RuleOfInference")
        let fplValue = FplValue.CreateFplValue((pos1, pos2), FplValueType.RuleOfInference, es.PeekEvalStack())
        es.PushEvalStack(fplValue)
        eval st signatureAst
        eval st premiseConclusionBlockAst
        es.PopEvalStack() 
        st.EvalPop() 
    | Ast.ClassIdentifier((pos1, pos2), ast1) ->
        st.EvalPush("ClassIdentifier")
        eval st ast1
        let fv = es.PeekEvalStack()
        fv.NameEndPos <- pos2
        eval_pos_ast st pos1 pos2
        st.EvalPop()
    | Ast.ExtDigits((pos1, pos2), ast1) ->
        st.EvalPush("ExtDigits")
        eval st ast1
        eval_pos_ast st pos1 pos2
        st.EvalPop()
    | Ast.ExtensionType((pos1, pos2), ast1) ->
        st.EvalPush("ExtensionType")
        eval st ast1
        eval_pos_ast st pos1 pos2
        st.EvalPop()
    | Ast.UsesClause((pos1, pos2), ast1) ->
        st.EvalPush("UsesClause")
        eval st ast1
        eval_pos_ast st pos1 pos2
        st.EvalPop()
    | Ast.Not((pos1, pos2), predicateAst) ->
        st.EvalPush("Not")
        let fv = es.PeekEvalStack()
        adjustSignature fv "not"
        adjustSignature fv "("
        eval st predicateAst
        adjustSignature fv ")"
        fv.NameEndPos <- pos2
        evaluateNegation fv
        emitLG000orLG001Diagnostics fv "negation"
        st.EvalPop()
    | Ast.InEntity((pos1, pos2), ast1) ->
        st.EvalPush("InEntity")
        eval st ast1
        eval_pos_ast st pos1 pos2
        st.EvalPop()
    | Ast.IsType((pos1, pos2), ast1) ->
        st.EvalPush("IsType")
        eval st ast1
        eval_pos_ast st pos1 pos2
        st.EvalPop()
    | Ast.Assertion((pos1, pos2), ast1) ->
        st.EvalPush("Assertion")
        eval st ast1
        eval_pos_ast st pos1 pos2
        st.EvalPop()
    | Ast.ByDef((pos1, pos2), predicateWithQualificationAst) ->
        st.EvalPush("ByDef")
        let fv = es.PeekEvalStack()
        fv.FplRepresentation <- FplRepresentation.PredRepr FplPredicate.Undetermined
        adjustSignature fv "bydef."
        eval st predicateWithQualificationAst
        emitPR001Diagnostics fv pos1 pos2
        st.EvalPop()
    | Ast.DottedPredicate((pos1, pos2), predicateWithOptSpecificationAst) ->
        st.EvalPush("DottedPredicate")
        let fv = es.PeekEvalStack()
        fv.Name <- fv.Name + "."
        fv.TypeSignature <- fv.TypeSignature @ ["."]
        let refBlock = FplValue.CreateFplValue((pos1, pos2), FplValueType.Reference, fv) 
        es.PushEvalStack(refBlock)
        eval st predicateWithOptSpecificationAst
        es.PopEvalStack()
        st.EvalPop()
    | Ast.Return((pos1, pos2), ast1) ->
        st.EvalPush("Return")
        eval st ast1
        eval_pos_ast st pos1 pos2
        st.EvalPop()
    | Ast.AssumeArgument((pos1, pos2), ast1) ->
        st.EvalPush("AssumeArgument")
        eval st ast1
        eval_pos_ast st pos1 pos2
        st.EvalPop()
    | Ast.RevokeArgument((pos1, pos2), ast1) ->
        st.EvalPush("RevokeArgument")
        eval st ast1
        eval_pos_ast st pos1 pos2
        st.EvalPop()
    | Ast.VariableType((pos1, pos2), compoundVariableTypeAst) ->
        st.EvalPush("VariableType")
        eval st compoundVariableTypeAst
        eval_pos_ast st pos1 pos2
        st.EvalPop()
    | Ast.AST((pos1, pos2), ast1) ->
        st.EvalPush("AST")
        eval st ast1
        eval_pos_ast st pos1 pos2
        st.EvalPop()
    // | NamespaceIdentifier of Positions * Ast list
    | Ast.PredicateIdentifier((pos1, pos2), asts) ->
        st.EvalPush("PredicateIdentifier")

        let pascalCaseIdList = asts |> List.collect (function Ast.PascalCaseId s -> [s] | _ -> [])
        let identifier = String.concat "." pascalCaseIdList
        
        let fv = es.PeekEvalStack()
        if FplValue.HasSignature(fv) then
            if (FplValue.IsVariadicVariableMany(fv)) then 
                adjustSignature fv ("*" + identifier)
            elif (FplValue.IsVariadicVariableMany1(fv)) then 
                adjustSignature fv ("+" + identifier)
            else
                adjustSignature fv identifier
            checkID008Diagnostics fv pos1 pos2
            checkID009_ID010_ID011_Diagnostics st fv identifier pos1 pos2
            emitSIG04TypeDiagnostics st identifier fv pos1 pos2
        elif FplValue.IsVariable(fv) then
            if (FplValue.IsVariadicVariableMany(fv)) then 
                adjustSignature fv ("*" + identifier)
            elif (FplValue.IsVariadicVariableMany1(fv)) then 
                adjustSignature fv ("+" + identifier)
            else
                adjustSignature fv identifier
            emitSIG04TypeDiagnostics st identifier fv pos1 pos2 
        elif FplValue.IsReference(fv) then
            adjustSignature fv identifier
            checkID012Diagnostics st fv identifier pos1 pos2
            checkID012Diagnostics st fv identifier pos1 pos2
            emitSIG04TypeDiagnostics st identifier fv pos1 pos2

        st.EvalPop()
    | Ast.ParamTuple((pos1, pos2), namedVariableDeclarationListAsts) ->
        st.EvalPush("ParamTuple")
        let fv = es.PeekEvalStack()
        adjustSignature fv "("
        namedVariableDeclarationListAsts |> List.map (
            fun child ->
            match child with 
            | Ast.NamedVarDecl(_,((varList,_),_)) -> fv.Arity <- fv.Arity + varList.Length
            | _ -> ()
            eval st child
        ) |> ignore
        adjustSignature fv ")"
        fv.NameEndPos <- pos2
        st.EvalPop()
    | Ast.BracketedCoordsInType((pos1, pos2), asts) ->
        st.EvalPush("BracketedCoordsInType")
        let fv = es.PeekEvalStack()
        adjustSignature fv "["
        asts 
        |> List.map (fun ast1 ->
            eval st ast1
        ) |> ignore
        adjustSignature fv "]"
        fv.NameEndPos <- pos2
        st.EvalPop()
    | Ast.NamespaceIdentifier((pos1, pos2), asts) ->
        st.EvalPush("NamespaceIdentifier")
        asts |> List.map (eval st) |> ignore
        st.EvalPop()
    | Ast.LocalizationTerm((pos1, pos2), asts) ->
        st.EvalPush("LocalizationTerm")
        asts |> List.map (eval st) |> ignore
        st.EvalPop()
    | Ast.LocalizationTermList((pos1, pos2), asts) ->
        st.EvalPush("LocalizationTermList")
        asts |> List.map (eval st) |> ignore
        st.EvalPop()
    | Ast.BrackedCoordList((pos1, pos2), asts) ->
        st.EvalPush("BrackedCoordList")
        let fv = es.PeekEvalStack()
        adjustSignature fv "["
        asts |> List.map (eval st) |> ignore
        adjustSignature fv "]"
        st.EvalPop()
    | Ast.And((pos1, pos2), predicateAsts) ->
        st.EvalPush("And")
        let fv = es.PeekEvalStack()
        fv.FplRepresentation <- FplRepresentation.PredRepr FplPredicate.Undetermined
        adjustSignature fv "and"
        adjustSignature fv "("
        predicateAsts |> List.map (eval st) |> ignore
        adjustSignature fv ")"
        fv.NameEndPos <- pos2
        evaluateConjunction fv
        emitLG000orLG001Diagnostics fv "conjunction"
        st.EvalPop()
    | Ast.Or((pos1, pos2), predicateAsts) ->
        st.EvalPush("Or")
        let fv = es.PeekEvalStack()
        fv.FplRepresentation <- FplRepresentation.PredRepr FplPredicate.Undetermined
        adjustSignature fv "or"
        adjustSignature fv "("
        predicateAsts |> List.map (eval st) |> ignore
        adjustSignature fv ")"
        fv.NameEndPos <- pos2
        evaluateDisjunction fv
        emitLG000orLG001Diagnostics fv "disjunction"
        st.EvalPop()
    | Ast.Xor((pos1, pos2), predicateAsts) ->
        st.EvalPush("Xor")
        let fv = es.PeekEvalStack()
        setRepresentation st (FplRepresentation.PredRepr FplPredicate.Undetermined)
        adjustSignature fv "xor"
        adjustSignature fv "("
        predicateAsts |> List.map (eval st) |> ignore
        adjustSignature fv ")"
        fv.NameEndPos <- pos2
        evaluateExclusiveOr fv
        emitLG000orLG001Diagnostics fv "exclusive-or"
        st.EvalPop()
    | Ast.VarDeclBlock((pos1, pos2), asts) ->
        st.EvalPush("VarDeclBlock")
        asts |> List.map (eval st) |> ignore
        st.EvalPop()
    | Ast.StatementList((pos1, pos2), asts) ->
        st.EvalPush("StatementList")
        asts |> List.map (eval st) |> ignore
        st.EvalPop()
    | Ast.DefaultResult((pos1, pos2), asts) ->
        st.EvalPush("DefaultResult")
        asts |> List.map (eval st) |> ignore
        st.EvalPop()
    | Ast.Justification((pos1, pos2), asts) ->
        st.EvalPush("Justification")
        asts |> List.map (eval st) |> ignore
        st.EvalPop()
    | Ast.ArgumentTuple((pos1, pos2), asts) ->
        st.EvalPush("ArgumentTuple")
        let fv = es.PeekEvalStack()
        adjustSignature fv "("
        asts |> List.map (eval st) |> ignore
        adjustSignature fv ")"
        st.EvalPop()
    | Ast.QualificationList((pos1, pos2), asts) ->
        st.EvalPush("QualificationList")
        if asts.Length > 0 then
            let fv = es.PeekEvalStack()
            asts |> List.map (eval st) |> ignore
        st.EvalPop()
    // | Namespace of Ast option * Ast list
    | Ast.Namespace(optAst, asts) ->
        st.EvalPush("Namespace")
        optAst |> Option.map (eval st) |> ignore
        asts |> List.map (eval st) |> ignore
        let fv = es.PeekEvalStack()
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
            adjustSignature fv "->"
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
    | Ast.ClassType((pos1, pos2), (ast1, optAst)) ->
        st.EvalPush("ClassType")
        eval st ast1
        optAst |> Option.map (eval st) |> ignore
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
            match tryMatchSignatures st refBlock with
            | (_, _, Some matchedFplValue) -> ()
            | (firstFailingArgument, candidates, None) -> 
                emitSIG04Diagnostics refBlock candidates firstFailingArgument pos1 pos2 
            es.PopEvalStack()
        | None -> 
            // if no specification was found then simply continue in the same context
            eval st fplIdentifierAst
        st.EvalPop()
    // | SelfAts of Positions * char list
    | Ast.SelfAts((pos1, pos2), chars) -> 
        st.EvalPush("SelfAts")
        let identifier = (chars |> List.map (fun c -> c.ToString()) |>  String.concat "") + "self"
        let fv = es.PeekEvalStack()
        adjustSignature fv identifier
        st.EvalPop()
    // | Translation of string * Ast
    | Ast.Translation(s, ast1) ->
        st.EvalPush("Translation")
        eval st ast1
        eval_pos_string_ast st s
        st.EvalPop()
    // | ExtensionBlock of Positions * (Ast * Ast)
    | Ast.InheritedClassType((pos1, pos2), ast1) -> 
        st.EvalPush("InheritedClassType")
        eval st ast1
        st.EvalPop()
    | Ast.ExtensionBlock((pos1, pos2), (ast1, ast2)) ->
        st.EvalPush("ExtensionBlock")
        eval st ast1
        eval st ast2
        st.EvalPop()
    | Ast.Impl((pos1, pos2), (predicateAst1, predicateAst2)) ->
        st.EvalPush("Impl")
        let fv = es.PeekEvalStack()
        setRepresentation st (FplRepresentation.PredRepr FplPredicate.Undetermined)
        adjustSignature fv "impl"
        adjustSignature fv "("
        eval st predicateAst1
        eval st predicateAst2
        adjustSignature fv ")"
        fv.NameEndPos <- pos2
        evaluateImplication fv
        emitLG000orLG001Diagnostics fv "implication"
        st.EvalPop()
    | Ast.Iif((pos1, pos2), (predicateAst1, predicateAst2)) ->
        st.EvalPush("Iif")
        let fv = es.PeekEvalStack()
        setRepresentation st (FplRepresentation.PredRepr FplPredicate.Undetermined)
        adjustSignature fv "iif"
        adjustSignature fv "("
        eval st predicateAst1
        eval st predicateAst2
        adjustSignature fv ")"
        fv.NameEndPos <- pos2
        evaluateEquivalence fv
        emitLG000orLG001Diagnostics fv "equivalence"
        st.EvalPop()
    | Ast.IsOperator((pos1, pos2), (isOpArgAst, variableTypeAst)) ->
        st.EvalPush("IsOperator")
        let fv = es.PeekEvalStack()
        setRepresentation st (FplRepresentation.PredRepr FplPredicate.Undetermined)
        adjustSignature fv "is"
        adjustSignature fv "("

        let operand = FplValue.CreateFplValue((pos1, pos2), FplValueType.Reference, fv) 
        es.PushEvalStack(operand)
        eval st isOpArgAst
        es.PopEvalStack()

        let typeOfoperand = FplValue.CreateFplValue((pos1, pos2), FplValueType.Reference, fv) 
        es.PushEvalStack(typeOfoperand)
        eval st variableTypeAst
        es.PopEvalStack()

        adjustSignature fv ")"
        st.EvalPop()
    | Ast.Delegate((pos1, pos2), (fplDelegateIdentifierAst, argumentTupleAst)) ->
        st.EvalPush("Delegate")
        let fv = es.PeekEvalStack()
        let refBlock = FplValue.CreateFplValue((pos1, pos2), FplValueType.Reference, fv) 
        adjustSignature refBlock "del."
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
        let fplValue = FplValue.CreateFplValue((pos1, pos2), FplValueType.MandatoryPredicate, es.PeekEvalStack())
        es.PushEvalStack(fplValue)
        eval st definitionPropertyAst
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
        let theory = es.PeekEvalStack()
        let loc = FplValue.CreateFplValue((pos1, pos2),FplValueType.Localization,theory)
        es.PushEvalStack(loc)
        eval st predicateAst
        translationListAsts |> List.map (eval st) |> ignore
        es.PopEvalStack()
        st.EvalPop()
    | Ast.FunctionalTermInstance((pos1, pos2), (functionalTermSignatureAst, functionalTermInstanceBlockAst)) ->
        st.EvalPush("FunctionalTermInstance")
        eval st functionalTermSignatureAst
        eval st functionalTermInstanceBlockAst
        st.EvalPop()
    // | All of Positions * ((Ast list * Ast option) list * Ast)
    | Ast.All((pos1, pos2), (variableListInOptDomainListAst, predicateAst)) ->
        st.EvalPush("All")
        let fv = es.PeekEvalStack()
        fv.FplRepresentation <- FplRepresentation.PredRepr FplPredicate.Undetermined
        adjustSignature fv "all"
        variableListInOptDomainListAst
        |> List.map (fun (asts, optAst) ->
            asts |> List.map (eval st) |> ignore
            optAst |> Option.map (eval st) |> Option.defaultValue ()
            ())
        |> ignore
        eval st predicateAst
        emitLG000orLG001Diagnostics fv "all quantor"
        st.EvalPop()
    | Ast.Exists((pos1, pos2), (variableListInOptDomainListAst, predicateAst)) ->
        st.EvalPush("Exists")
        let fv = es.PeekEvalStack()
        fv.FplRepresentation <- FplRepresentation.PredRepr FplPredicate.Undetermined
        adjustSignature fv "ex"
        variableListInOptDomainListAst
        |> List.map (fun (asts, optAst) ->
            asts |> List.map (eval st) |> ignore
            optAst |> Option.map (eval st) |> Option.defaultValue ()
            ())
        |> ignore
        eval st predicateAst
        emitLG000orLG001Diagnostics fv "exists quantor"
        st.EvalPop()
    // | ExistsN of Positions * ((Ast * (Ast * Ast option)) * Ast)
    | Ast.ExistsN((pos1, pos2), ((dollarDigitsAst, (variableAst, inOptDomainAst)), predicateAst)) ->
        st.EvalPush("ExistsN")
        let fv = es.PeekEvalStack()
        fv.FplRepresentation <- FplRepresentation.PredRepr FplPredicate.Undetermined
        adjustSignature fv "exn"
        eval st dollarDigitsAst
        eval st variableAst
        inOptDomainAst |> Option.map (eval st) |> Option.defaultValue () |> ignore
        eval st predicateAst
        emitLG000orLG001Diagnostics fv "exists n times quantor"
        st.EvalPop()
    // | FunctionalTermSignature of Positions * (Ast * Ast)
    | Ast.FunctionalTermSignature((pos1, pos2), ((optAst, signatureWithUserDefinedStringAst), mappingAst)) -> 
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
        | None -> 
            if FplValue.IsFplBlock(fv) then
                fv.BlockType <- FplValueType.FunctionalTerm
            else
                fv.BlockType <- FplValueType.MandatoryFunctionalTerm
        adjustSignature fv "->"
        fv.NameEndPos <- pos2
        eval st mappingAst
        st.EvalPop()
    | Ast.PredicateWithQualification(predicateWithOptSpecificationAst, qualificationListAst) ->
        st.EvalPush("PredicateWithQualification")
        eval st predicateWithOptSpecificationAst
        eval st qualificationListAst
        st.EvalPop()
    // | InfixOperation of Positions * (Ast * Ast option) list
    | Ast.InfixOperation((pos1, pos2), separatedPredicateListAst) ->
        st.EvalPush("InfixOperation")
        let fv = es.PeekEvalStack()
        let dictOfOperators = Dictionary<string,FplValue>()
        separatedPredicateListAst
        |> List.map (fun (_, optSeparatorAst) -> 
            let infixOperator = FplValue.CreateFplValue((pos1,pos2),FplValueType.Reference,fv)
            es.PushEvalStack(infixOperator)
            optSeparatorAst |> Option.map (eval st) |> Option.defaultValue ()
            es.PopEvalStack()
            dictOfOperators.Add(infixOperator.Name, infixOperator)
        )
        |> ignore

        let sortedSeparatedPredicateListAst = 
            separatedPredicateListAst 
            |> List.sortBy (fun (_,opOpt) -> 
                if opOpt.IsSome then 
                    match opOpt.Value with
                    | Ast.InfixOperator ((p1,p2),symbol) -> 
                        if dictOfOperators.ContainsKey(symbol) then 
                            dictOfOperators[symbol].AuxiliaryInfo
                        else
                            Int32.MaxValue
                    | _ -> Int32.MaxValue
                else
                    Int32.MaxValue
            )

        /// Transforms a precedence-sorted list infix-operations into a nested binary infix operations in reversed Polish notation
        let rec createReversedPolishNotation (sortedSeparatedPredicateList:(Ast * Ast option) list) (fv:FplValue) =
            match sortedSeparatedPredicateList with
            | (predicateAst,optOp) :: xs -> 
                if optOp.IsSome then
                    match optOp.Value with 
                    | Ast.InfixOperator ((p1,p2),symbol) ->
                        // add any found candidate FPL blocks matching this symbol to the newly created nested infix operation
                        dictOfOperators[symbol].Scope 
                        |> Seq.iter (fun kv -> fv.Scope.Add(kv.Key,kv.Value))
                        if dictOfOperators.ContainsKey(symbol) then 
                            adjustSignature fv symbol
                            adjustSignature fv "("
                            eval st predicateAst 
                            if xs.Length > 0 then
                                let nextInfixOperation = FplValue.CreateFplValue((p1,p2),FplValueType.Reference,fv)
                                es.PushEvalStack(nextInfixOperation)
                                createReversedPolishNotation xs nextInfixOperation
                                es.PopEvalStack()                                
                            adjustSignature fv ")"
                    | _ -> ()
                else
                    eval st predicateAst 
            | [] -> ()

        createReversedPolishNotation sortedSeparatedPredicateListAst fv
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
                adjustSignature refBlock "("
                // ... over prefix notation in mathematics
                prefixOpAst |> Option.map (eval st) |> Option.defaultValue ()
                adjustSignature refBlock "("
                eval st predicateAst
                adjustSignature refBlock ")"
                adjustSignature refBlock ")"
            elif prefixOpAst.IsSome then 
                prefixOpAst |> Option.map (eval st) |> Option.defaultValue ()
                adjustSignature refBlock "("
                eval st predicateAst
                adjustSignature refBlock ")"
            elif postfixOpAst.IsSome then 
                postfixOpAst |> Option.map (eval st) |> Option.defaultValue ()
                adjustSignature refBlock "("
                eval st predicateAst
                adjustSignature refBlock ")"
            else
                eval st predicateAst
        ensureReversedPolishNotation
        optionalSpecificationAst |> Option.map (eval st) |> Option.defaultValue ()
        eval st qualificationListAst
        let refBlock = es.PeekEvalStack() // if the reference was replaced, take this one
        match (fv.BlockType, fv.FplRepresentation,refBlock.FplRepresentation,fv.ValueList.Count) with
        | (FplValueType.Reference, FplRepresentation.Undef, FplRepresentation.Pointer var, 1) ->
            fv.FplRepresentation <- refBlock.FplRepresentation
        | _ -> ()
        refBlock.NameEndPos <- pos2
        if FplValue.IsFplBlock(fv) || FplValue.IsConstructorOrProperty(fv) then
            fv.ValueList.Add(refBlock)
        es.PopEvalStack()
        st.EvalPop()
    // | Cases of Positions * (Ast list * Ast)
    | Ast.Cases((pos1, pos2), (asts, ast1)) ->
        st.EvalPush("Cases")
        asts |> List.map (eval st) |> ignore
        eval st ast1
        st.EvalPop()
    // | Assignment of Positions * (Ast * Ast)
    | Ast.Signature((pos1, pos2), (predicateIdentifierAst, paramTupleAst)) ->
        st.EvalPush("Signature")
        eval st predicateIdentifierAst
        eval st paramTupleAst
        let fv = es.PeekEvalStack()
        st.EvalPop()
    | Ast.Assignment((pos1, pos2), (ast1, ast2)) ->
        st.EvalPush("Assignment")
        eval st ast1
        eval st ast2
        st.EvalPop()
    | Ast.PredicateInstance((pos1, pos2), ((optAst, signatureAst), predInstanceBlockAst)) ->
        st.EvalPush("PredicateInstance")
        eval st signatureAst
        let fv = es.PeekEvalStack()
        setRepresentation st (FplRepresentation.PredRepr FplPredicate.Undetermined)
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
        let refBlock = FplValue.CreateFplValue((pos1, pos2), FplValueType.Reference, fv) 
        es.PushEvalStack(refBlock)
        refBlock.Name <- "bas."
        eval st inheritedClassTypeAst
        eval st argumentTupleAst
        es.PopEvalStack()
        st.EvalPop()
    | Ast.JustifiedArgument((pos1, pos2), (ast1, ast2)) ->
        st.EvalPush("JustifiedArgument")
        eval st ast1
        eval st ast2
        st.EvalPop()
    | Ast.Argument((pos1, pos2), (ast1, ast2)) ->
        st.EvalPush("Argument")
        eval st ast1
        eval st ast2
        st.EvalPop()
    // | ForIn of Positions * ((Ast * Ast) * Ast list)
    | Ast.ForIn((pos1, pos2), ((ast1, ast2), asts)) ->
        st.EvalPush("ForIn")
        eval st ast1
        eval st ast2
        asts |> List.map (eval st) |> ignore
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
        let fplValue = FplValue.CreateFplValue((pos1, pos2), FplValueType.Theorem, es.PeekEvalStack())
        es.PushEvalStack(fplValue)
        eval st signatureAst
        evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst
        es.PopEvalStack()
        st.EvalPop()
    | Ast.Lemma((pos1, pos2), (signatureAst, (optVarDeclOrSpecList, predicateAst))) ->
        st.EvalPush("Lemma")
        let fplValue = FplValue.CreateFplValue((pos1, pos2), FplValueType.Lemma, es.PeekEvalStack())
        es.PushEvalStack(fplValue)
        eval st signatureAst
        evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst
        es.PopEvalStack()
        st.EvalPop()
    | Ast.Proposition((pos1, pos2), (signatureAst, (optVarDeclOrSpecList, predicateAst))) ->
        st.EvalPush("Proposition")
        let fplValue = FplValue.CreateFplValue((pos1, pos2), FplValueType.Proposition, es.PeekEvalStack())
        es.PushEvalStack(fplValue)
        eval st signatureAst
        evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst
        es.PopEvalStack()
        st.EvalPop()
    | Ast.Conjecture((pos1, pos2), (signatureAst, (optVarDeclOrSpecList, predicateAst))) ->
        st.EvalPush("Conjecture")
        let fplValue = FplValue.CreateFplValue((pos1, pos2), FplValueType.Conjecture, es.PeekEvalStack())
        es.PushEvalStack(fplValue)
        eval st signatureAst
        evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst
        es.PopEvalStack()
        st.EvalPop()
    | Ast.Axiom((pos1, pos2), (signatureAst, (optVarDeclOrSpecList, predicateAst))) ->
        st.EvalPush("Axiom")
        let fplValue = FplValue.CreateFplValue((pos1, pos2), FplValueType.Axiom, es.PeekEvalStack())
        es.PushEvalStack(fplValue)
        eval st signatureAst
        evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst
        es.PopEvalStack()
        st.EvalPop()
    // | Corollary of Positions * ((Ast * Ast) * (Ast list option * Ast))
    | Ast.CorollarySignature(referencingIdentifierAst, paramTupleAst) ->
        st.EvalPush("CorollarySignature")
        eval st referencingIdentifierAst
        eval st paramTupleAst
        let fv = es.PeekEvalStack()
        st.EvalPop()
    | Ast.Corollary((pos1, pos2), (corollarySignatureAst, (optVarDeclOrSpecList, predicateAst))) ->
        st.EvalPush("Corollary")
        let fplValue = FplValue.CreateFplValue((pos1, pos2), FplValueType.Corollary, es.PeekEvalStack())
        es.PushEvalStack(fplValue)
        eval st corollarySignatureAst
        evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst
        es.PopEvalStack()
        st.EvalPop()
    // | NamedVarDecl of Positions * ((Ast list * Ast) * Ast)
    | Ast.NamedVarDecl((pos1, pos2), ((variableListAst, varDeclModifierAst), variableTypeAst)) ->
        st.EvalPush("NamedVarDecl")
        let fplValue = es.PeekEvalStack()
        fplValue.AuxiliaryInfo <- variableListAst |> List.length // remember how many variables to create
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
        let fplValue = FplValue.CreateFplValue((pos1, pos2), FplValueType.Constructor, es.PeekEvalStack())
        es.PushEvalStack(fplValue)
        eval st signatureAst
        match optVarDeclOrSpecListAst with
        | Some astList -> astList |> List.map (eval st) |> ignore
        | None -> ()
        eval st keywordSelfAst
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
        eval st signatureWithUserDefinedStringAst
        eval st predicateContentAst
        optPropertyListAsts |> Option.map (List.map (eval st) >> ignore) |> Option.defaultValue ()
        es.PopEvalStack()
        st.EvalPop()
    // | DefinitionFunctionalTerm of Positions * ((Ast * Ast) * (Ast * Ast list option))
    | Ast.DefinitionFunctionalTerm((pos1, pos2), (functionalTermSignatureAst, (funcContentAst, optPropertyListAsts))) ->
        st.EvalPush("DefinitionFunctionalTerm")
        let fplValue = FplValue.CreateFplValue((pos1, pos2), FplValueType.FunctionalTerm, es.PeekEvalStack())
        es.PushEvalStack(fplValue)
        eval st functionalTermSignatureAst
        eval st funcContentAst
        optPropertyListAsts |> Option.map (List.map (eval st) >> ignore) |> Option.defaultValue ()
        es.PopEvalStack()
        st.EvalPop()
    // | DefinitionClass of Positions * (((Ast * Ast option) * Ast list) * (Ast * Ast list option))
    | Ast.DefinitionClass((pos1, pos2),
                          (((predicateIdentifierAst, optUserDefinedObjSymAst), classTypeListAsts),
                           (classContentAst, optPropertyListAsts))) ->
        st.EvalPush("DefinitionClass")
        let fv = FplValue.CreateFplValue((pos1, pos2), FplValueType.Class, es.PeekEvalStack())
        es.PushEvalStack(fv)
        eval st predicateIdentifierAst
        optUserDefinedObjSymAst |> Option.map (eval st) |> Option.defaultValue ()
        classTypeListAsts |> List.map (eval st) |> ignore
        eval st classContentAst
        optPropertyListAsts
        |> Option.map (List.map (eval st) >> ignore)
        |> Option.defaultValue ()
        es.PopEvalStack()
        st.EvalPop()
    // | DerivedPredicate of Ast
    | Ast.DerivedPredicate ast1 -> 
        st.EvalPush("DefinitionClass")
        eval st ast1
        st.EvalPop()
    // | Proof of Positions * (Ast * (Ast list * Ast option))
    | Ast.Proof((pos1, pos2), (referencingIdentifierAst, (proofArgumentListAst, optQedAst))) ->
        st.EvalPush("Proof")
        let fv = FplValue.CreateFplValue((pos1, pos2), FplValueType.Proof, es.PeekEvalStack())
        es.PushEvalStack(fv)
        eval st referencingIdentifierAst
        proofArgumentListAst |> List.map (eval st) |> ignore
        optQedAst |> Option.map (eval st) |> Option.defaultValue ()
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
            let theoryValue = FplValue.CreateFplValue((Position("",0,1,1), Position("",0,1,1)), FplValueType.Theory, st.Root)
            if not (st.Root.Scope.ContainsKey(pa.Id)) then
                st.Root.Scope.Add(pa.Id, theoryValue)
            else
                st.Root.Scope[pa.Id].Reset()
                st.Root.Scope[pa.Id] <- theoryValue
            theoryValue.Name <- pa.Id
            es.PushEvalStack(theoryValue)
            ad.CurrentUri <- pa.Parsing.Uri
            eval st pa.Parsing.Ast
            pa.Status <- ParsedAstStatus.Evaluated
            es.PopEvalStack()
        | None -> found <- false
