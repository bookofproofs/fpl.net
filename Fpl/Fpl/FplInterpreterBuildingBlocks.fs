module FplInterpreterBuildingBlocks

open System
open System.Linq
open System.Collections.Generic
open FParsec
open ErrDiagnostics
open FplGrammarTypes
open FplInterpreterTypes
open FplInterpreterDiagnosticsEmitter
open FplInterpreterPredicateEvaluator
open System.Text

let private addWithComma (name:string) str = 
    if str <> "" then
        if str = "(" || str = ")" 
            || str = "[" || str = "]" 
            || str = "->"
            || name.EndsWith "(" 
            || name.EndsWith "[" 
            || name.Length = 0 
            || name.EndsWith " " 
            || name.EndsWith "." 
            || str.StartsWith "$" then
                if str = "->" then 
                    name + " " + str + " "
                else
                    name + str
        else
            name + ", " + str
    else name

let rec adjustSignature (st:SymbolTable) (fplValue:FplValue) str = 
    if str <> "" then
        if FplValue.IsDefinition(fplValue) && fplValue.NameIsFinal then  
            () //  for definitions with final name stop changing the TypeSignature
        else
            // note: the manipulation of the TypeSignature is necessary for all kinds of fplValue
            if str.StartsWith("*") && str <> "*" then
                fplValue.TypeSignature <- fplValue.TypeSignature @ ["*"; str.Substring(1)]
            elif str.StartsWith("+") && str <> "+" then
                fplValue.TypeSignature <- fplValue.TypeSignature @ ["+"; str.Substring(1)]
            elif str.StartsWith("$") && str <> "$" then
                fplValue.TypeSignature <- fplValue.TypeSignature @ ["ind"]
            elif str = "true" || str = "false" then
                fplValue.TypeSignature <- fplValue.TypeSignature @ ["pred"]
            else
                fplValue.TypeSignature <- fplValue.TypeSignature @ [str]
        match st.CurrentContext with
        | EvalContext.InPropertySignature _
        | EvalContext.InConstructorSignature _
        | EvalContext.InSignature _ -> 
            if not (FplValue.IsFplBlock(fplValue)) then 
                match fplValue.Parent with
                | Some parent -> 
                        adjustSignature st parent str
                | None -> ()
        | EvalContext.NamedVarDeclarationInBlock _ -> 
            match fplValue.Parent with
            | Some parent -> 
                if (FplValue.IsVariable(parent)) then 
                    adjustSignature st parent str
            | None -> ()
        | _ -> ()

    if str <> "" && not (FplValue.IsVariable(fplValue)) then
        if FplValue.IsDefinition(fplValue) && fplValue.NameIsFinal then 
            () // for definitions with final name stop changing the name
        else
            // note: the Name attribute of variables are set in Ast.Var directly
            // and we do not want to append the type to the names of variables.
            fplValue.Name <- addWithComma fplValue.Name str 

    if str ="(" || str = "[" then 
        fplValue.AuxiliaryInfo <- fplValue.AuxiliaryInfo + 1
    elif str =")" || str = "]" then 
        fplValue.AuxiliaryInfo <- fplValue.AuxiliaryInfo - 1

let setRepresentation (st: SymbolTable) representation = 
    match st.CurrentContext with
    | EvalContext.NamedVarDeclarationInBlock fplValue 
    | EvalContext.InPropertySignature fplValue 
    | EvalContext.InConstructorSignature fplValue
    | EvalContext.InSignature fplValue 
    | EvalContext.InReferenceCreation fplValue -> 
        fplValue.FplRepresentation <- representation
    | _ -> ()

let eval_units (st: SymbolTable) unitType pos1 pos2 = 
    match st.CurrentContext with
    | EvalContext.NamedVarDeclarationInBlock fplValue 
    | EvalContext.InPropertySignature fplValue 
    | EvalContext.InConstructorSignature fplValue
    | EvalContext.InSignature fplValue -> 
        if unitType <> "" then 
            if (FplValue.IsVariadicVariableMany(fplValue)) then 
                adjustSignature st fplValue $"*{unitType}"
            elif (FplValue.IsVariadicVariableMany1(fplValue)) then 
                adjustSignature st fplValue $"+{unitType}"
            else
                adjustSignature st fplValue unitType
                checkID009_ID010_ID011_Diagnostics st fplValue unitType pos1 pos2
    | EvalContext.InReferenceCreation fplValue -> 
        checkID012Diagnostics st fplValue unitType pos1 pos2
    | _ -> ()

let eval_string (st: SymbolTable) s = ()

let eval_pos_string (st: SymbolTable) (startpos: Position) (endpos: Position) ast = ()

let eval_pos_unit (st: SymbolTable) (startpos: Position) (endpos: Position) = ()

let eval_pos_ast (st: SymbolTable) (startpos: Position) (endpos: Position) = ()

let eval_pos_ast_ast_opt (st: SymbolTable) (startpos: Position) (endpos: Position) = ()

let eval_pos_char_list (st: SymbolTable) (startpos: Position) (endpos: Position) charlist =
    charlist |> List.map string |> String.concat "" |> ignore

let eval_pos_string_ast (st: SymbolTable) str = ()

let tryAddBlock (fplValue:FplValue) =

    match FplValue.TryFindAssociatedBlockForProof fplValue with
    | ScopeSearchResult.FoundAssociate parentsName -> 
        // everything is ok, change the parent of the provable from theory to the found parent 
        fplValue.Parent <- Some fplValue.Parent.Value.Scope[parentsName]
    | ScopeSearchResult.FoundIncorrectBlock block ->
        emitID002diagnostics fplValue block  
    | ScopeSearchResult.NotFound ->
        emitID003diagnostics fplValue  
    | ScopeSearchResult.FoundMultiple listOfKandidates ->
        emitID004diagnostics fplValue listOfKandidates  
    | _ -> ()

    match FplValue.TryFindAssociatedBlockForCorollary fplValue with
    | ScopeSearchResult.FoundAssociate parentsName -> 
        // everything is ok, change the parent of the provable from theory to the found parent 
        fplValue.Parent <- Some fplValue.Parent.Value.Scope[parentsName]
        // now, we are ready to emit VAR03 diagnostics for all variables declared in the signature of the corollary.
        emitVAR03diagnosticsForCorollarysSignatureVariable fplValue  
    | ScopeSearchResult.FoundIncorrectBlock block ->
        emitID005diagnostics fplValue block  
    | ScopeSearchResult.NotFound ->
        emitID006diagnostics fplValue  
    | ScopeSearchResult.FoundMultiple listOfKandidates ->
        emitID007diagnostics fplValue listOfKandidates  
    | _ -> ()

    match FplValue.VariableInBlockScopeByName(fplValue) fplValue.Name with
    | ScopeSearchResult.Found other ->
        emitVAR03diagnostics fplValue other 
    | _ -> 
        match FplValue.InScopeOfParent(fplValue) fplValue.Name with
        | ScopeSearchResult.Found conflict -> 
            emitID001diagnostics fplValue conflict 
        | _ -> 
            fplValue.Parent.Value.Scope.Add(fplValue.Name,fplValue)
            fplValue.NameIsFinal <- true
            (* This code will fix a single VAR03 test but break ~1400 other unit tests!!!
               we leave the code for future inspiration of correctly fixing VAR03 
            match fplValue.Parent with
            | Some parent when FplValue.IsVariable(parent) -> 
                parent.Scope.Add(fplValue.Name,fplValue)
                fplValue.NameIsFinal <- true
                let rec addAlsoToTheScopeToWhichTheLastParentVariableBelongsTo (p:FplValue) =
                    match p.Parent with
                    | Some otherParent when FplValue.IsVariable(otherParent) -> addAlsoToTheScopeToWhichTheLastParentVariableBelongsTo (otherParent)
                    | Some otherParent -> otherParent.Scope.Add(fplValue.Name,fplValue)
                    | _ -> ()
                addAlsoToTheScopeToWhichTheLastParentVariableBelongsTo(parent)
            | Some parent -> 
                parent.Scope.Add(fplValue.Name,fplValue)
                fplValue.NameIsFinal <- true
            | _ -> ()
            *)

let propagateReference (refBlock:FplValue) withAdding = 
    let fplValue = refBlock.Parent.Value
    if fplValue.BlockType = FplValueType.Reference then
        if not fplValue.NameIsFinal && refBlock.AuxiliaryInfo = 0 then
            // propagate references only if refblock has all opened brackets closed and the name of its reference-typed parent is not yet ready
            if not (fplValue.ValueList.Contains(refBlock)) then 
                if withAdding then 
                    fplValue.ValueList.Add(refBlock)
                match refBlock.FplRepresentation with
                | FplRepresentation.Pointer variable ->
                    fplValue.Name <- addWithComma fplValue.Name variable.Name 
                    fplValue.TypeSignature <- fplValue.TypeSignature @ variable.TypeSignature
                | _ -> 
                    fplValue.Name <- addWithComma fplValue.Name refBlock.Name 
                    fplValue.TypeSignature <- fplValue.TypeSignature @ refBlock.TypeSignature
    else
        if withAdding then 
            if not (fplValue.ValueList.Contains(refBlock)) then 
                fplValue.ValueList.Add(refBlock)



/// A recursive function evaluating an AST and returning a list of EvalAliasedNamespaceIdentifier records
/// for each occurrence of the uses clause in the FPL code.
let rec eval (st: SymbolTable) ast =
    let evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst =
        match optVarDeclOrSpecList with
        | Some astList -> astList |> List.map (eval st) |> ignore
        | None -> ()
        eval st predicateAst

    let evalMany (st:SymbolTable) blockType pos1 pos2 = 
        match st.CurrentContext with
        | EvalContext.NamedVarDeclarationInBlock fplValue
        | EvalContext.InPropertySignature fplValue
        | EvalContext.InConstructorSignature fplValue
        | EvalContext.InSignature fplValue -> 
            tryAddVariadicVariables fplValue.AuxiliaryInfo pos1 pos2
            // adjust type of variables to variadic variables, if their type has not yet been established
            fplValue.Scope
            |> Seq.filter (fun varKeyValue -> FplValue.IsVariable(varKeyValue.Value) && varKeyValue.Value.TypeSignature = [])
            |> Seq.iter (fun varKeyValue -> 
                varKeyValue.Value.BlockType <- blockType
            )
        | _ -> ()

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
        match st.CurrentContext with
        | EvalContext.InReferenceCreation fplValue ->
            adjustSignature st fplValue s
        | _ -> ()
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
        match st.CurrentContext with
        | EvalContext.NamedVarDeclarationInBlock fplValue 
        | EvalContext.InPropertySignature fplValue 
        | EvalContext.InConstructorSignature fplValue
        | EvalContext.InSignature fplValue ->
            adjustSignature st fplValue ("$"+s.ToString())
            fplValue.NameEndPos <- pos2 // the full name ends where the dollar digits end 
        | EvalContext.InReferenceCreation fplValue ->
            adjustSignature st fplValue ("$"+s.ToString())
            fplValue.NameEndPos <- pos2 // the full name ends where the dollar digits end 
            if fplValue.FplRepresentation = FplRepresentation.Undef then
                fplValue.FplRepresentation <- FplRepresentation.Index s
        | _ -> ()
        st.EvalPop() 
    | Ast.Extensionname((pos1, pos2), s) ->
        st.EvalPush("Extensionname")
        match st.CurrentContext with
        | EvalContext.NamedVarDeclarationInBlock fplValue 
        | EvalContext.InPropertySignature fplValue 
        | EvalContext.InConstructorSignature fplValue
        | EvalContext.InSignature fplValue -> 
            if (FplValue.IsVariadicVariableMany(fplValue)) then 
                adjustSignature st fplValue ("*@" + s)
            elif (FplValue.IsVariadicVariableMany1(fplValue)) then 
                adjustSignature st fplValue ("+@" + s)
            else
                adjustSignature st fplValue ("@" + s)
        | _ -> ()
        st.EvalPop() 
    | Ast.TemplateType((pos1, pos2), s) -> 
        st.EvalPush("TemplateType")
        match st.CurrentContext with
        | EvalContext.NamedVarDeclarationInBlock fplValue 
        | EvalContext.InPropertySignature fplValue 
        | EvalContext.InConstructorSignature fplValue
        | EvalContext.InSignature fplValue -> 
            fplValue.FplRepresentation <- FplRepresentation.ObjRepr "obj"
            if (FplValue.IsVariadicVariableMany(fplValue)) then 
                adjustSignature st fplValue ("*" + s)
            elif (FplValue.IsVariadicVariableMany1(fplValue)) then 
                adjustSignature st fplValue ("+" + s)
            else
                adjustSignature st fplValue s
        | _ -> ()
        st.EvalPop() 
    | Ast.Var((pos1, pos2), name) ->
        st.EvalPush("Var")
        match st.CurrentContext with
        | EvalContext.NamedVarDeclarationInBlock fplValue 
        | EvalContext.InPropertySignature fplValue 
        | EvalContext.InConstructorSignature fplValue
        | EvalContext.InSignature fplValue -> 
            let varValue = FplValue.CreateFplValue((pos1,pos2), FplValueType.Variable, fplValue)
            varValue.Name <- name
            varValue.NameEndPos <- pos2
            tryAddBlock varValue 
        | EvalContext.InReferenceCreation fplValue ->
            match FplValue.VariableInBlockScopeByName fplValue name with 
            | ScopeSearchResult.Found variableInScope ->
                // replace the reference by a pointer to an existing declared variable
                fplValue.FplRepresentation <- FplRepresentation.Pointer variableInScope
            | _ -> 
                // otherwise, the variable is not declared, emit VAR01 diagnostics 
                fplValue.Name <- addWithComma fplValue.Name name
                fplValue.TypeSignature <- fplValue.TypeSignature @ ["undef"]
                emitVAR01diagnostics name pos1 pos2
        | _ -> ()
        st.EvalPop() 
    | Ast.DelegateId((pos1, pos2), s) -> 
        st.EvalPush("DelegateId")
        match st.CurrentContext with
        | EvalContext.InReferenceCreation fplValue ->
            adjustSignature st fplValue s
        | _ -> ()
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
        match st.CurrentContext with
        | EvalContext.InReferenceCreation fplValue ->
            adjustSignature st fplValue s
        | _ -> ()
        st.EvalPop() 
    | Ast.ArgumentIdentifier((pos1, pos2), s) -> 
        st.EvalPush("ArgumentIdentifier")
        match st.CurrentContext with
        | EvalContext.InReferenceCreation fplValue ->
            adjustSignature st fplValue s
            emitPR000Diagnostics fplValue s pos1 pos2
        | EvalContext.InBlock fplValue
        | EvalContext.InConstructorBlock fplValue
        | EvalContext.InConstructorSignature fplValue
        | EvalContext.InInfixOperation fplValue
        | EvalContext.InPropertyBlock fplValue
        | EvalContext.InPropertySignature fplValue
        | EvalContext.InSignature fplValue ->
            emitPR000Diagnostics fplValue s pos1 pos2
        | _ -> () 
        st.EvalPop() 
    | Ast.Prefix((pos1, pos2), symbol) -> 
        st.EvalPush("Prefix")
        match st.CurrentContext with
        | EvalContext.InSignature fplValue ->
            fplValue.ExpressionType <- FixType.Prefix symbol
        | _ -> ()
        st.EvalPop() 
    | Ast.Infix((pos1, pos2), (symbol, precedenceAsts)) -> 
        st.EvalPush("Infix")
        eval st precedenceAsts
        match st.CurrentContext with
        | EvalContext.InSignature fplValue ->
            fplValue.ExpressionType <- FixType.Infix (symbol, fplValue.AuxiliaryInfo)
            emitSIG02Diagnostics st fplValue pos1 pos2 
        | _ -> ()
        st.EvalPop() 
    | Ast.Postfix((pos1, pos2), symbol) -> 
        st.EvalPush("Postfix")
        match st.CurrentContext with
        | EvalContext.InSignature fplValue ->
            fplValue.ExpressionType <- FixType.Postfix symbol
        | _ -> ()
        st.EvalPop() 
    | Ast.Symbol((pos1, pos2), s) -> 
        st.EvalPush("Symbol")
        eval_pos_string st pos1 pos2 s
        st.EvalPop() 
    | Ast.InfixOperator((pos1, pos2), symbol) -> 
        st.EvalPush("InfixOperator")
        match st.CurrentContext with
        | EvalContext.InReferenceCreation fplValue ->
            adjustSignature st fplValue symbol
        | EvalContext.InInfixOperation fplValue ->
            adjustSignature st fplValue symbol
            emitSIG01Diagnostics st fplValue pos1 pos2 
        | _ -> ()
        st.EvalPop() 
    | Ast.PostfixOperator((pos1, pos2), symbol) -> 
        st.EvalPush("PostfixOperator")
        match st.CurrentContext with
        | EvalContext.InReferenceCreation fplValue ->
            adjustSignature st fplValue symbol
            emitSIG01Diagnostics st fplValue pos1 pos2 
        | _ -> ()
        st.EvalPop() 
    | Ast.PrefixOperator((pos1, pos2), symbol) -> 
        st.EvalPush("PrefixOperator")
        match st.CurrentContext with
        | EvalContext.InReferenceCreation fplValue ->
            adjustSignature st fplValue symbol
            emitSIG01Diagnostics st fplValue pos1 pos2 
        | _ -> ()
        st.EvalPop() 
    // | Self of Positions * unit
    | Ast.Self((pos1, pos2), _) -> 
        st.EvalPush("Self")
        eval_pos_unit st pos1 pos2
        st.EvalPop() 
    | Ast.True((pos1, pos2), _) -> 
        st.EvalPush("True")
        match st.CurrentContext with
        | EvalContext.InReferenceCreation fplValue ->
            fplValue.FplRepresentation <- FplRepresentation.PredRepr FplPredicate.True
            adjustSignature st fplValue "true"
        | _ -> ()
        st.EvalPop() 
    | Ast.False((pos1, pos2), _) -> 
        st.EvalPush("False")
        match st.CurrentContext with
        | EvalContext.InReferenceCreation fplValue ->
            fplValue.FplRepresentation <- FplRepresentation.PredRepr FplPredicate.False
            adjustSignature st fplValue "false"
        | _ -> ()
        st.EvalPop() 
    | Ast.Undefined((pos1, pos2), _) -> 
        st.EvalPush("Undefined")
        match st.CurrentContext with
        | EvalContext.InReferenceCreation fplValue ->
            adjustSignature st fplValue "undef"
        | _ -> ()
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
        let oldContext = st.CurrentContext
        match st.CurrentContext with
        | EvalContext.InTheory theoryValue -> 
            let fplValue = FplValue.CreateFplValue((pos1, pos2), FplValueType.RuleOfInference, theoryValue)
            st.SetContext(EvalContext.InSignature fplValue) LogContext.Start
            eval st signatureAst
            st.SetContext(EvalContext.InBlock fplValue) LogContext.Replace
            eval st premiseConclusionBlockAst
            tryAddBlock fplValue 
        | _ -> ()
        st.SetContext(oldContext) LogContext.End
        st.EvalPop() 
    | Ast.ClassIdentifier((pos1, pos2), ast1) ->
        st.EvalPush("ClassIdentifier")
        eval st ast1
        match st.CurrentContext with 
        | EvalContext.InPropertySignature fplValue 
        | EvalContext.InConstructorSignature fplValue
        | EvalContext.InSignature fplValue -> 
            fplValue.NameEndPos <- pos2
        | _ -> ()
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
        match st.CurrentContext with
        | EvalContext.InBlock fplValue
        | EvalContext.InPropertyBlock fplValue 
        | EvalContext.InConstructorBlock fplValue 
        | EvalContext.InReferenceCreation fplValue ->
            adjustSignature st fplValue "not"
            adjustSignature st fplValue "("
            eval st predicateAst
            adjustSignature st fplValue ")"
            fplValue.NameEndPos <- pos2
            evaluateNegation fplValue
            emitLG000orLG001Diagnostics fplValue "negation"
        | _ -> ()
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
        match st.CurrentContext with 
        | EvalContext.InReferenceCreation fplValue ->
            fplValue.FplRepresentation <- FplRepresentation.PredRepr FplPredicate.Undetermined
            adjustSignature st fplValue "bydef."
            eval st predicateWithQualificationAst
            emitPR001Diagnostics fplValue pos1 pos2
        | EvalContext.InBlock fplValue
        | EvalContext.InConstructorBlock fplValue
        | EvalContext.InConstructorSignature fplValue
        | EvalContext.InInfixOperation fplValue
        | EvalContext.InPropertyBlock fplValue
        | EvalContext.InPropertySignature fplValue
        | EvalContext.InSignature fplValue ->
            emitPR001Diagnostics fplValue pos1 pos2
        | _ -> ()
        st.EvalPop()
    | Ast.DottedPredicate((pos1, pos2), predicateWithOptSpecificationAst) ->
        st.EvalPush("DottedPredicate")
        let oldContext = st.CurrentContext
        match st.CurrentContext with 
        | EvalContext.InReferenceCreation fplValue -> 
            let refBlock = FplValue.CreateFplValue((pos1, pos2), FplValueType.Reference, fplValue) 
            fplValue.Name <- fplValue.Name + "."
            fplValue.TypeSignature <- fplValue.TypeSignature @ ["."]
            st.SetContext(EvalContext.InReferenceCreation refBlock) LogContext.Replace
            eval st predicateWithOptSpecificationAst
            propagateReference refBlock true
        | _ -> ()
        st.SetContext(oldContext) LogContext.End
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
        match st.CurrentContext with
        | EvalContext.NamedVarDeclarationInBlock fplValue ->
            if (FplValue.IsVariadicVariableMany(fplValue)) then 
                adjustSignature st fplValue ("*" + identifier)
            elif (FplValue.IsVariadicVariableMany1(fplValue)) then 
                adjustSignature st fplValue ("+" + identifier)
            else
                adjustSignature st fplValue identifier
            emitSIG04TypeDiagnostics st identifier fplValue pos1 pos2
        | EvalContext.InTheory fplValue
        | EvalContext.InPropertySignature fplValue 
        | EvalContext.InConstructorSignature fplValue 
        | EvalContext.InSignature fplValue -> 
            if (FplValue.IsVariadicVariableMany(fplValue)) then 
                adjustSignature st fplValue ("*" + identifier)
            elif (FplValue.IsVariadicVariableMany1(fplValue)) then 
                adjustSignature st fplValue ("+" + identifier)
            else
                adjustSignature st fplValue identifier
            checkID008Diagnostics fplValue pos1 pos2
            checkID009_ID010_ID011_Diagnostics st fplValue identifier pos1 pos2
            emitSIG04TypeDiagnostics st identifier fplValue pos1 pos2
            match st.CurrentContext with 
            | EvalContext.InPropertySignature _ -> 
                let repr = 
                    fplValue.ValueList
                    |> Seq.map(fun baseClass -> baseClass.Name)
                    |> Seq.toList
                    |> List.append ["obj"]
                    |> String.concat ","
                fplValue.FplRepresentation <- FplRepresentation.ObjRepr repr
            | _ -> ()
        | EvalContext.InReferenceCreation fplValue -> 
            adjustSignature st fplValue identifier
            checkID012Diagnostics st fplValue identifier pos1 pos2
            emitSIG04TypeDiagnostics st identifier fplValue pos1 pos2
        | _ -> ()
        st.EvalPop()
    | Ast.ParamTuple((pos1, pos2), namedVariableDeclarationListAsts) ->
        
        st.EvalPush("ParamTuple")
        match st.CurrentContext with
        | EvalContext.NamedVarDeclarationInBlock fplValue 
        | EvalContext.InPropertySignature fplValue 
        | EvalContext.InConstructorSignature fplValue
        | EvalContext.InSignature fplValue -> 
            adjustSignature st fplValue "("
            namedVariableDeclarationListAsts |> List.map (
                fun child ->
                match child with 
                | Ast.NamedVarDecl(_,((varList,_),_)) -> fplValue.Arity <- fplValue.Arity + varList.Length
                | _ -> ()
                eval st child
            ) |> ignore
            adjustSignature st fplValue ")"
            fplValue.NameEndPos <- pos2
        | _ -> ()
        st.EvalPop()
    | Ast.BracketedCoordsInType((pos1, pos2), asts) ->
        st.EvalPush("BracketedCoordsInType")
        
        match st.CurrentContext with
        | EvalContext.NamedVarDeclarationInBlock fplValue 
        | EvalContext.InPropertySignature fplValue 
        | EvalContext.InConstructorSignature fplValue
        | EvalContext.InSignature fplValue -> 
            adjustSignature st fplValue "["
            asts 
            |> List.map (fun ast1 ->
                eval st ast1
            ) |> ignore
            adjustSignature st fplValue "]"
            fplValue.NameEndPos <- pos2
        | _ -> ()
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
        match st.CurrentContext with
        | EvalContext.InReferenceCreation fplValue -> 
            adjustSignature st fplValue "["
            asts |> List.map (eval st) |> ignore
            adjustSignature st fplValue "]"
        | _-> ()
        st.EvalPop()
    | Ast.And((pos1, pos2), predicateAsts) ->
        st.EvalPush("And")
        match st.CurrentContext with
        | EvalContext.InReferenceCreation fplValue -> 
            fplValue.FplRepresentation <- FplRepresentation.PredRepr FplPredicate.Undetermined
            adjustSignature st fplValue "and"
            adjustSignature st fplValue "("
            predicateAsts |> List.map (eval st) |> ignore
            adjustSignature st fplValue ")"
            fplValue.NameEndPos <- pos2
            evaluateConjunction fplValue
            emitLG000orLG001Diagnostics fplValue "conjunction"
        | _-> ()
        st.EvalPop()
    | Ast.Or((pos1, pos2), predicateAsts) ->
        st.EvalPush("Or")
        match st.CurrentContext with
        | EvalContext.InReferenceCreation fplValue -> 
            fplValue.FplRepresentation <- FplRepresentation.PredRepr FplPredicate.Undetermined
            adjustSignature st fplValue "or"
            adjustSignature st fplValue "("
            predicateAsts |> List.map (eval st) |> ignore
            adjustSignature st fplValue ")"
            fplValue.NameEndPos <- pos2
            evaluateDisjunction fplValue
            emitLG000orLG001Diagnostics fplValue "disjunction"
        | _-> ()
        st.EvalPop()
    | Ast.Xor((pos1, pos2), predicateAsts) ->
        st.EvalPush("Xor")
        match st.CurrentContext with
        | EvalContext.InReferenceCreation fplValue -> 
            setRepresentation st (FplRepresentation.PredRepr FplPredicate.Undetermined)
            adjustSignature st fplValue "xor"
            adjustSignature st fplValue "("
            predicateAsts |> List.map (eval st) |> ignore
            adjustSignature st fplValue ")"
            fplValue.NameEndPos <- pos2
            evaluateExclusiveOr fplValue
            emitLG000orLG001Diagnostics fplValue "exclusive-or"
        | _-> ()
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
        match st.CurrentContext with
        | EvalContext.InReferenceCreation fplValue -> 
            adjustSignature st fplValue "("
            asts |> List.map (eval st) |> ignore
            adjustSignature st fplValue ")"
        | _-> ()
        st.EvalPop()
    | Ast.QualificationList((pos1, pos2), asts) ->
        st.EvalPush("QualificationList")
        match st.CurrentContext with
        | EvalContext.InReferenceCreation fplValue -> 
            if asts.Length > 0 then
                asts |> List.map (eval st) |> ignore
                propagateReference fplValue true
        | _-> ()
        st.EvalPop()
    // | Namespace of Ast option * Ast list
    | Ast.Namespace(optAst, asts) ->
        st.EvalPush("Namespace")
        optAst |> Option.map (eval st) |> ignore
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
            match st.CurrentContext with 
            | EvalContext.NamedVarDeclarationInBlock fplValue 
            | EvalContext.InPropertySignature fplValue 
            | EvalContext.InConstructorSignature fplValue
            | EvalContext.InSignature fplValue ->
                adjustSignature st fplValue "->"
                fplValue.NameEndPos <- pos2
            | _ -> ()
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
        let oldContext = st.CurrentContext 
        match st.CurrentContext with 
        | EvalContext.InBlock fplValue 
        | EvalContext.InPropertyBlock fplValue 
        | EvalContext.InConstructorBlock fplValue -> 
            let refBlock = FplValue.CreateFplValue((pos1, pos2), FplValueType.Reference, fplValue) 
            st.SetContext(EvalContext.InReferenceCreation refBlock) LogContext.Start
            eval st fplIdentifierAst
            optionalSpecificationAst |> Option.map (eval st) |> ignore
        | EvalContext.InReferenceCreation fplValue ->
            match optionalSpecificationAst with
            | Some specificationAst -> 
                let refBlock = FplValue.CreateFplValue((pos1, pos2), FplValueType.Reference, fplValue) 
                st.SetContext(EvalContext.InReferenceCreation refBlock) LogContext.Replace
                eval st fplIdentifierAst
                eval st specificationAst |> ignore
                // forget refBlock but propagate its name and typesignature into its parent
                propagateReference refBlock false
                match tryMatchSignatures st refBlock with
                | (_, _, Some matchedFplValue) -> ()
                | (firstFailingArgument, candidates, None) -> 
                    emitSIG04Diagnostics refBlock candidates firstFailingArgument pos1 pos2 

            | None -> 
                // if no specification was found then simply continue in the same context
                eval st fplIdentifierAst
        | _ -> ()
        st.SetContext(oldContext) LogContext.End
        st.EvalPop()
    // | SelfAts of Positions * char list
    | Ast.SelfAts((pos1, pos2), chars) -> 
        st.EvalPush("SelfAts")
        let identifier = (chars |> List.map (fun c -> c.ToString()) |>  String.concat "") + "self"
        match st.CurrentContext with
        | EvalContext.InReferenceCreation fplValue ->
            adjustSignature st fplValue identifier
        | _ -> ()
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
        match st.CurrentContext with
        | EvalContext.InBlock fplValue
        | EvalContext.InPropertyBlock fplValue 
        | EvalContext.InConstructorBlock fplValue 
        | EvalContext.InReferenceCreation fplValue ->
            setRepresentation st (FplRepresentation.PredRepr FplPredicate.Undetermined)
            adjustSignature st fplValue "impl"
            adjustSignature st fplValue "("
            eval st predicateAst1
            eval st predicateAst2
            adjustSignature st fplValue ")"
            fplValue.NameEndPos <- pos2
            evaluateImplication fplValue
            emitLG000orLG001Diagnostics fplValue "implication"
        | _ -> ()
        st.EvalPop()
    | Ast.Iif((pos1, pos2), (predicateAst1, predicateAst2)) ->
        st.EvalPush("Iif")
        match st.CurrentContext with
        | EvalContext.InBlock fplValue
        | EvalContext.InPropertyBlock fplValue 
        | EvalContext.InConstructorBlock fplValue 
        | EvalContext.InReferenceCreation fplValue ->
            setRepresentation st (FplRepresentation.PredRepr FplPredicate.Undetermined)
            adjustSignature st fplValue "iif"
            adjustSignature st fplValue "("
            eval st predicateAst1
            eval st predicateAst2
            adjustSignature st fplValue ")"
            fplValue.NameEndPos <- pos2
            evaluateEquivalence fplValue
            emitLG000orLG001Diagnostics fplValue "equivalence"
        | _ -> ()
        st.EvalPop()
    | Ast.IsOperator((pos1, pos2), (isOpArgAst, variableTypeAst)) ->
        st.EvalPush("IsOperator")
        match st.CurrentContext with
        | EvalContext.InBlock fplValue
        | EvalContext.InPropertyBlock fplValue 
        | EvalContext.InConstructorBlock fplValue 
        | EvalContext.InReferenceCreation fplValue ->
            setRepresentation st (FplRepresentation.PredRepr FplPredicate.Undetermined)
            adjustSignature st fplValue "is"
            adjustSignature st fplValue "("
            eval st isOpArgAst
            eval st variableTypeAst
            adjustSignature st fplValue ")"

        | _ -> ()
        st.EvalPop()
    | Ast.Delegate((pos1, pos2), (fplDelegateIdentifierAst, argumentTupleAst)) ->
        st.EvalPush("Delegate")
        let oldContext = st.CurrentContext 
        match st.CurrentContext with 
        | EvalContext.InBlock fplValue 
        | EvalContext.InPropertyBlock fplValue 
        | EvalContext.InConstructorBlock fplValue 
        | EvalContext.InReferenceCreation fplValue ->
            let refBlock = FplValue.CreateFplValue((pos1, pos2), FplValueType.Reference, fplValue) 
            adjustSignature st refBlock "del."
            st.SetContext(EvalContext.InReferenceCreation refBlock) LogContext.Start
            eval st fplDelegateIdentifierAst
            eval st argumentTupleAst
            // forget refBlock but propagate its name and typesignature into its parent
            emitID013Diagnostics refBlock pos1 pos2 |> ignore
            propagateReference refBlock false 
        | _ -> ()
        st.SetContext(oldContext) LogContext.End
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
        match st.CurrentContext with
        | EvalContext.InSignature fplValue ->
            emitSIG00Diagnostics fplValue pos1 pos2
        | _ -> ()
        st.EvalPop()
    | Ast.PropertyBlock((pos1, pos2), (keywordPropertyAst, definitionPropertyAst)) ->
        st.EvalPush("PropertyBlock")
        let oldContext = st.CurrentContext
        match st.CurrentContext with
        | EvalContext.InBlock fplBlock -> 
            eval st keywordPropertyAst
            let fplValue = FplValue.CreateFplValue((pos1, pos2), FplValueType.MandatoryPredicate, fplBlock)
            st.SetContext(EvalContext.InPropertySignature fplValue) LogContext.Start
            eval st definitionPropertyAst
            tryAddBlock fplValue 
        | _ -> ()
        st.SetContext(oldContext) LogContext.End
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
    | Ast.Localization((pos1, pos2), (ast1, asts)) ->
        st.EvalPush("Localization")
        eval st ast1
        asts |> List.map (eval st) |> ignore
        st.EvalPop()
    | Ast.FunctionalTermInstance((pos1, pos2), (functionalTermSignatureAst, functionalTermInstanceBlockAst)) ->
        st.EvalPush("FunctionalTermInstance")
        eval st functionalTermSignatureAst
        let oldContext = st.CurrentContext 
        match st.CurrentContext with
        | EvalContext.InPropertySignature fplValue ->
            st.SetContext(EvalContext.InPropertyBlock fplValue) LogContext.Start
        | _ -> ()
        eval st functionalTermInstanceBlockAst
        st.SetContext(oldContext) LogContext.End
        st.EvalPop()
    // | All of Positions * ((Ast list * Ast option) list * Ast)
    | Ast.All((pos1, pos2), (variableListInOptDomainListAst, predicateAst)) ->
        st.EvalPush("All")
        match st.CurrentContext with
        | EvalContext.InBlock fplValue
        | EvalContext.InPropertyBlock fplValue 
        | EvalContext.InConstructorBlock fplValue 
        | EvalContext.InReferenceCreation fplValue ->
            st.SetContext(EvalContext.InQuantorCreation fplValue) LogContext.Replace
            fplValue.FplRepresentation <- FplRepresentation.PredRepr FplPredicate.Undetermined
            adjustSignature st fplValue "all"
            variableListInOptDomainListAst
            |> List.map (fun (asts, optAst) ->
                asts |> List.map (eval st) |> ignore
                optAst |> Option.map (eval st) |> Option.defaultValue ()
                ())
            |> ignore
            eval st predicateAst
            emitLG000orLG001Diagnostics fplValue "all quantor"
        | _ -> ()

        st.EvalPop()
    | Ast.Exists((pos1, pos2), (variableListInOptDomainListAst, predicateAst)) ->
        st.EvalPush("Exists")
        match st.CurrentContext with
        | EvalContext.InBlock fplValue
        | EvalContext.InPropertyBlock fplValue 
        | EvalContext.InConstructorBlock fplValue 
        | EvalContext.InReferenceCreation fplValue ->
            st.SetContext(EvalContext.InQuantorCreation fplValue) LogContext.Replace
            fplValue.FplRepresentation <- FplRepresentation.PredRepr FplPredicate.Undetermined
            adjustSignature st fplValue "ex"
            variableListInOptDomainListAst
            |> List.map (fun (asts, optAst) ->
                asts |> List.map (eval st) |> ignore
                optAst |> Option.map (eval st) |> Option.defaultValue ()
                ())
            |> ignore
            eval st predicateAst
            emitLG000orLG001Diagnostics fplValue "exists quantor"
        | _ -> ()
        st.EvalPop()
    // | ExistsN of Positions * ((Ast * (Ast * Ast option)) * Ast)
    | Ast.ExistsN((pos1, pos2), ((dollarDigitsAst, (variableAst, inOptDomainAst)), predicateAst)) ->
        st.EvalPush("ExistsN")
        match st.CurrentContext with
        | EvalContext.InBlock fplValue
        | EvalContext.InPropertyBlock fplValue 
        | EvalContext.InConstructorBlock fplValue 
        | EvalContext.InReferenceCreation fplValue ->
            st.SetContext(EvalContext.InQuantorCreation fplValue) LogContext.Replace
            fplValue.FplRepresentation <- FplRepresentation.PredRepr FplPredicate.Undetermined
            adjustSignature st fplValue "exn"
            eval st dollarDigitsAst
            eval st variableAst
            inOptDomainAst |> Option.map (eval st) |> Option.defaultValue () |> ignore
            eval st predicateAst
            emitLG000orLG001Diagnostics fplValue "exists n times quantor"
        | _ -> ()
        st.EvalPop()
    // | FunctionalTermSignature of Positions * (Ast * Ast)
    | Ast.FunctionalTermSignature((pos1, pos2), ((optAst, signatureWithUserDefinedStringAst), mappingAst)) -> 
        st.EvalPush("FunctionalTermSignature")
        eval st signatureWithUserDefinedStringAst
        match st.CurrentContext with 
        | EvalContext.InPropertySignature fplValue -> 
            match optAst with
            | Some ast1 -> 
                eval st ast1
                fplValue.BlockType <- FplValueType.OptionalFunctionalTerm
            | None -> 
                fplValue.BlockType <- FplValueType.MandatoryFunctionalTerm
            adjustSignature st fplValue "->"
            fplValue.NameEndPos <- pos2
        | EvalContext.InSignature fplValue -> 
            match optAst with
            | Some ast1 -> 
                eval st ast1
                fplValue.BlockType <- FplValueType.FunctionalTerm
            | None -> ()
            adjustSignature st fplValue "->"
            fplValue.NameEndPos <- pos2
        | _ -> ()
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
        let oldContext = st.CurrentContext 
        match st.CurrentContext with
        | EvalContext.InReferenceCreation fplValue -> 
            let dictOfOperators = Dictionary<string,FplValue>()
            separatedPredicateListAst
            |> List.map (fun (_, optSeparatorAst) -> 
                let infixOperator = FplValue.CreateFplValue((pos1,pos2),FplValueType.Reference,fplValue)
                st.SetContext(EvalContext.InInfixOperation infixOperator) LogContext.Start
                optSeparatorAst |> Option.map (eval st) |> Option.defaultValue ()
                st.SetContext(oldContext) LogContext.End
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
                                adjustSignature st fv symbol
                                adjustSignature st fv "("
                                eval st predicateAst 
                                if xs.Length > 0 then
                                    let nextInfixOperation = FplValue.CreateFplValue((p1,p2),FplValueType.Reference,fv)
                                    st.SetContext(EvalContext.InReferenceCreation nextInfixOperation) LogContext.Replace
                                    createReversedPolishNotation xs nextInfixOperation
                                
                                adjustSignature st fv ")"
                                propagateReference fv true
                        | _ -> ()
                    else
                        eval st predicateAst 
                        propagateReference fv true
                | [] -> ()

            createReversedPolishNotation sortedSeparatedPredicateListAst fplValue
        | _-> ()
        st.EvalPop()
    // | Expression of Positions * ((((Ast option * Ast) * Ast option) * Ast option) * Ast)
    | Ast.Expression((pos1, pos2), ((((prefixOpAst, predicateAst), postfixOpAst), optionalSpecificationAst), qualificationListAst)) ->
        st.EvalPush("Expression")
        let oldContext = st.CurrentContext 
        match st.CurrentContext with 
        | EvalContext.InBlock fplValue 
        | EvalContext.InPropertyBlock fplValue 
        | EvalContext.InConstructorBlock fplValue 
        | EvalContext.InReferenceCreation fplValue ->
            let refBlock = FplValue.CreateFplValue((pos1, pos2), FplValueType.Reference, fplValue) 
            st.SetContext(EvalContext.InReferenceCreation refBlock) LogContext.Start
            let ensureReversedPolishNotation = 
                if prefixOpAst.IsSome && postfixOpAst.IsSome then 
                    // for heuristic reasons, we choose a precedence of postfix ...
                    postfixOpAst |> Option.map (eval st) |> Option.defaultValue () 
                    adjustSignature st refBlock "("
                    // ... over prefix notation in mathematics
                    prefixOpAst |> Option.map (eval st) |> Option.defaultValue ()
                    adjustSignature st refBlock "("
                    eval st predicateAst
                    adjustSignature st refBlock ")"
                    adjustSignature st refBlock ")"
                elif prefixOpAst.IsSome then 
                    prefixOpAst |> Option.map (eval st) |> Option.defaultValue ()
                    adjustSignature st refBlock "("
                    eval st predicateAst
                    adjustSignature st refBlock ")"
                elif postfixOpAst.IsSome then 
                    postfixOpAst |> Option.map (eval st) |> Option.defaultValue ()
                    adjustSignature st refBlock "("
                    eval st predicateAst
                    adjustSignature st refBlock ")"
                else
                    eval st predicateAst
            ensureReversedPolishNotation
            optionalSpecificationAst |> Option.map (eval st) |> Option.defaultValue ()
            eval st qualificationListAst
            propagateReference refBlock true
            refBlock.NameIsFinal <- true
            refBlock.NameEndPos <- pos2
        | _ -> ()
        st.SetContext(oldContext) LogContext.End
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
        st.EvalPop()
    | Ast.Assignment((pos1, pos2), (ast1, ast2)) ->
        st.EvalPush("Assignment")
        eval st ast1
        eval st ast2
        st.EvalPop()
    | Ast.PredicateInstance((pos1, pos2), ((optAst, signatureAst), predInstanceBlockAst)) ->
        st.EvalPush("PredicateInstance")
        let oldContext = st.CurrentContext 
        eval st signatureAst
        match st.CurrentContext with
        | EvalContext.InPropertySignature fplValue ->
            setRepresentation st (FplRepresentation.PredRepr FplPredicate.Undetermined)
            st.SetContext(EvalContext.InPropertyBlock fplValue) LogContext.Start
            match optAst with
            | Some ast1 -> 
                eval st ast1
                fplValue.BlockType <- FplValueType.OptionalPredicate
            | None -> 
                fplValue.BlockType <- FplValueType.MandatoryPredicate
        | _ -> ()
        eval st predInstanceBlockAst
        st.SetContext(oldContext) LogContext.End
        st.EvalPop()
    | Ast.ParentConstructorCall((pos1, pos2), (inheritedClassTypeAst, argumentTupleAst)) ->
        st.EvalPush("ParentConstructorCall")
        let oldContext = st.CurrentContext 
        match st.CurrentContext with 
        | EvalContext.InConstructorBlock fplValue ->
            let refBlock = FplValue.CreateFplValue((pos1, pos2), FplValueType.Reference, fplValue) 
            refBlock.Name <- "bas."
            st.SetContext(EvalContext.InReferenceCreation refBlock) LogContext.Start
            eval st inheritedClassTypeAst
            eval st argumentTupleAst
            refBlock.NameIsFinal <- true
        | _ -> ()
        st.SetContext(oldContext) LogContext.End
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
        let oldContext = st.CurrentContext
        match st.CurrentContext with
        | EvalContext.InTheory theoryValue -> 
            let fplValue = FplValue.CreateFplValue((pos1, pos2), FplValueType.Theorem, theoryValue)
            st.SetContext(EvalContext.InSignature fplValue) LogContext.Start
            eval st signatureAst
            tryAddBlock fplValue 
            st.SetContext(EvalContext.InBlock fplValue) LogContext.Replace
            evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst
        | _ -> ()
        st.SetContext(oldContext) LogContext.End
        st.EvalPop()
    | Ast.Lemma((pos1, pos2), (signatureAst, (optVarDeclOrSpecList, predicateAst))) ->
        st.EvalPush("Lemma")
        let oldContext = st.CurrentContext
        match st.CurrentContext with
        | EvalContext.InTheory theoryValue -> 
            let fplValue = FplValue.CreateFplValue((pos1, pos2), FplValueType.Lemma, theoryValue)
            st.SetContext(EvalContext.InSignature fplValue) LogContext.Start
            eval st signatureAst
            tryAddBlock fplValue 
            st.SetContext(EvalContext.InBlock fplValue) LogContext.Replace
            evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst
        | _ -> ()
        st.SetContext(oldContext) LogContext.End
        st.EvalPop()
    | Ast.Proposition((pos1, pos2), (signatureAst, (optVarDeclOrSpecList, predicateAst))) ->
        st.EvalPush("Proposition")
        let oldContext = st.CurrentContext
        match st.CurrentContext with
        | EvalContext.InTheory theoryValue -> 
            let fplValue = FplValue.CreateFplValue((pos1, pos2), FplValueType.Proposition, theoryValue)
            st.SetContext(EvalContext.InSignature fplValue) LogContext.Start
            eval st signatureAst
            tryAddBlock fplValue 
            st.SetContext(EvalContext.InBlock fplValue) LogContext.Replace
            evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst
        | _ -> ()
        st.SetContext(oldContext) LogContext.End
        st.EvalPop()
    | Ast.Conjecture((pos1, pos2), (signatureAst, (optVarDeclOrSpecList, predicateAst))) ->
        st.EvalPush("Conjecture")
        let oldContext = st.CurrentContext
        match st.CurrentContext with
        | EvalContext.InTheory theoryValue -> 
            let fplValue = FplValue.CreateFplValue((pos1, pos2), FplValueType.Conjecture, theoryValue)
            st.SetContext(EvalContext.InSignature fplValue) LogContext.Start
            eval st signatureAst
            tryAddBlock fplValue 
            st.SetContext(EvalContext.InBlock fplValue) LogContext.Replace
            evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst
        | _ -> ()
        st.SetContext(oldContext) LogContext.End
        st.EvalPop()
    | Ast.Axiom((pos1, pos2), (signatureAst, (optVarDeclOrSpecList, predicateAst))) ->
        st.EvalPush("Axiom")
        let oldContext = st.CurrentContext
        match st.CurrentContext with
        | EvalContext.InTheory theoryValue -> 
            let fplValue = FplValue.CreateFplValue((pos1, pos2), FplValueType.Axiom, theoryValue)
            st.SetContext(EvalContext.InSignature fplValue) LogContext.Start
            eval st signatureAst
            tryAddBlock fplValue 
            st.SetContext(EvalContext.InBlock fplValue) LogContext.Replace
            evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst
        | _ -> ()
        st.SetContext(oldContext) LogContext.End
        st.EvalPop()
    // | Corollary of Positions * ((Ast * Ast) * (Ast list option * Ast))
    | Ast.CorollarySignature(referencingIdentifierAst, paramTupleAst) ->
        st.EvalPush("CorollarySignature")
        eval st referencingIdentifierAst
        eval st paramTupleAst
        st.EvalPop()
    | Ast.Corollary((pos1, pos2), (corollarySignatureAst, (optVarDeclOrSpecList, predicateAst))) ->
        st.EvalPush("Corollary")
        let oldContext = st.CurrentContext
        match st.CurrentContext with
        | EvalContext.InTheory theoryValue -> 
            let fplValue = FplValue.CreateFplValue((pos1, pos2), FplValueType.Corollary, theoryValue)
            st.SetContext(EvalContext.InSignature fplValue) LogContext.Start
            eval st corollarySignatureAst
            tryAddBlock fplValue 
            st.SetContext(EvalContext.InBlock fplValue) LogContext.Replace
            evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst
        | _ -> ()
        st.SetContext(oldContext) LogContext.End
        st.EvalPop()
    // | NamedVarDecl of Positions * ((Ast list * Ast) * Ast)
    | Ast.NamedVarDecl((pos1, pos2), ((variableListAst, varDeclModifierAst), variableTypeAst)) ->
        st.EvalPush("NamedVarDecl")
        let oldContext = st.CurrentContext

        let evalNamedVarDecl (fplValue:FplValue) (context:string) = 
            fplValue.AuxiliaryInfo <- variableListAst |> List.length // remember how many variables to create
            eval st varDeclModifierAst
            fplValue.Scope 
            |> Seq.toList // Make this collection immutable since it might be changed in the below iteration
            |> List.filter (fun varKeyValue -> FplValue.IsVariable(varKeyValue.Value))
            |> List.iter (fun childKeyValue -> 
                if not (childKeyValue.Value.Parent.Value.AuxiliaryUniqueChilds.Contains(childKeyValue.Value.Name)) then 
                    if context = "NamedVarDeclarationInBlock" then 
                        st.SetContext(EvalContext.NamedVarDeclarationInBlock (childKeyValue.Value)) LogContext.Replace
                    elif context = "InPropertySignature" then
                        st.SetContext(EvalContext.InPropertySignature (childKeyValue.Value)) LogContext.Replace
                    elif context = "InConstructorSignature" then
                        st.SetContext(EvalContext.InConstructorSignature (childKeyValue.Value)) LogContext.Replace
                    elif context = "InSignature" then
                        st.SetContext(EvalContext.InSignature (childKeyValue.Value)) LogContext.Replace
                    else
                        raise (ArgumentException(sprintf "Unknown context %s" context))
                    eval st variableTypeAst
                    st.SetContext(oldContext) LogContext.End
                    childKeyValue.Value.Parent.Value.AuxiliaryUniqueChilds.Add(childKeyValue.Value.Name) |> ignore
            )

        // create all variables of the named variable declaration in the current scope
        match st.CurrentContext with
        | EvalContext.InBlock fplValue 
        | EvalContext.InConstructorBlock fplValue
        | EvalContext.InPropertyBlock fplValue ->
            st.SetContext(EvalContext.NamedVarDeclarationInBlock (fplValue)) LogContext.Start
        | _ -> ()
        variableListAst |> List.map (eval st) |> ignore 

        match st.CurrentContext with 
        | EvalContext.InBlock fplValue
        | EvalContext.InConstructorBlock fplValue
        | EvalContext.InPropertyBlock fplValue
        | EvalContext.InReferenceCreation fplValue
        | EvalContext.NamedVarDeclarationInBlock fplValue ->
            evalNamedVarDecl fplValue "NamedVarDeclarationInBlock"
        | EvalContext.InPropertySignature fplValue -> 
            evalNamedVarDecl fplValue "InPropertySignature"
        | EvalContext.InConstructorSignature fplValue -> 
            evalNamedVarDecl fplValue "InConstructorSignature"
        | EvalContext.InSignature fplValue -> 
            evalNamedVarDecl fplValue "InSignature"
        | _ -> ()
        st.SetContext(oldContext) LogContext.End
        st.EvalPop()
    // | Axiom of Constructor * (Ast * (Ast list option * Ast))
    | Ast.Constructor((pos1, pos2), (signatureAst, (optVarDeclOrSpecListAst, keywordSelfAst))) ->
        st.EvalPush("Constructor")
        let oldContext = st.CurrentContext
        match st.CurrentContext with
        | EvalContext.InBlock classBlock -> 
            let fplValue = FplValue.CreateFplValue((pos1, pos2), FplValueType.Constructor, classBlock)
            st.SetContext(EvalContext.InConstructorSignature fplValue) LogContext.Start
            eval st signatureAst
            tryAddBlock fplValue
            st.SetContext(EvalContext.InConstructorBlock fplValue) LogContext.Replace
            match optVarDeclOrSpecListAst with
            | Some astList -> astList |> List.map (eval st) |> ignore
            | None -> ()
            eval st keywordSelfAst
        | _ -> ()
        st.SetContext(oldContext) LogContext.End
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
    | Ast.DefClassCompleteContent(optAsts, asts) ->
        st.EvalPush("DefClassCompleteContent")
        optAsts |> Option.map (List.map (eval st) >> ignore) |> Option.defaultValue ()
        asts |> List.map (eval st) |> ignore
        st.EvalPop()
    // | DefinitionPredicate of Positions * (Ast * (Ast * Ast list option))
    | Ast.DefinitionPredicate((pos1, pos2), (signatureWithUserDefinedStringAst, (predicateContentAst, optPropertyListAsts))) ->
        st.EvalPush("DefinitionPredicate")
        let oldContext = st.CurrentContext
        match st.CurrentContext with
        | EvalContext.InTheory theoryValue -> 
            let fplValue = FplValue.CreateFplValue((pos1, pos2), FplValueType.Predicate, theoryValue)
            st.SetContext(EvalContext.InSignature fplValue) LogContext.Start
            eval st signatureWithUserDefinedStringAst
            tryAddBlock fplValue 
            st.SetContext(EvalContext.InBlock fplValue) LogContext.Replace
            eval st predicateContentAst
            optPropertyListAsts |> Option.map (List.map (eval st) >> ignore) |> Option.defaultValue ()
        | _ -> ()
        st.SetContext(oldContext) LogContext.End
        st.EvalPop()
    // | DefinitionFunctionalTerm of Positions * ((Ast * Ast) * (Ast * Ast list option))
    | Ast.DefinitionFunctionalTerm((pos1, pos2), (functionalTermSignatureAst, (funcContentAst, optPropertyListAsts))) ->
        st.EvalPush("DefinitionFunctionalTerm")
        let oldContext = st.CurrentContext
        match st.CurrentContext with
        | EvalContext.InTheory theoryValue -> 
            let fplValue = FplValue.CreateFplValue((pos1, pos2), FplValueType.FunctionalTerm, theoryValue)
            st.SetContext(EvalContext.InSignature fplValue) LogContext.Start
            eval st functionalTermSignatureAst
            tryAddBlock fplValue 
            st.SetContext(EvalContext.InBlock fplValue) LogContext.Replace
            eval st funcContentAst
            optPropertyListAsts |> Option.map (List.map (eval st) >> ignore) |> Option.defaultValue ()
        | _ -> ()
        st.SetContext(oldContext) LogContext.End
        st.EvalPop()
    // | DefinitionClass of Positions * (((Ast * Ast option) * Ast list) * (Ast * Ast list option))
    | Ast.DefinitionClass((pos1, pos2),
                          (((predicateIdentifierAst, optUserDefinedObjSymAst), classTypeListAsts),
                           (classContentAst, optPropertyListAsts))) ->
        st.EvalPush("DefinitionClass")
        let oldContext = st.CurrentContext
        match st.CurrentContext with
        | EvalContext.InTheory theoryValue -> 
            let fplValue = FplValue.CreateFplValue((pos1, pos2), FplValueType.Class, theoryValue)
            st.SetContext(EvalContext.InSignature fplValue) LogContext.Start
            eval st predicateIdentifierAst
            tryAddBlock fplValue 
            optUserDefinedObjSymAst |> Option.map (eval st) |> Option.defaultValue ()
            classTypeListAsts |> List.map (eval st) |> ignore
            st.SetContext(EvalContext.InBlock fplValue) LogContext.Replace
            eval st classContentAst
            optPropertyListAsts
            |> Option.map (List.map (eval st) >> ignore)
            |> Option.defaultValue ()
        | _ -> ()
        st.SetContext(oldContext) LogContext.End
        st.EvalPop()
    // | DerivedPredicate of Ast
    | Ast.DerivedPredicate ast1 -> 
        st.EvalPush("DefinitionClass")
        eval st ast1
        st.EvalPop()
    // | Proof of Positions * (Ast * (Ast list * Ast option))
    | Ast.Proof((pos1, pos2), (referencingIdentifierAst, (proofArgumentListAst, optQedAst))) ->
        st.EvalPush("Proof")
        let oldContext = st.CurrentContext
        match st.CurrentContext with
        | EvalContext.InTheory theoryValue -> 
            let fplValue = FplValue.CreateFplValue((pos1, pos2), FplValueType.Proof, theoryValue)
            st.SetContext(EvalContext.InSignature fplValue) LogContext.Start
            eval st referencingIdentifierAst
            tryAddBlock fplValue 
            st.SetContext(EvalContext.InBlock fplValue) LogContext.Replace
            proofArgumentListAst |> List.map (eval st) |> ignore
            optQedAst |> Option.map (eval st) |> Option.defaultValue ()
        | _ -> ()
        st.SetContext(oldContext) LogContext.End
        st.EvalPop()
    | Ast.Precedence((pos1, pos2), precedence) ->
        st.EvalPush("Precedence")
        match st.CurrentContext with
        | EvalContext.InSignature fplValue -> 
            fplValue.AuxiliaryInfo <- precedence
        | _ -> ()
        st.EvalPop()
    | ast1 ->
        let astType = ast1.GetType().Name
        emitID000Diagnostics astType


let tryFindParsedAstUsesClausesEvaluated (parsedAsts: List<ParsedAst>) =
    if parsedAsts.Exists(fun pa -> pa.Status = ParsedAstStatus.UsesClausesEvaluated) then
        Some(parsedAsts.Find(fun pa -> pa.Status = ParsedAstStatus.UsesClausesEvaluated))
    else
        None

let evaluateSymbolTable (uri:System.Uri) (st: SymbolTable) =
    st.OrderAsts()

    let mutable found = true

    while found do
        let usesClausesEvaluatedParsedAst =
            tryFindParsedAstUsesClausesEvaluated st.ParsedAsts

        let oldContext = st.CurrentContext
        match usesClausesEvaluatedParsedAst with
        | Some pa ->
            // evaluate the ParsedAst
            let theoryValue = FplValue.CreateFplValue((Position("",0,1,1), Position("",0,1,1)), FplValueType.Theory, st.Root)
            if not (st.Root.Scope.ContainsKey(pa.Id)) then
                st.Root.Scope.Add(pa.Id, theoryValue)
            theoryValue.Name <- pa.Id
            st.SetContext(EvalContext.InTheory theoryValue) LogContext.Start
            ad.CurrentUri <- pa.Parsing.Uri
            eval st pa.Parsing.Ast
            pa.Status <- ParsedAstStatus.Evaluated
            theoryValue.NameIsFinal <- true
            st.SetContext(oldContext) LogContext.End
        | None -> found <- false
