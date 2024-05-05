module FplInterpreterBuildingBlocks

open System
open System.Collections.Generic
open FParsec
open ErrDiagnostics
open FplGrammarTypes
open FplInterpreterTypes
open FplInterpreterDiagnosticsEmitter

let rec adjustSignature (st:SymbolTable) (fplValue:FplValue) str = 
    if str <> "" && not (FplValue.IsVariable(fplValue)) then
        if FplValue.IsDefinition(fplValue) && fplValue.NameIsFinal then 
            () // for definitions with final name stop changing the name
        else
            // note: the Name attribute of variables are set in Ast.Var directly
            // and we do not want to append the type to the names of variables.
            if str = "(" || str = ")" 
                || str = "[" || str = "]" 
                || str = "->"
                || fplValue.Name.EndsWith "(" 
                || fplValue.Name.EndsWith "[" 
                || fplValue.Name.Length = 0 
                || fplValue.Name.EndsWith "-> " 
                || str.StartsWith "$" then
                if str = "->" then 
                    fplValue.Name <- fplValue.Name + " " + str + " "
                else
                    fplValue.Name <- fplValue.Name + str
            else
                fplValue.Name <- fplValue.Name + ", " + str

    if str <> "" then
        if FplValue.IsDefinition(fplValue) && fplValue.NameIsFinal then  
            () //  for definitions with final name stop changing the TypeSignature
        else
            // note: the manipulation of the TypeSignature is necessary for all kinds of fplValue
            if str.StartsWith("*") then
                fplValue.TypeSignature <- fplValue.TypeSignature @ ["*"; str.Substring(1)]
            elif str.StartsWith("+") then
                fplValue.TypeSignature <- fplValue.TypeSignature @ ["+"; str.Substring(1)]
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

let eval_units (st: SymbolTable) unitType (uri:Uri) pos1 pos2 = 
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
                checkID009_ID010_ID011_Diagnostics st fplValue unitType uri pos1 pos2
    | EvalContext.InConstructorBlock fplValue ->
        checkID012Diagnostics fplValue unitType uri pos1 pos2
    | _ -> ()

let eval_string (st: SymbolTable) s = ()

let eval_pos_string (st: SymbolTable) (startpos: Position) (endpos: Position) ast = ()

let eval_pos_unit (st: SymbolTable) (startpos: Position) (endpos: Position) = ()

let eval_pos_ast (st: SymbolTable) (startpos: Position) (endpos: Position) = ()

let eval_pos_ast_ast_opt (st: SymbolTable) (startpos: Position) (endpos: Position) = ()

let eval_pos_char_list (st: SymbolTable) (startpos: Position) (endpos: Position) charlist =
    charlist |> List.map string |> String.concat "" |> ignore

let eval_pos_string_ast (st: SymbolTable) str = ()

let tryAddBlock (uri:Uri) (fplValue:FplValue) =

    match FplValue.TryFindAssociatedBlockForProof fplValue with
    | ScopeSearchResult.FoundAssociate parentsName -> 
        // everything is ok, change the parent of the provable from theory to the found parent 
        fplValue.Parent <- Some fplValue.Parent.Value.Scope[parentsName]
    | ScopeSearchResult.FoundIncorrectBlock block ->
        emitID002diagnostics fplValue block uri 
    | ScopeSearchResult.NotFound ->
        emitID003diagnostics fplValue uri 
    | ScopeSearchResult.FoundMultiple listOfKandidates ->
        emitID004diagnostics fplValue listOfKandidates uri 
    | _ -> ()

    match FplValue.TryFindAssociatedBlockForCorollary fplValue with
    | ScopeSearchResult.FoundAssociate parentsName -> 
        // everything is ok, change the parent of the provable from theory to the found parent 
        fplValue.Parent <- Some fplValue.Parent.Value.Scope[parentsName]
        // now, we are ready to emit VAR03 diagnostics for all variables declared in the signature of the corollary.
        emitVAR03diagnosticsForCorollarysSignatureVariale fplValue uri 
    | ScopeSearchResult.FoundIncorrectBlock block ->
        emitID005diagnostics fplValue block uri 
    | ScopeSearchResult.NotFound ->
        emitID006diagnostics fplValue uri 
    | ScopeSearchResult.FoundMultiple listOfKandidates ->
        emitID007diagnostics fplValue listOfKandidates uri 
    | _ -> ()

    match FplValue.InScopeOfParent(fplValue) fplValue.Name with
    | ScopeSearchResult.Found conflict -> 
        emitVAR01orID001diagnostics fplValue conflict uri
    | _ -> 
        match FplValue.ConstructorOrPropertyVariableInOuterScope(fplValue) with
        | ScopeSearchResult.Found other ->
            emitVAR02diagnostics fplValue other uri 
        | _ -> 
            match FplValue.ProofVariableInOuterScope(fplValue) with
            | ScopeSearchResult.Found other ->
                emitVAR03diagnostics fplValue other uri
            | _ -> 
                fplValue.Parent.Value.Scope.Add(fplValue.Name,fplValue)
                fplValue.NameIsFinal <- true

/// A recursive function evaluating an AST and returning a list of EvalAliasedNamespaceIdentifier records
/// for each occurrence of the uses clause in the FPL code.
let rec eval (uri:System.Uri) (st: SymbolTable) ast =
    let evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst =
        match optVarDeclOrSpecList with
        | Some astList -> astList |> List.map (eval uri st) |> ignore
        | None -> ()
        eval uri st predicateAst

    let evalMany (st:SymbolTable) blockType pos1 pos2 = 
        match st.CurrentContext with
        | EvalContext.NamedVarDeclarationInBlock fplValue
        | EvalContext.InPropertySignature fplValue
        | EvalContext.InConstructorSignature fplValue
        | EvalContext.InSignature fplValue -> 
            tryAddVariadicVariables uri fplValue.AuxiliaryInfo pos1 pos2
            // adjust type of variables to variadic variables, if their type has not yet been established
            fplValue.Scope
            |> Seq.filter (fun varKeyValue -> FplValue.IsVariable(varKeyValue.Value) && varKeyValue.Value.TypeSignature = [])
            |> Seq.iter (fun varKeyValue -> 
                varKeyValue.Value.BlockType <- blockType
            )
        | _ -> ()

    let correctFplTypeOfFunctionalTerms fplType = 
        match st.CurrentContext with
        | EvalContext.InSignature fplValue 
        | EvalContext.InPropertySignature fplValue ->
            if FplValue.IsFunctionalTerm(fplValue) then
                fplValue.EvaluationType <- fplType
        | _ -> ()
        

    match ast with
    // units: | Star
    | Ast.IndexType((pos1, pos2),()) -> 
        st.EvalPush("IndexType")
        eval_units st "ind" uri pos1 pos2 
        correctFplTypeOfFunctionalTerms FplType.Index
        st.EvalPop() |> ignore
    | Ast.ObjectType((pos1, pos2),()) -> 
        st.EvalPush("ObjectType")
        eval_units st "obj" uri pos1 pos2 
        correctFplTypeOfFunctionalTerms FplType.Object
        st.EvalPop()
    | Ast.PredicateType((pos1, pos2),()) -> 
        st.EvalPush("PredicateType")
        eval_units st "pred" uri pos1 pos2 
        st.EvalPop()
    | Ast.FunctionalTermType((pos1, pos2),()) -> 
        st.EvalPush("FunctionalTermType")
        eval_units st "func" uri pos1 pos2  
        correctFplTypeOfFunctionalTerms FplType.FunctionalTerm
        st.EvalPop()
    | Ast.Many((pos1, pos2),()) ->
        st.EvalPush("Many")
        evalMany st FplBlockType.VariadicVariableMany pos1 pos2
        st.EvalPop()
    | Ast.Many1((pos1, pos2),()) ->
        st.EvalPush("Many1")
        evalMany st FplBlockType.VariadicVariableMany1 pos1 pos2
        st.EvalPop()
    | Ast.One((pos1, pos2),()) ->
        st.EvalPush("One")
        eval_units st "" uri pos1 pos2  
        st.EvalPop()
    | Ast.Star((pos1, pos2),()) ->
        st.EvalPush("Star")
        eval_units st "" uri pos1 pos2  
        st.EvalPop()
    | Ast.Dot((pos1, pos2),()) ->
        st.EvalPush("Dot")
        eval_units st "" uri pos1 pos2  
        st.EvalPop()
    | Ast.Intrinsic((pos1, pos2),()) -> 
        st.EvalPush("Intrinsic")
        eval_units st "" uri pos1 pos2  
        st.EvalPop()
    | Ast.Property((pos1, pos2),()) -> 
        st.EvalPush("Property")
        eval_units st "" uri pos1 pos2  
        st.EvalPop()
    | Ast.Optional((pos1, pos2),()) -> 
        st.EvalPush("Optional")
        eval_units st "" uri pos1 pos2  
        st.EvalPop()
    | Ast.Error  ->   
        st.EvalPush("Error")
        let pos = Position("",0,1,1)
        eval_units st "" uri pos pos
        st.EvalPop()
    // strings: | Digits of string
    | Ast.Digits s -> 
        st.EvalPush("Digits")
        eval_string st s
        st.EvalPop()
    | Ast.PascalCaseId s -> 
        st.EvalPush("PascalCaseId")
        eval_string st s
        st.EvalPop() 
    | Ast.ExtensionRegex s -> 
        st.EvalPush("ExtensionRegex")
        eval_string st s
        st.EvalPop() 
    // | DollarDigits of Positions * string
    | Ast.DollarDigits((pos1, pos2), s) -> 
        st.EvalPush("DollarDigits")
        match st.CurrentContext with
        | EvalContext.NamedVarDeclarationInBlock fplValue 
        | EvalContext.InPropertySignature fplValue 
        | EvalContext.InConstructorSignature fplValue
        | EvalContext.InSignature fplValue ->
            adjustSignature st fplValue s
            fplValue.NameEndPos <- pos2 // the full name ends where the dollar digits end 
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
            if (FplValue.IsVariadicVariableMany(fplValue)) then 
                adjustSignature st fplValue ("*" + s)
            elif (FplValue.IsVariadicVariableMany1(fplValue)) then 
                adjustSignature st fplValue ("+" + s)
            else
                adjustSignature st fplValue s
            correctFplTypeOfFunctionalTerms FplType.Template
        | _ -> ()
        st.EvalPop() 
    | Ast.Var((pos1, pos2), s) ->
        st.EvalPush("Var")
        match st.CurrentContext with
        | EvalContext.NamedVarDeclarationInBlock fplValue 
        | EvalContext.InPropertySignature fplValue 
        | EvalContext.InConstructorSignature fplValue
        | EvalContext.InSignature fplValue -> 
            let varValue = FplValue.CreateFplValue((pos1,pos2), FplBlockType.Variable, fplValue)
            varValue.Name <- s
            varValue.NameEndPos <- pos2
            tryAddBlock uri varValue 
            
        | _ -> ()
        st.EvalPop() 
    | Ast.DelegateId((pos1, pos2), s) -> 
        st.EvalPush("DelegateId")
        eval_pos_string st pos1 pos2 s
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
        eval_pos_string st pos1 pos2 s
        st.EvalPop() 
    | Ast.ArgumentIdentifier((pos1, pos2), s) -> 
        st.EvalPush("ArgumentIdentifier")
        eval_pos_string st pos1 pos2 s
        st.EvalPop() 
    | Ast.Prefix((pos1, pos2), s) -> 
        st.EvalPush("Prefix")
        eval_pos_string st pos1 pos2 s
        st.EvalPop() 
    | Ast.Infix((pos1, pos2), s) -> 
        st.EvalPush("Infix")
        eval_pos_string st pos1 pos2 s
        st.EvalPop() 
    | Ast.Postfix((pos1, pos2), s) -> 
        st.EvalPush("Postfix")
        eval_pos_string st pos1 pos2 s
        st.EvalPop() 
    | Ast.Symbol((pos1, pos2), s) -> 
        st.EvalPush("Symbol")
        eval_pos_string st pos1 pos2 s
        st.EvalPop() 
    | Ast.InfixOperator((pos1, pos2), s) -> 
        st.EvalPush("InfixOperator")
        eval_pos_string st pos1 pos2 s
        st.EvalPop() 
    | Ast.PostfixOperator((pos1, pos2), s) -> 
        st.EvalPush("PostfixOperator")
        eval_pos_string st pos1 pos2 s
        st.EvalPop() 
    | Ast.PrefixOperator((pos1, pos2), s) -> 
        st.EvalPush("PrefixOperator")
        eval_pos_string st pos1 pos2 s
        st.EvalPop() 
    // | Self of Positions * unit
    | Ast.Self((pos1, pos2), _) -> 
        st.EvalPush("Self")
        eval_pos_unit st pos1 pos2
        st.EvalPop() 
    | Ast.True((pos1, pos2), _) -> 
        st.EvalPush("True")
        eval_pos_unit st pos1 pos2
        st.EvalPop() 
    | Ast.False((pos1, pos2), _) -> 
        st.EvalPush("False")
        eval_pos_unit st pos1 pos2
        st.EvalPop() 
    | Ast.Undefined((pos1, pos2), _) -> 
        st.EvalPush("Undefined")
        eval_pos_unit st pos1 pos2
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
            let fplValue = FplValue.CreateFplValue((pos1, pos2), FplBlockType.RuleOfInference, theoryValue)
            st.CurrentContext <- EvalContext.InSignature fplValue
            eval uri st signatureAst
            st.CurrentContext <- EvalContext.InBlock fplValue
            eval uri st premiseConclusionBlockAst
            tryAddBlock uri fplValue 
        | _ -> ()
        st.CurrentContext <- oldContext
        st.EvalPop() 
    | Ast.ClassIdentifier((pos1, pos2), ast1) ->
        st.EvalPush("ClassIdentifier")
        eval uri st ast1
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
        eval uri st ast1
        eval_pos_ast st pos1 pos2
        st.EvalPop()
    | Ast.ExtensionType((pos1, pos2), ast1) ->
        st.EvalPush("ExtensionType")
        eval uri st ast1
        eval_pos_ast st pos1 pos2
        st.EvalPop()
    | Ast.UsesClause((pos1, pos2), ast1) ->
        st.EvalPush("UsesClause")
        eval uri st ast1
        eval_pos_ast st pos1 pos2
        st.EvalPop()
    | Ast.Not((pos1, pos2), ast1) ->
        st.EvalPush("Not")
        eval uri st ast1
        eval_pos_ast st pos1 pos2
        st.EvalPop()
    | Ast.InEntity((pos1, pos2), ast1) ->
        st.EvalPush("InEntity")
        eval uri st ast1
        eval_pos_ast st pos1 pos2
        st.EvalPop()
    | Ast.IsType((pos1, pos2), ast1) ->
        st.EvalPush("IsType")
        eval uri st ast1
        eval_pos_ast st pos1 pos2
        st.EvalPop()
    | Ast.Assertion((pos1, pos2), ast1) ->
        st.EvalPush("Assertion")
        eval uri st ast1
        eval_pos_ast st pos1 pos2
        st.EvalPop()
    | Ast.ByDef((pos1, pos2), ast1) ->
        st.EvalPush("ByDef")
        eval uri st ast1
        eval_pos_ast st pos1 pos2
        st.EvalPop()
    | Ast.DottedPredicate((pos1, pos2), ast1) ->
        st.EvalPush("DottedPredicate")
        eval uri st ast1
        eval_pos_ast st pos1 pos2
        st.EvalPop()
    | Ast.Return((pos1, pos2), ast1) ->
        st.EvalPush("Return")
        eval uri st ast1
        eval_pos_ast st pos1 pos2
        st.EvalPop()
    | Ast.AssumeArgument((pos1, pos2), ast1) ->
        st.EvalPush("AssumeArgument")
        eval uri st ast1
        eval_pos_ast st pos1 pos2
        st.EvalPop()
    | Ast.RevokeArgument((pos1, pos2), ast1) ->
        st.EvalPush("RevokeArgument")
        eval uri st ast1
        eval_pos_ast st pos1 pos2
        st.EvalPop()
    | Ast.VariableType((pos1, pos2), ast1) ->
        st.EvalPush("VariableType")
        eval uri st ast1
        eval_pos_ast st pos1 pos2
        st.EvalPop()
    | Ast.AST((pos1, pos2), ast1) ->
        st.EvalPush("AST")
        eval uri st ast1
        eval_pos_ast st pos1 pos2
        st.EvalPop()
    // | NamespaceIdentifier of Positions * Ast list
    | Ast.PredicateIdentifier((pos1, pos2), asts) ->
        st.EvalPush("PredicateIdentifier")

        let pascalCaseIdList = asts |> List.collect (function Ast.PascalCaseId s -> [s] | _ -> [])
        let identifier = String.concat "." pascalCaseIdList
        match st.CurrentContext with
        | EvalContext.InTheory fplValue
        | EvalContext.NamedVarDeclarationInBlock fplValue
        | EvalContext.InPropertySignature fplValue 
        | EvalContext.InConstructorSignature fplValue 
        | EvalContext.InSignature fplValue -> 
            if (FplValue.IsVariadicVariableMany(fplValue)) then 
                adjustSignature st fplValue ("*" + identifier)
            elif (FplValue.IsVariadicVariableMany1(fplValue)) then 
                adjustSignature st fplValue ("+" + identifier)
            else
                adjustSignature st fplValue identifier
            correctFplTypeOfFunctionalTerms FplType.Object
            checkID008Diagnostics fplValue uri pos1 pos2
            checkID009_ID010_ID011_Diagnostics st fplValue identifier uri pos1 pos2
        | EvalContext.InConstructorBlock fplValue ->
            checkID012Diagnostics fplValue identifier uri pos1 pos2
        | _ -> ()
        st.EvalPop()
    | Ast.ParamTuple((pos1, pos2), asts) ->
        st.EvalPush("ParamTuple")
        match st.CurrentContext with
        | EvalContext.NamedVarDeclarationInBlock fplValue 
        | EvalContext.InPropertySignature fplValue 
        | EvalContext.InConstructorSignature fplValue
        | EvalContext.InSignature fplValue -> 
            adjustSignature st fplValue "("
            asts |> List.map (eval uri st) |> ignore
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
                eval uri st ast1
            ) |> ignore
            adjustSignature st fplValue "]"
            fplValue.NameEndPos <- pos2
        | _ -> ()
        st.EvalPop()
    | Ast.NamespaceIdentifier((pos1, pos2), asts) ->
        st.EvalPush("NamespaceIdentifier")
        asts |> List.map (eval uri st) |> ignore
        st.EvalPop()
    | Ast.LocalizationTerm((pos1, pos2), asts) ->
        st.EvalPush("LocalizationTerm")
        asts |> List.map (eval uri st) |> ignore
        st.EvalPop()
    | Ast.LocalizationTermList((pos1, pos2), asts) ->
        st.EvalPush("LocalizationTermList")
        asts |> List.map (eval uri st) |> ignore
        st.EvalPop()
    | Ast.BrackedCoordList((pos1, pos2), asts) ->
        st.EvalPush("BrackedCoordList")
        asts |> List.map (eval uri st) |> ignore
        st.EvalPop()
    | Ast.And((pos1, pos2), asts) ->
        st.EvalPush("And")
        asts |> List.map (eval uri st) |> ignore
        st.EvalPop()
    | Ast.Or((pos1, pos2), asts) ->
        st.EvalPush("Or")
        asts |> List.map (eval uri st) |> ignore
        st.EvalPop()
    | Ast.Xor((pos1, pos2), asts) ->
        st.EvalPush("Xor")
        asts |> List.map (eval uri st) |> ignore
        st.EvalPop()
    | Ast.VarDeclBlock((pos1, pos2), asts) ->
        st.EvalPush("VarDeclBlock")
        asts |> List.map (eval uri st) |> ignore
        st.EvalPop()
    | Ast.StatementList((pos1, pos2), asts) ->
        st.EvalPush("StatementList")
        asts |> List.map (eval uri st) |> ignore
        st.EvalPop()
    | Ast.DefaultResult((pos1, pos2), asts) ->
        st.EvalPush("DefaultResult")
        asts |> List.map (eval uri st) |> ignore
        st.EvalPop()
    | Ast.Justification((pos1, pos2), asts) ->
        st.EvalPush("Justification")
        asts |> List.map (eval uri st) |> ignore
        st.EvalPop()
    | Ast.ArgumentTuple((pos1, pos2), asts) ->
        st.EvalPush("ArgumentTuple")
        asts |> List.map (eval uri st) |> ignore
        st.EvalPop()
    | Ast.QualificationList((pos1, pos2), asts) ->
        st.EvalPush("QualificationList")
        asts |> List.map (eval uri st) |> ignore
        st.EvalPop()
    // | Namespace of Ast option * Ast list
    | Ast.Namespace(optAst, asts) ->
        st.EvalPush("Namespace")
        optAst |> Option.map (eval uri st) |> ignore
        asts |> List.map (eval uri st) |> ignore
        st.EvalPop()
    // CompoundFunctionalTermType of Positions * ((Ast * Ast) option)
    | Ast.CompoundFunctionalTermType((pos1, pos2), (ast1, astTupleOption)) ->
        st.EvalPush("CompoundFunctionalTermType")
        eval uri st ast1
        match astTupleOption with 
        | Some (ast2, _) -> eval uri st ast2 |> ignore
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
            eval uri st ast3 |> ignore
        | _ -> ()
        st.EvalPop()
    // AliasedNamespaceIdentifier of Positions * (Ast * Ast option)
    | Ast.AliasedNamespaceIdentifier((pos1, pos2), (ast1, optAst)) ->
        st.EvalPush("AliasedNamespaceIdentifier")
        eval uri st ast1
        optAst |> Option.map (eval uri st) |> ignore
        eval_pos_ast_ast_opt st pos1 pos2
        st.EvalPop()
    | Ast.ClassType((pos1, pos2), (ast1, optAst)) ->
        st.EvalPush("ClassType")
        eval uri st ast1
        optAst |> Option.map (eval uri st) |> ignore
        eval_pos_ast_ast_opt st pos1 pos2
        st.EvalPop()
    | Ast.CompoundPredicateType((pos1, pos2), (ast1, optAst)) ->
        st.EvalPush("CompoundPredicateType")
        eval uri st ast1
        optAst |> Option.map (eval uri st) |> ignore
        eval_pos_ast_ast_opt st pos1 pos2
        st.EvalPop()
    | Ast.ReferenceToProofOrCorollary((pos1, pos2), (ast1, optAst)) ->
        st.EvalPush("ReferenceToProofOrCorollary")
        eval uri st ast1
        optAst |> Option.map (eval uri st) |> ignore
        eval_pos_ast_ast_opt st pos1 pos2
        st.EvalPop()
    | Ast.PredicateWithOptSpecification((pos1, pos2), (ast1, optAst)) ->
        st.EvalPush("PredicateWithOptSpecification")
        eval uri st ast1
        optAst |> Option.map (eval uri st) |> ignore
        eval_pos_ast_ast_opt st pos1 pos2
        st.EvalPop()
    // | SelfAts of Positions * char list
    | Ast.SelfAts((pos1, pos2), chars) -> 
        st.EvalPush("SelfAts")
        eval_pos_char_list st pos1 pos2 chars
        st.EvalPop()
    // | Translation of string * Ast
    | Ast.Translation(s, ast1) ->
        st.EvalPush("Translation")
        eval uri st ast1
        eval_pos_string_ast st s
        st.EvalPop()
    // | ExtensionBlock of Positions * (Ast * Ast)
    | Ast.InheritedClassType((pos1, pos2), ast1) -> 
        st.EvalPush("InheritedClassType")
        eval uri st ast1
        st.EvalPop()
    | Ast.ExtensionBlock((pos1, pos2), (ast1, ast2)) ->
        st.EvalPush("ExtensionBlock")
        eval uri st ast1
        eval uri st ast2
        st.EvalPop()
    | Ast.Impl((pos1, pos2), (ast1, ast2)) ->
        st.EvalPush("Impl")
        eval uri st ast1
        eval uri st ast2
        st.EvalPop()
    | Ast.Iif((pos1, pos2), (ast1, ast2)) ->
        st.EvalPush("Iif")
        eval uri st ast1
        eval uri st ast2
        st.EvalPop()
    | Ast.IsOperator((pos1, pos2), (ast1, ast2)) ->
        st.EvalPush("IsOperator")
        eval uri st ast1
        eval uri st ast2
        st.EvalPop()
    | Ast.Delegate((pos1, pos2), (ast1, ast2)) ->
        st.EvalPush("Delegate")
        eval uri st ast1
        eval uri st ast2
        st.EvalPop()
    // | ClosedOrOpenRange of Positions * ((Ast * Ast option) * Ast)
    | Ast.SignatureWithUserDefinedString((pos1, pos2),
                                         ((predicateIdentifierAst, optUserDefinedSymbolAst), paramTupleAst)) ->
        st.EvalPush("SignatureWithUserDefinedString")
        eval uri st predicateIdentifierAst
        optUserDefinedSymbolAst
        |> Option.map (eval uri st)
        |> Option.defaultValue ()
        |> ignore
        eval uri st paramTupleAst
        st.EvalPop()
    | Ast.PropertyBlock((pos1, pos2), (keywordPropertyAst, definitionPropertyAst)) ->
        st.EvalPush("PropertyBlock")
        let oldContext = st.CurrentContext
        match st.CurrentContext with
        | EvalContext.InBlock fplBlock -> 
            eval uri st keywordPropertyAst
            let fplValue = FplValue.CreateFplValue((pos1, pos2), FplBlockType.MandatoryPredicate, fplBlock)
            st.CurrentContext <- EvalContext.InPropertySignature fplValue
            eval uri st definitionPropertyAst
            tryAddBlock uri fplValue 
        | _ -> ()
        st.CurrentContext <- oldContext
        st.EvalPop()
    // | ReferencingIdentifier of Positions * (Ast * Ast list)
    | ReferencingIdentifier((pos1, pos2), (ast1, asts)) ->
        st.EvalPush("ReferencingIdentifier")
        eval uri st ast1
        asts |> List.map (eval uri st) |> ignore
        st.EvalPop()
    | Ast.ConditionFollowedByResult((pos1, pos2), (ast1, asts)) ->
        st.EvalPush("ConditionFollowedByResult")
        eval uri st ast1
        asts |> List.map (eval uri st) |> ignore
        st.EvalPop()
    | Ast.Localization((pos1, pos2), (ast1, asts)) ->
        st.EvalPush("Localization")
        eval uri st ast1
        asts |> List.map (eval uri st) |> ignore
        st.EvalPop()
    | Ast.FunctionalTermInstance((pos1, pos2), (functionalTermSignatureAst, functionalTermInstanceBlockAst)) ->
        st.EvalPush("FunctionalTermInstance")
        eval uri st functionalTermSignatureAst
        let oldContext = st.CurrentContext 
        match st.CurrentContext with
        | EvalContext.InPropertySignature fplValue ->
            st.CurrentContext <- EvalContext.InPropertyBlock fplValue
        | _ -> ()
        eval uri st functionalTermInstanceBlockAst
        st.CurrentContext <- oldContext
        st.EvalPop()
    // | All of Positions * ((Ast list * Ast option) list * Ast)
    | Ast.All((pos1, pos2), (astsOpts, ast1)) ->
        st.EvalPush("All")
        eval uri st ast1
        astsOpts
        |> List.map (fun (asts, optAst) ->
            asts |> List.map (eval uri st) |> ignore
            optAst |> Option.map (eval uri st) |> Option.defaultValue ()
            ())
        |> ignore
        st.EvalPop()
    | Ast.Exists((pos1, pos2), (astsOpts, ast1)) ->
        st.EvalPush("Exists")
        eval uri st ast1
        astsOpts
        |> List.map (fun (asts, optAst) ->
            asts |> List.map (eval uri st) |> ignore
            optAst |> Option.map (eval uri st) |> Option.defaultValue ()
            ())
        |> ignore
        st.EvalPop()
    // | ExistsN of Positions * ((Ast * (Ast * Ast option)) * Ast)
    | Ast.ExistsN((pos1, pos2), ((ast1, (ast2, optAst)), ast3)) ->
        st.EvalPush("ExistsN")
        eval uri st ast1
        eval uri st ast2
        optAst |> Option.map (eval uri st) |> Option.defaultValue () |> ignore
        eval uri st ast3
        st.EvalPop()
    // | FunctionalTermSignature of Positions * (Ast * Ast)
    | Ast.FunctionalTermSignature((pos1, pos2), ((optAst, signatureWithUserDefinedStringAst), mappingAst)) -> 
        st.EvalPush("FunctionalTermSignature")
        eval uri st signatureWithUserDefinedStringAst
        match st.CurrentContext with 
        | EvalContext.InPropertySignature fplValue -> 
            match optAst with
            | Some ast1 -> 
                eval uri st ast1
                fplValue.BlockType <- FplBlockType.OptionalFunctionalTerm
            | None -> 
                fplValue.BlockType <- FplBlockType.MandatoryFunctionalTerm
            adjustSignature st fplValue "->"
            fplValue.NameEndPos <- pos2
        | EvalContext.InSignature fplValue -> 
            match optAst with
            | Some ast1 -> 
                eval uri st ast1
                fplValue.BlockType <- FplBlockType.FunctionalTerm
            | None -> ()
            adjustSignature st fplValue "->"
            fplValue.NameEndPos <- pos2
        | _ -> ()
        eval uri st mappingAst
        st.EvalPop()
    | Ast.PredicateWithQualification(ast1, ast2) ->
        st.EvalPush("PredicateWithQualification")
        eval uri st ast1
        eval uri st ast2
        st.EvalPop()
    // | InfixOperation of Positions * (Ast * Ast option) list
    | Ast.InfixOperation((pos1, pos2), astsOpts) ->
        st.EvalPush("InfixOperation")
        astsOpts
        |> List.map (fun (ast1, optAst) -> optAst |> Option.map (eval uri st) |> Option.defaultValue ())
        |> ignore
        st.EvalPop()
    // | Expression of Positions * ((((Ast option * Ast) * Ast option) * Ast option) * Ast)
    | Ast.Expression((pos1, pos2), ((((optAst1, ast1), optAst2), optAst3), ast2)) ->
        st.EvalPush("Expression")
        optAst1 |> Option.map (eval uri st) |> Option.defaultValue ()
        eval uri st ast1
        optAst2 |> Option.map (eval uri st) |> Option.defaultValue ()
        optAst3 |> Option.map (eval uri st) |> Option.defaultValue ()
        eval uri st ast2
        st.EvalPop()
    // | Cases of Positions * (Ast list * Ast)
    | Ast.Cases((pos1, pos2), (asts, ast1)) ->
        st.EvalPush("Cases")
        asts |> List.map (eval uri st) |> ignore
        eval uri st ast1
        st.EvalPop()
    // | Assignment of Positions * (Ast * Ast)
    | Ast.Signature((pos1, pos2), (predicateIdentifierAst, paramTupleAst)) ->
        st.EvalPush("Signature")
        eval uri st predicateIdentifierAst
        eval uri st paramTupleAst
        st.EvalPop()
    | Ast.Assignment((pos1, pos2), (ast1, ast2)) ->
        st.EvalPush("Assignment")
        eval uri st ast1
        eval uri st ast2
        st.EvalPop()
    | Ast.PredicateInstance((pos1, pos2), ((optAst, signatureAst), predInstanceBlockAst)) ->
        st.EvalPush("PredicateInstance")
        eval uri st signatureAst
        let oldContext = st.CurrentContext 
        match st.CurrentContext with
        | EvalContext.InPropertySignature fplValue ->
            st.CurrentContext <- EvalContext.InPropertyBlock fplValue
            match optAst with
            | Some ast1 -> 
                eval uri st ast1
                fplValue.BlockType <- FplBlockType.OptionalPredicate
            | None -> 
                fplValue.BlockType <- FplBlockType.MandatoryPredicate
        | _ -> ()
        eval uri st predInstanceBlockAst
        st.CurrentContext <- oldContext
        st.EvalPop()
    | Ast.ParentConstructorCall((pos1, pos2), (inheritedClassTypeAst, argumentTupleAst)) ->
        st.EvalPush("ParentConstructorCall")
        eval uri st inheritedClassTypeAst
        eval uri st argumentTupleAst
        st.EvalPop()
    | Ast.JustifiedArgument((pos1, pos2), (ast1, ast2)) ->
        st.EvalPush("JustifiedArgument")
        eval uri st ast1
        eval uri st ast2
        st.EvalPop()
    | Ast.Argument((pos1, pos2), (ast1, ast2)) ->
        st.EvalPush("Argument")
        eval uri st ast1
        eval uri st ast2
        st.EvalPop()
    // | ForIn of Positions * ((Ast * Ast) * Ast list)
    | Ast.ForIn((pos1, pos2), ((ast1, ast2), asts)) ->
        st.EvalPush("ForIn")
        eval uri st ast1
        eval uri st ast2
        asts |> List.map (eval uri st) |> ignore
        st.EvalPop()
    // | SignatureWithPreConBlock of Ast * ((Ast list option * Ast) * Ast)
    | Ast.PremiseConclusionBlock((pos1, pos2), ((optVarDeclOrSpecList, premiseAst), conclusionAst)) ->
        st.EvalPush("PremiseConclusionBlock")
        optVarDeclOrSpecList |> Option.map (List.map (eval uri st) >> ignore) |> Option.defaultValue ()
        eval uri st premiseAst
        eval uri st conclusionAst
        st.EvalPop()
    // | Theorem of Positions * (Ast * (Ast list option * Ast))
    | Ast.Theorem((pos1, pos2), (signatureAst, (optVarDeclOrSpecList, predicateAst))) ->
        st.EvalPush("Theorem")
        let oldContext = st.CurrentContext
        match st.CurrentContext with
        | EvalContext.InTheory theoryValue -> 
            let fplValue = FplValue.CreateFplValue((pos1, pos2), FplBlockType.Theorem, theoryValue)
            st.CurrentContext <- EvalContext.InSignature fplValue
            eval uri st signatureAst
            tryAddBlock uri fplValue 
            st.CurrentContext <- EvalContext.InBlock fplValue
            evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst
        | _ -> ()
        st.CurrentContext <- oldContext
        st.EvalPop()
    | Ast.Lemma((pos1, pos2), (signatureAst, (optVarDeclOrSpecList, predicateAst))) ->
        st.EvalPush("Lemma")
        let oldContext = st.CurrentContext
        match st.CurrentContext with
        | EvalContext.InTheory theoryValue -> 
            let fplValue = FplValue.CreateFplValue((pos1, pos2), FplBlockType.Lemma, theoryValue)
            st.CurrentContext <- EvalContext.InSignature fplValue
            eval uri st signatureAst
            tryAddBlock uri fplValue 
            st.CurrentContext <- EvalContext.InBlock fplValue
            evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst
        | _ -> ()
        st.CurrentContext <- oldContext
        st.EvalPop()
    | Ast.Proposition((pos1, pos2), (signatureAst, (optVarDeclOrSpecList, predicateAst))) ->
        st.EvalPush("Proposition")
        let oldContext = st.CurrentContext
        match st.CurrentContext with
        | EvalContext.InTheory theoryValue -> 
            let fplValue = FplValue.CreateFplValue((pos1, pos2), FplBlockType.Proposition, theoryValue)
            st.CurrentContext <- EvalContext.InSignature fplValue
            eval uri st signatureAst
            tryAddBlock uri fplValue 
            st.CurrentContext <- EvalContext.InBlock fplValue
            evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst
        | _ -> ()
        st.CurrentContext <- oldContext
        st.EvalPop()
    | Ast.Conjecture((pos1, pos2), (signatureAst, (optVarDeclOrSpecList, predicateAst))) ->
        st.EvalPush("Conjecture")
        let oldContext = st.CurrentContext
        match st.CurrentContext with
        | EvalContext.InTheory theoryValue -> 
            let fplValue = FplValue.CreateFplValue((pos1, pos2), FplBlockType.Conjecture, theoryValue)
            st.CurrentContext <- EvalContext.InSignature fplValue
            eval uri st signatureAst
            tryAddBlock uri fplValue 
            st.CurrentContext <- EvalContext.InBlock fplValue
            evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst
        | _ -> ()
        st.CurrentContext <- oldContext
        st.EvalPop()
    | Ast.Axiom((pos1, pos2), (signatureAst, (optVarDeclOrSpecList, predicateAst))) ->
        st.EvalPush("Axiom")
        let oldContext = st.CurrentContext
        match st.CurrentContext with
        | EvalContext.InTheory theoryValue -> 
            let fplValue = FplValue.CreateFplValue((pos1, pos2), FplBlockType.Axiom, theoryValue)
            st.CurrentContext <- EvalContext.InSignature fplValue
            eval uri st signatureAst
            tryAddBlock uri fplValue 
            st.CurrentContext <- EvalContext.InBlock fplValue
            evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst
        | _ -> ()
        st.CurrentContext <- oldContext
        st.EvalPop()
    // | Corollary of Positions * ((Ast * Ast) * (Ast list option * Ast))
    | Ast.CorollarySignature(referencingIdentifierAst, paramTupleAst) ->
        st.EvalPush("CorollarySignature")
        eval uri st referencingIdentifierAst
        eval uri st paramTupleAst
        st.EvalPop()
    | Ast.Corollary((pos1, pos2), (corollarySignatureAst, (optVarDeclOrSpecList, predicateAst))) ->
        st.EvalPush("Corollary")
        let oldContext = st.CurrentContext
        match st.CurrentContext with
        | EvalContext.InTheory theoryValue -> 
            let fplValue = FplValue.CreateFplValue((pos1, pos2), FplBlockType.Corollary, theoryValue)
            st.CurrentContext <- EvalContext.InSignature fplValue
            eval uri st corollarySignatureAst
            tryAddBlock uri fplValue 
            st.CurrentContext <- EvalContext.InBlock fplValue
            evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst
        | _ -> ()
        st.CurrentContext <- oldContext
        st.EvalPop()
    // | NamedVarDecl of Positions * ((Ast list * Ast) * Ast)
    | Ast.NamedVarDecl((pos1, pos2), ((variableListAst, varDeclModifierAst), variableTypeAst)) ->
        st.EvalPush("NamedVarDecl")
        let oldContext = st.CurrentContext

        let evalNamedVarDecl (fplValue:FplValue) (context:string) = 
            fplValue.AuxiliaryInfo <- variableListAst |> List.length // remember how many variables to create
            eval uri st varDeclModifierAst
            fplValue.Scope 
            |> Seq.filter (fun varKeyValue -> FplValue.IsVariable(varKeyValue.Value))
            |> Seq.iter (fun childKeyValue -> 
                if not (childKeyValue.Value.Parent.Value.AuxiliaryUniqueChilds.Contains(childKeyValue.Value.Name)) then 
                    if context = "NamedVarDeclarationInBlock" then 
                        st.CurrentContext <- EvalContext.NamedVarDeclarationInBlock (childKeyValue.Value)
                    elif context = "InPropertySignature" then
                        st.CurrentContext <- EvalContext.InPropertySignature (childKeyValue.Value)
                    elif context = "InConstructorSignature" then
                        st.CurrentContext <- EvalContext.InConstructorSignature (childKeyValue.Value)
                    elif context = "InSignature" then
                        st.CurrentContext <- EvalContext.InSignature (childKeyValue.Value)
                    else
                        raise (ArgumentException(sprintf "Unknown context %s" context))
                    eval uri st variableTypeAst
                    st.CurrentContext <- oldContext
                    childKeyValue.Value.Parent.Value.AuxiliaryUniqueChilds.Add(childKeyValue.Value.Name) |> ignore
            )

        // create all variables of the named variable declaration in the current scope
        match st.CurrentContext with
        | EvalContext.InBlock fplValue 
        | EvalContext.InConstructorBlock fplValue
        | EvalContext.InPropertyBlock fplValue ->
            st.CurrentContext <- EvalContext.NamedVarDeclarationInBlock (fplValue)
        | _ -> ()
        variableListAst |> List.map (eval uri st) |> ignore 

        match st.CurrentContext with 
        | EvalContext.InBlock fplValue
        | EvalContext.InConstructorBlock fplValue
        | EvalContext.InPropertyBlock fplValue
        | EvalContext.NamedVarDeclarationInBlock fplValue ->
            evalNamedVarDecl fplValue "NamedVarDeclarationInBlock"
        | EvalContext.InPropertySignature fplValue -> 
            evalNamedVarDecl fplValue "InPropertySignature"
        | EvalContext.InConstructorSignature fplValue -> 
            evalNamedVarDecl fplValue "InConstructorSignature"
        | EvalContext.InSignature fplValue -> 
            evalNamedVarDecl fplValue "InSignature"
        | _ -> ()
        st.CurrentContext <- oldContext
        st.EvalPop()
    // | Axiom of Constructor * (Ast * (Ast list option * Ast))
    | Ast.Constructor((pos1, pos2), (signatureAst, (optVarDeclOrSpecListAst, keywordSelfAst))) ->
        st.EvalPush("Constructor")
        let oldContext = st.CurrentContext
        match st.CurrentContext with
        | EvalContext.InBlock classBlock -> 
            let fplValue = FplValue.CreateFplValue((pos1, pos2), FplBlockType.Constructor, classBlock)
            st.CurrentContext <- EvalContext.InConstructorSignature fplValue
            eval uri st signatureAst
            tryAddBlock uri fplValue
            st.CurrentContext <- EvalContext.InConstructorBlock fplValue
            match optVarDeclOrSpecListAst with
            | Some astList -> astList |> List.map (eval uri st) |> ignore
            | None -> ()
            eval uri st keywordSelfAst
        | _ -> ()
        st.CurrentContext <- oldContext
        st.EvalPop()
    // | DefPredicateContent of Ast list option * Ast
    | Ast.DefPredicateContent(optAsts, ast1) ->
        st.EvalPush("DefPredicateContent")
        optAsts
        |> Option.map (List.map (eval uri st) >> ignore)
        |> Option.defaultValue ()
        |> ignore
        eval uri st ast1
        st.EvalPop()
    | Ast.DefFunctionContent(optAsts, ast1) ->
        st.EvalPush("DefFunctionContent")
        optAsts
        |> Option.map (List.map (eval uri st) >> ignore)
        |> Option.defaultValue ()
        |> ignore
        eval uri st ast1
        st.EvalPop()
    // | DefClassCompleteContent of Ast list option * Ast list
    | Ast.DefClassCompleteContent(optAsts, asts) ->
        st.EvalPush("DefClassCompleteContent")
        optAsts |> Option.map (List.map (eval uri st) >> ignore) |> Option.defaultValue ()
        asts |> List.map (eval uri st) |> ignore
        st.EvalPop()
    // | DefinitionPredicate of Positions * (Ast * (Ast * Ast list option))
    | Ast.DefinitionPredicate((pos1, pos2), (signatureWithUserDefinedStringAst, (predicateContentAst, optPropertyListAsts))) ->
        st.EvalPush("DefinitionPredicate")
        let oldContext = st.CurrentContext
        match st.CurrentContext with
        | EvalContext.InTheory theoryValue -> 
            let fplValue = FplValue.CreateFplValue((pos1, pos2), FplBlockType.Predicate, theoryValue)
            st.CurrentContext <- EvalContext.InSignature fplValue
            eval uri st signatureWithUserDefinedStringAst
            tryAddBlock uri fplValue 
            st.CurrentContext <- EvalContext.InBlock fplValue
            eval uri st predicateContentAst
            optPropertyListAsts |> Option.map (List.map (eval uri st) >> ignore) |> Option.defaultValue ()
        | _ -> ()
        st.CurrentContext <- oldContext
        st.EvalPop()
    // | DefinitionFunctionalTerm of Positions * ((Ast * Ast) * (Ast * Ast list option))
    | Ast.DefinitionFunctionalTerm((pos1, pos2), (functionalTermSignatureAst, (funcContentAst, optPropertyListAsts))) ->
        st.EvalPush("DefinitionFunctionalTerm")
        let oldContext = st.CurrentContext
        match st.CurrentContext with
        | EvalContext.InTheory theoryValue -> 
            let fplValue = FplValue.CreateFplValue((pos1, pos2), FplBlockType.FunctionalTerm, theoryValue)
            st.CurrentContext <- EvalContext.InSignature fplValue
            eval uri st functionalTermSignatureAst
            tryAddBlock uri fplValue 
            st.CurrentContext <- EvalContext.InBlock fplValue
            eval uri st funcContentAst
            optPropertyListAsts |> Option.map (List.map (eval uri st) >> ignore) |> Option.defaultValue ()
        | _ -> ()
        st.CurrentContext <- oldContext
        st.EvalPop()
    // | DefinitionClass of Positions * (((Ast * Ast option) * Ast list) * (Ast * Ast list option))
    | Ast.DefinitionClass((pos1, pos2),
                          (((predicateIdentifierAst, optUserDefinedObjSymAst), classTypeListAsts),
                           (classContentAst, optPropertyListAsts))) ->
        st.EvalPush("DefinitionClass")
        let oldContext = st.CurrentContext
        match st.CurrentContext with
        | EvalContext.InTheory theoryValue -> 
            let fplValue = FplValue.CreateFplValue((pos1, pos2), FplBlockType.Class, theoryValue)
            st.CurrentContext <- EvalContext.InSignature fplValue
            eval uri st predicateIdentifierAst
            tryAddBlock uri fplValue 
            optUserDefinedObjSymAst |> Option.map (eval uri st) |> Option.defaultValue ()
            classTypeListAsts |> List.map (eval uri st) |> ignore
            st.CurrentContext <- EvalContext.InBlock fplValue
            eval uri st classContentAst
            optPropertyListAsts
            |> Option.map (List.map (eval uri st) >> ignore)
            |> Option.defaultValue ()
        | _ -> ()
        st.CurrentContext <- oldContext
        st.EvalPop()
    // | DerivedPredicate of Ast
    | Ast.DerivedPredicate ast1 -> 
        st.EvalPush("DefinitionClass")
        eval uri st ast1
        st.EvalPop()
    // | Proof of Positions * (Ast * (Ast list * Ast option))
    | Ast.Proof((pos1, pos2), (referencingIdentifierAst, (proofArgumentListAst, optQedAst))) ->
        st.EvalPush("Proof")
        let oldContext = st.CurrentContext
        match st.CurrentContext with
        | EvalContext.InTheory theoryValue -> 
            let fplValue = FplValue.CreateFplValue((pos1, pos2), FplBlockType.Proof, theoryValue)
            st.CurrentContext <- EvalContext.InSignature fplValue
            eval uri st referencingIdentifierAst
            tryAddBlock uri fplValue 
            st.CurrentContext <- EvalContext.InBlock fplValue
            proofArgumentListAst |> List.map (eval uri st) |> ignore
            optQedAst |> Option.map (eval uri st) |> Option.defaultValue ()
        | _ -> ()
        st.CurrentContext <- oldContext
        st.EvalPop()
    | ast1 ->
        let astType = ast1.GetType().Name
        emitID000Diagnostics uri astType


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

        match usesClausesEvaluatedParsedAst with
        | Some pa ->
            // evaluate the ParsedAst
            let theoryValue = FplValue.CreateFplValue((Position("",0,1,1), Position("",0,1,1)), FplBlockType.Theory, st.Root)
            if not (st.Root.Scope.ContainsKey(pa.Id)) then
                st.Root.Scope.Add(pa.Id, theoryValue)
            theoryValue.Name <- pa.Id
            st.CurrentContext <- EvalContext.InTheory theoryValue
            eval uri st pa.Parsing.Ast
            pa.Status <- ParsedAstStatus.Evaluated
            theoryValue.NameIsFinal <- true
        | None -> found <- false
