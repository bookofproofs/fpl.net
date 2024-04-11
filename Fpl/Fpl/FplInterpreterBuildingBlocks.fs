module FplInterpreterBuildingBlocks

open System
open System.Collections.Generic
open FParsec
open ErrDiagnostics
open FplGrammarTypes
open FplInterpreterTypes

let rec adjustSignature (st:SymbolTable) (fplValue:FplValue) str = 
    if str <> "" then
        if str = "(" || str = ")" 
            || str = "[" || str = "]" 
            || fplValue.Name.EndsWith "(" 
            || fplValue.Name.EndsWith "[" 
            || fplValue.Name.Length = 0 
            || fplValue.Name.EndsWith "-> " 
            || str.StartsWith "$" then 
            fplValue.Name <- fplValue.Name + str
        else
            fplValue.Name <- fplValue.Name + ", " + str
        fplValue.TypeSignature <- fplValue.TypeSignature @ [str]
        match st.CurrentContext with
        | EvalContext.InSignature _ -> 
            if not fplValue.IsFplBlock then 
                match fplValue.Parent with
                | Some parent -> 
                    adjustSignature st parent str
                | None -> ()
        | EvalContext.InBlock _ -> 
            match fplValue.Parent with
            | Some parent -> 
                if parent.IsVariable then 
                    adjustSignature st parent str
            | None -> ()
        | _ -> ()

let eval_units (st: SymbolTable) unitType = 
    match st.CurrentContext with
    | EvalContext.InBlock fplValue 
    | EvalContext.InSignature fplValue -> 
        adjustSignature st fplValue unitType
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
    if fplValue.Parent.Value.Scope.ContainsKey(fplValue.Name) then
        let diagnostic = { 
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = fplValue.NameStartPos
            Diagnostic.EndPos = fplValue.NameEndPos
            Diagnostic.Code = 
                if fplValue.BlockType = FplBlockType.Variable || fplValue.BlockType = FplBlockType.VariadicVariable then 
                    VAR01 fplValue.Name
                else
                    ID001 fplValue.Name
            Diagnostic.Alternatives = None 
        }
        FplParser.parserDiagnostics.AddDiagnostic diagnostic
    else        
        fplValue.Parent.Value.Scope.Add(fplValue.Name, fplValue)

let tryAddVariadicVariables numberOfVariadicVars (startPos:Position) (endPos:Position) =
    if numberOfVariadicVars > 1 then
        let diagnostic = { 
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = startPos
            Diagnostic.EndPos = endPos
            Diagnostic.Code = VAR00 
            Diagnostic.Alternatives = None 
        }
        FplParser.parserDiagnostics.AddDiagnostic diagnostic

/// A recursive function evaluating an AST and returning a list of EvalAliasedNamespaceIdentifier records
/// for each occurrence of the uses clause in the FPL code.
let rec eval (st: SymbolTable) ast =

    let evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst =
        match optVarDeclOrSpecList with
        | Some astList -> astList |> List.map (eval st) |> ignore
        | None -> ()
        eval st predicateAst

    let evalMany st str pos1 pos2 = 
        match st.CurrentContext with
        | EvalContext.InBlock fplValue
        | EvalContext.InSignature fplValue -> 
            tryAddVariadicVariables fplValue.AuxiliaryInfo pos1 pos2
            // adjust type of variables to variadic variables, if their type has not yet been established
            fplValue.Scope
            |> Seq.filter (fun varKeyValue -> varKeyValue.Value.IsVariable && varKeyValue.Value.TypeSignature = [])
            |> Seq.iter (fun varKeyValue -> varKeyValue.Value.BlockType <- FplBlockType.VariadicVariable)
            adjustSignature st fplValue str
        | _ -> ()

    match ast with
    // units: | Star
    | Ast.IndexType -> eval_units st "ind"
    | Ast.ObjectType -> eval_units st "obj"
    | Ast.PredicateType -> eval_units st "pred"
    | Ast.FunctionalTermType -> eval_units st "func"  
    | Ast.Many((pos1, pos2),()) ->
        evalMany st "Many:" pos1 pos2
    | Ast.Many1((pos1, pos2),()) ->
        evalMany st "Many1:" pos1 pos2
    | Ast.One
    | Ast.Star
    | Ast.Dot
    | Ast.Intrinsic
    | Ast.Property
    | Ast.Optional
    | Ast.Error -> eval_units st ""
    // strings: | Digits of string
    | Ast.Digits s
    | Ast.PascalCaseId s
    | Ast.ExtensionRegex s -> eval_string st s
    // | DollarDigits of Positions * string
    | Ast.DollarDigits((pos1, pos2), s) -> 
        match st.CurrentContext with
        | EvalContext.InBlock fplValue 
        | EvalContext.InSignature fplValue ->
            adjustSignature st fplValue s
            fplValue.NameEndPos <- pos2 // the full name ends where the dollar digits end 
        | _ -> ()
    | Ast.Extensionname((pos1, pos2), s) ->
        match st.CurrentContext with
        | EvalContext.InBlock fplValue 
        | EvalContext.InSignature fplValue -> 
            adjustSignature st fplValue ("@" + s)
        | _ -> ()
    | Ast.TemplateType((pos1, pos2), s) -> 
        match st.CurrentContext with
        | EvalContext.InBlock fplValue 
        | EvalContext.InSignature fplValue -> 
            adjustSignature st fplValue s
        | _ -> ()
    | Ast.Var((pos1, pos2), s) ->
        match st.CurrentContext with
        | EvalContext.InBlock fplValue 
        | EvalContext.InSignature fplValue -> 
            let varValue = FplValue.CreateFplValue((pos1,pos2), FplBlockType.Variable, fplValue)
            varValue.Name <- s
            tryAddBlock varValue 
        | _ -> ()
    | Ast.DelegateId((pos1, pos2), s)
    | Ast.Alias((pos1, pos2), s)
    | Ast.LocalizationString((pos1, pos2), s)
    | Ast.ObjectSymbol((pos1, pos2), s)
    | Ast.ArgumentIdentifier((pos1, pos2), s)
    | Ast.Prefix((pos1, pos2), s)
    | Ast.Infix((pos1, pos2), s)
    | Ast.Postfix((pos1, pos2), s)
    | Ast.Symbol((pos1, pos2), s)
    | Ast.InfixOperator((pos1, pos2), s)
    | Ast.PostfixOperator((pos1, pos2), s)
    | Ast.PrefixOperator((pos1, pos2), s) -> eval_pos_string st pos1 pos2 s
    // | Self of Positions * unit
    | Ast.Self((pos1, pos2), _)
    | Ast.True((pos1, pos2), _)
    | Ast.False((pos1, pos2), _)
    | Ast.Undefined((pos1, pos2), _)
    | Ast.Trivial((pos1, pos2), _)
    | Ast.Qed((pos1, pos2), _) -> eval_pos_unit st pos1 pos2
    // | ExtDigits of Positions * Ast
    | Ast.RuleOfInference((pos1, pos2), signatureWithPremiseConclusionBlockAst) ->
        let oldContext = st.CurrentContext
        match st.CurrentContext with
        | EvalContext.InTheory theoryValue -> 
            let fplValue = FplValue.CreateFplValue((pos1, pos2), FplBlockType.RuleOfInference, theoryValue)
            st.CurrentContext <- EvalContext.InSignature fplValue
            eval st signatureWithPremiseConclusionBlockAst
            tryAddBlock fplValue 
        | _ -> ()
        st.CurrentContext <- oldContext
    | Ast.ClassIdentifier((pos1, pos2), ast1) 
    | Ast.ExtDigits((pos1, pos2), ast1)
    | Ast.ExtensionType((pos1, pos2), ast1)
    | Ast.UsesClause((pos1, pos2), ast1)
    | Ast.Not((pos1, pos2), ast1)
    | Ast.Domain((pos1, pos2), ast1)
    | Ast.Assertion((pos1, pos2), ast1)
    | Ast.ByDef((pos1, pos2), ast1)
    | Ast.DottedPredicate((pos1, pos2), ast1)
    | Ast.Return((pos1, pos2), ast1)
    | Ast.AssumeArgument((pos1, pos2), ast1)
    | Ast.RevokeArgument((pos1, pos2), ast1)
    | Ast.VariableType((pos1, pos2), ast1)
    | Ast.AST((pos1, pos2), ast1) ->
        eval st ast1
        eval_pos_ast st pos1 pos2
    // | NamespaceIdentifier of Positions * Ast list
    | Ast.PredicateIdentifier((pos1, pos2), asts) ->
        let pascalCaseIdList = asts |> List.collect (function Ast.PascalCaseId s -> [s] | _ -> [])
        let identifier = String.concat "." pascalCaseIdList
        match st.CurrentContext with
        | EvalContext.InTheory fplValue
        | EvalContext.InBlock fplValue
        | EvalContext.InSignature fplValue -> 
            adjustSignature st fplValue identifier
        | _ -> ()
    | Ast.ParamTuple((pos1, pos2), asts) ->
        match st.CurrentContext with
        | EvalContext.InBlock fplValue 
        | EvalContext.InSignature fplValue -> 
            adjustSignature st fplValue "("
            asts |> List.map (eval st) |> ignore
            adjustSignature st fplValue ")"
            fplValue.NameEndPos <- pos2
        | _ -> ()
    | Ast.BracketedCoordsInType((pos1, pos2), asts) ->
        match st.CurrentContext with
        | EvalContext.InBlock fplValue 
        | EvalContext.InSignature fplValue -> 
            adjustSignature st fplValue "["
            asts |> List.map (eval st) |> ignore
            adjustSignature st fplValue "]"
            fplValue.NameEndPos <- pos2
        | _ -> ()
    | Ast.NamespaceIdentifier((pos1, pos2), asts)
    | Ast.LocalizationTerm((pos1, pos2), asts)
    | Ast.LocalizationTermList((pos1, pos2), asts)
    | Ast.BrackedCoordList((pos1, pos2), asts)
    | Ast.And((pos1, pos2), asts)
    | Ast.Or((pos1, pos2), asts)
    | Ast.Xor((pos1, pos2), asts)
    | Ast.VarDeclBlock((pos1, pos2), asts)
    | Ast.StatementList((pos1, pos2), asts)
    | Ast.DefaultResult((pos1, pos2), asts)
    | Ast.Justification((pos1, pos2), asts)
    | Ast.ArgumentTuple((pos1, pos2), asts)
    | Ast.QualificationList((pos1, pos2), asts) ->
        asts |> List.map (eval st) |> ignore
    // | Namespace of Ast option * Ast list
    | Ast.Namespace(optAst, asts) ->
        optAst |> Option.map (eval st) |> ignore
        asts |> List.map (eval st) |> ignore
    // CompoundFunctionalTermType of Positions * ((Ast * Ast) option)
    | Ast.CompoundFunctionalTermType((pos1, pos2), (ast1, astTupleOption)) ->
        eval st ast1
        match astTupleOption with 
        | Some (ast2, _) -> eval st ast2 |> ignore
        | _ -> ()
        match astTupleOption with 
        | Some (_, ast3) -> eval st ast3 |> ignore
        | _ -> ()
    // AliasedNamespaceIdentifier of Positions * (Ast * Ast option)
    | Ast.AliasedNamespaceIdentifier((pos1, pos2), (ast1, optAst))
    | Ast.ClassType((pos1, pos2), (ast1, optAst))
    | Ast.CompoundPredicateType((pos1, pos2), (ast1, optAst))
    | Ast.ReferenceToProofOrCorollary((pos1, pos2), (ast1, optAst))
    | Ast.PredicateWithOptSpecification((pos1, pos2), (ast1, optAst)) ->
        eval st ast1
        optAst |> Option.map (eval st) |> ignore
        eval_pos_ast_ast_opt st pos1 pos2
    // | SelfAts of Positions * char list
    | Ast.SelfAts((pos1, pos2), chars) -> eval_pos_char_list st pos1 pos2 chars
    // | Translation of string * Ast
    | Ast.Translation(s, ast1) ->
        eval st ast1
        eval_pos_string_ast st s
    // | ExtensionBlock of Positions * (Ast * Ast)
    | Ast.ClassTypeWithModifier((pos1, pos2), (ast1, ast2)) -> 
        eval st ast1
        eval st ast2
    | Ast.ExtensionBlock((pos1, pos2), (ast1, ast2))
    | Ast.Impl((pos1, pos2), (ast1, ast2))
    | Ast.Iif((pos1, pos2), (ast1, ast2))
    | Ast.IsOperator((pos1, pos2), (ast1, ast2))
    | Ast.Delegate((pos1, pos2), (ast1, ast2)) ->
        eval st ast1
        eval st ast2
    // | ClosedOrOpenRange of Positions * ((Ast * Ast option) * Ast)
    | Ast.SignatureWithUserDefinedString((pos1, pos2),
                                         ((predicateIdentifierAst, optUserDefinedSymbolAst), paramTupleAst)) ->
        eval st predicateIdentifierAst
        optUserDefinedSymbolAst
        |> Option.map (eval st)
        |> Option.defaultValue ()
        |> ignore
        eval st paramTupleAst
    | Ast.PropertyBlock((pos1, pos2), ((ast1, optAst), ast2)) ->
        optAst |> Option.map (eval st) |> Option.defaultValue () |> ignore
        eval st ast1
        eval st ast2
    // | ReferencingIdentifier of Positions * (Ast * Ast list)
    | ReferencingIdentifier((pos1, pos2), (ast1, asts))
    | Ast.ConditionFollowedByResult((pos1, pos2), (ast1, asts))
    | Ast.Localization((pos1, pos2), (ast1, asts)) ->
        eval st ast1
        asts |> List.map (eval st) |> ignore
    // | ClassInstance of Positions * ((Ast * Ast) * Ast)
    | Ast.ClassInstance((pos1, pos2), ((ast1, ast2), ast3)) ->
        eval st ast1
        eval st ast2
        eval st ast3
    | Ast.FunctionalTermInstance((pos1, pos2), (functionalTermSignatureAst, ast2)) ->
        eval st functionalTermSignatureAst
        eval st ast2
    // | All of Positions * ((Ast list * Ast option) list * Ast)
    | Ast.All((pos1, pos2), (astsOpts, ast1))
    | Ast.Exists((pos1, pos2), (astsOpts, ast1)) ->
        eval st ast1

        astsOpts
        |> List.map (fun (asts, optAst) ->
            asts |> List.map (eval st) |> ignore
            optAst |> Option.map (eval st) |> Option.defaultValue ()
            ())
        |> ignore

    // | ExistsN of Positions * ((Ast * (Ast * Ast option)) * Ast)
    | Ast.ExistsN((pos1, pos2), ((ast1, (ast2, optAst)), ast3)) ->
        eval st ast1
        eval st ast2
        optAst |> Option.map (eval st) |> Option.defaultValue () |> ignore
        eval st ast3
    // | FunctionalTermSignature of (Ast * Ast)
    | Ast.FunctionalTermSignature(signatureWithUserDefinedStringAst, mappingAst) -> 
        eval st signatureWithUserDefinedStringAst
        match st.CurrentContext with 
        | EvalContext.InSignature fplBlock -> 
            fplBlock.Name <- fplBlock.Name + " -> "
            fplBlock.TypeSignature <- fplBlock.TypeSignature @ ["->"]
        | _ -> ()
        eval st mappingAst
    | Ast.PredicateWithQualification(ast1, ast2) ->
        eval st ast1
        eval st ast2
    // | InfixOperation of Positions * (Ast * Ast option) list
    | Ast.InfixOperation((pos1, pos2), astsOpts) ->
        astsOpts
        |> List.map (fun (ast1, optAst) -> optAst |> Option.map (eval st) |> Option.defaultValue ())
        |> ignore
    // | Expression of Positions * ((((Ast option * Ast) * Ast option) * Ast option) * Ast)
    | Ast.Expression((pos1, pos2), ((((optAst1, ast1), optAst2), optAst3), ast2)) ->
        optAst1 |> Option.map (eval st) |> Option.defaultValue ()
        eval st ast1
        optAst2 |> Option.map (eval st) |> Option.defaultValue ()
        optAst3 |> Option.map (eval st) |> Option.defaultValue ()
        eval st ast2
    // | Cases of Positions * (Ast list * Ast)
    | Ast.Cases((pos1, pos2), (asts, ast1)) ->
        asts |> List.map (eval st) |> ignore
        eval st ast1
    // | Assignment of Positions * (Ast * Ast)
    | Ast.Signature((pos1, pos2), (predicateIdentifierAst, paramTupleAst)) ->
        eval st predicateIdentifierAst
        eval st paramTupleAst
    | Ast.Assignment((pos1, pos2), (ast1, ast2))
    | Ast.PredicateInstance((pos1, pos2), (ast1, ast2))
    | Ast.ParentConstructorCall((pos1, pos2), (ast1, ast2))
    | Ast.JustifiedArgument((pos1, pos2), (ast1, ast2))
    | Ast.Argument((pos1, pos2), (ast1, ast2)) ->
        eval st ast1
        eval st ast2
    // | ForIn of Positions * ((Ast * Ast) * Ast list)
    | Ast.ForIn((pos1, pos2), ((ast1, ast2), asts)) ->
        eval st ast1
        eval st ast2
        asts |> List.map (eval st) |> ignore
    // | SignatureWithPreConBlock of Ast * ((Ast list option * Ast) * Ast)
    | Ast.SignatureWithPreConBlock(signatureAst, ((optVarDeclOrSpecList, premiseAst), conclusionAst)) ->
        eval st signatureAst
        optVarDeclOrSpecList |> Option.map (List.map (eval st) >> ignore) |> Option.defaultValue ()
        eval st premiseAst
        eval st conclusionAst
    // | Theorem of Positions * (Ast * (Ast list option * Ast))
    | Ast.Theorem((pos1, pos2), (signatureAst, (optVarDeclOrSpecList, predicateAst))) ->
        let oldContext = st.CurrentContext
        match st.CurrentContext with
        | EvalContext.InTheory theoryValue -> 
            let fplValue = FplValue.CreateFplValue((pos1, pos2), FplBlockType.Theorem, theoryValue)
            st.CurrentContext <- EvalContext.InSignature fplValue
            eval st signatureAst
            tryAddBlock fplValue 
            st.CurrentContext <- EvalContext.InBlock fplValue
            evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst
        | _ -> ()
        st.CurrentContext <- oldContext
    | Ast.Lemma((pos1, pos2), (signatureAst, (optVarDeclOrSpecList, predicateAst))) ->
        let oldContext = st.CurrentContext
        match st.CurrentContext with
        | EvalContext.InTheory theoryValue -> 
            let fplValue = FplValue.CreateFplValue((pos1, pos2), FplBlockType.Lemma, theoryValue)
            st.CurrentContext <- EvalContext.InSignature fplValue
            eval st signatureAst
            tryAddBlock fplValue 
            st.CurrentContext <- EvalContext.InBlock fplValue
            evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst
        | _ -> ()
        st.CurrentContext <- oldContext
    | Ast.Proposition((pos1, pos2), (signatureAst, (optVarDeclOrSpecList, predicateAst))) ->
        let oldContext = st.CurrentContext
        match st.CurrentContext with
        | EvalContext.InTheory theoryValue -> 
            let fplValue = FplValue.CreateFplValue((pos1, pos2), FplBlockType.Proposition, theoryValue)
            st.CurrentContext <- EvalContext.InSignature fplValue
            eval st signatureAst
            tryAddBlock fplValue 
            st.CurrentContext <- EvalContext.InBlock fplValue
            evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst
        | _ -> ()
        st.CurrentContext <- oldContext
    | Ast.Conjecture((pos1, pos2), (signatureAst, (optVarDeclOrSpecList, predicateAst))) ->
        let oldContext = st.CurrentContext
        match st.CurrentContext with
        | EvalContext.InTheory theoryValue -> 
            let fplValue = FplValue.CreateFplValue((pos1, pos2), FplBlockType.Conjecture, theoryValue)
            st.CurrentContext <- EvalContext.InSignature fplValue
            eval st signatureAst
            tryAddBlock fplValue 
            st.CurrentContext <- EvalContext.InBlock fplValue
            evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst
        | _ -> ()
        st.CurrentContext <- oldContext
    | Ast.Axiom((pos1, pos2), (signatureAst, (optVarDeclOrSpecList, predicateAst))) ->
        let oldContext = st.CurrentContext
        match st.CurrentContext with
        | EvalContext.InTheory theoryValue -> 
            let fplValue = FplValue.CreateFplValue((pos1, pos2), FplBlockType.Axiom, theoryValue)
            st.CurrentContext <- EvalContext.InSignature fplValue
            eval st signatureAst
            tryAddBlock fplValue 
            st.CurrentContext <- EvalContext.InBlock fplValue
            evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst
        | _ -> ()
        st.CurrentContext <- oldContext
    // | Corollary of Positions * ((Ast * Ast) * (Ast list option * Ast))
    | Ast.CorollarySignature(referencingIdentifierAst, paramTupleAst) ->
        eval st referencingIdentifierAst
        eval st paramTupleAst
    | Ast.Corollary((pos1, pos2), (corollarySignatureAst, (optVarDeclOrSpecList, predicateAst))) ->
        let oldContext = st.CurrentContext
        match st.CurrentContext with
        | EvalContext.InTheory theoryValue -> 
            let fplValue = FplValue.CreateFplValue((pos1, pos2), FplBlockType.Corollary, theoryValue)
            st.CurrentContext <- EvalContext.InSignature fplValue
            eval st corollarySignatureAst
            tryAddBlock fplValue 
            st.CurrentContext <- EvalContext.InBlock fplValue
            evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst
        | _ -> ()
        st.CurrentContext <- oldContext
    // | NamedVarDecl of Positions * ((Ast list * Ast) * Ast)
    | Ast.NamedVarDecl((pos1, pos2), ((variableListAst, varDeclModifierAst), variableTypeAst)) ->
        let oldContext = st.CurrentContext
        variableListAst |> List.map (eval st) |> ignore
        match st.CurrentContext with 
        | EvalContext.InBlock fplValue ->
            fplValue.AuxiliaryInfo <- variableListAst |> List.length // remember how many variables to create
            eval st varDeclModifierAst
            fplValue.Scope 
            |> Seq.filter (fun varKeyValue -> varKeyValue.Value.IsVariable)
            |> Seq.iter (fun childKeyValue -> 
                st.CurrentContext <- EvalContext.InBlock (childKeyValue.Value)
                eval st variableTypeAst
            )
        | EvalContext.InSignature fplValue -> 
            fplValue.AuxiliaryInfo <- variableListAst |> List.length // remember how many variables to create
            eval st varDeclModifierAst
            fplValue.Scope 
            |> Seq.filter (fun varKeyValue -> varKeyValue.Value.IsVariable)
            |> Seq.iter (fun childKeyValue -> 
                st.CurrentContext <- EvalContext.InSignature (childKeyValue.Value)
                eval st variableTypeAst
            )
        | _ -> ()
        st.CurrentContext <- oldContext
    // | Axiom of Constructor * (Ast * (Ast list option * Ast))

    | Ast.Constructor((pos1, pos2), (ast1, (Some astList, ast2))) ->
        eval st ast1
        astList |> List.map (eval st) |> ignore
        eval st ast2
    | Ast.Constructor((pos1, pos2), (ast1, (None, ast2))) ->
        eval st ast1
        eval st ast2
    // | DefPredicateContent of Ast list option * Ast
    | Ast.DefPredicateContent(optAsts, ast1)
    | Ast.DefFunctionContent(optAsts, ast1)
    | Ast.DefClassContent(optAsts, ast1) ->
        optAsts
        |> Option.map (List.map (eval st) >> ignore)
        |> Option.defaultValue ()
        |> ignore

        eval st ast1
    // | DefClassCompleteContent of Ast list option * Ast list
    | Ast.DefClassCompleteContent(optAsts, asts) ->
        optAsts |> Option.map (List.map (eval st) >> ignore) |> Option.defaultValue ()
        asts |> List.map (eval st) |> ignore
    // | DefinitionPredicate of Positions * (Ast * (Ast * Ast list option))
    | Ast.DefinitionPredicate((pos1, pos2), (signatureWithUserDefinedStringAst, (predicateContentAst, optPropertyListAsts))) ->
        let oldContext = st.CurrentContext
        match st.CurrentContext with
        | EvalContext.InTheory theoryValue -> 
            let fplValue = FplValue.CreateFplValue((pos1, pos2), FplBlockType.Predicate, theoryValue)
            st.CurrentContext <- EvalContext.InSignature fplValue
            eval st signatureWithUserDefinedStringAst
            tryAddBlock fplValue 
            st.CurrentContext <- EvalContext.InBlock fplValue
            eval st predicateContentAst
            optPropertyListAsts |> Option.map (List.map (eval st) >> ignore) |> Option.defaultValue ()
        | _ -> ()
        st.CurrentContext <- oldContext
    // | DefinitionFunctionalTerm of Positions * ((Ast * Ast) * (Ast * Ast list option))
    | Ast.DefinitionFunctionalTerm((pos1, pos2), (functionalTermSignatureAst, (funcContentAst, optPropertyListAsts))) ->
        let oldContext = st.CurrentContext
        match st.CurrentContext with
        | EvalContext.InTheory theoryValue -> 
            let fplValue = FplValue.CreateFplValue((pos1, pos2), FplBlockType.Predicate, theoryValue)
            st.CurrentContext <- EvalContext.InSignature fplValue
            eval st functionalTermSignatureAst
            tryAddBlock fplValue 
            st.CurrentContext <- EvalContext.InBlock fplValue
            eval st funcContentAst
            optPropertyListAsts |> Option.map (List.map (eval st) >> ignore) |> Option.defaultValue ()
        | _ -> ()
        st.CurrentContext <- oldContext
    // | DefinitionClass of Positions * (((Ast * Ast option) * Ast list) * (Ast * Ast list option))
    | Ast.DefinitionClass((pos1, pos2),
                          (((predicateIdentifierAst, optUserDefinedObjSymAst), classTypeWithModifierListAsts),
                           (classContentAst, optPropertyListAsts))) ->
        let oldContext = st.CurrentContext
        match st.CurrentContext with
        | EvalContext.InTheory theoryValue -> 
            let fplValue = FplValue.CreateFplValue((pos1, pos2), FplBlockType.Class, theoryValue)
            st.CurrentContext <- EvalContext.InSignature fplValue
            eval st predicateIdentifierAst
            tryAddBlock fplValue 
            st.CurrentContext <- EvalContext.InBlock fplValue
            optUserDefinedObjSymAst |> Option.map (eval st) |> Option.defaultValue ()
            classTypeWithModifierListAsts |> List.map (eval st) |> ignore
            eval st classContentAst
            optPropertyListAsts
            |> Option.map (List.map (eval st) >> ignore)
            |> Option.defaultValue ()
        | _ -> ()
        st.CurrentContext <- oldContext

    // | DerivedPredicate of Ast
    | Ast.DerivedPredicate ast1 -> eval st ast1
    // | Proof of Positions * (Ast * (Ast list * Ast option))
    | Ast.Proof((pos1, pos2), (referencingIdentifierAst, (proofArgumentListAst, optQedAst))) ->
        let oldContext = st.CurrentContext
        match st.CurrentContext with
        | EvalContext.InTheory theoryValue -> 
            let fplValue = FplValue.CreateFplValue((pos1, pos2), FplBlockType.Class, theoryValue)
            st.CurrentContext <- EvalContext.InSignature fplValue
            eval st referencingIdentifierAst
            tryAddBlock fplValue 
            st.CurrentContext <- EvalContext.InBlock fplValue
            proofArgumentListAst |> List.map (eval st) |> ignore
            optQedAst |> Option.map (eval st) |> Option.defaultValue ()
        | _ -> ()
        st.CurrentContext <- oldContext
    | ast1 ->
        let astType = ast1.GetType().Name

        let diagnostic =
            { Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
              Diagnostic.Severity = DiagnosticSeverity.Error
              Diagnostic.StartPos = Position("", 0, 1, 1)
              Diagnostic.EndPos = Position("", 0, 1, 1)
              Diagnostic.Code = ID000 astType
              Diagnostic.Alternatives = None }

        FplParser.parserDiagnostics.AddDiagnostic diagnostic


let tryFindParsedAstUsesClausesEvaluated (parsedAsts: List<ParsedAst>) =
    if parsedAsts.Exists(fun pa -> pa.Status = ParsedAstStatus.UsesClausesEvaluated) then
        Some(parsedAsts.Find(fun pa -> pa.Status = ParsedAstStatus.UsesClausesEvaluated))
    else
        None

let evaluateSymbolTable (st: SymbolTable) =
    // there is a valid topological sorting, order the list descending by this ordering
    st.ParsedAsts.Sort(
        Comparer<ParsedAst>.Create(fun b a -> compare a.Sorting.TopologicalSorting b.Sorting.TopologicalSorting)
    )

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
            eval st pa.Parsing.Ast
            pa.Status <- ParsedAstStatus.Evaluated
        | None -> found <- false
