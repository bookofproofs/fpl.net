﻿module FplInterpreterBuildingBlocks

open System
open System.Collections.Generic
open FParsec
open ErrDiagnostics
open FplGrammarTypes
open FplInterpreterTypes

let rec adjustSignature (st:SymbolTable) (fplValue:FplValue) str = 
    if str <> "" && not fplValue.IsVariable then
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
        // note: the manipulation of the TypeSignature is necessary for all kinds of fplValue
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

    let evalMany (st:SymbolTable) str pos1 pos2 = 
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
    | Ast.IndexType -> 
        st.EvalPush("IndexType")
        eval_units st "ind"
        st.EvalPop() |> ignore
    | Ast.ObjectType -> 
        st.EvalPush("ObjectType")
        eval_units st "obj"
        st.EvalPop()
    | Ast.PredicateType -> 
        st.EvalPush("PredicateType")
        eval_units st "pred"
        st.EvalPop()
    | Ast.FunctionalTermType -> 
        st.EvalPush("FunctionalTermType")
        eval_units st "func"  
        st.EvalPop()
    | Ast.Many((pos1, pos2),()) ->
        st.EvalPush("Many")
        evalMany st "Many:" pos1 pos2
        st.EvalPop()
    | Ast.Many1((pos1, pos2),()) ->
        st.EvalPush("Many1")
        evalMany st "Many1:" pos1 pos2
        st.EvalPop()
    | Ast.One -> 
        st.EvalPush("One")
        eval_units st ""
        st.EvalPop()
    | Ast.Star -> 
        st.EvalPush("Star")
        eval_units st ""
        st.EvalPop()
    | Ast.Dot -> 
        st.EvalPush("Dot")
        eval_units st ""
        st.EvalPop()
    | Ast.Intrinsic -> 
        st.EvalPush("Intrinsic")
        eval_units st ""
        st.EvalPop()
    | Ast.Property -> 
        st.EvalPush("Property")
        eval_units st ""
        st.EvalPop()
    | Ast.Optional -> 
        st.EvalPush("Optional")
        eval_units st ""
        st.EvalPop()
    | Ast.Error ->   
        st.EvalPush("Error")
        eval_units st ""
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
        | EvalContext.InBlock fplValue 
        | EvalContext.InSignature fplValue ->
            adjustSignature st fplValue s
            fplValue.NameEndPos <- pos2 // the full name ends where the dollar digits end 
        | _ -> ()
        st.EvalPop() 
    | Ast.Extensionname((pos1, pos2), s) ->
        st.EvalPush("Extensionname")
        match st.CurrentContext with
        | EvalContext.InBlock fplValue 
        | EvalContext.InSignature fplValue -> 
            adjustSignature st fplValue ("@" + s)
        | _ -> ()
        st.EvalPop() 
    | Ast.TemplateType((pos1, pos2), s) -> 
        st.EvalPush("TemplateType")
        match st.CurrentContext with
        | EvalContext.InBlock fplValue 
        | EvalContext.InSignature fplValue -> 
            adjustSignature st fplValue s
        | _ -> ()
        st.EvalPop() 
    | Ast.Var((pos1, pos2), s) ->
        st.EvalPush("Var")
        match st.CurrentContext with
        | EvalContext.InBlock fplValue 
        | EvalContext.InSignature fplValue -> 
            let varValue = FplValue.CreateFplValue((pos1,pos2), FplBlockType.Variable, fplValue)
            varValue.Name <- s
            tryAddBlock varValue 
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
    | Ast.RuleOfInference((pos1, pos2), signatureWithPremiseConclusionBlockAst) ->
        st.EvalPush("RuleOfInference")
        let oldContext = st.CurrentContext
        match st.CurrentContext with
        | EvalContext.InTheory theoryValue -> 
            let fplValue = FplValue.CreateFplValue((pos1, pos2), FplBlockType.RuleOfInference, theoryValue)
            st.CurrentContext <- EvalContext.InSignature fplValue
            eval st signatureWithPremiseConclusionBlockAst
            tryAddBlock fplValue 
        | _ -> ()
        st.CurrentContext <- oldContext
        st.EvalPop() 
    | Ast.ClassIdentifier((pos1, pos2), ast1) ->
        st.EvalPush("ClassIdentifier")
        eval st ast1
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
    | Ast.Not((pos1, pos2), ast1) ->
        st.EvalPush("Not")
        eval st ast1
        eval_pos_ast st pos1 pos2
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
    | Ast.ByDef((pos1, pos2), ast1) ->
        st.EvalPush("ByDef")
        eval st ast1
        eval_pos_ast st pos1 pos2
        st.EvalPop()
    | Ast.DottedPredicate((pos1, pos2), ast1) ->
        st.EvalPush("DottedPredicate")
        eval st ast1
        eval_pos_ast st pos1 pos2
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
    | Ast.VariableType((pos1, pos2), ast1) ->
        st.EvalPush("VariableType")
        eval st ast1
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
        | EvalContext.InTheory fplValue
        | EvalContext.InBlock fplValue
        | EvalContext.InSignature fplValue -> 
            adjustSignature st fplValue identifier
        | _ -> ()
        st.EvalPop()
    | Ast.ParamTuple((pos1, pos2), asts) ->
        st.EvalPush("ParamTuple")
        match st.CurrentContext with
        | EvalContext.InBlock fplValue 
        | EvalContext.InSignature fplValue -> 
            adjustSignature st fplValue "("
            asts |> List.map (eval st) |> ignore
            adjustSignature st fplValue ")"
            fplValue.NameEndPos <- pos2
        | _ -> ()
        st.EvalPop()
    | Ast.BracketedCoordsInType((pos1, pos2), asts) ->
        st.EvalPush("BracketedCoordsInType")
        
        match st.CurrentContext with
        | EvalContext.InBlock fplValue 
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
        asts |> List.map (eval st) |> ignore
        st.EvalPop()
    | Ast.And((pos1, pos2), asts) ->
        st.EvalPush("And")
        asts |> List.map (eval st) |> ignore
        st.EvalPop()
    | Ast.Or((pos1, pos2), asts) ->
        st.EvalPush("Or")
        asts |> List.map (eval st) |> ignore
        st.EvalPop()
    | Ast.Xor((pos1, pos2), asts) ->
        st.EvalPush("Xor")
        asts |> List.map (eval st) |> ignore
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
        asts |> List.map (eval st) |> ignore
        st.EvalPop()
    | Ast.QualificationList((pos1, pos2), asts) ->
        st.EvalPush("QualificationList")
        asts |> List.map (eval st) |> ignore
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
            | EvalContext.InBlock fplBlock 
            | EvalContext.InSignature fplBlock ->
                adjustSignature st fplBlock "->"
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
    | Ast.ReferenceToProofOrCorollary((pos1, pos2), (ast1, optAst)) ->
        st.EvalPush("ReferenceToProofOrCorollary")
        eval st ast1
        optAst |> Option.map (eval st) |> ignore
        eval_pos_ast_ast_opt st pos1 pos2
        st.EvalPop()
    | Ast.PredicateWithOptSpecification((pos1, pos2), (ast1, optAst)) ->
        st.EvalPush("PredicateWithOptSpecification")
        eval st ast1
        optAst |> Option.map (eval st) |> ignore
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
    | Ast.Impl((pos1, pos2), (ast1, ast2)) ->
        st.EvalPush("Impl")
        eval st ast1
        eval st ast2
        st.EvalPop()
    | Ast.Iif((pos1, pos2), (ast1, ast2)) ->
        st.EvalPush("Iif")
        eval st ast1
        eval st ast2
        st.EvalPop()
    | Ast.IsOperator((pos1, pos2), (ast1, ast2)) ->
        st.EvalPush("IsOperator")
        eval st ast1
        eval st ast2
        st.EvalPop()
    | Ast.Delegate((pos1, pos2), (ast1, ast2)) ->
        st.EvalPush("Delegate")
        eval st ast1
        eval st ast2
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
        st.EvalPop()
    | Ast.PropertyBlock((pos1, pos2), ((ast1, optAst), ast2)) ->
        st.EvalPush("PropertyBlock")
        optAst |> Option.map (eval st) |> Option.defaultValue () |> ignore
        eval st ast1
        eval st ast2
        st.EvalPop()
    // | ReferencingIdentifier of Positions * (Ast * Ast list)
    | ReferencingIdentifier((pos1, pos2), (ast1, asts)) ->
        st.EvalPush("ReferencingIdentifier")
        eval st ast1
        asts |> List.map (eval st) |> ignore
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
    // | ClassInstance of Positions * ((Ast * Ast) * Ast)
    | Ast.ClassInstance((pos1, pos2), ((ast1, ast2), ast3)) ->
        st.EvalPush("ClassInstance")
        eval st ast1
        eval st ast2
        eval st ast3
        st.EvalPop()
    | Ast.FunctionalTermInstance((pos1, pos2), (functionalTermSignatureAst, ast2)) ->
        st.EvalPush("FunctionalTermInstance")
        eval st functionalTermSignatureAst
        eval st ast2
        st.EvalPop()
    // | All of Positions * ((Ast list * Ast option) list * Ast)
    | Ast.All((pos1, pos2), (astsOpts, ast1)) ->
        st.EvalPush("All")
        eval st ast1
        astsOpts
        |> List.map (fun (asts, optAst) ->
            asts |> List.map (eval st) |> ignore
            optAst |> Option.map (eval st) |> Option.defaultValue ()
            ())
        |> ignore
        st.EvalPop()
    | Ast.Exists((pos1, pos2), (astsOpts, ast1)) ->
        st.EvalPush("Exists")
        eval st ast1
        astsOpts
        |> List.map (fun (asts, optAst) ->
            asts |> List.map (eval st) |> ignore
            optAst |> Option.map (eval st) |> Option.defaultValue ()
            ())
        |> ignore
        st.EvalPop()
    // | ExistsN of Positions * ((Ast * (Ast * Ast option)) * Ast)
    | Ast.ExistsN((pos1, pos2), ((ast1, (ast2, optAst)), ast3)) ->
        st.EvalPush("ExistsN")
        eval st ast1
        eval st ast2
        optAst |> Option.map (eval st) |> Option.defaultValue () |> ignore
        eval st ast3
        st.EvalPop()
    // | FunctionalTermSignature of (Ast * Ast)
    | Ast.FunctionalTermSignature(signatureWithUserDefinedStringAst, mappingAst) -> 
        st.EvalPush("FunctionalTermSignature")
        eval st signatureWithUserDefinedStringAst
        match st.CurrentContext with 
        | EvalContext.InBlock fplBlock 
        | EvalContext.InSignature fplBlock -> 
            adjustSignature st fplBlock "->"
        | _ -> ()
        eval st mappingAst
        st.EvalPop()
    | Ast.PredicateWithQualification(ast1, ast2) ->
        st.EvalPush("PredicateWithQualification")
        eval st ast1
        eval st ast2
        st.EvalPop()
    // | InfixOperation of Positions * (Ast * Ast option) list
    | Ast.InfixOperation((pos1, pos2), astsOpts) ->
        st.EvalPush("InfixOperation")
        astsOpts
        |> List.map (fun (ast1, optAst) -> optAst |> Option.map (eval st) |> Option.defaultValue ())
        |> ignore
        st.EvalPop()
    // | Expression of Positions * ((((Ast option * Ast) * Ast option) * Ast option) * Ast)
    | Ast.Expression((pos1, pos2), ((((optAst1, ast1), optAst2), optAst3), ast2)) ->
        st.EvalPush("Expression")
        optAst1 |> Option.map (eval st) |> Option.defaultValue ()
        eval st ast1
        optAst2 |> Option.map (eval st) |> Option.defaultValue ()
        optAst3 |> Option.map (eval st) |> Option.defaultValue ()
        eval st ast2
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
    | Ast.PredicateInstance((pos1, pos2), (ast1, ast2)) ->
        st.EvalPush("PredicateInstance")
        eval st ast1
        eval st ast2
        st.EvalPop()
    | Ast.ParentConstructorCall((pos1, pos2), (ast1, ast2)) ->
        st.EvalPush("ParentConstructorCall")
        eval st ast1
        eval st ast2
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
    | Ast.SignatureWithPreConBlock(signatureAst, ((optVarDeclOrSpecList, premiseAst), conclusionAst)) ->
        st.EvalPush("SignatureWithPreConBlock")
        eval st signatureAst
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
            let fplValue = FplValue.CreateFplValue((pos1, pos2), FplBlockType.Theorem, theoryValue)
            st.CurrentContext <- EvalContext.InSignature fplValue
            eval st signatureAst
            tryAddBlock fplValue 
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
            eval st signatureAst
            tryAddBlock fplValue 
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
            eval st signatureAst
            tryAddBlock fplValue 
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
            eval st signatureAst
            tryAddBlock fplValue 
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
            eval st signatureAst
            tryAddBlock fplValue 
            st.CurrentContext <- EvalContext.InBlock fplValue
            evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst
        | _ -> ()
        st.CurrentContext <- oldContext
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
            let fplValue = FplValue.CreateFplValue((pos1, pos2), FplBlockType.Corollary, theoryValue)
            st.CurrentContext <- EvalContext.InSignature fplValue
            eval st corollarySignatureAst
            tryAddBlock fplValue 
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
            eval st varDeclModifierAst
            fplValue.Scope 
            |> Seq.filter (fun varKeyValue -> varKeyValue.Value.IsVariable)
            |> Seq.iter (fun childKeyValue -> 
                if not (childKeyValue.Value.Parent.Value.AuxiliaryUniqueChilds.Contains(childKeyValue.Value.Name)) then 
                    if context = "InBlock" then 
                        st.CurrentContext <- EvalContext.InBlock (childKeyValue.Value)
                    elif context = "InSignature" then
                        st.CurrentContext <- EvalContext.InSignature (childKeyValue.Value)
                    eval st variableTypeAst
                    st.CurrentContext <- oldContext
                    childKeyValue.Value.Parent.Value.AuxiliaryUniqueChilds.Add(childKeyValue.Value.Name) |> ignore
            )

        // create all variables of the named variable declaration in the current scope
        variableListAst |> List.map (eval st) |> ignore 

        match st.CurrentContext with 
        | EvalContext.InBlock fplValue ->
            evalNamedVarDecl fplValue "InBlock"
        | EvalContext.InSignature fplValue -> 
            evalNamedVarDecl fplValue "InSignature"
        | _ -> ()
        st.CurrentContext <- oldContext
        st.EvalPop()
    // | Axiom of Constructor * (Ast * (Ast list option * Ast))
    | Ast.Constructor((pos1, pos2), (ast1, (optAstList, ast2))) ->
        st.EvalPush("Constructor")
        eval st ast1
        match optAstList with
        | Some astList -> astList |> List.map (eval st) |> ignore
        | None -> ()
        eval st ast2
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
    | Ast.DefClassContent(optAsts, ast1) ->
        st.EvalPush("DefClassContent")
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
            let fplValue = FplValue.CreateFplValue((pos1, pos2), FplBlockType.Predicate, theoryValue)
            st.CurrentContext <- EvalContext.InSignature fplValue
            eval st signatureWithUserDefinedStringAst
            tryAddBlock fplValue 
            st.CurrentContext <- EvalContext.InBlock fplValue
            eval st predicateContentAst
            optPropertyListAsts |> Option.map (List.map (eval st) >> ignore) |> Option.defaultValue ()
        | _ -> ()
        st.CurrentContext <- oldContext
        st.EvalPop()
    // | DefinitionFunctionalTerm of Positions * ((Ast * Ast) * (Ast * Ast list option))
    | Ast.DefinitionFunctionalTerm((pos1, pos2), (functionalTermSignatureAst, (funcContentAst, optPropertyListAsts))) ->
        st.EvalPush("DefinitionFunctionalTerm")
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
        st.EvalPop()
    // | DefinitionClass of Positions * (((Ast * Ast option) * Ast list) * (Ast * Ast list option))
    | Ast.DefinitionClass((pos1, pos2),
                          (((predicateIdentifierAst, optUserDefinedObjSymAst), classTypeWithModifierListAsts),
                           (classContentAst, optPropertyListAsts))) ->
        st.EvalPush("DefinitionClass")
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
            let fplValue = FplValue.CreateFplValue((pos1, pos2), FplBlockType.Class, theoryValue)
            st.CurrentContext <- EvalContext.InSignature fplValue
            eval st referencingIdentifierAst
            tryAddBlock fplValue 
            st.CurrentContext <- EvalContext.InBlock fplValue
            proofArgumentListAst |> List.map (eval st) |> ignore
            optQedAst |> Option.map (eval st) |> Option.defaultValue ()
        | _ -> ()
        st.CurrentContext <- oldContext
        st.EvalPop()
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
            eval st pa.Parsing.Ast
            pa.Status <- ParsedAstStatus.Evaluated
        | None -> found <- false
