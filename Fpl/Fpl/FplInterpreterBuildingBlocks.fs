﻿module FplInterpreterBuildingBlocks

open System
open System.Collections.Generic
open FParsec
open ErrDiagnostics
open FplGrammarTypes
open FplInterpreterTypes

let eval_units (st: SymbolTable) unitType = 
    match st.CurrentContext with
    | EvalContext.Multiple [ EvalContext.InTheory theoryId; EvalContext.InSignature fplValue] -> 
        fplValue.Name <- fplValue.Name + unitType
        fplValue.TypeSignature <- fplValue.Name + unitType
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
            Diagnostic.Code = ID001 fplValue.Name
            Diagnostic.Alternatives = None 
        }
        FplParser.parserDiagnostics.AddDiagnostic diagnostic
    else        
        fplValue.Parent.Value.Scope.Add(fplValue.Name, fplValue)

let tryAddVariadicVariables numberOfVariadicVars (startPos:Position) (endPos:Position)=
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
    let evalSimpleSignature signatureAst (fplBlockType:FplBlockType) pos1 pos2 = 
        match st.CurrentContext with
        | EvalContext.InTheory theoryId -> 
            let currentTheory = st.Theories.Value[theoryId]
            let createAndEval fplBlockType =
                let fv = FplValue.CreateFplValue((pos1, pos2), fplBlockType, currentTheory)
                EvalContext.Multiple [ EvalContext.InTheory theoryId; EvalContext.InSignature fv], fv
            let newContext, fplValue = createAndEval fplBlockType
            let oldContext = st.CurrentContext
            st.CurrentContext <- newContext
            eval st signatureAst
            st.CurrentContext <- oldContext
            tryAddBlock fplValue 
        | _ -> ()

    let evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst =
        match optVarDeclOrSpecList with
        | Some astList -> astList |> List.map (eval st) |> ignore
        | None -> ()
        eval st predicateAst

    match ast with
    // units: | Star
    | Ast.IndexType -> eval_units st "index"
    | Ast.ObjectType -> eval_units st "object"
    | Ast.PredicateType -> eval_units st "predicate"
    | Ast.FunctionalTermType -> eval_units st "function"  
    | Ast.Many((pos1, pos2),())
    | Ast.Many1((pos1, pos2),()) ->
        match st.CurrentContext with
        | EvalContext.Multiple [ EvalContext.InTheory _; EvalContext.InSignature fplBlock] -> 
            tryAddVariadicVariables fplBlock.AuxiliaryInfo pos1 pos2
        | _ -> ()
    | Ast.One
    | Ast.Star
    | Ast.Dot
    | Ast.Intrinsic
    | Ast.LeftClosed
    | Ast.LeftOpen
    | Ast.RightClosed
    | Ast.RightOpen
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
        | EvalContext.Multiple [ EvalContext.InTheory _; EvalContext.InSignature fplBlock] -> 
            fplBlock.Name <- fplBlock.Name + s
            fplBlock.NameEndPos <- pos2 // the full name ends where the dollar digits end 
        | _ -> ()
    | Ast.Extensionname((pos1, pos2), s) ->
        match st.CurrentContext with
        | EvalContext.Multiple [ EvalContext.InTheory _; EvalContext.InSignature fplBlock] -> 
            fplBlock.Name <- fplBlock.Name + "@" + s
            fplBlock.NameEndPos <- pos2 // the full name ends where the dollar digits end 
            fplBlock.TypeSignature <- fplBlock.TypeSignature + "@" + s
        | _ -> ()
    | Ast.TemplateType((pos1, pos2), s) -> 
        match st.CurrentContext with
        | EvalContext.Multiple [ EvalContext.InTheory _; EvalContext.InSignature fplBlock] -> 
            fplBlock.Name <- fplBlock.Name + s 
            fplBlock.NameEndPos <- pos2 // the full name ends where the dollar digits end 
            fplBlock.TypeSignature <- fplBlock.TypeSignature + s 
        | _ -> ()
    | Ast.DelegateId((pos1, pos2), s)
    | Ast.Alias((pos1, pos2), s)
    | Ast.LocalizationString((pos1, pos2), s)
    | Ast.Var((pos1, pos2), s)
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
            evalSimpleSignature signatureWithPremiseConclusionBlockAst FplBlockType.RuleOfInference pos1 pos2
    | Ast.ClassIdentifier((pos1, pos2), ast) 
    | Ast.ExtDigits((pos1, pos2), ast)
    | Ast.ExtensionType((pos1, pos2), ast)
    | Ast.UsesClause((pos1, pos2), ast)
    | Ast.SimpleVariableType((pos1, pos2), ast)
    | Ast.Not((pos1, pos2), ast)
    | Ast.Domain((pos1, pos2), ast)
    | Ast.Assertion((pos1, pos2), ast)
    | Ast.ByDef((pos1, pos2), ast)
    | Ast.DottedPredicate((pos1, pos2), ast)
    | Ast.Return((pos1, pos2), ast)
    | Ast.AssumeArgument((pos1, pos2), ast)
    | Ast.RevokeArgument((pos1, pos2), ast)
    | Ast.AST((pos1, pos2), ast) ->
        eval st ast
        eval_pos_ast st pos1 pos2
    // | NamespaceIdentifier of Positions * Ast list
    | Ast.PredicateIdentifier((pos1, pos2), asts) ->
        let pascalCaseIdList = asts |> List.collect (function Ast.PascalCaseId s -> [s] | _ -> [])
        let identifier = String.concat "." pascalCaseIdList
        match st.CurrentContext with
        | EvalContext.Multiple [ EvalContext.InTheory _; EvalContext.InSignature fplBlock] -> 
                fplBlock.Name <- fplBlock.Name + identifier
                fplBlock.NameStartPos <- pos1 // the full name begins where the PredicateIdentifier starts 
                match fplBlock.BlockType with 
                | FplBlockType.Class -> 
                    fplBlock.TypeSignature <- fplBlock.TypeSignature + identifier
                    fplBlock.NameEndPos <- pos2 // the full name stops where the PredicateIdentifier stops 
                | FplBlockType.Proof
                | FplBlockType.Predicate
                | FplBlockType.Theorem
                | FplBlockType.Lemma
                | FplBlockType.Proposition
                | FplBlockType.Conjecture
                | FplBlockType.Corollary
                | FplBlockType.RuleOfInference
                | FplBlockType.Conclusion
                | FplBlockType.Premise
                | FplBlockType.RuleOfInference
                | FplBlockType.Theory
                | FplBlockType.Axiom -> 
                    fplBlock.TypeSignature <- fplBlock.TypeSignature + "predicate"
                | FplBlockType.Constructor ->
                    fplBlock.TypeSignature <- fplBlock.TypeSignature + identifier
                | FplBlockType.SignatureVariable
                | FplBlockType.MandatoryProperty
                | FplBlockType.OptionalProperty
                | FplBlockType.InnerVariable
                | FplBlockType.FunctionalTerm -> ()
        | _ -> ()
    | Ast.ParamTuple((pos1, pos2), asts) ->
        match st.CurrentContext with
        | EvalContext.Multiple [ EvalContext.InTheory _; EvalContext.InSignature fplBlock] -> 
            fplBlock.Name <- fplBlock.Name + "("
            fplBlock.TypeSignature <- fplBlock.TypeSignature + "("
            asts |> List.map (eval st) |> ignore
            fplBlock.Name <- fplBlock.Name + ")"
            fplBlock.NameEndPos <- pos2 // the full name ends where the parameters end 
            fplBlock.TypeSignature <- fplBlock.TypeSignature + ")"
        | _ -> ()

    | Ast.NamespaceIdentifier((pos1, pos2), asts)
    | Ast.LocalizationTerm((pos1, pos2), asts)
    | Ast.LocalizationTermList((pos1, pos2), asts)
    | Ast.BrackedCoordList((pos1, pos2), asts)
    | Ast.BracketedCoordsInType((pos1, pos2), asts)
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
    // AliasedNamespaceIdentifier of Positions * (Ast * Ast option)
    | Ast.VariableType((pos1, pos2), (ast1, optAst))
    | Ast.AliasedNamespaceIdentifier((pos1, pos2), (ast1, optAst))
    | Ast.ClassType((pos1, pos2), (ast1, optAst))
    | Ast.ReferenceToProofOrCorollary((pos1, pos2), (ast1, optAst))
    | Ast.PredicateWithOptSpecification((pos1, pos2), (ast1, optAst)) ->
        eval st ast1
        optAst |> Option.map (eval st) |> ignore
        eval_pos_ast_ast_opt st pos1 pos2
    // | SelfAts of Positions * char list
    | Ast.SelfAts((pos1, pos2), chars) -> eval_pos_char_list st pos1 pos2 chars
    // | Translation of string * Ast
    | Ast.Translation(s, ast) ->
        eval st ast
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

    | Ast.ClosedOrOpenRange((pos1, pos2), ((ast1, optAst), ast2))
    | Ast.PropertyBlock((pos1, pos2), ((ast1, optAst), ast2)) ->
        optAst |> Option.map (eval st) |> Option.defaultValue () |> ignore
        eval st ast1
        eval st ast2
    // | RangeInType of Positions * (Ast option * Ast option)
    | Ast.RangeInType((pos1, pos2), (optAst1, optAst2)) ->
        optAst1 |> Option.map (eval st) |> Option.defaultValue () |> ignore
        optAst2 |> Option.map (eval st) |> Option.defaultValue () |> ignore
    // | ReferencingIdentifier of Positions * (Ast * Ast list)
    | ReferencingIdentifier((pos1, pos2), (ast, asts))
    | Ast.ConditionFollowedByResult((pos1, pos2), (ast, asts))
    | Ast.Localization((pos1, pos2), (ast, asts)) ->
        eval st ast
        asts |> List.map (eval st) |> ignore
    // | BoundedRangeInType of Positions * ((Ast * Ast) * Ast)
    | Ast.BoundedRangeInType((pos1, pos2), ((ast1, ast2), ast3))
    | Ast.ClassInstance((pos1, pos2), ((ast1, ast2), ast3)) ->
        eval st ast1
        eval st ast2
        eval st ast3
    | Ast.FunctionalTermInstance((pos1, pos2), (functionalTermSignatureAst, ast2)) ->
        eval st functionalTermSignatureAst
        eval st ast2
    // | All of Positions * ((Ast list * Ast option) list * Ast)
    | Ast.All((pos1, pos2), (astsOpts, ast))
    | Ast.Exists((pos1, pos2), (astsOpts, ast)) ->
        eval st ast

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
        | EvalContext.Multiple [ EvalContext.InTheory _; EvalContext.InSignature fplBlock] -> 
            fplBlock.Name <- fplBlock.Name + "->"
            fplBlock.TypeSignature <- fplBlock.TypeSignature + "->"
        | _ -> ()
        eval st mappingAst
    | Ast.PredicateWithQualification(ast1, ast2) ->
        eval st ast1
        eval st ast2
    // | InfixOperation of Positions * (Ast * Ast option) list
    | Ast.InfixOperation((pos1, pos2), astsOpts) ->
        eval st ast
        astsOpts
        |> List.map (fun (ast, optAst) -> optAst |> Option.map (eval st) |> Option.defaultValue ())
        |> ignore
    // | Expression of Positions * ((((Ast option * Ast) * Ast option) * Ast option) * Ast)
    | Ast.Expression((pos1, pos2), ((((optAst1, ast1), optAst2), optAst3), ast2)) ->
        optAst1 |> Option.map (eval st) |> Option.defaultValue ()
        eval st ast1
        optAst2 |> Option.map (eval st) |> Option.defaultValue ()
        optAst3 |> Option.map (eval st) |> Option.defaultValue ()
        eval st ast2
    // | Cases of Positions * (Ast list * Ast)
    | Ast.Cases((pos1, pos2), (asts, ast)) ->
        asts |> List.map (eval st) |> ignore
        eval st ast
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
        evalSimpleSignature signatureAst FplBlockType.Theorem pos1 pos2
        evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst
    | Ast.Lemma((pos1, pos2), (signatureAst, (optVarDeclOrSpecList, predicateAst))) ->
        evalSimpleSignature signatureAst FplBlockType.Lemma pos1 pos2
        evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst
    | Ast.Proposition((pos1, pos2), (signatureAst, (optVarDeclOrSpecList, predicateAst))) ->
        evalSimpleSignature signatureAst FplBlockType.Proposition pos1 pos2
        evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst
    | Ast.Conjecture((pos1, pos2), (signatureAst, (optVarDeclOrSpecList, predicateAst))) ->
        evalSimpleSignature signatureAst FplBlockType.Conjecture pos1 pos2
        evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst
    | Ast.Axiom((pos1, pos2), (signatureAst, (optVarDeclOrSpecList, predicateAst))) ->
        evalSimpleSignature signatureAst FplBlockType.Axiom pos1 pos2
        evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst
    // | Corollary of Positions * ((Ast * Ast) * (Ast list option * Ast))
    | Ast.CorollarySignature(referencingIdentifierAst, paramTupleAst) ->
        eval st referencingIdentifierAst
        eval st paramTupleAst
    | Ast.Corollary((pos1, pos2), (corollarySignatureAst, (optVarDeclOrSpecList, predicateAst))) ->
        evalSimpleSignature corollarySignatureAst FplBlockType.Corollary pos1 pos2
        evalCommonStepsVarDeclPredicate optVarDeclOrSpecList predicateAst
    // | NamedVarDecl of Positions * ((Ast list * Ast) * Ast)
    | Ast.NamedVarDecl((pos1, pos2), ((variableListAst, varDeclModifierAst), variableTypeAst)) ->
        match st.CurrentContext with 
        | EvalContext.Multiple [ EvalContext.InTheory _; EvalContext.InSignature fplBlock] -> 
            fplBlock.AuxiliaryInfo <- variableListAst |> List.length // remember how many variables to create
        | _ -> ()
        variableListAst |> List.map (eval st) |> ignore
        eval st varDeclModifierAst
        eval st variableTypeAst
    // | Axiom of Constructor * (Ast * (Ast list option * Ast))

    | Ast.Constructor((pos1, pos2), (ast, (Some astList, ast2))) ->
        eval st ast
        astList |> List.map (eval st) |> ignore
        eval st ast2
    | Ast.Constructor((pos1, pos2), (ast1, (None, ast2))) ->
        eval st ast1
        eval st ast2
    // | DefPredicateContent of Ast list option * Ast
    | Ast.DefPredicateContent(optAsts, ast)
    | Ast.DefFunctionContent(optAsts, ast)
    | Ast.DefClassContent(optAsts, ast) ->
        optAsts
        |> Option.map (List.map (eval st) >> ignore)
        |> Option.defaultValue ()
        |> ignore

        eval st ast
    // | DefClassCompleteContent of Ast list option * Ast list
    | Ast.DefClassCompleteContent(optAsts, asts) ->
        optAsts |> Option.map (List.map (eval st) >> ignore) |> Option.defaultValue ()
        asts |> List.map (eval st) |> ignore
    // | DefinitionPredicate of Positions * (Ast * (Ast * Ast list option))
    | Ast.DefinitionPredicate((pos1, pos2), (signatureWithUserDefinedStringAst, (predicateContentAst, optPropertyListAsts))) ->
        evalSimpleSignature signatureWithUserDefinedStringAst FplBlockType.Predicate pos1 pos2
        eval st predicateContentAst
        optPropertyListAsts |> Option.map (List.map (eval st) >> ignore) |> Option.defaultValue ()
    // | DefinitionFunctionalTerm of Positions * ((Ast * Ast) * (Ast * Ast list option))
    | Ast.DefinitionFunctionalTerm((pos1, pos2), (functionalTermSignatureAst, (funcContentAst, optPropertyListAsts))) ->
        evalSimpleSignature functionalTermSignatureAst FplBlockType.FunctionalTerm pos1 pos2
        eval st funcContentAst
        optPropertyListAsts |> Option.map (List.map (eval st) >> ignore) |> Option.defaultValue ()
    // | DefinitionClass of Positions * (((Ast * Ast option) * Ast list) * (Ast * Ast list option))
    | Ast.DefinitionClass((pos1, pos2),
                          (((predicateIdentifierAst, optUserDefinedObjSymAst), classTypeWithModifierListAsts),
                           (classContentAst, optPropertyListAsts))) ->
        evalSimpleSignature predicateIdentifierAst FplBlockType.Class pos1 pos2
        optUserDefinedObjSymAst |> Option.map (eval st) |> Option.defaultValue ()
        classTypeWithModifierListAsts |> List.map (eval st) |> ignore
        eval st classContentAst
        optPropertyListAsts
        |> Option.map (List.map (eval st) >> ignore)
        |> Option.defaultValue ()
    // | DerivedPredicate of Ast
    | Ast.DerivedPredicate ast -> eval st ast
    // | Proof of Positions * (Ast * (Ast list * Ast option))
    | Ast.Proof((pos1, pos2), (referencingIdentifierAst, (proofArgumentListAst, optQedAst))) ->
        evalSimpleSignature referencingIdentifierAst FplBlockType.Proof pos1 pos2
        proofArgumentListAst |> List.map (eval st) |> ignore
        optQedAst |> Option.map (eval st) |> Option.defaultValue ()
    | ast ->
        let astType = ast.GetType().Name

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
            st.CurrentContext <- EvalContext.InTheory pa.Id
            let theoryValue = FplValue.CreateTheory(pa.Id, (Position("", 0, 1, 1), Position("", 0, 1, 1)))
            st.Theories.Value.Add(pa.Id, theoryValue)
            eval st pa.Parsing.Ast
            pa.Status <- ParsedAstStatus.Evaluated
        | None -> found <- false
