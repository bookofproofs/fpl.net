module FplInterpreterBuildingBlocks
open System
open System.Collections.Generic
open FParsec
open ErrDiagnostics
open FplGrammarTypes
open FplInterpreterTypes

let eval_units (st:SymbolTable) = ""


let eval_string (st:SymbolTable) s = 
    match st.EvaluationContext with 
    | EvalContext.InSignature (startpos, endpos) -> 
        match st.Current with
        | Some pa -> 
            if pa.FplBlocks.FplBlockIds.ContainsKey(s) then 
                let diagnostic =
                    { 
                        Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter 
                        Diagnostic.Severity = DiagnosticSeverity.Error
                        Diagnostic.StartPos = startpos
                        Diagnostic.EndPos = endpos
                        Diagnostic.Code = ID001 s
                        Diagnostic.Alternatives = None 
                    }
                FplParser.parserDiagnostics.AddDiagnostic diagnostic 
            else
                pa.FplBlocks.FplBlockIds.Add(s,0)
        | None -> ()
    | EvalContext.ContextNone -> ()
    ""

let eval_pos_string (st:SymbolTable) (startpos:Position) (endpos:Position) ast = ""

let eval_pos_unit (st:SymbolTable) (startpos:Position) (endpos:Position) = ""

let eval_pos_ast (st:SymbolTable) (startpos:Position) (endpos:Position) = ""


let eval_pos_astlist (st:SymbolTable) (startpos:Position) (endpos:Position) = ""

let eval_pos_ast_ast_opt (st:SymbolTable) (startpos:Position) (endpos:Position) = ""

let eval_pos_char_list (st:SymbolTable) (startpos:Position) (endpos:Position) charlist = 
    charlist |> List.map string |> String.concat ""

let eval_pos_string_ast (st:SymbolTable) str = 
    str

/// A recursive function evaluating an AST and returning a list of EvalAliasedNamespaceIdentifier records
/// for each occurrence of the uses clause in the FPL code.
let rec eval (st:SymbolTable) ast = 
    match ast with
    // units: | Star
    | Ast.Star -> eval_units st
    | Ast.Dot -> eval_units st
    | Ast.LeftClosed -> eval_units st
    | Ast.LeftOpen -> eval_units st
    | Ast.RightClosed -> eval_units st
    | Ast.RightOpen -> eval_units st
    | Ast.One -> eval_units st
    | Ast.Many -> eval_units st
    | Ast.Many1 -> eval_units st
    | Ast.ObjectType -> eval_units st
    | Ast.PredicateType -> eval_units st
    | Ast.FunctionalTermType -> eval_units st
    | Ast.IndexType -> eval_units st
    | Ast.Property -> eval_units st
    | Ast.Optional -> eval_units st
    | Ast.Error -> eval_units st
    // strings: | Digits of string
    | Ast.Digits s -> eval_string st s
    | Ast.PascalCaseId s -> eval_string st s
    | Ast.ExtensionRegex s -> eval_string st s
    // | DollarDigits of Positions * string
    | Ast.DollarDigits ((pos1, pos2), s) -> eval_pos_string st pos1 pos2 s
    | Ast.DelegateId ((pos1, pos2), s) -> eval_pos_string st pos1 pos2 s
    | Ast.Alias ((pos1, pos2), s) -> eval_pos_string st pos1 pos2 s
    | Ast.LocalizationString ((pos1, pos2), s) -> eval_pos_string st pos1 pos2 s
    | Ast.Extensionname ((pos1, pos2), s) -> eval_pos_string st pos1 pos2 s
    | Ast.TemplateType ((pos1, pos2), s) -> eval_pos_string st pos1 pos2 s
    | Ast.Var ((pos1, pos2), s) -> eval_pos_string st pos1 pos2 s
    | Ast.ObjectSymbol ((pos1, pos2), s) -> eval_pos_string st pos1 pos2 s
    | Ast.ArgumentIdentifier ((pos1, pos2), s) -> eval_pos_string st pos1 pos2 s
    | Ast.Prefix ((pos1, pos2), s) -> eval_pos_string st pos1 pos2 s
    | Ast.Infix ((pos1, pos2), s) -> eval_pos_string st pos1 pos2 s
    | Ast.Postfix ((pos1, pos2), s) -> eval_pos_string st pos1 pos2 s
    | Ast.Symbol ((pos1, pos2), s) -> eval_pos_string st pos1 pos2 s
    | Ast.InfixOperator ((pos1, pos2), s) -> eval_pos_string st pos1 pos2 s
    | Ast.PostfixOperator ((pos1, pos2), s) -> eval_pos_string st pos1 pos2 s
    | Ast.PrefixOperator ((pos1, pos2), s) -> eval_pos_string st pos1 pos2 s
    // | Self of Positions * unit
    | Ast.Self ((pos1, pos2), _) -> eval_pos_unit st pos1 pos2
    | Ast.True ((pos1, pos2), _) -> eval_pos_unit st pos1 pos2
    | Ast.False ((pos1, pos2), _) -> eval_pos_unit st pos1 pos2
    | Ast.Undefined ((pos1, pos2), _) -> eval_pos_unit st pos1 pos2
    | Ast.Trivial ((pos1, pos2), _) -> eval_pos_unit st pos1 pos2
    | Ast.Qed ((pos1, pos2), _) -> eval_pos_unit st pos1 pos2
    // | ExtDigits of Positions * Ast
    | Ast.ExtDigits ((pos1, pos2), ast) -> 
        eval st ast |> ignore
        eval_pos_ast st pos1 pos2
    | Ast.ExtensionType ((pos1, pos2), ast) -> 
        eval st ast |> ignore
        eval_pos_ast st pos1 pos2
    | Ast.UsesClause ((pos1, pos2), ast) -> 
        eval st ast |> ignore
        eval_pos_ast st pos1 pos2
    | Ast.ClassIdentifier ((pos1, pos2), predicateIdentifierAst) -> 
        st.EvaluationContext <- EvalContext.InSignature (pos1, pos2)
        eval st predicateIdentifierAst |> ignore
        eval_pos_ast st pos1 pos2 |> ignore
        st.EvaluationContext <- EvalContext.ContextNone
        ""
    | Ast.SimpleVariableType ((pos1, pos2), ast) -> 
        eval st ast |> ignore
        eval_pos_ast st pos1 pos2
    | Ast.Not ((pos1, pos2), ast) -> 
        eval st ast |> ignore
        eval_pos_ast st pos1 pos2
    | Ast.Domain ((pos1, pos2), ast) -> 
        eval st ast |> ignore
        eval_pos_ast st pos1 pos2
    | Ast.Assertion ((pos1, pos2), ast) -> 
        eval st ast |> ignore
        eval_pos_ast st pos1 pos2
    | Ast.ByDef ((pos1, pos2), ast) -> 
        eval st ast |> ignore
        eval_pos_ast st pos1 pos2
    | Ast.DottedPredicate ((pos1, pos2), ast) -> 
        eval st ast |> ignore
        eval_pos_ast st pos1 pos2
    | Ast.Return ((pos1, pos2), ast) -> 
        eval st ast |> ignore
        eval_pos_ast st pos1 pos2
    | Ast.RuleOfInference ((pos1, pos2), ast) -> 
        eval st ast |> ignore
        eval_pos_ast st pos1 pos2
    | Ast.AssumeArgument ((pos1, pos2), ast) -> 
        eval st ast |> ignore
        eval_pos_ast st pos1 pos2
    | Ast.RevokeArgument ((pos1, pos2), ast) -> 
        eval st ast |> ignore
        eval_pos_ast st pos1 pos2
    | Ast.AST ((pos1, pos2), ast)  -> 
        eval st ast |> ignore
        eval_pos_ast st pos1 pos2
    // | NamespaceIdentifier of Positions * Ast list
    | Ast.NamespaceIdentifier ((pos1, pos2), asts) -> 
        asts |> List.map (eval st) |> ignore
        eval_pos_astlist st pos1 pos2 
    | Ast.PredicateIdentifier ((pos1, pos2), asts) -> 
        asts |> List.map (eval st) |> ignore
        eval_pos_astlist st pos1 pos2 
    | Ast.LocalizationTerm ((pos1, pos2), asts) -> 
        asts |> List.map (eval st) |> ignore
        eval_pos_astlist st pos1 pos2 
    | Ast.LocalizationTermList ((pos1, pos2), asts) -> 
        asts |> List.map (eval st) |> ignore
        eval_pos_astlist st pos1 pos2 
    | Ast.BrackedCoordList ((pos1, pos2), asts) -> 
        asts |> List.map (eval st) |> ignore
        eval_pos_astlist st pos1 pos2 
    | Ast.BracketedCoordsInType ((pos1, pos2), asts) -> 
        asts |> List.map (eval st) |> ignore
        eval_pos_astlist st pos1 pos2 
    | Ast.And ((pos1, pos2), asts) -> 
        asts |> List.map (eval st) |> ignore
        eval_pos_astlist st pos1 pos2 
    | Ast.Or ((pos1, pos2), asts) -> 
        asts |> List.map (eval st) |> ignore
        eval_pos_astlist st pos1 pos2 
    | Ast.Xor ((pos1, pos2), asts) -> 
        asts |> List.map (eval st) |> ignore
        eval_pos_astlist st pos1 pos2 
    | Ast.ParamTuple ((pos1, pos2), asts) -> 
        asts |> List.map (eval st) |> ignore
        eval_pos_astlist st pos1 pos2 
    | Ast.VarDeclBlock ((pos1, pos2), asts) -> 
        asts |> List.map (eval st) |> ignore
        eval_pos_astlist st pos1 pos2 
    | Ast.StatementList ((pos1, pos2), asts) -> 
        asts |> List.map (eval st) |> ignore
        eval_pos_astlist st pos1 pos2 
    | Ast.DefaultResult ((pos1, pos2), asts) -> 
        asts |> List.map (eval st) |> ignore
        eval_pos_astlist st pos1 pos2 
    | Ast.Justification ((pos1, pos2), asts) -> 
        asts |> List.map (eval st) |> ignore
        eval_pos_astlist st pos1 pos2 
    | Ast.ArgumentTuple ((pos1, pos2), asts) -> 
        asts |> List.map (eval st) |> ignore
        eval_pos_astlist st pos1 pos2 
    | Ast.QualificationList ((pos1, pos2), asts) -> 
        asts |> List.map (eval st) |> ignore
        eval_pos_astlist st pos1 pos2 
    // | Namespace of Ast option * Ast list
    | Ast.Namespace (optAst, asts) -> 
        optAst |> Option.map (eval st) |> ignore
        asts |> List.map (eval st) |> ignore
        ""
    // AliasedNamespaceIdentifier of Positions * (Ast * Ast option)
    | Ast.AliasedNamespaceIdentifier ((pos1, pos2), (ast, optAst)) -> 
        optAst |> Option.map (eval st) |> ignore
        eval st ast |> ignore
        eval_pos_ast_ast_opt st pos1 pos2
    | Ast.VariableType ((pos1, pos2), (ast, optAst)) -> 
        optAst |> Option.map (eval st) |> ignore
        eval st ast |> ignore
        eval_pos_ast_ast_opt st pos1 pos2
    | Ast.ClassType ((pos1, pos2), (ast, optAst)) -> 
        optAst |> Option.map (eval st) |> ignore
        eval st ast |> ignore
        eval_pos_ast_ast_opt st pos1 pos2
    | Ast.ReferenceToProofOrCorollary ((pos1, pos2), (ast, optAst)) -> 
        optAst |> Option.map (eval st) |> ignore
        eval st ast |> ignore
        eval_pos_ast_ast_opt st pos1 pos2
    | Ast.PredicateWithOptSpecification ((pos1, pos2), (ast, optAst)) -> 
        optAst |> Option.map (eval st) |> ignore
        eval st ast |> ignore
        eval_pos_ast_ast_opt st pos1 pos2
    // | SelfAts of Positions * char list 
    | Ast.SelfAts ((pos1, pos2), chars) -> 
        eval_pos_char_list st pos1 pos2 chars
    // | Translation of string * Ast
    | Ast.Translation (s, ast) -> 
        eval st ast |> ignore 
        eval_pos_string_ast st s
    // | ExtensionBlock of Positions * (Ast * Ast)
    | Ast.ExtensionBlock ((pos1, pos2), (ast1, ast2)) -> sprintf "ExtensionBlock at positions %A and %A: (%s, %s)" (pos1.ToString()) (pos2.ToString()) (eval st ast1) (eval st ast2)
    | Ast.ClassTypeWithModifier ((pos1, pos2), (ast1, ast2)) -> sprintf "ClassTypeWithModifier at positions %A and %A: (%s, %s)" (pos1.ToString()) (pos2.ToString()) (eval st ast1) (eval st ast2)
    | Ast.Impl ((pos1, pos2), (ast1, ast2)) -> sprintf "Impl at positions %A and %A: (%s, %s)" (pos1.ToString()) (pos2.ToString()) (eval st ast1) (eval st ast2)
    | Ast.Iif ((pos1, pos2), (ast1, ast2)) -> sprintf "Iif at positions %A and %A: (%s, %s)" (pos1.ToString()) (pos2.ToString()) (eval st ast1) (eval st ast2)
    | Ast.IsOperator ((pos1, pos2), (ast1, ast2)) -> sprintf "IsOperator at positions %A and %A: (%s, %s)" (pos1.ToString()) (pos2.ToString()) (eval st ast1) (eval st ast2)
    | Ast.Delegate ((pos1, pos2), (ast1, ast2)) -> sprintf "Delegate at positions %A and %A: (%s, %s)" (pos1.ToString()) (pos2.ToString()) (eval st ast1) (eval st ast2)
    // | ClosedOrOpenRange of Positions * ((Ast * Ast option) * Ast)
    | Ast.ClosedOrOpenRange ((pos1, pos2), ((ast1, optAst), ast2)) -> 
        let optAstStr = optAst |> Option.map (eval st) |> Option.defaultValue "None"
        sprintf "ClosedOrOpenRange at positions %A and %A: ((%s, %s), %s)" (pos1.ToString()) (pos2.ToString()) (eval st ast1) optAstStr (eval st ast2)
    | Ast.SignatureWithUserDefinedString ((pos1, pos2), ((predicateIdentifierAst, optUserDefinedSymbolAst), paramTupleAst)) -> 
        st.EvaluationContext <- EvalContext.InSignature (pos1, pos2)
        eval st predicateIdentifierAst |> ignore
        optUserDefinedSymbolAst |> Option.map (eval st) |> Option.defaultValue "None" |> ignore
        eval st paramTupleAst |> ignore
        st.EvaluationContext <- EvalContext.ContextNone
        ""
    | Ast.PropertyBlock ((pos1, pos2), ((ast, optAst), ast2)) -> 
        let optAstStr = optAst |> Option.map (eval st) |> Option.defaultValue "None"
        sprintf "PropertyBlock at positions %A and %A: ((%s, %s), %s)" (pos1.ToString()) (pos2.ToString()) (eval st ast) optAstStr (eval st ast2)
    // | RangeInType of Positions * (Ast option * Ast option) 
    | Ast.RangeInType ((pos1, pos2), (optAst1, optAst2)) -> 
        let optAstStr1 = optAst1 |> Option.map (eval st) |> Option.defaultValue "None"
        let optAstStr2 = optAst2 |> Option.map (eval st) |> Option.defaultValue "None"
        sprintf "RangeInType at positions %A and %A: (%s, %s)" (pos1.ToString()) (pos2.ToString()) optAstStr1 optAstStr2
    // | ReferencingIdentifier of Positions * (Ast * Ast list)
    | ReferencingIdentifier ((pos1, pos2), (ast, asts)) -> 
        let astsStr = asts |> List.map (eval st) |> String.concat ", "
        sprintf "ReferencingIdentifier at positions %A and %A: (%s, [%s])" (pos1.ToString()) (pos2.ToString()) (eval st ast) astsStr
    | Ast.ConditionFollowedByResult ((pos1, pos2), (ast, asts)) -> 
        let astsStr = asts |> List.map (eval st) |> String.concat ", "
        sprintf "ConditionFollowedByResult at positions %A and %A: (%s, [%s])" (pos1.ToString()) (pos2.ToString()) (eval st ast) astsStr
    | Ast.Localization ((pos1, pos2), (ast, asts)) -> 
        let astsStr = asts |> List.map (eval st) |> String.concat ", "
        sprintf "Localization at positions %A and %A: (%s, [%s])" (pos1.ToString()) (pos2.ToString()) (eval st ast) astsStr
    // | BoundedRangeInType of Positions * ((Ast * Ast) * Ast)
    | Ast.BoundedRangeInType ((pos1, pos2), ((ast1, ast2), ast3)) -> 
        sprintf "BoundedRangeInType at positions %A and %A: ((%s, %s), %s)" (pos1.ToString()) (pos2.ToString()) (eval st ast1) (eval st ast2) (eval st ast3)
    | Ast.ClassInstance ((pos1, pos2), ((ast1, ast2), ast3)) -> 
        sprintf "ClassInstance at positions %A and %A: ((%s, %s), %s)" (pos1.ToString()) (pos2.ToString()) (eval st ast1) (eval st ast2) (eval st ast3)
    | Ast.FunctionalTermInstance ((pos1, pos2), ((ast1, ast2), ast3)) -> 
        sprintf "FunctionalTermInstance at positions %A and %A: ((%s, %s), %s)" (pos1.ToString()) (pos2.ToString()) (eval st ast1) (eval st ast2) (eval st ast3)
    // | All of Positions * ((Ast list * Ast option) list * Ast)
    | Ast.All ((pos1, pos2), (astsOpts, ast)) -> 
        let astsOptsStr = astsOpts |> List.map (
            fun (asts, optAst) -> 
                let astsStr = asts |> List.map (eval st) |> String.concat ", "
                let optAstStr = optAst |> Option.map (eval st) |> Option.defaultValue "None"
                sprintf "(%s, %s)" astsStr optAstStr ) |> String.concat ", "
        sprintf "All at positions %A and %A: (%s, %s)" (pos1.ToString()) (pos2.ToString()) astsOptsStr (eval st ast)
    | Ast.Exists ((pos1, pos2), (astsOpts, ast)) -> 
        let astsOptsStr = astsOpts |> List.map (
            fun (asts, optAst) -> 
                let astsStr = asts |> List.map (eval st) |> String.concat ", "
                let optAstStr = optAst |> Option.map (eval st) |> Option.defaultValue "None"
                sprintf "(%s, %s)" astsStr optAstStr ) |> String.concat ", "
        sprintf "Exists at positions %A and %A: (%s, %s)" (pos1.ToString()) (pos2.ToString()) astsOptsStr (eval st ast)
    
    // | ExistsN of Positions * ((Ast * (Ast * Ast option)) * Ast)
    | Ast.ExistsN ((pos1, pos2), ((ast1, (ast2, optAst)), ast3)) -> 
        let optAstStr = optAst |> Option.map (eval st) |> Option.defaultValue "None"
        sprintf "ExistsN at positions %A and %A: ((%s, (%s, %s)), %s)" (pos1.ToString()) (pos2.ToString()) (eval st ast1) (eval st ast2) optAstStr (eval st ast3)
    // | PredicateWithQualification of (Ast * Ast) 
    | Ast.PredicateWithQualification (ast1, ast2) -> sprintf "PredicateWithQualification (%s, %s)" (eval st ast1) (eval st ast2)
    // | InfixOperation of Positions * (Ast * Ast option) list
    | Ast.InfixOperation ((pos1, pos2), astsOpts) -> 
        let astsOptsStr = astsOpts |> List.map (fun (ast, optAst) -> 
            let optAstStr = optAst |> Option.map (eval st) |> Option.defaultValue "None"
            sprintf "(%s, %s)" (eval st ast) optAstStr ) |> String.concat ", "
        sprintf "InfixOperation at positions %A and %A: [%s]" (pos1.ToString()) (pos2.ToString()) astsOptsStr
    // | Expression of Positions * ((((Ast option * Ast) * Ast option) * Ast option) * Ast)
    | Ast.Expression ((pos1, pos2), ((((optAst1, ast1), optAst2), optAst3), ast2)) -> 
        let optAstStr1 = optAst1 |> Option.map (eval st) |> Option.defaultValue "None"
        let optAstStr2 = optAst2 |> Option.map (eval st) |> Option.defaultValue "None"
        let optAstStr3 = optAst3 |> Option.map (eval st) |> Option.defaultValue "None"
        sprintf "Expression at positions %A and %A: ((((%s, %s), %s), %s), %s)" (pos1.ToString()) (pos2.ToString()) optAstStr1 (eval st ast1) optAstStr2 optAstStr3 (eval st ast2)
    // | Cases of Positions * (Ast list * Ast)
    | Ast.Cases ((pos1, pos2), (asts, ast)) -> 
        let astsStr = asts |> List.map (eval st) |> String.concat ", "
        sprintf "Cases at positions %A and %A: ([%s], %s)" (pos1.ToString()) (pos2.ToString()) astsStr (eval st ast)
    // | Assignment of Positions * (Ast * Ast)
    | Ast.Assignment ((pos1, pos2), (ast1, ast2)) -> sprintf "Assignment at positions %A and %A: (%s, %s)" (pos1.ToString()) (pos2.ToString()) (eval st ast1) (eval st ast2)
    | Ast.PredicateInstance ((pos1, pos2), (ast1, ast2)) -> sprintf "PredicateInstance at positions %A and %A: (%s, %s)" (pos1.ToString()) (pos2.ToString()) (eval st ast1) (eval st ast2)
    | Ast.ParentConstructorCall ((pos1, pos2), (ast1, ast2)) -> sprintf "ParentConstructorCall at positions %A and %A: (%s, %s)" (pos1.ToString()) (pos2.ToString()) (eval st ast1) (eval st ast2)
    | Ast.Signature ((pos1, pos2), (ast1, ast2)) -> 
        st.EvaluationContext <- EvalContext.InSignature (pos1, pos2)
        eval st ast1 |> ignore 
        eval st ast2 |> ignore
        st.EvaluationContext <- EvalContext.ContextNone
        ""
    | Ast.JustifiedArgument ((pos1, pos2), (ast1, ast2)) -> sprintf "JustifiedArgument at positions %A and %A: (%s, %s)" (pos1.ToString()) (pos2.ToString()) (eval st ast1) (eval st ast2)
    | Ast.Argument ((pos1, pos2), (ast1, ast2)) -> sprintf "Argument at positions %A and %A: (%s, %s)" (pos1.ToString()) (pos2.ToString()) (eval st ast1) (eval st ast2)
    // | ForIn of Positions * ((Ast * Ast) * Ast list)
    | Ast.ForIn ((pos1, pos2), ((ast1, ast2), asts)) -> 
        let astsStr = asts |> List.map (eval st) |> String.concat ", "
        sprintf "ForIn at positions %A and %A: ((%s, %s), [%s])" (pos1.ToString()) (pos2.ToString()) (eval st ast1) (eval st ast2) astsStr
    // | SignatureWithPreConBlock of Ast * ((Ast list option * Ast) * Ast)
    | Ast.SignatureWithPreConBlock (ast, ((optAsts, ast2), ast3)) -> 
        let optAstsStr = optAsts |> Option.map (List.map (eval st) >> String.concat ", ") |> Option.defaultValue "None"
        sprintf "SignatureWithPreConBlock (%s, ((%s, %s), %s))" (eval st ast) optAstsStr (eval st ast2) (eval st ast3)
    // | Theorem of Positions * (Ast *(Ast list option * Ast))
    | Ast.Theorem ((pos1, pos2), (ast, (Some astList, ast2))) ->
        let astStr = eval st ast
        let astListStr = astList |> List.map (eval st) |> String.concat ", "
        let ast2Str = eval st ast2
        sprintf "Theorem at positions %A and %A: [%s, [%s], %s]" (pos1.ToString()) (pos2.ToString()) astStr astListStr ast2Str
    | Ast.Theorem ((pos1, pos2), (ast, (None, ast2))) ->
        let astStr = eval st ast
        let ast2Str = eval st ast2
        sprintf "Theorem at positions %A and %A: [%s, None, %s]" (pos1.ToString()) (pos2.ToString()) astStr ast2Str
    | Ast.Lemma ((pos1, pos2), (ast, (Some astList, ast2))) ->
        let astStr = eval st ast
        let astListStr = astList |> List.map (eval st) |> String.concat ", "
        let ast2Str = eval st ast2
        sprintf "Lemma at positions %A and %A: [%s, [%s], %s]" (pos1.ToString()) (pos2.ToString()) astStr astListStr ast2Str
    | Ast.Lemma ((pos1, pos2), (ast, (None, ast2))) ->
        let astStr = eval st ast
        let ast2Str = eval st ast2
        sprintf "Lemma at positions %A and %A: [%s, None, %s]" (pos1.ToString()) (pos2.ToString()) astStr ast2Str
    | Ast.Proposition ((pos1, pos2), (ast, (Some astList, ast2))) ->
        let astStr = eval st ast
        let astListStr = astList |> List.map (eval st) |> String.concat ", "
        let ast2Str = eval st ast2
        sprintf "Proposition at positions %A and %A: [%s, [%s], %s]" (pos1.ToString()) (pos2.ToString()) astStr astListStr ast2Str
    | Ast.Proposition ((pos1, pos2), (ast, (None, ast2))) ->
        let astStr = eval st ast
        let ast2Str = eval st ast2
        sprintf "Proposition at positions %A and %A: [%s, None, %s]" (pos1.ToString()) (pos2.ToString()) astStr ast2Str
    | Ast.Conjecture ((pos1, pos2), (ast, (Some astList, ast2))) ->
        let astStr = eval st ast
        let astListStr = astList |> List.map (eval st) |> String.concat ", "
        let ast2Str = eval st ast2
        sprintf "Conjecture at positions %A and %A: [%s, [%s], %s]" (pos1.ToString()) (pos2.ToString()) astStr astListStr ast2Str
    | Ast.Conjecture ((pos1, pos2), (ast, (None, ast2))) ->
        let astStr = eval st ast
        let ast2Str = eval st ast2
        sprintf "Conjecture at positions %A and %A: [%s, None, %s]" (pos1.ToString()) (pos2.ToString()) astStr ast2Str
    // | Corollary of Positions * ((Ast * Ast) * (Ast list option * Ast))
    | Ast.Corollary ((pos1, pos2), ((ast1, ast2), (Some astList, ast3))) ->
        let ast1Str = eval st ast1
        let ast2Str = eval st ast2
        let astListStr = astList |> List.map (eval st) |> String.concat ", "
        let ast3Str = eval st ast3
        sprintf "Corollary at positions %A and %A: [[%s, %s], [%s], %s]" (pos1.ToString()) (pos2.ToString()) ast1Str ast2Str astListStr ast3Str
    | Ast.Corollary ((pos1, pos2), ((ast1, ast2), (None, ast3))) ->
        let ast1Str = eval st ast1
        let ast2Str = eval st ast2
        let ast3Str = eval st ast3
        sprintf "Corollary at positions %A and %A: [[%s, %s], None, %s]" (pos1.ToString()) (pos2.ToString()) ast1Str ast2Str ast3Str
    // | NamedVarDecl of Positions * ((Ast list * Ast) * Ast) 
    | Ast.NamedVarDecl ((pos1, pos2), ((asts, ast), ast2)) -> 
        let astsStr = asts |> List.map (eval st) |> String.concat ", "
        sprintf "NamedVarDecl at positions %A and %A: ((%s, %s), %s)" (pos1.ToString()) (pos2.ToString()) astsStr (eval st ast) (eval st ast2)
    // | Axiom of Positions * (Ast * (Ast list option * Ast))
    | Ast.Axiom ((pos1, pos2), (ast, (Some astList, ast2))) ->
        let astStr = eval st ast
        let astListStr = astList |> List.map (eval st) |> String.concat ", "
        let ast2Str = eval st ast2
        sprintf "Axiom at positions %A and %A: [%s, [%s], %s]" (pos1.ToString()) (pos2.ToString()) astStr astListStr ast2Str
    | Ast.Axiom ((pos1, pos2), (ast, (None, ast2))) ->
        let astStr = eval st ast
        let ast2Str = eval st ast2
        sprintf "Axiom at positions %A and %A: [%s, None, %s]" (pos1.ToString()) (pos2.ToString()) astStr ast2Str
    | Ast.Constructor ((pos1, pos2), (ast, (Some astList, ast2))) ->
        let astStr = eval st ast
        let astListStr = astList |> List.map (eval st) |> String.concat ", "
        let ast2Str = eval st ast2
        sprintf "Constructor at positions %A and %A: [%s, [%s], %s]" (pos1.ToString()) (pos2.ToString()) astStr astListStr ast2Str
    | Ast.Constructor ((pos1, pos2), (ast, (None, ast2))) ->
        let astStr = eval st ast
        let ast2Str = eval st ast2
        sprintf "Constructor at positions %A and %A: [%s, None, %s]" (pos1.ToString()) (pos2.ToString()) astStr ast2Str
    // | DefPredicateContent of Ast list option * Ast
    | Ast.DefPredicateContent (optAsts, ast) -> 
        let optAstsStr = optAsts |> Option.map (List.map (eval st) >> String.concat ", ") |> Option.defaultValue "None"
        sprintf "DefPredicateContent (%s, %s)" optAstsStr (eval st ast)
    | Ast.DefFunctionContent (optAsts, ast) -> 
        let optAstsStr = optAsts |> Option.map (List.map (eval st) >> String.concat ", ") |> Option.defaultValue "None"
        sprintf "DefFunctionContent (%s, %s)" optAstsStr (eval st ast)
    | Ast.DefClassContent (optAsts, ast) -> 
        let optAstsStr = optAsts |> Option.map (List.map (eval st) >> String.concat ", ") |> Option.defaultValue "None"
        sprintf "DefClassContent (%s, %s)" optAstsStr (eval st ast)
    // | DefClassCompleteContent of Ast list option * Ast list
    | Ast.DefClassCompleteContent (optAsts, asts) -> 
        let optAstsStr = optAsts |> Option.map (List.map (eval st) >> String.concat ", ") |> Option.defaultValue "None"
        let astsStr = asts |> List.map (eval st) |> String.concat ", "
        sprintf "DefClassCompleteContent (%s, [%s])" optAstsStr astsStr
    // | DefinitionPredicate of Positions * (Ast * (Ast * Ast list option))
    | Ast.DefinitionPredicate ((pos1, pos2), (ast, (ast2, optAsts))) -> 
        let optAstsStr = optAsts |> Option.map (List.map (eval st) >> String.concat ", ") |> Option.defaultValue "None"
        sprintf "DefinitionPredicate at positions %A and %A: (%s, (%s, %s))" (pos1.ToString()) (pos2.ToString()) (eval st ast) (eval st ast2) optAstsStr
    // | DefinitionFunctionalTerm of Positions * ((Ast * Ast) * (Ast * Ast list option))
    | Ast.DefinitionFunctionalTerm ((pos1, pos2), ((ast1, ast2), (ast3, optAsts))) -> 
        let optAstsStr = optAsts |> Option.map (List.map (eval st) >> String.concat ", ") |> Option.defaultValue "None"
        sprintf "DefinitionFunctionalTerm at positions %A and %A: ((%s, %s), (%s, %s))" (pos1.ToString()) (pos2.ToString()) (eval st ast1) (eval st ast2) (eval st ast3) optAstsStr
    // | DefinitionClass of Positions * (((Ast * Ast option) * Ast list) * (Ast * Ast list option)) 
    | Ast.DefinitionClass ((pos1, pos2), (((predicateIdentifierAst, optUserDefinedObjSymAst), classTypeWithModifierListAsts), (classContentAst, optPropertyListAsts))) -> 
        let optAstStr = optUserDefinedObjSymAst |> Option.map (eval st) |> Option.defaultValue "None"
        let astsStr = classTypeWithModifierListAsts |> List.map (eval st) |> String.concat ", "
        let optAstsStr = optPropertyListAsts |> Option.map (List.map (eval st) >> String.concat ", ") |> Option.defaultValue "None"
        sprintf "DefinitionClass at positions %A and %A: (((%s, %s), [%s]), (%s, %s))" (pos1.ToString()) (pos2.ToString()) (eval st predicateIdentifierAst) optAstStr astsStr (eval st classContentAst) optAstsStr
    // | DerivedPredicate of Ast
    | Ast.DerivedPredicate ast -> sprintf "DerivedPredicate %s" (eval st ast)

    // | Proof of Positions * (Ast * (Ast list * Ast option))
    | Ast.Proof ((pos1, pos2), (ast, (asts, optAst))) -> 
        let astsStr = asts |> List.map (eval st) |> String.concat ", "
        let optAstStr = optAst |> Option.map (eval st) |> Option.defaultValue "None"
        sprintf "Proof at positions %A and %A: (%s, ([%s], %s))" (pos1.ToString()) (pos2.ToString()) (eval st ast) astsStr optAstStr

    | ast -> 
        let astType = ast.GetType().Name
        sprintf "handling ast type %s not yet implemented" astType


let tryFindParsedAstUsesClausesEvaluated (parsedAsts:List<ParsedAst>) =
    if parsedAsts.Exists(fun pa -> pa.Status = ParsedAstStatus.UsesClausesEvaluated) then
        Some(parsedAsts.Find(fun pa -> pa.Status = ParsedAstStatus.UsesClausesEvaluated))
    else
        None
        
let evaluateSymbolTable (st:SymbolTable) = 
    // there is a valid topological sorting, order the list descending by this ordering
    st.ParsedAsts.Sort(Comparer<ParsedAst>.Create(fun b a -> compare a.Sorting.TopologicalSorting b.Sorting.TopologicalSorting))
    let mutable found = true
    while found do
        let usesClausesEvaluatedParsedAst = tryFindParsedAstUsesClausesEvaluated st.ParsedAsts
        match usesClausesEvaluatedParsedAst with
        | Some pa -> 
            // evaluate the ParsedAst
            pa.Status <- ParsedAstStatus.Evaluated
            st.Current <- Some pa
            eval st pa.Parsing.Ast |> ignore
        | None -> 
            found <- false
