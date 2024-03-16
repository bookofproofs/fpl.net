module FplInterpreter
open FplGrammarTypes
open FplParser
open FplInterpreterUsesClause
open FParsec

type SymbolTable =
    { ParsedAsts: ParsedAst list }

let fplInterpreter ast uri fplLibUrl = 
    let ad = FplParser.parserDiagnostics
    let parsedAsts = FplInterpreterUsesClause.findParsedAstsMatchingAliasedNamespaceIdentifier 0 ast ad uri fplLibUrl
    let symbolTable = { ParsedAsts = parsedAsts }
    symbolTable

let rec eval = function
    // units: | Star
    | Ast.Star -> sprintf "Star"
    | Ast.Dot -> sprintf "Dot"
    | Ast.LeftClosed -> sprintf "LeftClosed"
    | Ast.LeftOpen -> sprintf "LeftOpen" 
    | Ast.RightClosed -> sprintf "RightClosed"
    | Ast.RightOpen -> sprintf "RightOpen"
    | Ast.One -> sprintf "One"
    | Ast.Many -> sprintf "Many"
    | Ast.Many1 -> sprintf "Many1"
    | Ast.ObjectType -> sprintf "ObjectType"
    | Ast.PredicateType -> sprintf "PredicateType"
    | Ast.FunctionalTermType -> sprintf "FunctionalTermType"
    | Ast.IndexType -> sprintf "IndexType"
    | Ast.Property -> sprintf "Property"
    | Ast.Optional -> sprintf "Optional"
    | Ast.Error -> sprintf "Error"
    // strings: | Digits of string
    | Ast.Digits s -> sprintf "Digits %s" s
    | Ast.PascalCaseId s -> sprintf "PascalCaseId %s" s
    | Ast.ExtensionRegex s -> sprintf "ExtensionRegex %s" s
    // | DollarDigits of Positions * string
    | Ast.DollarDigits ((pos1, pos2), s) -> sprintf "DollarDigits %s at positions %A and %A" s (pos1.ToString()) (pos2.ToString())
    | Ast.DelegateId ((pos1, pos2), s) -> sprintf "DelegateId %s at positions %A and %A" s (pos1.ToString()) (pos2.ToString())
    | Ast.Alias ((pos1, pos2), s) -> sprintf "Alias %s at positions %A and %A" s (pos1.ToString()) (pos2.ToString())
    | Ast.LocalizationString ((pos1, pos2), s) -> sprintf "LocalizationString %s at positions %A and %A" s (pos1.ToString()) (pos2.ToString())
    | Ast.Extensionname ((pos1, pos2), s) -> sprintf "Extensionname %s at positions %A and %A" s (pos1.ToString()) (pos2.ToString())
    | Ast.TemplateType ((pos1, pos2), s) -> sprintf "TemplateType %s at positions %A and %A" s (pos1.ToString()) (pos2.ToString())
    | Ast.Var ((pos1, pos2), s) -> sprintf "Var %s at positions %A and %A" s (pos1.ToString()) (pos2.ToString())
    | Ast.ObjectSymbol ((pos1, pos2), s) -> sprintf "ObjectSymbol %s at positions %A and %A" s (pos1.ToString()) (pos2.ToString())
    | Ast.ArgumentIdentifier ((pos1, pos2), s) -> sprintf "ArgumentIdentifier %s at positions %A and %A" s (pos1.ToString()) (pos2.ToString())
    | Ast.Prefix ((pos1, pos2), s) -> sprintf "Prefix %s at positions %A and %A" s (pos1.ToString()) (pos2.ToString())
    | Ast.Infix ((pos1, pos2), s) -> sprintf "Infix %s at positions %A and %A" s (pos1.ToString()) (pos2.ToString())
    | Ast.Postfix ((pos1, pos2), s) -> sprintf "Postfix %s at positions %A and %A" s (pos1.ToString()) (pos2.ToString())
    | Ast.Symbol ((pos1, pos2), s) -> sprintf "Symbol %s at positions %A and %A" s (pos1.ToString()) (pos2.ToString())
    | Ast.InfixOperator ((pos1, pos2), s) -> sprintf "InfixOperator %s at positions %A and %A" s (pos1.ToString()) (pos2.ToString())
    | Ast.PostfixOperator ((pos1, pos2), s) -> sprintf "PostfixOperator %s at positions %A and %A" s (pos1.ToString()) (pos2.ToString())
    | Ast.PrefixOperator ((pos1, pos2), s) -> sprintf "PrefixOperator %s at positions %A and %A" s (pos1.ToString()) (pos2.ToString())
    // | Self of Positions * unit
    | Ast.Self ((pos1, pos2), _) -> sprintf "Self at positions %A and %A" (pos1.ToString()) (pos2.ToString())
    | Ast.True ((pos1, pos2), _) -> sprintf "True at positions %A and %A" (pos1.ToString()) (pos2.ToString())
    | Ast.False ((pos1, pos2), _) -> sprintf "False at positions %A and %A" (pos1.ToString()) (pos2.ToString())
    | Ast.Undefined ((pos1, pos2), _) -> sprintf "Undefined at positions %A and %A" (pos1.ToString()) (pos2.ToString())
    | Ast.Trivial ((pos1, pos2), _) -> sprintf "Trivial at positions %A and %A" (pos1.ToString()) (pos2.ToString())
    | Ast.Qed ((pos1, pos2), _) -> sprintf "Qed at positions %A and %A" (pos1.ToString()) (pos2.ToString())
    // | ExtDigits of Positions * Ast
    | Ast.ExtDigits ((pos1, pos2), ast) -> sprintf "ExtDigits at positions %A and %A: %s" (pos1.ToString()) (pos2.ToString()) (eval ast)
    | Ast.ExtensionType ((pos1, pos2), ast) -> sprintf "ExtensionType at positions %A and %A: %s" (pos1.ToString()) (pos2.ToString()) (eval ast)
    | Ast.UsesClause ((pos1, pos2), ast) -> sprintf "UsesClause at positions %A and %A: %s" (pos1.ToString()) (pos2.ToString()) (eval ast)
    | Ast.ClassIdentifier ((pos1, pos2), ast) -> sprintf "ClassIdentifier at positions %A and %A: %s" (pos1.ToString()) (pos2.ToString()) (eval ast)
    | Ast.SimpleVariableType ((pos1, pos2), ast) -> sprintf "SimpleVariableType at positions %A and %A: %s" (pos1.ToString()) (pos2.ToString()) (eval ast) 
    | Ast.Not ((pos1, pos2), ast) -> sprintf "Not at positions %A and %A: %s" (pos1.ToString()) (pos2.ToString()) (eval ast)
    | Ast.Domain ((pos1, pos2), ast) -> sprintf "Domain at positions %A and %A: %s" (pos1.ToString()) (pos2.ToString()) (eval ast)
    | Ast.Assertion ((pos1, pos2), ast) -> sprintf "Assertion at positions %A and %A: %s" (pos1.ToString()) (pos2.ToString()) (eval ast)
    | Ast.ByDef ((pos1, pos2), ast) -> sprintf "ByDef at positions %A and %A: %s" (pos1.ToString()) (pos2.ToString()) (eval ast)
    | Ast.DottedPredicate ((pos1, pos2), ast) -> sprintf "DottedPredicate at positions %A and %A: %s" (pos1.ToString()) (pos2.ToString()) (eval ast) 
    | Ast.Return ((pos1, pos2), ast) -> sprintf "Return at positions %A and %A: %s" (pos1.ToString()) (pos2.ToString()) (eval ast)
    | Ast.RuleOfInference ((pos1, pos2), ast) -> sprintf "RuleOfInference at positions %A and %A: %s" (pos1.ToString()) (pos2.ToString()) (eval ast)
    | Ast.AssumeArgument ((pos1, pos2), ast) -> sprintf "AssumeArgument at positions %A and %A: %s" (pos1.ToString()) (pos2.ToString()) (eval ast)
    | Ast.RevokeArgument ((pos1, pos2), ast) -> sprintf "RevokeArgument at positions %A and %A: %s" (pos1.ToString()) (pos2.ToString()) (eval ast)
    | Ast.AST ((pos1, pos2), ast) -> sprintf "AST at positions %A and %A: %s" (pos1.ToString()) (pos2.ToString()) (eval ast)
    // | NamespaceIdentifier of Positions * Ast list
    | Ast.NamespaceIdentifier ((pos1, pos2), asts) -> 
        let astsStr = asts |> List.map eval |> String.concat ", "
        sprintf "NamespaceIdentifier at positions %A and %A: [%s]" (pos1.ToString()) (pos2.ToString()) astsStr
    | Ast.PredicateIdentifier ((pos1, pos2), asts) -> 
        let astsStr = asts |> List.map eval |> String.concat ", "
        sprintf "PredicateIdentifier at positions %A and %A: [%s]" (pos1.ToString()) (pos2.ToString()) astsStr
    | Ast.LocalizationTerm ((pos1, pos2), asts) -> 
        let astsStr = asts |> List.map eval |> String.concat ", "
        sprintf "LocalizationTerm at positions %A and %A: [%s]" (pos1.ToString()) (pos2.ToString()) astsStr
    | Ast.LocalizationTermList ((pos1, pos2), asts) -> 
        let astsStr = asts |> List.map eval |> String.concat ", "
        sprintf "LocalizationTermList at positions %A and %A: [%s]" (pos1.ToString()) (pos2.ToString()) astsStr
    | Ast.BrackedCoordList ((pos1, pos2), asts) -> 
        let astsStr = asts |> List.map eval |> String.concat ", "
        sprintf "BrackedCoordList at positions %A and %A: [%s]" (pos1.ToString()) (pos2.ToString()) astsStr
    | Ast.BracketedCoordsInType ((pos1, pos2), asts) -> 
        let astsStr = asts |> List.map eval |> String.concat ", "
        sprintf "BracketedCoordsInType at positions %A and %A: [%s]" (pos1.ToString()) (pos2.ToString()) astsStr
    | Ast.And ((pos1, pos2), asts) -> 
        let astsStr = asts |> List.map eval |> String.concat ", "
        sprintf "And at positions %A and %A: [%s]" (pos1.ToString()) (pos2.ToString()) astsStr
    | Ast.Or ((pos1, pos2), asts) -> 
        let astsStr = asts |> List.map eval |> String.concat ", "
        sprintf "Or at positions %A and %A: [%s]" (pos1.ToString()) (pos2.ToString()) astsStr
    | Ast.Xor ((pos1, pos2), asts) -> 
        let astsStr = asts |> List.map eval |> String.concat ", "
        sprintf "Xor at positions %A and %A: [%s]" (pos1.ToString()) (pos2.ToString()) astsStr
    | Ast.ParamTuple ((pos1, pos2), asts) -> 
        let astsStr = asts |> List.map eval |> String.concat ", "
        sprintf "ParamTuple at positions %A and %A: [%s]" (pos1.ToString()) (pos2.ToString()) astsStr
    | Ast.VarDeclBlock ((pos1, pos2), asts) -> 
        let astsStr = asts |> List.map eval |> String.concat ", "
        sprintf "VarDeclBlock at positions %A and %A: [%s]" (pos1.ToString()) (pos2.ToString()) astsStr
    | Ast.StatementList ((pos1, pos2), asts) -> 
        let astsStr = asts |> List.map eval |> String.concat ", "
        sprintf "StatementList at positions %A and %A: [%s]" (pos1.ToString()) (pos2.ToString()) astsStr
    | Ast.DefaultResult ((pos1, pos2), asts) -> 
        let astsStr = asts |> List.map eval |> String.concat ", "
        sprintf "DefaultResult at positions %A and %A: [%s]" (pos1.ToString()) (pos2.ToString()) astsStr
    | Ast.Justification ((pos1, pos2), asts) -> 
        let astsStr = asts |> List.map eval |> String.concat ", "
        sprintf "Justification at positions %A and %A: [%s]" (pos1.ToString()) (pos2.ToString()) astsStr
    | Ast.ArgumentTuple ((pos1, pos2), asts) -> 
        let astsStr = asts |> List.map eval |> String.concat ", "
        sprintf "ArgumentTuple at positions %A and %A: [%s]" (pos1.ToString()) (pos2.ToString()) astsStr
    | Ast.QualificationList ((pos1, pos2), asts) -> 
        let astsStr = asts |> List.map eval |> String.concat ", "
        sprintf "QualificationList at positions %A and %A: [%s]" (pos1.ToString()) (pos2.ToString()) astsStr
    // | Namespace of Ast option * Ast list
    | Ast.Namespace (optAst, asts) -> 
        let optAstStr = optAst |> Option.map eval |> Option.defaultValue "None"
        let astsStr = asts |> List.map eval |> String.concat ", "
        sprintf "Namespace (%s, [%s])" optAstStr astsStr
    // AliasedNamespaceIdentifier of Positions * (Ast * Ast option)
    | Ast.AliasedNamespaceIdentifier ((pos1, pos2), (ast, optAst)) -> 
        let optAstStr = optAst |> Option.map eval |> Option.defaultValue "None"
        sprintf "AliasedNamespaceIdentifier at positions %A and %A: (%s, %s)" (pos1.ToString()) (pos2.ToString()) (eval ast) optAstStr
    | Ast.VariableType ((pos1, pos2), (ast, optAst)) -> 
        let optAstStr = optAst |> Option.map eval |> Option.defaultValue "None"
        sprintf "VariableType at positions %A and %A: (%s, %s)" (pos1.ToString()) (pos2.ToString()) (eval ast) optAstStr
    | Ast.ClassType ((pos1, pos2), (ast, optAst)) -> 
        let optAstStr = optAst |> Option.map eval |> Option.defaultValue "None"
        sprintf "ClassType at positions %A and %A: (%s, %s)" (pos1.ToString()) (pos2.ToString()) (eval ast) optAstStr
    | Ast.ReferenceToProofOrCorollary ((pos1, pos2), (ast, optAst)) -> 
        let optAstStr = optAst |> Option.map eval |> Option.defaultValue "None"
        sprintf "ReferenceToProofOrCorollary at positions %A and %A: (%s, %s)" (pos1.ToString()) (pos2.ToString()) (eval ast) optAstStr
    | Ast.PredicateWithOptSpecification ((pos1, pos2), (ast, optAst)) -> 
        let optAstStr = optAst |> Option.map eval |> Option.defaultValue "None"
        sprintf "PredicateWithOptSpecification at positions %A and %A: (%s, %s)" (pos1.ToString()) (pos2.ToString()) (eval ast) optAstStr
    // | SelfAts of Positions * char list 
    | Ast.SelfAts ((pos1, pos2), chars) -> 
        let charsStr = chars |> List.map string |> String.concat ""
        sprintf "SelfAts at positions %A and %A: %s" (pos1.ToString()) (pos2.ToString()) charsStr
    // | Translation of string * Ast
    | Ast.Translation (s, ast) -> sprintf "Translation %s: %s" s (eval ast)
    // | ExtensionBlock of Positions * (Ast * Ast)
    | Ast.ExtensionBlock ((pos1, pos2), (ast1, ast2)) -> sprintf "ExtensionBlock at positions %A and %A: (%s, %s)" (pos1.ToString()) (pos2.ToString()) (eval ast1) (eval ast2)
    | Ast.ClassTypeWithModifier ((pos1, pos2), (ast1, ast2)) -> sprintf "ClassTypeWithModifier at positions %A and %A: (%s, %s)" (pos1.ToString()) (pos2.ToString()) (eval ast1) (eval ast2)
    | Ast.Impl ((pos1, pos2), (ast1, ast2)) -> sprintf "Impl at positions %A and %A: (%s, %s)" (pos1.ToString()) (pos2.ToString()) (eval ast1) (eval ast2)
    | Ast.Iif ((pos1, pos2), (ast1, ast2)) -> sprintf "Iif at positions %A and %A: (%s, %s)" (pos1.ToString()) (pos2.ToString()) (eval ast1) (eval ast2)
    | Ast.IsOperator ((pos1, pos2), (ast1, ast2)) -> sprintf "IsOperator at positions %A and %A: (%s, %s)" (pos1.ToString()) (pos2.ToString()) (eval ast1) (eval ast2)
    | Ast.Delegate ((pos1, pos2), (ast1, ast2)) -> sprintf "Delegate at positions %A and %A: (%s, %s)" (pos1.ToString()) (pos2.ToString()) (eval ast1) (eval ast2)
    // | ClosedOrOpenRange of Positions * ((Ast * Ast option) * Ast)
    | Ast.ClosedOrOpenRange ((pos1, pos2), ((ast1, optAst), ast2)) -> 
        let optAstStr = optAst |> Option.map eval |> Option.defaultValue "None"
        sprintf "ClosedOrOpenRange at positions %A and %A: ((%s, %s), %s)" (pos1.ToString()) (pos2.ToString()) (eval ast1) optAstStr (eval ast2)
    | Ast.SignatureWithUserDefinedString ((pos1, pos2), ((ast, optAst), ast2)) -> 
        let optAstStr = optAst |> Option.map eval |> Option.defaultValue "None"
        sprintf "SignatureWithUserDefinedString at positions %A and %A: ((%s, %s), %s)" (pos1.ToString()) (pos2.ToString()) (eval ast) optAstStr (eval ast2)
    | Ast.PropertyBlock ((pos1, pos2), ((ast, optAst), ast2)) -> 
        let optAstStr = optAst |> Option.map eval |> Option.defaultValue "None"
        sprintf "PropertyBlock at positions %A and %A: ((%s, %s), %s)" (pos1.ToString()) (pos2.ToString()) (eval ast) optAstStr (eval ast2)
    // | RangeInType of Positions * (Ast option * Ast option) 
    | Ast.RangeInType ((pos1, pos2), (optAst1, optAst2)) -> 
        let optAstStr1 = optAst1 |> Option.map eval |> Option.defaultValue "None"
        let optAstStr2 = optAst2 |> Option.map eval |> Option.defaultValue "None"
        sprintf "RangeInType at positions %A and %A: (%s, %s)" (pos1.ToString()) (pos2.ToString()) optAstStr1 optAstStr2
    // | ReferencingIdentifier of Positions * (Ast * Ast list)
    | ReferencingIdentifier ((pos1, pos2), (ast, asts)) -> 
        let astsStr = asts |> List.map eval |> String.concat ", "
        sprintf "ReferencingIdentifier at positions %A and %A: (%s, [%s])" (pos1.ToString()) (pos2.ToString()) (eval ast) astsStr
    | Ast.ConditionFollowedByResult ((pos1, pos2), (ast, asts)) -> 
        let astsStr = asts |> List.map eval |> String.concat ", "
        sprintf "ConditionFollowedByResult at positions %A and %A: (%s, [%s])" (pos1.ToString()) (pos2.ToString()) (eval ast) astsStr
    | Ast.Localization ((pos1, pos2), (ast, asts)) -> 
        let astsStr = asts |> List.map eval |> String.concat ", "
        sprintf "Localization at positions %A and %A: (%s, [%s])" (pos1.ToString()) (pos2.ToString()) (eval ast) astsStr
    // | BoundedRangeInType of Positions * ((Ast * Ast) * Ast)
    | Ast.BoundedRangeInType ((pos1, pos2), ((ast1, ast2), ast3)) -> 
        sprintf "BoundedRangeInType at positions %A and %A: ((%s, %s), %s)" (pos1.ToString()) (pos2.ToString()) (eval ast1) (eval ast2) (eval ast3)
    | Ast.ClassInstance ((pos1, pos2), ((ast1, ast2), ast3)) -> 
        sprintf "ClassInstance at positions %A and %A: ((%s, %s), %s)" (pos1.ToString()) (pos2.ToString()) (eval ast1) (eval ast2) (eval ast3)
    | Ast.FunctionalTermInstance ((pos1, pos2), ((ast1, ast2), ast3)) -> 
        sprintf "FunctionalTermInstance at positions %A and %A: ((%s, %s), %s)" (pos1.ToString()) (pos2.ToString()) (eval ast1) (eval ast2) (eval ast3)
    // | All of Positions * ((Ast list * Ast option) list * Ast)
    | Ast.All ((pos1, pos2), (astsOpts, ast)) -> 
        let astsOptsStr = astsOpts |> List.map (
            fun (asts, optAst) -> 
                let astsStr = asts |> List.map eval |> String.concat ", "
                let optAstStr = optAst |> Option.map eval |> Option.defaultValue "None"
                sprintf "(%s, %s)" astsStr optAstStr ) |> String.concat ", "
        sprintf "All at positions %A and %A: (%s, %s)" (pos1.ToString()) (pos2.ToString()) astsOptsStr (eval ast)
    | Ast.Exists ((pos1, pos2), (astsOpts, ast)) -> 
        let astsOptsStr = astsOpts |> List.map (
            fun (asts, optAst) -> 
                let astsStr = asts |> List.map eval |> String.concat ", "
                let optAstStr = optAst |> Option.map eval |> Option.defaultValue "None"
                sprintf "(%s, %s)" astsStr optAstStr ) |> String.concat ", "
        sprintf "Exists at positions %A and %A: (%s, %s)" (pos1.ToString()) (pos2.ToString()) astsOptsStr (eval ast)
    
    // | ExistsN of Positions * ((Ast * (Ast * Ast option)) * Ast)
    | Ast.ExistsN ((pos1, pos2), ((ast1, (ast2, optAst)), ast3)) -> 
        let optAstStr = optAst |> Option.map eval |> Option.defaultValue "None"
        sprintf "ExistsN at positions %A and %A: ((%s, (%s, %s)), %s)" (pos1.ToString()) (pos2.ToString()) (eval ast1) (eval ast2) optAstStr (eval ast3)
    // | PredicateWithQualification of (Ast * Ast) 
    | Ast.PredicateWithQualification (ast1, ast2) -> sprintf "PredicateWithQualification (%s, %s)" (eval ast1) (eval ast2)
    // | InfixOperation of Positions * (Ast * Ast option) list
    | Ast.InfixOperation ((pos1, pos2), astsOpts) -> 
        let astsOptsStr = astsOpts |> List.map (fun (ast, optAst) -> 
            let optAstStr = optAst |> Option.map eval |> Option.defaultValue "None"
            sprintf "(%s, %s)" (eval ast) optAstStr ) |> String.concat ", "
        sprintf "InfixOperation at positions %A and %A: [%s]" (pos1.ToString()) (pos2.ToString()) astsOptsStr
    // | Expression of Positions * ((((Ast option * Ast) * Ast option) * Ast option) * Ast)
    | Ast.Expression ((pos1, pos2), ((((optAst1, ast1), optAst2), optAst3), ast2)) -> 
        let optAstStr1 = optAst1 |> Option.map eval |> Option.defaultValue "None"
        let optAstStr2 = optAst2 |> Option.map eval |> Option.defaultValue "None"
        let optAstStr3 = optAst3 |> Option.map eval |> Option.defaultValue "None"
        sprintf "Expression at positions %A and %A: ((((%s, %s), %s), %s), %s)" (pos1.ToString()) (pos2.ToString()) optAstStr1 (eval ast1) optAstStr2 optAstStr3 (eval ast2)
    // | Cases of Positions * (Ast list * Ast)
    | Ast.Cases ((pos1, pos2), (asts, ast)) -> 
        let astsStr = asts |> List.map eval |> String.concat ", "
        sprintf "Cases at positions %A and %A: ([%s], %s)" (pos1.ToString()) (pos2.ToString()) astsStr (eval ast)
    // | Assignment of Positions * (Ast * Ast)
    | Ast.Assignment ((pos1, pos2), (ast1, ast2)) -> sprintf "Assignment at positions %A and %A: (%s, %s)" (pos1.ToString()) (pos2.ToString()) (eval ast1) (eval ast2)
    | Ast.PredicateInstance ((pos1, pos2), (ast1, ast2)) -> sprintf "PredicateInstance at positions %A and %A: (%s, %s)" (pos1.ToString()) (pos2.ToString()) (eval ast1) (eval ast2)
    | Ast.ParentConstructorCall ((pos1, pos2), (ast1, ast2)) -> sprintf "ParentConstructorCall at positions %A and %A: (%s, %s)" (pos1.ToString()) (pos2.ToString()) (eval ast1) (eval ast2)
    | Ast.Signature ((pos1, pos2), (ast1, ast2)) -> sprintf "Signature at positions %A and %A: (%s, %s)" (pos1.ToString()) (pos2.ToString()) (eval ast1) (eval ast2)
    | Ast.JustifiedArgument ((pos1, pos2), (ast1, ast2)) -> sprintf "JustifiedArgument at positions %A and %A: (%s, %s)" (pos1.ToString()) (pos2.ToString()) (eval ast1) (eval ast2)
    | Ast.Argument ((pos1, pos2), (ast1, ast2)) -> sprintf "Argument at positions %A and %A: (%s, %s)" (pos1.ToString()) (pos2.ToString()) (eval ast1) (eval ast2)
    // | ForIn of Positions * ((Ast * Ast) * Ast list)
    | Ast.ForIn ((pos1, pos2), ((ast1, ast2), asts)) -> 
        let astsStr = asts |> List.map eval |> String.concat ", "
        sprintf "ForIn at positions %A and %A: ((%s, %s), [%s])" (pos1.ToString()) (pos2.ToString()) (eval ast1) (eval ast2) astsStr
    // | SignatureWithPreConBlock of Ast * ((Ast list option * Ast) * Ast)
    | Ast.SignatureWithPreConBlock (ast, ((optAsts, ast2), ast3)) -> 
        let optAstsStr = optAsts |> Option.map (List.map eval >> String.concat ", ") |> Option.defaultValue "None"
        sprintf "SignatureWithPreConBlock (%s, ((%s, %s), %s))" (eval ast) optAstsStr (eval ast2) (eval ast3)
    // | Theorem of Positions * (Ast *(Ast list option * Ast))
    | Ast.Theorem ((pos1, pos2), (ast, (Some astList, ast2))) ->
        let astStr = eval ast
        let astListStr = astList |> List.map eval |> String.concat ", "
        let ast2Str = eval ast2
        sprintf "Theorem at positions %A and %A: [%s, [%s], %s]" (pos1.ToString()) (pos2.ToString()) astStr astListStr ast2Str
    | Ast.Theorem ((pos1, pos2), (ast, (None, ast2))) ->
        let astStr = eval ast
        let ast2Str = eval ast2
        sprintf "Theorem at positions %A and %A: [%s, None, %s]" (pos1.ToString()) (pos2.ToString()) astStr ast2Str
    | Ast.Lemma ((pos1, pos2), (ast, (Some astList, ast2))) ->
        let astStr = eval ast
        let astListStr = astList |> List.map eval |> String.concat ", "
        let ast2Str = eval ast2
        sprintf "Lemma at positions %A and %A: [%s, [%s], %s]" (pos1.ToString()) (pos2.ToString()) astStr astListStr ast2Str
    | Ast.Lemma ((pos1, pos2), (ast, (None, ast2))) ->
        let astStr = eval ast
        let ast2Str = eval ast2
        sprintf "Lemma at positions %A and %A: [%s, None, %s]" (pos1.ToString()) (pos2.ToString()) astStr ast2Str
    | Ast.Proposition ((pos1, pos2), (ast, (Some astList, ast2))) ->
        let astStr = eval ast
        let astListStr = astList |> List.map eval |> String.concat ", "
        let ast2Str = eval ast2
        sprintf "Proposition at positions %A and %A: [%s, [%s], %s]" (pos1.ToString()) (pos2.ToString()) astStr astListStr ast2Str
    | Ast.Proposition ((pos1, pos2), (ast, (None, ast2))) ->
        let astStr = eval ast
        let ast2Str = eval ast2
        sprintf "Proposition at positions %A and %A: [%s, None, %s]" (pos1.ToString()) (pos2.ToString()) astStr ast2Str
    | Ast.Conjecture ((pos1, pos2), (ast, (Some astList, ast2))) ->
        let astStr = eval ast
        let astListStr = astList |> List.map eval |> String.concat ", "
        let ast2Str = eval ast2
        sprintf "Conjecture at positions %A and %A: [%s, [%s], %s]" (pos1.ToString()) (pos2.ToString()) astStr astListStr ast2Str
    | Ast.Conjecture ((pos1, pos2), (ast, (None, ast2))) ->
        let astStr = eval ast
        let ast2Str = eval ast2
        sprintf "Conjecture at positions %A and %A: [%s, None, %s]" (pos1.ToString()) (pos2.ToString()) astStr ast2Str
    // | Corollary of Positions * ((Ast * Ast) * (Ast list option * Ast))
    | Ast.Corollary ((pos1, pos2), ((ast1, ast2), (Some astList, ast3))) ->
        let ast1Str = eval ast1
        let ast2Str = eval ast2
        let astListStr = astList |> List.map eval |> String.concat ", "
        let ast3Str = eval ast3
        sprintf "Corollary at positions %A and %A: [[%s, %s], [%s], %s]" (pos1.ToString()) (pos2.ToString()) ast1Str ast2Str astListStr ast3Str
    | Ast.Corollary ((pos1, pos2), ((ast1, ast2), (None, ast3))) ->
        let ast1Str = eval ast1
        let ast2Str = eval ast2
        let ast3Str = eval ast3
        sprintf "Corollary at positions %A and %A: [[%s, %s], None, %s]" (pos1.ToString()) (pos2.ToString()) ast1Str ast2Str ast3Str
    // | NamedVarDecl of Positions * ((Ast list * Ast) * Ast) 
    | Ast.NamedVarDecl ((pos1, pos2), ((asts, ast), ast2)) -> 
        let astsStr = asts |> List.map eval |> String.concat ", "
        sprintf "NamedVarDecl at positions %A and %A: ((%s, %s), %s)" (pos1.ToString()) (pos2.ToString()) astsStr (eval ast) (eval ast2)
    // | Axiom of Positions * (Ast * (Ast list option * Ast))
    | Ast.Axiom ((pos1, pos2), (ast, (Some astList, ast2))) ->
        let astStr = eval ast
        let astListStr = astList |> List.map eval |> String.concat ", "
        let ast2Str = eval ast2
        sprintf "Axiom at positions %A and %A: [%s, [%s], %s]" (pos1.ToString()) (pos2.ToString()) astStr astListStr ast2Str
    | Ast.Axiom ((pos1, pos2), (ast, (None, ast2))) ->
        let astStr = eval ast
        let ast2Str = eval ast2
        sprintf "Axiom at positions %A and %A: [%s, None, %s]" (pos1.ToString()) (pos2.ToString()) astStr ast2Str
    | Ast.Constructor ((pos1, pos2), (ast, (Some astList, ast2))) ->
        let astStr = eval ast
        let astListStr = astList |> List.map eval |> String.concat ", "
        let ast2Str = eval ast2
        sprintf "Constructor at positions %A and %A: [%s, [%s], %s]" (pos1.ToString()) (pos2.ToString()) astStr astListStr ast2Str
    | Ast.Constructor ((pos1, pos2), (ast, (None, ast2))) ->
        let astStr = eval ast
        let ast2Str = eval ast2
        sprintf "Constructor at positions %A and %A: [%s, None, %s]" (pos1.ToString()) (pos2.ToString()) astStr ast2Str
    // | DefPredicateContent of Ast list option * Ast
    | Ast.DefPredicateContent (optAsts, ast) -> 
        let optAstsStr = optAsts |> Option.map (List.map eval >> String.concat ", ") |> Option.defaultValue "None"
        sprintf "DefPredicateContent (%s, %s)" optAstsStr (eval ast)
    | Ast.DefFunctionContent (optAsts, ast) -> 
        let optAstsStr = optAsts |> Option.map (List.map eval >> String.concat ", ") |> Option.defaultValue "None"
        sprintf "DefFunctionContent (%s, %s)" optAstsStr (eval ast)
    | Ast.DefClassContent (optAsts, ast) -> 
        let optAstsStr = optAsts |> Option.map (List.map eval >> String.concat ", ") |> Option.defaultValue "None"
        sprintf "DefClassContent (%s, %s)" optAstsStr (eval ast)
    // | DefClassCompleteContent of Ast list option * Ast list
    | Ast.DefClassCompleteContent (optAsts, asts) -> 
        let optAstsStr = optAsts |> Option.map (List.map eval >> String.concat ", ") |> Option.defaultValue "None"
        let astsStr = asts |> List.map eval |> String.concat ", "
        sprintf "DefClassCompleteContent (%s, [%s])" optAstsStr astsStr
    // | DefinitionPredicate of Positions * (Ast * (Ast * Ast list option))
    | Ast.DefinitionPredicate ((pos1, pos2), (ast, (ast2, optAsts))) -> 
        let optAstsStr = optAsts |> Option.map (List.map eval >> String.concat ", ") |> Option.defaultValue "None"
        sprintf "DefinitionPredicate at positions %A and %A: (%s, (%s, %s))" (pos1.ToString()) (pos2.ToString()) (eval ast) (eval ast2) optAstsStr
    // | DefinitionFunctionalTerm of Positions * ((Ast * Ast) * (Ast * Ast list option))
    | Ast.DefinitionFunctionalTerm ((pos1, pos2), ((ast1, ast2), (ast3, optAsts))) -> 
        let optAstsStr = optAsts |> Option.map (List.map eval >> String.concat ", ") |> Option.defaultValue "None"
        sprintf "DefinitionFunctionalTerm at positions %A and %A: ((%s, %s), (%s, %s))" (pos1.ToString()) (pos2.ToString()) (eval ast1) (eval ast2) (eval ast3) optAstsStr
    // | DefinitionClass of Positions * (((Ast * Ast option) * Ast list) * (Ast * Ast list option)) 
    | Ast.DefinitionClass ((pos1, pos2), (((ast1, optAst), asts), (ast2, optAsts))) -> 
        let optAstStr = optAst |> Option.map eval |> Option.defaultValue "None"
        let astsStr = asts |> List.map eval |> String.concat ", "
        let optAstsStr = optAsts |> Option.map (List.map eval >> String.concat ", ") |> Option.defaultValue "None"
        sprintf "DefinitionClass at positions %A and %A: (((%s, %s), [%s]), (%s, %s))" (pos1.ToString()) (pos2.ToString()) (eval ast1) optAstStr astsStr (eval ast2) optAstsStr
    // | DerivedPredicate of Ast
    | Ast.DerivedPredicate ast -> sprintf "DerivedPredicate %s" (eval ast)

    // | Proof of Positions * (Ast * (Ast list * Ast option))
    | Ast.Proof ((pos1, pos2), (ast, (asts, optAst))) -> 
        let astsStr = asts |> List.map eval |> String.concat ", "
        let optAstStr = optAst |> Option.map eval |> Option.defaultValue "None"
        sprintf "Proof at positions %A and %A: (%s, ([%s], %s))" (pos1.ToString()) (pos2.ToString()) (eval ast) astsStr optAstStr

    | ast -> 
        let astType = ast.GetType().Name
        sprintf "handling ast type %s not yet implemented" astType
