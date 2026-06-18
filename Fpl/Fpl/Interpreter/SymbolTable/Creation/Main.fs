/// This module evaluates the abstract syntax tree (AST) and interprets its semantics./// This module evaluates the abstract syntax tree (AST) and interprets its semantics.
/// It produces a SymbolTable object containing a current semantical representation of the AST.

(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)
module Fpl.Interpreter.SymbolTable.Creation.Main
open System
open System.Collections.Generic
open Fpl.Errors.Diagnostics
open Fpl.Primitives
open Fpl.Parser.Types
open Fpl.Errors.Emitter
open Fpl.Interpreter.BasicTypes
open Fpl.Interpreter.Helpers.Basic
open Fpl.Interpreter.Helpers.Checks
open Fpl.Interpreter.SymbolTable.Types1.TopLevel
open Fpl.Interpreter.SymbolTable.Storage.Asts
open Fpl.Interpreter.SymbolTable.Storage.Heap
open Fpl.Interpreter.SymbolTable.Storage.Util
open Fpl.Interpreter.SymbolTable.Types2.Intrinsic
open Fpl.Interpreter.SymbolTable.Types2.Variables
open Fpl.Interpreter.SymbolTable.Types2.References
open Fpl.Interpreter.SymbolTable.Types2.Definitions
open Fpl.Interpreter.SymbolTable.Types3.SelfParent
open Fpl.Interpreter.SymbolTable.Types3.RulesOfInferences
open Fpl.Interpreter.SymbolTable.Types3.Extensions
open Fpl.Interpreter.SymbolTable.Types3.Localization
open Fpl.Interpreter.SymbolTable.Creation.Forward
open Fpl.Interpreter.SymbolTable.Creation.LeafTokens
open Fpl.Interpreter.SymbolTable.Creation.Identifiers
open Fpl.Interpreter.SymbolTable.Creation.TypeConstructs
open Fpl.Interpreter.SymbolTable.Creation.Predicates
open Fpl.Interpreter.SymbolTable.Creation.Expressions
open Fpl.Interpreter.SymbolTable.Creation.Commands
open Fpl.Interpreter.SymbolTable.Creation.Definitions
open Fpl.Interpreter.SymbolTable.Creation.StatementBlocks
open Fpl.Interpreter.SymbolTable.Creation.Proofs
open Fpl.Interpreter.SymbolTable.Creation.TopLevel

/// A recursive function evaluating an AST and returning a list of EvalAliasedNamespaceIdentifier records
/// for each occurrence of the uses clause in the FPL code.
let rec eval ast =


    match ast with
    // Lexical / leaf tokens
    | Ast.Alias _
    | Ast.Dot _
    | Ast.Star _
    | Ast.Digits _
    | Ast.DollarDigits _
    | Ast.ExtensionRegex _
    | Ast.LanguageCode _
    | Ast.LocalizationString _
    | Ast.PrefixDecl _
    | Ast.PostfixDecl _
    | Ast.SymbolDecl _
    | Ast.ObjectSymbolWithPos _
    | Ast.InfixSymbolWithPos _
    | Ast.PostFixSymbolWithPos _
    | Ast.PrefixSymbolWithPos _
        ->
        evalLeafTokens ast

    // Identifiers & Identifier dispatchers
    | Ast.PascalCaseId _
    | Ast.BaseClassName _
    | Ast.PredicateIdentifier _
    | Ast.NamespaceIdentifier _
    | Ast.ClassIdentifier _
    | Ast.AliasedNamespaceIdentifier _
    | Ast.ArgumentIdentifier _
    | Ast.RefArgumentIdentifier _
    | Ast.DelegateName _
    | Ast.ExtensionName _
    | Ast.ReferencingIdentifier _
        ->
        evalIdentifiers ast

    // Types and type-related constructs
    | Ast.IndexType _
    | Ast.FunctionalTermType _
    | Ast.ObjectType _
    | Ast.PredicateType _
    | Ast.TemplateType _
    | Ast.ArrayType _
    | Ast.SimpleVariableType _
    | Ast.IndexAllowedType _
    | Ast.InheritedType _
    | Ast.InheritedTypeList _
    | Ast.CompoundPredicateType _
    | Ast.CompoundFunctionalTermType _
        ->
        evalTypeConstructs ast

    // Prime and compound predicates
    | Ast.True _
    | Ast.False _
    | Ast.And _
    | Ast.Or _
    | Ast.Xor _
    | Ast.Impl _
    | Ast.Iif _
    | Ast.Not _
    | Ast.All _
    | Ast.Exists _
    | Ast.Exists1 _
    | Ast.ExistsN _
    | Ast.IsOperator _
        ->
        evalPredicates ast

    // Expressions
    | Ast.PredicateWithQualification _
    | Ast.PredicateWithOptSpecification _
    | Ast.InfixOp _
    | Ast.PrefixOp _
    | Ast.PostfixOp _
    | Ast.Parens _
        ->
        evalExpressions ast

    // Commands & other actions
    | Ast.Delegate _
    | Ast.Assertion _
    | Ast.Cases _
    | Ast.CaseSingle _
    | Ast.CaseElse _ 
    | Ast.MapCases _
    | Ast.MapCaseSingle _
    | Ast.MapCaseElse _
    | Ast.Assignment _
    | Ast.ForIn _
    | Ast.InEntity _
    | Ast.Return _
        ->
        evalCommands ast

    // Definitions
    | Ast.DefinitionClass _
    | Ast.ClassSignature _
    | Ast.ClassDefinitionBlock _
    | Ast.DefClassCompleteContent _
    | Ast.Constructor _
    | Ast.ConstructorSignature _
    | Ast.ConstructorBlock _
    | Ast.BaseConstructorCall _
    | Ast.DefinitionPredicate _
    | Ast.PredicateSignature _
    | Ast.DefPredicateContent _
    | Ast.DefinitionFunctionalTerm _
    | Ast.FunctionalTermSignature _
    | Ast.Mapping _
    | Ast.FunctionalTermDefinitionBlock _
    | Ast.DefFunctionContent _
    | Ast.PredicateInstance _
    | Ast.PredicateInstanceSignature _
    | Ast.FunctionalTermInstance _
    | Ast.FunctionalTermInstanceSignature _
    | Ast.DefinitionExtension _
    | Ast.ExtensionSignature _
    | Ast.ExtensionAssignment _ 
        ->
        evalDefinitions ast

    // Axioms, conjectures and other theorem-like statements
    | Ast.Axiom _
    | Ast.AxiomSignature _
    | Ast.Conjecture _
    | Ast.ConjectureSignature _
    | Ast.Theorem _
    | Ast.TheoremSignature _
    | Ast.Lemma _
    | Ast.LemmaSignature _
    | Ast.Proposition _
    | Ast.PropositionSignature _
    | Ast.Corollary _
    | Ast.CorollarySignature _
        ->
        evalStatementBlocks ast

    | Ast.Proof _
    | Ast.ProofSignature _
    | Ast.ProofBlock _
    | Ast.ProofContent _
    | Ast.Argument _
    | Ast.JustArgInf _
    | Ast.StartArgument _
    | Ast.StartArgumentStictly _
    | Ast.Justification _
    | Ast.JustificationItem _
    | Ast.ReferenceToProofOrCorollary _
    | Ast.ByDef _
    | Ast.JustificationIdentifier _ 
    | Ast.TrivialArgument _
    | Ast.DeriveArgument _
    | Ast.AssumeArgument _
    | Ast.RevokeArgument _
    | Ast.Qed _
        ->
        evalProofs ast


    // Top level nodes
    | Ast.AST _
    | Ast.Namespace _
    | Ast.BuildingBlock _
    | Ast.ErrorSyntax _
    | Ast.ErrorSyntaxBacktracking _
    | Ast.ErrorSyntaxChain _
        ->
        evalTopLevel ast



    | Ast.Undefined((pos1, pos2), _) -> 
        let fv = heap.Eval.PeekEvalStack()
        let fvNew = new FplIntrinsicUndef((pos1, pos2), fv)
        heap.Eval.PushEvalStack(fvNew)
        heap.Eval.PopEvalStack()


    | Ast.Intrinsic((pos1, pos2),()) -> 
        let fv = heap.Eval.PeekEvalStack()
        fv.IsIntrinsic <- true // flag that this block is intrinsic
        match fv.Name with 
        | PrimClassL ->
            let cl = fv :?> FplClass
            cl.AddDefaultConstructor()
        | _ -> ()


    | Ast.Var((pos1, pos2), name) ->
        let searchVarByName (fv:FplGenericNode) = 
            match (searchInUpperScopeByName fv name) with
            | ScopeSearchResult.Found foundVar -> 
                // variable was declared in the scope
                match fv.Name with 
                | PrimJIByDefVar 
                | PrimRefL 
                | PrimForInStmtEntityL 
                | PrimForInStmtDomainL ->
                    fv.FplId <- name
                    fv.RefersTo <- Some foundVar
                | PrimTranslationL ->
                    // for translations, use the name of the variable
                    fv.FplId <- foundVar.FplId
                    fv.TypeId <- foundVar.TypeId 
                | _ -> ()
                match foundVar with
                | :? FplGenericVariable as var -> var .SetIsUsed()
                | _ -> ()
            | _ ->
                match fv.UltimateBlockNode with 
                | Some (:? FplLocalization as loc) when heap.Helper.InSignatureEvaluation -> 
                    () // localizations during 
                | _ ->
                    // otherwise emit variable not declared 
                    fv.ErrorOccurred <- emitVAR01diagnostics name pos1 pos2
                
                // if no variable in scope was found, spawn an undefined variable
                let undefVar = new FplVariable(name, (pos1, pos2), fv)
                undefVar.TypeId <- LiteralUndef
                undefVar.SetDefaultValue()
                heap.Eval.PushEvalStack(undefVar)
                heap.Eval.PopEvalStack()
            
        let fv = heap.Eval.PeekEvalStack()
        let parentFv = fv.Parent.Value
        match fv.Name with 
        | PrimVariableL
        | PrimVariableArrayL -> 
            // in the context of variable declarations, we set the name and positions of the variables
            fv.FplId <- name
            fv.TypeId <- LiteralUndef 
            fv.StartPos <- pos1
            fv.EndPos <- pos2
        | PrimExtensionL -> 
            let newVar = new FplVariable(name, (pos1, pos2), fv)
            newVar.TypeId <- fv.FplId
            newVar.IsSignatureVariable <- heap.Helper.InSignatureEvaluation
            heap.Eval.PushEvalStack(newVar)
            heap.Eval.PopEvalStack()
        | PrimRefL ->
            match box parentFv with 
            | :? IHasDotted as dotted when dotted.DottedChild.IsSome ->
                fv.FplId <- name
                fv.TypeId <- LiteralUndef
            | _ -> searchVarByName fv
        | _ -> 
            // in all other contexts, check by name, if this variable was declared in some scope
            searchVarByName fv

        match fv.UltimateBlockNode with 
        | Some (:? FplLocalization as loc) when loc.ArgList.Count = 0 && fv.RefersTo.IsSome -> 
            let variable = fv.RefersTo.Value
            if loc.Scope.ContainsKey(name) then 
                let other = loc.Scope[name]
                variable.ErrorOccurred <- emitVAR11diagnostics name other.QualifiedStartPos pos1 pos2 
            else 
                loc.Scope.Add(name, variable)
                variable.Parent <- Some loc
        | _ -> ()

    | Ast.InfixDeclWithPrecedence((pos1, pos2), (symbol, precedenceAsts)) -> 
        let fv = heap.Eval.PeekEvalStack()
        eval precedenceAsts
        fv.ExpressionType <- FixType.Infix (symbol, fv.AuxiliaryInfo)
    | Ast.Self((pos1, pos2), _) -> 
        let parent = heap.Eval.PeekEvalStack()
        let fv = new FplSelf((pos1, pos2), parent)
        match fv.SelfBlock with
        | ScopeSearchResult.Found block ->
            fv.RefersTo <- Some block
        | _ -> ()
        heap.Eval.PushEvalStack(fv)
        heap.Eval.PopEvalStack()
    | Ast.Parent((pos1, pos2), _) -> 
        let parent = heap.Eval.PeekEvalStack()
        let fv = new FplParent((pos1, pos2), parent)
        match fv.ParentBlock with
        | ScopeSearchResult.Found block ->
            fv.RefersTo <- Some block
        | _ -> ()
        heap.Eval.PushEvalStack(fv)
        heap.Eval.PopEvalStack()
    | Ast.RuleOfInferenceSignature((pos1, pos2), simpleSignatureAst) ->
        heap.Helper.InSignatureEvaluation <- true
        eval simpleSignatureAst
        setSignaturePositions pos1 pos2
        heap.Helper.InSignatureEvaluation <- false
    | Ast.RuleOfInference((pos1, pos2), (signatureAst, premiseConclusionBlockAst)) ->
        let parent = heap.Eval.PeekEvalStack()
        let fv = new FplRuleOfInference((pos1, pos2), parent, heap.Helper.GetNextAvailableFplBlockRunOrder)
        heap.Eval.PushEvalStack(fv)
        eval signatureAst
        eval premiseConclusionBlockAst
        heap.Eval.PopEvalStack() 
    | Ast.Extension((pos1, pos2), extensionString) ->
        let fv = heap.Eval.PeekEvalStack()
        let fplNew = new FplExtensionObj((pos1,pos2), fv)
        heap.Eval.PushEvalStack(fplNew)
        fplNew.FplId <- extensionString
        heap.Eval.PopEvalStack()
    | Ast.UsesClause((pos1, pos2), ast1) ->
        eval ast1
    | Ast.DottedPredicate((pos1, pos2), predicateWithOptSpecificationAst) ->
        let fv = heap.Eval.PeekEvalStack()
        let refBlock = new FplReference((pos1, pos2), fv) 
        match fv with 
        | :? FplReference as ref ->
            ref.DottedChild <- Some refBlock
        | _ -> ()
        heap.Eval.PushEvalStack(refBlock)
        eval predicateWithOptSpecificationAst
        heap.Eval.PopEvalStack()
    | Ast.ParamTuple namedVariableDeclarationListAsts ->
        let fv = heap.Eval.PeekEvalStack()
        fv.ArgType <- ArgType.Parentheses
        namedVariableDeclarationListAsts |> List.map (fun child ->
            match child with 
            | Ast.NamedVarDecl(_,(varList,_)) -> fv.Arity <- fv.Arity + varList.Length
            | _ -> ()
            eval child
        ) |> ignore
    | Ast.TranslationTerm((pos1, pos2), asts) ->
        let fv = heap.Eval.PeekEvalStack()
        asts |> List.map (fun ebnfTerm ->
            let trsl = new FplTranslation((pos1, pos2), fv)
            heap.Eval.PushEvalStack(trsl)
            eval ebnfTerm
            heap.Eval.PopEvalStack()
        ) |> ignore
    | Ast.TranslationTermList((pos1, pos2), ebnfTermAsts) ->
        let chooseRandomMember (lst: Ast list) =
            let rnd = Random()
            let index = rnd.Next(lst.Length)
            lst.[index]
        eval (chooseRandomMember ebnfTermAsts)
    | Ast.BrackedCoordList((pos1, pos2), coordListAst) ->
        let getProceedingReference = heap.Eval.GetProceedingReference()

        match getProceedingReference with 
        | Some ref -> 
            ref.ArgType <- ArgType.Brackets
            if coordListAst.Length > 0 then 
                coordListAst 
                |> List.iter (fun pred -> 
                    let ref = new FplReference((pos1, pos2), ref)
                    heap.Eval.PushEvalStack(ref)
                    eval pred
                    heap.Eval.PopEvalStack()
                ) 
        | _ -> ()
    | Ast.VarDeclBlock varDeclOrStmtAstList ->
        varDeclOrStmtAstList |> Option.map (List.map eval >> ignore) |> Option.defaultValue ()
    | Ast.PremiseList((pos1, pos2), predicateListAsts) ->
        let parent = heap.Eval.PeekEvalStack()
        let fv = new FplPredicateList((pos1, pos2), parent, heap.Helper.GetNextAvailableFplBlockRunOrder) 
        heap.Eval.PushEvalStack(fv)
        predicateListAsts |> List.map eval |> ignore
        heap.Eval.PopEvalStack()
    | Ast.ArgumentTuple((pos1, pos2), predicateListAst) ->
        let next = heap.Eval.PeekEvalStack()
        evalArgumentTuple next predicateListAst pos1 pos2
    | Ast.QualificationList((pos1, pos2), asts) ->
        asts |> List.map eval |> ignore
    | Ast.SelfOrParent((pos1, pos2), selforParentAst) -> 
        eval selforParentAst
    | Ast.Language((pos1, pos2),(langCode, ebnfAst)) ->
        let fv = heap.Eval.PeekEvalStack()
        let lang = new FplLanguage((pos1, pos2), fv) 
        heap.Eval.PushEvalStack(lang)
        eval langCode
        eval ebnfAst
        heap.Eval.PopEvalStack() // remove language
    | Ast.Localization(((pos1, pos2), predicateAst), translationListAsts) ->
        let parent = heap.Eval.PeekEvalStack()
        let fv = new FplLocalization((pos1, pos2), parent, heap.Helper.GetNextAvailableFplBlockRunOrder)
        let var04List = List<KeyValuePair<string, Positions>>()
        heap.Eval.PushEvalStack(fv)
        heap.Helper.InSignatureEvaluation <- true
        eval predicateAst
        heap.Helper.InSignatureEvaluation <- false
        translationListAsts |> List.map (fun subAst -> 
            eval subAst
            let vars = fv.GetVariables()
            vars
            |> List.map (fun var -> var :?> FplGenericVariable)
            |> List.filter (fun var -> not var.IsUsed)
            |> List.map (fun var ->
                let loc = heap.Eval.PeekEvalStack()
                let languageList = 
                    loc.Scope 
                    |> Seq.filter (fun kvp -> isLanguage kvp.Value) 
                    |> Seq.map (fun kvp -> kvp.Value) 
                    |> Seq.toList 
                    |> List.rev
                if not languageList.IsEmpty then
                    let lan = languageList.Head
                    let kvp = KeyValuePair(var.FplId,(lan.StartPos, lan.EndPos))
                    var04List.Add kvp
            )
        ) |> ignore
        let identifier = fv.ArgList |> Seq.map (fun arg -> arg.FplId) |> String.concat ""
        fv.FplId <- identifier
        fv.TypeId <- identifier
        heap.Eval.PopEvalStack()
        var04List
        |> Seq.iter (fun kvp -> 
            fv.ErrorOccurred <- emitVAR04diagnostics kvp.Key (fst kvp.Value) (snd kvp.Value)
        )
    | Ast.PremiseConclusionBlock (varDeclBlock, (premiseAst, conclusionAst)) ->
        eval varDeclBlock 
        eval premiseAst
        eval conclusionAst
    | Ast.NamedVarDecl((pos1, pos2), (variableListAst, variableTypeAst)) ->
        let parent = heap.Eval.PeekEvalStack()
        parent.AuxiliaryInfo <- variableListAst |> List.length // remember how many variables to create
        // create all variables of the named variable declaration in the current scope
        variableListAst |> List.iter (fun varAst ->
            match variableTypeAst with 
            | Ast.ArrayType((posMan1, posMan2),(mainTypeAst,  indexAllowedTypeListAst)) ->
                let numberOfVariadicVars = parent.AuxiliaryInfo
                if numberOfVariadicVars > 1 then
                    parent.ErrorOccurred <- emitVAR00Diagnostics posMan1 posMan2        
                match varAst with 
                | Ast.Var((varPos1, varPos2), varName) ->
                    let newVar = new FplVariableArray(varName, (varPos1, varPos2), parent)
                    newVar.IsSignatureVariable <- (heap.Helper.InSignatureEvaluation && hasSignature parent)
                    heap.Eval.PushEvalStack(newVar)
                    eval mainTypeAst
                    indexAllowedTypeListAst |> List.map eval |> ignore
                    heap.Eval.PopEvalStack()
                | _ -> ()
            | Ast.SimpleVariableType((_, _),simplVariableTypeAst) ->
                match varAst with 
                | Ast.Var((varPos1, varPos2), varName) ->
                    let newVar = new FplVariable(varName, (varPos1, varPos2), parent)
                    newVar.IsSignatureVariable <- (heap.Helper.InSignatureEvaluation && hasSignature parent)
                    heap.Eval.PushEvalStack(newVar)
                    eval simplVariableTypeAst
                    heap.Eval.PopEvalStack()
                | _ -> ()
            | _ -> ()
        ) |> ignore 

    | Ast.Precedence((pos1, pos2), precedence) ->
        let fv = heap.Eval.PeekEvalStack()
        fv.AuxiliaryInfo <- precedence

let tryFindParsedAstUsesClausesEvaluated (parsedAsts: List<ParsedAst>) =
    if parsedAsts.Exists(fun pa -> pa.Status = ParsedAstStatus.UsesClausesEvaluated) then
        Some(parsedAsts.Find(fun pa -> pa.Status = ParsedAstStatus.UsesClausesEvaluated))
    else
        None

let evaluateSymbolTable () =
    heap.ParsedAsts.OrderAsts()

    let mutable found = true
    evalRef.Value <- eval

    while found do
        let usesClausesEvaluatedParsedAst =
            tryFindParsedAstUsesClausesEvaluated heap.ParsedAsts

        match usesClausesEvaluatedParsedAst with
        | Some pa ->
            heap.ClearWorkingMemory()
            // evaluate the ParsedAst of a theory
            let theoryValue = new FplTheory(pa.Id, heap.Root, pa.Parsing.Uri.AbsolutePath, heap.Helper.GetNextAvailableFplBlockRunOrder);
            if not (heap.Root.Scope.ContainsKey(pa.Id)) then
                heap.Root.Scope.Add(pa.Id, theoryValue)
            else
                heap.Root.Scope[pa.Id] <- theoryValue
            heap.Eval.PushEvalStack(theoryValue)
            ad.CurrentUri <- pa.Parsing.Uri
            pa.Parsing.BuildingBlockAsts
            |> List.map (fun buildinBlockAst -> evalRef.Value buildinBlockAst) |> ignore
            pa.Status <- ParsedAstStatus.Evaluated
            heap.Eval.PopEvalStack()
            theoryValue.Run()
        | None -> found <- false


