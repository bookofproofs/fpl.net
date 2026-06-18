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
open Fpl.Interpreter.SymbolTable.Types2.Intrinsic
open Fpl.Interpreter.SymbolTable.Types2.Variables
open Fpl.Interpreter.SymbolTable.Types2.Definitions
open Fpl.Interpreter.SymbolTable.Types3.SelfParent
open Fpl.Interpreter.SymbolTable.Types3.Extensions
open Fpl.Interpreter.SymbolTable.Types3.Localization
open Fpl.Interpreter.SymbolTable.Creation.Forward
open Fpl.Interpreter.SymbolTable.Creation.LeafTokens
open Fpl.Interpreter.SymbolTable.Creation.Identifiers
open Fpl.Interpreter.SymbolTable.Creation.TypeConstructs
open Fpl.Interpreter.SymbolTable.Creation.Variables
open Fpl.Interpreter.SymbolTable.Creation.Predicates
open Fpl.Interpreter.SymbolTable.Creation.Expressions
open Fpl.Interpreter.SymbolTable.Creation.Tuples
open Fpl.Interpreter.SymbolTable.Creation.Commands
open Fpl.Interpreter.SymbolTable.Creation.Definitions
open Fpl.Interpreter.SymbolTable.Creation.RulesOfInferences
open Fpl.Interpreter.SymbolTable.Creation.StatementBlocks
open Fpl.Interpreter.SymbolTable.Creation.Proofs
open Fpl.Interpreter.SymbolTable.Creation.Localizations
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

    // Variables and declarations
    // Variables
    | Ast.VarDeclBlock _
    | Ast.NamedVarDecl _
    | Ast.Var _
        ->
        evalVariables ast

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

    // Tuple-like constructs and qualifies
    | BrackedCoordList _
    | ArgumentTuple _
    | DottedPredicate _
    | QualificationList _
    | ParamTuple _
        ->
        evalTuples ast

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

    // Rules of inferences
    | Ast.RuleOfInference _
    | Ast.RuleOfInferenceSignature _
    | Ast.PremiseConclusionBlock _
    | Ast.PremiseList _
        ->
        evalRulesOfInferences ast

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

    // Localizations
    | Ast.Localization _
    | Ast.TranslationTermList _
    | Ast.TranslationTerm _
    | Ast.Language _
    | Ast.LanguageCode _
    | Ast.LocalizationString _
        ->
        evalLocalizations ast

    // Top level nodes
    | Ast.AST _
    | Ast.Namespace _
    | Ast.UsesClause _
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
    | Ast.Extension((pos1, pos2), extensionString) ->
        let fv = heap.Eval.PeekEvalStack()
        let fplNew = new FplExtensionObj((pos1,pos2), fv)
        heap.Eval.PushEvalStack(fplNew)
        fplNew.FplId <- extensionString
        heap.Eval.PopEvalStack()
    | Ast.SelfOrParent((pos1, pos2), selforParentAst) -> 
        eval selforParentAst

    | Ast.Precedence((pos1, pos2), precedence) ->
        let fv = heap.Eval.PeekEvalStack()
        fv.AuxiliaryInfo <- precedence



let createSymbolTable () =
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


