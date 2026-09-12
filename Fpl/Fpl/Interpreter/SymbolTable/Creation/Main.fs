(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)

/// <summary>
/// Entry point for symbol-table construction: evaluates the parsed AST and produces
/// the runtime semantic representation used by the interpreter.
/// </summary>
/// <remarks>
/// This module wires the forward evaluator (<c>evalRef</c>) to a recursive dispatcher
/// (<c>eval</c>) that delegates AST node processing to specialized creation modules
/// (identifiers, types, variables, predicates, definitions, proofs, localizations, etc.).
/// The evaluator performs significant side effects: it mutates the global <c>heap</c>,
/// pushes and pops evaluation frames, emits diagnostics, and registers top-level FPL
/// blocks in the symbol table.
/// </remarks>
/// <exceptions cref="System.Exception">Propagates exceptions emitted by lower-level evaluators for unsupported or invalid AST nodes.</exceptions>
module Fpl.Interpreter.SymbolTable.Creation.Main
open Fpl.Errors.Diagnostics
open Fpl.Parser.Types
open Fpl.Interpreter.SymbolTable.Types1.TopLevel
open Fpl.Interpreter.SymbolTable.Storage.Asts
open Fpl.Interpreter.SymbolTable.Storage.Heap
open Fpl.Interpreter.SymbolTable.Creation.Forward
open Fpl.Interpreter.SymbolTable.Creation.LeafTokens
open Fpl.Interpreter.SymbolTable.Creation.Identifiers
open Fpl.Interpreter.SymbolTable.Creation.TypeConstructs
open Fpl.Interpreter.SymbolTable.Creation.Variables
open Fpl.Interpreter.SymbolTable.Creation.Predicates
open Fpl.Interpreter.SymbolTable.Creation.Expressions
open Fpl.Interpreter.SymbolTable.Creation.Tuples
open Fpl.Interpreter.SymbolTable.Creation.Commands
open Fpl.Interpreter.SymbolTable.Creation.SymbolExtensions
open Fpl.Interpreter.SymbolTable.Creation.Definitions
open Fpl.Interpreter.SymbolTable.Creation.RulesOfInferences
open Fpl.Interpreter.SymbolTable.Creation.StatementBlocks
open Fpl.Interpreter.SymbolTable.Creation.Proofs
open Fpl.Interpreter.SymbolTable.Creation.SpecialReferences
open Fpl.Interpreter.SymbolTable.Creation.Localizations
open Fpl.Interpreter.SymbolTable.Creation.TopLevel

/// <summary>
/// Recursively dispatches AST nodes to the appropriate specialized evaluator.
/// </summary>
/// <params name="ast">The AST node to evaluate. The dispatcher recognizes all parser-produced AST cases
/// and delegates to the corresponding creation modules.</params>
/// <returns>Unit. The function performs side effects on the global interpreter state (<c>heap</c>).</returns>
/// <remarks>
/// - This function is the central entry point used by <c>evalRef</c>. It pattern-matches on the
///   F# discriminated union <c>Ast</c> and routes nodes to functions such as <c>evalIdentifiers</c>,
///   <c>evalTypeConstructs</c>, <c>evalDefinitions</c>, <c>evalProofs</c>, <c>evalLocalizations</c>, etc.
/// - Side effects include pushing/popping evaluation frames, setting identifiers and types,
///   and emitting diagnostics via the diagnostics emitter helpers.
/// - The concrete behavior for each AST case is implemented in the specialized modules that this
///   dispatcher references; this function is intentionally minimal and focused on routing.
/// </remarks>
/// <exceptions cref="System.Exception">May be thrown by underlying evaluators if an AST case is invalid or not supported.</exceptions>
let rec eval ast =
    match ast with
    // Lexical / leaf tokens
    | Ast.Alias _
    | Ast.Dot _
    | Ast.Star _
    | Ast.Digits _
    | Ast.DollarDigits _
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

    // Variables and variable declarations
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

    // Symbol extensions
    | Ast.SymbolDecl _
    | Ast.PrefixDecl _
    | Ast.PostfixDecl _
    | Ast.InfixDeclWithPrecedence _
    | Ast.Precedence _
    | Ast.DefinitionExtension _
    | Ast.ExtensionSignature _
    | Ast.ExtensionRegex _
    | Ast.ExtensionAssignment _ 
        ->
        evalExtendSymbols ast

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
        ->
        evalDefinitions ast

    // Rules of inference
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

    // Special references
    | Intrinsic _
    | Undefined _
    | SelfOrParent _
    | Self _
    | Parent _
    | Extension _
        ->
        evalSpecRef ast

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

/// <summary>
/// Create and populate the global symbol table by evaluating parsed ASTs.
/// </summary>
/// <returns>Unit. The function mutates global interpreter state: it orders and evaluates parsed ASTs,
/// registers top-level theories in <c>heap.Root</c>, runs their semantic initialization, and emits diagnostics.</returns>
/// <remarks>
/// - The function sets <c>evalRef.Value</c> to the local <c>eval</c> dispatcher and iterates over
///   parsed ASTs until no pending uses-clauses remain. For each parsed AST it clears working memory,
///   constructs an <c>FplTheory</c> frame, evaluates its building blocks, marks the parsed AST as evaluated,
///   and invokes <c>Run</c> on the created theory frame.
/// - Side effects include multiple heap and diagnostics updates; callers should ensure the parsing
///   stage completed successfully before invoking this function.
/// </remarks>
/// <exceptions cref="System.Exception">May propagate exceptions from the evaluator or diagnostics emitter.</exceptions>
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
            diagnosticsContainer.CurrentUri <- pa.Parsing.Uri
            pa.Parsing.BuildingBlockAsts
            |> List.map (fun buildinBlockAst -> evalRef.Value buildinBlockAst) |> ignore
            pa.Status <- ParsedAstStatus.Evaluated
            heap.Eval.PopEvalStack()
            theoryValue.Run()
        | None -> found <- false


