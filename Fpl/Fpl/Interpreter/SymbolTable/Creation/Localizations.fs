(* Copyright (c) 2021+ bookofproofs See LICENSE in the project root for license terms. *)

/// <summary>
/// Provides specialized evaluators for AST nodes that represent localizations of
/// mathematical language in FPL (translation terms, languages, and localization strings).
/// </summary>
/// <remarks>
/// The evaluator registers localization-related frames on the global interpreter symbol-table
/// <c>heap</c>, evaluates nested ASTs by delegating to <c>evalRef.Value</c>, and emits
/// diagnostics for unused variables found inside translations. It relies on types defined
/// in <c>Fpl.Interpreter.SymbolTable.Types3.Localization</c> and helper functions such as
/// <c>emitVAR04Diagnostics</c> and <c>isLanguage</c>.
/// </remarks>
/// <exception cref="System.Exception">Thrown when an unsupported AST node is supplied to the top-level evaluator.</exception>
module Fpl.Interpreter.SymbolTable.Creation.Localizations
open System
open System.Collections.Generic
open Fpl.Parser.Types
open Fpl.Errors.Emitter
open Fpl.Interpreter.Helpers.Checks
open Fpl.Interpreter.SymbolTable.Storage.Heap
open Fpl.Interpreter.SymbolTable.Types2.Variables
open Fpl.Interpreter.SymbolTable.Types3.Localization
open Fpl.Interpreter.SymbolTable.Creation.Forward

/// <summary>
/// Choose a random element from a non-empty list of AST nodes.
/// </summary>
/// <param name="lst">A non-empty list of AST nodes to sample from.</param>
/// <returns>
/// One element randomly selected from <paramref name="lst"/>. Selection uses a new
/// <c>System.Random</c> instance per call.
/// </returns>
/// <remarks>
/// This helper is intentionally local and non-deterministic; callers that require stable
/// behavior should provide a deterministic selection mechanism instead.
/// </remarks>
/// <exception cref="System.ArgumentOutOfRangeException">Thrown when <paramref name="lst"/> is empty.</exception>
let private chooseRandomMember (lst: Ast list) =
    let rnd = Random()
    let index = rnd.Next(lst.Length)
    lst.[index]

/// <summary>
/// Evaluate AST nodes related to localization constructs and register them in the symbol table.
/// </summary>
/// <param name="ast">The AST node to evaluate. Expected node shapes include:
/// <c>Ast.Localization</c>, <c>Ast.TranslationTerm</c>, <c>Ast.TranslationTermList</c>,
/// <c>Ast.Language</c>, <c>Ast.LanguageCode</c>, and <c>Ast.LocalizationString</c>.</param>
/// <returns>Unit. The function has side effects on the global evaluation <c>heap</c>
/// (pushing/popping frames, setting identifiers and types, and emitting diagnostics).</returns>
/// <remarks>
/// - <c>Ast.Localization</c>: creates an <c>FplLocalization</c> frame, evaluates the predicate
///   signature while in signature-evaluation mode, evaluates translation entries, collects
///   unused variables (to emit VAR04 diagnostics), and sets the localization <c>FplId</c>
///   and <c>TypeId</c> based on its argument list.
/// - <c>Ast.TranslationTerm</c>: creates an <c>FplTranslation</c> frame per subterm and evaluates it.
/// - <c>Ast.TranslationTermList</c>: delegates to <c>chooseRandomMember</c> to pick a single
///   alternative translation term to evaluate (non-deterministic selection).
/// - <c>Ast.Language</c>: creates an <c>FplLanguage</c> frame, evaluates the language code
///   and the corresponding EBNF AST, then removes the temporary language frame.
/// - <c>Ast.LanguageCode</c> and <c>Ast.LocalizationString</c>: assign string identifiers to the
///   current frame (<c>FplId</c>, <c>TypeId</c>, and positions where applicable).
/// </remarks>
/// <exception cref="System.Exception">Thrown when <paramref name="ast"/> is not a localization or related node.</exception>
let evalLocalizations ast =
    match ast with
    | Ast.Localization(((pos1, pos2), predicateAst), translationListAsts) ->
        let parent = heap.Eval.PeekEvalStack()
        let fv = new FplLocalization((pos1, pos2), parent, heap.Helper.GetNextAvailableFplBlockRunOrder)
        let var04List = List<KeyValuePair<string, Positions>>()
        heap.Eval.PushEvalStack(fv)
        heap.Helper.InSignatureEvaluation <- true
        evalRef.Value predicateAst
        heap.Helper.InSignatureEvaluation <- false
        translationListAsts |> List.map (fun subAst -> 
            evalRef.Value subAst
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
            fv.ErrorOccurred <- emitVAR04Diagnostics kvp.Key (fst kvp.Value) (snd kvp.Value)
        )
    | Ast.TranslationTerm((pos1, pos2), asts) ->
        let fv = heap.Eval.PeekEvalStack()
        asts |> List.map (fun ebnfTerm ->
            let trsl = new FplTranslation((pos1, pos2), fv)
            heap.Eval.PushEvalStack(trsl)
            evalRef.Value ebnfTerm
            heap.Eval.PopEvalStack()
        ) |> ignore
    | Ast.TranslationTermList((pos1, pos2), ebnfTermAsts) ->
        evalRef.Value (chooseRandomMember ebnfTermAsts)
    | Ast.Language((pos1, pos2),(langCode, ebnfAst)) ->
        let fv = heap.Eval.PeekEvalStack()
        let lang = new FplLanguage((pos1, pos2), fv) 
        heap.Eval.PushEvalStack(lang)
        evalRef.Value langCode
        evalRef.Value ebnfAst
        heap.Eval.PopEvalStack() // remove language
    | Ast.LanguageCode((pos1, pos2), s) -> 
        let fv = heap.Eval.PeekEvalStack()
        fv.FplId <- s
        fv.TypeId <- s
        fv.StartPos <- pos1
        fv.EndPos <- pos2
    | Ast.LocalizationString((pos1, pos2), s) -> 
        let fv = heap.Eval.PeekEvalStack()
        fv.FplId <- s
        fv.TypeId <- s
    | _ ->
        failwith (sprintf "{%O} is not a localization or a related node" ast)
