module Fpl3LanguageServer.ServiceAutoCompletion.TheoremLikeStmt

open System.Collections.Generic
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open Fpl3LanguageServer.ServiceAutoCompletion.Item
open Fpl3LanguageServer.ServiceAutoCompletion.Choices
open Fpl3LanguageServer.ServiceAutoCompletion.Axiom

/// <summary>
/// Completion-item choice provider for theorem-like statements (theorem, lemma, proposition, etc.),
/// parameterized by the statement type name used in the generated snippet.
/// </summary>
type FplCompletionItemChoicesTheoremLikeStmt(statementType: string) =
    inherit FplCompletionItemChoices()

    let _statementType = statementType.Trim()

    member this.BuildBody(baseCi: FplCompletionItem) : FplCompletionItem =
        let mutable ci =
            baseCi
                .WithLabel(baseCi.Label + " ...")
                .WithInsertText(FplCompletionItemChoicesAxiom.GetBody(baseCi.Word, _statementType))
        if baseCi.IsShort then
            ci <- ci.WithDetail($"{_statementType.ToLower()} (short)")
        ci

    override this.GetChoices(defaultCi: FplCompletionItem) : List<FplCompletionItem> =
        let ret = List<FplCompletionItem>()
        // snippet
        let ci = this.BuildBody(defaultCi)
        ret.Add(ci)

        // keywords
        // ensure keyword variant uses the short-form sort marker when the base is short
        ret.Add(defaultCi.WithIsShort(defaultCi.IsShort).WithKind(CompletionItemKind.Keyword).WithKeyword())
        ret
