module FplLsLib.ServiceAutoCompletion.Corollary

open System.Collections.Generic
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open FplLsLib.ServiceAutoCompletion.Item
open FplLsLib.ServiceAutoCompletion.Choices
open FplLsLib.ServiceAutoCompletion.Axiom

/// <summary>
/// Completion-item choice provider for corollaries.
/// </summary>
type FplCompletionItemChoicesCorollary() =
    inherit FplCompletionItemChoices()

    override this.GetChoices(defaultCi: FplCompletionItem) : List<FplCompletionItem> =
        let ret = List<FplCompletionItem>()

        // snippet
        let ci =
            defaultCi
                .WithInsertText(FplCompletionItemChoicesAxiom.GetBody(defaultCi.Word, "Theorem$1"))
                .WithLabel(defaultCi.Label + " ...")
        let ci =
            if defaultCi.IsShort then
                ci.WithDetail("corollary (short)").WithSortText("z" + defaultCi.SortText)
            else
                ci
        ret.Add(ci)

        // keywords (immutable) — preserve short-marker so keyword sort-texts match expectations
        ret.Add(defaultCi.WithIsShort(defaultCi.IsShort).WithKind(CompletionItemKind.Keyword).WithKeyword())

        ret
