module FplLsLib.ServiceAutoCompletion.ArgumentIdentifier

open System.Collections.Generic
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open FplLsLib.ServiceAutoCompletion.Item
open FplLsLib.ServiceAutoCompletion.Choices

/// <summary>
/// Completion-item choice provider for argument identifiers (e.g. numbered argument references).
/// </summary>
type FplCompletionItemChoicesArgumentIdentifier() =
    inherit FplCompletionItemChoices()

    override this.GetChoices(defaultCi: FplCompletionItem) : List<FplCompletionItem> =
        let ret = List<FplCompletionItem>()
        let insert = "100:"
        let label = FplCompletionItemChoices.TokenPrefix + insert
        let ci =
            defaultCi
                .WithDetail(defaultCi.Word)
                .WithInsertText(insert)
                .WithLabel(label)
                .WithKind(CompletionItemKind.Unit)
        ret.Add(ci)
        ret
