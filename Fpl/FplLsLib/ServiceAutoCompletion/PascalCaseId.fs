module FplLsLib.ServiceAutoCompletion.PascalCaseId

open System.Collections.Generic
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open FplLsLib.ServiceAutoCompletion.Item
open FplLsLib.ServiceAutoCompletion.Choices

/// <summary>
/// Completion-item choice provider for PascalCase user-defined identifiers.
/// </summary>
type FplCompletionItemChoicesPascalCaseId() =
    inherit FplCompletionItemChoices()

    override this.GetChoices(defaultCi: FplCompletionItem) : List<FplCompletionItem> =
        let ret = List<FplCompletionItem>()
        let ci =
            defaultCi
                .WithDetail("user-defined id")
                .WithInsertText(defaultCi.Word + " ")
                .WithLabel(FplCompletionItemChoices.TokenPrefix + defaultCi.Word + " " + " ...")
                .WithKind(CompletionItemKind.Reference)
        ret.Add(ci)
        ret
