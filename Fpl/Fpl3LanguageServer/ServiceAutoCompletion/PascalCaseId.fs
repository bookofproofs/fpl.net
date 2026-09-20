module Fpl3LanguageServer.ServiceAutoCompletion.PascalCaseId

open System.Collections.Generic
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open Fpl3LanguageServer.ServiceAutoCompletion.Item
open Fpl3LanguageServer.ServiceAutoCompletion.Choices

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
