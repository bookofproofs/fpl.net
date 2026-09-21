module Fpl3LanguageServer.ServiceAutoCompletion.String

open System.Collections.Generic
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open Fpl3LanguageServer.ServiceAutoCompletion.Item
open Fpl3LanguageServer.ServiceAutoCompletion.Choices

/// <summary>
/// Completion-item choice provider for string literals.
/// </summary>
type FplCompletionItemChoicesString() =
    inherit FplCompletionItemChoices()

    override this.GetChoices(defaultCi: FplCompletionItem) : List<FplCompletionItem> =
        let ret = List<FplCompletionItem>()

        let ci =
            defaultCi
                .WithDetail(defaultCi.Word)
                .WithSortText("\"\"")
                .WithInsertText("\"...\" ")
                .WithLabel(FplCompletionItemChoices.TokenPrefix + "\"...\"")
                .WithKind(CompletionItemKind.Value)

        ret.Add(ci)

        ret
