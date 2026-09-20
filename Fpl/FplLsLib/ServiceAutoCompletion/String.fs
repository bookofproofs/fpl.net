module FplLsLib.ServiceAutoCompletion.String

open System.Collections.Generic
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open FplLsLib.ServiceAutoCompletion.Item
open FplLsLib.ServiceAutoCompletion.Choices

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
