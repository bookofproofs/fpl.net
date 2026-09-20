module Fpl3LanguageServer.ServiceAutoCompletion.Regex

open System.Collections.Generic
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open Fpl3LanguageServer.ServiceAutoCompletion.Item
open Fpl3LanguageServer.ServiceAutoCompletion.Choices

/// <summary>
/// Completion-item choice provider for regex literal snippets.
/// </summary>
type FplCompletionItemChoicesRegex() =
    inherit FplCompletionItemChoices()

    override this.GetChoices(defaultCi: FplCompletionItem) : List<FplCompletionItem> =
        let ret = List<FplCompletionItem>()

        let ci =
            defaultCi
                .WithDetail(defaultCi.Word)
                .WithInsertText("/+\\d/ ")
                .WithLabel(FplCompletionItemChoices.TokenPrefix + "some regex ...")
                .WithKind(CompletionItemKind.Text)
        ret.Add(ci)

        ret
