module Fpl3LanguageServer.ServiceAutoCompletion.Digits

open System.Collections.Generic
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open Fpl3LanguageServer.ServiceAutoCompletion.Item
open Fpl3LanguageServer.ServiceAutoCompletion.Choices

/// <summary>
/// Completion-item choice provider for digit literals (plain and dollar-prefixed).
/// </summary>
type FplCompletionItemChoicesDigits() =
    inherit FplCompletionItemChoices()

    override this.GetChoices(defaultCi: FplCompletionItem) : List<FplCompletionItem> =
        let ret = List<FplCompletionItem>()

        let ci =
            if defaultCi.Word.Contains("dollar") then
                defaultCi
                    .WithDetail("$digits")
                    .WithInsertText("$123")
                    .WithSortText("$123")
                    .WithLabel(FplCompletionItemChoices.TokenPrefix + "$123")
                    .WithKind(CompletionItemKind.Text)
            else
                defaultCi
                    .WithDetail("digits")
                    .WithInsertText("123")
                    .WithSortText("123")
                    .WithLabel(FplCompletionItemChoices.TokenPrefix + "123")
                    .WithKind(CompletionItemKind.Text)

        ret.Add(ci)
        ret
