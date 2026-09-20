module FplLsLib.ServiceAutoCompletion.Whitespace

open System.Collections.Generic
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open FplLsLib.ServiceAutoCompletion.Item
open FplLsLib.ServiceAutoCompletion.Choices

/// <summary>
/// Completion-item choice provider for whitespace.
/// </summary>
type FplCompletionItemChoicesWhitespace() =
    inherit FplCompletionItemChoices()

    override this.GetChoices(defaultCi: FplCompletionItem) : List<FplCompletionItem> =
        let ret = List<FplCompletionItem>()
        let ci =
            FplCompletionItem(" ")
                .WithInsertText(" ")
                .WithLabel(FplCompletionItemChoices.TokenPrefix + "' '")
                .WithKind(CompletionItemKind.Text)
                .WithDetail("(whitespace)")
                .WithSortText("zzzzz") // make sure whitespace appears at the end of any list.
        ret.Add(ci)
        ret
