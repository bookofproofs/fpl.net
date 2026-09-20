module FplLsLib.ServiceAutoCompletion.Word

open System.Collections.Generic
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open FplLsLib.ServiceAutoCompletion.Item
open FplLsLib.ServiceAutoCompletion.Choices

/// <summary>
/// Completion-item choice provider for generic word/identifier completions.
/// </summary>
type FplCompletionItemChoicesWord() =
    inherit FplCompletionItemChoices()

    override this.GetChoices(defaultCi: FplCompletionItem) : List<FplCompletionItem> =
        let ret = List<FplCompletionItem>()
        let insert = "someIdentifier "
        let ci =
            defaultCi
                .WithDetail(@"regex \w+")
                .WithInsertText(insert)
                .WithLabel(FplCompletionItemChoices.TokenPrefix + insert)
                .WithKind(CompletionItemKind.Value)
        ret.Add(ci)
        ret
