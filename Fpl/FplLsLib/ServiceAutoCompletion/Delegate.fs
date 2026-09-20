module FplLsLib.ServiceAutoCompletion.Delegate

open System.Collections.Generic
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open FplLsLib.ServiceAutoCompletion.Item
open FplLsLib.ServiceAutoCompletion.Choices

/// <summary>
/// Completion-item choice provider for delegates.
/// </summary>
type FplCompletionItemChoicesDelegate() =
    inherit FplCompletionItemChoices()

    override this.GetChoices(defaultCi: FplCompletionItem) : List<FplCompletionItem> =
        let ret = List<FplCompletionItem>()
        let insert = defaultCi.Word + ".SomeExternalMethod(x,1) "
        let ci =
            defaultCi
                .WithInsertText(insert)
                .WithLabel(FplCompletionItemChoices.TokenPrefix + insert)
                .WithKind(CompletionItemKind.Event)
        ret.Add(ci)
        ret
