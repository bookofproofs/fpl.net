module FplLsLib.ServiceAutoCompletion.ExtensionString

open System.Collections.Generic
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open FplLsLib.ServiceAutoCompletion.Item
open FplLsLib.ServiceAutoCompletion.Choices

/// <summary>
/// Completion-item choice provider for extension regex strings.
/// </summary>
type FplCompletionItemChoicesExtensionString() =
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
