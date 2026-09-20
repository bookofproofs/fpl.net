module FplLsLib.ServiceAutoCompletion.Keyword

open System.Collections.Generic
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open FplLsLib.ServiceAutoCompletion.Item
open FplLsLib.ServiceAutoCompletion.Choices

/// <summary>
/// Completion-item choice provider for plain keywords.
/// </summary>
type FplCompletionItemChoicesKeyword() =
    inherit FplCompletionItemChoices()

    override this.GetChoices(defaultCi: FplCompletionItem) : List<FplCompletionItem> =
        // keywords: create a new instance (do not mutate defaultCi) and preserve short-marker
        List<FplCompletionItem>(
            [ defaultCi.WithIsShort(defaultCi.IsShort).WithKind(CompletionItemKind.Keyword).WithKeyword() ]
        )
