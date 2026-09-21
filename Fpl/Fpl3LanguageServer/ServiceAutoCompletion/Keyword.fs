module Fpl3LanguageServer.ServiceAutoCompletion.Keyword

open System.Collections.Generic
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open Fpl3LanguageServer.ServiceAutoCompletion.Item
open Fpl3LanguageServer.ServiceAutoCompletion.Choices

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
