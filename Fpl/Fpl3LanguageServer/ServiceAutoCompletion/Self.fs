module Fpl3LanguageServer.ServiceAutoCompletion.Self

open System.Collections.Generic
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open Fpl0Base.Primitives
open Fpl3LanguageServer.ServiceAutoCompletion.Item
open Fpl3LanguageServer.ServiceAutoCompletion.Choices

/// <summary>
/// Completion-item choice provider for "self", "parent", and "base" references.
/// </summary>
type FplCompletionItemChoicesSelf() =
    inherit FplCompletionItemChoices()

    override this.GetChoices(defaultCi: FplCompletionItem) : List<FplCompletionItem> =
        let ret = List<FplCompletionItem>()

        if defaultCi.Word = LiteralSelf then
            let insert = "self "
            let ci =
                defaultCi
                    .WithInsertText(insert)
                    .WithLabel(FplCompletionItemChoices.TokenPrefix + insert)
                    .WithDetail("self reference")
                    .WithKind(CompletionItemKind.Reference)
                    .WithSortText("self01")
            ret.Add(ci)

        if defaultCi.Word = LiteralParent then
            let insert = "parent "
            let ci =
                defaultCi
                    .WithInsertText(insert)
                    .WithLabel(FplCompletionItemChoices.TokenPrefix + insert)
                    .WithDetail("parent self reference")
                    .WithKind(CompletionItemKind.Reference)
                    .WithSortText("parent02")
            ret.Add(ci)

        if defaultCi.Word = LiteralBase then
            let insert = LiteralBase
            let ci =
                defaultCi
                    .WithInsertText(insert)
                    .WithLabel(FplCompletionItemChoices.TokenPrefix + insert)
                    .WithDetail("ctor call (parent class)")
                    .WithKind(CompletionItemKind.Reference)
                    .WithSortText("self03")
            ret.Add(ci)

        ret
