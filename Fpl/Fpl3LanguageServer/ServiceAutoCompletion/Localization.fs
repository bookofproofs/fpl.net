module Fpl3LanguageServer.ServiceAutoCompletion.Localization

open System
open System.Collections.Generic
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open Fpl3LanguageServer.ServiceAutoCompletion.Item
open Fpl3LanguageServer.ServiceAutoCompletion.Choices

/// <summary>
/// Completion-item choice provider for localization snippets.
/// </summary>
type FplCompletionItemChoicesLocalization() =
    inherit FplCompletionItemChoices()

    override this.GetChoices(defaultCi: FplCompletionItem) : List<FplCompletionItem> =
        let ret = List<FplCompletionItem>()

        // snippet
        let insert =
            defaultCi.Word + " iif(x,y) :=" + Environment.NewLine
            + "!tex: x \"\\Leftrightarrow\" y" + Environment.NewLine
            + "!eng: x \" if and only if \" y" + Environment.NewLine
            + "!ger: x \" dann und nur dann \" y" + Environment.NewLine
            + ";" + Environment.NewLine
        let ci =
            defaultCi
                .WithInsertText(insert)
                .WithLabel(defaultCi.Label + " ...")
        ret.Add(ci)

        // keywords (immutable) — preserve short-marker so keyword sort-texts match expectations
        ret.Add(defaultCi.WithIsShort(defaultCi.IsShort).WithKind(CompletionItemKind.Keyword).WithKeyword())

        ret
