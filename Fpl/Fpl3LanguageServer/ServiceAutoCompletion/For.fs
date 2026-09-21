module Fpl3LanguageServer.ServiceAutoCompletion.For

open System
open System.Collections.Generic
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open Fpl3LanguageServer.ServiceAutoCompletion.Item
open Fpl3LanguageServer.ServiceAutoCompletion.Choices

/// <summary>
/// Completion-item choice provider for the "for" loop statement.
/// </summary>
type FplCompletionItemChoicesFor() =
    inherit FplCompletionItemChoices()

    /// <summary>
    /// Builds a single "for" snippet completion item for the given sub-type
    /// (0 = range, 1 = list, 2 = type).
    /// </summary>
    static member private BuildForSnippet(baseCi: FplCompletionItem, subType: int) : FplCompletionItem =
        let firstLine, sortTextInitial, detail, label =
            if subType = 0 then
                $"for i in Range(){Environment.NewLine}", "for01", "for statement (range)", $"{FplCompletionItemChoices.TokenPrefix}for ... []"
            elif subType = 1 then
                $"for i in someList{Environment.NewLine}", "for02", "for statement (list)", $"{FplCompletionItemChoices.TokenPrefix}for ... list"
            else
                $"for i in SomeFplType{Environment.NewLine}", "for03", "for statement (type)", $"{FplCompletionItemChoices.TokenPrefix}for ... type"

        let sortText =
            if baseCi.IsShort then
                "z" + sortTextInitial
            else
                sortTextInitial

        let insert =
            firstLine +
            $"{FplCompletionItemChoices.TokenLeftBrace}{Environment.NewLine}" +
            $"\tx[i] := 1{Environment.NewLine}" +
            $"{FplCompletionItemChoices.TokenRightBrace}{Environment.NewLine}"

        baseCi.WithSortText(sortText).WithDetail(detail).WithLabel(label).WithInsertText(insert)

    override this.GetChoices(defaultCi: FplCompletionItem) : List<FplCompletionItem> =
        let ret =
            List<FplCompletionItem>(
                [ // snippets
                  FplCompletionItemChoicesFor.BuildForSnippet(defaultCi, 0)
                  FplCompletionItemChoicesFor.BuildForSnippet(defaultCi, 1)
                  FplCompletionItemChoicesFor.BuildForSnippet(defaultCi, 2) ]
            )

        // keywords -> do not mutate default; preserve short-marker and ensure base sort respects short form
        let baseSort = if defaultCi.IsShort then "zfor03" else "for03"
        ret.Add(defaultCi.WithSortText(baseSort).WithIsShort(defaultCi.IsShort).WithKind(CompletionItemKind.Keyword).WithKeyword())
        ret
