module FplLsLib.ServiceAutoCompletion.Extension

open System
open System.Collections.Generic
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open FplLsLib.ServiceAutoCompletion.Item
open FplLsLib.ServiceAutoCompletion.Choices

/// <summary>
/// Completion-item choice provider for extensions.
/// </summary>
type FplCompletionItemChoicesExtension() =
    inherit FplCompletionItemChoices()

    override this.GetChoices(defaultCi: FplCompletionItem) : List<FplCompletionItem> =
        let ret = List<FplCompletionItem>()

        // snippets
        let mutable insert = $"{defaultCi.Word} Digits x@/\\d+/ -> SomeType"
        insert <- insert + $"\t{FplCompletionItemChoices.TokenLeftBrace}{Environment.NewLine}"
        insert <- insert + $"\t\tmcases{Environment.NewLine}"
        insert <- insert + $"\t\t({Environment.NewLine}"
        insert <- insert + $"\t\t\t| (x = @0): Zero{Environment.NewLine}"
        insert <- insert + $"\t\t\t| (x = @1): One{Environment.NewLine}"
        insert <- insert + $"\t\t\t| (x = @2): Two{Environment.NewLine}"
        insert <- insert + $"\t\t\t? undef{Environment.NewLine}"
        insert <- insert + $"\t\t){Environment.NewLine}"
        insert <- insert + $"\t{FplCompletionItemChoices.TokenRightBrace}{Environment.NewLine}"

        let ci = defaultCi.WithInsertText(insert).WithLabel(defaultCi.Label + " ...")
        ret.Add(ci)

        // keywords (don't mutate default) — preserve short-marker for correct keyword sort-texts
        ret.Add(defaultCi.WithIsShort(defaultCi.IsShort).WithKind(CompletionItemKind.Keyword).WithKeyword())

        ret
