module Fpl3LanguageServer.ServiceAutoCompletion.RuleOfInference

open System
open System.Collections.Generic
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open Fpl3LanguageServer.ServiceAutoCompletion.Item
open Fpl3LanguageServer.ServiceAutoCompletion.Choices

/// <summary>
/// Completion-item choice provider for rules of inference.
/// </summary>
type FplCompletionItemChoicesRuleOfInference() =
    inherit FplCompletionItemChoices()

    override this.GetChoices(defaultCi: FplCompletionItem) : List<FplCompletionItem> =
        let ret = List<FplCompletionItem>()
        // snippets
        let mutable ci =
            defaultCi.WithLabel(defaultCi.Label + " ...").WithInsertText(
                $"{defaultCi.Word} SomeFplRuleOfInference(){Environment.NewLine}" +
                $"{FplCompletionItemChoices.TokenLeftBrace}{Environment.NewLine}" +
                $"\t{this.TokenPremise}:{Environment.NewLine}" +
                $"\t\ttrue{Environment.NewLine}" +
                $"\t{this.TokenConclusion}:{Environment.NewLine}" +
                $"\t\ttrue{Environment.NewLine}" +
                $"{FplCompletionItemChoices.TokenRightBrace}{Environment.NewLine}"
            )
        if defaultCi.IsShort then
            ci <- ci.WithDetail("inference (short)")
        ret.Add(ci)

        // keywords
        ret.Add(defaultCi.WithKind(CompletionItemKind.Keyword).WithKeyword())
        ret
