module Fpl3LanguageServer.ServiceAutoCompletion.Uses

open System
open System.Collections.Generic
open Fpl3LanguageServer.ServiceAutoCompletion.Item
open Fpl3LanguageServer.ServiceAutoCompletion.Choices

/// <summary>
/// Completion-item choice provider for the "uses" namespace import statement.
/// </summary>
type FplCompletionItemChoicesUses() =
    inherit FplCompletionItemChoices()

    override this.GetChoices(defaultCi: FplCompletionItem) : List<FplCompletionItem> =
        let ret = List<FplCompletionItem>()
        // snippets
        let ci =
            defaultCi
                .WithInsertText($"uses SomeFplNamespace{Environment.NewLine}")
                .WithDetail("uses namespace")
                .WithSortText("uses01")
                .WithLabel(defaultCi.Label + " ...")
        ret.Add(ci)
        let ci1 =
            defaultCi
                .WithInsertText($"uses SomeFplNamespace alias Sfn{Environment.NewLine}")
                .WithDetail("uses namespace with alias")
                .WithSortText("uses02")
                .WithLabel(defaultCi.Label + " ... alias")
        ret.Add(ci1)
        let ci2 =
            defaultCi
                .WithInsertText($"uses SomeFplNamespace *{Environment.NewLine}")
                .WithDetail("uses namespace (all subspaces)")
                .WithSortText("uses02")
                .WithLabel(defaultCi.Label + " (all) ... ")
        ret.Add(ci2)
        // keywords
        let ciKeyword = defaultCi.WithKeyword().WithSortText("uses03")
        ret.Add(ciKeyword)
        ret
