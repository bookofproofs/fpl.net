module FplLsLib.ServiceAutoCompletion.Cases

open System
open System.Collections.Generic
open FplLsLib.ServiceAutoCompletion.Item
open FplLsLib.ServiceAutoCompletion.Choices

/// <summary>
/// Completion-item choice provider for the "cases" statement.
/// </summary>
type FplCompletionItemChoicesCases() =
    inherit FplCompletionItemChoices()

    override this.GetChoices(defaultCi: FplCompletionItem) : List<FplCompletionItem> =
        let ret = List<FplCompletionItem>()

        // snippets
        let insert =
            $"cases{Environment.NewLine}" +
            $"({Environment.NewLine}" +
            $"\t| p(x): y := a{Environment.NewLine}" +
            $"\t| q(x): y := b{Environment.NewLine}" +
            $"\t? y := c{Environment.NewLine}" +
            $"){Environment.NewLine}"
        let ci =
            defaultCi
                .WithInsertText(insert)
                .WithLabel(defaultCi.Label + " ...")
        ret.Add(ci)

        // keywords
        let keyword = defaultCi.WithKeyword()
        ret.Add(keyword)

        ret
