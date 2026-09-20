module FplLsLib.ServiceAutoCompletion.MapCases

open System
open System.Collections.Generic
open FplLsLib.ServiceAutoCompletion.Item
open FplLsLib.ServiceAutoCompletion.Choices

/// <summary>
/// Completion-item choice provider for "mcases" snippets.
/// </summary>
type FplCompletionItemChoicesMapCases() =
    inherit FplCompletionItemChoices()

    override this.GetChoices(defaultCi: FplCompletionItem) : List<FplCompletionItem> =
        let ret = List<FplCompletionItem>()

        // snippets
        let insert =
            "mcases" + Environment.NewLine
            + "(" + Environment.NewLine
            + "\t| p(x): a" + Environment.NewLine
            + "\t| q(x): b" + Environment.NewLine
            + "\t? c" + Environment.NewLine
            + ")" + Environment.NewLine
        let ci =
            defaultCi
                .WithInsertText(insert)
                .WithLabel(defaultCi.Label + " ...")
        ret.Add(ci)

        // keywords
        let keyword = defaultCi.WithKeyword()
        ret.Add(keyword)

        ret
