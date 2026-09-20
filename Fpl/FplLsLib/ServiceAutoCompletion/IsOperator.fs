module FplLsLib.ServiceAutoCompletion.IsOperator

open System.Collections.Generic
open FplLsLib.ServiceAutoCompletion.Item
open FplLsLib.ServiceAutoCompletion.Choices

/// <summary>
/// Completion-item choice provider for the "is" operator.
/// </summary>
type FplCompletionItemChoicesIsOperator() =
    inherit FplCompletionItemChoices()

    override this.GetChoices(defaultCi: FplCompletionItem) : List<FplCompletionItem> =
        let ret = List<FplCompletionItem>()

        // snippet
        let ci =
            defaultCi
                .WithInsertText("is (x, SomeFplType)")
                .WithLabel(FplCompletionItemChoices.TokenPrefix + defaultCi.InsertText + " ...")
        ret.Add(ci)

        // keywords
        let keyword = defaultCi.WithKeyword()
        ret.Add(keyword)

        ret
