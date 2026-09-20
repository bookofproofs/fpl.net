module FplLsLib.ServiceAutoCompletion.Quantifier

open System.Collections.Generic
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open Fpl.Primitives
open FplLsLib.ServiceAutoCompletion.Item
open FplLsLib.ServiceAutoCompletion.Choices

/// <summary>
/// Completion-item choice provider for quantifiers (all, exists, exists n-times).
/// </summary>
type FplCompletionItemChoicesQuantifier() =
    inherit FplCompletionItemChoices()

    static member private GetBody() : string =
        " { p(x) } "

    static member private BuildQuantifierSnippet(baseCi: FplCompletionItem, postfix: string) : FplCompletionItem =
        let mutable sortText = $"{baseCi.Word}02"
        let label = $"{FplCompletionItemChoices.TokenPrefix}{baseCi.Word}{postfix} of type ..."
        let insert = $"{baseCi.Word}{postfix} x:FplType" + FplCompletionItemChoicesQuantifier.GetBody()
        if baseCi.IsShort then
            sortText <- "z" + sortText
        let detail =
            match baseCi.Word with
            | LiteralAll -> "all quantifier (in type)"
            | LiteralEx -> "exists quantifier (in type)"
            | LiteralExN -> "exists n-times quantifier (in type)"
            | _ -> baseCi.Detail
        baseCi.WithSortText(sortText).WithLabel(label).WithInsertText(insert).WithDetail(if isNull detail then "" else detail)

    override this.GetChoices(defaultCi: FplCompletionItem) : List<FplCompletionItem> =
        let ret = List<FplCompletionItem>()
        let postfix = if defaultCi.Word.Contains(LiteralExN) then "$1" else ""
        // snippet
        let ci = FplCompletionItemChoicesQuantifier.BuildQuantifierSnippet(defaultCi, postfix)
        ret.Add(ci)
        // keyword
        ret.Add(defaultCi.WithKind(CompletionItemKind.Keyword).WithKeyword())
        ret
