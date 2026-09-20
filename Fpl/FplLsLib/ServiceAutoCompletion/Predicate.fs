module FplLsLib.ServiceAutoCompletion.Predicate

open System.Collections.Generic
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open Fpl.Primitives
open FplLsLib.ServiceAutoCompletion.Item
open FplLsLib.ServiceAutoCompletion.Choices

/// <summary>
/// Completion-item choice provider for predicate operators and keywords.
/// </summary>
type FplCompletionItemChoicesPredicate() =
    inherit FplCompletionItemChoices()

    static member private CreateWithBody(baseCi: FplCompletionItem, numbOfArgs: int) : FplCompletionItem =
        let insert =
            match numbOfArgs with
            | 0 -> baseCi.Word + " "
            | 1 -> baseCi.Word + " true "
            | 2 -> baseCi.Word + " ( false, true ) "
            | _ -> baseCi.Word + " ( true, false ) "
        baseCi
            .WithInsertText(insert)
            .WithLabel(baseCi.Label + " ...")

    static member private CreateEquality(baseCi: FplCompletionItem) : FplCompletionItem =
        let insert = " (x = y) "
        baseCi
            .WithInsertText(insert)
            .WithLabel(FplCompletionItemChoices.TokenPrefix + insert + "...")
            .WithDetail(PrimDelegateEqual)
            .WithSortText("(")
            .WithKind(CompletionItemKind.Operator)

    override this.GetChoices(defaultCi: FplCompletionItem) : List<FplCompletionItem> =
        let ret = List<FplCompletionItem>()
        // snippets / keywords (create new instances instead of mutating)
        match defaultCi.Word with
        | LiteralTrue
        | LiteralFalse
        | LiteralUndef
        | LiteralUndefL ->
            // keyword
            ret.Add(defaultCi.WithKeyword())
        | LiteralNot ->
            // snippet
            ret.Add(FplCompletionItemChoicesPredicate.CreateWithBody(defaultCi, 1))
            // keyword
            ret.Add(defaultCi.WithKeyword())
        | LiteralIif
        | LiteralImpl ->
            // snippet
            ret.Add(FplCompletionItemChoicesPredicate.CreateWithBody(defaultCi, 2))
            // keyword
            ret.Add(defaultCi.WithKeyword())
        | "(" ->
            // snippet for equality
            ret.Add(FplCompletionItemChoicesPredicate.CreateEquality(defaultCi))
        | LiteralAnd
        | LiteralOr
        | LiteralXor ->
            ret.Add(FplCompletionItemChoicesPredicate.CreateWithBody(defaultCi, 3))
            ret.Add(defaultCi.WithKeyword())
        | _ -> ()
        ret
