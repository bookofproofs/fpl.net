module Fpl3LanguageServer.ServiceAutoCompletion.Property

open System
open System.Collections.Generic
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open Fpl0Base.Primitives
open Fpl3LanguageServer.ServiceAutoCompletion.Item
open Fpl3LanguageServer.ServiceAutoCompletion.Choices

/// <summary>
/// Completion-item choice provider for predicate/function properties.
/// </summary>
type FplCompletionItemChoicesProperty() =
    inherit FplCompletionItemChoices()

    member this.GetLabelKeyword(ci: FplCompletionItem, propertyType: string) : string =
        match propertyType with
        | "Function" -> $"{FplCompletionItemChoices.TokenPrefix}{ci.Word} {this.TokenFunction}"
        | "Predicate"
        | _ -> $"{FplCompletionItemChoices.TokenPrefix}{ci.Word} {this.TokenPredicate}"

    member this.GetInsertText(ci: FplCompletionItem, propertyType: string) : string =
        let ret =
            match propertyType with
            | "Function" ->
                $"""{this.GetLabelKeyword(ci, propertyType).Replace("_ ", "")} SomeFpl{propertyType}Property() -> {this.TokenObject}{Environment.NewLine}"""
            | "Predicate"
            | _ ->
                $"""{this.GetLabelKeyword(ci, propertyType).Replace("_ ", "")} SomeFpl{propertyType}Property(){Environment.NewLine}"""
        ret +
        $"{FplCompletionItemChoices.TokenLeftBrace}{Environment.NewLine}" +
        $"\t{this.TokenIntrinsic}{Environment.NewLine}" +
        $"{FplCompletionItemChoices.TokenRightBrace}{Environment.NewLine}"

    member this.BuildProperty(baseCi: FplCompletionItem, propertyType: string, isKeyword: bool) : FplCompletionItem =
        // compute base sort text
        let mutable baseSort = if propertyType = "Function" then "property03" else "property02"
        if baseCi.IsShort then
            baseSort <- "z" + baseSort

        if baseCi.IsShort then
            this.TokenIntrinsic <- LiteralIntr
            this.TokenFunction <- LiteralFunc
            this.TokenPredicate <- LiteralPred
            if isKeyword then
                let label = this.GetLabelKeyword(baseCi, propertyType)
                let insert = label.Substring(FplCompletionItemChoices.TokenPrefix.Length)
                let detail = $"keywords '{insert}'"
                baseCi.WithLabel(label).WithDetail(detail).WithSortText("zzz" + baseSort).WithInsertText(insert).WithKind(CompletionItemKind.Keyword)
            else
                let label = this.GetLabelKeyword(baseCi, propertyType) + " ..."
                let insert = this.GetInsertText(baseCi, propertyType)
                let detail = $"{propertyType.ToLower()} property (short)"
                baseCi.WithLabel(label).WithDetail(detail).WithInsertText(insert).WithSortText(baseSort)
        else
            if isKeyword then
                let label = this.GetLabelKeyword(baseCi, propertyType)
                let insert = label.Substring(FplCompletionItemChoices.TokenPrefix.Length)
                let detail = $"keywords '{insert}'"
                baseCi.WithLabel(label).WithDetail(detail).WithSortText("zzz" + baseSort).WithInsertText(insert).WithKind(CompletionItemKind.Keyword)
            else
                let label = this.GetLabelKeyword(baseCi, propertyType) + " ..."
                let insert = this.GetInsertText(baseCi, propertyType)
                let detail = $"{propertyType.ToLower()} property"
                baseCi.WithLabel(label).WithDetail(detail).WithInsertText(insert).WithSortText(baseSort)

    override this.GetChoices(defaultCi: FplCompletionItem) : List<FplCompletionItem> =
        let ret = List<FplCompletionItem>()
        // snippets
        ret.Add(this.BuildProperty(defaultCi, "Predicate", false))
        ret.Add(this.BuildProperty(defaultCi, "Function", false))

        // keyword variants
        ret.Add(this.BuildProperty(defaultCi.WithKind(CompletionItemKind.Keyword), "Predicate", true))
        ret.Add(this.BuildProperty(defaultCi.WithKind(CompletionItemKind.Keyword), "Function", true))
        ret
