module FplLsLib.ServiceAutoCompletion.Definition

open System
open System.Collections.Generic
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open Fpl.Primitives
open FplLsLib.ServiceAutoCompletion.Item
open FplLsLib.ServiceAutoCompletion.Choices

/// <summary>
/// Completion-item choice provider for class/predicate/function definitions.
/// </summary>
type FplCompletionItemChoicesDefinition() =
    inherit FplCompletionItemChoices()

    /// <summary>
    /// Builds the keyword-form label for the given definition type.
    /// </summary>
    member private this.GetLabelKeyword(definitionType: string, ci: FplCompletionItem) : string =
        match definitionType with
        | "Class" -> $"{FplCompletionItemChoices.TokenPrefix}{ci.Word} {this.TokenClass}"
        | "Function" -> $"{FplCompletionItemChoices.TokenPrefix}{ci.Word} {this.TokenFunction}"
        | _ -> $"{FplCompletionItemChoices.TokenPrefix}{ci.Word} {this.TokenPredicate}"

    /// <summary>
    /// Builds the snippet body for the given definition type.
    /// </summary>
    member this.GetBody(definitionType: string, ci: FplCompletionItem) : string =
        let label = this.GetLabelKeyword(definitionType, ci)
        let ret =
            match definitionType with
            | "Class" -> $"{label.[FplCompletionItemChoices.TokenPrefix.Length ..]} SomeFpl{definitionType}: {this.TokenObject}"
            | "Function" -> $"{label.[FplCompletionItemChoices.TokenPrefix.Length ..]} SomeFpl{definitionType}() -> {this.TokenObject}"
            | _ -> $"{label.[FplCompletionItemChoices.TokenPrefix.Length ..]} SomeFpl{definitionType}()"
        ret + Environment.NewLine

    /// <summary>
    /// Builds a single completion item (snippet or keyword variant) for the given definition type.
    /// </summary>
    member private this.BuildDefinition(baseCi: FplCompletionItem, definitionType: string, forKeyword: bool) : FplCompletionItem =
        if baseCi.IsShort then
            this.TokenIntrinsic <- LiteralIntr
            this.TokenObject <- LiteralObj
            this.TokenFunction <- LiteralFunc
            this.TokenPredicate <- LiteralPred
            this.TokenClass <- LiteralCl

            // compute the base sort for each subtype (short forms include leading 'z')
            let baseSort =
                "z" +
                (match definitionType with
                 | "Class" -> "definition01"
                 | "Function" -> "definition03"
                 | _ -> "definition02")

            if forKeyword then
                let label = this.GetLabelKeyword(definitionType, baseCi)
                // ensure keyword variant receives the proper short-marked base sort
                baseCi.WithLabel(label).WithSortText(baseSort)
            else
                let label = this.GetLabelKeyword(definitionType, baseCi) + " ..."
                let detail = $"{definitionType.ToLower()} definition (short)"
                baseCi.WithLabel(label).WithDetail(detail).WithSortText(baseSort).WithInsertText(this.GetBody(definitionType, baseCi))
        else
            // non-short / long forms: compute base sort WITHOUT 'z'
            let baseSort =
                match definitionType with
                | "Class" -> "definition01"
                | "Function" -> "definition03"
                | _ -> "definition02"

            if forKeyword then
                let label = this.GetLabelKeyword(definitionType, baseCi)
                // ensure keyword variant has correct base sort so .WithKeyword() prefixes "zzz" correctly
                baseCi.WithLabel(label).WithSortText(baseSort)
            else
                let label = this.GetLabelKeyword(definitionType, baseCi) + " ..."
                let detail = $"{definitionType.ToLower()} definition"
                baseCi.WithLabel(label).WithDetail(detail).WithSortText(baseSort).WithInsertText(this.GetBody(definitionType, baseCi))

    override this.GetChoices(defaultCi: FplCompletionItem) : List<FplCompletionItem> =
        List<FplCompletionItem>(
            [ // snippets
              this.BuildDefinition(defaultCi, "Class", false)
              this.BuildDefinition(defaultCi, "Predicate", false)
              this.BuildDefinition(defaultCi, "Function", false)

              // keyword variants
              this.BuildDefinition(defaultCi.WithKind(CompletionItemKind.Keyword), "Class", true).WithKeyword()
              this.BuildDefinition(defaultCi.WithKind(CompletionItemKind.Keyword), "Predicate", true).WithKeyword()
              this.BuildDefinition(defaultCi.WithKind(CompletionItemKind.Keyword), "Function", true).WithKeyword() ]
        )
