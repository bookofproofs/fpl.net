module FplLsLib.ServiceAutoCompletion.Declaration

open System
open System.Collections.Generic
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open FplLsLib.ServiceAutoCompletion.Item
open FplLsLib.ServiceAutoCompletion.Choices

/// <summary>
/// Completion-item choice provider for declarations.
/// </summary>
type FplCompletionItemChoicesDeclaration() =
    inherit FplCompletionItemChoices()

    /// <summary>
    /// Builds the declaration snippet completion item based on the given default item.
    /// </summary>
    member private this.BuildDeclarationSnippet(baseCi: FplCompletionItem) : FplCompletionItem =
        let label = baseCi.Label + " ..."
        let insert =
            $"{baseCi.Word}{Environment.NewLine}" +
            $"\tx: {this.TokenObject}{Environment.NewLine}" +
            $"\ty: {this.TokenObject}{Environment.NewLine}" +
            $"\tx := 0{Environment.NewLine}" +
            $"\ty := 1{Environment.NewLine}" +
            $";{Environment.NewLine}"
        baseCi.WithLabel(label).WithInsertText(insert)

    override this.GetChoices(defaultCi: FplCompletionItem) : List<FplCompletionItem> =
        List<FplCompletionItem>(
            [ // snippet
              this.BuildDeclarationSnippet(defaultCi)
              // keywords - preserve short-marker so keyword sort-texts match expectations
              defaultCi.WithIsShort(defaultCi.IsShort).WithKind(CompletionItemKind.Keyword).WithKeyword() ]
        )
