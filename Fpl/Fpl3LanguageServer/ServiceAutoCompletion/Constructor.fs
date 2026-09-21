module Fpl3LanguageServer.ServiceAutoCompletion.Constructor

open System
open System.Collections.Generic
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open Fpl0Base.Primitives
open Fpl3LanguageServer.ServiceAutoCompletion.Item
open Fpl3LanguageServer.ServiceAutoCompletion.Choices

/// <summary>
/// Completion-item choice provider for constructors.
/// </summary>
type FplCompletionItemChoicesConstructor() =
    inherit FplCompletionItemChoices()

    /// <summary>
    /// Builds the constructor snippet completion item based on the given default item.
    /// </summary>
    member private this.BuildConstructorSnippet(baseCi: FplCompletionItem) : FplCompletionItem =
        if baseCi.IsShort then
            this.TokenDeclaration <- LiteralDec

        let insert =
            $"{baseCi.Word} SomeFplClass(){Environment.NewLine}" +
            $"{FplCompletionItemChoices.TokenLeftBrace}{Environment.NewLine}" +
            $"\t{this.TokenDeclaration}{Environment.NewLine}" +
            $"\t\tbase.Obj (){Environment.NewLine}" +
            $"\t;{Environment.NewLine}" +
            $"{FplCompletionItemChoices.TokenRightBrace}{Environment.NewLine}" +
            $"{Environment.NewLine}"

        baseCi.WithLabel(baseCi.Label + " ...").WithInsertText(insert)

    override this.GetChoices(defaultCi: FplCompletionItem) : List<FplCompletionItem> =
        List<FplCompletionItem>(
            [ // snippets
              this.BuildConstructorSnippet(defaultCi)
              // keywords - preserve short-marker when base is short so final sort-text is correct
              defaultCi.WithIsShort(defaultCi.IsShort).WithKind(CompletionItemKind.Keyword).WithKeyword() ]
        )
