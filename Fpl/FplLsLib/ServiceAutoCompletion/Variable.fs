module FplLsLib.ServiceAutoCompletion.Variable

open System.Collections.Generic
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open Fpl0Base.Primitives
open FplLsLib.ServiceAutoCompletion.Item
open FplLsLib.ServiceAutoCompletion.Choices

/// <summary>
/// Completion-item choice provider for variable declarations.
/// </summary>
type FplCompletionItemChoicesVariable() =
    inherit FplCompletionItemChoices()

    override this.GetChoices(defaultCi: FplCompletionItem) : List<FplCompletionItem> =
        let ret = List<FplCompletionItem>()
        let insert = "someVar "
        let label = FplCompletionItemChoices.TokenPrefix + insert
        let ci =
            defaultCi
                .WithDetail(PrimVariableL)
                .WithSortText(PrimVariableL)
                .WithInsertText(insert)
                .WithLabel(label)
                .WithKind(CompletionItemKind.Variable)
        ret.Add(ci)
        ret
