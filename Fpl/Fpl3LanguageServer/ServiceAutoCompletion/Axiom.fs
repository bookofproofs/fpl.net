module Fpl3LanguageServer.ServiceAutoCompletion.Axiom

open System
open System.Collections.Generic
open Fpl0Base.Primitives
open Fpl3LanguageServer.ServiceAutoCompletion.Item
open Fpl3LanguageServer.ServiceAutoCompletion.Choices

/// <summary>
/// Completion-item choice provider for axioms and postulates.
/// </summary>
type FplCompletionItemChoicesAxiom() =
    inherit FplCompletionItemChoices()

    /// <summary>
    /// Builds the snippet body for the given word/label combination.
    /// </summary>
    static member GetBody(word: string, label: string) : string =
        if word = LiteralInf then
            $"{word} SomeFpl{label}{Environment.NewLine}" +
            "{" + Environment.NewLine +
            $"\tpre: true{Environment.NewLine}" +
            $"\tcon: true{Environment.NewLine}" +
            "}" + Environment.NewLine
        elif word = LiteralInfL then
            $"{word} SomeFpl{label}{Environment.NewLine}" +
            "{" + Environment.NewLine +
            $"\tpremise: true{Environment.NewLine}" +
            $"\tconclusion: true{Environment.NewLine}" +
            "}" + Environment.NewLine
        else
            $"{word} SomeFpl{label}{Environment.NewLine}" +
            "{" + Environment.NewLine +
            $"\ttrue{Environment.NewLine}" +
            "}" + Environment.NewLine

    override this.GetChoices(defaultCi: FplCompletionItem) : List<FplCompletionItem> =
        let ret = List<FplCompletionItem>()

        // snippets
        let ci =
            if defaultCi.Word.StartsWith(LiteralAx) then
                defaultCi
                    .WithInsertText(FplCompletionItemChoicesAxiom.GetBody(defaultCi.Word, "Axiom"))
                    .WithLabel(defaultCi.Label + " ...")
            else
                defaultCi
                    .WithInsertText(FplCompletionItemChoicesAxiom.GetBody(defaultCi.Word, "Postulate"))
                    .WithLabel(defaultCi.Label + " ...")
        ret.Add(ci)

        // keywords
        let keyword = defaultCi.WithKeyword()
        ret.Add(keyword)

        ret
