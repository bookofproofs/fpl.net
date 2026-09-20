module FplLsLib.ServiceAutoCompletion.Default

open System.Collections.Generic
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open Fpl0Base.Primitives
open FplLsLib.ServiceAutoCompletion.Item
open FplLsLib.ServiceAutoCompletion.Choices

/// <summary>
/// Fallback completion-item choice provider used for punctuation and other
/// tokens without a more specific choice provider.
/// </summary>
type FplCompletionItemChoicesDefault() =
    inherit FplCompletionItemChoices()

    override _.GetChoices(defaultCi: FplCompletionItem) : List<FplCompletionItem> =
        let ret = List<FplCompletionItem>()
        let detail =
            match defaultCi.Word with
            | "?" -> "else case '?'"
            | "|" -> "new case '|'"
            | PrimDelegateEqual -> "equal sign '='"
            | ":=" -> "assignment sign ':='"
            | ":*" -> "zero or more '*'"
            | ":" -> "colon ':'"
            | "." -> "dot '.'"
            | "," -> "enumeration ','"
            | "|-" -> "follows logically '|-'"
            | "->" -> "map '->'"
            | "{" -> "opening '{'"
            | "}" -> "closing '}'"
            | "(" -> "opening '('"
            | ")" -> "closing '('"
            | "[" -> "opening '['"
            | "]" -> "closing ']'"
            | _ -> "unknown"

        let ci = defaultCi.WithDetail(detail).WithKind(CompletionItemKind.Text)
        ret.Add(ci)
        ret
