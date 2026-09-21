module Fpl3LanguageServer.ServiceAutoCompletion.Choices

open System.Collections.Generic
open Fpl0Base.Primitives
open Fpl3LanguageServer.ServiceAutoCompletion.Item

/// <summary>
/// Abstract base for FPL completion-item choice providers, exposing common token
/// constants (long/short forms) that concrete choice providers use to build their suggestions.
/// </summary>
[<AbstractClass>]
type FplCompletionItemChoices() =

    /// <summary>Prefix used to render completion item labels.</summary>
    static member val TokenPrefix = "_ "
    /// <summary>Left brace token text.</summary>
    static member val TokenLeftBrace = "{"
    /// <summary>Right brace token text.</summary>
    static member val TokenRightBrace = "}"

    member val TokenAssume = LiteralAssL with get, set
    member val TokenClass = LiteralClL with get, set
    member val TokenConclusion = LiteralConL with get, set
    member val TokenDeclaration = LiteralDecL with get, set
    member val TokenFunction = LiteralFuncL with get, set
    member val TokenIntrinsic = LiteralIntrL with get, set
    member val TokenObject = LiteralObjL with get, set
    member val TokenPredicate = LiteralPredL with get, set
    member val TokenPremise = LiteralPreL with get, set
    member val TokenRevoke = LiteralRevL with get, set

    /// <summary>
    /// Switches all long-form tokens to their short-form equivalents.
    /// </summary>
    member this.AdjustToShort() =
        this.TokenAssume <- LiteralAss
        this.TokenClass <- LiteralCl
        this.TokenConclusion <- LiteralCon
        this.TokenDeclaration <- LiteralDec
        this.TokenFunction <- LiteralFunc
        this.TokenIntrinsic <- LiteralIntr
        this.TokenObject <- LiteralObj
        this.TokenPredicate <- LiteralPred
        this.TokenPremise <- LiteralPre
        this.TokenRevoke <- LiteralRev

    /// <summary>
    /// Builds the list of concrete completion suggestions for the given default completion item.
    /// </summary>
    abstract member GetChoices : defaultCi: FplCompletionItem -> List<FplCompletionItem>
