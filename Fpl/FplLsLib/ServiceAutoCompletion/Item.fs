module FplLsLib.ServiceAutoCompletion.Item

open System.Collections.Generic
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open Fpl.Primitives

/// <summary>
/// Strips surrounding single quotes ('...') or angle brackets (&lt;...&gt;) from a token, if present.
/// </summary>
let stripQuotesOrBrackets (str: string) : string =
    if (str.StartsWith('\'') && str.EndsWith('\'')) || (str.StartsWith('<') && str.EndsWith('>')) then
        str.[1 .. str.Length - 2]
    else
        str

/// <summary>
/// A completion item enriched with FPL-specific semantic metadata (detail text, sort order,
/// completion kind, and whether it represents a "short form" keyword).
/// </summary>
type FplCompletionItem(word: string, ?insertText: string) as this =
    inherit CompletionItem()

    let prefix = "_ "
    let insertTextArg = defaultArg insertText ""

    let normalizedWord = stripQuotesOrBrackets word

    do
        this.InsertText <- normalizedWord + " "
        this.Label <- prefix + normalizedWord

        match normalizedWord with
        | LiteralAlias ->
            this.Detail <- LiteralAlias
            this.SortText <- LiteralAlias
            this.Kind <- CompletionItemKind.Struct
            this.IsShort <- false
        | LiteralAll ->
            this.Detail <- "predicate (all quantifier)"
            this.SortText <- LiteralAll
            this.Kind <- CompletionItemKind.Operator
            this.IsShort <- false
        | LiteralAnd ->
            this.Detail <- "predicate (conjunction)"
            this.SortText <- LiteralAnd
            this.Kind <- CompletionItemKind.Operator
            this.IsShort <- false
        | LiteralAss ->
            this.Detail <- "argument (assume, short form)"
            this.SortText <- "assume02"
            this.Kind <- CompletionItemKind.Property
            this.IsShort <- true
        | LiteralAssL ->
            this.Detail <- "argument (assume)"
            this.SortText <- "assume01"
            this.Kind <- CompletionItemKind.Property
            this.IsShort <- false
        | LiteralAssert ->
            this.Detail <- "statement (assert)"
            this.SortText <- LiteralAssert
            this.Kind <- CompletionItemKind.Property
            this.IsShort <- false
        | LiteralAx ->
            this.Detail <- "axiom (short form)"
            this.SortText <- "axiom02"
            this.Kind <- CompletionItemKind.Class
            this.IsShort <- true
        | LiteralAxL ->
            this.Detail <- LiteralAxL
            this.SortText <- "axiom01"
            this.Kind <- CompletionItemKind.Class
            this.IsShort <- false
        | LiteralCases ->
            this.Detail <- "statement (cases)"
            this.SortText <- LiteralCases
            this.Kind <- CompletionItemKind.Property
            this.IsShort <- false
        | LiteralCl ->
            this.Detail <- "class (short form)"
            this.SortText <- "class02"
            this.Kind <- CompletionItemKind.TypeParameter
            this.IsShort <- true
        | LiteralClL ->
            this.Detail <- LiteralClL
            this.SortText <- "class01"
            this.Kind <- CompletionItemKind.TypeParameter
            this.IsShort <- false
        | LiteralCon ->
            this.Detail <- "conclusion (short form)"
            this.SortText <- "conclusion02"
            this.Kind <- CompletionItemKind.Struct
            this.IsShort <- true
        | LiteralConL ->
            this.Detail <- LiteralConL
            this.SortText <- "conclusion01"
            this.Kind <- CompletionItemKind.Struct
            this.IsShort <- false
        | LiteralCor ->
            this.Detail <- "corollary (short form)"
            this.SortText <- "corollary02"
            this.Kind <- CompletionItemKind.Class
            this.IsShort <- true
        | LiteralCorL ->
            this.Detail <- LiteralCorL
            this.SortText <- "corollary01"
            this.Kind <- CompletionItemKind.Class
            this.IsShort <- false
        | LiteralConj ->
            this.Detail <- "conjecture (short form)"
            this.SortText <- "conjecture02"
            this.Kind <- CompletionItemKind.Class
            this.IsShort <- true
        | LiteralConjL ->
            this.Detail <- LiteralConjL
            this.SortText <- "conjecture01"
            this.Kind <- CompletionItemKind.Class
            this.IsShort <- false
        | LiteralCtor ->
            this.Detail <- "constructor (short form)"
            this.SortText <- "constructor02"
            this.Kind <- CompletionItemKind.Constructor
            this.IsShort <- true
        | LiteralCtorL ->
            this.Detail <- LiteralCtorL
            this.SortText <- "constructor01"
            this.Kind <- CompletionItemKind.Constructor
            this.IsShort <- false
        | LiteralDec ->
            this.Detail <- "declaration (short form)"
            this.SortText <- "declaration02"
            this.Kind <- CompletionItemKind.Property
            this.IsShort <- true
        | LiteralDecL ->
            this.Detail <- LiteralDecL
            this.SortText <- "declaration01"
            this.Kind <- CompletionItemKind.Property
            this.IsShort <- false
        | LiteralDel ->
            this.Detail <- "delegate (short form)"
            this.SortText <- "delegate02"
            this.Kind <- CompletionItemKind.Event
            this.IsShort <- true
        | LiteralDelL ->
            this.Detail <- LiteralDelL
            this.SortText <- "delegate01"
            this.Kind <- CompletionItemKind.Event
            this.IsShort <- false
        | LiteralDef ->
            this.Detail <- "definition (short form)"
            this.SortText <- "definition02"
            this.Kind <- CompletionItemKind.Class
            this.IsShort <- true
        | LiteralDefL ->
            this.Detail <- LiteralDefL
            this.SortText <- "definition01"
            this.Kind <- CompletionItemKind.Class
            this.IsShort <- false
        | LiteralEx ->
            this.Detail <- "predicate (exists quantifier)"
            this.SortText <- LiteralEx
            this.Kind <- CompletionItemKind.Operator
            this.IsShort <- false
        | LiteralExN ->
            this.Detail <- "predicate (exists n-times quantifier)"
            this.SortText <- LiteralExN
            this.Kind <- CompletionItemKind.Operator
            this.IsShort <- false
        | "exn!" ->
            this.Detail <- "predicate (exists n-times quantifier)"
            this.SortText <- "exn!"
            this.Kind <- CompletionItemKind.Operator
            this.IsShort <- false
        | LiteralExt ->
            this.Detail <- "extension (short form)"
            this.SortText <- "extension02"
            this.Kind <- CompletionItemKind.Class
            this.IsShort <- true
        | LiteralExtL ->
            this.Detail <- LiteralExtL
            this.SortText <- "extension01"
            this.Kind <- CompletionItemKind.Class
            this.IsShort <- false
        | LiteralFalse ->
            this.Detail <- "predicate (false)"
            this.SortText <- LiteralFalse
            this.Kind <- CompletionItemKind.Constant
            this.IsShort <- false
        | LiteralFor ->
            this.Detail <- "statement (for loop)"
            this.SortText <- LiteralFor
            this.Kind <- CompletionItemKind.Property
            this.IsShort <- false
        | LiteralFunc ->
            this.Detail <- "type (functional term, short form)"
            this.SortText <- "function02"
            this.Kind <- CompletionItemKind.TypeParameter
            this.IsShort <- true
        | LiteralFuncL ->
            this.Detail <- "type (functional term)"
            this.SortText <- "function01"
            this.Kind <- CompletionItemKind.TypeParameter
            this.IsShort <- false
        | LiteralIif ->
            this.Detail <- "predicate (equivalence, <=>)"
            this.SortText <- LiteralIif
            this.Kind <- CompletionItemKind.Operator
            this.IsShort <- false
        | LiteralImpl ->
            this.Detail <- "predicate (implication, =>)"
            this.SortText <- LiteralImpl
            this.Kind <- CompletionItemKind.Operator
            this.IsShort <- false
        | LiteralIn ->
            this.Detail <- "clause (in type or in range)"
            this.SortText <- LiteralIn
            this.Kind <- CompletionItemKind.Property
            this.IsShort <- false
        | LiteralInd ->
            this.Detail <- "type (index, short form)"
            this.SortText <- "index02"
            this.Kind <- CompletionItemKind.TypeParameter
            this.IsShort <- true
        | LiteralIndL ->
            this.Detail <- "type (index)"
            this.SortText <- "index01"
            this.Kind <- CompletionItemKind.TypeParameter
            this.IsShort <- false
        | LiteralInf ->
            this.Detail <- "rule of inference (short form)"
            this.SortText <- "inference02"
            this.Kind <- CompletionItemKind.Class
            this.IsShort <- true
        | LiteralInfL ->
            this.Detail <- PrimRuleOfInference
            this.SortText <- "inference01"
            this.Kind <- CompletionItemKind.Class
            this.IsShort <- false
        | LiteralInfix ->
            this.Detail <- "infix operator"
            this.SortText <- LiteralInfix
            this.Kind <- CompletionItemKind.Operator
            this.IsShort <- false
        | LiteralIntr ->
            this.Detail <- "intrinsic (short form)"
            this.SortText <- "intrinsic02"
            this.Kind <- CompletionItemKind.Struct
            this.IsShort <- true
        | LiteralIntrL ->
            this.Detail <- LiteralIntrL
            this.SortText <- "intrinsic01"
            this.Kind <- CompletionItemKind.Struct
            this.IsShort <- false
        | LiteralIs ->
            this.Detail <- "predicate (is of type)"
            this.SortText <- LiteralIs
            this.Kind <- CompletionItemKind.Interface
            this.IsShort <- false
        | LiteralLem ->
            this.Detail <- "lemma (short form)"
            this.SortText <- "lemma02"
            this.Kind <- CompletionItemKind.Class
            this.IsShort <- true
        | LiteralLemL ->
            this.Detail <- LiteralLemL
            this.SortText <- "lemma01"
            this.Kind <- CompletionItemKind.Class
            this.IsShort <- false
        | LiteralLoc ->
            this.Detail <- "localization (short form)"
            this.SortText <- "localization02"
            this.Kind <- CompletionItemKind.Class
            this.IsShort <- true
        | LiteralLocL ->
            this.Detail <- LiteralLocL
            this.SortText <- "localization01"
            this.Kind <- CompletionItemKind.Class
            this.IsShort <- false
        | LiteralNot ->
            this.Detail <- "predicate (negation)"
            this.SortText <- LiteralNot
            this.Kind <- CompletionItemKind.Operator
            this.IsShort <- false
        | LiteralObj ->
            this.Detail <- "type (object, short form)"
            this.SortText <- "object02"
            this.Kind <- CompletionItemKind.TypeParameter
            this.IsShort <- true
        | LiteralObjL ->
            this.Detail <- "type (object)"
            this.SortText <- "object01"
            this.Kind <- CompletionItemKind.TypeParameter
            this.IsShort <- false
        | LiteralOr ->
            this.Detail <- "predicate (disjunction)"
            this.SortText <- LiteralOr
            this.Kind <- CompletionItemKind.Operator
            this.IsShort <- false
        | LiteralParent ->
            this.Detail <- "reference (to parent)"
            this.SortText <- LiteralParent
            this.Kind <- CompletionItemKind.Reference
            this.IsShort <- false
        | LiteralPost ->
            this.Detail <- "postulate (short form)"
            this.SortText <- "postulate02"
            this.Kind <- CompletionItemKind.Class
            this.IsShort <- true
        | LiteralPostL ->
            this.Detail <- LiteralPostL
            this.SortText <- "postulate01"
            this.Kind <- CompletionItemKind.Class
            this.IsShort <- false
        | LiteralPre ->
            this.Detail <- "premise (short form)"
            this.SortText <- "premise02"
            this.Kind <- CompletionItemKind.Struct
            this.IsShort <- true
        | LiteralPred ->
            this.Detail <- "type (predicate, short form)"
            this.SortText <- "predicate02"
            this.Kind <- CompletionItemKind.TypeParameter
            this.IsShort <- true
        | LiteralPredL ->
            this.Detail <- "type (predicate)"
            this.SortText <- "predicate01"
            this.Kind <- CompletionItemKind.TypeParameter
            this.IsShort <- false
        | LiteralPrefix ->
            this.Detail <- "prefix operator"
            this.SortText <- LiteralPrefix
            this.Kind <- CompletionItemKind.Operator
            this.IsShort <- false
        | LiteralPostFix ->
            this.Detail <- "postfix operator"
            this.SortText <- LiteralPostFix
            this.Kind <- CompletionItemKind.Operator
            this.IsShort <- false
        | LiteralPreL ->
            this.Detail <- LiteralPreL
            this.SortText <- "premise01"
            this.Kind <- CompletionItemKind.Struct
            this.IsShort <- false
        | LiteralProp ->
            this.Detail <- "proposition (short form)"
            this.SortText <- "proposition02"
            this.Kind <- CompletionItemKind.Class
            this.IsShort <- true
        | LiteralPropL ->
            this.Detail <- LiteralPropL
            this.SortText <- "proposition01"
            this.Kind <- CompletionItemKind.Class
            this.IsShort <- false
        | LiteralPrty ->
            this.Detail <- "property (short form)"
            this.SortText <- "property02"
            this.Kind <- CompletionItemKind.Value
            this.IsShort <- true
        | LiteralPrtyL ->
            this.Detail <- LiteralPrtyL
            this.SortText <- "property01"
            this.Kind <- CompletionItemKind.Value
            this.IsShort <- false
        | LiteralPrf ->
            this.Detail <- "proof (short form)"
            this.SortText <- "proof02"
            this.Kind <- CompletionItemKind.Class
            this.IsShort <- true
        | LiteralPrfL ->
            this.Detail <- LiteralPrfL
            this.SortText <- "proof01"
            this.Kind <- CompletionItemKind.Class
            this.IsShort <- false
        | LiteralQed ->
            this.Detail <- "(quod erat demonstrandum)"
            this.SortText <- LiteralQed
            this.Kind <- CompletionItemKind.Constant
            this.IsShort <- false
        | LiteralRet ->
            this.Detail <- "statement (return, short form)"
            this.SortText <- "return02"
            this.Kind <- CompletionItemKind.Property
            this.IsShort <- true
        | LiteralRetL ->
            this.Detail <- "statement (return)"
            this.SortText <- "return01"
            this.Kind <- CompletionItemKind.Property
            this.IsShort <- false
        | LiteralRev ->
            this.Detail <- "argument (revoke, short form)"
            this.SortText <- "revoke02"
            this.Kind <- CompletionItemKind.Property
            this.IsShort <- true
        | LiteralRevL ->
            this.Detail <- "argument (revoke)"
            this.SortText <- "revoke01"
            this.Kind <- CompletionItemKind.Property
            this.IsShort <- false
        | LiteralSelf ->
            this.Detail <- "reference (to self)"
            this.SortText <- LiteralSelf
            this.Kind <- CompletionItemKind.Reference
            this.IsShort <- false
        | LiteralSymbol ->
            this.Detail <- "object symbol"
            this.SortText <- LiteralSymbol
            this.Kind <- CompletionItemKind.TypeParameter
            this.IsShort <- false
        | LiteralThm ->
            this.Detail <- "theorem (short form)"
            this.SortText <- "theorem02"
            this.Kind <- CompletionItemKind.Class
            this.IsShort <- true
        | LiteralThmL ->
            this.Detail <- LiteralThmL
            this.SortText <- "theorem01"
            this.Kind <- CompletionItemKind.Class
            this.IsShort <- false
        | LiteralTrue ->
            this.Detail <- "predicate (true)"
            this.SortText <- LiteralTrue
            this.Kind <- CompletionItemKind.Constant
            this.IsShort <- false
        | LiteralTrivial ->
            this.Detail <- "argument (trivial)"
            this.SortText <- LiteralTrivial
            this.Kind <- CompletionItemKind.Constant
            this.IsShort <- false
        | LiteralUndef ->
            this.Detail <- "undefined (short form)"
            this.SortText <- "undefined02"
            this.Kind <- CompletionItemKind.Constant
            this.IsShort <- true
        | LiteralUndefL ->
            this.Detail <- LiteralUndefL
            this.SortText <- "undefined01"
            this.Kind <- CompletionItemKind.Constant
            this.IsShort <- false
        | LiteralUses ->
            this.Detail <- "clause (uses)"
            this.SortText <- LiteralUses
            this.Kind <- CompletionItemKind.Module
            this.IsShort <- false
        | LiteralXor ->
            this.Detail <- "predicate (exclusive or)"
            this.SortText <- LiteralXor
            this.Kind <- CompletionItemKind.Operator
            this.IsShort <- false
        | _ ->
            this.Detail <- normalizedWord
            this.SortText <- normalizedWord
            this.Kind <- CompletionItemKind.Text
            this.IsShort <- false

        // post-construction override: if caller supplied explicit insertText, use it
        if not (System.String.IsNullOrEmpty(insertTextArg)) then
            this.InsertText <- insertTextArg.Replace("<replace>", word)
        else
            // previous behavior: when insertText wasn't supplied the item was treated as a keyword
            // Only override Detail for items which remained the default Text kind (i.e. unrecognized keywords).
            // Do NOT mutate SortText here — keyword ordering is handled by explicit helpers (WithKeyword / AdjustToKeyword).
            if this.Kind = CompletionItemKind.Text then
                this.Detail <- $"keyword '{word}'"

    /// <summary>
    /// The normalized word (quotes/brackets stripped) this completion item represents.
    /// </summary>
    member val Word = normalizedWord with get, set

    /// <summary>
    /// Whether this item represents the "short form" of a keyword/construct.
    /// </summary>
    member val IsShort = false with get, set

    // Public immutable-style helpers ------------------------------------------------

    /// <summary>
    /// Creates a modified copy of this completion item, overriding only the specified fields.
    /// </summary>
    member private this.Copy
        (
            ?label: string,
            ?detail: string,
            ?insertText: string,
            ?sortText: string,
            ?kind: CompletionItemKind,
            ?isShort: bool
        ) : FplCompletionItem =
        let finalSort = defaultArg sortText this.SortText
        let finalIsShort = defaultArg isShort this.IsShort
        let finalInsertText = defaultArg insertText (defaultArg (Option.ofObj this.InsertText) "")

        let copy = FplCompletionItem(this.Word, finalInsertText)
        copy.AdditionalTextEdits <- this.AdditionalTextEdits
        copy.Command <- this.Command
        copy.CommitCharacters <- this.CommitCharacters
        copy.Detail <- defaultArg detail this.Detail
        copy.Documentation <- this.Documentation
        copy.FilterText <- this.FilterText
        copy.InsertText <- defaultArg insertText this.InsertText
        copy.Kind <- defaultArg kind this.Kind
        copy.Label <- defaultArg label this.Label
        copy.Preselect <- this.Preselect
        copy.SortText <- finalSort
        copy.TextEdit <- this.TextEdit
        copy.IsShort <- finalIsShort
        copy

    member this.WithSortText(sortText: string) = this.Copy(sortText = sortText)

    member this.WithKind(kind: CompletionItemKind) = this.Copy(kind = kind)

    member this.WithLabel(label: string) = this.Copy(label = label)

    member this.WithInsertText(insertText: string) = this.Copy(insertText = insertText)

    member this.WithDetail(detail: string) = this.Copy(detail = detail)

    member this.WithIsShort(isShort: bool) =
        let newSort = if isShort then "z" + this.SortText else this.SortText
        this.Copy(sortText = newSort, isShort = isShort)

    member this.WithKeyword() =
        let insert = if this.Label.Length >= 2 then this.Label.[2..] else this.Label
        let detail =
            if insert.Split(' ').Length > 1 then
                $"keywords '{insert}'"
            else
                $"keyword '{insert}'"
        let newSort = "zzz" + this.SortText
        this.Copy(insertText = insert, detail = detail, sortText = newSort, kind = CompletionItemKind.Keyword)

    member this.Duplicate() = this.Copy()

    member this.WithShortAdjusted() =
        if not this.IsShort then
            this.Copy(isShort = true, sortText = "z" + this.SortText)
        else
            this.Copy()

