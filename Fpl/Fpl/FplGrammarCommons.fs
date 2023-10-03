module FplGrammarCommons

open FParsec
open System.Linq
open System.Collections.Generic
open System.Text.RegularExpressions

(* This module contains information needed by both, the error recovery module and the parser *)

(* Fpl Keywords *)
let keyWordSet =
    HashSet<_>(
        [| "alias"
           "all"
           "and"
           "assert"
           "ass"
           "assume"
           "ax"
           "axiom"
           "cases"
           "cl"
           "class"
           "conj"
           "conjecture"
           "con"
           "conclusion"
           "cor"
           "corollary"
           "del"
           "delegate"
           "else"
           "end"
           "ext"
           "ex"
           "false"
           "func"
           "function"
           "iif"
           "impl"
           "ind"
           "index"
           "inf"
           "inference"
           "is"
           "lem"
           "lemma"
           "loc"
           "localization"
           "loop"
           "mand"
           "mandatory"
           "not"
           "obj"
           "object"
           "opt"
           "optional"
           "or"
           "post"
           "postulate"
           "pred"
           "predicate"
           "pre"
           "premise"
           "prf"
           "proof"
           "prop"
           "proposition"
           "qed"
           "range"
           "ret"
           "return"
           "rev"
           "revoke"
           "self"
           "thm"
           "theorem"
           "th"
           "theory"
           "tpl"
           "template"
           "trivial"
           "true"
           "undef"
           "undefined"
           "uses"
           "xor" |]
    )

/// Checks if the string `s` starts with an FPL Keyword followed by any whitespace character.
let startsWithFplKeyword (s: string) =
    keyWordSet
    |> Seq.exists (fun element ->
        Regex.IsMatch(s, @"^" + Regex.Escape(element) + @"[\s\(\)\{\}]")
        || s.Equals(element))

/// Checks if the string `s` starts with one of the characters '(',')','{','}'
let startsWithParentheses (s: string) = Regex.IsMatch(s, @"^[\s\(\)\{\}]")

/// A low-level helper function for FPL error recovery that manipulates a string `input` at a given Parsing position 
/// `pos' by either replacing or inserting this position by the value of `text` with a trailing space after it.
/// (With the idea that we will retrieve the value of `text` from FPL parser's error message).
/// The function returns a tuple consisting of the resulting new `input` string that can now be re-parsed,
/// the lastRecoveryText
/// and an offset position by which all the remaining input string was shifted by the manipulation.
/// (With the idea that we will use this offset later to correct the positions of any new error diagnostics.
let manipulateString (input: string) (pos: Position) (lastRecoveryText: string) (text:string) (cumulativeIndexOffset:int64) =
    if pos.Index < 0 || pos.Index > input.Length then
        failwith "Position is out of range"
    if text.Contains('\n') then
        failwith "Cannot handle multi-line text"
    // text with a whitespace left and right
    let textWithWS =  text + " "
    let insertionOffset = int64 (textWithWS.Length)
    if pos.Index = input.Length then
        let newInput = input + textWithWS 
        let newRecText = textWithWS 
        let remainingInput = ""
        ( newInput, newRecText, remainingInput, cumulativeIndexOffset + insertionOffset )
    else
        let intInd = int pos.Index
        let preWithOptTrailingWS = input.Substring(0, intInd)
        let pre = preWithOptTrailingWS.TrimEnd()
        let optTrailingWs = preWithOptTrailingWS.Substring(pre.Length, preWithOptTrailingWS.Length-pre.Length)
        
        let post = input.Substring(intInd, input.Length - intInd)

        // new recovery Text depends on whether the input ended the lastRecText 
        // we also provide a correction of the index counting exactly one additional whitespace inserted in case
        // we have start a new recovery text.
        let (newRecText, corrIndex) = 
            if pre.EndsWith(lastRecoveryText.TrimEnd()) then
                (lastRecoveryText + textWithWS, int64 0)
            else
                (textWithWS, int64 1)

        if pre.EndsWith(',') || startsWithFplKeyword post || startsWithParentheses post then
            // insert text with a trailing whitespace
            let remainingInput = textWithWS + optTrailingWs + post
            let newInput = pre + " " + remainingInput
            ( newInput, newRecText, remainingInput.TrimStart(), cumulativeIndexOffset + insertionOffset - corrIndex )
        else
            // replace the beginning of post by text.
            if Regex.IsMatch(post, @"^\w") then
                // if the beginning is a word, replace this word
                let replacementOffset = int64 (Regex.Match(post, @"^\w+").Value.Length)
                let remainingInput = Regex.Replace(post, @"^\w+", textWithWS)
                let newInput = pre + optTrailingWs + remainingInput
                ( newInput, newRecText, remainingInput.TrimStart(), cumulativeIndexOffset + insertionOffset - replacementOffset - corrIndex )
            else
                // if the beginning starts with any other character
                let remainingInput = textWithWS + post.[1..]
                let newInput = pre + optTrailingWs + remainingInput
                ( newInput, newRecText, remainingInput.TrimStart(), cumulativeIndexOffset + insertionOffset - int64 1 - corrIndex )
