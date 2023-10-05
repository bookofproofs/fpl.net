module FplGrammarCommons

open System
open FParsec
open System.Collections.Generic
open System.Text.RegularExpressions

(* This module contains information needed by both, the error recovery module and the parser *)

(* A symbol that does not occur in the syntax of FPL to be used as error recovery code *)
let invalidSymbol = "§"

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

let recoveryMap = dict [
    ("':ext', 'inf', 'inference', 'th', 'theory', 'uses', <block comment>, <inline comment>, <significant whitespace>", ":ext")
    ("':'", ":")
    ("<PascalCaseId>", "T")
    ("<extension regex>", "/d/")
    ("<block comment>, <fpl identifier>, <inline comment>, <significant whitespace>", "T")
    ("'}', <block comment>, <fpl identifier>, <inline comment>, <significant whitespace>", "}")
    ("'th' or 'theory', <block comment>, <inline comment>, <significant whitespace>", "th")
    ("'(', <whitespace>", "(")
    ("':end'", ":end")
    ("'{'", "{")
    ("'" + invalidSymbol + "'", invalidSymbol)
    ("')', <variable>, <whitespace>", ")")
    ("'@', 'assert', 'cases', 'loop', 'pre', 'premise', 'range', 'ret', 'return', 'self', <block comment>, <indexed variable>, <inline comment>, <significant whitespace>, <variable>", "pre")
    ("' ' or ':'", ":")
    ("'}', <block comment>, <inline comment>, <significant whitespace>, <whitespace>", "}")
    ("'con', 'conclusion', <block comment>, <inline comment>, <significant whitespace>, <whitespace>", "con")
    ("'@', 'all', 'and', 'del', 'delegate', 'ex', 'false', 'iif', 'impl', 'is', 'not', 'or', 'self', 'true', 'undef', 'undefined', 'xor', <argument identifier>, <digits>, <fpl identifier>, <indexed variable>, <variable>", "true")
    ("' ', <fpl identifier>, '@', 'obj', 'object', 'template' or 'tpl'", "obj")
    ("' ', <block or inline comment>, <argument identifier>, <digits>, <fpl identifier>, <indexed variable>, <variable>, '@', 'all', 'and', 'assert', 'cases', 'del', 'delegate', 'ex', 'false', 'iif', 'impl', 'is', 'loop', 'mand', 'mandatory', 'not', 'opt', 'optional', 'or', 'range', 'ret', 'return', 'self', 'true', 'undef', 'undefined', 'xor' or '}'", "true")
    ("' ', ' ', <block or inline comment>, 'mand', 'mandatory', 'opt', 'optional' or '}'", "}")
    ("' ', <(closed) left bound '['>, <(open) left bound '[!'>, '[' or '{'", "{")
    ("'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, <inline comment>, <significant whitespace>", "}")
    ("' ', <block or inline comment>, 'loc', 'localization' or '}'", "}")
]

/// Checks if the string `s` starts with an FPL Keyword followed by any whitespace character.
let startsWithFplKeyword (s: string) =
    keyWordSet
    |> Seq.exists (fun element ->
        Regex.IsMatch(s, @"^" + Regex.Escape(element) + @"[\s\(\)\{\}]")
        || s.Equals(element))

/// Checks if the string `s` ends with an FPL keyword proceeded by any whitespace character
/// If so, this keyword will be returned (`string -> string option`)
let endsWithFplKeyword (s: string) =
    keyWordSet
    |> Seq.tryFind (fun element -> Regex.IsMatch(s, @"(\s)" + Regex.Escape(element) + @"$") || s.Equals(element))

/// Checks if the string `s` starts with one of the characters '(',')','{','}'
let startsWithParentheses (s: string) = Regex.IsMatch(s, @"^[\s\(\)\{\}]")


/// A low-level helper funtion splitting an `input` string at a given Parsing position `pos`
/// depending on text to be later injected at that position
let splitStringByTextAtPosition (input:string) (text:string) (pos:Position) = 
    let intIndTry = int pos.Index
    let preWithOptTrailingWSTry = input.Substring(0, intIndTry)
    let preTry = preWithOptTrailingWSTry.TrimEnd()

    let (pre, intInd, preWithOptTrailingWS) =
        let keywordAtTheend = endsWithFplKeyword preTry

        match keywordAtTheend with
        | Some str ->
            if text = invalidSymbol then
                let preWithOptTrailingWS = input.Substring(0, intIndTry - str.Length - 1)
                (preTry.Substring(0, preTry.Length - str.Length).TrimEnd(), intIndTry - str.Length - 1, preWithOptTrailingWS)
            else
                (preTry, intIndTry, preWithOptTrailingWSTry)
        | None -> (preTry, intIndTry, preWithOptTrailingWSTry)

    let optTrailingWs =
        preWithOptTrailingWS.Substring(pre.Length, preWithOptTrailingWS.Length - pre.Length)

    let post = input.Substring(intInd, input.Length - intInd)
    (pre, optTrailingWs, post)

/// A low-level helper function for FPL error recovery that manipulates a string `input` at a given Parsing position
/// `pos' by either replacing or inserting this position by the value of `text` with a trailing space after it.
/// (With the idea that we will retrieve the value of `text` from FPL parser's error message).
/// The function returns a tuple consisting of the resulting new `input` string that can now be re-parsed,
/// the lastRecoveryText
/// and an offset position by which all the remaining input string was shifted by the manipulation.
/// (With the idea that we will use this offset later to correct the positions of any new error diagnostics.
let manipulateString
    (input: string)
    (text: string)
    (pos: Position)
    (lastRecoveryText: string)
    (cumulativeIndexOffset: int64)
    =
    if pos.Index < 0 || pos.Index > input.Length then
        failwith "Position is out of range"

    // text with a trailing whitespace
    let textWithWS = text + " "
    let insertionOffset = int64 (textWithWS.Length)

    let (pre, optTrailingWs, post) = splitStringByTextAtPosition input text pos
    
    if pos.Index = input.Length then
        let newInput = input + textWithWS
        let newRecText = textWithWS
        (newInput, newRecText, cumulativeIndexOffset + insertionOffset)
    else

        // avoid false positives by inserting to many opening and closing braces, parentheses, or brackets
        let corrTextWithWS =
            if textWithWS.StartsWith("{") && post.StartsWith("}") then
                textWithWS + "} "
            elif textWithWS.StartsWith("(") && post.StartsWith(")") then
                textWithWS + ") "
            elif textWithWS.StartsWith("[") && post.StartsWith("]") then
                textWithWS + "] "
            else
                textWithWS
        // new recovery Text depends on whether the input ended the lastRecText
        // we also provide a correction of the index counting exactly one additional whitespace inserted in case
        // we have started a new recovery text.
        let (newRecText, corrIndex) =
            if pre.EndsWith(lastRecoveryText.TrimEnd()) then
                (lastRecoveryText + corrTextWithWS, int64 0)
            else
                (corrTextWithWS, int64 1)

        if text = invalidSymbol || pre.EndsWith(',') || startsWithFplKeyword post || startsWithParentheses post || post.StartsWith("//") || post.StartsWith("/*") then
            // insert text with a trailing whitespace
            let newInput = pre + " " + corrTextWithWS + optTrailingWs + post
            (newInput, newRecText, cumulativeIndexOffset + insertionOffset - corrIndex)
        elif Regex.IsMatch(post, @"^\w") then
            // if the beginning is a word, replace this word
            let replacementOffset = int64 (Regex.Match(post, @"^\w+").Value.Length)
            let newInput = pre + optTrailingWs + Regex.Replace(post, @"^\w+", corrTextWithWS)

            (newInput,
                newRecText,
                cumulativeIndexOffset + insertionOffset - replacementOffset - corrIndex)
        else
            // if the beginning starts with any other character
            let newInput = pre + optTrailingWs + corrTextWithWS + post.[1..]

            (newInput,
                newRecText,
                cumulativeIndexOffset + insertionOffset - int64 1 - corrIndex)

/// A helper replacing the FParsec error string by a string that can be better displayed in the VSCode problem window
let replaceFParsecErrMsgForFplParser (input: string) =
    let lines = input.Split('\n')
    let firstLine = lines.[1]
    let caretLine = lines.[2]
    let restOfLines = lines.[3..]

    // Find the position of the caret
    let caretPosition = caretLine.IndexOf('^')

    // Extract the significant characters
    let significantCharacters =
        Regex.Match(firstLine.Substring(caretPosition), @"\S+").Value

    // Replace the significant characters with quoted version in the first line
    let quotedFirstLine = sprintf "'%s'" significantCharacters

    // Trim all lines from the third line onwards and join them back together with newline characters
    let newLines = restOfLines |> Array.map (fun line -> line.Trim())

    // Join the transformed first line and the rest of the lines with a newline character to form the final output
    quotedFirstLine + "\n" + String.Join("\n", newLines)
