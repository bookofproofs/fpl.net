﻿module FplGrammarCommons

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
           "dec"
           "declaration"
           "del"
           "delegate"
           "else"
           "end"
           "ext"
           "ex"
           "exn"
           "false"
           "for"
           "func"
           "function"
           "iif"
           "impl"
           "ind"
           "index"
           "intr"
           "intrinsic"
           "inf"
           "inference"
           "in"
           "is"
           "lem"
           "lemma"
           "loc"
           "localization"
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
           "ret"
           "return"
           "rev"
           "revoke"
           "self"
           "spec"
           "specification"
           "thm"
           "theorem"
           "th"
           "theory"
           "trivial"
           "true"
           "undef"
           "undefined"
           "uses"
           "xor" |]
    )

let recoveryMap = dict [
    ("'->', <whitespace>", "->")
    ("',', ':', ':*', ':+', <whitespace>", ":")
    ("',', '<', '{', <(closed) left bound '['>, <(open) left bound '[('>, <whitespace>", "{")
    ("',', '>', <whitespace>", ">")
    ("',', 'alias', 'inf', 'inference', 'th', 'theory', <block comment>, <inline comment>, <significant whitespace>, <whitespace>", "inf")
    ("',', 'inf', 'inference', 'th', 'theory', <block comment>, <inline comment>, <significant whitespace>", "inf")
    ("';', '@', 'assert', 'cases', 'del', 'delegate', 'for', 'self', <PascalCaseId>, <block comment>, <digits>, <inline comment>, <significant whitespace>, <variable>, <whitespace>", ";")
    ("':', ':*', ':+', <whitespace>", ":")
    ("':', <whitespace>", ":")
    ("':'", ":")
    ("':=', <whitespace>", ":=")
    ("':end'", ":end")
    ("':ext', 'inf', 'inference', 'th', 'theory', 'uses', <block comment>, <inline comment>, <significant whitespace>", ":ext")
    ("'!', '(', '.', '<', '}', <(closed) left bound '['>, <(open) left bound '[('>, <block comment>, <inline comment>, <significant whitespace>, <whitespace>", "}")
    ("'!', '(', '.', '<', 'mand', 'mandatory', 'opt', 'optional', '}', <(closed) left bound '['>, <(open) left bound '[('>, <block comment>, <inline comment>, <significant whitespace>, <whitespace>", "}")
    ("'!', '(', ')', ',', '.', '<', <(closed) left bound '['>, <(open) left bound '[('>, <whitespace>", ")")
    ("'!', '.', <whitespace>", "!")
    ("'!', '~', <whitespace>", "~")
    ("'(', ',', <whitespace>", "(")
    ("'(', ';', '<', <(closed) left bound '['>, <(open) left bound '[('>, <block comment>, <inline comment>, <significant whitespace>, <variable (got keyword)>, <whitespace>", ";")
    ("'(', ';', '<', <(closed) left bound '['>, <(open) left bound '[('>, <block comment>, <inline comment>, <significant whitespace>, <variable>, <whitespace>", ";")
    ("'(', ';', '<', <(closed) left bound '['>, <(open) left bound '[('>, <PascalCaseId>, <block comment>, <inline comment>, <significant whitespace>, <variable>, <whitespace>", ";")
    ("'(', ';', '|', '~', <\"language-specific string\">, <block comment>, <inline comment>, <significant whitespace>, <variable (got keyword)>, <whitespace>", ";")
    ("'(', ';', '|', '~', <\"language-specific string\">, <block comment>, <inline comment>, <significant whitespace>, <variable>, <whitespace>", ";")
    ("'(', ';', <block comment>, <inline comment>, <significant whitespace>, <variable>, <whitespace>", ";")
    ("'(', '.', ';', '<', <(closed) left bound '['>, <(open) left bound '[('>, <block comment>, <inline comment>, <significant whitespace>, <variable>, <whitespace>", ";")
    ("'(', '.', ':=', '<', <(closed) left bound '['>, <(open) left bound '[('>, <whitespace>", ":=")
    ("'(', '.', '<', 'con', 'conclusion', <(closed) left bound '['>, <(open) left bound '[('>, <block comment>, <inline comment>, <significant whitespace>, <whitespace>", "con")
    ("'(', '.', <whitespace>", "(")
    ("'(', ')', ',', '.', '<', <(closed) left bound '['>, <(open) left bound '[('>, <whitespace>", ",")
    ("'(', ')', ',', '<', <(closed) left bound '['>, <(open) left bound '[('>, <PascalCaseId>, <whitespace>", ",")
    ("'(', ')', ',', '<', <(closed) left bound '['>, <(open) left bound '[('>, <whitespace>", ",")
    ("'(', ')', ',', <whitespace>", ",")
    ("'(', '<', '{', <(closed) left bound '['>, <(open) left bound '[('>, <whitespace>", "{")
    ("'(', <\"language-specific string\">, <variable (got keyword)>, <whitespace>", "\"\\operatorname{true}\"")
    ("'(', <\"language-specific string\">, <variable>", "\"\\operatorname{true}\"")
    ("'(', <whitespace>", "(")
    ("')', ',', <whitespace>", ")")
    ("')', '<', '@', 'all', 'and', 'del', 'delegate', 'ex', 'exn', 'false', 'iif', 'impl', 'is', 'not', 'or', 'self', 'true', 'undef', 'undefined', 'xor', <PascalCaseId>, <argument identifier>, <digits>, <variable (got keyword)>, <whitespace>", "true")
    ("')', '<', '@', 'all', 'and', 'del', 'delegate', 'ex', 'exn', 'false', 'iif', 'impl', 'is', 'not', 'or', 'self', 'true', 'undef', 'undefined', 'xor', <PascalCaseId>, <argument identifier>, <digits>, <variable>, <whitespace>", ")")
    ("')', '|', <significant whitespace>, <whitespace>", ")")
    ("')', <variable (got keyword)>, <whitespace>", ")")
    ("')', <variable>, <whitespace>", ")")
    ("')', <whitespace>", ")")
    ("')'", ")")
    ("'{', <block comment>, <inline comment>, <significant whitespace>", "{")
    ("'{', <whitespace>", "{")
    ("'{'", "{")
    ("'}', <block comment>, <inline comment>, <significant whitespace>, <whitespace>", "}")
    ("'}', <block comment>, <inline comment>, <significant whitespace>", "}")
    ("'}', <PascalCaseId>, <block comment>, <inline comment>, <significant whitespace>, <whitespace>", "}")
    ("'@', 'func', 'function', 'ind', 'index', 'obj', 'object', 'pred', 'predicate', 'template', 'tpl', <PascalCaseId>, <whitespace>", "T")
    ("'@', 'func', 'function', 'ind', 'index', 'obj', 'object', 'pred', 'predicate', 'template', 'tpl', <PascalCaseId>", "T")
    ("'@', 'obj', 'object', 'template', 'tpl', <PascalCaseId>, <whitespace>", "T")
    ("'@', 'obj', 'object', 'template', 'tpl', <PascalCaseId>", "T")
    ("'@', 'self', '~', <PascalCaseId>, <digits>, <variable>", "~")
    ("'@', 'self', <(closed) right bound ']'>, <(open) right bound ')]'>, <PascalCaseId>, <digits>, <variable (got keyword)>", "]")
    ("'@', 'self', <(closed) right bound ']'>, <(open) right bound ')]'>, <PascalCaseId>, <digits>, <variable>", "x")
    ("'@', 'self', <PascalCaseId>, <digits>, <variable>", "x")
    ("'<', '@', 'all', 'and', 'dec', 'declaration', 'del', 'delegate', 'ex', 'exn', 'false', 'iif', 'impl', 'intr', 'intrinsic', 'is', 'not', 'or', 'self', 'spec', 'specification', 'true', 'undef', 'undefined', 'xor', <PascalCaseId>, <argument identifier>, <block comment>, <digits>, <inline comment>, <significant whitespace>, <variable>", "true")
    ("'<', '@', 'all', 'and', 'dec', 'declaration', 'del', 'delegate', 'ex', 'exn', 'false', 'iif', 'impl', 'is', 'not', 'or', 'self', 'spec', 'specification', 'true', 'undef', 'undefined', 'xor', <PascalCaseId>, <argument identifier>, <block comment>, <digits>, <inline comment>, <significant whitespace>, <variable (got keyword)>", "true")
    ("'<', '@', 'all', 'and', 'dec', 'declaration', 'del', 'delegate', 'ex', 'exn', 'false', 'iif', 'impl', 'is', 'not', 'or', 'self', 'spec', 'specification', 'true', 'undef', 'undefined', 'xor', <PascalCaseId>, <argument identifier>, <block comment>, <digits>, <inline comment>, <significant whitespace>, <variable>, <whitespace>", "true")
    ("'<', '@', 'all', 'and', 'dec', 'declaration', 'del', 'delegate', 'ex', 'exn', 'false', 'iif', 'impl', 'is', 'not', 'or', 'self', 'spec', 'specification', 'true', 'undef', 'undefined', 'xor', <PascalCaseId>, <argument identifier>, <block comment>, <digits>, <inline comment>, <significant whitespace>, <variable>", "true")
    ("'<', '@', 'all', 'and', 'del', 'delegate', 'ex', 'exn', 'false', 'iif', 'impl', 'is', 'not', 'or', 'self', 'true', 'undef', 'undefined', 'xor', '}', <PascalCaseId>, <argument identifier>, <block comment>, <digits>, <inline comment>, <significant whitespace>, <variable>, <whitespace>", "}")
    ("'<', '@', 'all', 'and', 'del', 'delegate', 'ex', 'exn', 'false', 'iif', 'impl', 'is', 'not', 'or', 'self', 'true', 'undef', 'undefined', 'xor', <PascalCaseId>, <argument identifier>, <block comment>, <digits>, <inline comment>, <significant whitespace>, <variable>", "true")
    ("'<', '@', 'all', 'and', 'del', 'delegate', 'ex', 'exn', 'false', 'iif', 'impl', 'is', 'not', 'or', 'self', 'true', 'undef', 'undefined', 'xor', <PascalCaseId>, <argument identifier>, <digits>, <variable (got keyword)>", "true")
    ("'<', '@', 'all', 'and', 'del', 'delegate', 'ex', 'exn', 'false', 'iif', 'impl', 'is', 'not', 'or', 'self', 'true', 'undef', 'undefined', 'xor', <PascalCaseId>, <argument identifier>, <digits>, <variable>", "true")
    ("'~', <block comment>, <inline comment>, <significant whitespace>", "~")
    ("'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, <inline comment>, <significant whitespace>, <whitespace>", "}")
    ("'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <block comment>, <inline comment>, <significant whitespace>", "}")
    ("'con', 'conclusion', <block comment>, <inline comment>, <significant whitespace>, <whitespace>", "con")
    ("'con', 'conclusion', <block comment>, <inline comment>, <significant whitespace>", "con")
    ("'dec', 'declaration', 'spec', 'specification', <argument identifier>, <block comment>, <inline comment>, <significant whitespace>", "dec")
    ("'dec', 'declaration', 'intr', 'intrinsic', 'ret', 'return', 'spec', 'specification', <block comment>, <inline comment>, <significant whitespace>", "intr")
    ("'dec', 'declaration', 'intr', 'intrinsic', 'spec', 'specification', <PascalCaseId>, <block comment>, <inline comment>, <significant whitespace>", "intr")
    ("'dec', 'declaration', 'pre', 'premise', 'spec', 'specification', <block comment>, <inline comment>, <significant whitespace>, <whitespace>", "pre")
    ("'dec', 'declaration', 'pre', 'premise', 'spec', 'specification', <block comment>, <inline comment>, <significant whitespace>", "pre")
    ("'dec', 'declaration', 'self!', 'spec', 'specification', <block comment>, <inline comment>, <significant whitespace>, <whitespace>", "spec")
    ("'dec', 'declaration', 'self!', 'spec', 'specification', <block comment>, <inline comment>, <significant whitespace>", "self!")
    ("'dec', 'declaration', 'spec', 'specification', <PascalCaseId>, <block comment>, <inline comment>, <significant whitespace>, <whitespace>", "T")
    ("'inf', 'inference', 'th', 'theory', 'uses', <block comment>, <inline comment>, <significant whitespace>", "uses")
    ("'loc', 'localization', '}', <block comment>, <inline comment>, <significant whitespace>", "loc")
    ("'mand', 'mandatory', 'opt', 'optional', '}', <block comment>, <inline comment>, <significant whitespace>, <whitespace>", "}")
    ("'mand', 'mandatory', 'opt', 'optional', '}', <block comment>, <inline comment>, <significant whitespace>", "}")
    ("'mand', 'mandatory', 'opt', 'optional', '}', <PascalCaseId>, <block comment>, <inline comment>, <significant whitespace>, <whitespace>", "}")
    ("'self!', 'self', <block comment>, <inline comment>, <significant whitespace>", "self")
    ("'th', 'theory', <block comment>, <inline comment>, <significant whitespace>", "th")
    ("<(closed) right bound ']'>, <(open) right bound ')]'>, <whitespace>", "]")
    ("<extension regex>", "/d/")
    ("<ISO 639 language code>", "tex")
    ("<PascalCaseId>, <block comment>, <inline comment>, <significant whitespace>", "T")
    ("<PascalCaseId>, <whitespace>", "T")
    ("<PascalCaseId>", "T")
    ("<variable>", "x")
]

/// Checks if the string `s` starts with one of the characters '(',')','{','}'
let startsWithParentheses (s: string) = Regex.IsMatch(s, @"^[\s\(\)\{\}]")

/// Returns 0 if the string `s` does not start with an FPL keyword followed by any whitespace character.
/// otherwise, it returns the length of the keyword.
let lengthOfStartingFplKeyword (s: string) =
    keyWordSet
    |> Seq.tryFind (fun element ->
        Regex.IsMatch(s, @"^" + Regex.Escape(element) + @"[\s\(\)\{\}\[\]]")
        || s.Equals(element))
    |> function
    | Some keyword -> keyword.Length
    | None -> 0

/// Returns 0 if the string `s` does not end with any whitespace character followed by an FPL keyword.
/// otherwise, it returns the length of the keyword.
let lengthOfEndingFplKeyword (s: string) =
    keyWordSet
    |> Seq.tryFind (fun element ->
        Regex.IsMatch(s, "[\s\(\)\{\}\[\]]" + Regex.Escape(element) + "$")
        || s.Equals(element))
    |> function
    | Some keyword -> keyword.Length
    | None -> 0

/// Checks if the string `s` ends with an FPL keyword proceeded by any whitespace character
/// If so, this keyword will be returned (`string -> string option`)
let endsWithFplKeyword (s: string) =
    keyWordSet
    |> Seq.tryFind (fun element -> Regex.IsMatch(s, @"(\s)" + Regex.Escape(element) + @"$") || s.Equals(element))

/// A low-level helper function checking if a string starts with a given regex 
/// and returning the length of the first match or 0:
let checkRegex (regexPattern: string) (input: string) =
    let m = System.Text.RegularExpressions.Regex.Match(input, regexPattern)
    if m.Success && m.Index = 0 then
        m.Length
    else
        0

/// A low-level helper function splitting an `input` string at a given Parsing position `pos`
/// depending on text to be later injected at that position
let splitStringByTextAtPosition (input:string) (text:string) (pos:Position) =
    let ind1 = int pos.Index
    let preWithOptTrailingWS1 = input.Substring(0, ind1)
    let pre1 = preWithOptTrailingWS1.TrimEnd()
    
    let (pre, intInd, preWithOptTrailingWS) =
        let keywordAtTheend = endsWithFplKeyword pre1

        match keywordAtTheend with
        | Some str ->
            if text = invalidSymbol then
                let preWithOptTrailingWS = input.Substring(0, ind1 - str.Length - 1)
                (preWithOptTrailingWS.TrimEnd(), ind1 - str.Length - 1, preWithOptTrailingWS)
            else
                (pre1, ind1, preWithOptTrailingWS1)
        | None -> (pre1, ind1, preWithOptTrailingWS1)

    let optTrailingWs =
        preWithOptTrailingWS.Substring(pre.Length, preWithOptTrailingWS.Length - pre.Length)

    let post = input.Substring(intInd, input.Length - intInd ).TrimStart()
    
    let lengthOfFollowingInlineComment = checkRegex "\/\/[^\n]*\s*" post
    if lengthOfFollowingInlineComment = 0 then 
        let lengthOfFollowingBlockComment = checkRegex "\/\*((?:.|\n)*?)\*\/\s*" post
        if lengthOfFollowingBlockComment = 0 then
            (pre, optTrailingWs, post)
        else
            let a = input.Substring(intInd, lengthOfFollowingBlockComment)
            let b = post.Substring(lengthOfFollowingBlockComment)
            (pre, optTrailingWs + a, b)
    else
        let a = input.Substring(intInd, lengthOfFollowingInlineComment)
        let b = post.Substring(lengthOfFollowingInlineComment)
        (pre, optTrailingWs + a, b)
    


let invalidSymbolWS = invalidSymbol + " "

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
    =
    if pos.Index < 0 || pos.Index > input.Length then
        // position out of range
        (input, text, int64 0, true)
    else
        // text with a trailing whitespace

        let (pre, optTrailingWs, post) = splitStringByTextAtPosition input text pos
    
        let textWithWS = text + " "
        if pos.Index = input.Length then
            let newInput = input + textWithWS
            let newRecText = lastRecoveryText + textWithWS
            (newInput, newRecText, int64 (newInput.Length - input.Length), false)
        else
            // avoid false positives by inserting to many opening and closing braces, parentheses, or brackets
            let corrTextWithWS, fatalErrorOccured =
                if lastRecoveryText.Length < 100 then 
                    if textWithWS.StartsWith("{") && post.StartsWith("}") then
                        textWithWS + "} ", false
                    elif textWithWS.StartsWith("(") && post.StartsWith(")") then
                        textWithWS + ") ", false
                    elif textWithWS.StartsWith("[") && post.StartsWith("]") then
                        textWithWS + "] ", false
                    else
                        textWithWS, false
                else
                    // avoid infinite loops when inserting closing brackets/parentheses/braces
                    // did not succeed because the lastRecoveryText would otherwise get too (or infinitely) long 
                    textWithWS, true

            let postStartsWithParenthesis = startsWithParentheses post

            let cond1 = text = invalidSymbol
            let cond1a = text = "~" 
            let cond1b = post.StartsWith("]")
            let cond2 = pre.EndsWith(',')
            let cond3 = post.StartsWith("//")
            let cond4 = post.StartsWith("/*")

            if cond1 || (cond1a && cond1b) || cond2 || cond3 || cond4 then
                // insert text with a trailing whitespace
                let newInput = 
                    if pre.EndsWith(".") then
                        pre + corrTextWithWS + optTrailingWs + post
                    else
                        pre + " " + corrTextWithWS + optTrailingWs + post
                (newInput, lastRecoveryText + corrTextWithWS, int64 (newInput.Length - input.Length), fatalErrorOccured)
            elif postStartsWithParenthesis || text = "{" then
                // insert text with a trailing whitespace
                let newInput = 
                    if pre.EndsWith(".") then
                        pre + corrTextWithWS + optTrailingWs + post
                    else
                        pre + " " + corrTextWithWS + optTrailingWs + post
                (newInput, lastRecoveryText + corrTextWithWS, int64 (newInput.Length - input.Length), fatalErrorOccured)
            elif Regex.IsMatch(post, @"^\w") then
                // if the beginning is a word, replace this word
                let newInput = 
                    if (text = "T" || text = "x") && Regex.IsMatch(post,@"^\w+\.") then
                        // if post begins after the replacement with a dot
                        // and the replacement is a PascalCaseId "T" or a variable "x" then
                        // replace it without trailing spaces
                        pre + optTrailingWs + Regex.Replace(post, @"^\w+", text)
                    else
                        // else, try to simply remove the extra word and any optional trailing commas
                        let postAfterRemovingWrongWord = Regex.Replace(post, @"^\w+[\S]*", "").TrimStart()
                        if postAfterRemovingWrongWord.StartsWith(text) then
                            // if now, post starts with what was expected, do not replace the word
                            // by text with trailing spaces
                            pre + optTrailingWs + postAfterRemovingWrongWord
                        else
                            // else replace the wrong word by by text with trailing spaces
                            pre + optTrailingWs + Regex.Replace(post, @"^\w+[\S]*", corrTextWithWS)

                let newRecText = 
                    if lastRecoveryText.EndsWith(invalidSymbolWS) then
                        lastRecoveryText.Replace(invalidSymbolWS, corrTextWithWS)
                    else
                        corrTextWithWS

                (newInput,
                    newRecText,
                    int64 (newInput.Length - input.Length), fatalErrorOccured)
            else
                // if the beginning starts with any other character
                let postAfterRemovingWrongChar = post.[1..].TrimStart()
                let newInput =
                    if postAfterRemovingWrongChar.StartsWith(text) then
                        // if now, post starts with what was expected, do not replace the word
                        // by text with trailing spaces
                        pre + optTrailingWs + postAfterRemovingWrongChar
                    else
                        pre + optTrailingWs + corrTextWithWS + post.[1..]

                let newRecText = 
                    if lastRecoveryText.EndsWith(invalidSymbolWS) then
                        lastRecoveryText.Replace(invalidSymbolWS, corrTextWithWS)
                    else
                        corrTextWithWS

                (newInput,
                    newRecText,
                    int64 (newInput.Length - input.Length), fatalErrorOccured)

