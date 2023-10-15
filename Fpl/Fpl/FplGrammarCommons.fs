module FplGrammarCommons

open FParsec
open System.Collections.Generic
open System.Text.RegularExpressions

(* This module contains information needed by both, the error recovery module and the parser *)

(* A symbol that does not occur in the syntax of FPL to be used as error recovery code *)
let invalidSymbol = "§"

// labels that will be filtered out from parser choices during error recovery
let labelWhitespace = "<whitespace>"
let labelInlineComment = "<inline comment>"
let labelBlockComment = "<block comment>"

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
           "false"
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
           "spec"
           "specification"
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
    ("'->'", "->")
    ("',', ':', ':*', ':+'", ":")
    ("',', '}'", "}")
    ("',', '>'", ">")
    ("',', 'alias', '}'", "}")
    ("';', <significant whitespace>, <variable>", ";")
    ("':', ':*', ':+'", ":")
    ("':'", ":")
    ("':='", ":=")
    ("':end'", ":end")
    ("':ext', 'inf', 'inference', 'th', 'theory', 'uses', <significant whitespace>", ":ext")
    ("'(', ';', '<', <(closed) left bound '['>, <(open) left bound '[!'>, <PascalCaseId>, <significant whitespace>, <variable>", ";")
    ("'(', ';', '<', <(closed) left bound '['>, <(open) left bound '[!'>, <significant whitespace>, <variable>", ";")
    ("'(', ';', '|', '~', <\"language-specific string\">, <significant whitespace>, <variable>", ";")
    ("'(', ';', <significant whitespace>, <variable>", ";")
    ("'(', '.', ';', '<', <(closed) left bound '['>, <(open) left bound '[!'>, <significant whitespace>, <variable>", ";")
    ("'(', '.', '<', 'con', 'conclusion', <(closed) left bound '['>, <(open) left bound '[!'>, <significant whitespace>", "con")
    ("'(', ')', ',', '.', '<', <(closed) left bound '['>, <(open) left bound '[!'>", ",")
    ("'(', ')', ',', '<', <(closed) left bound '['>, <(open) left bound '[!'>, <PascalCaseId>", ",")
    ("'(', ')', ',', '<', <(closed) left bound '['>, <(open) left bound '[!'>", ",")
    ("'(', ')', ','", ",")
    ("'(', '<', '{', <(closed) left bound '['>, <(open) left bound '[!'>", "{")
    ("'(', <\"language-specific string\">, <variable>", "\"\\operatorname{true}\"")
    ("'('", "(")
    ("')', <variable>", ")")
    ("'{', <significant whitespace>", "{")
    ("'{'", "{")
    ("'}', <PascalCaseId>, <significant whitespace>", "}")
    ("'}', <significant whitespace>", "}")
    ("'@', 'all', 'and', 'dec', 'declaration', 'ex', 'false', 'iif', 'impl', 'intr', 'intrinsic', 'is', 'not', 'or', 'self', 'spec', 'specification', 'true', 'undef', 'undefined', 'xor', <PascalCaseId>, <argument identifier>, <digits>, <significant whitespace>, <variable>", "true")
    ("'@', 'all', 'and', 'dec', 'declaration', 'ex', 'false', 'iif', 'impl', 'is', 'not', 'or', 'self', 'spec', 'specification', 'true', 'undef', 'undefined', 'xor', <PascalCaseId>, <argument identifier>, <digits>, <significant whitespace>, <variable>", "true")
    ("'@', 'all', 'and', 'del', 'delegate', 'ex', 'false', 'iif', 'impl', 'is', 'not', 'or', 'self', 'true', 'undef', 'undefined', 'xor', <PascalCaseId>, <argument identifier>, <digits>, <variable>", "true")
    ("'@', 'all', 'and', 'ex', 'false', 'iif', 'impl', 'is', 'not', 'or', 'self', 'true', 'undef', 'undefined', 'xor', '}', <PascalCaseId>, <argument identifier>, <digits>, <significant whitespace>, <variable>", "}")
    ("'@', 'all', 'and', 'ex', 'false', 'iif', 'impl', 'is', 'not', 'or', 'self', 'true', 'undef', 'undefined', 'xor', <PascalCaseId>, <argument identifier>, <digits>, <significant whitespace>, <variable>", "true")
    ("'@', 'all', 'and', 'ex', 'false', 'iif', 'impl', 'is', 'not', 'or', 'self', 'true', 'undef', 'undefined', 'xor', <PascalCaseId>, <argument identifier>, <digits>, <variable>", "true")
    ("'@', 'func', 'function', 'ind', 'index', 'obj', 'object', 'pred', 'predicate', 'template', 'tpl', <PascalCaseId>", "obj")
    ("'@', 'obj', 'object', 'template', 'tpl', <PascalCaseId>", "obj")
    ("'@', 'self', '~', <PascalCaseId>, <digits>, <variable>", "~")
    ("'@', 'self', <(closed) right bound ']'>, <(open) right bound '!]'>, <PascalCaseId>, <digits>, <variable>", "x")
    ("'@', 'self', <PascalCaseId>, <digits>, <variable>", "x")
    ("'@', 'self'", "self")
    ("'<', '{', <(closed) left bound '['>, <(open) left bound '[!'>", "{")
    ("'~', <significant whitespace>", "~")
    ("'$', '(', '.', '<', 'mand', 'mandatory', 'opt', 'optional', '}', <(closed) left bound '['>, <(open) left bound '[!'>, <significant whitespace>", "}")
    ("'ax', 'axiom', 'cl', 'class', 'conj', 'conjecture', 'cor', 'corollary', 'func', 'function', 'lem', 'lemma', 'post', 'postulate', 'pred', 'predicate', 'prf', 'proof', 'prop', 'proposition', 'theorem', 'thm', '}', <significant whitespace>", "pred")
    ("'con', 'conclusion', <significant whitespace>", "con")
    ("'dec', 'declaration', 'intr', 'intrinsic', 'mand', 'mandatory', 'opt', 'optional', 'spec', 'specification', '}', <PascalCaseId>, <significant whitespace>", "}")
    ("'dec', 'declaration', 'intr', 'intrinsic', 'ret', 'return', 'spec', 'specification', <significant whitespace>", "intr")
    ("'dec', 'declaration', 'intr', 'intrinsic', 'spec', 'specification', <significant whitespace>", "intr")
    ("'dec', 'declaration', 'pre', 'premise', 'spec', 'specification', <significant whitespace>", "pre")
    ("'inf', 'inference', 'th', 'theory', 'uses', <significant whitespace>", "uses")
    ("'loc', 'localization', '}', <significant whitespace>", "loc")
    ("'mand', 'mandatory', 'opt', 'optional', '}', <PascalCaseId>, <significant whitespace>", "}")
    ("'mand', 'mandatory', 'opt', 'optional', '}', <significant whitespace>", "}")
    ("'th', 'theory', <significant whitespace>", "th")
    ("<(closed) right bound ']'>, <(open) right bound '!]'>", "]")
    ("<extension regex>", "/d/")
    ("<ISO 639 language code>", "tex")
    ("<PascalCaseId>, <significant whitespace>", "T")
    ("<PascalCaseId>", "T")
    ("<variable (got keyword)>", invalidSymbol)
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
    let optTrailingWs1 =
        preWithOptTrailingWS1.Substring(pre1.Length, preWithOptTrailingWS1.Length - pre1.Length)
    
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
        (input, text, int64 0, int64 0, true)
    else
        // text with a trailing whitespace

        let (pre, optTrailingWs, post) = splitStringByTextAtPosition input text pos
    
        let textWithWS = text + " "
        if pos.Index = input.Length then
            let newInput = input + textWithWS
            let newRecText = lastRecoveryText + textWithWS
            (newInput, newRecText, int64 (newInput.Length - input.Length), int64 0, false)
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

            let lengthKeyword = int64 (lengthOfStartingFplKeyword post)
            if text = invalidSymbol || pre.EndsWith(',') || lengthKeyword>0 || post.StartsWith("//") || post.StartsWith("/*") then
                // insert text with a trailing whitespace
                let newInput = 
                    if pre.EndsWith(".") then
                        pre + corrTextWithWS + optTrailingWs + post
                    else
                        pre + " " + corrTextWithWS + optTrailingWs + post
                (newInput, lastRecoveryText + corrTextWithWS, int64 (newInput.Length - input.Length), lengthKeyword, fatalErrorOccured)
            elif postStartsWithParenthesis || text = "{" then
                // insert text with a trailing whitespace
                let newInput = 
                    if pre.EndsWith(".") then
                        pre + corrTextWithWS + optTrailingWs + post
                    else
                        pre + " " + corrTextWithWS + optTrailingWs + post
                (newInput, lastRecoveryText + corrTextWithWS, int64 (newInput.Length - input.Length), lengthKeyword, fatalErrorOccured)
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
                    int64 (newInput.Length - input.Length), lengthKeyword, fatalErrorOccured)
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
                    int64 (newInput.Length - input.Length), lengthKeyword, fatalErrorOccured)

