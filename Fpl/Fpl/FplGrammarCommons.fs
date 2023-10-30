module FplGrammarCommons

open System
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
           "constructor"
           "cor"
           "corollary"
           "ctor"
           "dec"
           "declaration"
           "def"
           "definition"
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
           "thm"
           "theorem"
           "trivial"
           "true"
           "undef"
           "undefined"
           "uses"
           "xor" |]
    )

/// Replaces in the `input` all regex pattern matches by spaces while preserving the new lines
let replaceLinesWithSpaces (input: string) (pattern: string) =
    let regex = new Regex(pattern, RegexOptions.Multiline)
    let evaluator = MatchEvaluator(fun (m: Match) -> 
        m.Value.Split(Environment.NewLine)
        |> Array.map (fun line -> String.replicate line.Length " ")
        |> String.concat Environment.NewLine
    )
    regex.Replace(input, evaluator)

/// Replaces in the `input` all FPL comments by spaces while preserving the new lines
let removeFplComments (input:string) = 
    let r1 = replaceLinesWithSpaces input "\/\/[^\n]*" // replace inline comments
    replaceLinesWithSpaces r1 "\/\*((?:.|\n)*?)\*\/" // replace block comments

/// Replaces in the `input` all strings by spaces while preserving the new lines
let removeStrings (input:string) =
    let regex = new Regex("\"[^\"" + Environment.NewLine + "]*\"")
    let replacement = MatchEvaluator(fun m -> "\"" + String.replicate (m.Value.Length - 2) " " + "\"")
    regex.Replace(input, replacement)

