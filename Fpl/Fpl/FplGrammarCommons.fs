module FplGrammarCommons

open FParsec
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
           "spec"
           "specification"
           "thm"
           "theorem"
           "trivial"
           "true"
           "undef"
           "undefined"
           "uses"
           "xor" |]
    )

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


let errRecPattern = "(definition|def|mandatory|mand|optional|opt|axiom|ax|postulate|post|theorem|thm|proposition|prop|lemma|lem|corollary|cor|conjecture|conj|declaration|dec|constructor|ctor|proof|prf|inference|inf|localization|loc|uses|and|or|impl|iif|xor|not|all|exn|ex|is|assert|cases|self\!|for|delegate|del)\W"