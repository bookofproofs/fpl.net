module FplGrammarCommons
open System.Collections.Generic
open System.Text.RegularExpressions

(* This module contains information needed by both, the error recovery module and the parser *)

(* Fpl Keywords *)
let keyWordSet =
    HashSet<_>(
        [|
        "alias"; 
        "all"; 
        "and"; 
        "assert"; 
        "ass"; "assume"; 
        "ax"; "axiom";
        "cases"; 
        "cl"; "class"; 
        "conj"; "conjecture"; 
        "con"; "conclusion"; 
        "cor"; "corollary";
        "del"; "delegate"
        "else";
        "end";
        "ext";
        "ex";
        "false";
        "func"; "function";
        "iif";
        "impl";
        "ind"; "index";
        "inf"; "inference";
        "is";
        "lem"; "lemma";
        "loc"; "localization";
        "loop";
        "mand"; "mandatory";
        "not";
        "obj"; "object";
        "opt"; "optional";
        "or";
        "post"; "postulate";
        "pred"; "predicate";
        "pre"; "premise";
        "prf"; "proof";
        "prop"; "proposition";
        "qed";
        "range";
        "ret"; "return";
        "rev"; "revoke";
        "self";
        "thm"; "theorem";
        "th"; "theory";
        "tpl"; "template";
        "trivial";
        "true";
        "undef"; "undefined";
        "uses";
        "xor";
        |]
    )

/// Checks if the string `s` starts with an FPL Keyword followed by any whitespace character. 
let startsWithFplKeyword (s: string) =
    keyWordSet
    |> Seq.exists (fun element ->
        Regex.IsMatch(s, @"^" + Regex.Escape(element) + @"[\s\(\{]") || s.Equals(element))

