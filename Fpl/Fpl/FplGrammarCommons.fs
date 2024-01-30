module FplGrammarCommons

open System
open System.Collections.Generic
open System.Text.RegularExpressions
open FParsec

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
           "base"
           "bydef"
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
           "infix"
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
           "not"
           "obj"
           "object"
           "opt"
           "optional"
           "or"
           "post"
           "postfix"
           "postulate"
           "pred"
           "predicate"
           "pre"
           "prefix"
           "premise"
           "prf"
           "proof"
           "prop"
           "proposition"
           "prty"
           "property"
           "qed"
           "ret"
           "return"
           "rev"
           "revoke"
           "self"
           "symbol"
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

/// Replaces in the `input` all FPL comments by spaces while preserving new lines
let removeFplComments (input:string) = 
    let r1 = replaceLinesWithSpaces input "\/\/[^\n]*" // replace inline comments
    replaceLinesWithSpaces r1 "\/\*((?:.|\n)*?)\*\/" // replace block comments

/// Special math symbols that can be used as infix, postfix, prefix operators, or other mathematical symbols;
/// based on source: https://www.classe.cornell.edu/~dms79/LectureNotes/formulae/list-of-math-symbols-extended.htm
/// Code 0 = symbol is excluded from FPL unused
/// Code > 0 = symbol is included in FPL, with significant bits
/// Code 8 = math object symbol
/// Code 4 = prefix
/// Code 2 = infix
/// Code 1 = postfix
/// (sums like 8 + 2 + 1 are allowed).
let mathSymbols = dict [
        ('a', 2) // Latin Letter a, U+0041
        ('b', 2) // Latin Letter b, U+0042
        ('c', 2) // Latin Letter c, U+0043
        ('d', 2) // Latin Letter d, U+0044
        ('e', 2) // Latin Letter e, U+0045
        ('f', 2) // Latin Letter f, U+0046
        ('g', 2) // Latin Letter g, U+0047
        ('h', 2) // Latin Letter h, U+0048
        ('i', 2) // Latin Letter i, U+0049
        ('j', 2) // Latin Letter j, U+0050
        ('k', 2) // Latin Letter k, U+0051
        ('l', 2) // Latin Letter l, U+004C
        ('m', 2) // Latin Letter m, U+004D
        ('n', 2) // Latin Letter n, U+004E
        ('o', 2) // Latin Letter o, U+004F
        ('p', 2) // Latin Letter p, U+0050
        ('q', 2) // Latin Letter q, U+0051
        ('r', 2) // Latin Letter r, U+0052
        ('s', 2) // Latin Letter s, U+0053
        ('t', 2) // Latin Letter t, U+0054
        ('u', 2) // Latin Letter u, U+0055
        ('v', 2) // Latin Letter v, U+0056
        ('w', 2) // Latin Letter w, U+0057
        ('x', 2) // Latin Letter x, U+0058
        ('y', 2) // Latin Letter y, U+0059
        ('z', 2) // Latin Letter z, U+005A
        ('0', 8) // Digit 0, U+0030
        ('1', 8) // Digit 1, U+0031
        ('2', 8) // Digit 2, U+0032
        ('3', 8) // Digit 3, U+0033
        ('4', 8) // Digit 4, U+0034
        ('5', 8) // Digit 5, U+0035
        ('6', 8) // Digit 6, U+0036
        ('7', 8) // Digit 7, U+0037
        ('8', 8) // Digit 8, U+0038
        ('9', 8) // Digit 9, U+0039
        ('%', 5) // Percent Sign, U+0025
        ('_', 2) // Low Line, U+005F
        ('-', 7) // Hyphen (Minus), U+002D
        ('*', 7) // Asterisk, U+002A
        ('/', 7) // Slash, U+002F
        ('\\', 2) // Backslash, U+005C
        ('@', 2) // At, U+0040
        ('&', 5) // Ampresand, U+0026
        (''', 5) // Apostrophe, U+0027
        ('!', 1) // Exclamation Mark, U+0021
        ('^', 2) // Circumflex, U+005E
        ('+', 7) // Plus Sign, U+002B
        ('<', 2) // Less-Than Sign, U+003C
        ('=', 2) // Equals Sign, U+003D
        ('>', 2) // Greater-Than Sign, U+003E
        ('|', 0) // Vertical Line, U+007C
        ('~', 7) // Tilde, U+007E
        ('¬', 0) // Not Sign, U+00AC
        ('±', 0) // Plus-Minus Sign, U+00B1
        ('×', 0) // Multiplication Sign, U+00D7
        ('÷', 0) // Division Sign, U+00F7
        ('϶', 0) // Greek Reversed Lunate Epsilon Symbol, U+03F6
        ('؆', 0) // Arabic-Indic Cube Root, U+0606
        ('؇', 0) // Arabic-Indic Fourth Root, U+0607
        ('؈', 0) // Arabic Ray, U+0608
        ('⁄', 0) // Fraction Slash, U+2044
        ('⁒', 0) // Commercial Minus Sign, U+2052
        ('⁺', 0) // Superscript Plus Sign, U+207A
        ('⁻', 0) // Superscript Minus, U+207B
        ('⁼', 0) // Superscript Equals Sign, U+207C
        ('₊', 0) // Subscript Plus Sign, U+208A
        ('₋', 0) // Subscript Minus, U+208B
        ('₌', 0) // Subscript Equals Sign, U+208C
        ('℘', 0) // Script Capital P, U+2118
        ('⅀', 0) // Double-Struck N-Ary Summation, U+2140
        ('⅁', 0) // Turned Sans-Serif Capital G, U+2141
        ('⅂', 0) // Turned Sans-Serif Capital L, U+2142
        ('⅃', 0) // Reversed Sans-Serif Capital L, U+2143
        ('⅄', 0) // Turned Sans-Serif Capital Y, U+2144
        ('⅋', 0) // Turned Ampersand, U+214B
        ('←', 0) // Leftwards Arrow, U+2190
        ('↑', 0) // Upwards Arrow, U+2191
        ('→', 0) // Rightwards Arrow, U+2192
        ('↓', 0) // Downwards Arrow, U+2193
        ('↔', 0) // Left Right Arrow, U+2194
        ('↚', 0) // Leftwards Arrow with Stroke, U+219A
        ('↛', 0) // Rightwards Arrow with Stroke, U+219B
        ('↠', 0) // Rightwards Two Headed Arrow, U+21A0
        ('↣', 0) // Rightwards Arrow with Tail, U+21A3
        ('↦', 0) // Rightwards Arrow from Bar, U+21A6
        ('↮', 0) // Left Right Arrow with Stroke, U+21AE
        ('⇎', 0) // Left Right Double Arrow with Stroke, U+21CE
        ('⇏', 0) // Rightwards Double Arrow with Stroke, U+21CF
        ('⇒', 0) // Rightwards Double Arrow, U+21D2
        ('⇔', 0) // Left Right Double Arrow, U+21D4
        ('⇴', 0) // Right Arrow with Small Circle, U+21F4
        ('⇵', 0) // Downwards Arrow Leftwards of Upwards Arrow, U+21F5
        ('⇶', 0) // Three Rightwards Arrows, U+21F6
        ('⇷', 0) // Leftwards Arrow with Vertical Stroke, U+21F7
        ('⇸', 0) // Rightwards Arrow with Vertical Stroke, U+21F8
        ('⇹', 0) // Left Right Arrow with Vertical Stroke, U+21F9
        ('⇺', 0) // Leftwards Arrow with Double Vertical Stroke, U+21FA
        ('⇻', 0) // Rightwards Arrow with Double Vertical Stroke, U+21FB
        ('⇼', 0) // Left Right Arrow with Double Vertical Stroke, U+21FC
        ('⇽', 0) // Leftwards Open-Headed Arrow, U+21FD
        ('⇾', 0) // Rightwards Open-Headed Arrow, U+21FE
        ('⇿', 0) // Left Right Open-Headed Arrow, U+21FF
        ('∀', 0) // For All, U+2200
        ('∁', 0) // Complement, U+2201
        ('∂', 0) // Partial Differential, U+2202
        ('∃', 0) // There Exists, U+2203
        ('∄', 0) // There Does Not Exist, U+2204
        ('∅', 0) // Empty Set, U+2205
        ('∆', 0) // Increment, U+2206
        ('∇', 0) // Nabla, U+2207
        ('∈', 2) // Element Of, U+2208
        ('∉', 0) // Not An Element Of, U+2209
        ('∊', 0) // Small Element Of, U+220A
        ('∋', 0) // Contains as Member, U+220B
        ('∌', 0) // Does Not Contain as Member, U+220C
        ('∍', 0) // Small Contains as Member, U+220D
        ('∎', 0) // End of Proof, U+220E
        ('∏', 0) // N-Ary Product, U+220F
        ('∐', 0) // N-Ary Coproduct, U+2210
        ('∑', 0) // N-Ary Summation, U+2211
        ('−', 0) // Minus Sign, U+2212
        ('∓', 0) // Minus-or-Plus Sign, U+2213
        ('∔', 0) // Dot Plus, U+2214
        ('∕', 0) // Division Slash, U+2215
        ('∖', 0) // Set Minus, U+2216
        ('∗', 0) // Asterisk Operator, U+2217
        ('∘', 2) // Ring Operator, U+2218
        ('∙', 2) // Bullet Operator, U+2219
        ('√', 0) // Square Root, U+221A
        ('∛', 0) // Cube Root, U+221B
        ('∜', 0) // Fourth Root, U+221C
        ('∝', 0) // Proportional To, U+221D
        ('∞', 0) // Infinity, U+221E
        ('∟', 0) // Right Angle, U+221F
        ('∠', 0) // Angle, U+2220
        ('∡', 0) // Measured Angle, U+2221
        ('∢', 0) // Spherical Angle, U+2222
        ('∣', 0) // Divides, U+2223
        ('∤', 0) // Does Not Divide, U+2224
        ('∥', 0) // Parallel To, U+2225
        ('∦', 0) // Not Parallel To, U+2226
        ('∧', 0) // Logical And, U+2227
        ('∨', 0) // Logical Or, U+2228
        ('∩', 0) // Intersection, U+2229
        ('∪', 0) // Union, U+222A
        ('∫', 0) // Integral, U+222B
        ('∬', 0) // Double Integral, U+222C
        ('∭', 0) // Triple Integral, U+222D
        ('∮', 0) // Contour Integral, U+222E
        ('∯', 0) // Surface Integral, U+222F
        ('∰', 0) // Volume Integral, U+2230
        ('∱', 0) // Clockwise Integral, U+2231
        ('∲', 0) // Clockwise Contour Integral, U+2232
        ('∳', 0) // Anticlockwise Contour Integral, U+2233
        ('∴', 0) // Therefore, U+2234
        ('∵', 0) // Because, U+2235
        ('∶', 0) // Ratio, U+2236
        ('∷', 0) // Proportion, U+2237
        ('∸', 0) // Dot Minus, U+2238
        ('∹', 0) // Excess, U+2239
        ('∺', 0) // Geometric Proportion, U+223A
        ('∻', 0) // Homothetic, U+223B
        ('∼', 0) // Tilde Operator, U+223C
        ('∽', 0) // Reversed Tilde, U+223D
        ('∾', 0) // Inverted Lazy S, U+223E
        ('∿', 0) // Sine Wave, U+223F
        ('≀', 0) // Wreath Product, U+2240
        ('≁', 0) // Not Tilde, U+2241
        ('≂', 0) // Minus Tilde, U+2242
        ('≃', 0) // Asymptotically Equal To, U+2243
        ('≄', 0) // Not Asymptotically Equal To, U+2244
        ('≅', 0) // Approximately Equal To, U+2245
        ('≆', 0) // Approximately But Not Actually Equal To, U+2246
        ('≇', 0) // Neither Approximately Nor Actually Equal To, U+2247
        ('≈', 0) // Almost Equal To, U+2248
        ('≉', 0) // Not Almost Equal To, U+2249
        ('≊', 0) // Almost Equal or Equal To, U+224A
        ('≋', 0) // Triple Tilde, U+224B
        ('≌', 0) // All Equal To, U+224C
        ('≍', 0) // Equivalent To, U+224D
        ('≎', 0) // Geometrically Equivalent To, U+224E
        ('≏', 0) // Difference Between, U+224F
        ('≐', 0) // Approaches the Limit, U+2250
        ('≑', 0) // Geometrically Equal To, U+2251
        ('≒', 0) // Approximately Equal to or the Image Of, U+2252
        ('≓', 0) // Image of or Approximately Equal To, U+2253
        ('≔', 0) // Colon Equals, U+2254
        ('≕', 0) // Equals Colon, U+2255
        ('≖', 0) // Ring In Equal To, U+2256
        ('≗', 0) // Ring Equal To, U+2257
        ('≘', 0) // Corresponds To, U+2258
        ('≙', 0) // Estimates, U+2259
        ('≚', 0) // Equiangular To, U+225A
        ('≛', 0) // Star Equals, U+225B
        ('≜', 0) // Delta Equal To, U+225C
        ('≝', 0) // Equal to By Definition, U+225D
        ('≞', 0) // Measured By, U+225E
        ('≟', 0) // Questioned Equal To, U+225F
        ('≠', 0) // Not Equal To, U+2260
        ('≡', 0) // Identical To, U+2261
        ('≢', 0) // Not Identical To, U+2262
        ('≣', 0) // Strictly Equivalent To, U+2263
        ('≤', 0) // Less-Than or Equal To, U+2264
        ('≥', 0) // Greater-Than or Equal To, U+2265
        ('≦', 0) // Less-Than Over Equal To, U+2266
        ('≧', 0) // Greater-Than Over Equal To, U+2267
        ('≨', 0) // Less-Than But Not Equal To, U+2268
        ('≩', 0) // Greater-Than But Not Equal To, U+2269
        ('≪', 0) // Much Less-Than, U+226A
        ('≫', 0) // Much Greater-Than, U+226B
        ('≬', 0) // Between, U+226C
        ('≭', 0) // Not Equivalent To, U+226D
        ('≮', 0) // Not Less-Than, U+226E
        ('≯', 0) // Not Greater-Than, U+226F
        ('≰', 0) // Neither Less-Than Nor Equal To, U+2270
        ('≱', 0) // Neither Greater-Than Nor Equal To, U+2271
        ('≲', 0) // Less-Than or Equivalent To, U+2272
        ('≳', 0) // Greater-Than or Equivalent To, U+2273
        ('≴', 0) // Neither Less-Than Nor Equivalent To, U+2274
        ('≵', 0) // Neither Greater-Than Nor Equivalent To, U+2275
        ('≶', 0) // Less-Than or Greater-Than, U+2276
        ('≷', 0) // Greater-Than or Less-Than, U+2277
        ('≸', 0) // Neither Less-Than Nor Greater-Than, U+2278
        ('≹', 0) // Neither Greater-Than Nor Less-Than, U+2279
        ('≺', 0) // Precedes, U+227A
        ('≻', 0) // Succeeds, U+227B
        ('≼', 0) // Precedes or Equal To, U+227C
        ('≽', 0) // Succeeds or Equal To, U+227D
        ('≾', 0) // Precedes or Equivalent To, U+227E
        ('≿', 0) // Succeeds or Equivalent To, U+227F
        ('⊀', 0) // Does Not Precede, U+2280
        ('⊁', 0) // Does Not Succeed, U+2281
        ('⊂', 0) // Subset Of, U+2282
        ('⊃', 0) // Superset Of, U+2283
        ('⊄', 0) // Not A Subset Of, U+2284
        ('⊅', 0) // Not A Superset Of, U+2285
        ('⊆', 0) // Subset of or Equal To, U+2286
        ('⊇', 0) // Superset of or Equal To, U+2287
        ('⊈', 0) // Neither A Subset of Nor Equal To, U+2288
        ('⊉', 0) // Neither A Superset of Nor Equal To, U+2289
        ('⊊', 0) // Subset of with Not Equal To, U+228A
        ('⊋', 0) // Superset of with Not Equal To, U+228B
        ('⊌', 0) // Multiset, U+228C
        ('⊍', 0) // Multiset Multiplication, U+228D
        ('⊎', 0) // Multiset Union, U+228E
        ('⊏', 0) // Square Image Of, U+228F
        ('⊐', 0) // Square Original Of, U+2290
        ('⊑', 0) // Square Image of or Equal To, U+2291
        ('⊒', 0) // Square Original of or Equal To, U+2292
        ('⊓', 0) // Square Cap, U+2293
        ('⊔', 0) // Square Cup, U+2294
        ('⊕', 0) // Circled Plus, U+2295
        ('⊖', 0) // Circled Minus, U+2296
        ('⊗', 0) // Circled Times, U+2297
        ('⊘', 0) // Circled Division Slash, U+2298
        ('⊙', 0) // Circled Dot Operator, U+2299
        ('⊚', 0) // Circled Ring Operator, U+229A
        ('⊛', 0) // Circled Asterisk Operator, U+229B
        ('⊜', 0) // Circled Equals, U+229C
        ('⊝', 0) // Circled Dash, U+229D
        ('⊞', 0) // Squared Plus, U+229E
        ('⊟', 0) // Squared Minus, U+229F
        ('⊠', 0) // Squared Times, U+22A0
        ('⊡', 0) // Squared Dot Operator, U+22A1
        ('⊢', 0) // Right Tack, U+22A2
        ('⊣', 0) // Left Tack, U+22A3
        ('⊤', 0) // Down Tack, U+22A4
        ('⊥', 0) // Up Tack, U+22A5
        ('⊦', 0) // Assertion, U+22A6
        ('⊧', 0) // Models, U+22A7
        ('⊨', 0) // 1, U+22A8
        ('⊩', 0) // Forces, U+22A9
        ('⊪', 0) // Triple Vertical Bar Right Turnstile, U+22AA
        ('⊫', 0) // Double Vertical Bar Double Right Turnstile, U+22AB
        ('⊬', 0) // Does Not Prove, U+22AC
        ('⊭', 0) // Not True, U+22AD
        ('⊮', 0) // Does Not Force, U+22AE
        ('⊯', 0) // Negated Double Vertical Bar Double Right Turnstile, U+22AF
        ('⊰', 0) // Precedes Under Relation, U+22B0
        ('⊱', 0) // Succeeds Under Relation, U+22B1
        ('⊲', 0) // Normal Subgroup Of, U+22B2
        ('⊳', 0) // Contains as Normal Subgroup, U+22B3
        ('⊴', 0) // Normal Subgroup of or Equal To, U+22B4
        ('⊵', 0) // Contains as Normal Subgroup or Equal To, U+22B5
        ('⊶', 0) // Original Of, U+22B6
        ('⊷', 0) // Image Of, U+22B7
        ('⊸', 0) // Multimap, U+22B8
        ('⊹', 0) // Hermitian Conjugate Matrix, U+22B9
        ('⊺', 0) // Intercalate, U+22BA
        ('⊻', 0) // Xor, U+22BB
        ('⊼', 0) // Nand, U+22BC
        ('⊽', 0) // Nor, U+22BD
        ('⊾', 0) // Right Angle with Arc, U+22BE
        ('⊿', 0) // Right Triangle, U+22BF
        ('⋀', 0) // N-Ary Logical And, U+22C0
        ('⋁', 0) // N-Ary Logical Or, U+22C1
        ('⋂', 0) // N-Ary Intersection, U+22C2
        ('⋃', 0) // N-Ary Union, U+22C3
        ('⋄', 0) // Diamond Operator, U+22C4
        ('⋅', 0) // Dot Operator, U+22C5
        ('⋆', 0) // Star Operator, U+22C6
        ('⋇', 0) // Division Times, U+22C7
        ('⋈', 0) // Bowtie, U+22C8
        ('⋉', 0) // Left Normal Factor Semidirect Product, U+22C9
        ('⋊', 0) // Right Normal Factor Semidirect Product, U+22CA
        ('⋋', 0) // Left Semidirect Product, U+22CB
        ('⋌', 0) // Right Semidirect Product, U+22CC
        ('⋍', 0) // Reversed Tilde Equals, U+22CD
        ('⋎', 0) // Curly Logical Or, U+22CE
        ('⋏', 0) // Curly Logical And, U+22CF
        ('⋐', 0) // Double Subset, U+22D0
        ('⋑', 0) // Double Superset, U+22D1
        ('⋒', 0) // Double Intersection, U+22D2
        ('⋓', 0) // Double Union, U+22D3
        ('⋔', 0) // Pitchfork, U+22D4
        ('⋕', 0) // Equal and Parallel To, U+22D5
        ('⋖', 0) // Less-Than with Dot, U+22D6
        ('⋗', 0) // Greater-Than with Dot, U+22D7
        ('⋘', 0) // Very Much Less-Than, U+22D8
        ('⋙', 0) // Very Much Greater-Than, U+22D9
        ('⋚', 0) // Less-Than Equal to or Greater-Than, U+22DA
        ('⋛', 0) // Greater-Than Equal to or Less-Than, U+22DB
        ('⋜', 0) // Equal to or Less-Than, U+22DC
        ('⋝', 0) // Equal to or Greater-Than, U+22DD
        ('⋞', 0) // Equal to or Precedes, U+22DE
        ('⋟', 0) // Equal to or Succeeds, U+22DF
        ('⋠', 0) // Does Not Precede or Equal, U+22E0
        ('⋡', 0) // Does Not Succeed or Equal, U+22E1
        ('⋢', 0) // Not Square Image of or Equal To, U+22E2
        ('⋣', 0) // Not Square Original of or Equal To, U+22E3
        ('⋤', 0) // Square Image of or Not Equal To, U+22E4
        ('⋥', 0) // Square Original of or Not Equal To, U+22E5
        ('⋦', 0) // Less-Than But Not Equivalent To, U+22E6
        ('⋧', 0) // Greater-Than But Not Equivalent To, U+22E7
        ('⋨', 0) // Precedes But Not Equivalent To, U+22E8
        ('⋩', 0) // Succeeds But Not Equivalent To, U+22E9
        ('⋪', 0) // Not Normal Subgroup Of, U+22EA
        ('⋫', 0) // Does Not Contain as Normal Subgroup, U+22EB
        ('⋬', 0) // Not Normal Subgroup of or Equal To, U+22EC
        ('⋭', 0) // Does Not Contain as Normal Subgroup or Equal, U+22ED
        ('⋮', 0) // Vertical Ellipsis, U+22EE
        ('⋯', 0) // Midline Horizontal Ellipsis, U+22EF
        ('⋰', 0) // Up Right Diagonal Ellipsis, U+22F0
        ('⋱', 0) // Down Right Diagonal Ellipsis, U+22F1
        ('⋲', 0) // Element of with Long Horizontal Stroke, U+22F2
        ('⋳', 0) // Element of with Vertical Bar at End of Horizontal Stroke, U+22F3
        ('⋴', 0) // Small Element of with Vertical Bar at End of Horizontal Stroke, U+22F4
        ('⋵', 0) // Element of with Dot Above, U+22F5
        ('⋶', 0) // Element of with Overbar, U+22F6
        ('⋷', 0) // Small Element of with Overbar, U+22F7
        ('⋸', 0) // Element of with Underbar, U+22F8
        ('⋹', 0) // Element of with Two Horizontal Strokes, U+22F9
        ('⋺', 0) // Contains with Long Horizontal Stroke, U+22FA
        ('⋻', 0) // Contains with Vertical Bar at End of Horizontal Stroke, U+22FB
        ('⋼', 0) // Small Contains with Vertical Bar at End of Horizontal Stroke, U+22FC
        ('⋽', 0) // Contains with Overbar, U+22FD
        ('⋾', 0) // Small Contains with Overbar, U+22FE
        ('⋿', 0) // Z Notation Bag Membership, U+22FF
        ('⌠', 0) // Top Half Integral, U+2320
        ('⌡', 0) // Bottom Half Integral, U+2321
        ('⍼', 0) // Right Angle with Downwards Zigzag Arrow, U+237C
        ('⎛', 0) // Left Parenthesis Upper Hook, U+239B
        ('⎜', 0) // Left Parenthesis Extension, U+239C
        ('⎝', 0) // Left Parenthesis Lower Hook, U+239D
        ('⎞', 0) // Right Parenthesis Upper Hook, U+239E
        ('⎟', 0) // Right Parenthesis Extension, U+239F
        ('⎠', 0) // Right Parenthesis Lower Hook, U+23A0
        ('⎡', 0) // Left Square Bracket Upper Corner, U+23A1
        ('⎢', 0) // Left Square Bracket Extension, U+23A2
        ('⎣', 0) // Left Square Bracket Lower Corner, U+23A3
        ('⎤', 0) // Right Square Bracket Upper Corner, U+23A4
        ('⎥', 0) // Right Square Bracket Extension, U+23A5
        ('⎦', 0) // Right Square Bracket Lower Corner, U+23A6
        ('⎧', 0) // Left Curly Bracket Upper Hook, U+23A7
        ('⎨', 0) // Left Curly Bracket Middle Piece, U+23A8
        ('⎩', 0) // Left Curly Bracket Lower Hook, U+23A9
        ('⎪', 0) // Curly Bracket Extension, U+23AA
        ('⎫', 0) // Right Curly Bracket Upper Hook, U+23AB
        ('⎬', 0) // Right Curly Bracket Middle Piece, U+23AC
        ('⎭', 0) // Right Curly Bracket Lower Hook, U+23AD
        ('⎮', 0) // Integral Extension, U+23AE
        ('⎯', 0) // Horizontal Line Extension, U+23AF
        ('⎰', 0) // Upper Left or Lower Right Curly Bracket Section, U+23B0
        ('⎱', 0) // Upper Right or Lower Left Curly Bracket Section, U+23B1
        ('⎲', 0) // Summation Top, U+23B2
        ('⎳', 0) // Summation Bottom, U+23B3
        ('⏜', 0) // Top Parenthesis, U+23DC
        ('⏝', 0) // Bottom Parenthesis, U+23DD
        ('⏞', 0) // Top Curly Bracket, U+23DE
        ('⏟', 0) // Bottom Curly Bracket, U+23DF
        ('⏠', 0) // Top Tortoise Shell Bracket, U+23E0
        ('⏡', 0) // Bottom Tortoise Shell Bracket, U+23E1
        ('▷', 0) // White Right-Pointing Triangle, U+25B7
        ('◁', 0) // White Left-Pointing Triangle, U+25C1
        ('◸', 0) // Upper Left Triangle, U+25F8
        ('◹', 0) // Upper Right Triangle, U+25F9
        ('◺', 0) // Lower Left Triangle, U+25FA
        ('◻', 0) // White Medium Square, U+25FB
        ('◼', 0) // Black Medium Square, U+25FC
        ('◽', 0) // White Medium Small Square, U+25FD
        ('◾', 0) // Black Medium Small Square, U+25FE
        ('◿', 0) // Lower Right Triangle, U+25FF
        ('♯', 0) // Music Sharp Sign, U+266F
        ('⟀', 0) // Three Dimensional Angle, U+27C0
        ('⟁', 0) // White Triangle Containing Small White Triangle, U+27C1
        ('⟂', 0) // Perpendicular, U+27C2
        ('⟃', 0) // Open Subset, U+27C3
        ('⟄', 0) // Open Superset, U+27C4
        ('⟇', 0) // or with Dot Inside, U+27C7
        ('⟈', 0) // Reverse Solidus Preceding Subset, U+27C8
        ('⟉', 0) // Superset Preceding Solidus, U+27C9
        ('⟊', 0) // Vertical Bar with Horizontal Stroke, U+27CA
        ('⟋', 0) // Mathematical Rising Diagonal, U+27CB
        ('⟌', 0) // Long Division, U+27CC
        ('⟍', 0) // Mathematical Falling Diagonal, U+27CD
        ('⟎', 0) // Squared Logical And, U+27CE
        ('⟏', 0) // Squared Logical Or, U+27CF
        ('⟐', 0) // White Diamond with Centred Dot, U+27D0
        ('⟑', 0) // and with Dot, U+27D1
        ('⟒', 0) // Element of Opening Upwards, U+27D2
        ('⟓', 0) // Lower Right Corner with Dot, U+27D3
        ('⟔', 0) // Upper Left Corner with Dot, U+27D4
        ('⟕', 0) // Left Outer Join, U+27D5
        ('⟖', 0) // Right Outer Join, U+27D6
        ('⟗', 0) // Full Outer Join, U+27D7
        ('⟘', 0) // Large Up Tack, U+27D8
        ('⟙', 0) // Large Down Tack, U+27D9
        ('⟚', 0) // Left and Right Double Turnstile, U+27DA
        ('⟛', 0) // Left and Right Tack, U+27DB
        ('⟜', 0) // Left Multimap, U+27DC
        ('⟝', 0) // Long Right Tack, U+27DD
        ('⟞', 0) // Long Left Tack, U+27DE
        ('⟟', 0) // Up Tack with Circle Above, U+27DF
        ('⟠', 0) // Lozenge Divided By Horizontal Rule, U+27E0
        ('⟡', 0) // White Concave-Sided Diamond, U+27E1
        ('⟢', 0) // White Concave-Sided Diamond with Leftwards Tick, U+27E2
        ('⟣', 0) // White Concave-Sided Diamond with Rightwards Tick, U+27E3
        ('⟤', 0) // White Square with Leftwards Tick, U+27E4
        ('⟥', 0) // White Square with Rightwards Tick, U+27E5
        ('⟰', 0) // Upwards Quadruple Arrow, U+27F0
        ('⟱', 0) // Downwards Quadruple Arrow, U+27F1
        ('⟲', 0) // Anticlockwise Gapped Circle Arrow, U+27F2
        ('⟳', 0) // Clockwise Gapped Circle Arrow, U+27F3
        ('⟴', 0) // Right Arrow with Circled Plus, U+27F4
        ('⟵', 0) // Long Leftwards Arrow, U+27F5
        ('⟶', 0) // Long Rightwards Arrow, U+27F6
        ('⟷', 0) // Long Left Right Arrow, U+27F7
        ('⟸', 0) // Long Leftwards Double Arrow, U+27F8
        ('⟹', 0) // Long Rightwards Double Arrow, U+27F9
        ('⟺', 0) // Long Left Right Double Arrow, U+27FA
        ('⟻', 0) // Long Leftwards Arrow from Bar, U+27FB
        ('⟼', 0) // Long Rightwards Arrow from Bar, U+27FC
        ('⟽', 0) // Long Leftwards Double Arrow from Bar, U+27FD
        ('⟾', 0) // Long Rightwards Double Arrow from Bar, U+27FE
        ('⟿', 0) // Long Rightwards Squiggle Arrow, U+27FF
        ('⤀', 0) // Rightwards Two-Headed Arrow with Vertical Stroke, U+2900
        ('⤁', 0) // Rightwards Two-Headed Arrow with Double Vertical Stroke, U+2901
        ('⤂', 0) // Leftwards Double Arrow with Vertical Stroke, U+2902
        ('⤃', 0) // Rightwards Double Arrow with Vertical Stroke, U+2903
        ('⤄', 0) // Left Right Double Arrow with Vertical Stroke, U+2904
        ('⤅', 0) // Rightwards Two-Headed Arrow from Bar, U+2905
        ('⤆', 0) // Leftwards Double Arrow from Bar, U+2906
        ('⤇', 0) // Rightwards Double Arrow from Bar, U+2907
        ('⤈', 0) // Downwards Arrow with Horizontal Stroke, U+2908
        ('⤉', 0) // Upwards Arrow with Horizontal Stroke, U+2909
        ('⤊', 0) // Upwards Triple Arrow, U+290A
        ('⤋', 0) // Downwards Triple Arrow, U+290B
        ('⤌', 0) // Leftwards Double Dash Arrow, U+290C
        ('⤍', 0) // Rightwards Double Dash Arrow, U+290D
        ('⤎', 0) // Leftwards Triple Dash Arrow, U+290E
        ('⤏', 0) // Rightwards Triple Dash Arrow, U+290F
        ('⤐', 0) // Rightwards Two-Headed Triple Dash Arrow, U+2910
        ('⤑', 0) // Rightwards Arrow with Dotted Stem, U+2911
        ('⤒', 0) // Upwards Arrow to Bar, U+2912
        ('⤓', 0) // Downwards Arrow to Bar, U+2913
        ('⤔', 0) // Rightwards Arrow with Tail with Vertical Stroke, U+2914
        ('⤕', 0) // Rightwards Arrow with Tail with Double Vertical Stroke, U+2915
        ('⤖', 0) // Rightwards Two-Headed Arrow with Tail, U+2916
        ('⤗', 0) // Rightwards Two-Headed Arrow with Tail with Vertical Stroke, U+2917
        ('⤘', 0) // Rightwards Two-Headed Arrow with Tail with Double Vertical Stroke, U+2918
        ('⤙', 0) // Leftwards Arrow-Tail, U+2919
        ('⤚', 0) // Rightwards Arrow-Tail, U+291A
        ('⤛', 0) // Leftwards Double Arrow-Tail, U+291B
        ('⤜', 0) // Rightwards Double Arrow-Tail, U+291C
        ('⤝', 0) // Leftwards Arrow to Black Diamond, U+291D
        ('⤞', 0) // Rightwards Arrow to Black Diamond, U+291E
        ('⤟', 0) // Leftwards Arrow from Bar to Black Diamond, U+291F
        ('⤠', 0) // Rightwards Arrow from Bar to Black Diamond, U+2920
        ('⤡', 0) // North West and South East Arrow, U+2921
        ('⤢', 0) // North East and South West Arrow, U+2922
        ('⤣', 0) // North West Arrow with Hook, U+2923
        ('⤤', 0) // North East Arrow with Hook, U+2924
        ('⤥', 0) // South East Arrow with Hook, U+2925
        ('⤦', 0) // South West Arrow with Hook, U+2926
        ('⤧', 0) // North West Arrow and North East Arrow, U+2927
        ('⤨', 0) // North East Arrow and South East Arrow, U+2928
        ('⤩', 0) // South East Arrow and South West Arrow, U+2929
        ('⤪', 0) // South West Arrow and North West Arrow, U+292A
        ('⤫', 0) // Rising Diagonal Crossing Falling Diagonal, U+292B
        ('⤬', 0) // Falling Diagonal Crossing Rising Diagonal, U+292C
        ('⤭', 0) // South East Arrow Crossing North East Arrow, U+292D
        ('⤮', 0) // North East Arrow Crossing South East Arrow, U+292E
        ('⤯', 0) // Falling Diagonal Crossing North East Arrow, U+292F
        ('⤰', 0) // Rising Diagonal Crossing South East Arrow, U+2930
        ('⤱', 0) // North East Arrow Crossing North West Arrow, U+2931
        ('⤲', 0) // North West Arrow Crossing North East Arrow, U+2932
        ('⤳', 0) // Wave Arrow Pointing Directly Right, U+2933
        ('⤴', 0) // Arrow Pointing Rightwards Then Curving Upwards, U+2934
        ('⤵', 0) // Arrow Pointing Rightwards Then Curving Downwards, U+2935
        ('⤶', 0) // Arrow Pointing Downwards Then Curving Leftwards, U+2936
        ('⤷', 0) // Arrow Pointing Downwards Then Curving Rightwards, U+2937
        ('⤸', 0) // Right-Side Arc Clockwise Arrow, U+2938
        ('⤹', 0) // Left-Side Arc Anticlockwise Arrow, U+2939
        ('⤺', 0) // Top Arc Anticlockwise Arrow, U+293A
        ('⤻', 0) // Bottom Arc Anticlockwise Arrow, U+293B
        ('⤼', 0) // Top Arc Clockwise Arrow with Minus, U+293C
        ('⤽', 0) // Top Arc Anticlockwise Arrow with Plus, U+293D
        ('⤾', 0) // Lower Right Semicircular Clockwise Arrow, U+293E
        ('⤿', 0) // Lower Left Semicircular Anticlockwise Arrow, U+293F
        ('⥀', 0) // Anticlockwise Closed Circle Arrow, U+2940
        ('⥁', 0) // Clockwise Closed Circle Arrow, U+2941
        ('⥂', 0) // Rightwards Arrow Above Short Leftwards Arrow, U+2942
        ('⥃', 0) // Leftwards Arrow Above Short Rightwards Arrow, U+2943
        ('⥄', 0) // Short Rightwards Arrow Above Leftwards Arrow, U+2944
        ('⥅', 0) // Rightwards Arrow with Plus Below, U+2945
        ('⥆', 0) // Leftwards Arrow with Plus Below, U+2946
        ('⥇', 0) // Rightwards Arrow Through X, U+2947
        ('⥈', 0) // Left Right Arrow Through Small Circle, U+2948
        ('⥉', 0) // Upwards Two-Headed Arrow from Small Circle, U+2949
        ('⥊', 0) // Left Barb Up Right Barb Down Harpoon, U+294A
        ('⥋', 0) // Left Barb Down Right Barb Up Harpoon, U+294B
        ('⥌', 0) // Up Barb Right Down Barb Left Harpoon, U+294C
        ('⥍', 0) // Up Barb Left Down Barb Right Harpoon, U+294D
        ('⥎', 0) // Left Barb Up Right Barb Up Harpoon, U+294E
        ('⥏', 0) // Up Barb Right Down Barb Right Harpoon, U+294F
        ('⥐', 0) // Left Barb Down Right Barb Down Harpoon, U+2950
        ('⥑', 0) // Up Barb Left Down Barb Left Harpoon, U+2951
        ('⥒', 0) // Leftwards Harpoon with Barb Up to Bar, U+2952
        ('⥓', 0) // Rightwards Harpoon with Barb Up to Bar, U+2953
        ('⥔', 0) // Upwards Harpoon with Barb Right to Bar, U+2954
        ('⥕', 0) // Downwards Harpoon with Barb Right to Bar, U+2955
        ('⥖', 0) // Leftwards Harpoon with Barb Down to Bar, U+2956
        ('⥗', 0) // Rightwards Harpoon with Barb Down to Bar, U+2957
        ('⥘', 0) // Upwards Harpoon with Barb Left to Bar, U+2958
        ('⥙', 0) // Downwards Harpoon with Barb Left to Bar, U+2959
        ('⥚', 0) // Leftwards Harpoon with Barb Up from Bar, U+295A
        ('⥛', 0) // Rightwards Harpoon with Barb Up from Bar, U+295B
        ('⥜', 0) // Upwards Harpoon with Barb Right from Bar, U+295C
        ('⥝', 0) // Downwards Harpoon with Barb Right from Bar, U+295D
        ('⥞', 0) // Leftwards Harpoon with Barb Down from Bar, U+295E
        ('⥟', 0) // Rightwards Harpoon with Barb Down from Bar, U+295F
        ('⥠', 0) // Upwards Harpoon with Barb Left from Bar, U+2960
        ('⥡', 0) // Downwards Harpoon with Barb Left from Bar, U+2961
        ('⥢', 0) // Leftwards Harpoon with Barb Up Above Leftwards Harpoon with Barb Down, U+2962
        ('⥣', 0) // Upwards Harpoon with Barb Left Beside Upwards Harpoon with Barb Right, U+2963
        ('⥤', 0) // Rightwards Harpoon with Barb Up Above Rightwards Harpoon with Barb Down, U+2964
        ('⥥', 0) // Downwards Harpoon with Barb Left Beside Downwards Harpoon with Barb Right, U+2965
        ('⥦', 0) // Leftwards Harpoon with Barb Up Above Rightwards Harpoon with Barb Up, U+2966
        ('⥧', 0) // Leftwards Harpoon with Barb Down Above Rightwards Harpoon with Barb Down, U+2967
        ('⥨', 0) // Rightwards Harpoon with Barb Up Above Leftwards Harpoon with Barb Up, U+2968
        ('⥩', 0) // Rightwards Harpoon with Barb Down Above Leftwards Harpoon with Barb Down, U+2969
        ('⥪', 0) // Leftwards Harpoon with Barb Up Above Long Dash, U+296A
        ('⥫', 0) // Leftwards Harpoon with Barb Down Below Long Dash, U+296B
        ('⥬', 0) // Rightwards Harpoon with Barb Up Above Long Dash, U+296C
        ('⥭', 0) // Rightwards Harpoon with Barb Down Below Long Dash, U+296D
        ('⥮', 0) // Upwards Harpoon with Barb Left Beside Downwards Harpoon with Barb Right, U+296E
        ('⥯', 0) // Downwards Harpoon with Barb Left Beside Upwards Harpoon with Barb Right, U+296F
        ('⥰', 0) // Right Double Arrow with Rounded Head, U+2970
        ('⥱', 0) // Equals Sign Above Rightwards Arrow, U+2971
        ('⥲', 0) // Tilde Operator Above Rightwards Arrow, U+2972
        ('⥳', 0) // Leftwards Arrow Above Tilde Operator, U+2973
        ('⥴', 0) // Rightwards Arrow Above Tilde Operator, U+2974
        ('⥵', 0) // Rightwards Arrow Above Almost Equal To, U+2975
        ('⥶', 0) // Less-Than Above Leftwards Arrow, U+2976
        ('⥷', 0) // Leftwards Arrow Through Less-Than, U+2977
        ('⥸', 0) // Greater-Than Above Rightwards Arrow, U+2978
        ('⥹', 0) // Subset Above Rightwards Arrow, U+2979
        ('⥺', 0) // Leftwards Arrow Through Subset, U+297A
        ('⥻', 0) // Superset Above Leftwards Arrow, U+297B
        ('⥼', 0) // Left Fish Tail, U+297C
        ('⥽', 0) // Right Fish Tail, U+297D
        ('⥾', 0) // Up Fish Tail, U+297E
        ('⥿', 0) // Down Fish Tail, U+297F
        ('⦀', 0) // Triple Vertical Bar Delimiter, U+2980
        ('⦁', 0) // Z Notation Spot, U+2981
        ('⦂', 0) // Z Notation Type Colon, U+2982
        ('⦙', 0) // Dotted Fence, U+2999
        ('⦚', 0) // Vertical Zigzag Line, U+299A
        ('⦛', 0) // Measured Angle Opening Left, U+299B
        ('⦜', 0) // Right Angle Variant with Square, U+299C
        ('⦝', 0) // Measured Right Angle with Dot, U+299D
        ('⦞', 0) // Angle with S Inside, U+299E
        ('⦟', 0) // Acute Angle, U+299F
        ('⦠', 0) // Spherical Angle Opening Left, U+29A0
        ('⦡', 0) // Spherical Angle Opening Up, U+29A1
        ('⦢', 0) // Turned Angle, U+29A2
        ('⦣', 0) // Reversed Angle, U+29A3
        ('⦤', 0) // Angle with Underbar, U+29A4
        ('⦥', 0) // Reversed Angle with Underbar, U+29A5
        ('⦦', 0) // Oblique Angle Opening Up, U+29A6
        ('⦧', 0) // Oblique Angle Opening Down, U+29A7
        ('⦨', 0) // Measured Angle with Open Arm Ending In Arrow Pointing Up and Right, U+29A8
        ('⦩', 0) // Measured Angle with Open Arm Ending In Arrow Pointing Up and Left, U+29A9
        ('⦪', 0) // Measured Angle with Open Arm Ending In Arrow Pointing Down and Right, U+29AA
        ('⦫', 0) // Measured Angle with Open Arm Ending In Arrow Pointing Down and Left, U+29AB
        ('⦬', 0) // Measured Angle with Open Arm Ending In Arrow Pointing Right and Up, U+29AC
        ('⦭', 0) // Measured Angle with Open Arm Ending In Arrow Pointing Left and Up, U+29AD
        ('⦮', 0) // Measured Angle with Open Arm Ending In Arrow Pointing Right and Down, U+29AE
        ('⦯', 0) // Measured Angle with Open Arm Ending In Arrow Pointing Left and Down, U+29AF
        ('⦰', 0) // Reversed Empty Set, U+29B0
        ('⦱', 0) // Empty Set with Overbar, U+29B1
        ('⦲', 0) // Empty Set with Small Circle Above, U+29B2
        ('⦳', 0) // Empty Set with Right Arrow Above, U+29B3
        ('⦴', 0) // Empty Set with Left Arrow Above, U+29B4
        ('⦵', 0) // Circle with Horizontal Bar, U+29B5
        ('⦶', 0) // Circled Vertical Bar, U+29B6
        ('⦷', 0) // Circled Parallel, U+29B7
        ('⦸', 0) // Circled Reverse Solidus, U+29B8
        ('⦹', 0) // Circled Perpendicular, U+29B9
        ('⦺', 0) // Circle Divided By Horizontal Bar and Top Half Divided By Vertical Bar, U+29BA
        ('⦻', 0) // Circle with Superimposed X, U+29BB
        ('⦼', 0) // Circled Anticlockwise-Rotated Division Sign, U+29BC
        ('⦽', 0) // Up Arrow Through Circle, U+29BD
        ('⦾', 0) // Circled White Bullet, U+29BE
        ('⦿', 0) // Circled Bullet, U+29BF
        ('⧀', 0) // Circled Less-Than, U+29C0
        ('⧁', 0) // Circled Greater-Than, U+29C1
        ('⧂', 0) // Circle with Small Circle to the Right, U+29C2
        ('⧃', 0) // Circle with Two Horizontal Strokes to the Right, U+29C3
        ('⧄', 0) // Squared Rising Diagonal Slash, U+29C4
        ('⧅', 0) // Squared Falling Diagonal Slash, U+29C5
        ('⧆', 0) // Squared Asterisk, U+29C6
        ('⧇', 0) // Squared Small Circle, U+29C7
        ('⧈', 0) // Squared Square, U+29C8
        ('⧉', 0) // Two Joined Squares, U+29C9
        ('⧊', 0) // Triangle with Dot Above, U+29CA
        ('⧋', 0) // Triangle with Underbar, U+29CB
        ('⧌', 0) // S In Triangle, U+29CC
        ('⧍', 0) // Triangle with Serifs at Bottom, U+29CD
        ('⧎', 0) // Right Triangle Above Left Triangle, U+29CE
        ('⧏', 0) // Left Triangle Beside Vertical Bar, U+29CF
        ('⧐', 0) // Vertical Bar Beside Right Triangle, U+29D0
        ('⧑', 0) // Bowtie with Left Half Black, U+29D1
        ('⧒', 0) // Bowtie with Right Half Black, U+29D2
        ('⧓', 0) // Black Bowtie, U+29D3
        ('⧔', 0) // Times with Left Half Black, U+29D4
        ('⧕', 0) // Times with Right Half Black, U+29D5
        ('⧖', 0) // White Hourglass, U+29D6
        ('⧗', 0) // Black Hourglass, U+29D7
        ('⧜', 0) // Incomplete Infinity, U+29DC
        ('⧝', 0) // Tie Over Infinity, U+29DD
        ('⧞', 0) // Infinity Negated with Vertical Bar, U+29DE
        ('⧟', 0) // Double-Ended Multimap, U+29DF
        ('⧠', 0) // Square with Contoured Outline, U+29E0
        ('⧡', 0) // Increases As, U+29E1
        ('⧢', 0) // Shuffle Product, U+29E2
        ('⧣', 0) // Equals Sign and Slanted Parallel, U+29E3
        ('⧤', 0) // Equals Sign and Slanted Parallel with Tilde Above, U+29E4
        ('⧥', 0) // Identical to and Slanted Parallel, U+29E5
        ('⧦', 0) // Gleich Stark, U+29E6
        ('⧧', 0) // Thermodynamic, U+29E7
        ('⧨', 0) // Down-Pointing Triangle with Left Half Black, U+29E8
        ('⧩', 0) // Down-Pointing Triangle with Right Half Black, U+29E9
        ('⧪', 0) // Black Diamond with Down Arrow, U+29EA
        ('⧫', 0) // Black Lozenge, U+29EB
        ('⧬', 0) // White Circle with Down Arrow, U+29EC
        ('⧭', 0) // Black Circle with Down Arrow, U+29ED
        ('⧮', 0) // Error-Barred White Square, U+29EE
        ('⧯', 0) // Error-Barred Black Square, U+29EF
        ('⧰', 0) // Error-Barred White Diamond, U+29F0
        ('⧱', 0) // Error-Barred Black Diamond, U+29F1
        ('⧲', 0) // Error-Barred White Circle, U+29F2
        ('⧳', 0) // Error-Barred Black Circle, U+29F3
        ('⧴', 0) // Rule-Delayed, U+29F4
        ('⧵', 0) // Reverse Solidus Operator, U+29F5
        ('⧶', 0) // Solidus with Overbar, U+29F6
        ('⧷', 0) // Reverse Solidus with Horizontal Stroke, U+29F7
        ('⧸', 0) // Big Solidus, U+29F8
        ('⧹', 0) // Big Reverse Solidus, U+29F9
        ('⧺', 0) // Double Plus, U+29FA
        ('⧻', 0) // Triple Plus, U+29FB
        ('⧾', 0) // Tiny, U+29FE
        ('⧿', 0) // Miny, U+29FF
        ('⨀', 0) // N-Ary Circled Dot Operator, U+2A00
        ('⨁', 0) // N-Ary Circled Plus Operator, U+2A01
        ('⨂', 0) // N-Ary Circled Times Operator, U+2A02
        ('⨃', 0) // N-Ary Union Operator with Dot, U+2A03
        ('⨄', 0) // N-Ary Union Operator with Plus, U+2A04
        ('⨅', 0) // N-Ary Square Intersection Operator, U+2A05
        ('⨆', 0) // N-Ary Square Union Operator, U+2A06
        ('⨇', 0) // Two Logical and Operator, U+2A07
        ('⨈', 0) // Two Logical or Operator, U+2A08
        ('⨉', 0) // N-Ary Times Operator, U+2A09
        ('⨊', 0) // Modulo Two Sum, U+2A0A
        ('⨋', 0) // Summation with Integral, U+2A0B
        ('⨌', 0) // Quadruple Integral Operator, U+2A0C
        ('⨍', 0) // Finite Part Integral, U+2A0D
        ('⨎', 0) // Integral with Double Stroke, U+2A0E
        ('⨏', 0) // Integral Average with Slash, U+2A0F
        ('⨐', 0) // Circulation Function, U+2A10
        ('⨑', 0) // Anticlockwise Integration, U+2A11
        ('⨒', 0) // Line Integration with Rectangular Path Around Pole, U+2A12
        ('⨓', 0) // Line Integration with Semicircular Path Around Pole, U+2A13
        ('⨔', 0) // Line Integration Not Including the Pole, U+2A14
        ('⨕', 0) // Integral Around A Point Operator, U+2A15
        ('⨖', 0) // Quaternion Integral Operator, U+2A16
        ('⨗', 0) // Integral with Leftwards Arrow with Hook, U+2A17
        ('⨘', 0) // Integral with Times Sign, U+2A18
        ('⨙', 0) // Integral with Intersection, U+2A19
        ('⨚', 0) // Integral with Union, U+2A1A
        ('⨛', 0) // Integral with Overbar, U+2A1B
        ('⨜', 0) // Integral with Underbar, U+2A1C
        ('⨝', 0) // Join, U+2A1D
        ('⨞', 0) // Large Left Triangle Operator, U+2A1E
        ('⨟', 0) // Z Notation Schema Composition, U+2A1F
        ('⨠', 0) // Z Notation Schema Piping, U+2A20
        ('⨡', 0) // Z Notation Schema Projection, U+2A21
        ('⨢', 0) // Plus Sign with Small Circle Above, U+2A22
        ('⨣', 0) // Plus Sign with Circumflex Accent Above, U+2A23
        ('⨤', 0) // Plus Sign with Tilde Above, U+2A24
        ('⨥', 0) // Plus Sign with Dot Below, U+2A25
        ('⨦', 0) // Plus Sign with Tilde Below, U+2A26
        ('⨧', 0) // Plus Sign with Subscript Two, U+2A27
        ('⨨', 0) // Plus Sign with Black Triangle, U+2A28
        ('⨩', 0) // Minus Sign with Comma Above, U+2A29
        ('⨪', 0) // Minus Sign with Dot Below, U+2A2A
        ('⨫', 0) // Minus Sign with Falling Dots, U+2A2B
        ('⨬', 0) // Minus Sign with Rising Dots, U+2A2C
        ('⨭', 0) // Plus Sign In Left Half Circle, U+2A2D
        ('⨮', 0) // Plus Sign In Right Half Circle, U+2A2E
        ('⨯', 0) // Vector or Cross Product, U+2A2F
        ('⨰', 0) // Multiplication Sign with Dot Above, U+2A30
        ('⨱', 0) // Multiplication Sign with Underbar, U+2A31
        ('⨲', 0) // Semidirect Product with Bottom Closed, U+2A32
        ('⨳', 0) // Smash Product, U+2A33
        ('⨴', 0) // Multiplication Sign In Left Half Circle, U+2A34
        ('⨵', 0) // Multiplication Sign In Right Half Circle, U+2A35
        ('⨶', 0) // Circled Multiplication Sign with Circumflex Accent, U+2A36
        ('⨷', 0) // Multiplication Sign In Double Circle, U+2A37
        ('⨸', 0) // Circled Division Sign, U+2A38
        ('⨹', 0) // Plus Sign In Triangle, U+2A39
        ('⨺', 0) // Minus Sign In Triangle, U+2A3A
        ('⨻', 0) // Multiplication Sign In Triangle, U+2A3B
        ('⨼', 0) // Interior Product, U+2A3C
        ('⨽', 0) // Righthand Interior Product, U+2A3D
        ('⨾', 0) // Z Notation Relational Composition, U+2A3E
        ('⨿', 0) // Amalgamation or Coproduct, U+2A3F
        ('⩀', 0) // Intersection with Dot, U+2A40
        ('⩁', 0) // Union with Minus Sign, U+2A41
        ('⩂', 0) // Union with Overbar, U+2A42
        ('⩃', 0) // Intersection with Overbar, U+2A43
        ('⩄', 0) // Intersection with Logical And, U+2A44
        ('⩅', 0) // Union with Logical Or, U+2A45
        ('⩆', 0) // Union Above Intersection, U+2A46
        ('⩇', 0) // Intersection Above Union, U+2A47
        ('⩈', 0) // Union Above Bar Above Intersection, U+2A48
        ('⩉', 0) // Intersection Above Bar Above Union, U+2A49
        ('⩊', 0) // Union Beside and Joined with Union, U+2A4A
        ('⩋', 0) // Intersection Beside and Joined with Intersection, U+2A4B
        ('⩌', 0) // Closed Union with Serifs, U+2A4C
        ('⩍', 0) // Closed Intersection with Serifs, U+2A4D
        ('⩎', 0) // Double Square Intersection, U+2A4E
        ('⩏', 0) // Double Square Union, U+2A4F
        ('⩐', 0) // Closed Union with Serifs and Smash Product, U+2A50
        ('⩑', 0) // Logical and with Dot Above, U+2A51
        ('⩒', 0) // Logical or with Dot Above, U+2A52
        ('⩓', 0) // Double Logical And, U+2A53
        ('⩔', 0) // Double Logical Or, U+2A54
        ('⩕', 0) // Two Intersecting Logical And, U+2A55
        ('⩖', 0) // Two Intersecting Logical Or, U+2A56
        ('⩗', 0) // Sloping Large Or, U+2A57
        ('⩘', 0) // Sloping Large And, U+2A58
        ('⩙', 0) // Logical or Overlapping Logical And, U+2A59
        ('⩚', 0) // Logical and with Middle Stem, U+2A5A
        ('⩛', 0) // Logical or with Middle Stem, U+2A5B
        ('⩜', 0) // Logical and with Horizontal Dash, U+2A5C
        ('⩝', 0) // Logical or with Horizontal Dash, U+2A5D
        ('⩞', 0) // Logical and with Double Overbar, U+2A5E
        ('⩟', 0) // Logical and with Underbar, U+2A5F
        ('⩠', 0) // Logical and with Double Underbar, U+2A60
        ('⩡', 0) // Small Vee with Underbar, U+2A61
        ('⩢', 0) // Logical or with Double Overbar, U+2A62
        ('⩣', 0) // Logical or with Double Underbar, U+2A63
        ('⩤', 0) // Z Notation Domain Antirestriction, U+2A64
        ('⩥', 0) // Z Notation Range Antirestriction, U+2A65
        ('⩦', 0) // Equals Sign with Dot Below, U+2A66
        ('⩧', 0) // Identical with Dot Above, U+2A67
        ('⩨', 0) // Triple Horizontal Bar with Double Vertical Stroke, U+2A68
        ('⩩', 0) // Triple Horizontal Bar with Triple Vertical Stroke, U+2A69
        ('⩪', 0) // Tilde Operator with Dot Above, U+2A6A
        ('⩫', 0) // Tilde Operator with Rising Dots, U+2A6B
        ('⩬', 0) // Similar Minus Similar, U+2A6C
        ('⩭', 0) // Congruent with Dot Above, U+2A6D
        ('⩮', 0) // Equals with Asterisk, U+2A6E
        ('⩯', 0) // Almost Equal to with Circumflex Accent, U+2A6F
        ('⩰', 0) // Approximately Equal or Equal To, U+2A70
        ('⩱', 0) // Equals Sign Above Plus Sign, U+2A71
        ('⩲', 0) // Plus Sign Above Equals Sign, U+2A72
        ('⩳', 0) // Equals Sign Above Tilde Operator, U+2A73
        ('⩴', 0) // Double Colon Equal, U+2A74
        ('⩵', 0) // Two Consecutive Equals Signs, U+2A75
        ('⩶', 0) // Three Consecutive Equals Signs, U+2A76
        ('⩷', 0) // Equals Sign with Two Dots Above and Two Dots Below, U+2A77
        ('⩸', 0) // Equivalent with Four Dots Above, U+2A78
        ('⩹', 0) // Less-Than with Circle Inside, U+2A79
        ('⩺', 0) // Greater-Than with Circle Inside, U+2A7A
        ('⩻', 0) // Less-Than with Question Mark Above, U+2A7B
        ('⩼', 0) // Greater-Than with Question Mark Above, U+2A7C
        ('⩽', 0) // Less-Than or Slanted Equal To, U+2A7D
        ('⩾', 0) // Greater-Than or Slanted Equal To, U+2A7E
        ('⩿', 0) // Less-Than or Slanted Equal to with Dot Inside, U+2A7F
        ('⪀', 0) // Greater-Than or Slanted Equal to with Dot Inside, U+2A80
        ('⪁', 0) // Less-Than or Slanted Equal to with Dot Above, U+2A81
        ('⪂', 0) // Greater-Than or Slanted Equal to with Dot Above, U+2A82
        ('⪃', 0) // Less-Than or Slanted Equal to with Dot Above Right, U+2A83
        ('⪄', 0) // Greater-Than or Slanted Equal to with Dot Above Left, U+2A84
        ('⪅', 0) // Less-Than or Approximate, U+2A85
        ('⪆', 0) // Greater-Than or Approximate, U+2A86
        ('⪇', 0) // Less-Than and Single-Line Not Equal To, U+2A87
        ('⪈', 0) // Greater-Than and Single-Line Not Equal To, U+2A88
        ('⪉', 0) // Less-Than and Not Approximate, U+2A89
        ('⪊', 0) // Greater-Than and Not Approximate, U+2A8A
        ('⪋', 0) // Less-Than Above Double-Line Equal Above Greater-Than, U+2A8B
        ('⪌', 0) // Greater-Than Above Double-Line Equal Above Less-Than, U+2A8C
        ('⪍', 0) // Less-Than Above Similar or Equal, U+2A8D
        ('⪎', 0) // Greater-Than Above Similar or Equal, U+2A8E
        ('⪏', 0) // Less-Than Above Similar Above Greater-Than, U+2A8F
        ('⪐', 0) // Greater-Than Above Similar Above Less-Than, U+2A90
        ('⪑', 0) // Less-Than Above Greater-Than Above Double-Line Equal, U+2A91
        ('⪒', 0) // Greater-Than Above Less-Than Above Double-Line Equal, U+2A92
        ('⪓', 0) // Less-Than Above Slanted Equal Above Greater-Than Above Slanted Equal, U+2A93
        ('⪔', 0) // Greater-Than Above Slanted Equal Above Less-Than Above Slanted Equal, U+2A94
        ('⪕', 0) // Slanted Equal to or Less-Than, U+2A95
        ('⪖', 0) // Slanted Equal to or Greater-Than, U+2A96
        ('⪗', 0) // Slanted Equal to or Less-Than with Dot Inside, U+2A97
        ('⪘', 0) // Slanted Equal to or Greater-Than with Dot Inside, U+2A98
        ('⪙', 0) // Double-Line Equal to or Less-Than, U+2A99
        ('⪚', 0) // Double-Line Equal to or Greater-Than, U+2A9A
        ('⪛', 0) // Double-Line Slanted Equal to or Less-Than, U+2A9B
        ('⪜', 0) // Double-Line Slanted Equal to or Greater-Than, U+2A9C
        ('⪝', 0) // Similar or Less-Than, U+2A9D
        ('⪞', 0) // Similar or Greater-Than, U+2A9E
        ('⪟', 0) // Similar Above Less-Than Above Equals Sign, U+2A9F
        ('⪠', 0) // Similar Above Greater-Than Above Equals Sign, U+2AA0
        ('⪡', 0) // Double Nested Less-Than, U+2AA1
        ('⪢', 0) // Double Nested Greater-Than, U+2AA2
        ('⪣', 0) // Double Nested Less-Than with Underbar, U+2AA3
        ('⪤', 0) // Greater-Than Overlapping Less-Than, U+2AA4
        ('⪥', 0) // Greater-Than Beside Less-Than, U+2AA5
        ('⪦', 0) // Less-Than Closed By Curve, U+2AA6
        ('⪧', 0) // Greater-Than Closed By Curve, U+2AA7
        ('⪨', 0) // Less-Than Closed By Curve Above Slanted Equal, U+2AA8
        ('⪩', 0) // Greater-Than Closed By Curve Above Slanted Equal, U+2AA9
        ('⪪', 0) // Smaller Than, U+2AAA
        ('⪫', 0) // Larger Than, U+2AAB
        ('⪬', 0) // Smaller Than or Equal To, U+2AAC
        ('⪭', 0) // Larger Than or Equal To, U+2AAD
        ('⪮', 0) // Equals Sign with Bumpy Above, U+2AAE
        ('⪯', 0) // Precedes Above Single-Line Equals Sign, U+2AAF
        ('⪰', 0) // Succeeds Above Single-Line Equals Sign, U+2AB0
        ('⪱', 0) // Precedes Above Single-Line Not Equal To, U+2AB1
        ('⪲', 0) // Succeeds Above Single-Line Not Equal To, U+2AB2
        ('⪳', 0) // Precedes Above Equals Sign, U+2AB3
        ('⪴', 0) // Succeeds Above Equals Sign, U+2AB4
        ('⪵', 0) // Precedes Above Not Equal To, U+2AB5
        ('⪶', 0) // Succeeds Above Not Equal To, U+2AB6
        ('⪷', 0) // Precedes Above Almost Equal To, U+2AB7
        ('⪸', 0) // Succeeds Above Almost Equal To, U+2AB8
        ('⪹', 0) // Precedes Above Not Almost Equal To, U+2AB9
        ('⪺', 0) // Succeeds Above Not Almost Equal To, U+2ABA
        ('⪻', 0) // Double Precedes, U+2ABB
        ('⪼', 0) // Double Succeeds, U+2ABC
        ('⪽', 0) // Subset with Dot, U+2ABD
        ('⪾', 0) // Superset with Dot, U+2ABE
        ('⪿', 0) // Subset with Plus Sign Below, U+2ABF
        ('⫀', 0) // Superset with Plus Sign Below, U+2AC0
        ('⫁', 0) // Subset with Multiplication Sign Below, U+2AC1
        ('⫂', 0) // Superset with Multiplication Sign Below, U+2AC2
        ('⫃', 0) // Subset of or Equal to with Dot Above, U+2AC3
        ('⫄', 0) // Superset of or Equal to with Dot Above, U+2AC4
        ('⫅', 0) // Subset of Above Equals Sign, U+2AC5
        ('⫆', 0) // Superset of Above Equals Sign, U+2AC6
        ('⫇', 0) // Subset of Above Tilde Operator, U+2AC7
        ('⫈', 0) // Superset of Above Tilde Operator, U+2AC8
        ('⫉', 0) // Subset of Above Almost Equal To, U+2AC9
        ('⫊', 0) // Superset of Above Almost Equal To, U+2ACA
        ('⫋', 0) // Subset of Above Not Equal To, U+2ACB
        ('⫌', 0) // Superset of Above Not Equal To, U+2ACC
        ('⫍', 0) // Square Left Open Box Operator, U+2ACD
        ('⫎', 0) // Square Right Open Box Operator, U+2ACE
        ('⫏', 0) // Closed Subset, U+2ACF
        ('⫐', 0) // Closed Superset, U+2AD0
        ('⫑', 0) // Closed Subset or Equal To, U+2AD1
        ('⫒', 0) // Closed Superset or Equal To, U+2AD2
        ('⫓', 0) // Subset Above Superset, U+2AD3
        ('⫔', 0) // Superset Above Subset, U+2AD4
        ('⫕', 0) // Subset Above Subset, U+2AD5
        ('⫖', 0) // Superset Above Superset, U+2AD6
        ('⫗', 0) // Superset Beside Subset, U+2AD7
        ('⫘', 0) // Superset Beside and Joined By Dash with Subset, U+2AD8
        ('⫙', 0) // Element of Opening Downwards, U+2AD9
        ('⫚', 0) // Pitchfork with Tee Top, U+2ADA
        ('⫛', 0) // Transversal Intersection, U+2ADB
        ('⫝̸', 0) // Forking, U+2ADC
        ('⫝', 0) // Nonforking, U+2ADD
        ('⫞', 0) // Short Left Tack, U+2ADE
        ('⫟', 0) // Short Down Tack, U+2ADF
        ('⫠', 0) // Short Up Tack, U+2AE0
        ('⫡', 0) // Perpendicular with S, U+2AE1
        ('⫢', 0) // Vertical Bar Triple Right Turnstile, U+2AE2
        ('⫣', 0) // Double Vertical Bar Left Turnstile, U+2AE3
        ('⫤', 0) // Vertical Bar Double Left Turnstile, U+2AE4
        ('⫥', 0) // Double Vertical Bar Double Left Turnstile, U+2AE5
        ('⫦', 0) // Long Dash from Left Member of Double Vertical, U+2AE6
        ('⫧', 0) // Short Down Tack with Overbar, U+2AE7
        ('⫨', 0) // Short Up Tack with Underbar, U+2AE8
        ('⫩', 0) // Short Up Tack Above Short Down Tack, U+2AE9
        ('⫪', 0) // Double Down Tack, U+2AEA
        ('⫫', 0) // Double Up Tack, U+2AEB
        ('⫬', 0) // Double Stroke Not Sign, U+2AEC
        ('⫭', 0) // Reversed Double Stroke Not Sign, U+2AED
        ('⫮', 0) // Does Not Divide with Reversed Negation Slash, U+2AEE
        ('⫯', 0) // Vertical Line with Circle Above, U+2AEF
        ('⫰', 0) // Vertical Line with Circle Below, U+2AF0
        ('⫱', 0) // Down Tack with Circle Below, U+2AF1
        ('⫲', 0) // Parallel with Horizontal Stroke, U+2AF2
        ('⫳', 0) // Parallel with Tilde Operator, U+2AF3
        ('⫴', 0) // Triple Vertical Bar Binary Relation, U+2AF4
        ('⫵', 0) // Triple Vertical Bar with Horizontal Stroke, U+2AF5
        ('⫶', 0) // Triple Colon Operator, U+2AF6
        ('⫷', 0) // Triple Nested Less-Than, U+2AF7
        ('⫸', 0) // Triple Nested Greater-Than, U+2AF8
        ('⫹', 0) // Double-Line Slanted Less-Than or Equal To, U+2AF9
        ('⫺', 0) // Double-Line Slanted Greater-Than or Equal To, U+2AFA
        ('⫻', 0) // Triple Solidus Binary Relation, U+2AFB
        ('⫼', 0) // Large Triple Vertical Bar Operator, U+2AFC
        ('⫽', 0) // Double Solidus Operator, U+2AFD
        ('⫾', 0) // White Vertical Bar, U+2AFE
        ('⫿', 0) // N-Ary White Vertical Bar, U+2AFF
        ('⬰', 0) // Left Arrow with Small Circle, U+2B30
        ('⬱', 0) // Three Leftwards Arrows, U+2B31
        ('⬲', 0) // Left Arrow with Circled Plus, U+2B32
        ('⬳', 0) // Long Leftwards Squiggle Arrow, U+2B33
        ('⬴', 0) // Leftwards Two-Headed Arrow with Vertical Stroke, U+2B34
        ('⬵', 0) // Leftwards Two-Headed Arrow with Double Vertical Stroke, U+2B35
        ('⬶', 0) // Leftwards Two-Headed Arrow from Bar, U+2B36
        ('⬷', 0) // Leftwards Two-Headed Triple Dash Arrow, U+2B37
        ('⬸', 0) // Leftwards Arrow with Dotted Stem, U+2B38
        ('⬹', 0) // Leftwards Arrow with Tail with Vertical Stroke, U+2B39
        ('⬺', 0) // Leftwards Arrow with Tail with Double Vertical Stroke, U+2B3A
        ('⬻', 0) // Leftwards Two-Headed Arrow with Tail, U+2B3B
        ('⬼', 0) // Leftwards Two-Headed Arrow with Tail with Vertical Stroke, U+2B3C
        ('⬽', 0) // Leftwards Two-Headed Arrow with Tail with Double Vertical Stroke, U+2B3D
        ('⬾', 0) // Leftwards Arrow Through X, U+2B3E
        ('⬿', 0) // Wave Arrow Pointing Directly Left, U+2B3F
        ('⭀', 0) // Equals Sign Above Leftwards Arrow, U+2B40
        ('⭁', 0) // Reverse Tilde Operator Above Leftwards Arrow, U+2B41
        ('⭂', 0) // Leftwards Arrow Above Reverse Almost Equal To, U+2B42
        ('⭃', 0) // Rightwards Arrow Through Greater-Than, U+2B43
        ('⭄', 0) // Rightwards Arrow Through Superset, U+2B44
        ('⭇', 0) // Reverse Tilde Operator Above Rightwards Arrow, U+2B47
        ('⭈', 0) // Rightwards Arrow Above Reverse Almost Equal To, U+2B48
        ('⭉', 0) // Tilde Operator Above Leftwards Arrow, U+2B49
        ('⭊', 0) // Leftwards Arrow Above Almost Equal To, U+2B4A
        ('⭋', 0) // Leftwards Arrow Above Reverse Tilde Operator, U+2B4B
        ('⭌', 0) // Rightwards Arrow Above Reverse Tilde Operator, U+2B4C
        ('﬩', 0) // Hebrew Letter Alternative Plus Sign, U+FB29
        ('﹢', 0) // Small Plus Sign, U+FE62
        ('﹤', 0) // Small Less-Than Sign, U+FE64
        ('﹥', 0) // Small Greater-Than Sign, U+FE65
        ('﹦', 0) // Small Equals Sign, U+FE66
        ('＋', 0) // Fullwidth Plus Sign, U+FF0B
        ('＜', 0) // Fullwidth Less-Than Sign, U+FF1C
        ('＝', 0) // Fullwidth Equals Sign, U+FF1D
        ('＞', 0) // Fullwidth Greater-Than Sign, U+FF1E
        ('｜', 0) // Fullwidth Vertical Line, U+FF5C
        ('～', 0) // Fullwidth Tilde, U+FF5E
        ('￢', 0) // Fullwidth Not Sign, U+FFE2
        ('￩', 0) // Halfwidth Leftwards Arrow, U+FFE9
        ('￪', 0) // Halfwidth Upwards Arrow, U+FFEA
        ('￫', 0) // Halfwidth Rightwards Arrow, U+FFEB
        ('￬', 0) // Halfwidth Downwards Arrow, U+FFEC
    ]

let isInfix c =
    match mathSymbols.TryGetValue(c) with
    | (true, a) -> (a &&& 2) = 2
    | _ -> false

let isPrefix c =
    match mathSymbols.TryGetValue(c) with
    | (true, a) -> (a &&& 4) = 4
    | _ -> false

let isPostfix c =
    match mathSymbols.TryGetValue(c) with
    | (true, a) -> (a &&& 1) = 1
    | _ -> false

let isObject c =
    match mathSymbols.TryGetValue(c) with
    | (true, a) -> (a &&& 8) = 8
    | _ -> false

let infixMathSymbols: Parser<string,unit> = many1Satisfy isInfix 

let postfixMathSymbols: Parser<string,unit> = many1Satisfy isPostfix 

let prefixMathSymbols: Parser<string,unit> = many1Satisfy isPrefix 

let objectMathSymbols: Parser<string,unit> = many1Satisfy isObject 
