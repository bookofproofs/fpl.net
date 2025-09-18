module FplPrimitives

open System
open System.Collections.Generic
open System.Text.RegularExpressions
open FParsec

(* This module contains information needed by both, the error recovery module and the parser *)

(* Keyword constants *)
[<Literal>]
let literalAlias = "alias"
[<Literal>]
let literalAll = "all"
[<Literal>]
let literalAnd = "and"
[<Literal>]
let literalAss = "ass"
[<Literal>]
let literalAssL = "assert"
[<Literal>]
let literalAssume = "assume"
[<Literal>]
let literalAx = "ax"
[<Literal>]
let literalAxL = "axiom"
[<Literal>]
let literalBase = "base"
[<Literal>]
let literalByAx = "byax"
[<Literal>]
let literalByDef = "bydef"
[<Literal>]
let literalByCor = "bycor"
[<Literal>]
let literalByInf = "byinf"
[<Literal>]
let literalCases = "cases"
[<Literal>]
let literalCl = "cl"
[<Literal>]
let literalClL = "class"
[<Literal>]
let literalCon = "con"
[<Literal>]
let literalConj = "conj"
[<Literal>]
let literalConjL = "conjecture"
[<Literal>]
let literalConL = "conclusion"
[<Literal>]
let literalCor = "cor"
[<Literal>]
let literalCorL = "corollary"
[<Literal>]
let literalCtor = "ctor"
[<Literal>]
let literalCtorL = "constructor"
[<Literal>]
let literalDec = "dec"
[<Literal>]
let literalDecL = "declaration"
[<Literal>]
let literalDef = "def"
[<Literal>]
let literalDefL = "definition"
[<Literal>]
let literalDel = "del"
[<Literal>]
let literalDelL = "delegate"
[<Literal>]
let literalEx = "ex"
[<Literal>]
let literalExN = "exn"
[<Literal>]
let literalExt = "ext"
[<Literal>]
let literalExtL = "extension"
[<Literal>]
let literalFalse = "false"
[<Literal>]
let literalFor = "for"
[<Literal>]
let literalFunc = "func"
[<Literal>]
let literalFuncL = "function"
[<Literal>]
let literalIif = "iif"
[<Literal>]
let literalImpl = "impl"
[<Literal>]
let literalIn = "in"
[<Literal>]
let literalInd = "ind"
[<Literal>]
let literalIndL = "index"
[<Literal>]
let literalInf = "inf"
[<Literal>]
let literalInfix = "infix"
[<Literal>]
let literalInfL = "inference"
[<Literal>]
let literalIntr = "intr"
[<Literal>]
let literalIntrL = "intrinsic"
[<Literal>]
let literalIs = "is"
[<Literal>]
let literalLem = "lem"
[<Literal>]
let literalLemL = "lemma"
[<Literal>]
let literalLoc = "loc"
[<Literal>]
let literalLocL = "localization"
[<Literal>]
let literalMapCases = "mcases"
[<Literal>]
let literalNot = "not"
[<Literal>]
let literalObj = "obj"
[<Literal>]
let literalObjL = "object"
[<Literal>]
let literalOpt = "opt"
[<Literal>]
let literalOptL = "optional"
[<Literal>]
let literalOr = "or"
[<Literal>]
let literalParent = "parent"
[<Literal>]
let literalPost = "post"
[<Literal>]
let literalPostFix = "postfix"
[<Literal>]
let literalPostL = "postulate"
[<Literal>]
let literalPre = "pre"
[<Literal>]
let literalPred = "pred"
[<Literal>]
let literalPredL = "predicate"
[<Literal>]
let literalPrefix = "prefix"
[<Literal>]
let literalPreL = "premise"
[<Literal>]
let literalPrf = "prf"
[<Literal>]
let literalPrfL = "proof"
[<Literal>]
let literalProp = "prop"
[<Literal>]
let literalPropL = "proposition"
[<Literal>]
let literalPrty = "prty"
[<Literal>]
let literalPrtyL = "property"
[<Literal>]
let literalQed = "qed"
[<Literal>]
let literalRet = "ret"
[<Literal>]
let literalRetL = "return"
[<Literal>]
let literalRev = "rev"
[<Literal>]
let literalRevL = "revoke"
[<Literal>]
let literalSelf = "self"
[<Literal>]
let literalSymbol = "symbol"
[<Literal>]
let literalThm = "thm"
[<Literal>]
let literalThmL = "theorem"
[<Literal>]
let literalTpl = "tpl"
[<Literal>]
let literalTplL = "template"
[<Literal>]
let literalTrivial = "trivial"
[<Literal>]
let literalTrue = "true"
[<Literal>]
let literalUndef = "undef"
[<Literal>]
let literalUndefL = "undefined"
[<Literal>]
let literalUndetermined = "undetermined"
[<Literal>]
let literalUses = "uses"
[<Literal>]
let literalXor = "xor"

let keyWordSet =
    HashSet<_>(
        [| literalAlias
           literalAll
           literalAnd
           literalAssL
           literalAss
           literalAssume
           literalAx
           literalAxL
           literalBase
           literalByAx
           literalByCor
           literalByDef
           literalByInf
           literalCases
           literalCl
           literalClL
           literalConj
           literalConjL
           literalCon
           literalConL
           literalCtorL
           literalCor
           literalCorL
           literalCtor
           literalDec
           literalDecL
           literalDef
           literalDefL
           literalDel
           literalDelL
           literalExt
           literalExtL
           literalEx
           literalExN
           literalFalse
           literalFor
           literalFunc
           literalFuncL
           literalIif
           literalImpl
           literalInd
           literalIndL
           literalInfix
           literalIntr
           literalIntrL
           literalInf
           literalInfL
           literalIn
           literalIs
           literalLem
           literalLemL
           literalLoc
           literalLocL
           literalMapCases
           literalNot
           literalObj
           literalObjL
           literalOpt
           literalOptL
           literalOr
           literalParent
           literalPost
           literalPostFix
           literalPostL
           literalPred
           literalPredL
           literalPre
           literalPrefix
           literalPreL
           literalPrf
           literalPrf
           literalProp
           literalPropL
           literalPrty
           literalPrtyL
           literalQed
           literalRet
           literalRetL
           literalRev
           literalRevL
           literalSelf
           literalSymbol
           literalThm
           literalThmL
           literalTrivial
           literalTrue
           literalUndef
           literalUndefL
           literalUses
           literalXor |]
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
        ('a', (2, "Latin Letter a", "U+0041"))
        ('b', (2, "Latin Letter b", "U+0042"))
        ('c', (2, "Latin Letter c", "U+0043"))
        ('d', (2, "Latin Letter d", "U+0044"))
        ('e', (2, "Latin Letter e", "U+0045"))
        ('f', (2, "Latin Letter f", "U+0046"))
        ('g', (2, "Latin Letter g", "U+0047"))
        ('h', (2, "Latin Letter h", "U+0048"))
        ('i', (2, "Latin Letter I", "U+0049"))
        ('j', (2, "Latin Letter j", "U+004A"))
        ('k', (2, "Latin Letter k", "U+004B"))
        ('I', (2, "Latin Letter l", "U+004C"))
        ('m', (2, "Latin Letter m", "U+004D"))
        ('n', (2, "Latin Letter n", "U+004E"))
        ('o', (2, "Latin Letter o", "U+004F"))
        ('p', (2, "Latin Letter p", "U+0050"))
        ('q', (2, "Latin Letter q", "U+0051"))
        ('r', (2, "Latin Letter r", "U+0052"))
        ('s', (2, "Latin Letter s", "U+0053"))
        ('t', (2, "Latin Letter t", "U+0054"))
        ('u', (2, "Latin Letter u", "U+0055"))
        ('v', (2, "Latin Letter v", "U+0056"))
        ('w', (2, "Latin Letter w", "U+0057"))
        ('x', (2, "Latin Letter x", "U+0058"))
        ('y', (2, "Latin Letter y", "U+0059"))
        ('z', (2, "Latin Letter z", "U+005A"))
        ('0', (8, "Digit 0", "U+0030"))
        ('1', (8, "Digit 1", "U+0031"))
        ('2', (8, "Digit 2", "U+0032"))
        ('3', (8, "Digit 3", "U+0033"))
        ('4', (8, "Digit 4", "U+0034"))
        ('5', (8, "Digit 5", "U+0035"))
        ('6', (8, "Digit 6", "U+0036"))
        ('7', (8, "Digit 7", "U+0037"))
        ('8', (8, "Digit 8", "U+0038"))
        ('9', (8, "Digit 9", "U+0039"))
        ('%', (5, "Percent Sign", "U+0025"))
        ('_', (2, "Low Line", "U+005F"))
        ('-', (7, "Hyphen (Minus)", "U+002D"))
        ('*', (7, "Asterisk", "U+002A"))
        ('/', (7, "Slash", "U+002F"))
        ('\\', (2, "Backslash", "U+005C"))
        ('@', (2, "At", "U+0040"))
        ('&', (5, "Ampersand", "U+0026"))
        (''', (5, "Apostrophe", "U+0027"))
        ('!', (1, "Exclamation Mark", "U+0021"))
        ('^', (2, "Circumflex", "U+005E"))
        ('+', (7, "Plus Sign", "U+002B"))
        ('<', (2, "Less-Than Sign", "U+003C"))
        ('=', (2, "Equals Sign", "U+003D"))
        ('>', (2, "Greater-Than Sign", "U+003E"))
        ('|', (0, "Vertical Line", "U+007C"))
        ('¬', (4, "Not Sign", "U+00AC"))
        ('±', (2, "Plus-Minus Sign", "U+00B1"))
        ('×', (2, "Multiplication Sign", "U+00D7"))
        ('÷', (2, "Division Sign", "U+00F7"))
        ('϶', (2, "Greek Reversed Lunate Epsilon Symbol", "U+03F6"))
        ('؆', (4, "Arabic-Indic Cube Root", "U+0606"))
        ('؇', (4, "Arabic-Indic Fourth Root", "U+0607"))
        ('؈', (2, "Arabic Ray", "U+0608"))
        ('⁄', (2, "Fraction Slash", "U+2044"))
        ('⁒', (2, "Commercial Minus Sign", "U+2052"))
        ('⁺', (3, "Superscript Plus Sign", "U+207A"))
        ('⁻', (1, "Superscript Minus", "U+207B"))
        ('⁼', (3, "Superscript Equals Sign", "U+207C"))
        ('₊', (3, "Subscript Plus Sign", "U+208A"))
        ('₋', (1, "Subscript Minus", "U+208B"))
        ('₌', (3, "Subscript Equals Sign", "U+208C"))
        ('℘', (4, "Script Capital P", "U+2118"))
        ('⅀', (4, "Double-Struck N-Ary Summation", "U+2140"))
        ('⅁', (8, "Turned Sans-Serif Capital G", "U+2141"))
        ('⅂', (8, "Turned Sans-Serif Capital L", "U+2142"))
        ('⅃', (8, "Reversed Sans-Serif Capital L", "U+2143"))
        ('⅄', (8, "Turned Sans-Serif Capital Y", "U+2144"))
        ('⅋', (7, "Turned Ampersand", "U+214B"))
        ('←', (2, "Leftwards Arrow", "U+2190"))
        ('↑', (2, "Upwards Arrow", "U+2191"))
        ('→', (2, "Rightwards Arrow", "U+2192"))
        ('↓', (2, "Downwards Arrow", "U+2193"))
        ('↔', (2, "Left Right Arrow", "U+2194"))
        ('↚', (2, "Leftwards Arrow with Stroke", "U+219A"))
        ('↛', (2, "Rightwards Arrow with Stroke", "U+219B"))
        ('↠', (2, "Rightwards Two Headed Arrow", "U+21A0"))
        ('↣', (2, "Rightwards Arrow with Tail", "U+21A3"))
        ('↦', (2, "Rightwards Arrow from Bar", "U+21A6"))
        ('↮', (2, "Left Right Arrow with Stroke", "U+21AE"))
        ('⇎', (2, "Left Right Double Arrow with Stroke", "U+21CE"))
        ('⇏', (2, "Rightwards Double Arrow with Stroke", "U+21CF"))
        ('⇒', (2, "Rightwards Double Arrow", "U+21D2"))
        ('⇔', (2, "Left Right Double Arrow", "U+21D4"))
        ('⇴', (2, "Right Arrow with Small Circle", "U+21F4"))
        ('⇵', (2, "Downwards Arrow Leftwards of Upwards Arrow", "U+21F5"))
        ('⇶', (2, "Three Rightwards Arrows", "U+21F6"))
        ('⇷', (2, "Leftwards Arrow with Vertical Stroke", "U+21F7"))
        ('⇸', (2, "Rightwards Arrow with Vertical Stroke", "U+21F8"))
        ('⇹', (2, "Left Right Arrow with Vertical Stroke", "U+21F9"))
        ('⇺', (2, "Leftwards Arrow with Double Vertical Stroke", "U+21FA"))
        ('⇻', (2, "Rightwards Arrow with Double Vertical Stroke", "U+21FB"))
        ('⇼', (2, "Left Right Arrow with Double Vertical Stroke", "U+21FC"))
        ('⇽', (2, "Leftwards Open-Headed Arrow", "U+21FD"))
        ('⇾', (2, "Rightwards Open-Headed Arrow", "U+21FE"))
        ('⇿', (2, "Left Right Open-Headed Arrow", "U+21FF"))
        ('∀', (4, "For All", "U+2200"))
        ('∁', (4, "Complement", "U+2201"))
        ('∂', (4, "Partial Differential", "U+2202"))
        ('∃', (2, "There Exists", "U+2203"))
        ('∄', (2, "There Does Not Exist", "U+2204"))
        ('∅', (8, "Empty Set", "U+2205"))
        ('∆', (6, "Increment", "U+2206"))
        ('∇', (4, "Nabla", "U+2207"))
        ('∈', (2, "Element Of", "U+2208"))
        ('∉', (2, "Not An Element Of", "U+2209"))
        ('∊', (2, "Small Element Of", "U+220A"))
        ('∋', (2, "Contains as Member", "U+220B"))
        ('∌', (2, "Does Not Contain as Member", "U+220C"))
        ('∍', (2, "Small Contains as Member", "U+220D"))
        ('∏', (4, "N-Ary Product", "U+220F"))
        ('∐', (4, "N-Ary Coproduct", "U+2210"))
        ('∑', (4, "N-Ary Summation", "U+2211"))
        ('−', (6, "Minus Sign", "U+2212"))
        ('∓', (2, "Minus-or-Plus Sign", "U+2213"))
        ('∔', (2, "Dot Plus", "U+2214"))
        ('∕', (2, "Division Slash", "U+2215"))
        ('∖', (2, "Set Minus", "U+2216"))
        ('∗', (2, "Asterisk Operator", "U+2217"))
        ('∘', (2, "Ring Operator", "U+2218"))
        ('∙', (2, "Bullet Operator", "U+2219"))
        ('√', (4, "Square Root", "U+221A"))
        ('∛', (4, "Cube Root", "U+221B"))
        ('∜', (4, "Fourth Root", "U+221C"))
        ('∝', (2, "Proportional To", "U+221D"))
        ('∞', (8, "Infinity", "U+221E"))
        ('∟', (8, "Right Angle", "U+221F"))
        ('∠', (8, "Angle", "U+2220"))
        ('∡', (8, "Measured Angle", "U+2221"))
        ('∢', (8, "Spherical Angle", "U+2222"))
        ('∣', (2, "Divides", "U+2223"))
        ('∤', (2, "Does Not Divide", "U+2224"))
        ('∥', (2, "Parallel To", "U+2225"))
        ('∦', (2, "Not Parallel To", "U+2226"))
        ('∧', (2, "Logical And", "U+2227"))
        ('∨', (2, "Logical Or", "U+2228"))
        ('∩', (2, "Intersection", "U+2229"))
        ('∪', (2, "Union", "U+222A"))
        ('∫', (4, "Integral", "U+222B"))
        ('∬', (4, "Double Integral", "U+222C"))
        ('∭', (4, "Triple Integral", "U+222D"))
        ('∮', (4, "Contour Integral", "U+222E"))
        ('∯', (4, "Surface Integral", "U+222F"))
        ('∰', (4, "Volume Integral", "U+2230"))
        ('∱', (4, "Clockwise Integral", "U+2231"))
        ('∲', (4, "Clockwise Contour Integral", "U+2232"))
        ('∳', (4, "Anticlockwise Contour Integral", "U+2233"))
        ('∴', (2, "Therefore", "U+2234"))
        ('∵', (2, "Because", "U+2235"))
        ('∶', (2, "Ratio", "U+2236"))
        ('∷', (2, "Proportion", "U+2237"))
        ('∸', (7, "Dot Minus", "U+2238"))
        ('∹', (2, "Excess", "U+2239"))
        ('∺', (7, "Geometric Proportion", "U+223A"))
        ('∻', (7, "Homothetic", "U+223B"))
        ('∼', (7, "Tilde Operator", "U+223C"))
        ('∽', (7, "Reversed Tilde", "U+223D"))
        ('∾', (7, "Inverted Lazy S", "U+223E"))
        ('∿', (8, "Sine Wave", "U+223F"))
        ('≀', (2, "Wreath Product", "U+2240"))
        ('≁', (7, "Not Tilde", "U+2241"))
        ('≂', (2, "Minus Tilde", "U+2242"))
        ('≃', (2, "Asymptotically Equal To", "U+2243"))
        ('≄', (2, "Not Asymptotically Equal To", "U+2244"))
        ('≅', (2, "Approximately Equal To", "U+2245"))
        ('≆', (2, "Approximately But Not Actually Equal To", "U+2246"))
        ('≇', (2, "Neither Approximately Nor Actually Equal To", "U+2247"))
        ('≈', (2, "Almost Equal To", "U+2248"))
        ('≉', (2, "Not Almost Equal To", "U+2249"))
        ('≊', (2, "Almost Equal or Equal To", "U+224A"))
        ('≋', (2, "Triple Tilde", "U+224B"))
        ('≌', (2, "All Equal To", "U+224C"))
        ('≍', (2, "Equivalent To", "U+224D"))
        ('≎', (2, "Geometrically Equivalent To", "U+224E"))
        ('≏', (2, "Difference Between", "U+224F"))
        ('≐', (2, "Approaches the Limit", "U+2250"))
        ('≑', (2, "Geometrically Equal To", "U+2251"))
        ('≒', (2, "Approximately Equal to or the Image Of", "U+2252"))
        ('≓', (2, "Image of or Approximately Equal To", "U+2253"))
        ('≖', (2, "Ring In Equal To", "U+2256"))
        ('≗', (2, "Ring Equal To", "U+2257"))
        ('≘', (2, "Corresponds To", "U+2258"))
        ('≙', (2, "Estimates", "U+2259"))
        ('≚', (2, "Equiangular To", "U+225A"))
        ('≛', (2, "Star Equals", "U+225B"))
        ('≜', (2, "Delta Equal To", "U+225C"))
        ('≞', (2, "Measured By", "U+225E"))
        ('≟', (2, "Questioned Equal To", "U+225F"))
        ('≠', (2, "Not Equal To", "U+2260"))
        ('≡', (2, "Identical To", "U+2261"))
        ('≢', (2, "Not Identical To", "U+2262"))
        ('≣', (2, "Strictly Equivalent To", "U+2263"))
        ('≤', (2, "Less-Than or Equal To", "U+2264"))
        ('≥', (2, "Greater-Than or Equal To", "U+2265"))
        ('≦', (2, "Less-Than Over Equal To", "U+2266"))
        ('≧', (2, "Greater-Than Over Equal To", "U+2267"))
        ('≨', (2, "Less-Than But Not Equal To", "U+2268"))
        ('≩', (2, "Greater-Than But Not Equal To", "U+2269"))
        ('≪', (2, "Much Less-Than", "U+226A"))
        ('≫', (2, "Much Greater-Than", "U+226B"))
        ('≬', (2, "Between", "U+226C"))
        ('≭', (2, "Not Equivalent To", "U+226D"))
        ('≮', (2, "Not Less-Than", "U+226E"))
        ('≯', (2, "Not Greater-Than", "U+226F"))
        ('≰', (2, "Neither Less-Than Nor Equal To", "U+2270"))
        ('≱', (2, "Neither Greater-Than Nor Equal To", "U+2271"))
        ('≲', (2, "Less-Than or Equivalent To", "U+2272"))
        ('≳', (2, "Greater-Than or Equivalent To", "U+2273"))
        ('≴', (2, "Neither Less-Than Nor Equivalent To", "U+2274"))
        ('≵', (2, "Neither Greater-Than Nor Equivalent To", "U+2275"))
        ('≶', (2, "Less-Than or Greater-Than", "U+2276"))
        ('≷', (2, "Greater-Than or Less-Than", "U+2277"))
        ('≸', (2, "Neither Less-Than Nor Greater-Than", "U+2278"))
        ('≹', (2, "Neither Greater-Than Nor Less-Than", "U+2279"))
        ('≺', (2, "Precedes", "U+227A"))
        ('≻', (2, "Succeeds", "U+227B"))
        ('≼', (2, "Precedes or Equal To", "U+227C"))
        ('≽', (2, "Succeeds or Equal To", "U+227D"))
        ('≾', (2, "Precedes or Equivalent To", "U+227E"))
        ('≿', (2, "Succeeds or Equivalent To", "U+227F"))
        ('⊀', (2, "Does Not Precede", "U+2280"))
        ('⊁', (2, "Does Not Succeed", "U+2281"))
        ('⊂', (2, "Subset Of", "U+2282"))
        ('⊃', (2, "Superset Of", "U+2283"))
        ('⊄', (2, "Not A Subset Of", "U+2284"))
        ('⊅', (2, "Not A Superset Of", "U+2285"))
        ('⊆', (2, "Subset of or Equal To", "U+2286"))
        ('⊇', (2, "Superset of or Equal To", "U+2287"))
        ('⊈', (2, "Neither A Subset of Nor Equal To", "U+2288"))
        ('⊉', (2, "Neither A Superset of Nor Equal To", "U+2289"))
        ('⊊', (2, "Subset of with Not Equal To", "U+228A"))
        ('⊋', (2, "Superset of with Not Equal To", "U+228B"))
        ('⊌', (2, "Multiset", "U+228C"))
        ('⊍', (2, "Multiset Multiplication", "U+228D"))
        ('⊎', (2, "Multiset Union", "U+228E"))
        ('⊏', (2, "Square Image Of", "U+228F"))
        ('⊐', (2, "Square Original Of", "U+2290"))
        ('⊑', (2, "Square Image of or Equal To", "U+2291"))
        ('⊒', (2, "Square Original of or Equal To", "U+2292"))
        ('⊓', (2, "Square Cap", "U+2293"))
        ('⊔', (2, "Square Cup", "U+2294"))
        ('⊕', (2, "Circled Plus", "U+2295"))
        ('⊖', (2, "Circled Minus", "U+2296"))
        ('⊗', (2, "Circled Times", "U+2297"))
        ('⊘', (2, "Circled Division Slash", "U+2298"))
        ('⊙', (2, "Circled Dot Operator", "U+2299"))
        ('⊚', (2, "Circled Ring Operator", "U+229A"))
        ('⊛', (2, "Circled Asterisk Operator", "U+229B"))
        ('⊜', (2, "Circled Equals", "U+229C"))
        ('⊝', (2, "Circled Dash", "U+229D"))
        ('⊞', (2, "Squared Plus", "U+229E"))
        ('⊟', (2, "Squared Minus", "U+229F"))
        ('⊠', (2, "Squared Times", "U+22A0"))
        ('⊡', (2, "Squared Dot Operator", "U+22A1"))
        ('⊢', (6, "Right Tack", "U+22A2"))
        ('⊣', (6, "Left Tack", "U+22A3"))
        ('⊤', (6, "Down Tack", "U+22A4"))
        ('⊥', (6, "Up Tack", "U+22A5"))
        ('⊦', (6, "Assertion", "U+22A6"))
        ('⊧', (6, "Models", "U+22A7"))
        ('⊨', (6, "1", "U+22A8"))
        ('⊩', (6, "Forces", "U+22A9"))
        ('⊪', (6, "Triple Vertical Bar Right Turnstile", "U+22AA"))
        ('⊫', (6, "Double Vertical Bar Double Right Turnstile", "U+22AB"))
        ('⊬', (6, "Does Not Prove", "U+22AC"))
        ('⊭', (6, "Not True", "U+22AD"))
        ('⊮', (6, "Does Not Force", "U+22AE"))
        ('⊯', (6, "Negated Double Vertical Bar Double Right Turnstile", "U+22AF"))
        ('⊰', (6, "Precedes Under Relation", "U+22B0"))
        ('⊱', (6, "Succeeds Under Relation", "U+22B1"))
        ('⊲', (2, "Normal Subgroup Of", "U+22B2"))
        ('⊳', (2, "Contains as Normal Subgroup", "U+22B3"))
        ('⊴', (2, "Normal Subgroup of or Equal To", "U+22B4"))
        ('⊵', (2, "Contains as Normal Subgroup or Equal To", "U+22B5"))
        ('⊶', (2, "Original Of", "U+22B6"))
        ('⊷', (2, "Image Of", "U+22B7"))
        ('⊸', (2, "Multimap", "U+22B8"))
        ('⊹', (7, "Hermitian Conjugate Matrix", "U+22B9"))
        ('⊺', (7, "Intercalate", "U+22BA"))
        ('⊻', (2, "Xor", "U+22BB"))
        ('⊼', (2, "Nand", "U+22BC"))
        ('⊽', (2, "Nor", "U+22BD"))
        ('⊾', (8, "Right Angle with Arc", "U+22BE"))
        ('⊿', (8, "Right Triangle", "U+22BF"))
        ('⋀', (4, "N-Ary Logical And", "U+22C0"))
        ('⋁', (4, "N-Ary Logical Or", "U+22C1"))
        ('⋂', (4, "N-Ary Intersection", "U+22C2"))
        ('⋃', (4, "N-Ary Union", "U+22C3"))
        ('⋄', (7, "Diamond Operator", "U+22C4"))
        ('⋅', (7, "Dot Operator", "U+22C5"))
        ('⋆', (7, "Star Operator", "U+22C6"))
        ('⋇', (7, "Division Times", "U+22C7"))
        ('⋈', (7, "Bowtie", "U+22C8"))
        ('⋉', (7, "Left Normal Factor Semidirect Product", "U+22C9"))
        ('⋊', (7, "Right Normal Factor Semidirect Product", "U+22CA"))
        ('⋋', (7, "Left Semidirect Product", "U+22CB"))
        ('⋌', (7, "Right Semidirect Product", "U+22CC"))
        ('⋍', (2, "Reversed Tilde Equals", "U+22CD"))
        ('⋎', (2, "Curly Logical Or", "U+22CE"))
        ('⋏', (2, "Curly Logical And", "U+22CF"))
        ('⋐', (2, "Double Subset", "U+22D0"))
        ('⋑', (2, "Double Superset", "U+22D1"))
        ('⋒', (2, "Double Intersection", "U+22D2"))
        ('⋓', (2, "Double Union", "U+22D3"))
        ('⋔', (2, "Pitchfork", "U+22D4"))
        ('⋕', (2, "Equal and Parallel To", "U+22D5"))
        ('⋖', (2, "Less-Than with Dot", "U+22D6"))
        ('⋗', (2, "Greater-Than with Dot", "U+22D7"))
        ('⋘', (2, "Very Much Less-Than", "U+22D8"))
        ('⋙', (2, "Very Much Greater-Than", "U+22D9"))
        ('⋚', (2, "Less-Than Equal to or Greater-Than", "U+22DA"))
        ('⋛', (2, "Greater-Than Equal to or Less-Than", "U+22DB"))
        ('⋜', (2, "Equal to or Less-Than", "U+22DC"))
        ('⋝', (2, "Equal to or Greater-Than", "U+22DD"))
        ('⋞', (2, "Equal to or Precedes", "U+22DE"))
        ('⋟', (2, "Equal to or Succeeds", "U+22DF"))
        ('⋠', (2, "Does Not Precede or Equal", "U+22E0"))
        ('⋡', (2, "Does Not Succeed or Equal", "U+22E1"))
        ('⋢', (2, "Not Square Image of or Equal To", "U+22E2"))
        ('⋣', (2, "Not Square Original of or Equal To", "U+22E3"))
        ('⋤', (2, "Square Image of or Not Equal To", "U+22E4"))
        ('⋥', (2, "Square Original of or Not Equal To", "U+22E5"))
        ('⋦', (2, "Less-Than But Not Equivalent To", "U+22E6"))
        ('⋧', (2, "Greater-Than But Not Equivalent To", "U+22E7"))
        ('⋨', (2, "Precedes But Not Equivalent To", "U+22E8"))
        ('⋩', (2, "Succeeds But Not Equivalent To", "U+22E9"))
        ('⋪', (2, "Not Normal Subgroup Of", "U+22EA"))
        ('⋫', (2, "Does Not Contain as Normal Subgroup", "U+22EB"))
        ('⋬', (2, "Not Normal Subgroup of or Equal To", "U+22EC"))
        ('⋭', (2, "Does Not Contain as Normal Subgroup or Equal", "U+22ED"))
        ('⋲', (2, "Element of with Long Horizontal Stroke", "U+22F2"))
        ('⋳', (2, "Element of with Vertical Bar at End of Horizontal Stroke", "U+22F3"))
        ('⋴', (2, "Small Element of with Vertical Bar at End of Horizontal Stroke", "U+22F4"))
        ('⋵', (2, "Element of with Dot Above", "U+22F5"))
        ('⋶', (2, "Element of with Overbar", "U+22F6"))
        ('⋷', (2, "Small Element of with Overbar", "U+22F7"))
        ('⋸', (2, "Element of with Underbar", "U+22F8"))
        ('⋹', (2, "Element of with Two Horizontal Strokes", "U+22F9"))
        ('⋺', (2, "Contains with Long Horizontal Stroke", "U+22FA"))
        ('⋻', (2, "Contains with Vertical Bar at End of Horizontal Stroke", "U+22FB"))
        ('⋼', (2, "Small Contains with Vertical Bar at End of Horizontal Stroke", "U+22FC"))
        ('⋽', (2, "Contains with Overbar", "U+22FD"))
        ('⋾', (2, "Small Contains with Overbar", "U+22FE"))
        ('⋿', (2, "Z Notation Bag Membership", "U+22FF"))
        ('⍼', (8, "Right Angle with Downwards Zigzag Arrow", "U+237C"))
        ('▷', (2, "White Right-Pointing Triangle", "U+25B7"))
        ('◁', (2, "White Left-Pointing Triangle", "U+25C1"))
        ('◸', (7, "Upper Left Triangle", "U+25F8"))
        ('◹', (7, "Upper Right Triangle", "U+25F9"))
        ('◺', (7, "Lower Left Triangle", "U+25FA"))
        ('◻', (7, "White Medium Square", "U+25FB"))
        ('◼', (7, "Black Medium Square", "U+25FC"))
        ('◽', (7, "White Medium Small Square", "U+25FD"))
        ('◾', (7, "Black Medium Small Square", "U+25FE"))
        ('◿', (7, "Lower Right Triangle", "U+25FF"))
        ('♯', (7, "Music Sharp Sign", "U+266F"))
        ('⟀', (7, "Three Dimensional Angle", "U+27C0"))
        ('⟁', (7, "White Triangle Containing Small White Triangle", "U+27C1"))
        ('⟂', (2, "Perpendicular", "U+27C2"))
        ('⟃', (2, "Open Subset", "U+27C3"))
        ('⟄', (2, "Open Superset", "U+27C4"))
        ('⟇', (2, "or with Dot Inside", "U+27C7"))
        ('⟈', (2, "Reverse Solidus Preceding Subset", "U+27C8"))
        ('⟉', (7, "Superset Preceding Solidus", "U+27C9"))
        ('⟊', (7, "Vertical Bar with Horizontal Stroke", "U+27CA"))
        ('⟋', (7, "Mathematical Rising Diagonal", "U+27CB"))
        ('⟌', (7, "Long Division", "U+27CC"))
        ('⟍', (7, "Mathematical Falling Diagonal", "U+27CD"))
        ('⟎', (2, "Squared Logical And", "U+27CE"))
        ('⟏', (2, "Squared Logical Or", "U+27CF"))
        ('⟐', (7, "White Diamond with Centred Dot", "U+27D0"))
        ('⟑', (7, "and with Dot", "U+27D1"))
        ('⟒', (2, "Element of Opening Upwards", "U+27D2"))
        ('⟓', (7, "Lower Right Corner with Dot", "U+27D3"))
        ('⟔', (7, "Upper Left Corner with Dot", "U+27D4"))
        ('⟕', (7, "Left Outer Join", "U+27D5"))
        ('⟖', (7, "Right Outer Join", "U+27D6"))
        ('⟗', (7, "Full Outer Join", "U+27D7"))
        ('⟘', (7, "Large Up Tack", "U+27D8"))
        ('⟙', (7, "Large Down Tack", "U+27D9"))
        ('⟚', (7, "Left and Right Double Turnstile", "U+27DA"))
        ('⟛', (7, "Left and Right Tack", "U+27DB"))
        ('⟜', (7, "Left Multimap", "U+27DC"))
        ('⟝', (7, "Long Right Tack", "U+27DD"))
        ('⟞', (7, "Long Left Tack", "U+27DE"))
        ('⟟', (7, "Up Tack with Circle Above", "U+27DF"))
        ('⟠', (7, "Lozenge Divided By Horizontal Rule", "U+27E0"))
        ('⟡', (7, "White Concave-Sided Diamond", "U+27E1"))
        ('⟢', (2, "White Concave-Sided Diamond with Leftwards Tick", "U+27E2"))
        ('⟣', (2, "White Concave-Sided Diamond with Rightwards Tick", "U+27E3"))
        ('⟤', (2, "White Square with Leftwards Tick", "U+27E4"))
        ('⟥', (2, "White Square with Rightwards Tick", "U+27E5"))
        ('⟰', (2, "Upwards Quadruple Arrow", "U+27F0"))
        ('⟱', (2, "Downwards Quadruple Arrow", "U+27F1"))
        ('⟲', (7, "Anticlockwise Gapped Circle Arrow", "U+27F2"))
        ('⟳', (7, "Clockwise Gapped Circle Arrow", "U+27F3"))
        ('⟴', (2, "Right Arrow with Circled Plus", "U+27F4"))
        ('⟵', (2, "Long Leftwards Arrow", "U+27F5"))
        ('⟶', (2, "Long Rightwards Arrow", "U+27F6"))
        ('⟷', (2, "Long Left Right Arrow", "U+27F7"))
        ('⟸', (2, "Long Leftwards Double Arrow", "U+27F8"))
        ('⟹', (2, "Long Rightwards Double Arrow", "U+27F9"))
        ('⟺', (2, "Long Left Right Double Arrow", "U+27FA"))
        ('⟻', (2, "Long Leftwards Arrow from Bar", "U+27FB"))
        ('⟼', (2, "Long Rightwards Arrow from Bar", "U+27FC"))
        ('⟽', (2, "Long Leftwards Double Arrow from Bar", "U+27FD"))
        ('⟾', (2, "Long Rightwards Double Arrow from Bar", "U+27FE"))
        ('⟿', (2, "Long Rightwards Squiggle Arrow", "U+27FF"))
        ('⤀', (2, "Rightwards Two-Headed Arrow with Vertical Stroke", "U+2900"))
        ('⤁', (2, "Rightwards Two-Headed Arrow with Double Vertical Stroke", "U+2901"))
        ('⤂', (2, "Leftwards Double Arrow with Vertical Stroke", "U+2902"))
        ('⤃', (2, "Rightwards Double Arrow with Vertical Stroke", "U+2903"))
        ('⤄', (2, "Left Right Double Arrow with Vertical Stroke", "U+2904"))
        ('⤅', (2, "Rightwards Two-Headed Arrow from Bar", "U+2905"))
        ('⤆', (2, "Leftwards Double Arrow from Bar", "U+2906"))
        ('⤇', (2, "Rightwards Double Arrow from Bar", "U+2907"))
        ('⤈', (2, "Downwards Arrow with Horizontal Stroke", "U+2908"))
        ('⤉', (2, "Upwards Arrow with Horizontal Stroke", "U+2909"))
        ('⤊', (2, "Upwards Triple Arrow", "U+290A"))
        ('⤋', (2, "Downwards Triple Arrow", "U+290B"))
        ('⤌', (2, "Leftwards Double Dash Arrow", "U+290C"))
        ('⤍', (2, "Rightwards Double Dash Arrow", "U+290D"))
        ('⤎', (2, "Leftwards Triple Dash Arrow", "U+290E"))
        ('⤏', (2, "Rightwards Triple Dash Arrow", "U+290F"))
        ('⤐', (2, "Rightwards Two-Headed Triple Dash Arrow", "U+2910"))
        ('⤑', (2, "Rightwards Arrow with Dotted Stem", "U+2911"))
        ('⤒', (2, "Upwards Arrow to Bar", "U+2912"))
        ('⤓', (2, "Downwards Arrow to Bar", "U+2913"))
        ('⤔', (2, "Rightwards Arrow with Tail with Vertical Stroke", "U+2914"))
        ('⤕', (2, "Rightwards Arrow with Tail with Double Vertical Stroke", "U+2915"))
        ('⤖', (2, "Rightwards Two-Headed Arrow with Tail", "U+2916"))
        ('⤗', (2, "Rightwards Two-Headed Arrow with Tail with Vertical Stroke", "U+2917"))
        ('⤘', (2, "Rightwards Two-Headed Arrow with Tail with Double Vertical Stroke", "U+2918"))
        ('⤙', (2, "Leftwards Arrow-Tail", "U+2919"))
        ('⤚', (2, "Rightwards Arrow-Tail", "U+291A"))
        ('⤛', (2, "Leftwards Double Arrow-Tail", "U+291B"))
        ('⤜', (2, "Rightwards Double Arrow-Tail", "U+291C"))
        ('⤝', (2, "Leftwards Arrow to Black Diamond", "U+291D"))
        ('⤞', (2, "Rightwards Arrow to Black Diamond", "U+291E"))
        ('⤟', (2, "Leftwards Arrow from Bar to Black Diamond", "U+291F"))
        ('⤠', (2, "Rightwards Arrow from Bar to Black Diamond", "U+2920"))
        ('⤡', (2, "North West and South East Arrow", "U+2921"))
        ('⤢', (2, "North East and South West Arrow", "U+2922"))
        ('⤣', (2, "North West Arrow with Hook", "U+2923"))
        ('⤤', (2, "North East Arrow with Hook", "U+2924"))
        ('⤥', (2, "South East Arrow with Hook", "U+2925"))
        ('⤦', (2, "South West Arrow with Hook", "U+2926"))
        ('⤧', (2, "North West Arrow and North East Arrow", "U+2927"))
        ('⤨', (2, "North East Arrow and South East Arrow", "U+2928"))
        ('⤩', (2, "South East Arrow and South West Arrow", "U+2929"))
        ('⤪', (2, "South West Arrow and North West Arrow", "U+292A"))
        ('⤫', (7, "Rising Diagonal Crossing Falling Diagonal", "U+292B"))
        ('⤬', (7, "Falling Diagonal Crossing Rising Diagonal", "U+292C"))
        ('⤭', (2, "South East Arrow Crossing North East Arrow", "U+292D"))
        ('⤮', (2, "North East Arrow Crossing South East Arrow", "U+292E"))
        ('⤯', (2, "Falling Diagonal Crossing North East Arrow", "U+292F"))
        ('⤰', (2, "Rising Diagonal Crossing South East Arrow", "U+2930"))
        ('⤱', (2, "North East Arrow Crossing North West Arrow", "U+2931"))
        ('⤲', (2, "North West Arrow Crossing North East Arrow", "U+2932"))
        ('⤳', (2, "Wave Arrow Pointing Directly Right", "U+2933"))
        ('⤴', (2, "Arrow Pointing Rightwards Then Curving Upwards", "U+2934"))
        ('⤵', (2, "Arrow Pointing Rightwards Then Curving Downwards", "U+2935"))
        ('⤶', (2, "Arrow Pointing Downwards Then Curving Leftwards", "U+2936"))
        ('⤷', (2, "Arrow Pointing Downwards Then Curving Rightwards", "U+2937"))
        ('⤸', (2, "Right-Side Arc Clockwise Arrow", "U+2938"))
        ('⤹', (2, "Left-Side Arc Anticlockwise Arrow", "U+2939"))
        ('⤺', (2, "Top Arc Anticlockwise Arrow", "U+293A"))
        ('⤻', (2, "Bottom Arc Anticlockwise Arrow", "U+293B"))
        ('⤼', (2, "Top Arc Clockwise Arrow with Minus", "U+293C"))
        ('⤽', (2, "Top Arc Anticlockwise Arrow with Plus", "U+293D"))
        ('⤾', (2, "Lower Right Semicircular Clockwise Arrow", "U+293E"))
        ('⤿', (2, "Lower Left Semicircular Anticlockwise Arrow", "U+293F"))
        ('⥀', (2, "Anticlockwise Closed Circle Arrow", "U+2940"))
        ('⥁', (2, "Clockwise Closed Circle Arrow", "U+2941"))
        ('⥂', (2, "Rightwards Arrow Above Short Leftwards Arrow", "U+2942"))
        ('⥃', (2, "Leftwards Arrow Above Short Rightwards Arrow", "U+2943"))
        ('⥄', (2, "Short Rightwards Arrow Above Leftwards Arrow", "U+2944"))
        ('⥅', (2, "Rightwards Arrow with Plus Below", "U+2945"))
        ('⥆', (2, "Leftwards Arrow with Plus Below", "U+2946"))
        ('⥇', (2, "Rightwards Arrow Through X", "U+2947"))
        ('⥈', (2, "Left Right Arrow Through Small Circle", "U+2948"))
        ('⥉', (2, "Upwards Two-Headed Arrow from Small Circle", "U+2949"))
        ('⥊', (7, "Left Barb Up Right Barb Down Harpoon", "U+294A"))
        ('⥋', (7, "Left Barb Down Right Barb Up Harpoon", "U+294B"))
        ('⥌', (7, "Up Barb Right Down Barb Left Harpoon", "U+294C"))
        ('⥍', (7, "Up Barb Left Down Barb Right Harpoon", "U+294D"))
        ('⥎', (7, "Left Barb Up Right Barb Up Harpoon", "U+294E"))
        ('⥏', (7, "Up Barb Right Down Barb Right Harpoon", "U+294F"))
        ('⥐', (7, "Left Barb Down Right Barb Down Harpoon", "U+2950"))
        ('⥑', (7, "Up Barb Left Down Barb Left Harpoon", "U+2951"))
        ('⥒', (2, "Leftwards Harpoon with Barb Up to Bar", "U+2952"))
        ('⥓', (2, "Rightwards Harpoon with Barb Up to Bar", "U+2953"))
        ('⥔', (2, "Upwards Harpoon with Barb Right to Bar", "U+2954"))
        ('⥕', (2, "Downwards Harpoon with Barb Right to Bar", "U+2955"))
        ('⥖', (2, "Leftwards Harpoon with Barb Down to Bar", "U+2956"))
        ('⥗', (2, "Rightwards Harpoon with Barb Down to Bar", "U+2957"))
        ('⥘', (2, "Upwards Harpoon with Barb Left to Bar", "U+2958"))
        ('⥙', (2, "Downwards Harpoon with Barb Left to Bar", "U+2959"))
        ('⥚', (2, "Leftwards Harpoon with Barb Up from Bar", "U+295A"))
        ('⥛', (2, "Rightwards Harpoon with Barb Up from Bar", "U+295B"))
        ('⥜', (2, "Upwards Harpoon with Barb Right from Bar", "U+295C"))
        ('⥝', (2, "Downwards Harpoon with Barb Right from Bar", "U+295D"))
        ('⥞', (2, "Leftwards Harpoon with Barb Down from Bar", "U+295E"))
        ('⥟', (2, "Rightwards Harpoon with Barb Down from Bar", "U+295F"))
        ('⥠', (2, "Upwards Harpoon with Barb Left from Bar", "U+2960"))
        ('⥡', (2, "Downwards Harpoon with Barb Left from Bar", "U+2961"))
        ('⥢', (2, "Leftwards Harpoon with Barb Up Above Leftwards Harpoon with Barb Down", "U+2962"))
        ('⥣', (2, "Upwards Harpoon with Barb Left Beside Upwards Harpoon with Barb Right", "U+2963"))
        ('⥤', (2, "Rightwards Harpoon with Barb Up Above Rightwards Harpoon with Barb Down", "U+2964"))
        ('⥥', (2, "Downwards Harpoon with Barb Left Beside Downwards Harpoon with Barb Right", "U+2965"))
        ('⥦', (2, "Leftwards Harpoon with Barb Up Above Rightwards Harpoon with Barb Up", "U+2966"))
        ('⥧', (2, "Leftwards Harpoon with Barb Down Above Rightwards Harpoon with Barb Down", "U+2967"))
        ('⥨', (2, "Rightwards Harpoon with Barb Up Above Leftwards Harpoon with Barb Up", "U+2968"))
        ('⥩', (2, "Rightwards Harpoon with Barb Down Above Leftwards Harpoon with Barb Down", "U+2969"))
        ('⥪', (2, "Leftwards Harpoon with Barb Up Above Long Dash", "U+296A"))
        ('⥫', (2, "Leftwards Harpoon with Barb Down Below Long Dash", "U+296B"))
        ('⥬', (2, "Rightwards Harpoon with Barb Up Above Long Dash", "U+296C"))
        ('⥭', (2, "Rightwards Harpoon with Barb Down Below Long Dash", "U+296D"))
        ('⥮', (2, "Upwards Harpoon with Barb Left Beside Downwards Harpoon with Barb Right", "U+296E"))
        ('⥯', (2, "Downwards Harpoon with Barb Left Beside Upwards Harpoon with Barb Right", "U+296F"))
        ('⥰', (2, "Right Double Arrow with Rounded Head", "U+2970"))
        ('⥱', (2, "Equals Sign Above Rightwards Arrow", "U+2971"))
        ('⥲', (2, "Tilde Operator Above Rightwards Arrow", "U+2972"))
        ('⥳', (2, "Leftwards Arrow Above Tilde Operator", "U+2973"))
        ('⥴', (2, "Rightwards Arrow Above Tilde Operator", "U+2974"))
        ('⥵', (2, "Rightwards Arrow Above Almost Equal To", "U+2975"))
        ('⥶', (2, "Less-Than Above Leftwards Arrow", "U+2976"))
        ('⥷', (2, "Leftwards Arrow Through Less-Than", "U+2977"))
        ('⥸', (2, "Greater-Than Above Rightwards Arrow", "U+2978"))
        ('⥹', (2, "Subset Above Rightwards Arrow", "U+2979"))
        ('⥺', (2, "Leftwards Arrow Through Subset", "U+297A"))
        ('⥻', (2, "Superset Above Leftwards Arrow", "U+297B"))
        ('⥼', (7, "Left Fish Tail", "U+297C"))
        ('⥽', (7, "Right Fish Tail", "U+297D"))
        ('⥾', (7, "Up Fish Tail", "U+297E"))
        ('⥿', (7, "Down Fish Tail", "U+297F"))
        ('⦀', (7, "Triple Vertical Bar Delimiter", "U+2980"))
        ('⦁', (7, "Z Notation Spot", "U+2981"))
        ('⦂', (7, "Z Notation Type Colon", "U+2982"))
        ('⦙', (7, "Dotted Fence", "U+2999"))
        ('⦚', (7, "Vertical Zigzag Line", "U+299A"))
        ('⦛', (8, "Measured Angle Opening Left", "U+299B"))
        ('⦜', (8, "Right Angle Variant with Square", "U+299C"))
        ('⦝', (8, "Measured Right Angle with Dot", "U+299D"))
        ('⦞', (8, "Angle with S Inside", "U+299E"))
        ('⦟', (8, "Acute Angle", "U+299F"))
        ('⦠', (8, "Spherical Angle Opening Left", "U+29A0"))
        ('⦡', (8, "Spherical Angle Opening Up", "U+29A1"))
        ('⦢', (7, "Turned Angle", "U+29A2"))
        ('⦣', (7, "Reversed Angle", "U+29A3"))
        ('⦤', (7, "Angle with Underbar", "U+29A4"))
        ('⦥', (7, "Reversed Angle with Underbar", "U+29A5"))
        ('⦦', (7, "Oblique Angle Opening Up", "U+29A6"))
        ('⦧', (7, "Oblique Angle Opening Down", "U+29A7"))
        ('⦨', (2, "Measured Angle with Open Arm Ending In Arrow Pointing Up and Right", "U+29A8"))
        ('⦩', (2, "Measured Angle with Open Arm Ending In Arrow Pointing Up and Left", "U+29A9"))
        ('⦪', (2, "Measured Angle with Open Arm Ending In Arrow Pointing Down and Right", "U+29AA"))
        ('⦫', (2, "Measured Angle with Open Arm Ending In Arrow Pointing Down and Left", "U+29AB"))
        ('⦬', (2, "Measured Angle with Open Arm Ending In Arrow Pointing Right and Up", "U+29AC"))
        ('⦭', (2, "Measured Angle with Open Arm Ending In Arrow Pointing Left and Up", "U+29AD"))
        ('⦮', (2, "Measured Angle with Open Arm Ending In Arrow Pointing Right and Down", "U+29AE"))
        ('⦯', (2, "Measured Angle with Open Arm Ending In Arrow Pointing Left and Down", "U+29AF"))
        ('⦰', (7, "Reversed Empty Set", "U+29B0"))
        ('⦱', (7, "Empty Set with Overbar", "U+29B1"))
        ('⦲', (7, "Empty Set with Small Circle Above", "U+29B2"))
        ('⦳', (2, "Empty Set with Right Arrow Above", "U+29B3"))
        ('⦴', (2, "Empty Set with Left Arrow Above", "U+29B4"))
        ('⦵', (7, "Circle with Horizontal Bar", "U+29B5"))
        ('⦶', (7, "Circled Vertical Bar", "U+29B6"))
        ('⦷', (7, "Circled Parallel", "U+29B7"))
        ('⦸', (7, "Circled Reverse Solidus", "U+29B8"))
        ('⦹', (7, "Circled Perpendicular", "U+29B9"))
        ('⦺', (7, "Circle Divided By Horizontal Bar and Top Half Divided By Vertical Bar", "U+29BA"))
        ('⦻', (7, "Circle with Superimposed X", "U+29BB"))
        ('⦼', (7, "Circled Anticlockwise-Rotated Division Sign", "U+29BC"))
        ('⦽', (2, "Up Arrow Through Circle", "U+29BD"))
        ('⦾', (7, "Circled White Bullet", "U+29BE"))
        ('⦿', (7, "Circled Bullet", "U+29BF"))
        ('⧀', (2, "Circled Less-Than", "U+29C0"))
        ('⧁', (2, "Circled Greater-Than", "U+29C1"))
        ('⧂', (7, "Circle with Small Circle to the Right", "U+29C2"))
        ('⧃', (7, "Circle with Two Horizontal Strokes to the Right", "U+29C3"))
        ('⧄', (7, "Squared Rising Diagonal Slash", "U+29C4"))
        ('⧅', (7, "Squared Falling Diagonal Slash", "U+29C5"))
        ('⧆', (7, "Squared Asterisk", "U+29C6"))
        ('⧇', (7, "Squared Small Circle", "U+29C7"))
        ('⧈', (7, "Squared Square", "U+29C8"))
        ('⧉', (7, "Two Joined Squares", "U+29C9"))
        ('⧊', (7, "Triangle with Dot Above", "U+29CA"))
        ('⧋', (7, "Triangle with Underbar", "U+29CB"))
        ('⧌', (7, "S In Triangle", "U+29CC"))
        ('⧍', (7, "Triangle with Serifs at Bottom", "U+29CD"))
        ('⧎', (2, "Right Triangle Above Left Triangle", "U+29CE"))
        ('⧏', (2, "Left Triangle Beside Vertical Bar", "U+29CF"))
        ('⧐', (2, "Vertical Bar Beside Right Triangle", "U+29D0"))
        ('⧑', (2, "Bowtie with Left Half Black", "U+29D1"))
        ('⧒', (2, "Bowtie with Right Half Black", "U+29D2"))
        ('⧓', (2, "Black Bowtie", "U+29D3"))
        ('⧔', (2, "Times with Left Half Black", "U+29D4"))
        ('⧕', (2, "Times with Right Half Black", "U+29D5"))
        ('⧖', (7, "White Hourglass", "U+29D6"))
        ('⧗', (7, "Black Hourglass", "U+29D7"))
        ('⧜', (8, "Incomplete Infinity", "U+29DC"))
        ('⧝', (8, "Tie Over Infinity", "U+29DD"))
        ('⧞', (2, "Infinity Negated with Vertical Bar", "U+29DE"))
        ('⧟', (2, "Double-Ended Multimap", "U+29DF"))
        ('⧠', (7, "Square with Contoured Outline", "U+29E0"))
        ('⧡', (2, "Increases As", "U+29E1"))
        ('⧢', (2, "Shuffle Product", "U+29E2"))
        ('⧣', (2, "Equals Sign and Slanted Parallel", "U+29E3"))
        ('⧤', (2, "Equals Sign and Slanted Parallel with Tilde Above", "U+29E4"))
        ('⧥', (2, "Identical to and Slanted Parallel", "U+29E5"))
        ('⧦', (2, "Gleich Stark", "U+29E6"))
        ('⧧', (2, "Thermodynamic", "U+29E7"))
        ('⧨', (7, "Down-Pointing Triangle with Left Half Black", "U+29E8"))
        ('⧩', (7, "Down-Pointing Triangle with Right Half Black", "U+29E9"))
        ('⧪', (2, "Black Diamond with Down Arrow", "U+29EA"))
        ('⧫', (7, "Black Lozenge", "U+29EB"))
        ('⧬', (2, "White Circle with Down Arrow", "U+29EC"))
        ('⧭', (2, "Black Circle with Down Arrow", "U+29ED"))
        ('⧮', (7, "Error-Barred White Square", "U+29EE"))
        ('⧯', (7, "Error-Barred Black Square", "U+29EF"))
        ('⧰', (7, "Error-Barred White Diamond", "U+29F0"))
        ('⧱', (7, "Error-Barred Black Diamond", "U+29F1"))
        ('⧲', (7, "Error-Barred White Circle", "U+29F2"))
        ('⧳', (7, "Error-Barred Black Circle", "U+29F3"))
        ('⧴', (2, "Rule-Delayed", "U+29F4"))
        ('⧵', (7, "Reverse Solidus Operator", "U+29F5"))
        ('⧶', (7, "Solidus with Overbar", "U+29F6"))
        ('⧷', (7, "Reverse Solidus with Horizontal Stroke", "U+29F7"))
        ('⧸', (7, "Big Solidus", "U+29F8"))
        ('⧹', (7, "Big Reverse Solidus", "U+29F9"))
        ('⧺', (2, "Double Plus", "U+29FA"))
        ('⧻', (2, "Triple Plus", "U+29FB"))
        ('⧾', (2, "Tiny", "U+29FE"))
        ('⧿', (2, "Miny", "U+29FF"))
        ('⨀', (4, "N-Ary Circled Dot Operator", "U+2A00"))
        ('⨁', (6, "N-Ary Circled Plus Operator", "U+2A01"))
        ('⨂', (4, "N-Ary Circled Times Operator", "U+2A02"))
        ('⨃', (4, "N-Ary Union Operator with Dot", "U+2A03"))
        ('⨄', (6, "N-Ary Union Operator with Plus", "U+2A04"))
        ('⨅', (4, "N-Ary Square Intersection Operator", "U+2A05"))
        ('⨆', (4, "N-Ary Square Union Operator", "U+2A06"))
        ('⨇', (2, "Two Logical and Operator", "U+2A07"))
        ('⨈', (2, "Two Logical or Operator", "U+2A08"))
        ('⨉', (4, "N-Ary Times Operator", "U+2A09"))
        ('⨊', (4, "Modulo Two Sum", "U+2A0A"))
        ('⨋', (4, "Summation with Integral", "U+2A0B"))
        ('⨌', (4, "Quadruple Integral Operator", "U+2A0C"))
        ('⨍', (4, "Finite Part Integral", "U+2A0D"))
        ('⨎', (4, "Integral with Double Stroke", "U+2A0E"))
        ('⨏', (4, "Integral Average with Slash", "U+2A0F"))
        ('⨐', (4, "Circulation Function", "U+2A10"))
        ('⨑', (4, "Anticlockwise Integration", "U+2A11"))
        ('⨒', (4, "Line Integration with Rectangular Path Around Pole", "U+2A12"))
        ('⨓', (4, "Line Integration with Semicircular Path Around Pole", "U+2A13"))
        ('⨔', (4, "Line Integration Not Including the Pole", "U+2A14"))
        ('⨕', (4, "Integral Around A Point Operator", "U+2A15"))
        ('⨖', (4, "Quaternion Integral Operator", "U+2A16"))
        ('⨗', (2, "Integral with Leftwards Arrow with Hook", "U+2A17"))
        ('⨘', (4, "Integral with Times Sign", "U+2A18"))
        ('⨙', (4, "Integral with Intersection", "U+2A19"))
        ('⨚', (4, "Integral with Union", "U+2A1A"))
        ('⨛', (4, "Integral with Overbar", "U+2A1B"))
        ('⨜', (4, "Integral with Underbar", "U+2A1C"))
        ('⨝', (2, "Join", "U+2A1D"))
        ('⨞', (2, "Large Left Triangle Operator", "U+2A1E"))
        ('⨟', (2, "Z Notation Schema Composition", "U+2A1F"))
        ('⨠', (2, "Z Notation Schema Piping", "U+2A20"))
        ('⨡', (2, "Z Notation Schema Projection", "U+2A21"))
        ('⨢', (2, "Plus Sign with Small Circle Above", "U+2A22"))
        ('⨣', (2, "Plus Sign with Circumflex Accent Above", "U+2A23"))
        ('⨤', (2, "Plus Sign with Tilde Above", "U+2A24"))
        ('⨥', (2, "Plus Sign with Dot Below", "U+2A25"))
        ('⨦', (2, "Plus Sign with Tilde Below", "U+2A26"))
        ('⨧', (2, "Plus Sign with Subscript Two", "U+2A27"))
        ('⨨', (2, "Plus Sign with Black Triangle", "U+2A28"))
        ('⨩', (2, "Minus Sign with Comma Above", "U+2A29"))
        ('⨪', (2, "Minus Sign with Dot Below", "U+2A2A"))
        ('⨫', (2, "Minus Sign with Falling Dots", "U+2A2B"))
        ('⨬', (2, "Minus Sign with Rising Dots", "U+2A2C"))
        ('⨭', (2, "Plus Sign In Left Half Circle", "U+2A2D"))
        ('⨮', (2, "Plus Sign In Right Half Circle", "U+2A2E"))
        ('⨯', (2, "Vector or Cross Product", "U+2A2F"))
        ('⨰', (2, "Multiplication Sign with Dot Above", "U+2A30"))
        ('⨱', (2, "Multiplication Sign with Underbar", "U+2A31"))
        ('⨲', (2, "Semidirect Product with Bottom Closed", "U+2A32"))
        ('⨳', (2, "Smash Product", "U+2A33"))
        ('⨴', (2, "Multiplication Sign In Left Half Circle", "U+2A34"))
        ('⨵', (2, "Multiplication Sign In Right Half Circle", "U+2A35"))
        ('⨶', (2, "Circled Multiplication Sign with Circumflex Accent", "U+2A36"))
        ('⨷', (2, "Multiplication Sign In Double Circle", "U+2A37"))
        ('⨸', (2, "Circled Division Sign", "U+2A38"))
        ('⨹', (2, "Plus Sign In Triangle", "U+2A39"))
        ('⨺', (2, "Minus Sign In Triangle", "U+2A3A"))
        ('⨻', (2, "Multiplication Sign In Triangle", "U+2A3B"))
        ('⨼', (7, "Interior Product", "U+2A3C"))
        ('⨽', (7, "Righthand Interior Product", "U+2A3D"))
        ('⨾', (2, "Z Notation Relational Composition", "U+2A3E"))
        ('⨿', (6, "Amalgamation or Coproduct", "U+2A3F"))
        ('⩀', (2, "Intersection with Dot", "U+2A40"))
        ('⩁', (2, "Union with Minus Sign", "U+2A41"))
        ('⩂', (2, "Union with Overbar", "U+2A42"))
        ('⩃', (2, "Intersection with Overbar", "U+2A43"))
        ('⩄', (2, "Intersection with Logical And", "U+2A44"))
        ('⩅', (2, "Union with Logical Or", "U+2A45"))
        ('⩆', (2, "Union Above Intersection", "U+2A46"))
        ('⩇', (2, "Intersection Above Union", "U+2A47"))
        ('⩈', (2, "Union Above Bar Above Intersection", "U+2A48"))
        ('⩉', (2, "Intersection Above Bar Above Union", "U+2A49"))
        ('⩊', (7, "Union Beside and Joined with Union", "U+2A4A"))
        ('⩋', (7, "Intersection Beside and Joined with Intersection", "U+2A4B"))
        ('⩌', (2, "Closed Union with Serifs", "U+2A4C"))
        ('⩍', (2, "Closed Intersection with Serifs", "U+2A4D"))
        ('⩎', (6, "Double Square Intersection", "U+2A4E"))
        ('⩏', (6, "Double Square Union", "U+2A4F"))
        ('⩐', (2, "Closed Union with Serifs and Smash Product", "U+2A50"))
        ('⩑', (2, "Logical and with Dot Above", "U+2A51"))
        ('⩒', (2, "Logical or with Dot Above", "U+2A52"))
        ('⩓', (2, "Double Logical And", "U+2A53"))
        ('⩔', (2, "Double Logical Or", "U+2A54"))
        ('⩕', (2, "Two Intersecting Logical And", "U+2A55"))
        ('⩖', (2, "Two Intersecting Logical Or", "U+2A56"))
        ('⩗', (2, "Sloping Large Or", "U+2A57"))
        ('⩘', (2, "Sloping Large And", "U+2A58"))
        ('⩙', (2, "Logical or Overlapping Logical And", "U+2A59"))
        ('⩚', (2, "Logical and with Middle Stem", "U+2A5A"))
        ('⩛', (2, "Logical or with Middle Stem", "U+2A5B"))
        ('⩜', (2, "Logical and with Horizontal Dash", "U+2A5C"))
        ('⩝', (2, "Logical or with Horizontal Dash", "U+2A5D"))
        ('⩞', (2, "Logical and with Double Overbar", "U+2A5E"))
        ('⩟', (2, "Logical and with Underbar", "U+2A5F"))
        ('⩠', (2, "Logical and with Double Underbar", "U+2A60"))
        ('⩡', (2, "Small Vee with Underbar", "U+2A61"))
        ('⩢', (2, "Logical or with Double Overbar", "U+2A62"))
        ('⩣', (2, "Logical or with Double Underbar", "U+2A63"))
        ('⩤', (2, "Z Notation Domain Antirestriction", "U+2A64"))
        ('⩥', (2, "Z Notation Range Antirestriction", "U+2A65"))
        ('⩦', (2, "Equals Sign with Dot Below", "U+2A66"))
        ('⩧', (2, "Identical with Dot Above", "U+2A67"))
        ('⩨', (2, "Triple Horizontal Bar with Double Vertical Stroke", "U+2A68"))
        ('⩩', (2, "Triple Horizontal Bar with Triple Vertical Stroke", "U+2A69"))
        ('⩪', (2, "Tilde Operator with Dot Above", "U+2A6A"))
        ('⩫', (2, "Tilde Operator with Rising Dots", "U+2A6B"))
        ('⩬', (2, "Similar Minus Similar", "U+2A6C"))
        ('⩭', (2, "Congruent with Dot Above", "U+2A6D"))
        ('⩮', (2, "Equals with Asterisk", "U+2A6E"))
        ('⩯', (2, "Almost Equal to with Circumflex Accent", "U+2A6F"))
        ('⩰', (2, "Approximately Equal or Equal To", "U+2A70"))
        ('⩱', (2, "Equals Sign Above Plus Sign", "U+2A71"))
        ('⩲', (2, "Plus Sign Above Equals Sign", "U+2A72"))
        ('⩳', (2, "Equals Sign Above Tilde Operator", "U+2A73"))
        ('⩴', (2, "Double Colon Equal", "U+2A74"))
        ('⩵', (0, "Two Consecutive Equals Signs", "U+2A75"))
        ('⩶', (0, "Three Consecutive Equals Signs", "U+2A76"))
        ('⩷', (2, "Equals Sign with Two Dots Above and Two Dots Below", "U+2A77"))
        ('⩸', (2, "Equivalent with Four Dots Above", "U+2A78"))
        ('⩹', (2, "Less-Than with Circle Inside", "U+2A79"))
        ('⩺', (2, "Greater-Than with Circle Inside", "U+2A7A"))
        ('⩻', (2, "Less-Than with Question Mark Above", "U+2A7B"))
        ('⩼', (2, "Greater-Than with Question Mark Above", "U+2A7C"))
        ('⩽', (2, "Less-Than or Slanted Equal To", "U+2A7D"))
        ('⩾', (2, "Greater-Than or Slanted Equal To", "U+2A7E"))
        ('⩿', (2, "Less-Than or Slanted Equal to with Dot Inside", "U+2A7F"))
        ('⪀', (2, "Greater-Than or Slanted Equal to with Dot Inside", "U+2A80"))
        ('⪁', (2, "Less-Than or Slanted Equal to with Dot Above", "U+2A81"))
        ('⪂', (2, "Greater-Than or Slanted Equal to with Dot Above", "U+2A82"))
        ('⪃', (2, "Less-Than or Slanted Equal to with Dot Above Right", "U+2A83"))
        ('⪄', (2, "Greater-Than or Slanted Equal to with Dot Above Left", "U+2A84"))
        ('⪅', (2, "Less-Than or Approximate", "U+2A85"))
        ('⪆', (2, "Greater-Than or Approximate", "U+2A86"))
        ('⪇', (2, "Less-Than and Single-Line Not Equal To", "U+2A87"))
        ('⪈', (2, "Greater-Than and Single-Line Not Equal To", "U+2A88"))
        ('⪉', (2, "Less-Than and Not Approximate", "U+2A89"))
        ('⪊', (2, "Greater-Than and Not Approximate", "U+2A8A"))
        ('⪋', (2, "Less-Than Above Double-Line Equal Above Greater-Than", "U+2A8B"))
        ('⪌', (2, "Greater-Than Above Double-Line Equal Above Less-Than", "U+2A8C"))
        ('⪍', (2, "Less-Than Above Similar or Equal", "U+2A8D"))
        ('⪎', (2, "Greater-Than Above Similar or Equal", "U+2A8E"))
        ('⪏', (2, "Less-Than Above Similar Above Greater-Than", "U+2A8F"))
        ('⪐', (2, "Greater-Than Above Similar Above Less-Than", "U+2A90"))
        ('⪑', (2, "Less-Than Above Greater-Than Above Double-Line Equal", "U+2A91"))
        ('⪒', (2, "Greater-Than Above Less-Than Above Double-Line Equal", "U+2A92"))
        ('⪓', (2, "Less-Than Above Slanted Equal Above Greater-Than Above Slanted Equal", "U+2A93"))
        ('⪔', (2, "Greater-Than Above Slanted Equal Above Less-Than Above Slanted Equal", "U+2A94"))
        ('⪕', (2, "Slanted Equal to or Less-Than", "U+2A95"))
        ('⪖', (2, "Slanted Equal to or Greater-Than", "U+2A96"))
        ('⪗', (2, "Slanted Equal to or Less-Than with Dot Inside", "U+2A97"))
        ('⪘', (2, "Slanted Equal to or Greater-Than with Dot Inside", "U+2A98"))
        ('⪙', (2, "Double-Line Equal to or Less-Than", "U+2A99"))
        ('⪚', (2, "Double-Line Equal to or Greater-Than", "U+2A9A"))
        ('⪛', (2, "Double-Line Slanted Equal to or Less-Than", "U+2A9B"))
        ('⪜', (2, "Double-Line Slanted Equal to or Greater-Than", "U+2A9C"))
        ('⪝', (2, "Similar or Less-Than", "U+2A9D"))
        ('⪞', (2, "Similar or Greater-Than", "U+2A9E"))
        ('⪟', (2, "Similar Above Less-Than Above Equals Sign", "U+2A9F"))
        ('⪠', (2, "Similar Above Greater-Than Above Equals Sign", "U+2AA0"))
        ('⪡', (2, "Double Nested Less-Than", "U+2AA1"))
        ('⪢', (2, "Double Nested Greater-Than", "U+2AA2"))
        ('⪣', (2, "Double Nested Less-Than with Underbar", "U+2AA3"))
        ('⪤', (2, "Greater-Than Overlapping Less-Than", "U+2AA4"))
        ('⪥', (2, "Greater-Than Beside Less-Than", "U+2AA5"))
        ('⪦', (2, "Less-Than Closed By Curve", "U+2AA6"))
        ('⪧', (2, "Greater-Than Closed By Curve", "U+2AA7"))
        ('⪨', (2, "Less-Than Closed By Curve Above Slanted Equal", "U+2AA8"))
        ('⪩', (2, "Greater-Than Closed By Curve Above Slanted Equal", "U+2AA9"))
        ('⪪', (2, "Smaller Than", "U+2AAA"))
        ('⪫', (2, "Larger Than", "U+2AAB"))
        ('⪬', (2, "Smaller Than or Equal To", "U+2AAC"))
        ('⪭', (2, "Larger Than or Equal To", "U+2AAD"))
        ('⪮', (2, "Equals Sign with Bumpy Above", "U+2AAE"))
        ('⪯', (2, "Precedes Above Single-Line Equals Sign", "U+2AAF"))
        ('⪰', (2, "Succeeds Above Single-Line Equals Sign", "U+2AB0"))
        ('⪱', (2, "Precedes Above Single-Line Not Equal To", "U+2AB1"))
        ('⪲', (2, "Succeeds Above Single-Line Not Equal To", "U+2AB2"))
        ('⪳', (2, "Precedes Above Equals Sign", "U+2AB3"))
        ('⪴', (2, "Succeeds Above Equals Sign", "U+2AB4"))
        ('⪵', (2, "Precedes Above Not Equal To", "U+2AB5"))
        ('⪶', (2, "Succeeds Above Not Equal To", "U+2AB6"))
        ('⪷', (2, "Precedes Above Almost Equal To", "U+2AB7"))
        ('⪸', (2, "Succeeds Above Almost Equal To", "U+2AB8"))
        ('⪹', (2, "Precedes Above Not Almost Equal To", "U+2AB9"))
        ('⪺', (2, "Succeeds Above Not Almost Equal To", "U+2ABA"))
        ('⪻', (2, "Double Precedes", "U+2ABB"))
        ('⪼', (2, "Double Succeeds", "U+2ABC"))
        ('⪽', (2, "Subset with Dot", "U+2ABD"))
        ('⪾', (2, "Superset with Dot", "U+2ABE"))
        ('⪿', (2, "Subset with Plus Sign Below", "U+2ABF"))
        ('⫀', (2, "Superset with Plus Sign Below", "U+2AC0"))
        ('⫁', (2, "Subset with Multiplication Sign Below", "U+2AC1"))
        ('⫂', (2, "Superset with Multiplication Sign Below", "U+2AC2"))
        ('⫃', (2, "Subset of or Equal to with Dot Above", "U+2AC3"))
        ('⫄', (2, "Superset of or Equal to with Dot Above", "U+2AC4"))
        ('⫅', (2, "Subset of Above Equals Sign", "U+2AC5"))
        ('⫆', (2, "Superset of Above Equals Sign", "U+2AC6"))
        ('⫇', (2, "Subset of Above Tilde Operator", "U+2AC7"))
        ('⫈', (2, "Superset of Above Tilde Operator", "U+2AC8"))
        ('⫉', (2, "Subset of Above Almost Equal To", "U+2AC9"))
        ('⫊', (2, "Superset of Above Almost Equal To", "U+2ACA"))
        ('⫋', (2, "Subset of Above Not Equal To", "U+2ACB"))
        ('⫌', (2, "Superset of Above Not Equal To", "U+2ACC"))
        ('⫍', (2, "Square Left Open Box Operator", "U+2ACD"))
        ('⫎', (2, "Square Right Open Box Operator", "U+2ACE"))
        ('⫏', (2, "Closed Subset", "U+2ACF"))
        ('⫐', (2, "Closed Superset", "U+2AD0"))
        ('⫑', (2, "Closed Subset or Equal To", "U+2AD1"))
        ('⫒', (2, "Closed Superset or Equal To", "U+2AD2"))
        ('⫓', (2, "Subset Above Superset", "U+2AD3"))
        ('⫔', (2, "Superset Above Subset", "U+2AD4"))
        ('⫕', (2, "Subset Above Subset", "U+2AD5"))
        ('⫖', (2, "Superset Above Superset", "U+2AD6"))
        ('⫗', (2, "Superset Beside Subset", "U+2AD7"))
        ('⫘', (2, "Superset Beside and Joined By Dash with Subset", "U+2AD8"))
        ('⫙', (2, "Element of Opening Downwards", "U+2AD9"))
        ('⫚', (6, "Pitchfork with Tee Top", "U+2ADA"))
        ('⫛', (2, "Transversal Intersection", "U+2ADB"))
        ('⫝̸', (7, "Forking", "U+2ADC"))
        ('⫝', (7, "Nonforking", "U+2ADD"))
        ('⫞', (7, "Short Left Tack", "U+2ADE"))
        ('⫟', (7, "Short Down Tack", "U+2ADF"))
        ('⫠', (7, "Short Up Tack", "U+2AE0"))
        ('⫡', (7, "Perpendicular with S", "U+2AE1"))
        ('⫢', (7, "Vertical Bar Triple Right Turnstile", "U+2AE2"))
        ('⫣', (7, "Double Vertical Bar Left Turnstile", "U+2AE3"))
        ('⫤', (7, "Vertical Bar Double Left Turnstile", "U+2AE4"))
        ('⫥', (7, "Double Vertical Bar Double Left Turnstile", "U+2AE5"))
        ('⫦', (7, "Long Dash from Left Member of Double Vertical", "U+2AE6"))
        ('⫧', (7, "Short Down Tack with Overbar", "U+2AE7"))
        ('⫨', (7, "Short Up Tack with Underbar", "U+2AE8"))
        ('⫩', (7, "Short Up Tack Above Short Down Tack", "U+2AE9"))
        ('⫪', (7, "Double Down Tack", "U+2AEA"))
        ('⫫', (7, "Double Up Tack", "U+2AEB"))
        ('⫬', (4, "Double Stroke Not Sign", "U+2AEC"))
        ('⫭', (4, "Reversed Double Stroke Not Sign", "U+2AED"))
        ('⫮', (2, "Does Not Divide with Reversed Negation Slash", "U+2AEE"))
        ('⫯', (7, "Vertical Line with Circle Above", "U+2AEF"))
        ('⫰', (7, "Vertical Line with Circle Below", "U+2AF0"))
        ('⫱', (7, "Down Tack with Circle Below", "U+2AF1"))
        ('⫲', (2, "Parallel with Horizontal Stroke", "U+2AF2"))
        ('⫳', (2, "Parallel with Tilde Operator", "U+2AF3"))
        ('⫴', (2, "Triple Vertical Bar Binary Relation", "U+2AF4"))
        ('⫵', (2, "Triple Vertical Bar with Horizontal Stroke", "U+2AF5"))
        ('⫶', (2, "Triple Colon Operator", "U+2AF6"))
        ('⫷', (2, "Triple Nested Less-Than", "U+2AF7"))
        ('⫸', (2, "Triple Nested Greater-Than", "U+2AF8"))
        ('⫹', (2, "Double-Line Slanted Less-Than or Equal To", "U+2AF9"))
        ('⫺', (2, "Double-Line Slanted Greater-Than or Equal To", "U+2AFA"))
        ('⫻', (2, "Triple Solidus Binary Relation", "U+2AFB"))
        ('⫼', (2, "Large Triple Vertical Bar Operator", "U+2AFC"))
        ('⫽', (2, "Double Solidus Operator", "U+2AFD"))
        ('⫾', (2, "White Vertical Bar", "U+2AFE"))
        ('⫿', (4, "N-Ary White Vertical Bar", "U+2AFF"))
        ('⬰', (2, "Left Arrow with Small Circle", "U+2B30"))
        ('⬱', (2, "Three Leftwards Arrows", "U+2B31"))
        ('⬲', (2, "Left Arrow with Circled Plus", "U+2B32"))
        ('⬳', (2, "Long Leftwards Squiggle Arrow", "U+2B33"))
        ('⬴', (2, "Leftwards Two-Headed Arrow with Vertical Stroke", "U+2B34"))
        ('⬵', (2, "Leftwards Two-Headed Arrow with Double Vertical Stroke", "U+2B35"))
        ('⬶', (2, "Leftwards Two-Headed Arrow from Bar", "U+2B36"))
        ('⬷', (2, "Leftwards Two-Headed Triple Dash Arrow", "U+2B37"))
        ('⬸', (2, "Leftwards Arrow with Dotted Stem", "U+2B38"))
        ('⬹', (2, "Leftwards Arrow with Tail with Vertical Stroke", "U+2B39"))
        ('⬺', (2, "Leftwards Arrow with Tail with Double Vertical Stroke", "U+2B3A"))
        ('⬻', (2, "Leftwards Two-Headed Arrow with Tail", "U+2B3B"))
        ('⬼', (2, "Leftwards Two-Headed Arrow with Tail with Vertical Stroke", "U+2B3C"))
        ('⬽', (2, "Leftwards Two-Headed Arrow with Tail with Double Vertical Stroke", "U+2B3D"))
        ('⬾', (2, "Leftwards Arrow Through X", "U+2B3E"))
        ('⬿', (2, "Wave Arrow Pointing Directly Left", "U+2B3F"))
        ('⭀', (2, "Equals Sign Above Leftwards Arrow", "U+2B40"))
        ('⭁', (2, "Reverse Tilde Operator Above Leftwards Arrow", "U+2B41"))
        ('⭂', (2, "Leftwards Arrow Above Reverse Almost Equal To", "U+2B42"))
        ('⭃', (2, "Rightwards Arrow Through Greater-Than", "U+2B43"))
        ('⭄', (2, "Rightwards Arrow Through Superset", "U+2B44"))
        ('⭇', (2, "Reverse Tilde Operator Above Rightwards Arrow", "U+2B47"))
        ('⭈', (2, "Rightwards Arrow Above Reverse Almost Equal To", "U+2B48"))
        ('⭉', (2, "Tilde Operator Above Leftwards Arrow", "U+2B49"))
        ('⭊', (2, "Leftwards Arrow Above Almost Equal To", "U+2B4A"))
        ('⭋', (2, "Leftwards Arrow Above Reverse Tilde Operator", "U+2B4B"))
        ('⭌', (2, "Rightwards Arrow Above Reverse Tilde Operator", "U+2B4C"))
        ('﬩', (2, "Hebrew Letter Alternative Plus Sign", "U+FB29"))
        ('﹢', (2, "Small Plus Sign", "U+FE62"))
        ('﹤', (2, "Small Less-Than Sign", "U+FE64"))
        ('﹥', (2, "Small Greater-Than Sign", "U+FE65"))
        ('﹦', (2, "Small Equals Sign", "U+FE66"))
        ('＋', (2, "Fullwidth Plus Sign", "U+FF0B"))
        ('＜', (2, "Fullwidth Less-Than Sign", "U+FF1C"))
        ('＝', (2, "Fullwidth Equals Sign", "U+FF1D"))
        ('＞', (2, "Fullwidth Greater-Than Sign", "U+FF1E"))
        ('｜', (7, "Fullwidth Vertical Line", "U+FF5C"))
        ('～', (7, "Fullwidth Tilde", "U+FF5E"))
        ('￢', (4, "Fullwidth Not Sign", "U+FFE2"))
        ('￩', (2, "Halfwidth Leftwards Arrow", "U+FFE9"))
        ('￪', (2, "Halfwidth Upwards Arrow", "U+FFEA"))
        ('￫', (2, "Halfwidth Rightwards Arrow", "U+FFEB"))
        ('￬', (2, "Halfwidth Downwards Arrow", "U+FFEC"))
    ]

let isInfix c =
    match mathSymbols.TryGetValue(c) with
    | (true, (a,_,_)) -> (a &&& 2) = 2
    | _ -> false

let isPrefix c =
    match mathSymbols.TryGetValue(c) with
    | (true, (a,_,_)) -> (a &&& 4) = 4
    | _ -> false

let isPostfix c =
    match mathSymbols.TryGetValue(c) with
    | (true, (a,_,_)) -> (a &&& 1) = 1
    | _ -> false

let isObject c =
    match mathSymbols.TryGetValue(c) with
    | (true, (a,_,_)) -> (a &&& 8) = 8
    | _ -> false


let fixChar predicate expected = 
    let prefixChar = 
        satisfy predicate <|> fail ("Expecting: " + expected)
    many1Chars prefixChar

let infixMathSymbols: Parser<string,unit> = fixChar isInfix "<infix symbol>"

let postfixMathSymbols: Parser<string,unit> = fixChar isPostfix "<postfix symbol>" 

let prefixMathSymbols: Parser<string,unit> = fixChar isPrefix "<prefix symbol>"

let objectMathSymbols: Parser<string,unit> = fixChar isObject "<object symbol>"


(* Strint primitives *)
[<Literal>]
let PrimArgInf = "ainf"
[<Literal>]
let PrimArg = "arg"
[<Literal>]
let PrimArgL = "argument"
[<Literal>]
let PrimAssertion = "assertion"
[<Literal>]
let PrimAssignment = "assignment statement"
[<Literal>]
let PrimArgInfAssume = "assume argument inference"
[<Literal>]
let PrimArgInfDerive = "derived argument inference"
[<Literal>]
let PrimArgInfRevoke = "revoke argument inference"
[<Literal>]
let PrimArgInfTrivial = "trivial argument inference"
[<Literal>]
let PrimCases = "cases statement"
[<Literal>]
let PrimCaseElse = "else case statement"
[<Literal>]
let PrimCaseSingle = "single case statement"
[<Literal>]
let PrimClass = "def cl"
[<Literal>]
let PrimClassL = "class definition"
[<Literal>]
let PrimConjunction = "conjunction"
[<Literal>]
let PrimDecrement = "decr"
[<Literal>]
let PrimDecrementL = "decrement statement"
[<Literal>]
let PrimDisjunction = "disjunction"
[<Literal>]
let PrimEquality = "="
[<Literal>]
let PrimEqualityL = "equality"
[<Literal>]
let PrimEquivalence = "equivalence"
[<Literal>]
let PrimExclusiveOr = "exclusive disjunction"
[<Literal>]
let PrimExtension = "def ext"
[<Literal>]
let PrimExtensionL = "extension definition"
[<Literal>]
let PrimExtensionObj = "extension object"
[<Literal>]
let PrimForInStmt = "for in statement"
[<Literal>]
let PrimForInStmtDomain = "for in statement's domain"
[<Literal>]
let PrimForInStmtEntity = "for in statement's entity"
[<Literal>]
let PrimFuncionalTerm = "def func"
[<Literal>]
let PrimFuncionalTermL = "functional term definition"
[<Literal>]
let PrimInstance = "inst" 
[<Literal>]
let PrimInstanceL = "instance"
[<Literal>]
let PrimIntrinsicFunc = "intrinsic functional term"
[<Literal>]
let PrimIntrinsicInd = "intrinsic index"
[<Literal>]
let PrimIntrinsicObj = "intrinsic object"
[<Literal>]
let PrimIntrinsicPred = "intrinsic predicate"
[<Literal>]
let PrimIntrinsicTpl = "intrinsic template"
[<Literal>]
let PrimIntrinsicUndef = "intrinsic undefined"
[<Literal>]
let PrimIsOperator = "is operator"
[<Literal>]
let PrimImplication = "implication"
[<Literal>]
let PrimJIByAx = "justification by axiom"
[<Literal>]
let PrimJIByCor = "justification by corollary"
[<Literal>]
let PrimJIByDef = "justification by definition"
[<Literal>]
let PrimJIByDefVar = "justification by variable definition"
[<Literal>]
let PrimJIByInf = "justification by rule of inference"
[<Literal>]
let PrimJIByProofArgument = "justification by argument in another proof"
[<Literal>]
let PrimJIByRefArgument = "justification by argument reference"
[<Literal>]
let PrimJIByTheoremLikeStmt = "justification by theorem-like statement"
[<Literal>]
let PrimJustification = "just"
[<Literal>]
let PrimJustificationL = "justification"
[<Literal>]
let PrimLanguage = "lang"
[<Literal>]
let PrimLanguageL = "language"
[<Literal>]
let PrimMandatoryFunctionalTerm = "mfunc"
[<Literal>]
let PrimMandatoryFunctionalTermL = "functional term property"
[<Literal>]
let PrimMandatoryPredicate = "mpred"
[<Literal>]
let PrimMandatoryPredicateL = "predicate property"
[<Literal>]
let PrimMapCases = "mapcases statement"
[<Literal>]
let PrimMapCaseElse = "else mapcase statement"
[<Literal>]
let PrimMapCaseSingle = "single mapcase statement"
[<Literal>]
let PrimMapping = "map"
[<Literal>]
let PrimMappingL = "mapping"
[<Literal>]
let PrimNegation = "negation"
[<Literal>]
let PrimOptionalFunctionalTerm = "ofunc"
[<Literal>]
let PrimOptionalFunctionalTermL = "optional functional term property"
[<Literal>]
let PrimOptionalPredicate = "opred"
[<Literal>]
let PrimOptionalPredicateL = "optional predicate property"
[<Literal>]
let PrimPredicate = "def pred"
[<Literal>]
let PrimPredicateL = "predicate definition"
[<Literal>]
let PrimQuantor = "qtr"
[<Literal>]
let PrimQuantorAll = "all quantor"
[<Literal>]
let PrimQuantorExists = "exists quantor"
[<Literal>]
let PrimQuantorExistsN = "exists n times quantor"
[<Literal>]
let PrimReturn = "return statement"
[<Literal>]
let PrimRef = "ref"
[<Literal>]
let PrimRefL = "reference"
[<Literal>]
let PrimRuleOfInference = "rule of inference"
[<Literal>]
let PrimRoot = "root"
[<Literal>]
let PrimStmt = "stmt"
[<Literal>]
let PrimStmtL = "statement"
[<Literal>]
let PrimTheory = "th"
[<Literal>]
let PrimTheoryL = "theory"
[<Literal>]
let PrimTranslation = "trsl"
[<Literal>]
let PrimTranslationL = "translation"
[<Literal>]
let PrimVariable = "var"
[<Literal>]
let PrimVariableL = "variable"
[<Literal>]
let PrimVariableMany = "*var"
[<Literal>]
let PrimVariableManyL = "zero-or-more variable"
[<Literal>]
let PrimVariableMany1 = "+var"
[<Literal>]
let PrimVariableMany1L = "one-or-more variable"
