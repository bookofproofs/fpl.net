/// This module contains the FPL parser conbinators producing an abstract syntax tree out of a given FPL code.
module FplParsing.Basic
open System.Text.RegularExpressions
open FParsec
open FplPrimitives
open FplParsing.Debug
(* MIT License

Copyright (c) 2026+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)

// ============================================================================
// Whitespace control
// ============================================================================

// ============================================================================
// Whitespace control
// ============================================================================

let whiteSpaces = anyOf " \t\r\n"
// No whitespace allowed at this point

let NW : Parser<unit,unit> =
    notFollowedBy (skipMany1 whiteSpaces) <?> "<no whitespace>" <!> "NW"

// At least one whitespace required
let SW : Parser<unit,unit> =
    skipMany1 whiteSpaces >>% () <?> "<significant whitespace>" <!> "SW"

// Optional whitespace (for comma lists etc.)
let IW : Parser<unit,unit> =
    skipMany whiteSpaces >>% () <?> "<whitespace>" <!> "IW"

let attemptSW = SW <|> (IW .>> attempt (lookAhead (choice [skipChar '('; skipChar ')'; skipChar '{'; skipChar ','; skipChar ';'; skipChar '[' ]))) <!> "atemptSW"

// ============================================================================
// Operator sets
// ============================================================================

// Prefix operators: - ~ #
let prefixMathSymbols : Parser<string,unit> =
    many1Satisfy (fun c -> "%&'*+-/؆؇℘⅀⅋∁∂∆∇∏∐∑−√∛∜∫∬∭∮∯∰∱∲∳∸∺∻∼∽∾≁⊢⊣⊤⊥⊦⊧⊨⊩⊪⊫⊬⊭⊮⊯⊰⊱⊹⊺⋀⋁⋂⋃⋄⋅⋆⋇⋈⋉⋊⋋⋌◸◹◺◻◼◽◾◿♯⟀⟁⟉⟊⟋⟌⟍⟐⟑⟓⟔⟕⟖⟗⟘⟙⟚⟛⟜⟝⟞⟟⟠⟡⟲⟳⤫⤬⥊⥋⥌⥍⥎⥏⥐⥑⥼⥽⥾⥿⦀⦁⦂⦙⦚⦢⦣⦤⦥⦦⦧⦰⦱⦲⦵⦶⦷⦸⦹⦺⦻⦼⦾⦿⧂⧃⧄⧅⧆⧇⧈⧉⧊⧋⧌⧍⧖⧗⧠⧨⧩⧫⧮⧯⧰⧱⧲⧳⧵⧶⧷⧸⧹⨀⨁⨂⨃⨄⨅⨆⨉⨊⨋⨌⨍⨎⨏⨐⨑⨒⨓⨔⨕⨖⨘⨙⨚⨛⨜⨼⨽⨿⩊⩋⩎⩏⫚⫝̸⫝⫞⫟⫠⫡⫢⫣⫤⫥⫦⫧⫨⫩⫪⫫⫬⫭⫯⫰⫱⫿｜～￢".Contains(c))

// Postfix operators: ! ' /
let postfixMathSymbols : Parser<string,unit> =
    many1Satisfy (fun c -> "!%&'*+-/⁺⁻⁼₊₋₌⅋∸∺∻∼∽∾≁⊹⊺⋄⋅⋆⋇⋈⋉⋊⋋⋌◸◹◺◻◼◽◾◿♯⟀⟁⟉⟊⟋⟌⟍⟐⟑⟓⟔⟕⟖⟗⟘⟙⟚⟛⟜⟝⟞⟟⟠⟡⟲⟳⤫⤬⥊⥋⥌⥍⥎⥏⥐⥑⥼⥽⥾⥿⦀⦁⦂⦙⦚⦢⦣⦤⦥⦦⦧⦰⦱⦲⦵⦶⦷⦸⦹⦺⦻⦼⦾⦿⧂⧃⧄⧅⧆⧇⧈⧉⧊⧋⧌⧍⧖⧗⧠⧨⧩⧫⧮⧯⧰⧱⧲⧳⧵⧶⧷⧸⧹⨼⨽⩊⩋⫝̸⫝⫞⫟⫠⫡⫢⫣⫤⫥⫦⫧⫨⫩⫪⫫⫯⫰⫱｜～".Contains(c))

/// Taken from https://www.quanttec.com/fparsec/users-guide/looking-ahead-and-backtracking.html#parser-predicates
let resultSatisfies predicate msg (p: Parser<_,_>) : Parser<_,_> =
    let error = messageError msg
    fun stream ->
        let state = stream.State
        let reply = p stream
        if reply.Status <> Ok || predicate reply.Result then reply
        else
            stream.BacktrackTo(state) // backtrack to beginning
            Reply(Primitives.Error, error)

let tplRegex = Regex(@"^(tpl|template)(([A-Z]\w*)|\d*)$", RegexOptions.Compiled)


// Infix operators: + - * /
let infixMathSymbols : Parser<string,unit> =
    many1Satisfy (fun c -> "*+-/<=>@\^_v±×÷϶؈⁄⁒⁺⁼₊₌⅋←↑→↓↔↚↛↠↣↦↮⇎⇏⇒⇔⇴⇵⇶⇷⇸⇹⇺⇻⇼⇽⇾⇿∄∆∈∉∊∋∌∍−∓∔∕∖∗∘∙∝∣∤∥∦∧∨∩∪∴∵∶∷∸∹∺∻∼∽∾≀≁≂≃≄≅≆≇≈≉≊≋≌≍≎≏≐≑≒≓≖≗≘≙≚≛≜≞≟≠≡≢≣≤≥≦≧≨≩≪≫≬≭≮≯≰≱≲≳≴≵≶≷≸≹≺≻≼≽≾≿⊀⊁⊂⊃⊄⊅⊆⊇⊈⊉⊊⊋⊌⊍⊎⊏⊐⊑⊒⊓⊔⊕⊖⊗⊘⊙⊚⊛⊜⊝⊞⊟⊠⊡⊢⊣⊤⊥⊦⊧⊨⊩⊪⊫⊬⊭⊮⊯⊰⊱⊲⊳⊴⊵⊶⊷⊸⊹⊺⊻⊼⊽⋄⋅⋆⋇⋈⋉⋊⋋⋌⋍⋎⋏⋐⋑⋒⋓⋔⋕⋖⋗⋘⋙⋚⋛⋜⋝⋞⋟⋠⋡⋢⋣⋤⋥⋦⋧⋨⋩⋪⋫⋬⋭⋲⋳⋴⋵⋶⋷⋸⋹⋺⋻⋼⋽⋾⋿▷◁◸◹◺◻◼◽◾◿♯⟀⟁⟂⟃⟄⟇⟈⟉⟊⟋⟌⟍⟎⟏⟐⟑⟒⟓⟔⟕⟖⟗⟘⟙⟚⟛⟜⟝⟞⟟⟠⟡⟢⟣⟤⟥⟰⟱⟲⟳⟴⟵⟶⟷⟸⟹⟺⟻⟼⟽⟾⟿⤀⤁⤂⤃⤄⤅⤆⤇⤈⤉⤊⤋⤌⤍⤎⤏⤐⤑⤒⤓⤔⤕⤖⤗⤘⤙⤚⤛⤜⤝⤞⤟⤠⤡⤢⤣⤤⤥⤦⤧⤨⤩⤪⤫⤬⤭⤮⤯⤰⤱⤲⤳⤴⤵⤶⤷⤸⤹⤺⤻⤼⤽⤾⤿⥀⥁⥂⥃⥄⥅⥆⥇⥈⥉⥊⥋⥌⥍⥎⥏⥐⥑⥒⥓⥔⥕⥖⥗⥘⥙⥚⥛⥜⥝⥞⥟⥠⥡⥢⥣⥤⥥⥦⥧⥨⥩⥪⥫⥬⥭⥮⥯⥰⥱⥲⥳⥴⥵⥶⥷⥸⥹⥺⥻⥼⥽⥾⥿⦀⦁⦂⦙⦚⦢⦣⦤⦥⦦⦧⦨⦩⦪⦫⦬⦭⦮⦯⦰⦱⦲⦳⦴⦵⦶⦷⦸⦹⦺⦻⦼⦽⦾⦿⧀⧁⧂⧃⧄⧅⧆⧇⧈⧉⧊⧋⧌⧍⧎⧏⧐⧑⧒⧓⧔⧕⧖⧗⧞⧟⧠⧡⧢⧣⧤⧥⧦⧧⧨⧩⧪⧫⧬⧭⧮⧯⧰⧱⧲⧳⧴⧵⧶⧷⧸⧹⧺⧻⧾⧿⨁⨄⨇⨈⨗⨝⨞⨟⨠⨡⨢⨣⨤⨥⨦⨧⨨⨩⨪⨫⨬⨭⨮⨯⨰⨱⨲⨳⨴⨵⨶⨷⨸⨹⨺⨻⨼⨽⨾⨿⩀⩁⩂⩃⩄⩅⩆⩇⩈⩉⩊⩋⩌⩍⩎⩏⩐⩑⩒⩓⩔⩕⩖⩗⩘⩙⩚⩛⩜⩝⩞⩟⩠⩡⩢⩣⩤⩥⩦⩧⩨⩩⩪⩫⩬⩭⩮⩯⩰⩱⩲⩳⩴⩷⩸⩹⩺⩻⩼⩽⩾⩿⪀⪁⪂⪃⪄⪅⪆⪇⪈⪉⪊⪋⪌⪍⪎⪏⪐⪑⪒⪓⪔⪕⪖⪗⪘⪙⪚⪛⪜⪝⪞⪟⪠⪡⪢⪣⪤⪥⪦⪧⪨⪩⪪⪫⪬⪭⪮⪯⪰⪱⪲⪳⪴⪵⪶⪷⪸⪹⪺⪻⪼⪽⪾⪿⫀⫁⫂⫃⫄⫅⫆⫇⫈⫉⫊⫋⫌⫍⫎⫏⫐⫑⫒⫓⫔⫕⫖⫗⫘⫙⫚⫛⫝̸⫝⫞⫟⫠⫡⫢⫣⫤⫥⫦⫧⫨⫩⫪⫫⫮⫯⫰⫱⫲⫳⫴⫵⫶⫷⫸⫹⫺⫻⫼⫽⫾⬰⬱⬲⬳⬴⬵⬶⬷⬸⬹⬺⬻⬼⬽⬾⬿⭀⭁⭂⭃⭄⭇⭈⭉⭊⭋⭌﬩﹢﹤﹥﹦＋＜＝＞｜～￩￪￫￬".Contains(c))
    <?> "<infix symbol>"

let objectMathSymbols : Parser<string,unit> =
    many1Satisfy (fun c -> "0123456789⅁⅂⅃⅄∅∞∟∠∡∢∿⊾⊿⍼⦛⦜⦝⦞⦟⦠⦡⧜⧝".Contains(c))


// ============================================================================
// Literals and base atoms
// ============================================================================

let leftParen : Parser<unit,unit> = 
    skipChar '(' >>. IW <!> "leftParen"

let rightParen : Parser<unit,unit> = 
    IW >>. skipChar ')' <!> "rightParen"

let leftBrace : Parser<unit,unit> = 
    skipChar '{' >>. IW <!> "leftBrace"

let rightBrace : Parser<unit,unit> = 
    IW >>. skipChar '}' <!> "rightBrace"

let leftBracket : Parser<unit,unit> =
    skipChar '[' >>. IW <!> "leftBracket"

let rightBracket : Parser<unit,unit> =
    IW >>. skipChar ']' <!> "rightBracket"

let comma : Parser<unit,unit> =
    attempt (IW >>. skipChar ',' >>. IW) <!> "comma"

