/// This module contains the FPL parser conbinators producing an abstract syntax tree out of a given FPL code.
module FplParsing.Basic
open FParsec
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

let attemptSW = SW <|> (IW .>> attempt (lookAhead (choice [skipChar '('; skipChar ')'; skipChar '{'; skipChar ','; skipChar ';'; skipChar '[' ]))) <!> "IW"

// ============================================================================
// Operator sets
// ============================================================================

// Prefix operators: - ~ #
let pPrefixOp : Parser<string,unit> =
    many1Satisfy (fun c -> "%&'*+-/؆؇℘⅀⅋∁∂∆∇∏∐∑−√∛∜∫∬∭∮∯∰∱∲∳∸∺∻∼∽∾≁⊢⊣⊤⊥⊦⊧⊨⊩⊪⊫⊬⊭⊮⊯⊰⊱⊹⊺⋀⋁⋂⋃⋄⋅⋆⋇⋈⋉⋊⋋⋌◸◹◺◻◼◽◾◿♯⟀⟁⟉⟊⟋⟌⟍⟐⟑⟓⟔⟕⟖⟗⟘⟙⟚⟛⟜⟝⟞⟟⟠⟡⟲⟳⤫⤬⥊⥋⥌⥍⥎⥏⥐⥑⥼⥽⥾⥿⦀⦁⦂⦙⦚⦢⦣⦤⦥⦦⦧⦰⦱⦲⦵⦶⦷⦸⦹⦺⦻⦼⦾⦿⧂⧃⧄⧅⧆⧇⧈⧉⧊⧋⧌⧍⧖⧗⧠⧨⧩⧫⧮⧯⧰⧱⧲⧳⧵⧶⧷⧸⧹⨀⨁⨂⨃⨄⨅⨆⨉⨊⨋⨌⨍⨎⨏⨐⨑⨒⨓⨔⨕⨖⨘⨙⨚⨛⨜⨼⨽⨿⩊⩋⩎⩏⫚⫝̸⫝⫞⫟⫠⫡⫢⫣⫤⫥⫦⫧⫨⫩⫪⫫⫬⫭⫯⫰⫱⫿｜～￢".Contains(c))

// Postfix operators: ! ' /
let pPostfixOp : Parser<string,unit> =
    many1Satisfy (fun c -> "!%&'*+-/⁺⁻⁼₊₋₌⅋∸∺∻∼∽∾≁⊹⊺⋄⋅⋆⋇⋈⋉⋊⋋⋌◸◹◺◻◼◽◾◿♯⟀⟁⟉⟊⟋⟌⟍⟐⟑⟓⟔⟕⟖⟗⟘⟙⟚⟛⟜⟝⟞⟟⟠⟡⟲⟳⤫⤬⥊⥋⥌⥍⥎⥏⥐⥑⥼⥽⥾⥿⦀⦁⦂⦙⦚⦢⦣⦤⦥⦦⦧⦰⦱⦲⦵⦶⦷⦸⦹⦺⦻⦼⦾⦿⧂⧃⧄⧅⧆⧇⧈⧉⧊⧋⧌⧍⧖⧗⧠⧨⧩⧫⧮⧯⧰⧱⧲⧳⧵⧶⧷⧸⧹⨼⨽⩊⩋⫝̸⫝⫞⫟⫠⫡⫢⫣⫤⫥⫦⫧⫨⫩⫪⫫⫯⫰⫱｜～".Contains(c))

// Infix operators: + - * /
let infixMathSymbols : Parser<string,unit> =
    many1Satisfy (fun c -> "*+-/<=>@\^_abcdefghijklmnopqrstuvwxyz±×÷϶؈⁄⁒⁺⁼₊₌⅋←↑→↓↔↚↛↠↣↦↮⇎⇏⇒⇔⇴⇵⇶⇷⇸⇹⇺⇻⇼⇽⇾⇿∄∆∈∉∊∋∌∍−∓∔∕∖∗∘∙∝∣∤∥∦∧∨∩∪∴∵∶∷∸∹∺∻∼∽∾≀≁≂≃≄≅≆≇≈≉≊≋≌≍≎≏≐≑≒≓≖≗≘≙≚≛≜≞≟≠≡≢≣≤≥≦≧≨≩≪≫≬≭≮≯≰≱≲≳≴≵≶≷≸≹≺≻≼≽≾≿⊀⊁⊂⊃⊄⊅⊆⊇⊈⊉⊊⊋⊌⊍⊎⊏⊐⊑⊒⊓⊔⊕⊖⊗⊘⊙⊚⊛⊜⊝⊞⊟⊠⊡⊢⊣⊤⊥⊦⊧⊨⊩⊪⊫⊬⊭⊮⊯⊰⊱⊲⊳⊴⊵⊶⊷⊸⊹⊺⊻⊼⊽⋄⋅⋆⋇⋈⋉⋊⋋⋌⋍⋎⋏⋐⋑⋒⋓⋔⋕⋖⋗⋘⋙⋚⋛⋜⋝⋞⋟⋠⋡⋢⋣⋤⋥⋦⋧⋨⋩⋪⋫⋬⋭⋲⋳⋴⋵⋶⋷⋸⋹⋺⋻⋼⋽⋾⋿▷◁◸◹◺◻◼◽◾◿♯⟀⟁⟂⟃⟄⟇⟈⟉⟊⟋⟌⟍⟎⟏⟐⟑⟒⟓⟔⟕⟖⟗⟘⟙⟚⟛⟜⟝⟞⟟⟠⟡⟢⟣⟤⟥⟰⟱⟲⟳⟴⟵⟶⟷⟸⟹⟺⟻⟼⟽⟾⟿⤀⤁⤂⤃⤄⤅⤆⤇⤈⤉⤊⤋⤌⤍⤎⤏⤐⤑⤒⤓⤔⤕⤖⤗⤘⤙⤚⤛⤜⤝⤞⤟⤠⤡⤢⤣⤤⤥⤦⤧⤨⤩⤪⤫⤬⤭⤮⤯⤰⤱⤲⤳⤴⤵⤶⤷⤸⤹⤺⤻⤼⤽⤾⤿⥀⥁⥂⥃⥄⥅⥆⥇⥈⥉⥊⥋⥌⥍⥎⥏⥐⥑⥒⥓⥔⥕⥖⥗⥘⥙⥚⥛⥜⥝⥞⥟⥠⥡⥢⥣⥤⥥⥦⥧⥨⥩⥪⥫⥬⥭⥮⥯⥰⥱⥲⥳⥴⥵⥶⥷⥸⥹⥺⥻⥼⥽⥾⥿⦀⦁⦂⦙⦚⦢⦣⦤⦥⦦⦧⦨⦩⦪⦫⦬⦭⦮⦯⦰⦱⦲⦳⦴⦵⦶⦷⦸⦹⦺⦻⦼⦽⦾⦿⧀⧁⧂⧃⧄⧅⧆⧇⧈⧉⧊⧋⧌⧍⧎⧏⧐⧑⧒⧓⧔⧕⧖⧗⧞⧟⧠⧡⧢⧣⧤⧥⧦⧧⧨⧩⧪⧫⧬⧭⧮⧯⧰⧱⧲⧳⧴⧵⧶⧷⧸⧹⧺⧻⧾⧿⨁⨄⨇⨈⨗⨝⨞⨟⨠⨡⨢⨣⨤⨥⨦⨧⨨⨩⨪⨫⨬⨭⨮⨯⨰⨱⨲⨳⨴⨵⨶⨷⨸⨹⨺⨻⨼⨽⨾⨿⩀⩁⩂⩃⩄⩅⩆⩇⩈⩉⩊⩋⩌⩍⩎⩏⩐⩑⩒⩓⩔⩕⩖⩗⩘⩙⩚⩛⩜⩝⩞⩟⩠⩡⩢⩣⩤⩥⩦⩧⩨⩩⩪⩫⩬⩭⩮⩯⩰⩱⩲⩳⩴⩷⩸⩹⩺⩻⩼⩽⩾⩿⪀⪁⪂⪃⪄⪅⪆⪇⪈⪉⪊⪋⪌⪍⪎⪏⪐⪑⪒⪓⪔⪕⪖⪗⪘⪙⪚⪛⪜⪝⪞⪟⪠⪡⪢⪣⪤⪥⪦⪧⪨⪩⪪⪫⪬⪭⪮⪯⪰⪱⪲⪳⪴⪵⪶⪷⪸⪹⪺⪻⪼⪽⪾⪿⫀⫁⫂⫃⫄⫅⫆⫇⫈⫉⫊⫋⫌⫍⫎⫏⫐⫑⫒⫓⫔⫕⫖⫗⫘⫙⫚⫛⫝̸⫝⫞⫟⫠⫡⫢⫣⫤⫥⫦⫧⫨⫩⫪⫫⫮⫯⫰⫱⫲⫳⫴⫵⫶⫷⫸⫹⫺⫻⫼⫽⫾⬰⬱⬲⬳⬴⬵⬶⬷⬸⬹⬺⬻⬼⬽⬾⬿⭀⭁⭂⭃⭄⭇⭈⭉⭊⭋⭌﬩﹢﹤﹥﹦＋＜＝＞｜～￩￪￫￬".Contains(c))

let objectMathSymbols : Parser<string,unit> =
    many1Satisfy (fun c -> "0123456789⅁⅂⅃⅄∅∞∟∠∡∢∿⊾⊿⍼⦛⦜⦝⦞⦟⦠⦡⧜⧝".Contains(c))
