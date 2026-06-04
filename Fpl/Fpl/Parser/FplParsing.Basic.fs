/// This module contains the basic FPL parser helpers and combinators
/// producing an abstract syntax tree out of a given FPL code.
module FplParsing.Basic
open System.Text.RegularExpressions
open FParsec
open FplGrammarTypes
open FplParsing.Debug
(* MIT License

Copyright (c) 2026+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)

// ============================================================================
// Helpers
// ============================================================================

/// A helper parser that consumes any input and can be combined with existing parsers to enrich them with 
/// the parsing position.
let private _position: Parser<_,_> = fun stream -> Reply stream.Position

/// Takes the parser `p` and returns a tuple with it starting parsing position
let private _startingPosition p = _position .>>. p

/// Takes the parser `p` and returns a tuple with it starting parsing position
let private _endingPosition p = 
    let result = p .>>. _position
    result 
    >>= fun (p, pos) ->
    preturn (pos, p)


/// Takes the parser `p` and returns a tuple of its result, together with its starting and ending position.
let positions (p: Parser<_,_>): Parser<Positions * _,_> =
    pipe2
        (_position .>>. p)
        (_position)
        (
            // correct columns to keep the convention of jumping to
            // the beginning and not to the end of a diagnostics in an IDE
            let offset = (int64)1
            fun (startPos, result) endPos ->
            let pos1 = Position("", startPos.Index, startPos.Line, startPos.Column-offset)
            let pos2 = Position("", endPos.Index, endPos.Line, endPos.Column-offset)
            (Positions(pos1, pos2), result)
        )

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

let colon : Parser<unit,unit> =
    skipChar ':' .>> IW

let colonEqual : Parser<unit,unit> =
    skipString ":=" >>. IW 

let at : Parser<unit,unit> =
    skipChar '@'

let case : Parser<unit,unit> =
    skipChar '|' >>. IW

let elseCase : Parser<unit,unit> =
    skipChar '?' >>. IW

let semiColon : Parser<unit,unit> =
    skipChar ';' .>> IW 

let exclamationMark : Parser<unit,unit> =
    skipChar '!'

let toArrow : Parser<unit,unit> =
    skipString "->"

let vDash : Parser<unit,unit> =
    skipString "|-"

let quote : Parser<unit,unit> =
    skipChar '"' 

let slash : Parser<unit,unit> =
    skipChar '/'

let dot : Parser<Ast,unit> =
    skipChar '.' |>> Ast.Dot <!> "Dot"

