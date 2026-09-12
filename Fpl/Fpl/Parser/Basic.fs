(* Copyright (c) 2021+ bookofproofs See LICENSE in the project root for license terms. *)

/// <summary>
/// Basic FPL parser helpers and combinators built on top of FParsec.
/// </summary>
/// <remarks>
/// Provides low-level parsers, whitespace control combinators and operator
/// symbol parsers used across the FPL parser pipeline. Parsers here are
/// intentionally small and composable to produce rich AST diagnostics and
/// position information.
/// </remarks>
module Fpl.Parser.Basic

open System.Text.RegularExpressions
open FParsec
open Fpl.Parser.Types
open Fpl.Parser.Debug


// ============================================================================
// Helpers
// ============================================================================

/// <summary>
/// A low-level parser that returns the current stream position as a value.
/// </summary>
/// <remarks>
/// This parser consumes no input; it yields the stream position so other
/// combinators can attach position information to parsed values.
/// </remarks>
let private _position: Parser<_,_> = fun stream -> Reply stream.Position

/// <summary>
/// Wraps parser <paramref name="p"/> and returns a tuple of the starting
/// position and the parser result.
/// </summary>
/// <param name="p">Parser to run.</param>
/// <returns>Parser that yields a tuple of (start position, result).</returns>
let private _startingPosition p = _position .>>. p

/// <summary>
/// Wraps parser <paramref name="p"/> and returns a tuple of the parser
/// result and the ending position.
/// </summary>
/// <param name="p">Parser to run.</param>
/// <returns>Parser that yields a tuple of (end position, result).</returns>
let private _endingPosition p = 
    let result = p .>>. _position
    result 
    >>= fun (p, pos) ->
    preturn (pos, p)


/// <summary>
/// Runs parser <paramref name="p"/> and returns a pair of the parser result
/// and its start/end positions as an instance of <c>Positions</c>.
/// </summary>
/// <param name="p">Parser to run.</param>
/// <returns>
/// Parser that yields a tuple: (<c>Positions</c>, parsed value).
/// </returns>
/// <remarks>
/// The columns stored in the resulting positions are adjusted so diagnostics
/// point to the start of the token rather than its end. This helps editor
/// tooling show more intuitive error locations.
/// </remarks>
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

/// <summary>
/// Runs parser <paramref name="p"/> but ensures that on failure the input
/// stream state is restored to its original state (backtracked) so that
/// FParsec error reporting remains accurate.
/// </summary>
/// <param name="p">Parser to run with backtracked error behavior.</param>
/// <returns>Parser with backtracked error behavior on failure.</returns>
/// <remarks>
/// Useful to prevent intermediate failures from preventing other alternatives
/// from being attempted when producing diagnostics for keywords and names.
/// </remarks>
let withBacktrackedError p: Parser<_,_> =
    fun stream ->
        let mutable oldState = stream.State
        match p stream with
        | Success(result, restInput, userState) ->
            Reply(result, restInput)
        | _ ->
            Reply(oldState)

// ============================================================================
// Whitespace control
// ============================================================================

/// <summary>
/// Character parser that matches standard ASCII whitespace characters used
/// by the FPL grammar.
/// </summary>
let whiteSpaces = anyOf " \t\r\n"

/// <summary>
/// Parser that fails if any whitespace is present at the current position.
/// </summary>
/// <remarks>Used to assert that no whitespace is allowed at a location.</remarks>
let NW : Parser<unit,unit> =
    notFollowedBy (skipMany1 whiteSpaces) <?> "<no whitespace>" <!> "NW"

/// <summary>
/// Parser that requires at least one whitespace character.
/// </summary>
let SW : Parser<unit,unit> =
    skipMany1 whiteSpaces >>% () <?> "<significant whitespace>" <!> "SW"

/// <summary>
/// Parser that accepts zero or more whitespace characters.
/// </summary>
let IW : Parser<unit,unit> =
    skipMany whiteSpaces >>% () <?> "<whitespace>" <!> "IW"

/// <summary>
/// Attempts to parse significant whitespace or, failing that, accepts
/// optional whitespace only if followed by one of several punctuation
/// tokens. This is an optimization for lookahead-sensitive grammar points.
/// </summary>
let attemptSW = SW <|> (IW .>> attempt (lookAhead (choice [skipChar '('; skipChar ')'; skipChar '{'; skipChar ','; skipChar ';'; skipChar '[' ]))) <!> "atemptSW"

/// <summary>
/// Ensures that a parser that may be preceded by optional whitespace is
/// handled safely with lookahead so it does not interfere with backtracking
/// and operator parsing.
/// </summary>
/// <param name="p">Parser to run with safe preceding whitespace handling.</param>
/// <returns>Parser that will look ahead for optional whitespace before running <paramref name="p"/>.</returns>
let safeProceedingSpace p =
    lookAhead (IW >>. p)
    >>. IW
    >>. p

// ============================================================================
// Operator sets
// ============================================================================

/// <summary>
/// Parser that recognizes a single prefix operator character and returns it as a string.
/// </summary>
let prefixMathSymbols : Parser<string,unit> =
    satisfy (fun c -> "~%&'*+-/؆؇℘⅀⅋∁∂∆∇∏∐∑−√∛∜∫∬∭∮∯∰∱∲∳∸∺∻∼∽∾≁⊢⊣⊤⊥⊦⊧⊨⊩⊪⊫⊬⊭⊮⊯⊰⊱⊹⊺⋀⋁⋂⋃⋄⋅⋆⋇⋈⋉⋊⋋⋌◸◹◺◻◼◽◾◿♯⟀⟁⟉⟊⟋⟌⟍⟐⟑⟓⟔⟕⟖⟗⟘⟙⟚⟛⟜⟝⟞⟟⟠⟡⟲⟳⤫⤬⥊⥋⥌⥍⥎⥏⥐⥑⥼⥽⥾⥿⦀⦁⦂⦙⦚⦢⦣⦤⦥⦦⦧⦰⦱⦲⦵⦶⦷⦸⦹⦺⦻⦼⦾⦿⧂⧃⧄⧅⧆⧇⧈⧉⧊⧋⧌⧍⧖⧗⧠⧨⧩⧫⧮⧯⧰⧱⧲⧳⧵⧶⧷⧸⧹⨀⨁⨂⨃⨄⨅⨆⨉⨊⨋⨌⨍⨎⨏⨐⨑⨒⨓⨔⨕⨖⨘⨙⨚⨛⨜⨼⨽⨿⩊⩋⩎⩏⫚⫝̸⫝⫞⫟⫠⫡⫢⫣⫤⫥⫦⫧⫨⫩⫪⫫⫬⫭⫯⫰⫱⫿｜～￢".Contains(c))
    |>> string

/// <summary>
/// Parser that recognizes a single postfix operator character and returns it as a string.
/// </summary>
let postfixMathSymbols : Parser<string,unit> =
    satisfy (fun c -> "!%&'*+-/⁺⁻⁼₊₋₌⅋∸∺∻∼∽∾≁⊹⊺⋄⋅⋆⋇⋈⋉⋊⋋⋌◸◹◺◻◼◽◾◿♯⟀⟁⟉⟊⟋⟌⟍⟐⟑⟓⟔⟕⟖⟗⟘⟙⟚⟛⟜⟝⟞⟟⟠⟡⟲⟳⤫⤬⥊⥋⥌⥍⥎⥏⥐⥑⥼⥽⥾⥿⦀⦁⦂⦙⦚⦢⦣⦤⦥⦦⦧⦰⦱⦲⦵⦶⦷⦸⦹⦺⦻⦼⦾⦿⧂⧃⧄⧅⧆⧇⧈⧉⧊⧋⧌⧍⧖⧗⧠⧨⧩⧫⧮⧯⧰⧱⧲⧳⧵⧶⧷⧸⧹⨼⨽⩊⩋⩎⩏⫝̸⫝⫞⫟⫠⫡⫢⫣⫤⫥⫦⫧⫨⫩⫪⫫⫯⫰⫱｜～".Contains(c))
    |>> string

/// <summary>
/// Runs parser <paramref name="p"/> and ensures its result satisfies the
/// provided predicate; otherwise the input is backtracked and an error is returned.
/// </summary>
/// <param name="predicate">Predicate to validate the parsed result.</param>
/// <param name="msg">Error message used when the predicate fails.</param>
/// <param name="p">Parser whose result is validated.</param>
/// <returns>Parser that enforces <paramref name="predicate"/> on the result.</returns>
/// <remarks>
/// Behavior is based on the FParsec users guide pattern for lookahead and
/// backtracking-aware predicates.
/// </remarks>
let resultSatisfies predicate msg (p: Parser<_,_>) : Parser<_,_> =
    let error = messageError msg
    fun stream ->
        let state = stream.State
        let reply = p stream
        if reply.Status <> Ok || predicate reply.Result then reply
        else
            stream.BacktrackTo(state) // backtrack to beginning
            Reply(Primitives.Error, error)

/// <summary>
/// Regular expression used to detect template identifiers such as 'tpl' or 'template'.
/// </summary>
let tplRegex = Regex(@"^(tpl|template)(([A-Z]\w*)|\d*)$", RegexOptions.Compiled)


/// <summary>
/// Parser that recognizes one or more infix operator characters and returns them as a string.
/// </summary>
let infixMathSymbols : Parser<string,unit> =
    many1Satisfy (fun c -> "*+-/<=>@\^_v±×÷϶؈⁄⁒⁺⁼₊₌⅋←↑→↓↔↚↛↠↣↦↮⇎⇏⇒⇔⇴⇵⇶⇷⇸⇹⇺⇻⇼⇽⇾⇿∄∆∈∉∊∋∌∍−∓∔∕∖∗∘∙∝∣∤∥∦∧∨∩∪∴∵∶∷∸∹∺∻∼∽∾≀≁≂≃≄≅≆≇≈≉≊≋≌≍≎≏≐≑≒≓≖≗≘≙≚≛≜≞≟≠≡≢≣≤≥≦≧≨≩≪≫≬≭≮≯≰≱≲≳≴≵≶≷≸≹≺≻≼≽≾≿⊀⊁⊂⊃⊄⊅⊆⊇⊈⊉⊊⊋⊌⊍⊎⊏⊐⊑⊒⊓⊔⊕⊖⊗⊘⊙⊚⊛⊜⊝⊞⊟⊠⊡⊢⊣⊤⊥⊦⊧⊨⊩⊪⊫⊬⊭⊮⊯⊰⊱⊲⊳⊴⊵⊶⊷⊸⊹⊺⊻⊼⊽⋄⋅⋆⋇⋈⋉⋊⋋⋌⋍⋎⋏⋐⋑⋒⋓⋔⋕⋖⋗⋘⋙⋚⋛⋜⋝⋞⋟⋠⋡⋢⋣⋤⋥⋦⋧⋨⋩⋪⋫⋬⋭⋲⋳⋴⋵⋶⋷⋸⋹⋺⋻⋼⋽⋾⋿▷◁◸◹◺◻◼◽◾◿♯⟀⟁⟂⟃⟄⟇⟈⟉⟊⟋⟌⟍⟎⟏⟐⟑⟒⟓⟔⟕⟖⟗⟘⟙⟚⟛⟜⟝⟞⟟⟠⟡⟢⟣⟤⟥⟰⟱⟲⟳⟴⟵⟶⟷⟸⟹⟺⟻⟼⟽⟾⟿⤀⤁⤂⤃⤄⤅⤆⤇⤈⤉⤊⤋⤌⤍⤎⤏⤐⤑⤒⤓⤔⤕⤖⤗⤘⤙⤚⤛⤜⤝⤞⤟⤠⤡⤢⤣⤤⤥⤦⤧⤨⤩⤪⤫⤬⤭⤮⤯⤰⤱⤲⤳⤴⤵⤶⤷⤸⤹⤺⤻⤼⤽⤾⤿⥀⥁⥂⥃⥄⥅⥆⥇⥈⥉⥊⥋⥌⥍⥎⥏⥐⥑⥒⥓⥔⥕⥖⥗⥘⥙⥚⥛⥜⥝⥞⥟⥠⥡⥢⥣⥤⥥⥦⥧⥨⥩⥪⥫⥬⥭⥮⥯⥰⥱⥲⥳⥴⥵⥶⥷⥸⥹⥺⥻⥼⥽⥾⥿⦀⦁⦂⦙⦚⦢⦣⦤⦥⦦⦧⦨⦩⦪⦫⦬⦭⦮⦯⦰⦱⦲⦳⦴⦵⦶⦷⦸⦹⦺⦻⦼⦽⦾⦿⧀⧁⧂⧃⧄⧅⧆⧇⧈⧉⧊⧋⧌⧍⧎⧏⧐⧑⧒⧓⧔⧕⧖⧗⧞⧟⧠⧡⧢⧣⧤⧥⧦⧧⧨⧩⧪⧫⧬⧭⧮⧯⧰⧱⧲⧳⧴⧵⧶⧷⧸⧹⧺⧻⧾⧿⨁⨄⨇⨈⨗⨝⨞⨟⨠⨡⨢⨣⨤⨥⨦⨧⨨⨩⨪⨫⨬⨭⨮⨯⨰⨱⨲⨳⨴⨵⨶⨷⨸⨹⨺⨻⨼⨽⨾⨿⩀⩁⩂⩃⩄⩅⩆⩇⩈⩉⩊⩋⩌⩍⩎⩏⩐⩑⩒⩓⩔⩕⩖⩗⩘⩙⩚⩛⩜⩝⩞⩟⩠⩡⩢⩣⩤⩥⩦⩧⩨⩩⩪⩫⩬⩭⩮⩯⩰⩱⩲⩳⩴⩷⩸⩹⩺⩻⩼⩽⩾⩿⪀⪁⪂⪃⪄⪅⪆⪇⪈⪉⪊⪋⪌⪍⪎⪏⪐⪑⪒⪓⪔⪕⪖⪗⪘⪙⪚⪛⪜⪝⪞⪟⪠⪡⪢⪣⪤⪥⪦⪧⪨⪩⪪⪫⪬⪭⪮⪯⪰⪱⪲⪳⪴⪵⪶⪷⪸⪹⪺⪻⪼⪽⪾⪿⫀⫁⫂⫃⫄⫅⫆⫇⫈⫉⫊⫋⫌⫍⫎⫏⫐⫑⫒⫓⫔⫕⫖⫗⫘⫙⫚⫛⫝̸⫝⫞⫟⫠⫡⫢⫣⫤⫥⫦⫧⫨⫩⫪⫫⫮⫯⫰⫱⫲⫳⫴⫵⫶⫷⫸⫹⫺⫻⫼⫽⫾⬰⬱⬲⬳⬴⬵⬶⬷⬸⬹⬺⬻⬼⬽⬾⬿⭀⭁⭂⭃⭄⭇⭈⭉⭊⭋⭌﬩﹢﹤﹥﹦＋＜＝＞｜～￩￪￫￬".Contains(c))
    <?> "<infix symbol>"

/// <summary>
/// Parser that recognizes numeric-like object math symbols and returns them as a string.
/// </summary>
let objectMathSymbols : Parser<string,unit> =
    many1Satisfy (fun c -> "0123456789⅁⅂⅃⅄∅∞∟∠∡∢∿⊾⊿⍼⦛⦜⦝⦞⦟⦠⦡⧜⧝".Contains(c))


// ============================================================================
// Literals and base atoms
// ============================================================================

/// <summary>Parses '(' and consumes optional following whitespace.</summary>
let leftParen : Parser<unit,unit> = 
    skipChar '(' >>. IW <!> "leftParen"

/// <summary>Parses ')' allowing safe preceding whitespace.</summary>
let rightParen : Parser<unit,unit> = safeProceedingSpace (skipChar ')') <!> "rightParen"

/// <summary>Parses '{' and consumes optional following whitespace.</summary>
let leftBrace : Parser<unit,unit> = 
    skipChar '{' >>. IW <!> "leftBrace"

/// <summary>Parses '}' allowing safe preceding whitespace.</summary>
let rightBrace : Parser<unit,unit> = safeProceedingSpace (skipChar '}') <!> "rightBrace"

/// <summary>Parses '[' and consumes optional following whitespace.</summary>
let leftBracket : Parser<unit,unit> =
    skipChar '[' >>. IW <!> "leftBracket"

/// <summary>Parses ']' allowing safe preceding whitespace.</summary>
let rightBracket : Parser<unit,unit> = safeProceedingSpace (skipChar ']') <!> "rightBracket"

/// <summary>Parses a comma with optional surrounding whitespace.</summary>
let comma : Parser<unit,unit> =
    attempt (IW >>. skipChar ',' >>. IW) <!> "comma"

/// <summary>Parses ':' followed by optional whitespace.</summary>
let colon : Parser<unit,unit> =
    skipChar ':' .>> IW

/// <summary>Parses ':=' followed by optional whitespace.</summary>
let colonEqual : Parser<unit,unit> =
    skipString ":=" >>. IW 

/// <summary>Parses '@'.</summary>
let at : Parser<unit,unit> =
    skipChar '@'

/// <summary>Parses a case separator '|' and consumes optional following whitespace.</summary>
let case : Parser<unit,unit> =
    skipChar '|' >>. IW

/// <summary>Parses an else-case separator '?' and consumes optional following whitespace.</summary>
let elseCase : Parser<unit,unit> =
    skipChar '?' >>. IW

/// <summary>Parses ';' followed by optional whitespace.</summary>
let semiColon : Parser<unit,unit> =
    skipChar ';' .>> IW 

/// <summary>Parses '!'.</summary>
let exclamationMark : Parser<unit,unit> =
    skipChar '!'

/// <summary>Parses the arrow token "->".</summary>
let toArrow : Parser<unit,unit> =
    skipString "->"

/// <summary>Parses the turnstile symbol "|-".</summary>
let vDash : Parser<unit,unit> =
    skipString "|-"

/// <summary>Parses a double-quote mark.</summary>
let quote : Parser<unit,unit> =
    skipChar '"' 

/// <summary>Parses '/'.</summary>
let slash : Parser<unit,unit> =
    skipChar '/'

/// <summary>Parses a '.' and maps it to <c>Ast.Dot</c>.</summary>
let dot : Parser<Ast,unit> =
    skipChar '.' |>> Ast.Dot <!> "Dot"

