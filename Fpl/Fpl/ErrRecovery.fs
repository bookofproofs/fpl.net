module ErrRecovery
open System.Collections.Generic
open FParsec
open FplGrammarTypes


(* MIT License

Copyright (c) 2023 bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 
*)

type DiagnosticEmitter = FplParser | FplInterpreter 
type DiagnosticSeverity = Error | Warning | Hint | Information
type DiagnosticMessage = 
    | DiagnosticMessage of string
    member this.Value = 
        match this with 
        | DiagnosticMessage(value) -> value
type Diagnostic = Diagnostic of DiagnosticEmitter * DiagnosticSeverity * Position * DiagnosticMessage
    with 
    member this.Emitter with get() = 
        match this with 
        | Diagnostic(emitter, _, _, _) -> emitter
    member this.Severity with get() = 
        match this with 
        | Diagnostic(_, severity, _, _) -> severity
    member this.Position with get() = 
        match this with 
        | Diagnostic(_, _, position, _) -> position
    member this.Message with get() = 
        match this with 
        | Diagnostic(_, _, _, message) -> message

type Diagnostics () =
    let myHashset = new HashSet<Diagnostic>()
    member this.Collection with get() = myHashset
    member this.AddDiagnostic d = 
        if not (myHashset.Contains(d)) then 
            myHashset.Add(d) |> ignore
            
    member this.PrintDiagnostics = 
        for d in myHashset do printfn "%O" d
        printfn "%s" "^------------------------^\n" 
    member this.DiagnosticsToString = 
        myHashset
        |> Seq.map string
        |> String.concat "\n"
    member this.Clear() = myHashset.Clear()

let ad = Diagnostics() 


/// Emit any errors occurring in the globalParser
/// This is to make sure that the parser will always emit diagnostics, 
/// even if the error recovery fails on a global level (and so does the parser).
let tryParse globalParser expectMessage (ad:Diagnostics) input = 
    match run globalParser input with
    | Success(result, restInput, userState) -> 
        result 
    | Failure(errorMsg, restInput, userState) -> 
        let diagnosticMsg = DiagnosticMessage (expectMessage + " " + errorMsg)
        let diagnostic = Diagnostic (DiagnosticEmitter.FplParser, DiagnosticSeverity.Error,restInput.Position,diagnosticMsg)
        ad.AddDiagnostic diagnostic
        Ast.Error

/// A helper parser applying the escapeParser to the input and emitting a diagnostic 
/// a the current parsing position with a user-defined error message 
/// explaining why this diagnostic was generated. 
/// An Ast.Escape node is returned as a placeholder for the part of Ast that the parser failed to generate. 
let emitDiagnostics (ad:Diagnostics) escapeParser msg = 
    let errorMsg = DiagnosticMessage msg
    let positionedEscapeParser = 
        getPosition .>>. escapeParser
        |>> fun (pos, escape) -> (pos, escape)
    positionedEscapeParser >>= fun (pos, escape) ->
    let diagnostic = Diagnostic (DiagnosticEmitter.FplParser, DiagnosticSeverity.Error,pos,errorMsg)
    ad.AddDiagnostic diagnostic
    preturn () >>% Ast.Escape
    

/// Emits diagnostics at the current position.
let emitDiagnostics1 (ad:Diagnostics) (msg:string) pos =
    let errorMsg = DiagnosticMessage msg
    let diagnostic = Diagnostic (DiagnosticEmitter.FplParser, DiagnosticSeverity.Error,pos,errorMsg)
    ad.AddDiagnostic diagnostic
    preturn () |> ignore

/// A helper parser that skips any characters until innerSeparator would succeed,
/// but where innerSeparator does not consume any input, 
/// unless, at the same position, outerSeparator occurs.
let skipUntilLookaheadSeparatorFail innerSeparator outerSeparator = 
    skipMany (notFollowedBy (attempt innerSeparator <|> outerSeparator) >>. anyChar)

/// A helper parser that skips any characters until innerSeparator would succeed,
/// but where innerSeparator does not consume any input.
let skipUntilLookaheadSeparator innerSeparator = 
    skipMany (notFollowedBy innerSeparator >>. anyChar)

/// A helper parser that skips any characters until innerSeparator would succeed,
/// but where innerSeparator does not consume any input, 
/// unless, at the same position, one of a listed outerSeparators occurs in their respective list order.
let skipUntilLookaheadSeparatorListFail innerSeparator (outerSeparators:Parser<_,_> list) = 
    outerSeparators
    |> List.map (fun outerSeparator -> attempt (skipUntilLookaheadSeparatorFail innerSeparator outerSeparator))
    |> List.reduce (<|>)
    
/// Applies either the parser `p` without diagnostics, or emits diagnostics while recovering from 
/// the error by means of consuming any character until the next occurrence of `innerSeparator`, or
/// if one of the parsers from a `outerSeparatorList` could be applied to the input
/// without consuming it. This is helpful if these outer separators are most likely applied in the particular grammar after the sequence. 
let private _sequenceDiagnostics (p:Parser<_,_>) (innerSeparator:Parser<_,_>) (outerSeparatorList:Parser<_,_> list) (ad:Diagnostics) (msg:string) = 
    let breakCondition = skipUntilLookaheadSeparatorListFail innerSeparator outerSeparatorList
    p <|> emitDiagnostics ad breakCondition msg

/// Applies the parser `sepBy1 pWithdiagnostics innerSeparator` where `pWithDiagnostics`
/// applies each time either `p` without diagnostics, or emits diagnostics while recovering from 
/// the error by means of consuming any character until the next occurrence of `innerSeparator`. The application of this sequence 
/// generated by `sepBy1` will end if one of the parsers from a `outerSeparatorList` could be applied to the input
/// without consuming it. This is helpful if these outer separators are most likely applied in the particular grammar after the sequence. 
let sequenceDiagnostics1 (p:Parser<_,_>) (innerSeparator:Parser<_,_>) (outerSeparatorList:Parser<_,_> list) (ad:Diagnostics) (msg:string) = 
    let result = _sequenceDiagnostics p innerSeparator outerSeparatorList ad msg
    sepBy1 result innerSeparator

/// Similar to `sequenceDiagnostics1` except that it matches also zero occurrences of `p`, returning a `[Ast.Empty]`.
let sequenceDiagnostics (p:Parser<_,_>) (innerSeparator:Parser<_,_>) (sepList:Parser<_,_> list) (ad:Diagnostics) (msg:string) = 
    lookAhead p >>. sequenceDiagnostics1 p innerSeparator sepList ad msg
    <|> preturn [Ast.Empty] 


/// A helper parser combining each parser a list of parsers `q` with a given parser `p` by the 
/// parser combinator `op` and returning a new list of parsers of the form `p op qi` where 
/// `qi` are the elements of `q`.
let combineWithParserList (p:Parser<_,_>) op (q:Parser<_,_> list) = List.map (fun qi -> op p qi) q

/// Creates a diagnostic alternative for a parser `p`. Attempts `p`. If it succeeds, the 
/// input stream will be consumed by `p`. Otherwise, the parser will attempt every alternative parser
/// from a given list of parsers `pErrorParsers`. If any of these parsers succeeds, it will consume
/// the input stream correspondingly and emit the diagnostics `expecting <pName>`.
/// If neither `p` nor one of the alternative parsers succeed, the parser will change the user stated by failing.
let alternative p (pName:string) (pErrorParsers:Parser<_,_> list) (ad:Diagnostics) = 
    let pErrorDiagnostic pError = 
        pError >>= fun r -> 
        getPosition >>= fun pos -> 
        let msg = "expecting " + pName
        emitDiagnostics1 ad msg pos |> ignore
        preturn r
    p <|>
    (pErrorParsers
    |> List.map pErrorDiagnostic
    |> List.reduce (<|>))
    // make sure the alternative will at least change the user state by failing the parser if 
    // no alternative was applicable
    <|> fail "no alternative found"

/// Like `alternative` but the diagnostic position will be at the beginning 
/// (and not at the end) of some alternative.
let alternativePre p (pName:string) (pErrorParsers:Parser<_,_> list) (ad:Diagnostics) = 
    let pErrorDiagnostic pError = 
        getPosition >>= fun pos -> 
        pError >>= fun r -> 
        let msg = "expecting " + pName
        emitDiagnostics1 ad msg pos |> ignore
        preturn r
    p <|>
    (pErrorParsers
    |> List.map pErrorDiagnostic
    |> List.reduce (<|>))
    // make sure the alternative will at least change the user state by failing the parser if 
    // no alternative was applicable
    <|> fail "no alternative found"

/// Creates a diagnostic alternative for a parser `p` with a diagnostic 
/// message `msg` and a list of alternative parsers `others`.
let altMsg (p:Parser<_,_>) (msg:string) (others:Parser<_,_> list) = 
    // create an alternative of the keyword with a word regex
    alternative p msg others ad

/// Like `altMsg` but the diagnostic position will be at the beginning 
/// (and not at the end) of some alternative.
let altMsgPre (p:Parser<_,_>) (msg:string) (others:Parser<_,_> list) = 
    // create an alternative of the keyword with a word regex
    alternativePre p msg others ad

/// Creates a diagnostic alternative for a string parser `p` with a diagnostic 
/// message `msg` and some typos, including words `"\w+"` or concatenations of `()[]{}@`.
let altTypoMsg (p:Parser<string,_>) (msg:string)  = 
    // create an alternative of p with a word regex
    alternative p msg [(regex @"\w+|[\{\}\[\]\(\)@]+")] ad

/// Like `altTypoMsg` but the diagnostic position will be at the beginning 
/// (and not at the end) of some alternative.
let altTypoMsgPre (p:Parser<string,_>) (msg:string)  = 
    // create an alternative of p with a word regex
    alternativePre p msg [(regex @"\w+|[\{\}\[\]\(\)@]+")] ad

/// Creates a diagnostic alternative for an ignored string parser `p` with a diagnostic 
/// message `msg` and some typos, including words `"\w+"` or concatenations of `()[]{}@`.
let altTypoMsgI (p:Parser<unit,_>) (msg:string)  = 
    // create an alternative of the keyword with a word regex
    let ignoreWord = skipMany1Satisfy (fun c -> System.Char.IsLetterOrDigit(c) || c = '_' || c = '{' || c = '}' || c = '[' || c = ']' || c = '(' || c = ')'  || c = '@')
    alternative p msg [ignoreWord] ad

/// Like `altTypoMsgI` but the diagnostic position will be at the beginning 
/// (and not at the end) of some alternative.
let altTypoMsgIPre (p:Parser<unit,_>) (msg:string)  = 
    // create an alternative of the keyword with a word regex
    let ignoreWord = skipMany1Satisfy (fun c -> System.Char.IsLetterOrDigit(c) || c = '_' || c = '{' || c = '}' || c = '[' || c = ']' || c = '(' || c = ')'  || c = '@')
    alternativePre p msg [ignoreWord] ad

/// Creates a diagnostic alternative for a particular keyword. 
/// If `optionalMsg` is a non-empty string, it will be concatenated to the standard
/// error message `"expected keyword <some keyword>"`, e.g. `optionalMsg= " or <put your alternative here>"`
let keywordAlternative (keyword:string) (optionalMsg:string)= 
    // create an alternative of the keyword with a word regex
    altMsg (pstring keyword) (sprintf "'%s' keyword" keyword + optionalMsg) [(regex @"\w+")] 

/// Creates a diagnostic alternative for two keyword alternatives (a long and a short version). 
let keywordAlternativesI (keyword:string) (keywordShort:string) = 
    // create an alternative of the keyword with a word regex
    let ignoreWord = skipMany1Satisfy (fun c -> System.Char.IsLetterOrDigit(c) || c = '_')
    altMsg (skipString keyword <|> skipString keywordShort) (sprintf "keywords '%s' or '%s'" keyword keywordShort) [ignoreWord] 

/// Creates a diagnostic alternative for a particular character
let charAlternative (c:char) = 
    // create an alternative of the keyword with a word regex
    altMsgPre (pchar c) (sprintf "'%c' " c) [(anyOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_")]

let abc a b c (aName:string) (bName:string) (cName:string) (ad:Diagnostics) =
    let aMissing = 
        getPosition >>= fun pos -> 
        b .>> c >>= fun r -> 
            emitDiagnostics1 ad ("missing opening " + aName) pos 
            preturn r
    let cMissing = 
        getPosition >>= fun pos -> 
        a >>. b >>= fun r -> 
            emitDiagnostics1 ad ("missing closing " + cName) pos 
            preturn r
    let acMissing = 
        getPosition >>= fun pos -> 
        b >>= fun r -> 
            emitDiagnostics1 ad ("missing opening " + aName) pos 
            getPosition >>= fun pos -> 
            emitDiagnostics1 ad ("missing closing " + cName) pos 
            preturn r 
    let bMissing = a >>. c >>% Ast.Empty 

    attempt bMissing 
    <|> (attempt (a >>. b .>> c) <|> cMissing)
    <|> (attempt aMissing <|> acMissing)

(*
// Other parser combinators that might become useful for other grammars but proved unnecessary in this study:
/// A simple helper function for printing trace information to the console (taken from FParsec Docs)
let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
    fun stream ->
        printfn "%A: Entering %s" stream.Position label
        let reply = p stream
        printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
        reply

/// A helper parser that skips any characters until innerSeparator would succeed,
/// but where innerSeparator does not consume any input, 
/// unless, at the same position, one of a listed outerSeparators occurs.
let skipUntilLookaheadSeparatorListFail innerSeparator outerSeparators = 
    outerSeparators
    |> List.map (fun outerSeparator -> attempt (skipUntilLookaheadSeparatorFail innerSeparator outerSeparator))
    |> List.reduce (<|>)

/// Similar to tryParse but instead of applying the 'run p input', it will 
/// return a lambda function that takes an 'input' and applies 'run p' on it.
/// Useful when you want to tryParse parsers that are not the entry point parser.
let tryParseCurrying p msg (ad:Diagnostics) = 
    fun input ->
        match run p input with
        | Success(result, restInput, userState) -> 
            result 
        | Failure(errorMsg, restInput, _) -> 
            let diagnosticMsg = DiagnosticMessage (msg + " " + errorMsg)
            let diagnostic = Diagnostic (DiagnosticEmitter.Parser, DiagnosticSeverity.Error,restInput.Position,diagnosticMsg)
            ad.AddDiagnostic diagnostic
            Ast.Error


let tryParseOther p msg (ad:Diagnostics) = 
    fun input ->
        match run p input with
        | Success(result, restInput, userState) -> result
        | Failure(errorMsg, restInput, _) -> 
            let diagnosticMsg = DiagnosticMessage (msg + " " + errorMsg)
            let diagnostic = Diagnostic (DiagnosticEmitter.Parser, DiagnosticSeverity.Error,restInput.Position,diagnosticMsg)
            ad.AddDiagnostic diagnostic
            preturn Ast.Error


if correction by choice results in the same error message then, the choice was wrong 
=> to avoid infinite loops, try another 'nonempty string' in the error message 
=> otherwise fail with the message infinite loop in error recovery at 'choice'

the error messages must be tested for all possible productions in the grammar, the test criteria are:
1) every path has to show one or  more 'choices'
2) watch out for paths with a message for a single 'choice' where actually more choices are possible. This 
can happen e.g. in pList .>> q if the pList is a separated list and the parser matches a single element 
of the list, now expecting q. Actually, we expect q or a separator from pList. The error might be that 
your pList is probably accepting an optional separator at the end and you might consider it an error in your grammar to correct it.





*)
