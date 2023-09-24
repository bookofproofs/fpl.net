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
    let myList = new List<Diagnostic>()
    member this.List with get() = myList
    member this.AddDiagnostic d = myList.Add(d)
    member this.PrintDiagnostics = 
        for d in myList do printfn "%O" d
        printfn "%s" "^------------------------^\n" 
    member this.DiagnosticsToString = 
        myList
        |> Seq.map string
        |> String.concat "\n"
    member this.Clear() = myList.Clear()

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
    preturn ()

/// A helper parser that skips any characters until innerSeparator would succeed,
/// but where innerSeparator does not consume any input, 
/// unless, at the same position, outerSeparator occurs.
let skipUntilLookaheadSeparatorFail innerSeparator outerSeparator = 
    skipMany (notFollowedBy (attempt innerSeparator <|> outerSeparator) >>. anyChar)

let abc a b c (aName:string) (bName:string) (cName:string) (ad:Diagnostics) =
    let aMissing = 
        getPosition >>= fun pos -> 
        b .>> c >>= fun r -> 
            emitDiagnostics1 ad ("missing opening " + aName) pos |> ignore
            preturn r
    let cMissing = 
        getPosition >>= fun pos -> 
        a >>. b >>= fun r -> 
            emitDiagnostics1 ad ("missing closing " + cName) pos |> ignore
            preturn r
    let acMissing = 
        getPosition >>= fun pos -> 
        b >>= fun r -> 
            emitDiagnostics1 ad ("missing opening " + aName) pos |> ignore
            getPosition >>= fun pos -> 
            emitDiagnostics1 ad ("missing closing " + cName) pos |> ignore
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
/// but where innerSeparator does not consume any input.
let skipUntilLookaheadSeparator innerSeparator = 
    skipMany (notFollowedBy innerSeparator >>. anyChar)

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
*)
