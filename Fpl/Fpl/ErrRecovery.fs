module ErrRecovery

open System.Text.RegularExpressions
open System.Collections.Generic
open FParsec
open FplGrammarCommons
open FplGrammarTypes



(* MIT License

Copyright (c) 2023 bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 
*)

type DiagnosticEmitter =
    | FplParser
    | FplInterpreter

type DiagnosticSeverity =
    | Error
    | Warning
    | Hint
    | Information

type DiagnosticMessage =
    | DiagnosticMessage of string

    member this.Value =
        match this with
        | DiagnosticMessage(value) -> value

type Diagnostic =
    | Diagnostic of DiagnosticEmitter * DiagnosticSeverity * Position * DiagnosticMessage

    member this.Emitter =
        match this with
        | Diagnostic(emitter, _, _, _) -> emitter

    member this.Severity =
        match this with
        | Diagnostic(_, severity, _, _) -> severity

    member this.Position =
        match this with
        | Diagnostic(_, _, position, _) -> position

    member this.Message =
        match this with
        | Diagnostic(_, _, _, message) -> message

type Diagnostics() =
    let myHashset = new HashSet<Diagnostic>()
    member this.Collection = myHashset

    member this.AddDiagnostic d =
        if not (myHashset.Contains(d)) then
            myHashset.Add(d) |> ignore

    member this.PrintDiagnostics =
        for d in myHashset do
            printfn "%O" d

        printfn "%s" "^------------------------^\n"

    member this.DiagnosticsToString = myHashset |> Seq.map string |> String.concat "\n"
    member this.Clear() = myHashset.Clear()

let ad = Diagnostics()

let private _position: Parser<_, _> = fun stream -> Reply stream.Position

/// A helper function replacing a list of strings into a HashSet
let listToHashSet (list: string list) =
    System.Collections.Generic.HashSet<string>(list)

/// A helper function looking for all quoted substrings in a diagnostic error message
let findQuotedSubstrings (input: string) =
    let regex = System.Text.RegularExpressions.Regex("'(.*?)'")

    regex.Matches(input)
    |> Seq.cast<System.Text.RegularExpressions.Match>
    |> Seq.map (fun m -> m.Groups.[1].Value)
    |> List.ofSeq


let retrieveExpectedParserChoices (errMsg:string) =
    if errMsg.Contains("Cannot use ") then 
        "'" + invalidSymbol + "'"
    else
        let newErrMsgSplit = errMsg.Split("Expecting: ")
        let last = Array.last newErrMsgSplit
        last.Replace("or ",",").Replace(System.Environment.NewLine, " ").Trim()
        |> fun s -> s.Split([|','|])
        |> Array.map (fun s -> s.Trim())
        |> Array.sort
        |> String.concat ", "

let mapErrMsgToRecText (errMsg: string) =
    let result choices =
        let keyFound, text = recoveryMap.TryGetValue(choices)
        if keyFound then
            (Some choices, Some text)
        else
            (Some choices, None)

    result (retrieveExpectedParserChoices errMsg)



/// Returns the first list element from `candidates` that is contained in `source`.
let findRecoveryString (source: HashSet<string>) (candidates: string list) =
    candidates |> List.tryFind (fun candidate -> source.Contains(candidate))

/// adds an offset parser Position to a given Position
let private addPos (pos: Position) (offset: Position) =
    let newColumn =
        if offset.Line = 0 then
            pos.Column
        else
            pos.Column + offset.Column

    Position(pos.StreamName, pos.Index + offset.Index, pos.Line + offset.Line, newColumn)

let private _zeroOffsetPos = Position("", 0, 0, 0)

/// subtracts an offset parser Position from a given Position
let private subtractPos (pos: Position) (offset: Position) =
    let newColumn =
        if offset.Line = 0 then
            pos.Column
        else
            pos.Column - offset.Column

    Position(pos.StreamName, pos.Index + offset.Index, pos.Line - offset.Line, newColumn)


/// Emit any errors occurring in the globalParser
/// This is to make sure that the parser will always emit diagnostics,
/// even if the error recovery fails on a global level (and so does the parser).
let rec tryParse globalParser (input: string) (lastRecoveryText: string) (cumulativeIndexOffset: int64) =
    match run globalParser input with
    | Success(result, restInput, userState) ->

        (result, "")
    | Failure(errorMsg, restInput, userState) ->
        let (choices, nextRecoveryString) = mapErrMsgToRecText errorMsg

        match (choices, nextRecoveryString) with
        | (None, None) ->
            // this case should never happen because mapErrMsgToRecText never returns this
            let diagnosticMsg =
                DiagnosticMessage(errorMsg + System.Environment.NewLine + "(unknown parser choice or no recovery string provided)")

            let diagnostic =
                Diagnostic(DiagnosticEmitter.FplParser, DiagnosticSeverity.Error, restInput.Position, diagnosticMsg)

            ad.AddDiagnostic diagnostic
            (Ast.Error, "<not found>")
        | (Some cho, None) -> 
            let diagnosticMsg = DiagnosticMessage("Expecting: " + cho + System.Environment.NewLine + "(unknown parser choice)")

            let diagnostic =
                Diagnostic(DiagnosticEmitter.FplParser, DiagnosticSeverity.Error, restInput.Position, diagnosticMsg)

            ad.AddDiagnostic diagnostic
            (Ast.Error, "<not found>")
        | (None, Some recStr) ->
            // this case should never happen because mapErrMsgToRecText never returns this
            let diagnosticMsg = DiagnosticMessage(errorMsg + System.Environment.NewLine + "(unknown error)")

            let diagnostic =
                Diagnostic(DiagnosticEmitter.FplParser, DiagnosticSeverity.Error, restInput.Position, diagnosticMsg)

            ad.AddDiagnostic diagnostic
            (Ast.Error, "<not found>")
            
        | (Some cho, Some recStr) ->
            let (newInput, newRecoveryText, newIndexOffset) =
                manipulateString input recStr restInput.Position lastRecoveryText cumulativeIndexOffset

            if not (newRecoveryText.StartsWith(lastRecoveryText.Replace(invalidSymbol,recStr))) || lastRecoveryText = "" then
                // emit diagnostic if there is a new remainingInput
                let diagnosticMsg = DiagnosticMessage("Expecting: " + cho + System.Environment.NewLine)

                let diagnostic =
                    // this is to ensure that the input insertions of error recovery remain invisible to the user
                    // so that when double-clicking the error, the IDE will go to the right position in the source code
                    let correctedErrorPosition =
                        if lastRecoveryText = "" then
                            restInput.Position
                        else
                            Position(
                                restInput.Position.StreamName,
                                restInput.Position.Index - newIndexOffset,
                                restInput.Position.Line,
                                restInput.Position.Column
                            )

                    Diagnostic(
                        DiagnosticEmitter.FplParser,
                        DiagnosticSeverity.Error,
                        correctedErrorPosition,
                        diagnosticMsg
                    )

                ad.AddDiagnostic diagnostic

            tryParse globalParser newInput newRecoveryText newIndexOffset
