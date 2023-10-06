module ErrRecovery

open System
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


let wrapEveryNthComma (str: string) (n:int) =
    str.Split(", ")
    |> Array.mapi (fun i s -> if i <> 0 && i % n = 0 then Environment.NewLine + s.Trim() else s.Trim())
    |> String.concat ", "

/// A helper replacing the FParsec error string by a string that can be better displayed in the VSCode problem window
let replaceFParsecErrMsgForFplParser (errMsg: string) (choices:string) =
    let lines = errMsg.Split(Environment.NewLine)
    let firstLine = lines.[1]
    let caretLine = lines.[2]

    // Find the position of the caret
    let caretPosition = caretLine.IndexOf('^')

    // Extract the significant characters
    let significantCharacters =
        Regex.Match(firstLine.Substring(caretPosition), @"\S+").Value

    // Replace the significant characters with quoted version in the first line
    let quotedFirstLine = sprintf "'%s'" significantCharacters

    // Join the transformed first line and the rest of the lines with a newline character to form the final output
    if errMsg.Contains("Cannot use ") then
        lines[3] // return only the line containing the relevant message with "Cannot use ..."
    else
        // els return a line with a quoted string that caused the error followed by a sorted list of syntactically
        // corrected choices
        quotedFirstLine + Environment.NewLine + "Expecting: " + (wrapEveryNthComma choices 8)

let retrieveExpectedParserChoices (errMsg:string) =
    if errMsg.Contains("Cannot use ") then 
       "'" + invalidSymbol + "'"
    else
        let errMsgMod = errMsg.Replace(Environment.NewLine, " ")
        let regex = System.Text.RegularExpressions.Regex("Expecting: (.*)")
        let matches = regex.Matches(errMsgMod)
        let hashSet = System.Collections.Generic.HashSet<string>()
        for m in matches do
            let s = m.Groups.[1].Value.Split([|" or "; ", "|], System.StringSplitOptions.RemoveEmptyEntries)
            let trimmedS = s |> Array.map (fun str -> str.Trim())
            let hs = System.Collections.Generic.HashSet<string>(trimmedS)
            hashSet.UnionWith(hs) |> ignore
        hashSet
        |> Seq.toList
        |> List.sort
        |> String.concat ", "

let mapErrMsgToRecText (errMsg: string) =
    let recText choices =
        let keyFound, text = recoveryMap.TryGetValue(choices)
        if keyFound then
            (Some text)
        else
            (None)
    let choices = retrieveExpectedParserChoices errMsg
    let text = recText choices
    (Some choices, text, replaceFParsecErrMsgForFplParser errMsg choices)

/// Returns the first list element from `candidates` that is contained in `source`.
let findRecoveryString (source: HashSet<string>) (candidates: string list) =
    candidates |> List.tryFind (fun candidate -> source.Contains(candidate))

let tryParse' globalParser expectMessage (ad: Diagnostics) input =
    match run globalParser input with
    | Success(result, restInput, userState) -> result
    | Failure(errorMsg, restInput, userState) ->
        let diagnosticMsg = DiagnosticMessage(expectMessage + " " + errorMsg)

        let diagnostic =
            Diagnostic(DiagnosticEmitter.FplParser, DiagnosticSeverity.Error, restInput.Position, diagnosticMsg)

        ad.AddDiagnostic diagnostic
        Ast.Error

/// Emit any errors occurring in the globalParser
/// This is to make sure that the parser will always emit diagnostics,
/// even if the error recovery fails on a global level (and so does the parser).
let rec tryParse globalParser (input: string) (lastRecoveryText: string) (cumulativeIndexOffset: int64) =
    match run globalParser input with
    | Success(result, restInput, userState) ->

        (result, "")
    | Failure(errorMsg, restInput, userState) ->
        let (choices, nextRecoveryString, newErrMsg) = mapErrMsgToRecText errorMsg

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
            let diagnosticMsg = DiagnosticMessage(newErrMsg + System.Environment.NewLine + "(unknown parser choice)")

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

                let diagnosticMsg = DiagnosticMessage(newErrMsg + System.Environment.NewLine)

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
