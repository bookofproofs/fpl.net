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

        printfn "%s" "\n^------------------------^\n"

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
let replaceFParsecErrMsgForFplParser (errMsg: string) (choices:string) (pos: Position)=
    let lines = errMsg.Split(Environment.NewLine)
    let firstLine = lines.[1]
    let caretLine = lines.[2]

    let carretPos = caretLine.IndexOf('^')
    // Find the position of the caret
    let caretPosition, newPos = 
        if errMsg.Contains("<variable (got") then
            let pre = firstLine.Substring(0,carretPos).TrimEnd()
            let keywordLength = lengthOfEndingFplKeyword pre
            let np = Position(
                        pos.StreamName,
                        pos.Index - (int64 keywordLength),
                        pos.Line,
                        pos.Column - (int64 keywordLength)
                    )
            carretPos - keywordLength, np
        else
            carretPos, pos

    // Extract the significant characters
    let significantCharacters =
        let search = Regex.Match(firstLine.Substring(caretPosition), @"\S+").Value
        if search = "" then 
            firstLine.Trim()
        else
            search

    // Replace the significant characters with quoted version in the first line
    let quotedFirstLine = sprintf "'%s'" significantCharacters

    quotedFirstLine + Environment.NewLine + "Expecting: " + (wrapEveryNthComma choices 8), newPos

let split = [|" or "; "or" + Environment.NewLine ; "or\r" ; "or "; ", "; "," + Environment.NewLine; Environment.NewLine + Environment.NewLine|]
let groupRegex = "(?<=Expecting: )(.+?)(?=(Expecting|(\n.+)+|$))"
let retrieveExpectedParserChoices (errMsg:string) =
    // replace accidental new lines injected by FParsec into FPL parser labels that start by "<" and end by ">"
    // to avoid them from being split apart later
    let replacement (m:Match) = m.Value.Replace(Environment.NewLine, " ")
    let errMsgMod = Regex.Replace(errMsg, "(?<=<)[^>]*", MatchEvaluator(replacement))
    // now, look in this manipulated error FParsec message by looking for groups 
    // starting with "Expecting: " and followed by an enumeration of choices
    let regex = Regex(groupRegex)
    let matches = regex.Matches(errMsgMod)
    // now, we extract all choices so they contain only those parts of the matched group that
    // are choice enumerations relevant for us.
    let hashSet = System.Collections.Generic.HashSet<string>()
    for m in matches do
        for g in m.Groups do
            let s = g.Value.Split(split, System.StringSplitOptions.RemoveEmptyEntries)
            let trimmedS = 
                s 
                |> Array.map (fun str -> str.Trim() ) // trim all strings
                |> Array.filter (fun str -> str.Length > 0) // ignore all empty strings
                |> Array.map (fun str -> if str.EndsWith(",") then str.Substring(0, str.Length - 1) else str) // remove incidental trailing commas 
                |> Array.filter (fun str -> (str.StartsWith("<") && str.EndsWith(">")) || (str.StartsWith("'") && str.EndsWith("'"))) // filter out all split elements that are not quoted or start with "<"
            let hs = System.Collections.Generic.HashSet<string>(trimmedS)
            hashSet.UnionWith(hs) |> ignore
    // return the choices as a comma-separated list of alphabetically sorted choices
    let choices = 
        hashSet
        |> Seq.toList
        |> List.sort
        |> String.concat ", "
    let choicesModCW = 
        hashSet
        |> Seq.toList
        |> List.sort
        |> List.filter (fun str -> str<>labelWhitespace && str<>labelBlockComment && str<>labelInlineComment ) // filter out all elements that will never become choices for error recovery
        |> String.concat ", "
    choices, choicesModCW


let getLineOffset (input: string) (line:int)=
    let lines = input.Split(Environment.NewLine)
    let lengthLineSep = Environment.NewLine.Length
    let mutable offset = 0
    for i in 0..(line - 2) do
        offset <- offset + lines.[i].Length + lengthLineSep
    offset

let getLineAndColumn (input: string) =
    let regex = System.Text.RegularExpressions.Regex("Error in Ln: (\\d+) Col: (\\d+)")
    let m = regex.Match(input)
    if m.Success then
        let line = int m.Groups.[1].Value
        let column = int m.Groups.[2].Value
        Some(line, column)
    else
        None

let getLastSubstringAfterSeparator (input:string) (sep:string) =
    let substrings = input.Split(sep)
    if substrings.Length > 0 then
        substrings.[substrings.Length - 1].Trim()
    else
        ""

let extractBacktrackingFreeErrMsgAndPos (input: string) (errMsg: string) (pos:Position) =
    let backtrackingFreeErrMsg = getLastSubstringAfterSeparator errMsg "backtracked after:"
    let lineColumn = getLineAndColumn backtrackingFreeErrMsg
    match lineColumn with
    | Some(line, column) -> 
        let index = column + getLineOffset input line - 1
        let backtrackingFreePos = Position(
                                    pos.StreamName,
                                    index,
                                    line,
                                    column 
                                )
        (backtrackingFreeErrMsg, backtrackingFreePos)
    | None ->
        (errMsg, pos)

/// A low-level error recovery function mapping error messages to tuples
/// consisting of some parser choices, a pick from this choices and a replace
/// error message that is formatted suitable for the VS Code Problems window.
let mapErrMsgToRecText (input: string) (errMsg: string) (pos:Position) =
    let recText choices =
        let keyFound, text = recoveryMap.TryGetValue(choices)
        if keyFound then
            Some text
        else
            None
    // extract from errMsg only this part that is contained in the last 
    // part of the FParsec error message after any before FParsec started any 
    // backtracking to find another syntax error. We consider those
    // "other syntax errors" irrelevant for a correct error recovery process 
    // because we want to report only those errors that the user 
    // is most likely to handle while typing FPL source code.
    let backtrackingFreeErrMsg,  backtrackingFreePos = extractBacktrackingFreeErrMsgAndPos input errMsg pos 
    let test = input.Substring(int backtrackingFreePos.Index)

    let choices, choicesModCW = retrieveExpectedParserChoices backtrackingFreeErrMsg
    let text = recText choicesModCW
    let newErrMsg, modifiedPos = replaceFParsecErrMsgForFplParser backtrackingFreeErrMsg choices backtrackingFreePos
    (Some choices, text, newErrMsg, modifiedPos)

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

        result
    | Failure(errorMsg, restInput, userState) ->
        let (choices, nextRecoveryString, newErrMsg, backtrackingFreePos) = mapErrMsgToRecText input errorMsg restInput.Position

        match (choices, nextRecoveryString) with
        | (None, None) ->
            // this case should never happen because mapErrMsgToRecText never returns this
            let diagnosticMsg =
                DiagnosticMessage(errorMsg + System.Environment.NewLine + "(unknown parser choice or no recovery string provided)")

            let diagnostic =
                Diagnostic(DiagnosticEmitter.FplParser, DiagnosticSeverity.Error, backtrackingFreePos, diagnosticMsg)

            ad.AddDiagnostic diagnostic
            Ast.Error
        | (Some cho, None) -> 
            let diagnosticMsg = DiagnosticMessage(newErrMsg + System.Environment.NewLine + "(unknown parser choice)")

            let diagnostic =
                Diagnostic(DiagnosticEmitter.FplParser, DiagnosticSeverity.Error, backtrackingFreePos, diagnosticMsg)

            ad.AddDiagnostic diagnostic
            Ast.Error
        | (None, Some recStr) ->
            // this case should never happen because mapErrMsgToRecText never returns this
            let diagnosticMsg = DiagnosticMessage(errorMsg + System.Environment.NewLine + "(unknown error)")

            let diagnostic =
                Diagnostic(DiagnosticEmitter.FplParser, DiagnosticSeverity.Error, backtrackingFreePos, diagnosticMsg)

            ad.AddDiagnostic diagnostic
            Ast.Error
            
        | (Some cho, Some recStr) ->
            let (newInput, newRecoveryText, newOffset, keyWordLength, fatalError) =
                manipulateString input recStr backtrackingFreePos lastRecoveryText
            
            if fatalError then
                let diagnosticMsg = DiagnosticMessage(System.Environment.NewLine + "(recovery failed due to likely infinite loop)")

                let diagnostic =
                    Diagnostic(DiagnosticEmitter.FplParser, DiagnosticSeverity.Error, backtrackingFreePos, diagnosticMsg)

                ad.AddDiagnostic diagnostic
                Ast.Error
            else
                let lastRecoveryTextMod = lastRecoveryText.Replace(invalidSymbol,recStr)
                let newIndexOffset = cumulativeIndexOffset + newOffset
                
                let cond1 = not (newRecoveryText.StartsWith(lastRecoveryTextMod))
                let cond2 = newRecoveryText="{ pred "
                let cond3 = newRecoveryText="T T "
                let cond4 = newRecoveryText="T { "
                let cond5 = not (lastRecoveryTextMod.EndsWith("{ ")) 
                let cond6 = lastRecoveryTextMod.EndsWith("( ) { ")
                let cond7 = not (newRecoveryText = "} ")
                let cond8 = not (lastRecoveryText = "intr ")
                let cond0 = lastRecoveryText = ""
                // emit diagnostics using a heuristics while preventing false positives 
                let conditionForEmittingDiagnostics = 
                    cond0 
                    || ( 
                            ( cond1 || cond2 || cond3 || cond4) 
                            && (cond5 || cond6) 
                            && (cond7 || cond8)
                       ) 
                if conditionForEmittingDiagnostics then
                    // emit diagnostic if there is a new remainingInput

                    let diagnosticMsg = DiagnosticMessage(newErrMsg + System.Environment.NewLine)

                    let diagnostic =
                        // this is to ensure that the input insertions of error recovery remain invisible to the user
                        // so that when double-clicking the error, the IDE will go to the right position in the source code
                        let correctedErrorPosition =
                            if newRecoveryText = "§ " then
                                let newIndexOffset = newIndexOffset - keyWordLength - int64 2
                                Position(
                                    backtrackingFreePos.StreamName,
                                    backtrackingFreePos.Index + newIndexOffset,
                                    backtrackingFreePos.Line,
                                    backtrackingFreePos.Column 
                                )
                            elif errorMsg.Contains("The parser backtracked after") then
                                Position(
                                    backtrackingFreePos.StreamName,
                                    backtrackingFreePos.Index + (int64 1),
                                    backtrackingFreePos.Line,
                                    backtrackingFreePos.Column 
                                )
                            elif lastRecoveryText = "" then
                                backtrackingFreePos
                            else
                                Position(
                                    backtrackingFreePos.StreamName,
                                    backtrackingFreePos.Index - cumulativeIndexOffset,
                                    backtrackingFreePos.Line,
                                    backtrackingFreePos.Column 
                                )

                        Diagnostic(
                            DiagnosticEmitter.FplParser,
                            DiagnosticSeverity.Error,
                            correctedErrorPosition,
                            diagnosticMsg
                        )

                    ad.AddDiagnostic diagnostic


                tryParse globalParser newInput newRecoveryText newIndexOffset 

/// A simple helper function for printing trace information to the console (taken from FParsec Docs)
let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
    fun stream ->
        printfn "%A: Entering %s" stream.Position label
        let reply = p stream
        printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
        reply
