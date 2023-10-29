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

type DiagnosticCode = 
    | DiagnosticCode of string * string

    member this.Code =
        match this with
        | DiagnosticCode(code, msg) -> code

    member this.OrigMsg =
        match this with
        | DiagnosticCode(code, msg) -> msg

    member this.CodeMessage = 
        match this.Code with 
        | "DEF000" -> 
            if this.OrigMsg.StartsWith("':'") then 
                ("Syntax error in definition" + ": " + this.OrigMsg).Replace("Expecting:", "Missing '~' before the variable(s) or expecting:")
            else
                "Syntax error in definition" + ": " + this.OrigMsg 
        | "PRP000" -> "Syntax error in property" + ": " + this.OrigMsg 
        | "AXI000" -> "Syntax error in axiom" + ": " + this.OrigMsg 
        | "THM000" -> "Syntax error in theorem" + ": " + this.OrigMsg 
        | "COR000" -> "Syntax error in corollary" + ": " + this.OrigMsg 
        | "CNJ000" -> "Syntax error in conjecture" + ": " + this.OrigMsg 
        | "VAR000" -> "Syntax error in variable declaration and/or specification" + ": " + this.OrigMsg 
        | "CTR000" -> "Syntax error in constructor" + ": " + this.OrigMsg 
        | "PRF000" -> "Syntax error in proof" + ": " + this.OrigMsg 
        | "INF000" -> "Syntax error in rule of inference" + ": " + this.OrigMsg 
        | "LOC000" -> "Syntax error in localization" + ": " + this.OrigMsg 
        | "USE000" -> "Syntax error in uses clause" + ": " + this.OrigMsg 
        | "PRE000" -> "Syntax error in predicate" + ": " + this.OrigMsg 
        | "SMT000" -> "Syntax error in statement" + ": " + this.OrigMsg 
        | "SYN000" -> "Other syntax error" + ": " + this.OrigMsg 
        | _ -> failwith ("Unknown code " + this.Code)

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
    | Diagnostic of DiagnosticEmitter * DiagnosticSeverity * Position * DiagnosticMessage * DiagnosticCode

    member this.Emitter =
        match this with
        | Diagnostic(emitter, _, _, _, _) -> emitter

    member this.Severity =
        match this with
        | Diagnostic(_, severity, _, _, _) -> severity

    member this.Position =
        match this with
        | Diagnostic(_, _, position, _, _) -> position

    member this.Message =
        match this with
        | Diagnostic(_, _, _, message, _) -> message

    member this.Code =
        match this with
        | Diagnostic(_, _, _, _, code) -> code

type Diagnostics() =
    let myDictionary = new Dictionary<string, Diagnostic>()
    member this.Collection = 
        myDictionary
        |> Seq.map (fun kvp -> (kvp.Key, kvp.Value))
        |> Seq.sortBy fst
        |> Seq.map snd
        |> Seq.toList

    member this.AddDiagnostic (d:Diagnostic) =
        let keyOfd = (sprintf "%07d" d.Position.Index) + d.Emitter.ToString()
        if not (myDictionary.ContainsKey(keyOfd)) then
            myDictionary.Add(keyOfd, d) |> ignore

    member this.CountDiagnostics  =
        myDictionary.Count

    member this.PrintDiagnostics =
        for d in this.Collection do
            printfn "%O" d

        printfn "%s" "\n^------------------------^\n"

    member this.DiagnosticsToString = this.Collection |> Seq.map string |> String.concat "\n"
    member this.Clear() = myDictionary.Clear()

let ad = Diagnostics()

let private _position: Parser<_, _> = fun stream -> Reply stream.Position

/// A helper replacing the FParsec error string by a string that can be better displayed in the VSCode problem window
let replaceFParsecErrMsgForFplParser (errMsg: string) (choices:string) (pos: Position)=
    let lines = errMsg.Split(Environment.NewLine)
    let firstLine = lines.[1]
    let caretLine = lines.[2]

    let carretPos = caretLine.IndexOf('^')
    if carretPos>=0 then
        // Extract the significant characters
        let significantCharacters =
            let search = Regex.Match(firstLine.Substring(carretPos), @"\S+").Value
            if search = "" then 
                firstLine.Trim()
            else
                search

        // Replace the significant characters with quoted version in the first line
        let quotedFirstLine = sprintf "'%s'" significantCharacters

        quotedFirstLine + Environment.NewLine + "Expecting: " + choices
    else
        errMsg

let split = [|" or "; "or" + Environment.NewLine ; "or\r" ; "or "; " Other error"; Environment.NewLine + "Other error"; ", "; "," + Environment.NewLine; Environment.NewLine + Environment.NewLine; Environment.NewLine|]
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
    choices


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
    // extract from errMsg only this part that is contained in the last 
    // part of the FParsec error message after any before FParsec started any 
    // backtracking to find another syntax error. We consider those
    // "other syntax errors" irrelevant for a correct error recovery process 
    // because we want to report only those errors that the user 
    // is most likely to handle while typing FPL source code.
    let backtrackingFreeErrMsg,  backtrackingFreePos = extractBacktrackingFreeErrMsgAndPos input errMsg pos 

    let choices = retrieveExpectedParserChoices backtrackingFreeErrMsg
    let newErrMsg = replaceFParsecErrMsgForFplParser backtrackingFreeErrMsg choices backtrackingFreePos
    newErrMsg

/// Replaces in the `input` all regex pattern matches by spaces while preserving the new lines
let replaceLinesWithSpaces (input: string) (pattern: string) =
    let regex = new Regex(pattern, RegexOptions.Multiline)
    let evaluator = MatchEvaluator(fun (m: Match) -> 
        m.Value.Split(Environment.NewLine)
        |> Array.map (fun line -> String.replicate line.Length " ")
        |> String.concat Environment.NewLine
    )
    regex.Replace(input, evaluator)

/// Replaces in the `input` all FPL comments by spaces while preserving the new lines
let removeFplComments (input:string) = 
    let r1 = replaceLinesWithSpaces input "\/\/[^\n]*" // replace inline comments
    replaceLinesWithSpaces r1 "\/\*((?:.|\n)*?)\*\/" // replace block comments

/// Replaces in the `input` all strings by spaces while preserving the new lines
let removeStrings (input:string) =
    replaceLinesWithSpaces input "\"[^\"\n]*\"" // replace inline comments

/// Replaces in the `input` all starting non-whitespace characters by as many spaces 
let replaceFirstNonWhitespace str =
    let regex = new Regex("\\S+")
    let replacement = MatchEvaluator(fun m -> String.replicate m.Value.Length " ")
    regex.Replace(str, replacement, 1)

let rec tryParse someParser (ad: Diagnostics) input startIndexOfInput maxIndex (code:string) lastCorrectedPosition =
    match run someParser input with
    | Success(result, restInput, userState) -> 
        result, int userState.Index
    | Failure(errorMsg, restInput, userState) ->
        let newErrMsg = mapErrMsgToRecText input errorMsg restInput.Position

        let correctedPosition = 
            Position(
                restInput.Position.StreamName,
                restInput.Position.Index + (int64 startIndexOfInput),
                restInput.Position.Line,
                restInput.Position.Column 
            )
        let diagnosticCode = DiagnosticCode(code, newErrMsg)
        let diagnosticMsg = DiagnosticMessage(diagnosticCode.CodeMessage )
        let diagnostic =
            Diagnostic(DiagnosticEmitter.FplParser, DiagnosticSeverity.Error, correctedPosition, diagnosticMsg, diagnosticCode)

        ad.AddDiagnostic diagnostic
        let cond0 = lastCorrectedPosition = correctedPosition.Index
        let cond1 = restInput.Position.Index < maxIndex
        let cond2 = startIndexOfInput <= restInput.Position.Index 
        let cond2a = not (restInput.Position.Index < maxIndex)
        let cond3 = (int64 0 < startIndexOfInput)
        let cond = cond0 || cond1 || (cond2 && cond2a && cond3)
        if cond then
            Ast.Error, int maxIndex
        else
            let preErrorString = input.Substring(0, int restInput.Position.Index)
            let postErrorString = replaceFirstNonWhitespace (input.Substring(int restInput.Position.Index))

            tryParse someParser ad (preErrorString + postErrorString) startIndexOfInput maxIndex code correctedPosition.Index 

/// A simple helper function for printing trace information to the console (taken from FParsec Docs)
let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
    fun stream ->
        printfn "%A: Entering %s" stream.Position label
        let reply = p stream
        printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
        reply


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

/// Replaces all comments and strings in a FPL source code by spaces with as many line breaks as in the replacements.
/// This significantly simplifies the grammar because we do not have to parse comments and inject them everywhere in the grammar the could be possible.
/// The string replacement, on the other hand, prevents false positives in the error recovery mechanism in user-defined 
/// localization strings that would otherwise emit diagnostics for regex matches inside those user-defined strings.
let preParsePreProcess (input:string) = 
    input
    |> removeFplComments 
    |> removeStrings


/// Returns an array of tuples with (position,regexMmatch) of string matches based on an error-recovery regex pattern 
/// The pattern has to start with non-whitespace characters (e.g. keywords or other strings that are distinctive for the language)
7// we want to provide with emitting error recovery messages
/// The array will be filtered to include only those matches that really start with that pattern, i.e. are proceeded some whitespace character 
/// in the remaining source code. For instance, if "for" is the pattern than " for" will match, but "_for" will not.
let stringMatches (inputString: string) (pattern: string) =
    let regex = new Regex(pattern)
    let matchList = regex.Matches(preParsePreProcess inputString) |> Seq.cast<Match> |> Seq.toList
    let rec collectMatches matchList index =
        
        match (matchList:Match list) with
        | [] -> 
            (index, inputString.Substring(index)) :: []
        | m::ms ->
            (index, inputString.Substring(index)) :: collectMatches ms m.Index
    collectMatches matchList 0 
    |> List.toArray
    |> Array.filter (fun (i, s) -> 
            let preCharacter = 
                if i > 0 then
                    inputString.Substring(i - 1, 1)
                else
                    "#"
            Regex.IsMatch(preCharacter, "\W")
        )

