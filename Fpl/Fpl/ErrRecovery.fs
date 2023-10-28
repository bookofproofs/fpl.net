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
    | DiagnosticCode of string

    member this.Value =
        match this with
        | DiagnosticCode(value) -> value

    member this.CodeMessage = 
        match this.CodeMessage with 
        | "DEF000" -> "Syntax error in definition"
        | "PRP000" -> "Syntax error in property"
        | "AXI000" -> "Syntax error in axiom"
        | "THM000" -> "Syntax error in theorem"
        | "CNJ000" -> "Syntax error in conjecture"
        | "VAR000" -> "Syntax error in variable declaration and/or specification"
        | "CTR000" -> "Syntax error in constructor"
        | "PRF000" -> "Syntax error in proof"
        | "INF000" -> "Syntax error in rule of inference"
        | "LOC000" -> "Syntax error in localization"
        | "USE000" -> "Syntax error in uses clause"
        | "SYN000" -> "Other syntax error"
        | _ -> failwith ("Unknown code " + this.Value)

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

    quotedFirstLine + Environment.NewLine + "Expecting: " + choices, newPos

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
    let newErrMsg, modifiedPos = replaceFParsecErrMsgForFplParser backtrackingFreeErrMsg choices backtrackingFreePos
    (Some choices, newErrMsg, modifiedPos)


let tryParse someParser (ad: Diagnostics) input corrIndex (code:string)=
    match run someParser input with
    | Success(result, restInput, userState) -> 
        result, int userState.Index
    | Failure(errorMsg, restInput, userState) ->
        let (choices, newErrMsg, backtrackingFreePos) = mapErrMsgToRecText input errorMsg restInput.Position

        let correctedPosition = 
            Position(
                restInput.Position.StreamName,
                restInput.Position.Index + (int64 corrIndex),
                restInput.Position.Line,
                restInput.Position.Column 
            )
        let diagnosticMsg = DiagnosticMessage(newErrMsg)
        let diagnosticCode = DiagnosticCode(code)
        let diagnostic =
            Diagnostic(DiagnosticEmitter.FplParser, DiagnosticSeverity.Error, correctedPosition, diagnosticMsg, diagnosticCode)

        ad.AddDiagnostic diagnostic
        Ast.Error, int diagnostic.Position.Index

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

let preParsePreProcess (input:string) = 
    removeFplComments input

let stringMatches (inputString: string) =
    let pattern = "(definition|def|mandatory|mand|optional|opt|axiom|ax|postulate|post|theorem|thm|proposition|prop|lemma|lem|corollary|cor|conjecture|conj|declaration|dec|constructor|ctor|proof|prf|inference|inf|localization|loc)\s+"
    let regex = new Regex(pattern)
    let matchList = regex.Matches(inputString) |> Seq.cast<Match> |> Seq.toList
    let rec collectMatches matchList index =
        
        match (matchList:Match list) with
        | [] -> 
            (index, inputString.Substring(index)) :: []
        | m::ms ->
            (index, inputString.Substring(index)) :: collectMatches ms m.Index
    collectMatches matchList 0 

