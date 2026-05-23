/// This module contains the formatting functions and helpers to re-format
/// FParsec-based syntax error messages to make them more suitable for diagnostics
/// error messages in language serverss and IDEs.
module FplParsing.Formatting
open System
open System.Text.RegularExpressions
open FplGrammarTypes
open FParsec

(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)

//------------

/// Splits an FParsec error message to sub-errors (if it contains many)
let private splitByBacktrackMarker (input: string) =
    let pattern = @"\s*The parser backtracked after:\s*Error"
    let split = Regex.Split(input, pattern, RegexOptions.Multiline)
    let result = 
        split
        |> Array.mapi (fun i s ->
            match i with
            | 0 -> s.Replace("Error in ", "FPL syntax error in ")
            | _ -> $"Backtracking syntax error{s}")
        |> Array.toList
    result 

/// Takes the string list output of splitByBacktrackMarker and extracts FParsec positions from the error messages producing a list of tuples (Position, error string).
let private extractPositions (lines: string list) =
    let pattern = @"syntax error in Ln:\s*(\d+)\s*Col:\s*(\d+)\s*"

    lines
    |> List.map (fun line ->
        let m = Regex.Match(line, pattern)
        if m.Success then
            let ln  = int m.Groups.[1].Value
            let col = int m.Groups.[2].Value - 1   // one less than reported (because FParsec 1-based instead of 0-based)
            let pos = Position("", 0L, int64 ln, int64 col)
            pos, line
        else
            // no match → default position
            let pos = Position("", 0L, 0L, 0L)
            pos, line
    )

/// Groups the output list of tuples from the extractPositions function
/// by identical Position, but only aggregates those whose message ends with an 'Expecting:' clause,
/// and then merges all the expectation‑tails into one combined message.
let private aggregateExpecting (items: (Position * string) list) =
    let expectingKey = "Expecting:"

    // Helper: split a message into (prefix, tail) if it contains "Expecting:"
    let trySplitExpecting (s: string) =
        let idx = s.IndexOf(expectingKey)
        if idx >= 0 then
            let prefix = s.Substring(0, idx + expectingKey.Length)
            let tail   = s.Substring(idx + expectingKey.Length).Trim()
            Some(prefix, tail)
        else None

    items
    |> List.groupBy fst
    |> List.collect (fun (pos, group) ->
        // Extract only those with an Expecting: clause
        let withExp =
            group
            |> List.choose (fun (p, s) ->
                match trySplitExpecting s with
                | Some(prefix, tail) -> Some(prefix, tail)
                | None -> None)

        match withExp with
        | [] ->
            // No aggregatable entries → keep original group unchanged
            group

        | (prefix, _) :: _ ->
            // Aggregate all tails
            let tails =
                withExp
                |> List.map snd
                |> String.concat ", "

            let combined = prefix + " " + tails
            [pos, combined])

let private distinguishSyntaxFromBacktrickingErrors (items: (Position * string) list) =
    items
    |> List.map (fun (pos, errMsg) ->
        let lines = errMsg.Split(Environment.NewLine) |> Array.toList
        let modifiedErrMsg =
            match lines with
            | [] -> ""
            | [l1] -> l1 
            | l1 :: l2 :: rest ->
                let restConcatenated =
                    rest
                    |> List.map (fun s -> s.Trim())
                    |> String.concat ""
                if l1.StartsWith("FPL syntax error") then
                    $"SY000:`{l2.Trim()}`{Environment.NewLine}{restConcatenated}" 
                elif l1.StartsWith("Backtracking syntax error") then 
                    $"SY001:`{l2.Trim()}`{Environment.NewLine}{restConcatenated}" 
                else 
                    $"{l1}{l2}{rest}" 
        pos, modifiedErrMsg
    )

/// Scans input line‑by‑line, detecting a line that trims to "^",
/// and inserts ⚡ into the preceding line at the same column,
/// skipping the caret‑line, and preserving all other lines unchanged.
let private insertLightning (input: string) =
    let lines = input.Split(Environment.NewLine) |> Array.toList

    // Process line-by-line with access to previous output line
    let rec loop acc remaining =
        match remaining with
        | [] ->
            acc |> List.rev |> String.concat Environment.NewLine

        | (line:string) :: rest ->
            let trimmed = line.Trim()

            if trimmed = "^" && acc <> [] then
                // caret-line: determine caret column in the *original* line
                let caretCol = line.IndexOf("^")

                // modify the previous line
                let prev = List.head acc
                let prevLen = prev.Length

                let updatedPrev =
                    if caretCol < prevLen then
                        // insert ⚡ at caretCol
                        prev.[0..caretCol-1] + "⚡" + prev.[caretCol..]
                    else
                        // previous line too short → append ⚡
                        prev + "⚡"

                // replace previous line, skip caret-line
                loop (updatedPrev :: (List.tail acc)) rest

            else
                // normal line → append
                loop (line :: acc) rest

    loop [] lines

/// Computes FParsec’s Position.Index based on Line and Column in an input string.
let private computeIndex (pos: Position) (lines: string array) inputLength =
    let lengthNewLine = Environment.NewLine.Length
    // FParsec Position.Line and Column are 1-based
    let lineIdx  = int pos.Line - 1
    let colIdx   = int pos.Column

    if lineIdx >= 0 && lineIdx < lines.Length then
        let line = lines.[lineIdx]

        if colIdx >= 0 && colIdx <= line.Length then
            // sum of lengths of all previous lines + newline characters
            let prefixLength =
                lines
                |> Seq.take lineIdx
                |> Seq.sumBy (fun l -> l.Length + lengthNewLine)   // +1 for line brack

            int64 prefixLength + int64 colIdx
        else
            int64 inputLength
    else
        int64 inputLength

let private replaceLine (line:string) =
    line
        .Trim()
        .Replace("Other error messages:", "")
        .Replace("Expecting:", "")
        .Replace("' or '", ", ")
        .Replace("end of input", "<end of theory>")

/// Transforms an error message of FParserc preserving the first two lines, and if the third line starts with "Expecting:",
/// then flattening all remaining lines into that third line by concatenating them without line breaks.
let private collapseExpectingBlock (input: string) : string =
    let lines = input.Split(Environment.NewLine) |> Array.toList

    let res = 
        match lines with
        | [] -> ""
        | [l1] -> l1
        | [l1; l2] -> l1 + Environment.NewLine + $"Expecting:{replaceLine l2}"
        | l1 :: l2 :: rest ->
            if l2.TrimStart().StartsWith("Expecting:") then
                // Concatenate line 2 with all remaining lines (no newlines)
                let merged =
                    rest
                    |> List.map (fun s -> replaceLine s)
                    |> String.concat " "
                $"{l1}{Environment.NewLine}{l2}{merged}"
            else
                // No special rule → return unchanged
                String.concat Environment.NewLine lines
    res.Substring(6).Trim()

/// Calculates the index Position based on line and column and replaces Positions missing index
/// with some having them.
let private correctPositionIndexBasedOnLineAndColumn (lines:string array) length (items: (Position * string) list) =
    items
    |> List.map (fun (pos, errMsg) ->
        (Position("", computeIndex pos lines length, pos.Line, pos.Column), errMsg)
    )

/// Masks an input, replacing all non-whitespace characters with spaces, preserving line braeks and line lengths.
let masked (input:String) =
    input
    |> Seq.map (fun ch -> if Char.IsWhiteSpace(ch) then ch else ' ')
    |> Seq.toArray
    |> fun arr -> System.String(arr)

/// Transforms an FParsec syntax error message (including the complex ones with parser backtracking)
/// into a list of BuildingBlockError ast nodes that are aggregated by error position they occur.
let getErrorNodes (errorMsg:string) origLines origLength = 
    errorMsg
    |> insertLightning
    |> splitByBacktrackMarker
    |> extractPositions
    |> aggregateExpecting
    |> correctPositionIndexBasedOnLineAndColumn origLines origLength
    |> distinguishSyntaxFromBacktrickingErrors 
    |> List.map (fun (pos, errMsg) ->
        if errMsg.StartsWith("SY000:") then
            Ast.ErrorSyntax((pos, pos), collapseExpectingBlock errMsg)
        else
            Ast.ErrorSyntaxBacktracking((pos, pos), collapseExpectingBlock errMsg)
    )

