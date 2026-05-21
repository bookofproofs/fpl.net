/// This module contains the final FPL parser including error recovery
// producing an abstract syntax tree out of a given FPL code 
module FplParsing.Main
open System
open System.Collections.Generic
open System.Text
open System.Text.RegularExpressions
open FplPrimitives
open FplGrammarTypes
open ErrDiagnostics
open FParsec
open FplParsing.Combinators
open FplParsing.Formatting
(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)

//------------
/// Regex used for the error recovery of FPL blocks using their keywords to be matched as whole words (\b option), followed by space (\s+ option)
let errRecoveryBlocks = $"\\b({LiteralDefL}|{LiteralDef}|{LiteralAxL}|{LiteralAx}|{LiteralPostL}|{LiteralPost}|{LiteralThmL}|{LiteralThm}|{LiteralPropL}|{LiteralProp}|{LiteralLemL}|{LiteralLem}|{LiteralCorL}|{LiteralCor}|{LiteralConjL}|{LiteralConj}|{LiteralPrfL}|{LiteralPrf}|{LiteralInfL}|{LiteralInf}|{LiteralLocL}|{LiteralLoc}|{LiteralExtL}|{LiteralExt}|{LiteralUses})\\s+"

let private getMatches (pattern: string) (input: string) =
    Regex.Matches(input, pattern)
    |> Seq.cast<Match>
    |> Seq.toList

/// Tries to parse all chunks of input of FPL building blocks. If a chunk produces a syntax error,
/// a diagnosics will be issued and the chunk will (at least at its faulty end) be replaced by a masked chunk, where
/// all non-whitespace characters will be replaced with spaces while preserving line breaks and line lengths.
/// Preserving line sizes is necessary to keep other diagnostics well-positioned.
/// The syntax errors are generated using an input starting with the chunk, so syntax error messages
/// realistically reflect the remaining code after the chunk.
let private getParseableInputAndErrorNodes input origLines origLength =
    let matches = getMatches errRecoveryBlocks input
    let parseAbleInput = StringBuilder()
    let errorList = List<Ast list>()


    // Helper to produce chunk, maskedChunk and the 'remainder' used for diagnostics
    let getChunkMaskedAndRemainder i =
        if matches.Length = 0 then
            let chu = input
            let mChu = masked chu
            chu, mChu, chu
        else
            let pos = matches[i].Index
            let len =
                if i < matches.Length - 1 then
                    matches[i + 1].Index - matches[i].Index
                else
                    input.Length - matches[i].Index
            let chu = input.Substring(pos, len)
            let mChu = masked chu
            let remainder =
                if i = 0 && matches[0].Index > 0 then
                    // when there is a prefix before the first match, diagnostics should see the masked prefix + the chunk
                    let prefix = input.Substring(0, matches[0].Index)
                    (masked prefix) + chu
                else
                    chu
            chu, mChu, remainder

    // Iterate once for each relevant chunk (if no matches, still run once)
    let upper =
        if matches.Length = 0 then 0 else matches.Length - 1

    for i in [0..upper] do
        // If there is a non-matching prefix before the first recovery match, preserve its masked form
        if i = 0 && matches.Length > 0 && matches[0].Index > 0 then
            let prefix = input.Substring(0, matches[0].Index)
            parseAbleInput.Append(masked prefix) |> ignore

        let chunk, maskedChunk, remainder = getChunkMaskedAndRemainder i
        let trimedInput = chunk.Trim()

        // Try a strict parse of the building block (must consume all of the trimmed chunk)
        match run (buildingBlock .>> eof) trimedInput with
        | Success(_, _, _) when trimedInput.Length > 0 ->
            parseAbleInput.Append(chunk) |> ignore
        | _ ->
            // Try parsing without eof to see how far we can get
            match run buildingBlock trimedInput with
            | Success(_, _, userState) when trimedInput.Length > 0 ->
                let posSuccess = int userState.Index
                // keep the successfully parsed prefix from the trimmed input
                parseAbleInput.Append(trimedInput.Substring(0, posSuccess)) |> ignore

                // Append masked remainder of this chunk (special-case single match with trailing ';')
                if maskedChunk.Length > posSuccess then
                    if matches.Length = 1 then
                        let rest = chunk.Substring(posSuccess).Trim()
                        if rest = ";" then
                            parseAbleInput.Append(chunk.Substring(posSuccess)) |> ignore
                        else
                            parseAbleInput.Append(maskedChunk.Substring(posSuccess)) |> ignore
                    else
                        parseAbleInput.Append(maskedChunk.Substring(posSuccess)) |> ignore
                else
                    parseAbleInput.Append(maskedChunk) |> ignore

            | _ ->
                // If there were no recovery matches at all, keep the whole chunk,
                // otherwise mask it entirely
                if i = 0 && matches.Length = 0 then
                    parseAbleInput.Append(chunk) |> ignore
                else
                    parseAbleInput.Append(maskedChunk) |> ignore

            // Run the full AST parser on the remainder to produce diagnostics for the chunk
            match run (stdParser .>> eof) remainder with
            | Failure(errorMsg, _, _) ->
                errorList.Add (getErrorNodes errorMsg origLines origLength)
            | _ -> ()
    parseAbleInput.ToString(), errorList |> Seq.toList |> List.concat

let private getBuildingBlockAsts (topAst:Ast) =
    let rec getBlocks (subAst:Ast) = 
        match subAst with 
        | Ast.AST ((pos1, pos2), ast) -> 
            getBlocks ast
        | Ast.Namespace (buildingBlocksAsts) ->
            buildingBlocksAsts 
        | _ -> []
    getBlocks topAst

let fplParser fplCode =
    let input = fplCode |> removeFplComments
    match run (stdParser .>> eof) input with
    | Success(ast, _, _) ->
        getBuildingBlockAsts ast, true
    | _ ->
        let origLines = input.Split(Environment.NewLine)
        let origLength = input.Length
        let parseAbleInput, errorList = getParseableInputAndErrorNodes input origLines origLength
        match run stdParser parseAbleInput with 
        | Success(ast, _, _) ->
            let resultWithSyntaxErrors = errorList @ getBuildingBlockAsts ast
            resultWithSyntaxErrors
            |> List.sortBy (fun buildingBlockAst ->
                match buildingBlockAst with
                | Ast.BuildingBlock((pos1,_),_) 
                | Ast.BuildingBlockError((pos1,_),_) -> pos1.Index
                | _ -> Int64.MaxValue
            ), false
        | Failure(errorMsg, _, _) ->
                getErrorNodes errorMsg origLines origLength, false

let parserDiagnostics = ad

/// Returns the parser choices at position (if any).
let getParserChoicesAtPosition (input:string) index =
    let newInput = input |> removeFplComments
    match run (stdParser .>> eof) (newInput.Substring(0, index))  with
    | Success(result, restInput, userState) -> 
        // In the success case, we always return the current parser position in the input
        List.empty, userState.Index
    | Failure(errorMsg, restInput, userState) ->
        let newErrMsg, choices = mapErrMsgToRecText input errorMsg restInput.Position
        choices, restInput.Position.Index

/// Used only to test FplLS CompletionItems correct syntax, which is written in C# and requires this module to run
let testParser (parserType:string) (input:string) =
    let trimmed = (input.Trim()) |> removeFplComments
    match parserType with 
    | LiteralLoc -> 
        let result = run (localization .>> eof) trimmed
        sprintf "%O" result
    | LiteralAx -> 
        let result = run (axiom .>> eof) trimmed 
        sprintf "%O" result
    | LiteralCases ->
        let result = run (casesStatement .>> eof) trimmed 
        sprintf "%O" result
    | LiteralMapCases ->
        let result = run (mapCases .>> eof) trimmed 
        sprintf "%O" result
    | LiteralCtor ->
        let result = run (constructor .>> eof) trimmed 
        sprintf "%O" result
    | LiteralCor ->
        let result = run (corollary .>> eof) trimmed 
        sprintf "%O" result
    | LiteralDec ->
        let result = run (varDeclBlock .>> eof) trimmed 
        sprintf "%O" result
    | LiteralDef ->
        let result = run (definition .>> eof) trimmed 
        sprintf "%O" result
    | LiteralDel ->
        let result = run (fplDelegate .>> eof) trimmed 
        sprintf "%O" result
    | LiteralExt ->
        let result = run (definitionExtension .>> eof) trimmed 
        sprintf "%O" result
    | LiteralFor ->
        let result = run (forStatement .>> eof) trimmed 
        sprintf "%O" result
    | LiteralIs ->
        let result = run (isOperator .>> eof) trimmed 
        sprintf "%O" result
    | PrimPascalCaseId ->
        let result = run (pascalCaseId .>> eof) trimmed 
        sprintf "%O" result
    | PrimPredicate ->
        let result = run (predicate .>> eof) trimmed 
        sprintf "%O" result
    | LiteralPrf ->
        let result = run (proof .>> eof) trimmed 
        sprintf "%O" result
    | LiteralPrty ->
        let result = run (definitionProperty .>> eof) trimmed 
        sprintf "%O" result
    | PrimQuantor ->
        let result = run (compoundPredicate .>> eof) trimmed 
        sprintf "%O" result
    | PrimTheoremLike ->
        let result = run (buildingBlock .>> eof) trimmed 
        sprintf "%O" result
    | _ -> $"testParser {parserType} not implemented"
