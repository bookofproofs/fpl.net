(* Copyright (c) 2021+ bookofproofs See LICENSE in the project root for license terms. *)

/// <summary>
/// This module contains the final FPL parser including error recovery
/// producing an abstract syntax tree out of a given FPL code.
/// </summary>
module Fpl.Parser.Main
open System
open System.Collections.Generic
open System.Text
open System.Text.RegularExpressions
open Fpl.Primitives
open Fpl.Parser.Types
open Fpl.Errors.Diagnostics
open FParsec
open Fpl.Parser.Grammar
open Fpl.Parser.Formatting

/// <summary>
/// Regex used for the error recovery of FPL blocks. Matches FPL block keywords as whole words
/// followed by whitespace.
/// </summary>
/// <remarks>
/// Pattern is composed from literals defined in grammar and intended for chunking the input
/// during error-tolerant parsing.
/// </remarks>
let errRecoveryBlocks = $"\\b({LiteralDefL}|{LiteralDef}|{LiteralAxL}|{LiteralAx}|{LiteralPostL}|{LiteralPost}|{LiteralThmL}|{LiteralThm}|{LiteralPropL}|{LiteralProp}|{LiteralLemL}|{LiteralLem}|{LiteralCorL}|{LiteralCor}|{LiteralConjL}|{LiteralConj}|{LiteralPrfL}|{LiteralPrf}|{LiteralInfL}|{LiteralInf}|{LiteralLocL}|{LiteralLoc}|{LiteralExtL}|{LiteralExt}|{LiteralUses})\\s+"

/// <summary>
/// Return all matches for the given regular expression pattern against the provided input.
/// </summary>
/// <param name="pattern">Regular expression pattern to search for.</param>
/// <param name="input">Input text to search within.</param>
/// <returns>List of <see cref="System.Text.RegularExpressions.Match"/> instances found in the input.</returns>
/// <remarks>
/// Uses <see cref="System.Text.RegularExpressions.Regex.Matches"/> and casts the returned
/// collection to a list for convenience.
/// </remarks>
/// <exceptions>
/// <exception cref="System.ArgumentException">Thrown when <paramref name="pattern"/> is not a valid regular expression.</exception>
/// </exceptions>
let private getMatches (pattern: string) (input: string) =
    Regex.Matches(input, pattern)
    |> Seq.cast<Match>
    |> Seq.toList

/// <summary>
/// Run the full AST parser on the provided remainder and collect diagnostics for that chunk.
/// </summary>
/// <param name="input">Text representing the remainder to parse.</param>
/// <param name="errorList">Mutable list to which discovered error AST nodes will be appended.</param>
/// <param name="origLines">Original input split into lines (used to compute positions).</param>
/// <param name="origLength">Original input length (used for position computations).</param>
/// <returns>Unit. Diagnostics are appended to <paramref name="errorList"/>.</returns>
/// <remarks>
/// This function executes the standard parser and converts parser failures into error AST nodes
/// using the project's diagnostic helpers.
/// </remarks>
let private collectErrorsIfAny input (errorList:List<Ast list>) origLines origLength =
    match run (stdParser .>> eof) (input) with
    | Failure(errorMsg, _, _) ->
        errorList.Add (getErrorNodes errorMsg origLines origLength)
    | _ -> ()

/// <summary>
/// Produce a parseable variant of the original input by chunking on FPL block keywords and masking
/// syntactically invalid regions. Also collect syntactic error AST nodes extracted from failed chunks.
/// </summary>
/// <param name="input">Original FPL source with comments removed.</param>
/// <param name="origLines">Original input split into lines (used for computing diagnostics positions).</param>
/// <param name="origLength">Length of the original input string.</param>
/// <returns>
/// A tuple containing:
/// - The transformed input string where unrecoverable regions are masked but line lengths preserved.
/// - A list of syntax error AST nodes extracted from the problematic chunks.
/// </returns>
/// <remarks>
/// The function performs a two-stage attempt per chunk: a strict parse that must consume the whole
/// chunk, and a lenient parse (without EOF) to preserve any successfully parsed prefix. Masking
/// preserves layout so other diagnostics remain correctly positioned.
/// </remarks>
let private getParseableInputAndErrorNodes input origLines origLength =
    let matches = getMatches errRecoveryBlocks input
    let parseAbleInput = StringBuilder()
    let maskedPrefix = StringBuilder()
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
            let remainder = maskedPrefix.ToString() + chu
            chu, mChu, remainder

    // Iterate once for each relevant chunk (if no matches, still run once)
    let upper =
        if matches.Length = 0 then
            0
        else
            matches.Length - 1
        
    if matches.Length > 0 && matches[0].Index>0 then
        let prefix1 = input.Substring(0,matches[0].Index)
        collectErrorsIfAny prefix1 errorList origLines origLength
        let maskedPrefix1 = masked prefix1
        maskedPrefix.Append(maskedPrefix1) |> ignore

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
            collectErrorsIfAny remainder errorList origLines origLength
        maskedPrefix.Append(maskedChunk) |> ignore

    parseAbleInput.ToString(), errorList |> Seq.toList |> List.concat

/// <summary>
/// Extract the list of building-block ASTs from a top-level AST node (namespace/AST wrapper).
/// </summary>
/// <param name="topAst">Top-level AST produced by the standard parser.</param>
/// <returns>List of building-block AST nodes contained in the provided top-level AST.</returns>
/// <remarks>
/// Recursively descends wrapper AST nodes to return the contained building block sequence.
/// </remarks>
let private getBuildingBlockAsts (topAst:Ast) =
    let rec getBlocks (subAst:Ast) = 
        match subAst with 
        | Ast.AST ((pos1, pos2), ast) -> 
            getBlocks ast
        | Ast.Namespace (buildingBlocksAsts) ->
            buildingBlocksAsts 
        | _ -> []
    getBlocks topAst

/// <summary>
/// Full FPL parser entry point that returns building-block ASTs and a success flag.
/// </summary>
/// <param name="fplCode">Raw FPL source code to parse.</param>
/// <returns>
/// A tuple containing:
/// - A list of AST nodes representing building blocks or syntax-error placeholders.
/// - A boolean indicating whether the parse completed without syntax recovery (true if no recovery was needed).
/// </returns>
/// <remarks>
/// On an initial parse failure the function attempts error-tolerant chunking and masking to recover
/// as many building blocks as possible while producing diagnostics for faulty regions.
/// </remarks>
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
            let sortingComparer group subGroup = $"{group} {subGroup}" 
            let indexFormat (pos:Position) = sprintf "%0*d" 10 (pos.Index)
            resultWithSyntaxErrors
            |> List.sortBy (fun buildingBlockAst ->
                match buildingBlockAst with
                | Ast.BuildingBlock((pos1,_),_) -> sortingComparer (indexFormat pos1) ""
                | Ast.ErrorSyntax((pos1,_),_) -> sortingComparer (indexFormat pos1) ""
                | Ast.ErrorSyntaxBacktracking((pos1,_),_) -> sortingComparer (indexFormat pos1) ""
                | Ast.ErrorSyntaxChain(((pos1,_),maxPos),(_, chain)) -> sortingComparer (indexFormat maxPos) chain
                | _ -> sortingComparer "ZZZ" ""
            ), false
        | Failure(errorMsg, _, _) ->
            getErrorNodes errorMsg origLines origLength, false

/// <summary>
/// Return parser choice suggestions for a given input position.
/// </summary>
/// <param name="input">Full FPL input (comments will be removed internally).</param>
/// <param name="index">Position index (character offset) within the input to query choices for.</param>
/// <returns>
/// A tuple containing:
/// - A list of textual parser choices (possibly empty) describing expected tokens at the position.
/// - The parser position index used as reference for the choices.
/// </returns>
/// <remarks>
/// Attempts to parse the input prefix up to <paramref name="index"/> and, on failure, maps the raw
/// parser error into a human-friendly choices list and an adjusted position.
/// </remarks>
let getParserChoicesAtPosition (input:string) index =
    let newInput = input |> removeFplComments
    match run (stdParser .>> eof) (newInput.Substring(0, index))  with
    | Success(result, restInput, userState) -> 
        // In the success case, we always return the current parser position in the input
        List.empty, userState.Index
    | Failure(errorMsg, restInput, userState) ->
        let newErrMsg, choices = mapErrMsgToRecText input errorMsg restInput.Position
        choices, restInput.Position.Index

/// <summary>
/// Test harness that runs a specific parser production on the given input and returns the raw result.
/// </summary>
/// <param name="parserType">Name of the parser production to test (one of the known literal names).</param>
/// <param name="input">Input text for the selected parser production.</param>
/// <returns>A string representation of the parser result (Success/Failure) for diagnostics/testing.</returns>
/// <remarks>
/// Used by the language-service test code to validate individual parser productions from external code.
/// Unknown <paramref name="parserType"/> values return a short not-implemented message.
/// </remarks>
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
        let result = run (varDeclOrSpecList .>> eof) trimmed 
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
    | PrimQuantifier ->
        let result = run (compoundPredicate .>> eof) trimmed 
        sprintf "%O" result
    | PrimTheoremLike ->
        let result = run (buildingBlock .>> eof) trimmed 
        sprintf "%O" result
    | _ -> $"testParser {parserType} not implemented"
