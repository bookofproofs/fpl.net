/// <summary>
/// This module contains the final FPL parser including error recovery
/// producing an abstract syntax tree out of a given FPL code.
/// </summary>
module Fpl1Parser.Main
open System
open System.Collections.Generic
open System.Text
open System.Text.RegularExpressions
open Fpl0Base.Primitives
open Fpl0Base.Errors.Diagnostics
open Fpl1Parser.Types
open FParsec
open Fpl1Parser.Grammar
open Fpl1Parser.Formatting
open Fpl1Parser.LSRelated.CommentLexer
open Fpl1Parser.LSRelated.Trivia



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
/// <param name="blockSpan">The source span of the enclosing building block that failed to parse.</param>
/// <param name="verbatim">The verbatim FPL source text of the enclosing building block.</param>
/// <returns>Unit. Diagnostics are appended to <paramref name="errorList"/>.</returns>
/// <remarks>
/// This function executes the standard parser and converts parser failures into error AST nodes
/// using the project's diagnostic helpers. <paramref name="blockSpan"/> and <paramref name="verbatim"/>
/// are stamped identically onto every error node produced for this chunk, per the design that a
/// chunk's building-block span/verbatim text is shared across its whole error chain.
/// </remarks>
let private collectErrorsIfAny input (errorList:List<Ast list>) origLines origLength (blockSpan: Positions) (verbatim: string) =
    match run (stdParser .>> eof) (input) with
    | Failure(errorMsg, _, _) ->
        errorList.Add (getErrorNodes errorMsg origLines origLength blockSpan verbatim)
    | _ -> ()

/// <summary>
/// Produce a parseable variant of the original input by chunking on FPL block keywords and masking
/// syntactically invalid regions. Also collect syntactic error AST nodes extracted from failed chunks.
/// </summary>
/// <param name="input">Original FPL source with comments removed.</param>
/// <param name="originalFplCode">
/// The original FPL source exactly as provided by the caller, before comment removal. Used only to
/// capture verbatim building-block text so error-containing blocks can be reprinted unchanged by the
/// formatting service; relies on <see cref="removeFplComments"/> preserving character offsets 1:1
/// between <paramref name="originalFplCode"/> and <paramref name="input"/>.
/// </param>
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
/// preserves layout so other diagnostics remain correctly positioned. Each chunk corresponds to one
/// building block delimited by <see cref="errRecoveryBlocks"/> keywords (or the whole input, when no
/// such keyword is found at all); its character span and verbatim source (sliced from
/// <paramref name="originalFplCode"/> at the same offsets) are computed once per chunk and passed to
/// <c>collectErrorsIfAny</c> so every error node derived from that chunk shares the same span/verbatim.
/// </remarks>
let private getParseableInputAndErrorNodes input (originalFplCode: string) origLines origLength =
    let matches = getMatches errRecoveryBlocks input
    let parseAbleInput = StringBuilder()
    let maskedPrefix = StringBuilder()
    let errorList = List<Ast list>()

    // Slices originalFplCode at the same [start, start+len) offsets used to slice `input`,
    // relying on removeFplComments being length/offset-preserving.
    let verbatimSlice (start: int) (len: int) =
        let clampedStart = min start originalFplCode.Length
        let clampedLen = max 0 (min len (originalFplCode.Length - clampedStart))
        originalFplCode.Substring(clampedStart, clampedLen)

    let blockSpanOf (start: int) (len: int) : Positions =
        indexToPosition input start, indexToPosition input (start + len)

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
        let prefixSpan = blockSpanOf 0 matches[0].Index
        let prefixVerbatim = verbatimSlice 0 matches[0].Index
        collectErrorsIfAny prefix1 errorList origLines origLength prefixSpan prefixVerbatim
        let maskedPrefix1 = masked prefix1
        maskedPrefix.Append(maskedPrefix1) |> ignore

    for i in [0..upper] do
        // If there is a non-matching prefix before the first recovery match, preserve its masked form
        if i = 0 && matches.Length > 0 && matches[0].Index > 0 then
            let prefix = input.Substring(0, matches[0].Index)
            parseAbleInput.Append(masked prefix) |> ignore


        let chunk, maskedChunk, remainder = getChunkMaskedAndRemainder i
        // This chunk's own [start, start+len) span within `input` (and, by offset-preservation,
        // within `originalFplCode`), used for both the building-block span and verbatim capture.
        let chunkStart, chunkLen =
            if matches.Length = 0 then 0, input.Length
            else
                let s = matches[i].Index
                let l = if i < matches.Length - 1 then matches[i + 1].Index - s else input.Length - s
                s, l
        let chunkSpan = blockSpanOf chunkStart chunkLen
        let chunkVerbatim = verbatimSlice chunkStart chunkLen

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

                // The successfully-parsed prefix (trimedInput.[0 .. posSuccess)) is already committed
                // above as its own, independent BuildingBlock AST node (it will be re-parsed from
                // parseAbleInput by the caller). Stamping the *whole* chunk's span/verbatim on the
                // error node derived from the leftover suffix would therefore duplicate that prefix
                // text: it would be emitted once by the genuine BuildingBlock node and a second time
                // verbatim inside the error node. Instead, compute a span/verbatim that covers only
                // the unconsumed suffix of the chunk, so error reprinting never repeats already-parsed
                // text.
                // Note: use TrimStart's length delta (not Trim's), since chunk.Trim() also strips
                // trailing whitespace, which would overshoot suffixStart by the trailing whitespace
                // amount and truncate the start of the unconsumed suffix.
                let leadingTrimLen = chunk.Length - (chunk.TrimStart()).Length
                let suffixStart = chunkStart + leadingTrimLen + posSuccess
                let suffixLen = chunkStart + chunkLen - suffixStart
                let suffixSpan = blockSpanOf suffixStart suffixLen
                let suffixVerbatim = verbatimSlice suffixStart suffixLen
                collectErrorsIfAny remainder errorList origLines origLength suffixSpan suffixVerbatim

            | _ ->
                // If there were no recovery matches at all, keep the whole chunk,
                // otherwise mask it entirely
                if i = 0 && matches.Length = 0 then
                    parseAbleInput.Append(chunk) |> ignore
                else
                    parseAbleInput.Append(maskedChunk) |> ignore
                collectErrorsIfAny remainder errorList origLines origLength chunkSpan chunkVerbatim
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
        let parseAbleInput, errorList = getParseableInputAndErrorNodes input fplCode origLines origLength
        match run stdParser parseAbleInput with 
        | Success(ast, _, _) ->
            let resultWithSyntaxErrors = errorList @ getBuildingBlockAsts ast
            let sortingComparer group subGroup = $"{group} {subGroup}" 
            let indexFormat (pos:Position) = sprintf "%0*d" 10 (pos.Index)
            resultWithSyntaxErrors
            |> List.sortBy (fun buildingBlockAst ->
                match buildingBlockAst with
                | Ast.BuildingBlock((pos1,_),_) -> sortingComparer (indexFormat pos1) ""
                | Ast.ErrorSyntax((pos1,_),_,_,_) -> sortingComparer (indexFormat pos1) ""
                | Ast.ErrorSyntaxBacktracking((pos1,_),_,_,_) -> sortingComparer (indexFormat pos1) ""
                | Ast.ErrorSyntaxChain(((pos1,_),maxPos),_,(_, chain),_) -> sortingComparer (indexFormat maxPos) chain
                | _ -> sortingComparer "ZZZ" ""
            ), false
        | Failure(errorMsg, _, _) ->
            // Whole document is unparseable even after chunked recovery: there is no
            // keyword-delimited building-block span to speak of, so the "block" is the entire
            // (comment-stripped) input, and its verbatim text is the entire original source.
            let wholeDocSpan : Positions = indexToPosition input 0, indexToPosition input input.Length
            getErrorNodes errorMsg origLines origLength wholeDocSpan fplCode, false

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

/// <summary>
/// Parses <paramref name="fplCode"/> for the formatting service: returns the same building-block
/// ASTs and success flag as <c>fplParser</c>, together with the full, independently-discovered list
/// of comments (with kind and positions), ready to be merged into a <c>TriviaMap</c>.
/// </summary>
/// <param name="fplCode">Raw FPL source code to parse.</param>
/// <returns>
/// A tuple of: the result of <c>fplParser</c> (building blocks, full-success flag), and the list
/// of <c>CommentLexer.Comment</c> found by an independent full-text scan of the raw source. Comment
/// discovery does not depend on <c>fplParser</c>'s outcome and is always complete, even when the
/// FPL code has syntax errors.
/// </returns>
let fplParserWithTrivia fplCode =
    let asts, wasFullyParsed = fplParser fplCode
    let comments = findComments fplCode
    let nodePositions =
        asts
        |> List.collect (fun a ->
            let acc = List<Positions>()
            collectPositions acc a                      // <-- still used here
            List.ofSeq acc)
    (asts, wasFullyParsed), nodePositions, comments
