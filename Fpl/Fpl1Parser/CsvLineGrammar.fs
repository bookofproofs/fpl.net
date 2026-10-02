module CsvLineGrammar

open System
open System.Text
open FParsec

type CsvAST =
    | ColumnVal of string
    | Line of CsvAST list

type CsvConfig =
    {
        Encoding        : Encoding
        ColumnSeparator : char
        Quote           : char option
    }

let private validateConfig encodingParam colSepParam quoteParam =
    let encoding =
        match encodingParam with
        | "UTF8" -> Encoding.UTF8
        | _ ->
            invalidArg
                "config"
                $"Unknown or unsupported encoding `{encodingParam}`"
    let quote =
        match quoteParam with
        | "" -> None
        | _ when quoteParam.Length > 1 ->
            invalidArg
                "config"
                $"Quote must be exactly one character, was {quoteParam.Length}: `{quoteParam}`."
        | _ -> Some quoteParam[0]
    let colSep =
        match colSepParam with
        | "" -> None
        | _ when colSepParam.Length > 1 ->
            invalidArg
                "config"
                $"ColumnSeparator must be exactly one character, was {colSepParam.Length}: `{colSepParam}`."
        | _ -> Some colSepParam[0]
    match colSep with
    | Some cs ->
        match quote with
        | Some qt when qt = cs ->
            invalidArg
                "config"
                $"Quote and ColumnSeparator must be different, were same = `{cs}`."
        | _ ->
            {
                Encoding        = encoding
                ColumnSeparator = cs
                Quote           = quote
            }
    | _ ->
        invalidArg
            "config"
            "ColumnSeparator must be set to a character (was empty)"

/// Parses a quoted column.
/// Quotation marks inside a quoted value are escaped by doubling them:
///
///     "He said ""hello"""
///
/// becomes:
///
///     He said "hello"
///
/// No line-separator logic is applied here. The parser simply parses
/// characters until the closing quotation mark.
let private quotedColumn
    (quote: char)
    : Parser<string, unit> =

    // `>>?` only backtracks on failure, without the full state
    // save/restore overhead of `attempt`. Here, failure of the second
    // `pchar quote` only ever consumes a single character, so this is
    // sufficient and considerably cheaper than `attempt`.
    let escapedQuote : Parser<char, unit> =
        pchar quote >>? pchar quote

    let ordinaryCharacters =
        // `many1Satisfy` scans directly over the input buffer using the
        // predicate, avoiding the per-character parser-combinator
        // dispatch overhead of `manyChars (satisfy ...)`.
        many1Satisfy (fun c -> c <> quote)

    let contents =
        manyStrings (
            (escapedQuote |>> string)
            <|> ordinaryCharacters
        )

    pchar quote
    >>. contents
    .>> pchar quote

/// Parses an unquoted column.
///
/// An unquoted column cannot contain the column separator. When quoted
/// values are enabled, it also cannot contain the quotation character.
let private unquotedColumn
    (config: CsvConfig)
    : Parser<string, unit> =

    let isAllowed c =
        c <> config.ColumnSeparator
        &&
        match config.Quote with
        | Some quote ->
            c <> quote
        | _ ->
            true

    // `manySatisfy` scans the underlying char buffer directly using the
    // predicate instead of repeatedly invoking a `satisfy` parser, which
    // is significantly cheaper for long runs of allowed characters.
    manySatisfy isAllowed

let private columnParser
    (config: CsvConfig)
    : Parser<CsvAST, unit> =

    match config.Quote with
    | Some quote ->
        let quoted =
            quotedColumn quote
            |>> ColumnVal

        let unquoted =
            unquotedColumn config
            |>> ColumnVal

        // A field beginning with a quotation mark must be a valid
        // quoted field. Malformed quoted fields therefore fail parsing.
        quoted <|> unquoted

    | _ ->
        unquotedColumn config
        |>> ColumnVal

/// Parses one complete input line/record.
///
/// Empty columns are preserved:
///
///     a,,c,
///
/// becomes:
///
///     Line [ColumnVal "a"; ColumnVal ""; ColumnVal "c"; ColumnVal ""]
let private lineParser
    (config: CsvConfig)
    : Parser<CsvAST, unit> =

    let separator =
        pchar config.ColumnSeparator

    let firstColumn =
        columnParser config

    let remainingColumns =
        many (
            separator
            >>. columnParser config
        )

    firstColumn
    .>>. remainingColumns
    |>> fun (first, rest) ->
        Line (first :: rest)

type CsvLineReader(encoding: string, columnSeparator: string, quote: string) =
    let mutable _config: CsvConfig =
        {
            Encoding        = Encoding.Default
            ColumnSeparator = ','
            Quote           = None
        }

    // The underlying FParsec parser is expensive to build (it allocates
    // closures and combinator wrappers for `many`, `<|>`, `.>>.`, etc.).
    // It is therefore constructed exactly once per `CsvLineReader`
    // instance instead of once per `ParseLine` call, which matters a
    // great deal when parsing billions of lines.
    let mutable _parser : Parser<CsvAST, unit> =
        Unchecked.defaultof<_>

    // Constructor validation is performed before the configuration is stored.
    do
        _config <- validateConfig encoding columnSeparator quote
        _parser <- lineParser _config .>> eof
        let quoteStr = sprintf "%A" _config.Quote
        printf $"Parser config: encoding= `{_config.Encoding.ToString()}` column separator= `{_config.ColumnSeparator}` quoted strings = `{quoteStr}`{Environment.NewLine}"

    /// Parses one input line supplied as a string.
    member this.ParseLine(input: string) =
        // When no quotation character is configured, columns can never
        // contain the separator in escaped/quoted form, so a plain
        // `String.Split` is both correct and dramatically faster than
        // running the general-purpose FParsec parser for this common
        // case, avoiding `CharStream` setup, position tracking and
        // per-character parser dispatch entirely.
        match _config.Quote with
        | None ->
            input.Split(_config.ColumnSeparator)
            |> Array.map ColumnVal
            |> Array.toList
            |> Line
        | Some _ ->
            match run _parser input with
            | Success(ast, _, _) ->
                ast
            | Failure(errorMessage, _, _) ->
                failwith errorMessage

    /// Decodes a byte array using the configured encoding and parses one line.
    member this.ParseBytes(input: byte[]) =
        let text = _config.Encoding.GetString input
        this.ParseLine text

    /// Decodes a span of bytes using the configured encoding and parses one
    /// line, without requiring a separate `byte[]` copy of the slice.
    member this.ParseBytes(input: ReadOnlySpan<byte>) =
        let text = _config.Encoding.GetString input
        this.ParseLine text
