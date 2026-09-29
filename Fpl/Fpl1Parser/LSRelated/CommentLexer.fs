/// <summary>
/// A standalone, minimal lexer that scans raw FPL source text for <c>//</c> line comments and
/// <c>/* ... */</c> block comments, producing a flat, positioned list of them.
/// </summary>
/// <remarks>
/// This parser is intentionally decoupled from <c>Fpl1Parser.Grammar</c>: comment positions are a
/// purely lexical fact and must be discoverable even when the surrounding FPL code has syntax
/// errors (which is the common case while a user is actively editing). Running comment discovery
/// as its own independent full-text scan (rather than piggy-backing on <c>stdParser</c>'s success)
/// guarantees comments are always found regardless of whether <c>fplParser</c> takes its
/// error-recovery path.
/// </remarks>
module Fpl1Parser.LSRelated.CommentLexer
open FParsec
open Fpl1Parser.Types
open Fpl1Parser.Basic

/// <summary>Distinguishes the two FPL comment syntax so the formatter can re-emit them faithfully.</summary>
type CommentKind =
    | LineComment
    | BlockComment

/// <summary>A single discovered comment: its kind, source positions, and raw text (delimiters included).</summary>
type Comment = { Kind: CommentKind; Positions: Positions; Text: string }

let private lineComment : Parser<Comment,unit> =
    positions (pstring "//" .>>. restOfLine false)
    |>> fun (pos, (slashes, rest)) -> { Kind = LineComment; Positions = pos; Text = slashes + rest }

let private blockComment : Parser<Comment,unit> =
    positions (pstring "/*" .>>. manyCharsTill anyChar (pstring "*/"))
    |>> fun (pos, (open_, body)) -> { Kind = BlockComment; Positions = pos; Text = open_ + body + "*/" }

let private commentToken : Parser<Comment,unit> =
    attempt blockComment <|> lineComment

/// <summary>
/// A string literal skip-parser, needed so the comment lexer does not mistake `//` or `/*`
/// appearing inside FPL string literals for actual comments.
/// </summary>
let private stringLiteralSkip : Parser<unit,unit> =
    skipChar '"' >>. skipManyTill anyChar (skipChar '"')

/// <summary>Skips exactly one character that is not the start of a comment or string literal.</summary>
let private otherChar : Parser<unit,unit> =
    notFollowedBy (attempt blockComment |>> ignore <|> (lineComment |>> ignore) <|> stringLiteralSkip)
    >>. skipAnyChar

/// <summary>
/// Scans the entire input and returns every comment found, in source order, skipping over
/// string-literal contents so that <c>//</c>/<c>/*</c> characters inside strings are ignored.
/// </summary>
let scanComments : Parser<Comment list,unit> =
    many (
        (attempt commentToken |>> Some)
        <|> (attempt stringLiteralSkip >>% None)
        <|> (otherChar >>% None)
    )
    |>> List.choose id

/// <summary>
/// Runs <c>scanComments</c> over <paramref name="fplCode"/> and returns the discovered comments.
/// This never fails: any unrecognized character is simply skipped.
/// </summary>
let findComments (fplCode: string) : Comment list =
    match run scanComments fplCode with
    | Success(comments, _, _) -> comments
    | Failure(msg, _, _) -> failwith $"Unexpected: comment scanning must not fail: {msg}"
