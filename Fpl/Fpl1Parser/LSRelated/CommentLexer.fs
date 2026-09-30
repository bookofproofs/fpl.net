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

/// <summary>Distinguishes the two FPL comment syntax forms so the formatter can re-emit them faithfully.</summary>
/// <remarks>
/// <see cref="LineComment"/> (<c>//...</c>) is terminated implicitly by the end of the physical
/// line, whereas <see cref="BlockComment"/> (<c>/* ... */</c>) is terminated explicitly by its
/// closing delimiter and may span multiple lines or appear inline within a single line. The
/// pretty-printer (<c>Fpl1Parser.LSRelated.PrettyPrint</c>) relies on this distinction to decide
/// whether a comment must force a line break after it (line comments always do) or may remain
/// inline (block comments may).
/// </remarks>
type CommentKind =
    | LineComment
    | BlockComment

/// <summary>A single discovered comment: its kind, source positions, and raw text (delimiters included).</summary>
/// <remarks>
/// <c>Text</c> always includes the comment's opening delimiter (<c>//</c> or <c>/*</c>) and, for
/// <see cref="BlockComment"/>, its closing delimiter (<c>*/</c>), so that
/// <c>Fpl1Parser.LSRelated.PrettyPrint</c> can re-emit the comment verbatim without having to
/// reconstruct delimiters itself.
/// </remarks>
type Comment = { Kind: CommentKind; Positions: Positions; Text: string }

/// <summary>Parses a single <c>//</c> line comment, from the opening <c>//</c> to the end of the current line.</summary>
/// <returns>
/// A parser yielding a <see cref="Comment"/> with <see cref="CommentKind.LineComment"/>, the
/// comment's <see cref="Positions"/>, and its full text (including the leading <c>//</c> but
/// excluding the terminating newline, per <c>restOfLine false</c>).
/// </returns>
let private lineComment : Parser<Comment,unit> =
    positions (pstring "//" .>>. restOfLine false)
    |>> fun (pos, (slashes, rest)) -> { Kind = LineComment; Positions = pos; Text = slashes + rest }

/// <summary>Parses a single <c>/* ... */</c> block comment, from the opening <c>/*</c> to the matching closing <c>*/</c>.</summary>
/// <returns>
/// A parser yielding a <see cref="Comment"/> with <see cref="CommentKind.BlockComment"/>, the
/// comment's <see cref="Positions"/>, and its full text including both the opening <c>/*</c> and
/// closing <c>*/</c> delimiters.
/// </returns>
/// <remarks>
/// May span multiple source lines. Unterminated block comments (missing a closing <c>*/</c>)
/// cause this parser to fail, which in turn causes <see cref="scanComments"/> to stop matching
/// further comments at that point (see <see cref="findComments"/> remarks for the resulting behavior).
/// </remarks>
let private blockComment : Parser<Comment,unit> =
    positions (pstring "/*" .>>. manyCharsTill anyChar (pstring "*/"))
    |>> fun (pos, (open_, body)) -> { Kind = BlockComment; Positions = pos; Text = open_ + body + "*/" }

/// <summary>Parses either a block comment or a line comment, preferring the block-comment form.</summary>
/// <returns>A parser yielding whichever <see cref="Comment"/> form matched at the current position.</returns>
/// <remarks>
/// Block comments are attempted first (via <c>attempt</c>) so that a leading <c>/*</c> is not
/// mistaken for the start of a line comment; both alternatives share the <c>/</c> prefix.
/// </remarks>
let private commentToken : Parser<Comment,unit> =
    attempt blockComment <|> lineComment

/// <summary>Skips over the contents of a double-quoted FPL string literal, including its delimiters.</summary>
/// <returns>A parser that consumes one complete <c>"..."</c> string literal and yields <c>unit</c>.</returns>
/// <remarks>
/// Used by <see cref="scanComments"/> so that <c>//</c> or <c>/*</c> characters occurring inside a
/// string literal are not misidentified as the start of a comment. This is a deliberately minimal,
/// standalone re-implementation of string-literal skipping rather than a dependency on the full
/// string/regex parsers in <c>Fpl1Parser.Grammar</c>, keeping this lexer independent of the main
/// grammar so it can run even when the rest of the source has syntax errors.
/// </remarks>
let private stringLiteralSkip : Parser<unit,unit> =
    skipChar '"' >>. skipManyTill anyChar (skipChar '"')

/// <summary>Skips exactly one character that is not the start of a comment or a string literal.</summary>
/// <returns>A parser that consumes a single non-comment, non-string-literal character and yields <c>unit</c>.</returns>
/// <remarks>
/// Used as the fallback alternative in <see cref="scanComments"/>'s <c>many</c> loop, ensuring the
/// scan advances one character at a time through ordinary source text that is neither a comment
/// nor a string literal.
/// </remarks>
let private otherChar : Parser<unit,unit> =
    notFollowedBy (attempt blockComment |>> ignore <|> (lineComment |>> ignore) <|> stringLiteralSkip)
    >>. skipAnyChar

/// <summary>
/// Scans the entire input and returns every comment found, in source order, skipping over
/// string-literal contents so that <c>//</c>/<c>/*</c> characters inside strings are ignored.
/// </summary>
/// <returns>
/// A parser yielding the list of <see cref="Comment"/> values found, in source order, with all
/// non-comment, non-string-literal characters discarded.
/// </returns>
/// <remarks>
/// Runs to the end of the input stream (via the implicit end-of-input termination of <c>many</c>),
/// consuming the entire source text one comment, string literal, or single character at a time.
/// This parser does not itself validate FPL syntax and is unaffected by syntax errors elsewhere in
/// the source, other than an unterminated block comment (see <see cref="blockComment"/> remarks),
/// which will cause the <c>many</c> loop to stop consuming further input at that point.
/// </remarks>
let scanComments : Parser<Comment list,unit> =
    many (
        (attempt commentToken |>> Some)
        <|> (attempt stringLiteralSkip >>% None)
        <|> (otherChar >>% None)
    )
    |>> List.choose id

/// <summary>
/// Runs <see cref="scanComments"/> over <paramref name="fplCode"/> and returns the discovered comments.
/// </summary>
/// <param name="fplCode">The raw FPL source text to scan for comments.</param>
/// <returns>The list of <see cref="Comment"/> values found in <paramref name="fplCode"/>, in source order.</returns>
/// <exception cref="System.Exception">
/// Thrown if <see cref="scanComments"/> itself fails to run to completion (e.g. due to an
/// unterminated block comment leaving the parser unable to make further progress). This is not
/// expected under normal operation, since every character in well-formed input is consumed by one
/// of <see cref="commentToken"/>, <see cref="stringLiteralSkip"/>, or <see cref="otherChar"/>.
/// </exception>
let findComments (fplCode: string) : Comment list =
    match run scanComments fplCode with
    | Success(comments, _, _) -> comments
    | Failure(msg, _, _) -> failwith $"Unexpected: comment scanning must not fail: {msg}"
