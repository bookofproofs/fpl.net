module ErrorHandling
open FplGrammarTypes
open FParsec

(*
MIT License

Copyright (c) 2020 Will Speak

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*)

// Flop - Sometimes you fail, and that's OK
//
// This is a small investigation into error handling and recovery in FParsec
// based on the ideas discussed by @ebkalderon in [Error recovery with parser
// combinators (using nom)](https://www.eyalkalderon.com/nom-error-recovery/)

/// Structured error information to be emitted by our parser.
/// Structured error information to be emitted by our parser.
type Diagnostic = Diagnostic of Position * string * string

/// Our Parser State. Used to keep track of the diagnostics we encountered while
/// parsing the source text so far.
type State =
    { mutable Diagnostics: Diagnostic list }

    /// Emit Diagnostic
    member s.EmitDiagnostic pos severity msg =
        let diag = Diagnostic(pos, severity, msg)
        s.Diagnostics <- diag::s.Diagnostics

    /// Initial parser state
    static member Initial = { Diagnostics = [] }

// ~~~~~~~~~~~~~ Error Handling Constructs ~~~~~~~~~~~~~~~~~~
//
// These parsers are responsible for handling and syncrhonising after errors are
// encountered.

/// Expect a given parser to match in the current location. If the parser fails
/// the given `err` is emitted as a diagnostic at the current location.
let expect (p: Parser<'a, State>) err =
    let raiseErr (stream: CharStream<State>) =
        // emit a parser error 'PE' for later correct diagnostic severity in the language server
        stream.UserState.EmitDiagnostic stream.Position "PE" err
        Reply(None)
    attempt p |>> Some <|> raiseErr



/// A variant of `expet` that resolves parser failures with the `Error` syntax
/// node rather than returning an `option`al value.
let expectSyn (p: Parser<SyntaxNode, State>) err =
    expect p err |>> Option.defaultValue SyntaxNode.Error

/// Error syncrhonisation. used to parse any characters and emit them as
/// diagnostics. This is `<|>`ed into the standard combinator chain right at the
/// root to try and ensure the parser _always_ has a way to succeed.
let error (stream: CharStream<State>) =
    match stream.ReadCharOrNewline() with
    | EOS -> Reply(ReplyStatus.Error, expected "valid expression character")
    | ch ->
        let err = sprintf "Unexpected character %c" ch
        stream.UserState.EmitDiagnostic stream.Position "PE" err
        Reply(SyntaxNode.Error)

/// Runs the parser on the input string and throws an exception if the parser
/// fails. We expect the parser should _always_ succeed. For malformed source
/// text an `SyntaxNode.Error` should be returned and a `Diagnostic` emitted.
let tryParse parser streamName input =
    match runParserOnString parser State.Initial streamName input with
    | ParserResult.Failure(_) as f -> failwithf "Parser failed! %A" f
    | ParserResult.Success(r, s, _) -> (r, s.Diagnostics)


