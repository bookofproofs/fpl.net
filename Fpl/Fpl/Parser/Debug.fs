/// <summary>
/// Debug helpers for FParsec parsers used during development and tests.
/// When enabled, the wrappers log parser entry and exit information to a debug file.
/// </summary>
module Fpl.Parser.Debug

(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)

open System
open System.IO
open FParsec
open TestSharedConfig


/// <summary>
/// Wraps a parser so that, when parser debug mode is enabled, entry and exit events
/// (including the parser's reply status) are appended to a debug log file.
/// </summary>
/// <param name="p">The parser to wrap.</param>
/// <param name="label">A textual label used to identify the parser in the log.</param>
/// <returns>
/// A parser that behaves like <paramref name="p"/> but logs entry/exit information when
/// <c>TestConfig.DebugModeParser</c> is true.
/// </returns>
///<example id="uint8-1"><code lang="fsharp">
/// let pExpr, pExprRef = createParserForwardedToRef()
/// let pNumber = pint32 |> trace "number"
/// let pParens = between (pchar '(') (pchar ')') pExpr |> trace "parens"
/// do pExprRef :=
///    choice [
///        pNumber
///        pParens
///    ]
///    |> trace "expr"
/// run pExpr "(12)"
/// </code>
/// </example>
let debugWrapper (p: Parser<_,_>) label : Parser<_,_> =
    fun stream ->
        if TestConfig.DebugModeParser then
            let currDir =  Directory.GetCurrentDirectory()
            let logLine = sprintf "%A: Entering %s%s" stream.Position label Environment.NewLine
            File.AppendAllText(Path.Combine(currDir, "Debug.txt"), logLine)
            let reply = p stream
            let logLine1 = sprintf "%A: Leaving %s (%A)%s" stream.Position label reply.Status Environment.NewLine
            File.AppendAllText(Path.Combine(currDir, "Debug.txt"), logLine1)
            reply
        else
            p stream

#if DEBUG
/// <summary>
/// Conditional operator alias for <see cref="debugWrapper"/>.
/// In debug builds (when the DEBUG symbol is defined) this operator is bound to <c>debugWrapper</c>;
/// in non-debug builds it is a no-op that returns the original parser unchanged.
/// </summary>
/// <param name="p">Parser to wrap.</param>
/// <param name="label">Label for logging; ignored in non-debug builds.</param>
/// <returns>The wrapped parser in debug builds, otherwise the original parser.</returns>
let (<!>) = debugWrapper
#else
let (<!>) p _ = p
#endif
