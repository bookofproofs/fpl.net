module FplLsLib.Buffers.TextPos
(*
Derived from https://github.com/tintoy/msbuild-project-tools-server/blob/37f635e4cd2ddcaebb32ad113dad1cbbc331a92e/src/LanguageServer.Common/Utilities/TextPositions.cs
MIT License

Copyright (c) 2017 Adam Friedman

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
open System
open System.Collections.Generic



/// <summary>
///     An alias for the type OmniSharp.Extensions.LanguageServer.Protocol.Models.Position
/// </summary>
type UOPosition = OmniSharp.Extensions.LanguageServer.Protocol.Models.Position

/// <summary>
///     An alias for the type OmniSharp.Extensions.LanguageServer.Protocol.Models.Range
/// </summary>
type UORange = OmniSharp.Extensions.LanguageServer.Protocol.Models.Range

/// <summary>
///     Calculate the start position for each line in the text.
/// </summary>
let private calculateLineStartPositions (text: string) : int[] =
    if isNull text then
        raise (ArgumentNullException(nameof text))

    let lineStarts = List<int>()

    let mutable currentPosition = 0
    let mutable currentLineStart = 0
    while currentPosition < text.Length do
        let currentChar = text.[currentPosition]
        currentPosition <- currentPosition + 1

        match currentChar with
        | '\r' ->
            if currentPosition < text.Length && text.[currentPosition] = '\n' then
                currentPosition <- currentPosition + 1
            lineStarts.Add(currentLineStart)
            currentLineStart <- currentPosition
        | '\n' ->
            lineStarts.Add(currentLineStart)
            currentLineStart <- currentPosition
        | _ -> ()

    lineStarts.Add(currentLineStart)
    lineStarts.ToArray()

/// <summary>
///     A quick-and-dirty calculator for text positions.
/// </summary>
/// <remarks>
///     This could easily be improved by also storing a character sub-total for each line.
/// </remarks>
type TextPositions(text: string) =

    let lineStartPositions =
        if isNull text then
            raise (ArgumentNullException(nameof text))
        calculateLineStartPositions text

    /// <summary>
    ///     The number of lines in the text.
    /// </summary>
    member _.LineCount = lineStartPositions.Length

    /// <summary>
    ///     The absolute starting position, within the text, of each line.
    /// </summary>
    member _.LineStartPositions : IReadOnlyList<int> = lineStartPositions

    /// <summary>
    ///     Convert line and column numbers to an absolute position within the text.
    /// </summary>
    /// <param name="line">The target line (0-based).</param>
    /// <param name="column">The target column (0-based).</param>
    member this.GetAbsolutePosition(line: int, column: int) : int =
        if line < 0 then
            raise (ArgumentOutOfRangeException(nameof line, line, "Line cannot be less than 0."))

        if line >= lineStartPositions.Length then
            raise (ArgumentOutOfRangeException(nameof line, line, "Line is past the end of the text."))

        if column < 0 then
            raise (ArgumentOutOfRangeException(nameof column, column, "Column cannot be less than 0."))

        lineStartPositions.[line] + column

    /// <summary>
    ///     Convert a <see cref="T:FplLsLib.Buffers.TextPos.UOPosition"/> to an absolute position within the text.
    /// </summary>
    /// <param name="position">The target <see cref="T:FplLsLib.Buffers.TextPos.UOPosition"/> (0-based or 1-based).</param>
    member this.GetAbsolutePosition(position: UOPosition) : int =
        if isNull (box position) then
            raise (ArgumentNullException(nameof position))

        this.GetAbsolutePosition(position.Line, position.Character)

    /// <summary>
    ///     Convert an absolute position to a line and column in the text.
    /// </summary>
    /// <param name="absolutePosition">The absolute position (0-based).</param>
    member _.GetPosition(absolutePosition: int) : UOPosition =
        let mutable targetLine = Array.BinarySearch(lineStartPositions, absolutePosition)
        if targetLine < 0 then
            // No match, so BinarySearch returns 2's complement of the following line index.
            targetLine <- ~~~targetLine - 1

        UOPosition(targetLine, absolutePosition - lineStartPositions.[targetLine])

    /// <summary>
    ///     Get a <see cref="T:FplLsLib.Buffers.TextPos.UORange"/> representing the specified absolute positions.
    /// </summary>
    /// <param name="absoluteStartPosition">The (0-based) absolute start position.</param>
    /// <param name="absoluteEndPosition">The (1-based) absolute end position.</param>
    member this.GetRange(absoluteStartPosition: int, absoluteEndPosition: int) : UORange =
        UORange(
            start = this.GetPosition(absoluteStartPosition),
            ``end`` = this.GetPosition(absoluteEndPosition)
        )

    /// <summary>
    ///     Calculate the number of characters, in the text, between the specified positions.
    /// </summary>
    /// <param name="position1">The first position.</param>
    /// <param name="position2">The second position.</param>
    member this.GetDistance(position1: UOPosition, position2: UOPosition) : int =
        if isNull (box position1) then
            raise (ArgumentNullException(nameof position1))

        if isNull (box position2) then
            raise (ArgumentNullException(nameof position2))

        this.GetAbsolutePosition(position2) - this.GetAbsolutePosition(position1)

    /// <summary>
    ///     Calculate the length of the specified <see cref="T:FplLsLib.Buffers.TextPos.UORange"/> in the text.
    /// </summary>
    /// <param name="range">The range.</param>
    member this.GetLength(range: UORange) : int =
        if isNull (box range) then
            raise (ArgumentNullException(nameof range))

        this.GetDistance(range.Start, range.End)
