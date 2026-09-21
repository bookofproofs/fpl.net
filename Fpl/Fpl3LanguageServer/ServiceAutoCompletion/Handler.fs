module Fpl3LanguageServer.ServiceAutoCompletion.Handler
(*
MIT License

Copyright (c) 2018 Martin Björkström

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
open System.Threading
open System.Threading.Tasks
open OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities
open OmniSharp.Extensions.LanguageServer.Protocol.Document
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open OmniSharp.Extensions.LanguageServer.Protocol.Server
open Fpl0Base.Errors.Diagnostics
open Fpl3LanguageServer.Buffers.BuffMgr
open Fpl3LanguageServer.Buffers.Logging
open Fpl3LanguageServer.ServiceAutoCompletion.Main

/// <summary>
/// Computes the character offset (position) within a buffer for a given line and column.
/// </summary>
let private getPosition (buffer: string) (line: int) (col: int) : int =
    let mutable position = 0
    for _ in 0 .. line - 1 do
        position <- buffer.IndexOf('\n', position) + 1
    position + col

/// <summary>
/// Handles textDocument/completion requests, delegating to the FPL parser-driven
/// autocompletion service to compute suggestions at the requested cursor position.
/// </summary>
type CompletionHandler(languageServer: ILanguageServer, bufferManager: BufferManager) =

    let documentSelector = DocumentSelector(DocumentFilter(Pattern = "**/*.fpl"))

    let mutable capability = CompletionCapability()

    interface ICompletionHandler with

        member _.GetRegistrationOptions() =
            CompletionRegistrationOptions(DocumentSelector = documentSelector, ResolveProvider = false)

        member _.Handle(request: CompletionParams, cancellationToken: CancellationToken) : Task<CompletionList> =
            task {
                logMsg languageServer "Task<CompletionList>" "CompletionHandler.Handle"
                let uri = PathEquivalentUri(request.TextDocument.Uri.GetFileSystemPath())
                let buffer = bufferManager.GetBuffer(uri)
                if isNull buffer then
                    return CompletionList()
                else
                    let position =
                        getPosition (buffer.ToString().Substring(0, buffer.Length))
                                    (int request.Position.Line)
                                    (int request.Position.Character)
                    return! getParserChoices buffer position languageServer
            }

        member _.SetCapability(cap: CompletionCapability) =
            capability <- cap
