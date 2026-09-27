module Fpl3LanguageServer.ServiceAutoCompletion.Handler
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
