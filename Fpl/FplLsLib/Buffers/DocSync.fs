module FplLsLib.Buffers.DocSync
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
open System
open System.Text
open System.Threading
open System.Threading.Tasks
open MediatR
open OmniSharp.Extensions.LanguageServer.Protocol
open OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities
open OmniSharp.Extensions.LanguageServer.Protocol.Document
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open OmniSharp.Extensions.LanguageServer.Protocol.Server
open OmniSharp.Extensions.LanguageServer.Protocol.Server.Capabilities
open Fpl.Errors.Diagnostics
open FplLsLib.Buffers.Logging
open FplLsLib.Buffers.BuffMgr

/// <summary>
/// Handles text document synchronization notifications (open/change/close/save)
/// from the language client, keeping the in-memory <see cref="BufferManager"/> up to date.
/// </summary>
type TextDocumentSyncHandler(router: ILanguageServer, bufferManager: BufferManager) as this =

    let documentSelector =
        DocumentSelector(DocumentFilter(Pattern = "**/*.fpl"))

    let mutable capability : SynchronizationCapability option = None

    /// <summary>
    /// The kind of synchronization used for document changes (full document replacement).
    /// </summary>
    member this.Change = TextDocumentSyncKind.Full

    /// <summary>
    /// Returns registration options describing which documents this handler synchronizes and how.
    /// </summary>
    member this.GetRegistrationOptions() : TextDocumentChangeRegistrationOptions =
        TextDocumentChangeRegistrationOptions(DocumentSelector = documentSelector, SyncKind = this.Change)

    /// <summary>
    /// Returns the attributes (language id, etc.) associated with the given document URI.
    /// </summary>
    member this.GetTextDocumentAttributes(uri: Uri) : TextDocumentAttributes =
        logMsg router $"{uri.AbsolutePath}" "GetTextDocumentAttributes.GetTextDocumentAttributes"
        TextDocumentAttributes(uri, "fpl")

    /// <summary>
    /// Handles a full-document change notification, updating the corresponding buffer.
    /// </summary>
    member this.Handle(request: DidChangeTextDocumentParams, cancellationToken: CancellationToken) : Task<Unit> =
        logMsg router $"{cancellationToken}" "GetTextDocumentAttributes.Handle"
        try
            let uri = PathEquivalentUri.EscapedUri(request.TextDocument.Uri.GetFileSystemPath())
            let text =
                request.ContentChanges
                |> Seq.tryHead
                |> Option.map (fun change -> change.Text)
                |> Option.defaultValue null

            logMsg router "updating buffer" $"GetTextDocumentAttributes.Handle {uri}"
            bufferManager.UpdateBuffer(uri, StringBuilder(text))
            logMsg router "buffer updated" "GetTextDocumentAttributes.Handle"
        with ex ->
            logException router ex "GetTextDocumentAttributes.Handle (DidChangeTextDocumentParams)"

        Unit.Task

    /// <summary>
    /// Handles a document-open notification, seeding the corresponding buffer with its full text.
    /// </summary>
    member this.Handle(request: DidOpenTextDocumentParams, cancellationToken: CancellationToken) : Task<Unit> =
        logMsg router "x(DidOpenTextDocumentParams)" "GetTextDocumentAttributes.Handle"
        try
            let uri = PathEquivalentUri.EscapedUri(request.TextDocument.Uri.GetFileSystemPath())
            logMsg router "updating buffer (DidOpenTextDocumentParams)" $"GetTextDocumentAttributes.Handle {uri}"
            bufferManager.UpdateBuffer(uri, StringBuilder(request.TextDocument.Text))
            logMsg router "buffer updated (DidOpenTextDocumentParams)" "GetTextDocumentAttributes.Handle"
        with ex ->
            logException router ex "GetTextDocumentAttributes.Handle (DidOpenTextDocumentParams)"

        Unit.Task

    /// <summary>
    /// Handles a document-close notification.
    /// </summary>
    member this.Handle(request: DidCloseTextDocumentParams, cancellationToken: CancellationToken) : Task<Unit> =
        logMsg router "y(DidCloseTextDocumentParams)" "GetTextDocumentAttributes.Handle"
        Unit.Task

    /// <summary>
    /// Handles a document-save notification.
    /// </summary>
    member this.Handle(request: DidSaveTextDocumentParams, cancellationToken: CancellationToken) : Task<Unit> =
        logMsg router "z(DidSaveTextDocumentParams)" "GetTextDocumentAttributes.Handle"
        Unit.Task

    /// <summary>
    /// Stores the client's synchronization capability negotiated during initialization.
    /// </summary>
    member this.SetCapability(newCapability: SynchronizationCapability) =
        logMsg router "" "GetTextDocumentAttributes.SetCapability"
        capability <- Some newCapability

    interface ITextDocumentSyncHandler with
        member this.Handle(request: DidChangeTextDocumentParams, cancellationToken: CancellationToken) =
            this.Handle(request, cancellationToken)
        member this.Handle(request: DidOpenTextDocumentParams, cancellationToken: CancellationToken) =
            this.Handle(request, cancellationToken)
        member this.Handle(request: DidCloseTextDocumentParams, cancellationToken: CancellationToken) =
            this.Handle(request, cancellationToken)
        member this.Handle(request: DidSaveTextDocumentParams, cancellationToken: CancellationToken) =
            this.Handle(request, cancellationToken)
        member this.SetCapability(newCapability: SynchronizationCapability) =
            this.SetCapability(newCapability)
        member this.GetRegistrationOptions() : TextDocumentChangeRegistrationOptions =
            this.GetRegistrationOptions()

    interface IRegistration<TextDocumentRegistrationOptions> with
        member _.GetRegistrationOptions() : TextDocumentRegistrationOptions =
            TextDocumentChangeRegistrationOptions(DocumentSelector = documentSelector, SyncKind = TextDocumentSyncKind.Full)

    interface IRegistration<TextDocumentSaveRegistrationOptions> with
        member _.GetRegistrationOptions() : TextDocumentSaveRegistrationOptions =
            TextDocumentSaveRegistrationOptions(DocumentSelector = documentSelector)

    interface ITextDocumentIdentifier with
        member _.GetTextDocumentAttributes(uri: DocumentUri) : TextDocumentAttributes =
            TextDocumentAttributes(uri, "fpl")
