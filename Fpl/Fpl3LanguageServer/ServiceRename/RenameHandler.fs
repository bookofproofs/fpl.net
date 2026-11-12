/// <summary>
/// Language server handlers for the textDocument/prepareRename and textDocument/rename requests of FPL files.
/// </summary>
/// <remarks>
/// The handlers only translate LSP requests into calls of <c>Fpl3LanguageServer.ServiceRename.RenameCore</c>.
/// </remarks>
module Fpl3LanguageServer.ServiceRename.RenameHandler

open System
open System.IO
open System.Threading
open System.Threading.Tasks
open OmniSharp.Extensions.LanguageServer.Protocol
open OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities
open OmniSharp.Extensions.LanguageServer.Protocol.Document
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open OmniSharp.Extensions.LanguageServer.Protocol.Server
open Fpl2Interpreter.SymbolTable.Storage.Heap
open Fpl3LanguageServer.Buffers.Logging
open Fpl3LanguageServer.ServiceRename.RenameCore

/// <summary>
/// Normalizes a file path so that editor paths and symbol-table paths can be compared.
/// </summary>
/// <param name="path">File path to normalize.</param>
/// <returns>The full path with forward slashes and no trailing separator.</returns>
let private normalizePath (path: string) =
    Path.GetFullPath(path).Replace('\\', '/').TrimEnd('/')

/// <summary>
/// Rejects an LSP request with a message that the client shows to the user.
/// </summary>
/// <param name="message">Message explaining why the request was rejected.</param>
/// <returns>Never returns; always raises an <c>Exception</c>.</returns>
let private rejectRequest (message: string) : 'a =
    raise (Exception message)

/// <summary>
/// Converts the edits of <c>renameEdits</c> into an LSP workspace edit with one text document edit per file.
/// </summary>
/// <param name="edits">Tuples of file path, range and new text.</param>
/// <returns>The workspace edit for the client.</returns>
let private toWorkspaceEdit (edits: (string * Range * string) list) : WorkspaceEdit =
    let documentChanges =
        edits
        |> List.groupBy (fun (path, _, _) -> path)
        |> List.map (fun (path, fileEdits) ->
            let textEdits =
                fileEdits
                |> List.map (fun (_, range, newText) -> TextEdit(Range = range, NewText = newText))
            let textDocumentEdit =
                TextDocumentEdit(
                    TextDocument = VersionedTextDocumentIdentifier(Uri = DocumentUri.FromFileSystemPath path, Version = 0L),
                    Edits = TextEditContainer(textEdits))
            WorkspaceEditDocumentChange(textDocumentEdit))
    WorkspaceEdit(DocumentChanges = Container<WorkspaceEditDocumentChange>(documentChanges))

/// <summary>
/// Handles textDocument/prepareRename requests by checking that the cursor is on a renameable identifier.
/// </summary>
/// <param name="languageServer">The language server instance used for logging.</param>
/// <remarks>
/// The request is rejected if the cursor is not on a renameable identifier, or if the identifier range lies in
/// another file.
/// </remarks>
type PrepareRenameHandler(languageServer: ILanguageServer) =
    let logName = "PrepareRenameHandler: "
    /// <summary>
    /// Document selector that limits the registration to FPL source files.
    /// </summary>
    let documentSelector = DocumentSelector(DocumentFilter(Pattern = "**/*.fpl"))

    /// <summary>
    /// The client capability for prepareRename, stored by <c>SetCapability</c>. The handler does not read it.
    /// </summary>
    let mutable capability = RenameCapability()

    interface IPrepareRenameHandler with

        /// <summary>
        /// Returns the registration options for prepareRename requests.
        /// </summary>
        /// <returns>A <c>RenameRegistrationOptions</c> with the FPL document selector and prepare support enabled.</returns>
        member _.GetRegistrationOptions() =
            logMsg languageServer "registration options requested" $"{logName}GetRegistrationOptions"
            RenameRegistrationOptions(DocumentSelector = documentSelector, PrepareProvider = true)

        /// <summary>
        /// Returns the range of the identifier under the cursor, or rejects the request.
        /// </summary>
        /// <param name="request">The request with the document URI and the 0-based cursor position.</param>
        /// <param name="cancellationToken">Cancellation token of the request.</param>
        /// <returns>A task that yields the range of the identifier to rename.</returns>
        member _.Handle(request: PrepareRenameParams, cancellationToken: CancellationToken) =
            task {
                logMsg languageServer "Task<RangeOrPlaceholderRange>" $"{logName}Handle"

                while heap.IsEvaluating do
                    do! Task.Delay(10, cancellationToken)

                let cursorPath = normalizePath (request.TextDocument.Uri.GetFileSystemPath())
                let line = int64 request.Position.Line + 1L
                let column = int64 request.Position.Character + 1L
                logMsg languageServer $"{cursorPath} {line}:{column}" $"{logName}prepareRename request"

                match heap.SymbolTable.CursorNode cursorPath line column with
                | None ->
                    logMsg languageServer "no symbol-table node at the cursor" $"{logName}prepareRename"
                    return rejectRequest "No identifier at the cursor position."
                | Some cursorNode ->
                    logMsg languageServer $"{cursorNode.Name} '{cursorNode.FplId}'" $"{logName}prepareRename cursor node"
                    match prepareRenameAt cursorNode with
                    | Result.Error message ->
                        logMsg languageServer message $"{logName}prepareRename rejected"
                        return rejectRequest message
                    | Result.Ok(path, range) ->
                        if normalizePath path <> cursorPath then
                            logMsg languageServer $"declared in {path}" $"{logName}prepareRename rejected"
                            return rejectRequest "The identifier is declared in another file. Rename it there."
                        else
                            logMsg languageServer $"range {range.Start.Line}:{range.Start.Character}-{range.End.Line}:{range.End.Character}" $"{logName}prepareRename accepted"
                            return RangeOrPlaceholderRange(range)
            }

        /// <summary>
        /// Stores the rename capability that the client reported during initialization.
        /// </summary>
        /// <param name="cap">The client's <c>RenameCapability</c>.</param>
        member _.SetCapability(cap: RenameCapability) =
            logMsg languageServer "client capability received" $"{logName}SetCapability"
            capability <- cap

/// <summary>
/// Handles textDocument/rename requests by validating the new name and returning the workspace edit.
/// </summary>
/// <param name="languageServer">The language server instance used for logging.</param>
/// <remarks>
/// The edits come from <c>renameEdits</c>, which uses the same reference set as find-references.
/// </remarks>
type RenameHandler(languageServer: ILanguageServer) =
    let logName = "RenameHandler: "
    /// <summary>
    /// Document selector that limits the registration to FPL source files.
    /// </summary>
    let documentSelector = DocumentSelector(DocumentFilter(Pattern = "**/*.fpl"))

    /// <summary>
    /// The client capability for rename, stored by <c>SetCapability</c>. The handler does not read it.
    /// </summary>
    let mutable capability = RenameCapability()

    interface IRenameHandler with

        /// <summary>
        /// Returns the registration options for rename requests.
        /// </summary>
        /// <returns>A <c>RenameRegistrationOptions</c> with the FPL document selector and prepare support enabled.</returns>
        member _.GetRegistrationOptions() =
            logMsg languageServer "registration options requested" $"{logName}GetRegistrationOptions"
            RenameRegistrationOptions(DocumentSelector = documentSelector, PrepareProvider = true)

        /// <summary>
        /// Validates the new name and returns the edits that rename the declaration and all its references.
        /// </summary>
        /// <param name="request">The request with the document URI, the cursor position and the new name.</param>
        /// <param name="cancellationToken">Cancellation token of the request.</param>
        /// <returns>A task that yields the workspace edit, or rejects the request with a message.</returns>
        member _.Handle(request: RenameParams, cancellationToken: CancellationToken) =
            task {
                logMsg languageServer "Task<WorkspaceEdit>" $"{logName}Handle"

                while heap.IsEvaluating do
                    do! Task.Delay(10, cancellationToken)

                let cursorPath = normalizePath (request.TextDocument.Uri.GetFileSystemPath())
                let line = int64 request.Position.Line + 1L
                let column = int64 request.Position.Character + 1L
                logMsg languageServer $"{cursorPath} {line}:{column} new name '{request.NewName}'" $"{logName}rename request"

                match heap.SymbolTable.CursorNode cursorPath line column with
                | None ->
                    logMsg languageServer $"{logName}no symbol-table node at the cursor" "rename"
                    return rejectRequest $"{logName}No identifier at the cursor position."
                | Some cursorNode ->
                    logMsg languageServer $"{cursorNode.Name} '{cursorNode.FplId}'" $"{logName}rename cursor node"
                    match renameEdits cursorNode heap.SymbolTable.Nodes request.NewName with
                    | Result.Error message ->
                        logMsg languageServer message $"{logName}rename rejected"
                        return rejectRequest message
                    | Result.Ok edits ->
                        logMsg languageServer $"{edits.Length} edit(s) in {edits |> List.map (fun (p, _, _) -> p) |> List.distinct |> List.length} file(s)" $"{logName}rename accepted"
                        return toWorkspaceEdit edits
            }

        /// <summary>
        /// Stores the rename capability that the client reported during initialization.
        /// </summary>
        /// <param name="cap">The client's <c>RenameCapability</c>.</param>
        member _.SetCapability(cap: RenameCapability) =
            logMsg languageServer "client capability received" $"{logName}SetCapability"
            capability <- cap
