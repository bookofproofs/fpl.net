/// <summary>
/// Language server handler for the textDocument/references request of FPL files.
/// </summary>
/// <remarks>
/// The handler resolves the symbol-table node under the cursor and returns the locations of all nodes
/// that resolve to the same declaration, as computed by <c>findReferenceNodes</c> in
/// <c>Fpl2Interpreter.SymbolTable.GoToDefinition</c>.
/// </remarks>
module Fpl3LanguageServer.ServiceFindReferences.FindReferencesHandler
open System
open System.IO
open System.Threading
open System.Threading.Tasks
open OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities
open OmniSharp.Extensions.LanguageServer.Protocol.Document
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open OmniSharp.Extensions.LanguageServer.Protocol.Server
open OmniSharp.Extensions.LanguageServer.Protocol
open Fpl2Interpreter.BasicTypes
open Fpl2Interpreter.SymbolTable.GoToDefinition
open Fpl2Interpreter.SymbolTable.Storage.Heap
open Fpl3LanguageServer.Buffers.Logging

/// <summary>
/// Normalizes a file path so that paths from the editor (backslashes) and from the symbol table
/// (forward slashes) can be compared.
/// </summary>
/// <param name="path">File path to normalize. May be <c>null</c>, empty, or whitespace.</param>
/// <returns>
/// The full path with forward slashes and no trailing separator, or an empty string if <paramref name="path"/>
/// is <c>null</c>, empty, or whitespace.
/// </returns>
/// <remarks>
/// <c>Path.GetFullPath</c> can throw for malformed paths. The function does not catch this exception.
/// </remarks>
let private normalizePath (path: string) =
    if String.IsNullOrWhiteSpace path then
        ""
    else
        Path.GetFullPath(path).Replace('\\', '/').TrimEnd('/')

/// <summary>
/// Converts a symbol-table node to an LSP location, using the node's identifier span (approximation).
/// </summary>
/// <param name="node">Symbol-table node to convert.</param>
/// <returns>The LSP <c>Location</c> of the node, with the node's file as URI.</returns>
/// <remarks>
/// The range starts at the node's <c>StartPos</c> and ends after <c>FplId.Length</c> characters on the same line.
/// This is an approximation: it is only exact for nodes whose <c>FplId</c> is the source text at the start position.
/// Nodes with an empty <c>FplId</c> produce an empty range. Nodes that span several lines (for example, nodes
/// whose identifier is written differently in the source) can get a range that does not match the source text.
/// </remarks>
let private toLocation (node: FplGenericNode) : Location =
    // FParsec int64 -> LSP int; FParsec lines are 1-based, LSP lines are 0-based
    let toLspLine (line: int64) = max 0 (int line - 1)
    let toLspCharacter (column: int64) = max 0 (int column)
    let start =
        Position(
            Line = toLspLine node.StartPos.Line,
            Character = toLspCharacter node.StartPos.Column)
    // Approximation until the identifier range is known: same line, length of the identifier
    let finish =
        Position(
            Line = start.Line,
            Character = start.Character + node.FplId.Length)
    Location(
        Uri = DocumentUri.FromFileSystemPath(node.FilePath),
        Range = Range(Start = start, End = finish))

/// <summary>
/// Handles textDocument/references requests by returning the locations of all nodes that resolve to the
/// declaration under the cursor.
/// </summary>
/// <param name="languageServer">The language server instance used for logging.</param>
/// <remarks>
/// The handler registers for files matching <c>**/*.fpl</c>.
/// The handler ignores <c>ReferenceParams.Context.IncludeDeclaration</c>. <c>findReferenceNodes</c> always
/// includes the declaration in its result, so a client that sets <c>IncludeDeclaration = false</c> still receives it.
/// </remarks>
type ReferencesHandler(languageServer: ILanguageServer) =

    /// <summary>
    /// Document selector that limits the registration to FPL source files.
    /// </summary>
    let documentSelector = DocumentSelector(DocumentFilter(Pattern = "**/*.fpl"))

    /// <summary>
    /// The client capability for references, stored by <c>SetCapability</c>. The handler does not read it.
    /// </summary>
    let mutable capability = ReferenceCapability()

    interface IReferencesHandler with

        /// <summary>
        /// Returns the registration options that tell the client which documents this handler supports.
        /// </summary>
        /// <returns>A <c>ReferenceRegistrationOptions</c> with the FPL document selector.</returns>
        member _.GetRegistrationOptions() =
            ReferenceRegistrationOptions(DocumentSelector = documentSelector)

        /// <summary>
        /// Handles a references request: finds the node under the cursor and returns the locations of all nodes
        /// that resolve to the same declaration.
        /// </summary>
        /// <param name="request">The request with the document URI and the 0-based cursor position.</param>
        /// <param name="cancellationToken">Cancellation token of the request. The handler does not check it.</param>
        /// <returns>
        /// A task that yields a <c>LocationContainer</c>. The container is empty if no node is under the cursor.
        /// </returns>
        /// <remarks>
        /// The method waits in a busy loop until <c>heap.IsEvaluating</c> is false. This blocks the thread that runs
        /// the task while the interpreter is busy.
        /// TODO: consider <c>Task.Delay</c> in the loop or waiting for an event, and honor <paramref name="cancellationToken"/>.
        /// The request position is converted from 0-based LSP to 1-based FParsec values before the cursor lookup.
        /// </remarks>
        member _.Handle(request: ReferenceParams, cancellationToken: CancellationToken) : Task<LocationContainer> =
            task {
                logMsg languageServer "Task<LocationContainer>" "ReferencesHandler.Handle"

                // wait for the interpreter
                while heap.IsEvaluating do ()

                let cursorPath = normalizePath (request.TextDocument.Uri.GetFileSystemPath())
                // LSP positions are 0-based, FParsec positions are 1-based (int64)
                let line = int64 request.Position.Line + 1L
                let column = int64 request.Position.Character + 1L

                let cursorNode = heap.SymbolTable.CursorNode cursorPath line column

                let nodes =
                    match cursorNode with
                    | None -> []
                    | Some node ->
                        findReferenceNodes node heap.SymbolTable.Nodes

                logMsg languageServer $"{nodes.Length} location(s)" "references"
                return LocationContainer(nodes |> List.map toLocation)
            }

        /// <summary>
        /// Stores the reference capability that the client reported during initialization.
        /// </summary>
        /// <param name="cap">The client's <c>ReferenceCapability</c>.</param>
        /// <remarks>
        /// The stored value is not used by <c>Handle</c>. Keep it only if other parts of the server need it.
        /// </remarks>
        member _.SetCapability(cap: ReferenceCapability) =
            capability <- cap
