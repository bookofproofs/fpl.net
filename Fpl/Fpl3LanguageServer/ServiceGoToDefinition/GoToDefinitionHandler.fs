/// <summary>
/// Language server handler for the textDocument/definition request (go-to-definition, F12) of FPL files.
/// </summary>
/// <remarks>
/// The handler resolves the symbol-table node under the cursor, maps it to its declaration with
/// <c>cursorTargetNode</c> and <c>resolveDeclaration</c> from <c>Fpl2Interpreter.SymbolTable.GoToDefinition</c>,
/// and returns the declaration's location as a <c>LocationOrLocationLinks</c> with one entry.
/// Positions are converted between the 0-based LSP convention and the 1-based FParsec convention of the symbol table.
/// </remarks>
module Fpl3LanguageServer.ServiceGoToDefinition.GoToDefinitionHandler
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
/// Handles textDocument/definition requests by resolving the declaration of the symbol under the cursor.
/// </summary>
/// <param name="languageServer">The language server instance used for logging.</param>
/// <remarks>
/// The handler registers for files matching <c>**/*.fpl</c>.
/// </remarks>
type DefinitionHandler(languageServer: ILanguageServer) =
    let logName = "DefinitionHandler: "
    /// <summary>
    /// Document selector that limits the registration to FPL source files.
    /// </summary>
    let documentSelector = DocumentSelector(DocumentFilter(Pattern = "**/*.fpl"))

    /// <summary>
    /// The client capability for definitions, stored by <c>SetCapability</c>. The handler does not read it.
    /// </summary>
    let mutable capability = DefinitionCapability()

    interface IDefinitionHandler with

        /// <summary>
        /// Returns the registration options that tell the client which documents this handler supports.
        /// </summary>
        /// <returns>A <c>DefinitionRegistrationOptions</c> with the FPL document selector.</returns>
        member _.GetRegistrationOptions() =
            DefinitionRegistrationOptions(DocumentSelector = documentSelector)

        /// <summary>
        /// Handles a definition request: finds the node under the cursor, resolves its declaration, and returns
        /// the declaration's location.
        /// </summary>
        /// <param name="request">The request with the document URI and the 0-based cursor position.</param>
        /// <param name="cancellationToken">Cancellation token of the request. The handler does not check it.</param>
        /// <returns>
        /// A task that yields a <c>LocationOrLocationLinks</c> with one location, or an empty result if no
        /// declaration is found.
        /// </returns>
        /// <remarks>
        /// The method waits in a busy loop until <c>heap.IsEvaluating</c> is false. This blocks the thread that runs
        /// the task while the interpreter is busy.
        /// </remarks>
        member _.Handle(request: DefinitionParams, cancellationToken: CancellationToken) : Task<LocationOrLocationLinks> =
            task {
                logMsg languageServer "Task<LocationOrLocationLinks>" $"{logName}Handle"

                // wait for the interpreter
                while heap.IsEvaluating do ()

                // normalize the cursor path and convert the cursor position to LSP standard
                let cursorPath = normalizePath (request.TextDocument.Uri.GetFileSystemPath())
                // LSP positions are 0-based, FParsec positions are 1-based (int64)
                let line = int64 request.Position.Line + 1L
                let column = int64 request.Position.Character + 1L

                // Try to find the innermost symbol table node containing the cursor: greatest start, then smallest end
                let cursorNode = heap.SymbolTable.CursorNode cursorPath line column

                if cursorNode.IsSome then 
                    logMsg languageServer $"{cursorNode.Value.Name} {cursorNode.Value.Type SignatureType.Name}" "cursorNode" 
                else
                    logMsg languageServer "None" "cursorNode" 
                logMsg languageServer $"{cursorPath} {line} {column}" $"{logName}Coords" 

                // Try to find the declaration / definition of the innermost symbol table node
                let resolvedNodeOpt =
                    match cursorNode with
                    | None -> None
                    | Some node -> resolveDeclaration (cursorTargetNode node)

                // Try to return the definition location to the F12 "go-to-definition" service
                match resolvedNodeOpt with
                | Some declaration ->
                    // FParsec int64 -> OmniSharp int. Node columns are not always 1-based
                    // (block nodes such as "axiom" report column 0), so clamp at zero.
                    let toLspLine (line: int64) = max 0 (int line - 1)
                    let toLspCharacter (column: int64) = max 0 (int column)
                    let start =
                        Position(
                            Line = toLspLine declaration.StartPos.Line,
                            Character = toLspCharacter declaration.StartPos.Column)
                    let finish =
                        Position(
                            Line = toLspLine declaration.EndPos.Line,
                            Character = toLspCharacter declaration.EndPos.Column)
                    let location =
                        Location(
                            Uri = DocumentUri.FromFileSystemPath(declaration.FilePath),
                            Range = Range(Start = start, End = finish))
                    return LocationOrLocationLinks([| LocationOrLocationLink(location) |])
                | None ->
                    logMsg languageServer "F12" $"{logName}no definition found"
                    return LocationOrLocationLinks()
            }

        /// <summary>
        /// Stores the definition capability that the client reported during initialization.
        /// </summary>
        /// <param name="cap">The client's <c>DefinitionCapability</c>.</param>
        /// <remarks>
        /// The stored value is not used by <c>Handle</c>. Keep it only if other parts of the server need it.
        /// </remarks>
        member _.SetCapability(cap: DefinitionCapability) =
            logMsg languageServer "client capability received" $"{logName}SetCapability"
            capability <- cap
