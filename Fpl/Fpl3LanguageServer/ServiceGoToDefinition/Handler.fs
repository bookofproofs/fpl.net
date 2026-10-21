module Fpl3LanguageServer.ServiceGoToDefinition.Handler
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
/// (forward slashes) can be compared. Empty paths stay empty.
/// </summary>
/// <param name="path">File path to normalize.</param>
/// <returns>Full path with forward slashes and no trailing separator.</returns>
let private normalizePath (path: string) =
    if String.IsNullOrWhiteSpace path then
        ""
    else
        Path.GetFullPath(path).Replace('\\', '/').TrimEnd('/')

/// <summary>
/// Handles textDocument/definition requests by resolving the declaration of the symbol under the cursor.
/// </summary>
type DefinitionHandler(languageServer: ILanguageServer) =

    let documentSelector = DocumentSelector(DocumentFilter(Pattern = "**/*.fpl"))

    let mutable capability = DefinitionCapability()

    interface IDefinitionHandler with

        member _.GetRegistrationOptions() =
            DefinitionRegistrationOptions(DocumentSelector = documentSelector)

        member _.Handle(request: DefinitionParams, cancellationToken: CancellationToken) : Task<LocationOrLocationLinks> =
            task {
                logMsg languageServer "Task<LocationOrLocationLinks>" "DefinitionHandler.Handle"

                // wait for the interpreter
                while heap.IsEvaluating do ()

                // normalize the cursor path and convert the cursor position to LSP standard
                let cursorPath = normalizePath (request.TextDocument.Uri.GetFileSystemPath())
                // LSP positions are 0-based, FParsec positions are 1-based (int64)
                let line = int64 request.Position.Line + 1L
                let column = int64 request.Position.Character + 1L

                // Try to find the innermost symbol table node containing the cursor: greatest start, then smallest end
                let cursorNode = heap.Eval.CursorNode cursorPath line column

                if cursorNode.IsSome then 
                    logMsg languageServer $"{cursorNode.Value.Name} {cursorNode.Value.Type SignatureType.Name}" "cursorNode" 
                else
                    logMsg languageServer "None" "cursorNode" 
                logMsg languageServer $"{cursorPath} {line} {column}" "Coords" 

                // Try to find the declaration / definition of the innermost symbol table node
                let resolvedNodeOpt = 
                    match cursorNode with
                    | None -> None
                    | Some node -> resolveDeclaration node

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
                    logMsg languageServer "F12" "no definition found"
                    return LocationOrLocationLinks()
            }

        member _.SetCapability(cap: DefinitionCapability) =
            capability <- cap
