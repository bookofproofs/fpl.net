module Fpl3LanguageServer.ServiceFindReferences.Handler
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
/// Converts a symbol-table node to an LSP location, using the node's identifier span (approximation).
/// </summary>
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
/// Handles textDocument/references requests by returning all nodes that resolve to the declaration
/// under the cursor, optionally including the declaration itself.
/// </summary>
type ReferencesHandler(languageServer: ILanguageServer) =

    let documentSelector = DocumentSelector(DocumentFilter(Pattern = "**/*.fpl"))

    let mutable capability = ReferenceCapability()

    interface IReferencesHandler with

        member _.GetRegistrationOptions() =
            ReferenceRegistrationOptions(DocumentSelector = documentSelector)

        member _.Handle(request: ReferenceParams, cancellationToken: CancellationToken) : Task<LocationContainer> =
            task {
                logMsg languageServer "Task<LocationContainer>" "ReferencesHandler.Handle"

                // wait for the interpreter
                while heap.IsEvaluating do ()

                let cursorPath = normalizePath (request.TextDocument.Uri.GetFileSystemPath())
                // LSP positions are 0-based, FParsec positions are 1-based (int64)
                let line = int64 request.Position.Line + 1L
                let column = int64 request.Position.Character + 1L

                let cursorNode = heap.Eval.CursorNode cursorPath line column

                // The declaration is either the resolved target of the cursor node or the cursor node itself
                let declarationOpt =
                    match cursorNode with
                    | None -> None
                    | Some node ->
                        match resolveDeclaration node with
                        | Some declaration -> Some declaration
                        | None -> Some node

                match declarationOpt with
                | Some declaration ->
                    let index = getReferenceIndex (box heap.SymbolTable.Root) heap.Eval.FlattenedNodes
                    let references =
                        findReferences index declaration
                        |> List.filter (fun node -> not (obj.ReferenceEquals(node, declaration)))
                        |> List.map toLocation
                    let withDeclaration =
                        if request.Context.IncludeDeclaration && declaration.FilePath <> "" then
                            toLocation declaration :: references
                        else
                            references
                    logMsg languageServer $"{withDeclaration.Length} location(s)" "references"
                    return LocationContainer(withDeclaration)
                | None ->
                    logMsg languageServer "None" "no declaration under cursor"
                    return LocationContainer()
            }

        member _.SetCapability(cap: ReferenceCapability) =
            capability <- cap
