module Fpl3LanguageServer.ServiceGoToDefinition.Handler
open System
open System.Collections.Generic
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
/// Walks the whole symbol table once and maps every reachable node to the file path it belongs to.
/// The path is inherited from the nearest ancestor that defines one (theories define it).
/// </summary>
/// <param name="root">Root node of the symbol table.</param>
/// <returns>Dictionary from node (by reference) to file path.</returns>
let private collectNodePaths (root: FplGenericNode) : Dictionary<FplGenericNode, string> =
    let paths = Dictionary<FplGenericNode, string>(HashIdentity.Reference)

    let rec walk (node: FplGenericNode) (inheritedPath: string) =
        if not (paths.ContainsKey node) then
            let path = defaultArg node.FilePath inheritedPath
            paths.[node] <- path
            for child in node.Scope.Values do
                walk child path
            for child in node.ArgList do
                walk child path

    walk root ""
    paths

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
/// Checks whether a 1-based (line, column) position lies within the node's start and end positions.
/// </summary>
/// <param name="node">Node to test.</param>
/// <param name="line">1-based line (FParsec int64).</param>
/// <param name="column">1-based column (FParsec int64).</param>
/// <returns>True when the position lies inside the node.</returns>
let private containsCursor (node: FplGenericNode) (line: int64) (column: int64) =
    let startPos = node.StartPos
    let endPos = node.EndPos
    (startPos.Line, startPos.Column) <= (line, column) && (line, column) <= (endPos.Line, endPos.Column)

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
                let nodePaths = collectNodePaths heap.SymbolTable.Root

                // Try to find the innermost symbol table node containing the cursor: greatest start, then smallest end
                let cursorNode =
                    nodePaths
                    |> Seq.filter (fun kv ->
                        containsCursor kv.Key line column
                        && String.Equals(normalizePath kv.Value, cursorPath, StringComparison.OrdinalIgnoreCase))
                    |> Seq.map (fun kv -> kv.Key)
                    |> Seq.sortBy (fun n ->
                        (n.StartPos.Line, n.StartPos.Column, -n.EndPos.Line, -n.EndPos.Column))
                    |> Seq.tryLast

                // Try to find the declaration / definition of the innermost symbol table node
                let resolvedNodeOpt = 
                    match cursorNode with
                    | None -> None
                    | Some node -> resolveDeclaration node

                // Try to return the definition location to the F12 "go-to-definition" service
                match resolvedNodeOpt with
                | Some declaration ->
                    match nodePaths.TryGetValue declaration with
                    | true, declPath when declPath <> "" ->
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
                                Uri = DocumentUri.FromFileSystemPath(declPath),
                                Range = Range(Start = start, End = finish))
                        return LocationOrLocationLinks([| LocationOrLocationLink(location) |])
                    | _ ->
                        logMsg languageServer "F12" "no file of definition node found"
                        return LocationOrLocationLinks()
                | None ->
                    logMsg languageServer "F12" "no definition found"
                    return LocationOrLocationLinks()
            }

        member _.SetCapability(cap: DefinitionCapability) =
            capability <- cap
