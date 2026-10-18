module Fpl3LanguageServer.ServiceGoToDefinition.Handler
open System
open System.Collections.Generic
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
                while heap.IsEvaluating do ()

                let filePath = request.TextDocument.Uri.GetFileSystemPath()
                // LSP positions are 0-based, FParsec positions are 1-based (int64)
                let line = int64 request.Position.Line + 1L
                let column = int64 request.Position.Character + 1L
                let nodePaths = collectNodePaths heap.SymbolTable.Root

                // Innermost node containing the cursor: greatest start, then smallest end
                let cursorNode =
                    nodePaths
                    |> Seq.filter (fun kv ->
                        String.Equals(kv.Value, filePath, StringComparison.OrdinalIgnoreCase)
                        && containsCursor kv.Key line column)
                    |> Seq.map (fun kv -> kv.Key)
                    |> Seq.sortBy (fun n ->
                        (n.StartPos.Line, n.StartPos.Column, -n.EndPos.Line, -n.EndPos.Column))
                    |> Seq.tryLast

                match cursorNode |> Option.bind resolveDeclaration with
                | Some declaration ->
                    match nodePaths.TryGetValue declaration with
                    | true, declPath ->
                        // FParsec int64 -> OmniSharp int, and 1-based -> 0-based
                        let start =
                            Position(
                                Line = int (declaration.StartPos.Line - 1L),
                                Character = int (declaration.StartPos.Column - 1L))
                        let finish =
                            Position(
                                Line = int (declaration.EndPos.Line - 1L),
                                Character = int (declaration.EndPos.Column - 1L))
                        let location =
                            Location(
                                Uri = DocumentUri.FromFileSystemPath(declPath),
                                Range = Range(Start = start, End = finish))
                        return LocationOrLocationLinks([| LocationOrLocationLink(location) |])
                    | _ -> return LocationOrLocationLinks()
                | None -> return LocationOrLocationLinks()
            }

        member _.SetCapability(cap: DefinitionCapability) =
            capability <- cap
