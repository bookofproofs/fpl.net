/// <summary>
/// Language server handlers for the textDocument/prepareRename and textDocument/rename requests of FPL files.
/// </summary>
/// <remarks>
/// The reference set is computed by <c>findReferenceNodes</c>, the same function used by find-references,
/// so rename and find-references always agree. The first version renames variables and axioms only.
/// </remarks>
module Fpl3LanguageServer.ServiceRename.RenameHandler

open System
open System.IO
open System.Text.RegularExpressions
open System.Threading
open System.Threading.Tasks
open OmniSharp.Extensions.JsonRpc
open OmniSharp.Extensions.LanguageServer.Protocol
open OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities
open OmniSharp.Extensions.LanguageServer.Protocol.Document
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open OmniSharp.Extensions.LanguageServer.Protocol.Server
open Fpl0Base.Primitives
open Fpl2Interpreter.BasicTypes
open Fpl2Interpreter.SymbolTable.Types2.Variables
open Fpl2Interpreter.SymbolTable.Types3.PredicativeBlocks
open Fpl2Interpreter.SymbolTable.GoToDefinition
open Fpl2Interpreter.SymbolTable.Storage.Heap

/// <summary>
/// Returns true if the node kind is supported by the first version of the rename service.
/// </summary>
/// <param name="node">Declaration node to test.</param>
/// <returns><c>true</c> for variables and axioms; otherwise <c>false</c>.</returns>
/// <remarks>
/// Classes, constructors, predicates and other kinds are excluded until the rename service supports them.
/// </remarks>
let private isRenameableKind (node: FplGenericNode) : bool =
    match node with
    | :? FplVariable
    | :? FplAxiom -> true
    | _ -> false

/// <summary>
/// Returns true if <paramref name="newName"/> matches the identifier grammar for the declaration's kind.
/// </summary>
/// <param name="declaration">The declaration that will be renamed.</param>
/// <param name="newName">The proposed new name.</param>
/// <returns><c>true</c> if the name is a valid identifier for the declaration's kind; otherwise <c>false</c>.</returns>
/// <remarks>
/// Variables use <c>PrimFplLowerCase</c>. All other renameable kinds use <c>PrimFplPascalCase</c>.
/// The primitives carry no anchors, so the pattern is wrapped in <c>\A(?:...)\z</c>.
/// </remarks>
let private isValidIdentifierFormat (declaration: FplGenericNode) (newName: string) : bool =
    let pattern =
        match declaration with
        | :? FplVariable -> PrimFplLowerCase
        | _ -> PrimFplPascalCase
    newName <> "" && Regex.IsMatch(newName, @"\A(?:" + pattern + @")\z", RegexOptions.CultureInvariant)

/// <summary>
/// Returns the LSP range of the identifier <c>name</c> inside the source span of <c>node</c>.
/// </summary>
/// <param name="node">Symbol-table node whose identifier is searched.</param>
/// <param name="name">Identifier text to locate.</param>
/// <param name="text">Full source text of the file that contains the node.</param>
/// <returns>The range of the identifier, or <c>None</c> if the text does not contain it inside the span.</returns>
/// <remarks>
/// Symbol-table positions are 1-based (FParsec). LSP positions are 0-based, so the conversion subtracts one.
/// </remarks>
let private identifierRange (node: FplGenericNode) (name: string) (text: string) : Range option =
    let spanStart = int node.StartPos.Index
    let spanEnd = min text.Length (int node.EndPos.Index)
    if name = "" || spanStart < 0 || spanStart >= spanEnd then
        None
    else
        let span = text.Substring(spanStart, spanEnd - spanStart)
        match span.IndexOf(name, StringComparison.Ordinal) with
        | -1 -> None
        | offset ->
            let toPosition (index: int) =
                let before = text.Substring(0, index)
                let line = before |> Seq.filter (fun c -> c = '\n') |> Seq.length
                let lastNewline = before.LastIndexOf '\n'
                Position(Line = line, Character = index - (lastNewline + 1))
            let startIndex = spanStart + offset
            Some(Range(Start = toPosition startIndex, End = toPosition (startIndex + name.Length)))

/// <summary>
/// Returns true if <paramref name="newName"/> is a valid new name for the given declaration.
/// </summary>
/// <param name="declaration">The declaration that will be renamed.</param>
/// <param name="newName">The proposed new name.</param>
/// <returns><c>true</c> if the format is valid, the name is not a keyword and it does not collide with the scope.</returns>
/// <remarks>
/// The collision check looks only at the <c>Scope</c> of the ultimate block node of the declaration.
/// </remarks>
let private isValidNewName (declaration: FplGenericNode) (newName: string) : bool =
    let collisionFree =
        match declaration.UltimateBlockNode with
        | Some ultimate -> not (ultimate.Scope.ContainsKey newName)
        | None -> true
    isValidIdentifierFormat declaration newName && not (keyWordSet.Contains newName) && collisionFree

/// <summary>
/// Returns the renameable declaration for the cursor node, or <c>None</c> if the cursor does not identify one.
/// </summary>
/// <param name="cursorNode">Innermost symbol-table node containing the cursor.</param>
/// <returns>The declaration to rename, or <c>None</c>.</returns>
/// <remarks>
/// Variables resolve to themselves, because their <c>RefersTo</c> is the declared type. All other nodes follow
/// the same rule as <c>findReferenceNodes</c>.
/// </remarks>
let private renameDeclarationOf (cursorNode: FplGenericNode) : FplGenericNode option =
    let target = cursorTargetNode cursorNode
    let declaration =
        match target with
        | :? FplGenericVariable -> target
        | _ -> defaultArg (resolveDeclaration target) target
    if isRenameableKind declaration && declaration.FplId <> "" then Some declaration else None

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
/// Builds one <c>TextDocumentEdit</c> per file for all nodes that must be renamed.
/// </summary>
/// <param name="nodes">Nodes whose identifier is replaced, across all files.</param>
/// <param name="oldName">Current identifier text.</param>
/// <param name="newName">Replacement identifier text.</param>
/// <returns>
/// The workspace edit, or an error message if an identifier range cannot be located in the source text.
/// </returns>
let private buildWorkspaceEdit (nodes: FplGenericNode list) (oldName: string) (newName: string)
    : Result<WorkspaceEdit, string> =
    let perFile =
        nodes
        |> List.groupBy (fun node -> node.FilePath)
        |> List.map (fun (path, fileNodes) ->
            let text = File.ReadAllText path
            let ranges = fileNodes |> List.map (fun node -> identifierRange node oldName text)
            if List.exists Option.isNone ranges then
                Result.Error $"Cannot locate the identifier '{oldName}' in '{path}'."
            else
                Result.Ok(path, List.choose id ranges))
    match List.tryPick (function Result.Error e -> Some e | Result.Ok _ -> None) perFile with
    | Some error -> Result.Error error
    | None ->
        let documentChanges =
            perFile
            |> List.choose (function Result.Ok(path, ranges) -> Some(path, ranges) | Result.Error _ -> None)
            |> List.map (fun (path, ranges) ->
                let edits = ranges |> List.map (fun range -> TextEdit(Range = range, NewText = newName))
                let textDocumentEdit =
                    TextDocumentEdit(
                        TextDocument = VersionedTextDocumentIdentifier(Uri = DocumentUri.FromFileSystemPath path, Version = 0L),
                        Edits = TextEditContainer(edits))
                WorkspaceEditDocumentChange(textDocumentEdit))
        Result.Ok(WorkspaceEdit(DocumentChanges = Container<WorkspaceEditDocumentChange>(documentChanges)))

/// <summary>
/// Handles textDocument/prepareRename requests by checking that the cursor is on a renameable identifier.
/// </summary>
/// <param name="languageServer">The language server instance used for logging.</param>
/// <remarks>
/// Rejects the request with a message if the cursor is not on a renameable identifier.
/// </remarks>
type PrepareRenameHandler(languageServer: ILanguageServer) =

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
            RenameRegistrationOptions(DocumentSelector = documentSelector, PrepareProvider = true)

        /// <summary>
        /// Returns the range of the identifier under the cursor, or rejects the request.
        /// </summary>
        /// <param name="request">The request with the document URI and the 0-based cursor position.</param>
        /// <param name="cancellationToken">Cancellation token of the request.</param>
        /// <returns>A task that yields the range of the identifier to rename.</returns>
        member _.Handle(request: PrepareRenameParams, cancellationToken: CancellationToken) =
            task {
                while heap.IsEvaluating do
                    do! Task.Delay(10, cancellationToken)

                let cursorPath = normalizePath (request.TextDocument.Uri.GetFileSystemPath())
                let line = int64 request.Position.Line + 1L
                let column = int64 request.Position.Character + 1L

                match heap.SymbolTable.CursorNode cursorPath line column with
                | None -> return rejectRequest "No identifier at the cursor position."
                | Some cursorNode ->
                    match renameDeclarationOf cursorNode with
                    | None -> return rejectRequest "The identifier under the cursor cannot be renamed."
                    | Some _ ->
                        let target = cursorTargetNode cursorNode
                        let text = File.ReadAllText target.FilePath
                        match identifierRange target target.FplId text with
                        | Some range -> return RangeOrPlaceholderRange(range)
                        | None -> return rejectRequest "The identifier range cannot be determined."
            }

        /// <summary>
        /// Stores the rename capability that the client reported during initialization.
        /// </summary>
        /// <param name="cap">The client's <c>RenameCapability</c>.</param>
        member _.SetCapability(cap: RenameCapability) =
            capability <- cap

/// <summary>
/// Handles textDocument/rename requests by validating the new name and returning the workspace edit.
/// </summary>
/// <param name="languageServer">The language server instance used for logging.</param>
/// <remarks>
/// The reference set is computed by <c>findReferenceNodes</c>, so the edits match find-references.
/// </remarks>
type RenameHandler(languageServer: ILanguageServer) =

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
            RenameRegistrationOptions(DocumentSelector = documentSelector, PrepareProvider = true)

        /// <summary>
        /// Validates the new name and returns the edits that rename the declaration and all its references.
        /// </summary>
        /// <param name="request">The request with the document URI, the cursor position and the new name.</param>
        /// <param name="cancellationToken">Cancellation token of the request.</param>
        /// <returns>A task that yields the workspace edit, or rejects the request with a message.</returns>
        member _.Handle(request: RenameParams, cancellationToken: CancellationToken) =
            task {
                while heap.IsEvaluating do
                    do! Task.Delay(10, cancellationToken)

                let cursorPath = normalizePath (request.TextDocument.Uri.GetFileSystemPath())
                let line = int64 request.Position.Line + 1L
                let column = int64 request.Position.Character + 1L

                match heap.SymbolTable.CursorNode cursorPath line column with
                | None -> return rejectRequest "No identifier at the cursor position."
                | Some cursorNode ->
                    match renameDeclarationOf cursorNode with
                    | None -> return rejectRequest "The identifier under the cursor cannot be renamed."
                    | Some declaration ->
                        if not (isValidNewName declaration request.NewName) then
                            return rejectRequest $"'{request.NewName}' is not a valid name or already exists in this scope."
                        else
                            let references = findReferenceNodes cursorNode heap.SymbolTable.Nodes
                            match buildWorkspaceEdit references declaration.FplId request.NewName with
                            | Result.Ok edit -> return edit
                            | Result.Error message -> return rejectRequest message
            }

        /// <summary>
        /// Stores the rename capability that the client reported during initialization.
        /// </summary>
        /// <param name="cap">The client's <c>RenameCapability</c>.</param>
        member _.SetCapability(cap: RenameCapability) =
            capability <- cap
