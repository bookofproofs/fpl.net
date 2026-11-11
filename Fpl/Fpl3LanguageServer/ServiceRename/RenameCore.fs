/// <summary>
/// Pure rename logic of the FPL language server: declaration lookup, new-name validation and edit computation.
/// </summary>
/// <remarks>
/// The reference set is computed by <c>findReferenceNodes</c>, the same function used by find-references,
/// so rename and find-references always agree. Identifier ranges are read from the source text, not approximated
/// from the identifier length. The module does not depend on the LSP request types, so it can be tested directly.
/// </remarks>
module Fpl3LanguageServer.ServiceRename.RenameCore

open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open Fpl0Base.Primitives
open Fpl2Interpreter.BasicTypes
open Fpl2Interpreter.SymbolTable.Types2.Variables
open Fpl2Interpreter.SymbolTable.GoToDefinition

/// <summary>
/// Returns true if the node is a proof or a corollary. These nodes are their own declarations.
/// </summary>
/// <param name="node">Node to test.</param>
/// <returns><c>true</c> for proofs and corollaries; otherwise <c>false</c>.</returns>
/// <remarks>
/// Mirrors the special case in <c>findReferenceNodes</c>. Depends on the <c>Name</c> of these nodes matching
/// <c>LiteralPrfL</c> and <c>LiteralCorL</c>, which is not verified yet.
/// </remarks>
let private isProofOrCorollary (node: FplGenericNode) : bool =
    node.Name = LiteralPrfL || node.Name = LiteralCorL

/// <summary>
/// Resolves the declaration that a cursor node refers to.
/// </summary>
/// <param name="cursorNode">Innermost symbol-table node containing the cursor.</param>
/// <returns>
/// <c>Result.Ok</c> with the declaration, or <c>Result.Error</c> with a message for the client.
/// </returns>
/// <remarks>
/// Variables, proofs and corollaries are their own declarations. Every other node uses <c>resolveDeclaration</c>
/// on its cursor target. An unresolved reference, an intrinsic (built-in) symbol and a node without an identifier
/// are rejected.
/// </remarks>
let private declarationOf (cursorNode: FplGenericNode) : Result<FplGenericNode, string> =
    let target = cursorTargetNode cursorNode
    let declaration =
        match target with
        | :? FplGenericVariable -> Some target
        | _ when isProofOrCorollary target -> Some target
        | _ -> resolveDeclaration target
    match declaration with
    | None -> Result.Error "The identifier under the cursor does not resolve to a declaration."
    | Some node when node.IsIntrinsic -> Result.Error $"The built-in symbol '{node.FplId}' cannot be renamed."
    | Some node when node.FplId = "" -> Result.Error "The node under the cursor has no identifier."
    | Some node -> Result.Ok node

/// <summary>
/// Returns true if the text matches the whole pattern from <c>Fpl0Base.Primitives</c>.
/// </summary>
/// <param name="pattern">Unanchored pattern, such as <c>PrimFplLowerCase</c>.</param>
/// <param name="name">Text to test.</param>
/// <returns><c>true</c> if the whole text matches the pattern; otherwise <c>false</c>.</returns>
/// <remarks>
/// The primitives carry no anchors, so the pattern is wrapped in <c>\A(?:...)\z</c>. Unlike <c>^</c> and <c>$</c>,
/// <c>\z</c> rejects a trailing newline.
/// </remarks>
let private matchesGrammar (pattern: string) (name: string) : bool =
    name <> "" && Regex.IsMatch(name, @"\A(?:" + pattern + @")\z", RegexOptions.CultureInvariant)

/// <summary>
/// Returns true if <paramref name="newName"/> has the identifier form required for the declaration.
/// </summary>
/// <param name="declaration">The declaration that will be renamed.</param>
/// <param name="newName">The proposed new name.</param>
/// <returns><c>true</c> if the form is valid; otherwise <c>false</c>.</returns>
/// <remarks>
/// Variables use <c>PrimFplLowerCase</c>. Argument identifiers, which start with a digit, must be numeric.
/// All other declarations use <c>PrimFplPascalCase</c>.
/// </remarks>
let private isValidIdentifierForm (declaration: FplGenericNode) (newName: string) : bool =
    match declaration with
    | :? FplGenericVariable -> matchesGrammar PrimFplLowerCase newName
    | _ when declaration.FplId <> "" && Char.IsDigit declaration.FplId[0] ->
        newName <> "" && String.forall Char.IsDigit newName
    | _ -> matchesGrammar PrimFplPascalCase newName

/// <summary>
/// Validates a proposed new name for the declaration.
/// </summary>
/// <param name="declaration">The declaration that will be renamed.</param>
/// <param name="newName">The proposed new name.</param>
/// <returns><c>Result.Ok ()</c> if the name is valid; otherwise <c>Result.Error</c> with the reason.</returns>
/// <remarks>
/// Checks the identifier form, then the keyword set, then the scope of the declaration. The scope check ignores
/// the declaration itself, so renaming a node to its current name is allowed.
/// </remarks>
let private validateNewName (declaration: FplGenericNode) (newName: string) : Result<unit, string> =
    let collides =
        match declaration.UltimateBlockNode with
        | Some ultimate ->
            match ultimate.Scope.TryGetValue newName with
            | true, existing -> not (obj.ReferenceEquals(existing, declaration))
            | _ -> false
        | None -> false
    if not (isValidIdentifierForm declaration newName) then
        Result.Error $"'{newName}' is not a valid identifier for '{declaration.FplId}'."
    elif keyWordSet.Contains newName then
        Result.Error $"'{newName}' is a keyword and cannot be used as an identifier."
    elif collides then
        Result.Error $"'{newName}' already exists in the scope of '{declaration.FplId}'."
    else
        Result.Ok()

/// <summary>
/// Returns true if the character can be part of an FPL identifier.
/// </summary>
/// <param name="c">Character to test.</param>
/// <returns><c>true</c> for letters, digits, underscore and dollar sign; otherwise <c>false</c>.</returns>
let private isIdentifierChar (c: char) : bool =
    Char.IsLetterOrDigit c || c = '_' || c = '$'

/// <summary>
/// Finds the first occurrence of <paramref name="name"/> in <paramref name="span"/> that is not part of a longer identifier.
/// </summary>
/// <param name="span">Source text to search.</param>
/// <param name="name">Identifier to find.</param>
/// <returns>The 0-based offset inside <paramref name="span"/>, or <c>None</c>.</returns>
/// <remarks>
/// Checks the characters before and after each match, so <c>x</c> does not match inside <c>max</c>, and
/// <c>Le$1</c> does not match inside <c>Le$1$2</c>. This avoids a regular expression.
/// </remarks>
let private findWholeWord (span: string) (name: string) : int option =
    let rec search (from: int) : int option =
        if from > span.Length - name.Length then
            None
        else
            match span.IndexOf(name, from, StringComparison.Ordinal) with
            | -1 -> None
            | index ->
                let startsWord = (index = 0) || not (isIdentifierChar span[index - 1])
                let endIndex = index + name.Length
                let endsWord = (endIndex >= span.Length) || not (isIdentifierChar span[endIndex])
                if startsWord && endsWord then Some index else search (index + 1)
    if name = "" then None else search 0

/// <summary>
/// Converts a 0-based offset in a text into an LSP position.
/// </summary>
/// <param name="text">Full source text.</param>
/// <param name="index">0-based character offset.</param>
/// <returns>The LSP position with a 0-based line and character.</returns>
/// <remarks>
/// Lines are separated by <c>\n</c>. A preceding <c>\r</c> stays on the previous line, so CRLF files work.
/// </remarks>
let private toPosition (text: string) (index: int) : Position =
    let before = text.Substring(0, index)
    let line = before |> Seq.filter (fun c -> c = '\n') |> Seq.length
    let lastNewline = before.LastIndexOf '\n'
    Position(Line = line, Character = index - (lastNewline + 1))

/// <summary>
/// Returns the exact LSP range of the node's identifier in the source text.
/// </summary>
/// <param name="node">Symbol-table node whose <c>FplId</c> is located.</param>
/// <param name="text">Full source text of the file that contains the node.</param>
/// <returns>The range of the identifier, or <c>None</c> if it is not found inside the node's span.</returns>
/// <remarks>
/// <c>StartPos.Index</c> and <c>EndPos.Index</c> are 0-based character offsets into the file text. The identifier is
/// searched as a whole word inside this span. Some nodes start before the identifier, for example <c>base.</c> or
/// <c>.</c> in dotted usages, and the search skips that prefix.
/// </remarks>
let private identifierRange (node: FplGenericNode) (text: string) : Range option =
    let name = node.FplId
    let spanStart = int node.StartPos.Index
    let spanEnd = min text.Length (int node.EndPos.Index)
    if name = "" || spanStart < 0 || spanStart >= spanEnd then
        None
    else
        match findWholeWord (text.Substring(spanStart, spanEnd - spanStart)) name with
        | None -> None
        | Some offset ->
            let startIndex = spanStart + offset
            Some(Range(Start = toPosition text startIndex, End = toPosition text (startIndex + name.Length)))

/// <summary>
/// Returns the file path and the range of the identifier at the cursor, as shown by <c>textDocument/prepareRename</c>.
/// </summary>
/// <param name="cursorNode">Innermost symbol-table node containing the cursor.</param>
/// <returns>
/// <c>Result.Ok</c> with the file path and the identifier range, or <c>Result.Error</c> with a message for the client.
/// </returns>
/// <remarks>
/// The range is taken from the cursor node when it has its own identifier, so the client highlights the text under
/// the cursor. A justification has no identifier of its own. For it, the range of its argument is returned, which may
/// lie in another file. The handler rejects such cases.
/// </remarks>
let prepareRenameAt (cursorNode: FplGenericNode) : Result<string * Range, string> =
    match declarationOf cursorNode with
    | Result.Error message -> Result.Error message
    | Result.Ok _ ->
        let ownRange =
            if cursorNode.FplId = "" then
                None
            else
                identifierRange cursorNode (File.ReadAllText cursorNode.FilePath)
        match ownRange with
        | Some range -> Result.Ok(cursorNode.FilePath, range)
        | None ->
            let target = cursorTargetNode cursorNode
            match identifierRange target (File.ReadAllText target.FilePath) with
            | Some range -> Result.Ok(target.FilePath, range)
            | None -> Result.Error "The identifier range cannot be determined."

/// <summary>
/// Computes the edits that rename the declaration under the cursor and all nodes that refer to it.
/// </summary>
/// <param name="cursorNode">Innermost symbol-table node containing the cursor.</param>
/// <param name="nodes">All flattened symbol-table nodes of the current evaluation, across all files.</param>
/// <param name="newName">The proposed new name.</param>
/// <returns>
/// <c>Result.Ok</c> with one <c>(filePath, range, newText)</c> entry per identifier occurrence, or <c>Result.Error</c>
/// with a message for the client.
/// </returns>
/// <remarks>
/// The reference set is the one of <c>findReferenceNodes</c>. Nodes with an empty <c>FplId</c> are placeholders and
/// are excluded. Each edit replaces the node's own identifier, not the new name's length. Duplicate ranges are removed.
/// Rejects the rename if any identifier cannot be located in its source text, since a partial rename would leave the
/// code inconsistent.
/// </remarks>
let renameEdits (cursorNode: FplGenericNode) (nodes: seq<FplGenericNode>) (newName: string)
    : Result<(string * Range * string) list, string> =
    match declarationOf cursorNode with
    | Result.Error message -> Result.Error message
    | Result.Ok declaration ->
        match validateNewName declaration newName with
        | Result.Error message -> Result.Error message
        | Result.Ok() ->
            let texts = Dictionary<string, string>()
            let readText (path: string) : string =
                match texts.TryGetValue path with
                | true, text -> text
                | _ ->
                    let text = File.ReadAllText path
                    texts[path] <- text
                    text

            let references =
                findReferenceNodes cursorNode nodes
                |> List.filter (fun node -> node.FplId <> "")

            let located =
                references
                |> List.map (fun node -> node, identifierRange node (readText node.FilePath))

            match located |> List.tryFind (fun (_, range) -> range.IsNone) with
            | Some(node, _) ->
                Result.Error $"Cannot locate the identifier '{node.FplId}' in '{node.FilePath}'."
            | None ->
                let edits =
                    located
                    |> List.choose (fun (node, range) -> range |> Option.map (fun r -> node.FilePath, r, newName))
                    |> List.distinctBy (fun (path, r, _) ->
                        path, r.Start.Line, r.Start.Character, r.End.Line, r.End.Character)
                Result.Ok edits

