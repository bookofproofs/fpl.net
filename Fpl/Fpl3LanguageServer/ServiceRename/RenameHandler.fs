/// <summary>
/// Language server handler for the textDocument/prepareRename and textDocument/rename requests of FPL files.
/// </summary>
/// <remarks>
/// The reference set is computed by <c>findReferenceNodes</c>, the same function used by find-references.
/// The first version renames variables and axioms only. Other kinds are rejected by <c>isRenameableKind</c>.
/// </remarks>
module Fpl3LanguageServer.ServiceRename.RenameHandler

open System
open System.Text.RegularExpressions
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open OmniSharp.Extensions.LanguageServer.Protocol
open Fpl0Base.Primitives
open Fpl2Interpreter.BasicTypes
open Fpl2Interpreter.SymbolTable.Types2.Variables
open Fpl2Interpreter.SymbolTable.Types3.PredicativeBlocks
open Fpl2Interpreter.SymbolTable.GoToDefinition

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
/// Variables use <c>PrimFplLowerCase</c>. Axioms use <c>PrimFplPascalCase</c>. Both patterns are anchored
/// so that the whole name must match.
/// </remarks>
let private isValidIdentifierFormat (declaration: FplGenericNode) (newName: string) : bool =
    let pattern =
        match declaration with
        | :? FplVariable -> PrimFplLowerCase
        | _ -> PrimFplPascalCase
    newName <> "" && Regex.IsMatch(newName, "^" + pattern + "$", RegexOptions.CultureInvariant)

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
/// <returns><c>true</c> if the format is valid and the name does not collide with the declaration's scope.</returns>
/// <remarks>
/// The collision check looks only at the <c>Scope</c> of the ultimate block node of the declaration.
/// </remarks>
let private isValidNewName (declaration: FplGenericNode) (newName: string) : bool =
    let collisionFree =
        match declaration.UltimateBlockNode with
        | Some ultimate -> not (ultimate.Scope.ContainsKey newName)
        | None -> true
    isValidIdentifierFormat declaration newName && collisionFree
