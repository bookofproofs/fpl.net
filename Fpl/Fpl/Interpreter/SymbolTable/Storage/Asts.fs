/// <summary>
/// This module contains types necessary to pre-process AST from the FPL Parser
/// before it can be further analysed by the FPL interpreter
/// </summary>
(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)

module Fpl.Interpreter.SymbolTable.Storage.Asts

open System
open System.Text.RegularExpressions
open System.Collections.Generic
open System.IO
open System.Text
open FParsec
open Fpl.Parser.Types
open Fpl.Parser.Main
open Fpl.Errors.Diagnostics
open Newtonsoft.Json

/// <summary>
/// Serializes a two-element key/value list into a compact JSON object.
/// </summary>
/// <param name="keyValueList">A list containing exactly two strings: [key; value].</param>
/// <returns>A JSON string representing the single key/value pair.</returns>
/// <exceptions>
/// <exception cref="System.Exception">Thrown when the list does not contain exactly two elements.</exception>
/// </exceptions>
let toJson (keyValueList: string list) =
    match keyValueList with
    | [key; value] ->
        let dict = dict [ (key, value) ]
        JsonConvert.SerializeObject(dict, Formatting.None)
    | _ -> failwith "List must contain exactly two elements"

/// <summary>
/// Represents an evaluated alias found in a uses clause: start/end positions and the alias or '*' marker.
/// </summary>
type EvalAlias =
    { StartPos: Position
      EndPos: Position
      AliasOrStar: string }

/// <summary>
/// Stores information produced when evaluating a uses clause (evaluated aliased namespace identifier).
/// </summary>
type EvalAliasedNamespaceIdentifier =
    { StartPos: Position
      EndPos: Position
      EvalAlias: EvalAlias
      PascalCaseIdList: string list 
      DebugMode: bool}

    /// <summary>
    /// Creates an EvalAliasedNamespaceIdentifier from a list of pascal-case identifiers and an EvalAlias.
    /// </summary>
    /// <param name="pascalCaseIdList">List of pascal-case identifiers.</param>
    /// <param name="evalAlias">An <see cref="EvalAlias"/> value.</param>
    /// <param name="startPos">Start position in the source.</param>
    /// <param name="endPos">End position in the source.</param>
    /// <param name="debugMode">Whether debug mode is enabled for this EANI.</param>
    /// <returns>A new <see cref="EvalAliasedNamespaceIdentifier"/> instance.</returns>
    static member CreateEani(pascalCaseId: string, aliasOrStar: string, startPos, endPos, debugMode) =
        let evalAlias =
            { EvalAlias.StartPos = startPos
              EvalAlias.EndPos = endPos
              EvalAlias.AliasOrStar = aliasOrStar }

        { EvalAliasedNamespaceIdentifier.StartPos = startPos
          EvalAliasedNamespaceIdentifier.EndPos = endPos
          EvalAliasedNamespaceIdentifier.EvalAlias = evalAlias
          EvalAliasedNamespaceIdentifier.PascalCaseIdList = [ pascalCaseId ] 
          EvalAliasedNamespaceIdentifier.DebugMode = debugMode}

    /// <summary>
    /// Creates an EvalAliasedNamespaceIdentifier from a PathEquivalentUri (uses the theory name).
    /// </summary>
    /// <param name="uri">The source URI of the theory.</param>
    /// <param name="debugMode">Whether debug mode is enabled for this EANI.</param>
    /// <returns>A new <see cref="EvalAliasedNamespaceIdentifier"/> instance.</returns>
    static member CreateEani(pascalCaseIdList: string list, evalAlias: EvalAlias, startPos, endPos, debugMode) =
        { EvalAliasedNamespaceIdentifier.StartPos = startPos
          EvalAliasedNamespaceIdentifier.EndPos = endPos
          EvalAliasedNamespaceIdentifier.EvalAlias = evalAlias
          EvalAliasedNamespaceIdentifier.PascalCaseIdList = pascalCaseIdList 
          EvalAliasedNamespaceIdentifier.DebugMode = debugMode}

    /// Creates an EvalAliasedNamespaceIdentifier with a given Uri.
    static member CreateEani(uri: PathEquivalentUri, debugMode) =
        let pascalCaseId = uri.TheoryName
        let pos = Position("", 0, 1, 1)
        EvalAliasedNamespaceIdentifier.CreateEani(pascalCaseId, "*", pos, pos, debugMode)

    /// <summary>
    /// Pattern used to match filenames for this EANI (supports '*' wildcard when alias is '*').
    /// </summary>
    member this.FileNamePattern =
        let pascalCaseIdList = String.concat "." this.PascalCaseIdList

        match this.EvalAlias.AliasOrStar with
        | "*" -> sprintf "%s*.fpl" pascalCaseIdList
        | _ -> sprintf "%s.fpl" pascalCaseIdList

    /// <summary>
    /// The effective name for this EANI: either the alias, the concatenated identifiers or the wildcard namespace.
    /// </summary>
    member this.Name =
        let concatenatedPascalCaseIds = String.concat "." this.PascalCaseIdList

        match this.EvalAlias.AliasOrStar with
        | "*" -> concatenatedPascalCaseIds
        | _ when this.EvalAlias.AliasOrStar <> "*" && this.EvalAlias.AliasOrStar <> "" -> this.EvalAlias.AliasOrStar
        | _ -> concatenatedPascalCaseIds

/// <summary>
/// Status of a parsed AST during the preprocessing/evaluation pipeline.
/// </summary>
type ParsedAstStatus =
    | Loaded
    | UsesClausesEvaluated
    | Evaluated

/// <summary>
/// Sorting related metadata used to topologically order parsed ASTs and to track uses-clause relationships.
/// </summary>
type SortingProperties =
    { mutable TopologicalSorting: int // an order in which the ParsedAsts have to be interpreted to avoid undeclared identifiers (undefined if a circle was caused by uses clauses)
      mutable ReferencingAsts: string list // list of asts "referencing" this one with a uses clause
      mutable ReferencedAsts: string list // list of asts "referenced" by this one in a uses clause
      mutable EANIList: EvalAliasedNamespaceIdentifier list } // evaluated uses clauses found in the Ast

    /// <summary>
    /// Reset sorting properties to their initial state.
    /// </summary>
    member this.Reset() =
        this.TopologicalSorting <- 0
        this.ReferencingAsts <- []
        this.ReferencedAsts <- []
        this.EANIList <- []

    /// <summary>
    /// Factory creating a fresh <see cref="SortingProperties"/> instance with default values.
    /// </summary>
    static member Create() =
        { SortingProperties.TopologicalSorting = 0
          SortingProperties.ReferencingAsts = []
          SortingProperties.ReferencedAsts = []
          SortingProperties.EANIList = [] }

/// <summary>
/// Encapsulates discovered sources for a uses clause and provides utilities to filter and prefer local sources.
/// </summary>
/// <param name="paths">List of candidate <see cref="PathEquivalentUri"/> values found for a uses clause.</param>
/// <param name="pathToLocalRegistry">Path to the local registry used when preferring local copies.</param>
type FplSources(paths: PathEquivalentUri list, pathToLocalRegistry: string) =
    let _pathToLocalRegistry = pathToLocalRegistry
    /// All found paths for a uses clause, including those from the web.
    member this.Paths = paths

    /// <summary>
    /// Path to the local registry directory used to prefer local copies over remote (Internet) registry originals.
    /// </summary>
    member this.PathToLocalRegistry = pathToLocalRegistry

    /// <summary>
    /// Groups found FPL theories by filename returning the grouped data structure for further processing.
    /// </summary>
    /// <returns>
    /// A list of tuples where the first element is the filename and the second element is the list of
    /// tuples (filename, PathEquivalentUri) that produced that group.
    /// </returns>
    member this.Grouped =
        let fplTheories =
            this.Paths |> List.map (fun fp -> (Path.GetFileName fp.AbsolutePath, fp))

        fplTheories |> List.groupBy fst

    /// <summary>
    /// Returns true if the given URI appears to be an HTTP/HTTPS URL.
    /// </summary>
    /// <param name="uri">The URI to inspect.</param>
    /// <returns>True when the URI is a web URL.</returns>
    static member IsUrl(uri: PathEquivalentUri) =
        let pattern = "^https?:\/\/"
        Regex.IsMatch(uri.AbsoluteUri, pattern)

    /// <summary>
    /// Returns true if the given URI appears to be a local file path.
    /// </summary>
    /// <param name="uri">The URI to inspect.</param>
    /// <returns>True when the URI is a local file path.</returns>
    static member IsFilePath(uri: PathEquivalentUri) =
        try
            Path.GetFullPath(uri.AbsoluteUri) |> ignore
            let pattern = "^https?:\/\/"
            not (Regex.IsMatch(uri.AbsoluteUri, pattern))
        with :? ArgumentException ->
            false

    /// <summary>
    /// Returns only the web URLs from the collected paths.
    /// </summary>
    member this.Urls = List.filter FplSources.IsUrl this.Paths

    /// <summary>
    /// Returns only the file paths from the collected paths.
    /// </summary>
    member this.FilePaths = List.filter FplSources.IsFilePath this.Paths

    /// <summary>
    /// Number of collected candidate paths.
    /// </summary>
    member this.Length = this.Paths.Length

    /// <summary>
    /// Returns grouped candidates with an explicitly chosen/preferred source for each theory.
    /// </summary>
    /// <returns>
    /// A list of tuples: (fileName, chosenPath, chosenPathType, allPathTypes, theoryName).
    /// </returns>
    member this.GroupedWithPreferedSource =
        let result =
            let grouped = this.Grouped

            grouped
            |> List.collect (fun (fileName, paths) ->
                let pathType =
                    paths
                    |> List.map snd
                    |> List.tryFind (fun path ->
                        if
                            FplSources.IsFilePath(path)
                            && not (path.AbsolutePath.Contains("/lib/") || path.AbsolutePath.Contains(@"\lib\"))
                        then
                            true // the first source is the current directory
                        elif
                            FplSources.IsFilePath(path)
                            && (path.AbsolutePath.Contains("/lib/") || path.AbsolutePath.Contains(@"\lib\"))
                        then
                            true // the second is the lib subdirectory
                        else
                            true // the third is the Internet source
                    )

                let pathTypes =
                    List.map snd paths
                    |> List.map (fun path ->
                        if
                            FplSources.IsFilePath(path)
                            && (path.AbsolutePath.Contains("/lib/") || path.AbsolutePath.Contains(@"\lib\"))
                        then
                            "./lib"
                        elif FplSources.IsFilePath(path) then
                            "./"
                        else
                            "https")

                let theoryName = Path.GetFileNameWithoutExtension(fileName)

                match pathType with
                | Some path ->
                    let chosenPathType =
                        if
                            FplSources.IsFilePath(path)
                            && not (path.AbsolutePath.Contains("/lib/") || path.AbsolutePath.Contains(@"\lib\"))
                        then
                            "./"
                        elif
                            FplSources.IsFilePath(path)
                            && (path.AbsolutePath.Contains("/lib/") || path.AbsolutePath.Contains(@"\lib\"))
                        then
                            "./lib"
                        else
                            "https"

                    [ (fileName, path, chosenPathType, pathTypes, theoryName) ]
                | None -> [])

        result

    /// <summary>
    /// Tests whether a filename matches a wildcard pattern (supports '*' and '?').
    /// </summary>
    /// <param name="fileName">Filename to test.</param>
    /// <param name="pattern">Wildcard pattern.</param>
    /// <returns>True when the filename matches the pattern.</returns>
    static member HasPattern(fileName: string, pattern) =
        let wildcardToRegex (wildcard: string) =
            "^" + Regex.Escape(wildcard).Replace("\\*", ".*").Replace("\\?", ".") + "$"

        let regexPattern = wildcardToRegex pattern
        let regex = Regex(regexPattern, RegexOptions.IgnoreCase)
        regex.IsMatch(fileName)

    /// <summary>
    /// Finds all grouped entries whose filename matches the provided pattern.
    /// </summary>
    /// <param name="pattern">Wildcard pattern to match against filenames.</param>
    /// <returns>Filtered list with the same tuple shape as <see cref="GroupedWithPreferedSource"/>.</returns>
    member this.FindWithPattern(pattern: string) =
        this.GroupedWithPreferedSource
        |> List.filter (fun (fileName, _, _, _, _) -> FplSources.HasPattern(fileName, pattern))

/// <summary>
/// Parsing properties attached to a parsed AST: source URI, original source code, parsed building-block ASTs and checksum.
/// </summary>
type ParsingProperties =
    { mutable Uri: PathEquivalentUri // source of the ast
      mutable FplSourceCode: string // source code of the ast
      mutable BuildingBlockAsts: Ast list // parsed asts of all building blocks
      mutable Checksum: string } // checksum of the parsed ast

    /// <summary>
    /// Reset this ParsingProperties with new source code and URI; reparses when checksum differs.
    /// </summary>
    /// <param name="fplCode">Source code to parse.</param>
    /// <param name="uri">Source URI of the code.</param>
    /// <returns>
    /// True when the parsing produced updated content (checksum changed), false when unchanged.
    /// </returns>
    member this.Reset (fplCode: string) (uri: PathEquivalentUri) =
        let checksum = computeMD5Checksum fplCode

        if this.Checksum <> checksum then
            // if there is a Parsed Ast with the same Name as the eani.Name
            // and its checksum differs from the previous checksum
            // then replace the ast, checksum, location, source code, the
            this.Uri <- uri
            diagnosticsContainer.ResetStream(uri)
            let buildingBlocks, success = fplParser fplCode
            this.BuildingBlockAsts <- buildingBlocks
            this.FplSourceCode <- fplCode
            this.Checksum <- checksum
            true
        else
            false

    /// <summary>
    /// Create a new ParsingProperties instance by parsing <c>fplCode</c> for the given <c>uri</c>.
    /// </summary>
    /// <param name="fplCode">Source code to parse.</param>
    /// <param name="uri">Source URI of the code.</param>
    /// <returns>A fresh <see cref="ParsingProperties"/> instance containing parsed ASTs and a checksum.</returns>
    static member Create (fplCode: string) (uri: PathEquivalentUri) =
        diagnosticsContainer.ResetStream(uri)
        let buildingBlockAsts, success = fplParser fplCode
        { ParsingProperties.Uri = uri
          ParsingProperties.FplSourceCode = fplCode
          ParsingProperties.BuildingBlockAsts = buildingBlockAsts
          ParsingProperties.Checksum = computeMD5Checksum fplCode }

/// <summary>
/// Holds mapping counters for building block identifiers within a parsed AST.
/// </summary>
type FplBlockProperties =
    { FplBlockIds: Dictionary<string, int> }

    member this.Reset() = this.FplBlockIds.Clear()

/// <summary>
/// Container for all data computed for a parsed FPL namespace/source (checksum, parsed blocks, sorting info, etc.).
/// </summary>
type ParsedAst =
    { Id: string // id of this ast giving the order in which it was parsed with other asts
      Parsing: ParsingProperties
      Sorting: SortingProperties
      FplBlocks: FplBlockProperties
      mutable Status: ParsedAstStatus }

/// <summary>
/// Reference type storing a mutable list of <see cref="ParsedAst"/> with convenience lookup and reporting helpers.
/// </summary>
type ParsedAstList() =
    inherit System.Collections.Generic.List<ParsedAst>()
    let this = List<ParsedAst>()

    /// <summary>
    /// Attempts to find any ParsedAst in the list that has status <c>Loaded</c>.
    /// </summary>
    /// <returns>Some(<see cref="ParsedAst"/>) if a loaded AST exists, otherwise None.</returns>
    member this.TryFindAstById(identifier: string) =
        if this.Exists(fun pa -> pa.Id = identifier) then
            Some(this.Find(fun pa -> pa.Id = identifier))
        else
            None

    /// Finds some loaded ParsedAst. Returns None if none was found.
    member this.TryFindLoadedAst() =
        if this.Exists(fun pa -> pa.Status = ParsedAstStatus.Loaded) then
            Some(this.Find(fun pa -> pa.Status = ParsedAstStatus.Loaded))
        else
            None

    /// <summary>
    /// Builds a dictionary mapping each parsed AST URI to its source code.
    /// </summary>
    /// <returns>A <see cref="System.Collections.Generic.Dictionary{PathEquivalentUri,string}"/> of sources.</returns>
    member this.DictionaryOfSUri2FplSourceCode() =
        let ret = System.Collections.Generic.Dictionary<PathEquivalentUri, string>()

        this
        |> Seq.iter (fun pa -> ret.TryAdd(pa.Parsing.Uri, pa.Parsing.FplSourceCode) |> ignore)

        ret

    /// <summary>
    /// Returns a combined string representation of all parsed ASTs in the list (debugging only)
    /// </summary>
    member this.AstsToString =
        let res =
            this
            |> Seq.map (fun pa -> pa.Parsing.BuildingBlockAsts)
            |> Seq.map (fun ast ->ast.ToString())
            |> String.concat Environment.NewLine
        res

    /// <summary>
    /// Orders stored ASTs according to computed topological sorting (descending).
    /// </summary>
    member this.OrderAsts() =
        this.Sort(
            Comparer<ParsedAst>.Create(fun b a -> compare a.Sorting.TopologicalSorting b.Sorting.TopologicalSorting)
        )

    /// <summary>
    /// Creates a diagnostic trace summary for debugging (language server logging use only).
    /// </summary>
    member this.TraceStatistics =
        let sb = StringBuilder()

        this
        |> Seq.iter (fun pa ->
            let paDiagnostics = diagnosticsContainer.GetStreamDiagnostics(pa.Parsing.Uri)

            let statsDiags =
                paDiagnostics.Values
                |> Seq.groupBy (fun d -> $"{d.Emitter}({d.Code.Code})")
                |> Seq.map (fun (groupId, group) -> $"{groupId}:{Seq.length group}")
                |> String.concat ", "

            sb.AppendLine $"{pa.Id}(chksm {pa.Parsing.Checksum}): #total diags {paDiagnostics.Count}, {statsDiags}"
            |> ignore)

        sb.ToString()

    /// <summary>
    /// Appends an enriched textual view of uses-dependencies into the provided StringBuilder (debugging helper).
    /// </summary>
    /// <param name="sb">StringBuilder to append enriched dependency lines to.</param>
    member this.EnrichDependencies (sb: StringBuilder) =
        this
        |> Seq.map (fun pa ->
            $"[{pa.Id}, {pa.Sorting.TopologicalSorting}, {pa.Sorting.ReferencedAsts}, {pa.Sorting.ReferencingAsts}]")
        |> String.concat Environment.NewLine
        |> sb.AppendLine
        |> ignore
