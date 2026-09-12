(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)

/// <summary>
/// Utilities to evaluate and resolve FPL <c>uses</c> clauses, download or load referenced sources,
/// update the parsed AST registry and emit diagnostics for ambiguous or invalid references.
/// </summary>
/// <remarks>
/// This module implements the bulk of self-containment and dependency resolution for FPL sources:
/// it evaluates ASTs to extract aliased namespace identifiers, finds matching source files locally or
/// remotely, populates the parsed AST list and checks for cycles in the uses graph.
/// </remarks>

module Fpl.Interpreter.SymbolTable.Creation.UsesClauses
open System.Text.RegularExpressions
open System.Net.Http
open System.IO
open System.Collections.Generic
open System
open FParsec
open Fpl.Parser.Types
open Fpl.Errors.Emitter
open Fpl.Interpreter.Helpers.Debug
open Fpl.Interpreter.SymbolTable.Storage.Heap
open Fpl.Interpreter.SymbolTable.Storage.Asts
open Fpl.Errors.Diagnostics

/// <summary>
/// Recursively evaluate an AST tree and collect <c>EvalAliasedNamespaceIdentifier</c> records
/// representing every <c>uses</c> clause occurrence.
/// </summary>
/// <param name="debugMode">When true, remote downloads are avoided to support offline debugging.</param>
/// <param name="ast">The AST node to evaluate.</param>
/// <returns>List of <c>EvalAliasedNamespaceIdentifier</c> records extracted from the AST node.</returns>
let rec eval_uses_clause debugMode = function 
    | Ast.AST ((pos1, pos2), ast) -> 
        eval_uses_clause debugMode ast
    | Ast.Namespace (buildingBlockAsts) ->
        let results = buildingBlockAsts |> List.collect (eval_uses_clause debugMode)
        results
    | Ast.BuildingBlock((_, _),buidlingBlockAst) ->
        eval_uses_clause debugMode buidlingBlockAst
    | Ast.UsesClause ((pos1, pos2), ast) -> 
        eval_uses_clause debugMode ast 
    | Ast.AliasedNamespaceIdentifier ((pos1, pos2), (ast, optAst)) -> 
        let evalAlias = match optAst with
                        | Some (Ast.Alias ((p1, p2), s)) ->
                            { 
                                StartPos = p1
                                EndPos = p2
                                AliasOrStar = s
                            }
                        | Some (Ast.Star ((p1,p2),())) -> 
                            { 
                                StartPos = p1
                                EndPos = p2
                                AliasOrStar = "*"
                            }

                        | _ -> 
                            { 
                                StartPos = pos1
                                EndPos = pos2
                                AliasOrStar = ""
                            }

        match ast with
        | Ast.NamespaceIdentifier ((p1, p2), asts) -> 
            let pascalCaseIdList =
                asts
                |> List.collect (
                    function Ast.PascalCaseId ((p_1, p_2),s)
                            -> [s]
                            | _ -> []
                )

            [EvalAliasedNamespaceIdentifier.CreateEani(pascalCaseIdList, evalAlias, p1, p2, debugMode)]
        | _ -> []
    | _ -> []

/// <summary>
/// Download the content at the specified URL as a string.
/// </summary>
/// <param name="url">The HTTP(S) URL to download.</param>
/// <param name="e">Contextual evaluation record used for diagnostics and debug mode.</param>
/// <returns>
/// The downloaded content as a string on success; the empty string on failure or when debug mode is enabled.
/// </returns>
/// <remarks>
/// Uses <see cref="HttpClient"/> and performs synchronous blocking of the async task for simplicity.
/// Emits NSP02 diagnostics on HTTP failures or exceptions.
/// </remarks>
let downloadFile url (e:EvalAliasedNamespaceIdentifier) =
    if not (e.DebugMode) then
        use client = new HttpClient()
        try
            let response =
                client.GetAsync(PathEquivalentUri(url))
                |> Async.AwaitTask
                |> Async.RunSynchronously

            if not response.IsSuccessStatusCode then
                let reason = sprintf "%d %s" (int response.StatusCode) response.ReasonPhrase
                emitNSP02Diagnostics url reason e.StartPos e.EndPos
                ""
            else
                response.Content.ReadAsStringAsync()
                |> Async.AwaitTask
                |> Async.RunSynchronously
        with
        | ex ->
            emitNSP02Diagnostics url ex.Message e.StartPos e.EndPos
            ""
    else
        ""

/// <summary>
/// Load the contents of a local file into a string.
/// </summary>
/// <param name="filename">Absolute path to the file to read.</param>
/// <param name="e">Contextual evaluation record used for diagnostics.</param>
/// <returns>The file content as a string, or empty string if reading fails.</returns>
/// <exceptions>
/// <exception>
/// <paramref name="filename"/> may not exist or be inaccessible; such exceptions are caught and translated
/// into NSP01 diagnostics, and the function returns an empty string.
/// </exception>
/// </exceptions>
let loadFile filename (e:EvalAliasedNamespaceIdentifier) =
    try
        File.ReadAllText filename
    with
    | ex ->
        emitNSP01Diagnostics filename ex.Message e.StartPos e.EndPos
        ""

/// <summary>
/// Create or locate the requested subfolder relative to a provided URI and return the
/// parent directory and the created subdirectory path.
/// </summary>
/// <param name="uri">A <c>PathEquivalentUri</c> representing the current file or location.</param>
/// <param name="subFolder">Name of the subfolder to locate or create (for example "lib" or "repo").</param>
/// <returns>
/// Tuple of (<c>directoryPath</c>, <c>subDirectoryPath</c>), where <c>directoryPath</c> is the directory
/// containing the source and <c>subDirectoryPath</c> is the requested subfolder path.
/// </returns>
/// <remarks>
/// Handles Windows drive-root ambiguity by unescaping and stripping a leading separator when necessary.
/// If the source directory itself is already named "lib" or "repo", the function returns the parent directory
/// and the sibling subfolder path instead.
/// </remarks>
let createSubfolder (uri: PathEquivalentUri) subFolder =
    let unescapedPath = 
        let p = Uri.UnescapeDataString(uri.LocalPath)
        let pattern = @"^[\/\\][a-zA-Z]:"
        if Regex.IsMatch(p, pattern) then
            p.Substring(1)
        else
            p
    let directoryPath = Path.GetDirectoryName(unescapedPath)
    let directoryName = Path.GetFileName(directoryPath)
    if directoryName = "lib" || directoryName = "repo" then
        let subDirectoryPath = Path.Combine(Path.GetDirectoryName(directoryPath), subFolder)
        (Path.GetDirectoryName(directoryPath), subDirectoryPath)
    else
        let subDirectoryPath = Path.Combine(directoryPath, subFolder)
        if not <| Directory.Exists(subDirectoryPath) then
            Directory.CreateDirectory(subDirectoryPath) |> ignore
        (directoryPath, subDirectoryPath)

/// <summary>
/// Download the library map (libmap.txt) from the web repository.
/// </summary>
/// <param name="uri">A <c>PathEquivalentUri</c> representing the current file or location.</param>
/// <param name="currentWebRepo">Base URL of the web repository (no trailing slash expected).</param>
/// <param name="debugMode">Pass-through to control download behaviour for diagnostics.</param>
/// <returns>The raw content of <c>libmap.txt</c> or an empty string on failure.</returns>
let downloadLibMap (uri:PathEquivalentUri) (currentWebRepo: string) debugMode =
    let pos = Position("", 0, 1, 1)
    let libMap = downloadFile (currentWebRepo + "/libmap.txt") (EvalAliasedNamespaceIdentifier.CreateEani("","", pos, pos, debugMode))
    libMap    

/// <summary>
/// Acquire available FPL source URIs for resolution of a single uses clause.
/// </summary>
/// <param name="uri">The URI of the currently processed file.</param>
/// <param name="fplLibUrl">Base URL of the remote FPL lib repository.</param>
/// <param name="debugMode">If true, remote downloads are avoided where possible.</param>
/// <returns>
/// An <c>FplSources</c> record containing:
/// - A list of local and remote source URIs to consider
/// - A local registry directory path for caching downloaded files.
/// </returns>
/// <remarks>
/// Searches the current directory, the <c>lib</c> subfolder and the remote repository listed in libmap.txt.
/// Remote entries are represented as escaped URIs and cached under the <c>repo</c> subfolder when downloaded.
/// </remarks>
let acquireSources (uri: PathEquivalentUri) (fplLibUrl: string) debugMode =

    let (_,libDirectoryPath) = createSubfolder uri "lib"
    let (directoryPath,repoDirectoryPath) = createSubfolder uri "repo"


    let fileNamesInCurrDir = Directory.EnumerateFiles(directoryPath, "*.fpl") |> Seq.map (fun path -> PathEquivalentUri.EscapedUri(path)) |> Seq.toList
    let fileNamesInLibSubDir = Directory.EnumerateFiles(libDirectoryPath, "*.fpl") |> Seq.map (fun path -> PathEquivalentUri.EscapedUri(path)) |> Seq.toList
    let libMap = downloadLibMap uri fplLibUrl debugMode
    let filesToDownload = libMap.Split("\n") 
                           |> Seq.filter(fun s -> s<>"")
                           |> Seq.map (fun s -> PathEquivalentUri.EscapedUri(fplLibUrl + "/" + s))
                           |> Seq.toList
    FplSources(fileNamesInCurrDir @ fileNamesInLibSubDir @ filesToDownload, repoDirectoryPath)
    
/// <summary>
/// Add a new ParsedAst to the collection or update an existing one when the source content changed.
/// </summary>
/// <param name="fileContent">Source code content for the AST.</param>
/// <param name="uri">The source file URI used to derive the parsed AST identifier.</param>
/// <param name="parsedAsts">The global <c>ParsedAstList</c> repository to update.</param>
/// <returns>The <c>TheoryName</c> (identifier) associated with the added or updated ParsedAst.</returns>
/// <remarks>
/// If an existing parsed AST with the same Id is found, the function checks the parsing checksum via
/// <c>pa.Parsing.Reset</c> and updates only when necessary. Otherwise a new <c>ParsedAst</c> is created
/// with default sorting and block properties.
/// </remarks>
let private addOrUpdateParsedAst fileContent (uri:PathEquivalentUri) (parsedAsts:ParsedAstList) = 
    let name = uri.TheoryName
    let idAlreadyFound = parsedAsts.TryFindAstById name
    match idAlreadyFound with
    | Some pa -> 
        if pa.Parsing.Reset fileContent uri then
            // if there ist a Parsed Ast with the same Name as the eani.Name 
            // and its checksum differs from the previous checksum 
            // then replace the ast, checksum, location, source code, the 
            pa.Sorting.Reset()
            pa.FplBlocks.Reset()
            pa.Status <- ParsedAstStatus.Loaded

        else
            // if the checksum is the same, do not replace anything
            ()
    | None -> 
        // add a new ParsedAst
        let parsing = ParsingProperties.Create fileContent uri
        let sorting = SortingProperties.Create()
        let fplBlocks = {
            FplBlockProperties.FplBlockIds = Dictionary<string, int>()
        }
        let pa = { 
            ParsedAst.Id = name 
            ParsedAst.Parsing = parsing
            ParsedAst.Sorting = sorting
            ParsedAst.FplBlocks = fplBlocks
            ParsedAst.Status = ParsedAstStatus.Loaded
        }
        parsedAsts.Add(pa)
    name

/// <summary>
/// Detect duplicate alias names among a list of <c>EvalAliasedNamespaceIdentifier</c> entries and emit diagnostics.
/// </summary>
/// <param name="eaniList">List of aliased namespace identifiers to inspect.</param>
/// <remarks>
/// Aliases that are "*" or empty are ignored. When the same alias is used multiple times,
/// this function emits NSP03 diagnostics referencing the duplicate alias and its positions.
/// </remarks>
let private findDuplicateAliases (eaniList: EvalAliasedNamespaceIdentifier list) =
    let uniqueAliases = HashSet<string>()
    eaniList
    |> List.map (fun eani -> eani.EvalAlias)
    // filter out the identifiers with AliasOrStar equal to None or Some "*"
    |> List.filter (fun alias -> alias.AliasOrStar <> "*" && alias.AliasOrStar <> "")
    |> List.map (fun alias ->
        if uniqueAliases.Contains alias.AliasOrStar then
            emitNSP03Diagnostics alias.AliasOrStar alias.StartPos alias.EndPos
        else    
            uniqueAliases.Add(alias.AliasOrStar) |> ignore
    )

/// <summary>
/// Emit diagnostics when the same theory name can be found in multiple source locations.
/// </summary>
/// <param name="availableSources">The <c>FplSources</c> collection to query.</param>
/// <param name="eani">The evaluated aliased namespace identifier used to derive the filename pattern.</param>
/// <remarks>
/// For each match of the requested pattern the function inspects the returned path types and will emit
/// NSP05 diagnostics if multiple path types exist for the same theory.
/// </remarks>
let private emitDiagnosticsForDuplicateFiles (availableSources:FplSources) (eani:EvalAliasedNamespaceIdentifier) =
    availableSources.FindWithPattern eani.FileNamePattern
    |> List.iter (fun (_, _, chosenPathType, pathTypes, theoryName) ->
        if pathTypes.Length > 1 then
            emitNSP05Diagnostics pathTypes theoryName chosenPathType eani.StartPos eani.EndPos
    )
    |> ignore

/// <summary>
/// Update sorting relationships between a parsed AST and a referenced parsed AST.
/// </summary>
/// <param name="alreadyLoaded">The global parsed AST registry.</param>
/// <param name="parsedAst">The parsed AST that references <paramref name="eaniName"/>.</param>
/// <param name="eaniName">The identifier of the referenced parsed AST.</param>
/// <remarks>
/// Ensures that <c>parsedAst</c> lists <c>eaniName</c> among its referenced ASTs and
/// that the referenced AST lists <c>parsedAst.Id</c> among its referencing ASTs.
/// </remarks>
let private chainParsedAsts (alreadyLoaded:ParsedAstList) parsedAst (eaniName:string) = 
    // complement referenced asts 
    if not (List.contains eaniName parsedAst.Sorting.ReferencedAsts) then 
        parsedAst.Sorting.ReferencedAsts <- parsedAst.Sorting.ReferencedAsts @ [eaniName]
    // complement referencing asts in the specific parsedAst even if it was already loaded
    let referencedPa = alreadyLoaded.TryFindAstById(eaniName)
    match referencedPa with
    | Some pa -> 
        if not (List.contains parsedAst.Id pa.Sorting.ReferencingAsts) then 
            pa.Sorting.ReferencingAsts <- pa.Sorting.ReferencingAsts @ [parsedAst.Id]
    | None -> ()

/// <summary>
/// For a given <c>EvalAliasedNamespaceIdentifier</c>, find matching parsed ASTs from available sources,
/// load or download their contents, add or update parsed ASTs and chain references.
/// </summary>
/// <param name="sources">The <c>FplSources</c> repository that can locate candidates by filename pattern.</param>
/// <param name="parsedAsts">Global parsed AST registry to add/update entries.</param>
/// <param name="eani">The evaluated aliased namespace identifier to resolve.</param>
/// <param name="currenParsedAst">The currently processed parsed AST that references <paramref name="eani"/>.</param>
/// <remarks>
/// If a matching source is remote it will be downloaded and cached into the local repo folder.
/// Diagnostics NSP00 is emitted when no files match the pattern.
/// </remarks>
let getParsedAstsMatchingAliasedNamespaceIdentifier (sources:FplSources) (parsedAsts:ParsedAstList) (eani:EvalAliasedNamespaceIdentifier) (currenParsedAst: ParsedAst)=
    let filtered = sources.FindWithPattern eani.FileNamePattern
    if filtered.IsEmpty then
        // Emits diagnostics if there are no files for the pattern
        emitNSP00Diagnostics eani.FileNamePattern eani.StartPos eani.EndPos
    else
        filtered
        |> Seq.map (fun (_, uri, _, _, theoryName) ->
            if FplSources.IsFilePath(uri) then
                // load or download the content of every source
                let fileContent = loadFile uri.AbsolutePath eani
                addOrUpdateParsedAst fileContent uri parsedAsts |> ignore
            else
                let fileContent = downloadFile uri.AbsoluteUri eani
                let pathToLocalRegistryCopy = Path.Combine(sources.PathToLocalRegistry,Path.GetFileName(uri.AbsolutePath))
                if File.Exists(pathToLocalRegistryCopy) then
                    File.SetAttributes(pathToLocalRegistryCopy, FileAttributes.Normal)
                    File.Delete(pathToLocalRegistryCopy)
                File.WriteAllText(pathToLocalRegistryCopy, fileContent)
                File.SetAttributes(pathToLocalRegistryCopy, File.GetAttributes(pathToLocalRegistryCopy) ||| FileAttributes.ReadOnly)

                let escapedUri = PathEquivalentUri.EscapedUri(pathToLocalRegistryCopy)
                addOrUpdateParsedAst fileContent escapedUri parsedAsts |> ignore
          
            theoryName
        ) 
        |> Seq.iter (fun theoryName ->
            match parsedAsts.TryFindAstById(theoryName) with
            | Some pa -> 
                chainParsedAsts parsedAsts currenParsedAst theoryName 
            | _ -> ()
        )
    
/// <summary>
/// Compute whether the directed graph of parsed AST dependencies contains a cycle.
/// </summary>
/// <param name="parsedAsts">Global parsed AST registry.</param>
/// <returns>
/// <c>true</c> if the graph contains a cycle; otherwise <c>false</c>. Note: the function mutates
/// <c>ParsedAst.Sorting.TopologicalSorting</c> for nodes that are processed.
/// </returns>
/// <remarks>
/// Implements Kahn's algorithm for topological sorting; returns true when a cycle prevents completion.
/// </remarks>
let private isCircular (parsedAsts:ParsedAstList) = 
    let l0 = Stack<ParsedAst>()
    let igrad = Dictionary<string,int>()
    parsedAsts |> Seq.iter (fun pa -> 
        igrad.Add(pa.Id, pa.Sorting.ReferencingAsts.Length)
        if pa.Sorting.ReferencingAsts.Length = 0 then l0.Push(pa)
    ) |> ignore
    let mutable i = -1
    let mutable hasCycle = false
    while not hasCycle && i < parsedAsts.Count do
        i <- i + 1 
        hasCycle <- l0.Count = 0 
        if not hasCycle then
            let v = l0.Pop()
            v.Sorting.TopologicalSorting <- i
            v.Sorting.ReferencedAsts |> List.iter (fun name -> 
                if igrad.ContainsKey(name) then 
                    igrad[name] <- igrad[name] - 1
                    if igrad[name] = 0 then
                        let paNew = parsedAsts.TryFindAstById(name)
                        match paNew with 
                        | Some pa -> 
                            l0.Push(pa)
                        | None -> ()
            )
    hasCycle

/// <summary>
/// Attempt to find a cycle in the provided parsed AST list and return the path that composes it.
/// </summary>
/// <param name="parsedAsts">List of parsed ASTs to inspect for cycles.</param>
/// <returns>
/// <c>Some</c> list of theory identifiers representing a cycle path if a cycle is found; otherwise <c>None</c>.
/// </returns>
let private findCycle (parsedAsts:List<ParsedAst>) =  
    let rec dfs visited path node =
        if List.contains node.Id path then
            Some (path)
        elif Set.contains node.Id visited then
            None
        else
            let visited = Set.add node.Id visited
            let path = node.Id :: path
            parsedAsts
            |> Seq.toList
            |> List.choose (fun x -> if List.contains x.Id node.Sorting.ReferencingAsts then Some x else None)
            |> List.tryPick (dfs visited path)
    parsedAsts |> Seq.toList |> List.tryPick (dfs Set.empty [])

/// <summary>
/// Reorder a list so that the supplied element becomes the first element; the remainder keeps order circularly.
/// </summary>
/// <param name="element">Element which should become the head of the returned list.</param>
/// <param name="list">The source list to rearrange.</param>
/// <returns>The rearranged list starting at the first occurrence of <paramref name="element"/>.</returns>
let private rearrangeList element list =
    let afterElement = list |> List.skipWhile ((<>) element)
    let beforeElement = list |> List.takeWhile ((<>) element)
    afterElement @ beforeElement

/// <summary>
/// Garbage-collect parsed AST entries and symbol table entries that are no longer reachable from the main theory.
/// </summary>
/// <param name="uriToBeReset">The URI corresponding to the parsed AST that triggered the reset.</param>
/// <remarks>
/// Removes parsed ASTs that are not reachable from <c>heap.SymbolTable.MainTheory</c> and
/// resets status for the parsed AST that was re-evaluated. Diagnostics streams for removed
/// parsed ASTs are reset as part of the cleanup.
/// </remarks>
let garbageCollector (uriToBeReset:PathEquivalentUri) = 
    let referencedAstsOfCurrentTheory currTheory = 
        match heap.ParsedAsts.TryFindAstById(currTheory) with
        | Some pa -> pa.Sorting.ReferencedAsts
        | _ -> []

    // remove the current theory from the ReferencingAsts list of each parsedAst, if they are not contained 
    // in the current theory's reference Asts
    let rec removeNotReferencedAsts currTheory = 
        heap.ParsedAsts 
        |> Seq.iter (fun pa ->
            match pa.Sorting.ReferencingAsts |> List.tryFindIndex (fun referencedTheory -> 
                referencedTheory = currTheory
                && not (referencedAstsOfCurrentTheory currTheory |> List.contains pa.Id)
                ) with
            | Some indexOfCurrentTheory -> 
                pa.Sorting.ReferencingAsts <- pa.Sorting.ReferencingAsts |> List.removeAt(indexOfCurrentTheory)
            | _ -> ()
        )

    let rec findComponent (parsedAsts: ParsedAstList) (visited: Set<string>) (nodeId: string) =
        if Set.contains nodeId visited then visited
        else
            match parsedAsts.TryFindAstById nodeId with
            | Some node -> 
                let newVisited = Set.add nodeId visited
                List.fold (findComponent parsedAsts) newVisited node.Sorting.ReferencedAsts
            | None -> visited 

    match heap.ParsedAsts.TryFindAstById(heap.SymbolTable.MainTheory) with
    | Some mainTheory -> 
        removeNotReferencedAsts mainTheory.Id
        let astComponent = findComponent heap.ParsedAsts Set.empty mainTheory.Id

        let willBeRemoved = 
            heap.ParsedAsts
            |> Seq.filter (fun pa -> not (Set.contains pa.Id astComponent))
            |> Seq.map (fun pa -> pa.Id)
            |> Seq.toList

        willBeRemoved 
        |> List.map (fun theoryName ->
            match heap.ParsedAsts.TryFindAstById theoryName with
            | Some pa ->
                diagnosticsContainer.ResetStream(pa.Parsing.Uri)
                if heap.Root.Scope.ContainsKey(theoryName) then
                    heap.Root.Scope.Remove theoryName |> ignore
                heap.ParsedAsts.RemoveAll (fun pAst -> pAst.Id = theoryName) |> ignore
            | None -> ()
        ) |> ignore
    | None -> ()

    match heap.ParsedAsts.TryFindAstById(uriToBeReset.TheoryName) with
    | Some theoryToBeReset -> 
        if heap.Root.Scope.ContainsKey(theoryToBeReset.Id) then
            theoryToBeReset.Status <- ParsedAstStatus.UsesClausesEvaluated
    | None -> ()



/// <summary>
/// Parse and load all referenced namespaces reachable via <c>uses</c> clauses from the provided input.
/// </summary>
/// <param name="input">Source content of the current file to parse and register.</param>
/// <param name="uri">URI for the current source; used to derive the theory name and cache locations.</param>
/// <param name="fplLibUrl">Base URL of the remote FPL library repository to consult for remote sources.</param>
/// <returns>Unit. The function populates the global <c>heap.ParsedAsts</c> registry and emits diagnostics as needed.</returns>
/// <remarks>
/// This function:
/// - ensures the input AST is added or updated in the parsed AST registry,
/// - iteratively resolves all uses clauses (including nested references),
/// - detects duplicate aliases and duplicate files,
/// - performs garbage collection of unreachable parsed ASTs,
/// - and detects cycles in the resulting dependency graph emitting NSP04 diagnostics when found.
/// </remarks>
let loadAllUsesClauses input (uri:PathEquivalentUri) fplLibUrl = 
    diagnosticsContainer.CurrentUri <- uri
    let sources = acquireSources uri fplLibUrl offlineWatcher.OfflineMode
    let currentName = addOrUpdateParsedAst input uri heap.ParsedAsts
    emitDiagnosticsForDuplicateFiles sources (EvalAliasedNamespaceIdentifier.CreateEani(uri, offlineWatcher.OfflineMode))
    let mutable found = true

    while found do
        let loadedParsedAst = heap.ParsedAsts.TryFindLoadedAst()
        match loadedParsedAst with
        | Some pa -> 
            // evaluate the EvalAliasedNamespaceIdentifier list of the ast
            let eaniList = 
                pa.Parsing.BuildingBlockAsts
                |> List.map (fun buildingBlock -> eval_uses_clause offlineWatcher.OfflineMode buildingBlock)
                |> List.concat
            pa.Sorting.EANIList <- eaniList
            pa.Status <- ParsedAstStatus.UsesClausesEvaluated
            findDuplicateAliases pa.Sorting.EANIList |> ignore
            pa.Sorting.EANIList
            |> List.iter (fun (eani:EvalAliasedNamespaceIdentifier) -> 
                getParsedAstsMatchingAliasedNamespaceIdentifier sources heap.ParsedAsts eani pa 
                emitDiagnosticsForDuplicateFiles sources eani
            ) |> ignore
        | None -> 
            found <- false
    garbageCollector uri
    if isCircular heap.ParsedAsts then
        let cycle = findCycle heap.ParsedAsts
        match cycle with
        | Some lst -> 
            let lstWithCurrentAsHead = rearrangeList currentName lst @ [currentName]
            let path = String.concat " -> " lstWithCurrentAsHead
            let parsedAstThatStartsTheCycle = heap.ParsedAsts.TryFindAstById(lstWithCurrentAsHead.Head)
            let circularReferencedName = List.item 1 lstWithCurrentAsHead
            match parsedAstThatStartsTheCycle with 
            | Some pa -> 
                    let circularEaniReferenceList = 
                        pa.Sorting.EANIList |> List.filter (fun eani -> eani.Name = circularReferencedName)
                    if circularEaniReferenceList.Length > 0 then 
                        let circularEaniReference = circularEaniReferenceList |> List.head
                        emitNSP04Diagnostics path circularEaniReference.StartPos circularEaniReference.EndPos
            | None -> ()
        | None -> ()


