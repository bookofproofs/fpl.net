module FplInterpreterUsesClause
open System.Text.RegularExpressions
open System.Net.Http
open System.IO
open System.Collections.Generic
open System
open FParsec
open FplGrammarTypes
open FplInterpreterTypes
open ErrDiagnostics



/// A recursive function evaluating an AST and returning a list of EvalAliasedNamespaceIdentifier records
/// for each occurrence of the uses clause in the FPL code.
let rec eval_uses_clause = function 
    | Ast.AST ((pos1, pos2), ast) -> 
        eval_uses_clause ast
    | Ast.Namespace (asts) -> 
        let results = asts |> List.collect eval_uses_clause
        results
    | Ast.UsesClause ((pos1, pos2), ast) -> 
        eval_uses_clause ast
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
            let pascalCaseIdList = asts |> List.collect (function Ast.PascalCaseId s -> [s] | _ -> [])
            [EvalAliasedNamespaceIdentifier.CreateEani(pascalCaseIdList, evalAlias, p1, p2)]
        | _ -> []
    | _ -> []

let private downloadFile url (e:EvalAliasedNamespaceIdentifier) =
    let client = new HttpClient()
    try
        async {
            let! data = client.GetStringAsync(PathEquivalentUri(url)) |> Async.AwaitTask
            return data
        } |> Async.RunSynchronously
    with
    | ex -> 
        let diagnostic =
            { 
                Diagnostic.Uri = ad.CurrentUri
                Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter 
                Diagnostic.Severity = DiagnosticSeverity.Error
                Diagnostic.StartPos = e.StartPos
                Diagnostic.EndPos = e.EndPos
                Diagnostic.Code = NSP02 (url, ex.Message)
                Diagnostic.Alternatives = None 
            }
        ad.AddDiagnostic diagnostic 
        ""

let private loadFile fileName (e:EvalAliasedNamespaceIdentifier) =
    try
        File.ReadAllText fileName
    with
    | ex -> 
        let diagnostic =
            { 
                Diagnostic.Uri = ad.CurrentUri
                Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter 
                Diagnostic.Severity = DiagnosticSeverity.Error
                Diagnostic.StartPos = e.StartPos
                Diagnostic.EndPos = e.EndPos
                Diagnostic.Code = NSP01 (fileName, ex.Message)
                Diagnostic.Alternatives = None
            }
        ad.AddDiagnostic diagnostic
        ""

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


let downloadLibMap (uri:PathEquivalentUri) (currentWebRepo: string) =
    let pos = Position("", 0, 1, 1)
    let libMap = downloadFile (currentWebRepo + "/libmap.txt") (EvalAliasedNamespaceIdentifier.CreateEani("","", pos, pos))
    libMap    

/// Acquires FPL sources that can be found with a single uses clause.
/// They are searched for in the current directory of the file, in the lib subfolder
/// as well as in the web resource 
let acquireSources (uri: PathEquivalentUri) (fplLibUrl: string) =

    let (_,libDirectoryPath) = createSubfolder uri "lib"
    let (directoryPath,repoDirectoryPath) = createSubfolder uri "repo"


    let fileNamesInCurrDir = Directory.EnumerateFiles(directoryPath, "*.fpl") |> Seq.map (fun path -> PathEquivalentUri.EscapedUri(path)) |> Seq.toList
    let fileNamesInLibSubDir = Directory.EnumerateFiles(libDirectoryPath, "*.fpl") |> Seq.map (fun path -> PathEquivalentUri.EscapedUri(path)) |> Seq.toList
    let libMap = downloadLibMap uri fplLibUrl
    let filesToDownload = libMap.Split("\n") 
                           |> Seq.filter(fun s -> s<>"")
                           |> Seq.map (fun s -> PathEquivalentUri.EscapedUri(fplLibUrl + "/" + s))
                           |> Seq.toList
    FplSources(fileNamesInCurrDir @ fileNamesInLibSubDir @ filesToDownload, repoDirectoryPath)
    
let private addOrUpdateParsedAst fileContent (uri:PathEquivalentUri) (parsedAsts:ParsedAstList) = 
    let name = uri.TheoryName
    let idAlreadyFound = parsedAsts.TryFindAstById name
    match idAlreadyFound with
    | Some pa -> 
        if pa.Parsing.Reset fileContent uri then
            // if there ist a Parsed Ast with the same Name as the eani.Name 
            // and its checksum differs from the previous checksum 
            // then replace the ast, checksum, location, sourcecode, the 
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

let private findDuplicateAliases (eaniList: EvalAliasedNamespaceIdentifier list) =
    let uniqueAliases = HashSet<string>()
    eaniList
    |> List.map (fun eani -> eani.EvalAlias)
    // filter out the identifiers with AliasOrStar equal to None or Some "*"
    |> List.filter (fun alias -> alias.AliasOrStar <> "*" && alias.AliasOrStar <> "")
    |> List.map (fun alias ->
        if uniqueAliases.Contains alias.AliasOrStar then
            let diagnostic =
                { 
                    Diagnostic.Uri = ad.CurrentUri
                    Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter 
                    Diagnostic.Severity = DiagnosticSeverity.Error
                    Diagnostic.StartPos = alias.StartPos
                    Diagnostic.EndPos = alias.EndPos
                    Diagnostic.Code = NSP03 alias.AliasOrStar
                    Diagnostic.Alternatives = None
                }
            ad.AddDiagnostic diagnostic        
        else    
            uniqueAliases.Add(alias.AliasOrStar) |> ignore
    )

/// Emits diagnostics if the same FPL theory can be found in multiple sources.
let private emitDiagnosticsForDuplicateFiles (availableSources:FplSources) (eani:EvalAliasedNamespaceIdentifier) =
    availableSources.GroupedWithPreferedSource
    |> List.iter (fun (fileName, path, chosenPathType, pathTypes, theoryName) ->
        if pathTypes.Length > 1 then 
            let diagnostic =
                { 
                    Diagnostic.Uri = ad.CurrentUri
                    Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter 
                    Diagnostic.Severity = DiagnosticSeverity.Error
                    Diagnostic.StartPos = eani.StartPos
                    Diagnostic.EndPos = eani.EndPos
                    Diagnostic.Code = NSP05 (pathTypes, theoryName, chosenPathType)
                    Diagnostic.Alternatives = None
                }
            ad.AddDiagnostic diagnostic
    )
    |> ignore

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


let getParsedAstsMatchingAliasedNamespaceIdentifier (sources:FplSources) (parsedAsts:ParsedAstList) (eani:EvalAliasedNamespaceIdentifier) (currenParsedAst: ParsedAst)=
    let filtered = sources.FindWithPattern eani.FileNamePattern
    if filtered.IsEmpty then
        // Emits diagnostics if there are no files for the pattern 
        let diagnostic =
            { 
                Diagnostic.Uri = ad.CurrentUri
                Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter 
                Diagnostic.Severity = DiagnosticSeverity.Error
                Diagnostic.StartPos = eani.StartPos
                Diagnostic.EndPos = eani.EndPos
                Diagnostic.Code = NSP00 eani.FileNamePattern
                Diagnostic.Alternatives = None
            }
        ad.AddDiagnostic diagnostic
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
    
/// Calculates the ParsedAst.TopologicalSorting property of the all ParsedAsts 
/// unless the resulting directed graph is circular. If the function returns false, 
/// there is a valid topological sorting. If true is returned, there is no 
/// valid topological sorting and there is a cycle caused by the uses clauses in the
/// ParsedAsts.
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

let private rearrangeList element list =
    let afterElement = list |> List.skipWhile ((<>) element)
    let beforeElement = list |> List.takeWhile ((<>) element)
    afterElement @ beforeElement

let garbageCollector (st:SymbolTable) (uriToBeReset:PathEquivalentUri) = 
    let referencedAstsOfCurrentTheory currTheory = 
        match st.ParsedAsts.TryFindAstById(currTheory) with
        | Some pa -> pa.Sorting.ReferencedAsts
        | _ -> []

    // remove the current theory from the ReferencingAsts list of each parsedAst, if they are not contained 
    // in the current theory's reference Asts
    let rec removeNotReferencedAsts currTheory = 
        st.ParsedAsts 
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

    match st.ParsedAsts.TryFindAstById(st.MainTheory) with
    | Some mainTheory -> 
        removeNotReferencedAsts mainTheory.Id
        let astComponent = findComponent st.ParsedAsts Set.empty mainTheory.Id

        let willBeRemoved = 
            st.ParsedAsts
            |> Seq.filter (fun pa -> not (Set.contains pa.Id astComponent))
            |> Seq.map (fun pa -> pa.Id)
            |> Seq.toList

        willBeRemoved 
        |> List.map (fun theoryName ->
            match st.ParsedAsts.TryFindAstById theoryName with
            | Some pa ->
                ad.ResetStream(pa.Parsing.Uri)
                if st.Root.Scope.ContainsKey(theoryName) then
                    st.Root.Scope.Remove theoryName |> ignore
                st.ParsedAsts.RemoveAll (fun pAst -> pAst.Id = theoryName) |> ignore
            | None -> ()
        ) |> ignore
    | None -> ()

    match st.ParsedAsts.TryFindAstById(uriToBeReset.TheoryName) with
    | Some theoryToBeReset -> 
        if st.Root.Scope.ContainsKey(theoryToBeReset.Id) then
            theoryToBeReset.Status <- ParsedAstStatus.UsesClausesEvaluated
    | None -> ()



/// Parses the input at Uri and loads all referenced namespaces until
/// each of them was loaded. If a referenced namespace contains even more uses clauses,
/// their namespaces will also be loaded. The result is a list of ParsedAst objects.
let loadAllUsesClauses (st:SymbolTable) input (uri:PathEquivalentUri) fplLibUrl = 
    ad.CurrentUri <- uri
    let sources = acquireSources uri fplLibUrl
    let currentName = addOrUpdateParsedAst input uri st.ParsedAsts
    emitDiagnosticsForDuplicateFiles sources (EvalAliasedNamespaceIdentifier.CreateEani(uri))
    let mutable found = true

    while found do
        let loadedParsedAst = st.ParsedAsts.TryFindLoadedAst()
        match loadedParsedAst with
        | Some pa -> 
            // evaluate the EvalAliasedNamespaceIdentifier list of the ast
            pa.Sorting.EANIList <- eval_uses_clause pa.Parsing.Ast 
            pa.Status <- ParsedAstStatus.UsesClausesEvaluated
            findDuplicateAliases pa.Sorting.EANIList |> ignore
            pa.Sorting.EANIList
            |> List.iter (fun (eani:EvalAliasedNamespaceIdentifier) -> 
                getParsedAstsMatchingAliasedNamespaceIdentifier sources st.ParsedAsts eani pa 
                emitDiagnosticsForDuplicateFiles sources eani
            ) |> ignore
        | None -> 
            found <- false
    garbageCollector st uri
    if isCircular st.ParsedAsts then
        let cycle = findCycle st.ParsedAsts
        match cycle with
        | Some lst -> 
            let lstWithCurrentAsHead = rearrangeList currentName lst @ [currentName]
            let path = String.concat " -> " lstWithCurrentAsHead
            let parsedAstThatStartsTheCycle = st.ParsedAsts.TryFindAstById(lstWithCurrentAsHead.Head)
            let circularReferencedName = List.item 1 lstWithCurrentAsHead
            match parsedAstThatStartsTheCycle with 
            | Some pa -> 
                    let circularEaniReferenceList = 
                        pa.Sorting.EANIList |> List.filter (fun eani -> eani.Name = circularReferencedName)
                    if circularEaniReferenceList.Length > 0 then 
                        let circularEaniReference = circularEaniReferenceList |> List.head
                        let diagnostic =
                                { 
                                    Diagnostic.Uri = ad.CurrentUri
                                    Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter 
                                    Diagnostic.Severity = DiagnosticSeverity.Error
                                    Diagnostic.StartPos = circularEaniReference.StartPos
                                    Diagnostic.EndPos = circularEaniReference.EndPos
                                    Diagnostic.Code = NSP04 path
                                    Diagnostic.Alternatives = None
                                }
                        ad.AddDiagnostic diagnostic
            | None -> ()
        | None -> ()


