module FplInterpreterUsesClause
open System.Text.RegularExpressions
open System.Net.Http
open System.IO
open System.Collections.Generic
open System
open FParsec
open FplGrammarTypes
open FplInterpreterTypes
open FplParser
open ErrDiagnostics



/// A recursive function evaluating an AST and returning a list of EvalAliasedNamespaceIdentifier records
/// for each occurrence of the uses clause in the FPL code.
let rec eval_uses_clause = function 
    | Ast.AST ((pos1, pos2), ast) -> 
        eval_uses_clause ast
    | Ast.Namespace (optAst, asts) -> 
        let results = asts |> List.collect eval_uses_clause
        let optAstResults = optAst |> Option.map eval_uses_clause |> Option.defaultValue []
        optAstResults @ results
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
                          | Some Ast.Star -> 
                                    { 
                                        StartPos = Position("",0,1,1)
                                        EndPos = Position("",0,1,1)
                                        AliasOrStar = "*"
                                    }

                          | _ -> 
                                    { 
                                        StartPos = Position("",0,1,1)
                                        EndPos = Position("",0,1,1)
                                        AliasOrStar = ""
                                    }

        match ast with
        | Ast.NamespaceIdentifier ((p1, p2), asts) -> 
            let pascalCaseIdList = asts |> List.collect (function Ast.PascalCaseId s -> [s] | _ -> [])
            [{  
                EvalAliasedNamespaceIdentifier.StartPos = p1 
                EvalAliasedNamespaceIdentifier.EndPos = p2 
                EvalAliasedNamespaceIdentifier.EvalAlias = evalAlias
                EvalAliasedNamespaceIdentifier.PascalCaseIdList = pascalCaseIdList 
            }]
        | _ -> []
    | _ -> []

let private downloadFile url (e:EvalAliasedNamespaceIdentifier) =
    let client = new HttpClient()
    try
        async {
            let! data = client.GetStringAsync(Uri(url)) |> Async.AwaitTask
            return data
        } |> Async.RunSynchronously
    with
    | ex -> 
        let diagnostic =
            { 
                Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter 
                Diagnostic.Severity = DiagnosticSeverity.Error
                Diagnostic.StartPos = e.StartPos
                Diagnostic.EndPos = e.EndPos
                Diagnostic.Code = NSP02 (url, ex.Message)
                Diagnostic.Alternatives = None 
            }
        FplParser.parserDiagnostics.AddDiagnostic diagnostic 
        ""

let private loadFile fileName (e:EvalAliasedNamespaceIdentifier) =
    try
        File.ReadAllText fileName
    with
    | ex -> 
        let diagnostic =
            { 
                Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter 
                Diagnostic.Severity = DiagnosticSeverity.Error
                Diagnostic.StartPos = e.StartPos
                Diagnostic.EndPos = e.EndPos
                Diagnostic.Code = NSP01 (fileName, ex.Message)
                Diagnostic.Alternatives = None
            }
        FplParser.parserDiagnostics.AddDiagnostic diagnostic
        ""

let private wildcardToRegex (wildcard : string) =
    "^" + Regex.Escape(wildcard).Replace("\\*", ".*").Replace("\\?", ".") + "$"

let findFilesInLibMapWithWildcard (libmap:string) wildcard =
    let regexPattern = wildcardToRegex wildcard
    let regex = Regex(regexPattern, RegexOptions.IgnoreCase)
    libmap.Split("\n")
    |> Seq.filter regex.IsMatch
    |> Seq.toList

let createLibSubfolder (uri: Uri) =
    let unescapedPath = 
        let p = Uri.UnescapeDataString(uri.LocalPath)
        let pattern = @"^[\/\\][a-zA-Z]:"
        if Regex.IsMatch(p, pattern) then
            p.Substring(1)
        else
            p
    let directoryPath = Path.GetDirectoryName(unescapedPath)
    let libDirectoryPath = Path.Combine(directoryPath, "lib")
    if not <| Directory.Exists(libDirectoryPath) then
        Directory.CreateDirectory(libDirectoryPath) |> ignore
    (directoryPath, libDirectoryPath)


let downloadLibMap (currentWebRepo: string) (e:EvalAliasedNamespaceIdentifier) =
    let libMap = downloadFile (currentWebRepo + "/libmap.txt") e
    libMap    

/// Acquires FPL sources that can be found with a single uses clause.
/// They are searched for in the current directory of the file, in the lib subfolder
/// as well as in the web resource 
let acquireSources (e:EvalAliasedNamespaceIdentifier) (uri: Uri) (fplLibUrl: string) =
    let (directoryPath,libDirectoryPath) = createLibSubfolder uri
    let fileNamesInCurrDir = Directory.EnumerateFiles(directoryPath, e.FileNamePattern) |> Seq.toList
    let fileNamesInLibSubDir = Directory.EnumerateFiles(libDirectoryPath, e.FileNamePattern) |> Seq.toList
    let libMap = downloadLibMap fplLibUrl e
    let filesToDownload = findFilesInLibMapWithWildcard libMap e.FileNamePattern 
                            |> List.map (fun s -> fplLibUrl + "/" + s)
    FplSources(fileNamesInCurrDir @ fileNamesInLibSubDir @ filesToDownload)
    
let tryFindParsedAstByName identifier (parsedAsts:List<ParsedAst>) =
    if parsedAsts.Exists(fun pa -> pa.Id = identifier) then
        Some(parsedAsts.Find(fun pa -> pa.Id = identifier))
    else
        None

let tryFindParsedAstLoaded (parsedAsts:List<ParsedAst>) =
    if parsedAsts.Exists(fun pa -> pa.Status = ParsedAstStatus.Loaded) then
        Some(parsedAsts.Find(fun pa -> pa.Status = ParsedAstStatus.Loaded))
    else
        None

let private addOrUpdateParsedAst fileContent (fileLoc:string) (parsedAsts:List<ParsedAst>) = 
    let name = Path.GetFileNameWithoutExtension(fileLoc)
    let idAlreadyFound = tryFindParsedAstByName name parsedAsts
    match idAlreadyFound with
    | Some pa -> 
        if pa.Parsing.Reset fileContent fileLoc then
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
        let parsing = {
            ParsingProperties.UriPath = fileLoc
            ParsingProperties.FplSourceCode = fileContent
            ParsingProperties.Ast = fplParser fileContent
            ParsingProperties.Checksum = computeMD5Checksum fileContent
        }
        let sorting = {
            SortingProperties.TopologicalSorting = 0
            SortingProperties.ReferencingAsts = []
            SortingProperties.ReferencedAsts = []
            SortingProperties.EANIList = [] 
        }
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

/// For a given EvalAliasedNamespaceIdentifier, a sequence of FplSources was found (these can be either absolute file paths or URLs).
/// 
/// EvalAliasedNamespaceIdentifier  given EvalAliasedNamespaceIdentifier and a 
/// Generates a list of ParsedAsts in a collection, in which even more ParseAsts will 
/// be added if this ast contains some 'uses' clauses.
let private getParsedAstsFromSources 
    (eani:EvalAliasedNamespaceIdentifier) 
    (fplSources: seq<string>) (
    getContent: string -> EvalAliasedNamespaceIdentifier -> string) 
    (parsedAsts:List<ParsedAst>) =

    fplSources
    |> Seq.iter (fun fileLoc ->
        // load the content of every source
        let fileContent = getContent fileLoc eani
        addOrUpdateParsedAst fileContent fileLoc parsedAsts |> ignore
    ) 


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

    // groups the remaining identifiers by AliasOrStar 
    |> List.groupBy id  
    // Select the ones that appear more than once
    |> List.choose (fun (alias, instances) -> if List.length instances > 1 then Some alias else None)

/// Emits diagnostics if the same FPL theory can be found in multiple sources.
let private findDuplicateFiles (uri: Uri) (fplLibUrl: string) =
    let pos = Position("", 0, 0, 1)
    let name = Path.GetFileNameWithoutExtension(uri.LocalPath)
    let evalAlias = { 
        EvalAlias.StartPos = pos 
        EvalAlias.EndPos = pos 
        EvalAlias.AliasOrStar = ""
        }
    let eani =  { 
        EvalAliasedNamespaceIdentifier.StartPos = pos
        EvalAliasedNamespaceIdentifier.EndPos = pos
        EvalAliasedNamespaceIdentifier.EvalAlias = evalAlias
        EvalAliasedNamespaceIdentifier.PascalCaseIdList = [name]
        }

    let availableSources = acquireSources eani uri fplLibUrl 
    let fplTheories = 
        availableSources.Paths
        |> List.map (fun fp -> (Path.GetFileNameWithoutExtension fp, fp))
    let groupedFplTheories = 
        fplTheories
        |> List.groupBy fst
    let duplicates = 
        groupedFplTheories
        |> List.filter (fun (_, paths) -> List.length paths > 1)
        |> List.map (fun (fileName, paths) -> 
            let pathTypes =
                List.map snd paths
                |> List.map (fun path -> 
                    if availableSources.IsFilePath(path) && (path.Contains("/lib/") || path.Contains(@"\lib\")) then
                        "./lib"
                    elif  availableSources.IsFilePath(path) then 
                        "./"
                    else
                        "https"
                )
            (fileName, pathTypes)
            )
    duplicates
    |> List.map (fun (duplicate,pathTypes) ->
            let diagnostic =
                { 
                    Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter 
                    Diagnostic.Severity = DiagnosticSeverity.Error
                    Diagnostic.StartPos = Position("",0,0,1)
                    Diagnostic.EndPos = Position("",0,0,1)
                    Diagnostic.Code = NSP05 (duplicate, pathTypes)
                    Diagnostic.Alternatives = None
                }
            FplParser.parserDiagnostics.AddDiagnostic diagnostic
    )
    |> ignore


let getParsedAstsMatchingAliasedNamespaceIdentifier (uri: Uri) (fplLibUrl: string) (parsedAsts:List<ParsedAst>) (eani:EvalAliasedNamespaceIdentifier) =
    let availableSources = acquireSources eani uri fplLibUrl 
    if availableSources.NoneFound then 
        let diagnostic =
            { 
                Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter 
                Diagnostic.Severity = DiagnosticSeverity.Error
                Diagnostic.StartPos = eani.StartPos
                Diagnostic.EndPos = eani.EndPos
                Diagnostic.Code = NSP00 eani.FileNamePattern
                Diagnostic.Alternatives = None
            }
        FplParser.parserDiagnostics.AddDiagnostic diagnostic
        ()
    else
        getParsedAstsFromSources eani availableSources.FilePaths loadFile parsedAsts 
        getParsedAstsFromSources eani availableSources.Urls downloadFile parsedAsts 

/// Calculates the ParsedAst.TopologicalSorting property of the all ParsedAsts 
/// unless the resulting directed graph is circular. If the function returns false, 
/// there is a valid topological sorting. If true is returned, there is no 
/// valid topological sorting and there is a cycle caused by the uses clauses in the
/// ParsedAsts.
let private isCircular (parsedAsts:List<ParsedAst>) = 
    let l0 = Stack<ParsedAst>()
    let igrad = Dictionary<string,int>()
    parsedAsts |> Seq.map (fun pa -> 
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
                igrad[name] <- igrad[name] - 1
                if igrad[name] = 0 then
                    l0.Push(parsedAsts.Find(fun pa -> pa.Id = name))
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


let private chainParsedAsts (alreadyLoaded:List<ParsedAst>) parsedAst (eani:EvalAliasedNamespaceIdentifier) = 
    // complement referenced asts 
    if not (List.contains eani.Name parsedAst.Sorting.ReferencedAsts) then 
        parsedAst.Sorting.ReferencedAsts <- parsedAst.Sorting.ReferencedAsts @ [eani.Name]
    // complement referencing asts in the specific parsedAst even if it was already loaded
    let referencedPa = tryFindParsedAstByName eani.Name alreadyLoaded
    match referencedPa with
    | Some pa -> 
        if not (List.contains parsedAst.Id pa.Sorting.ReferencingAsts) then 
            pa.Sorting.ReferencingAsts <- pa.Sorting.ReferencingAsts @ [parsedAst.Id]
    | None -> ()


let rearrangeList element list =
    let afterElement = list |> List.skipWhile ((<>) element)
    let beforeElement = list |> List.takeWhile ((<>) element)
    afterElement @ beforeElement


/// Parses the input at Uri and loads all referenced namespaces until
/// each of them was loaded. If a referenced namespace contains even more uses clauses,
/// their namespaces will also be loaded. The result is a list of ParsedAst objects.
let loadAllUsesClauses input (uri:Uri) fplLibUrl (parsedAsts:List<ParsedAst>) = 
    findDuplicateFiles uri fplLibUrl
    let currentName = addOrUpdateParsedAst input uri.LocalPath parsedAsts
    let mutable found = true

    while found do
        let loadedParsedAst = tryFindParsedAstLoaded parsedAsts
        match loadedParsedAst with
        | Some pa -> 
            // evaluate the EvalAliasedNamespaceIdentifier list of the ast
            pa.Sorting.EANIList <- eval_uses_clause pa.Parsing.Ast 
            pa.Status <- ParsedAstStatus.UsesClausesEvaluated
            findDuplicateAliases pa.Sorting.EANIList |> ignore
            pa.Sorting.EANIList
            |> List.map (fun (eani:EvalAliasedNamespaceIdentifier) -> 
                getParsedAstsMatchingAliasedNamespaceIdentifier uri fplLibUrl parsedAsts eani
                chainParsedAsts parsedAsts pa eani
            ) |> ignore
        | None -> 
            found <- false
    if isCircular parsedAsts then
        let cycle = findCycle parsedAsts
        match cycle with
        | Some lst -> 
            let lstWithCurrentAsHead = rearrangeList currentName lst @ [currentName]
            let path = String.concat " -> " lstWithCurrentAsHead
            let parsedAstThatStartsTheCycle = parsedAsts |> Seq.filter (fun pa -> pa.Id = lstWithCurrentAsHead.Head) |> Seq.head
            let circularReferencedName = List.item 1 lstWithCurrentAsHead
            let circularEaniReference = parsedAstThatStartsTheCycle.Sorting.EANIList |> List.filter (fun eani -> eani.Name = circularReferencedName) |> List.head
            let diagnostic =
                    { 
                        Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter 
                        Diagnostic.Severity = DiagnosticSeverity.Error
                        Diagnostic.StartPos = circularEaniReference.StartPos
                        Diagnostic.EndPos = circularEaniReference.EndPos
                        Diagnostic.Code = NSP04 path
                        Diagnostic.Alternatives = None
                    }
            FplParser.parserDiagnostics.AddDiagnostic diagnostic

        | None -> ()


