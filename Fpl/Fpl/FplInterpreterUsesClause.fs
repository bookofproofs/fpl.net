module FplInterpreterUsesClause
open System.Text.RegularExpressions
open System.Net.Http
open System.IO
open System.Security.Cryptography
open System.Text
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
                Diagnostic.Code = NSP002 (url, ex.Message)
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
                Diagnostic.Code = NSP001 (fileName, ex.Message)
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

/// A type that encapsulates the sources found for a uses clause 
/// and provides members to filter those from the file system and those from 
/// the web.
type FplSources(strings: string list) =
    member this.Strings = strings
    member this.IsUrl (s: string) =
        let pattern = @"^https?://"
        Regex.IsMatch(s, pattern)
    member this.IsFilePath (s: string) =
        try
            Path.GetFullPath(s) |> ignore
            let pattern = @"^https?://"
            not (Regex.IsMatch(s, pattern))
        with
        | :? ArgumentException -> false
    member this.Urls = List.filter this.IsUrl this.Strings
    member this.FilePaths = List.filter this.IsFilePath this.Strings
    member this.Length = this.Strings.Length
    member this.NoneFound = this.Strings.Length = 0

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

/// computes an MD5 checksum of a string
let computeMD5Checksum (input: string) =
    let md5 = MD5.Create()
    let inputBytes = Encoding.ASCII.GetBytes(input)
    let hash = md5.ComputeHash(inputBytes)
    hash |> Array.map (fun b -> b.ToString("x2")) |> String.concat ""
    
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
    let checkSum = computeMD5Checksum fileContent
    match idAlreadyFound with
    | Some pa -> 
        if pa.Checksum <> checkSum then
            // if there ist a Parsed Ast with the same Name as the eani.Name 
            // and its checksum differs from the previous checksum 
            // then replace the ast, checksum, location, sourcecode, the 
            pa.Ast <- fplParser fileContent
            pa.UriPath <- fileLoc
            pa.FplSourceCode <- fileContent
            pa.Checksum <- checkSum
            pa.EANIList <- []
            pa.Status <- ParsedAstStatus.Loaded

        else
            // if the checksum is the same, do not replace anything
            ()
    | None -> 
        // add a new ParsedAst
        let pa = { 
            ParsedAst.UriPath = fileLoc
            ParsedAst.FplSourceCode = fileContent
            ParsedAst.Ast = fplParser fileContent
            ParsedAst.Checksum = checkSum
            ParsedAst.Id = name 
            ParsedAst.ReferencingAsts = []
            ParsedAst.ReferencedAsts = []
            ParsedAst.TopologicalSorting = 0
            ParsedAst.EANIList = [] 
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


let findDuplicateAliases (eaniList: EvalAliasedNamespaceIdentifier list) =
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
                    Diagnostic.Code = NSP003 alias.AliasOrStar
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


let getParsedAstsMatchingAliasedNamespaceIdentifier (uri: Uri) (fplLibUrl: string) (parsedAsts:List<ParsedAst>) (eani:EvalAliasedNamespaceIdentifier) =
    let availableSources = acquireSources eani uri fplLibUrl 
    if availableSources.NoneFound then 
        let diagnostic =
            { 
                Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter 
                Diagnostic.Severity = DiagnosticSeverity.Error
                Diagnostic.StartPos = eani.StartPos
                Diagnostic.EndPos = eani.EndPos
                Diagnostic.Code = NSP000 eani.FileNamePattern
                Diagnostic.Alternatives = None
            }
        FplParser.parserDiagnostics.AddDiagnostic diagnostic
        ()
    else
        getParsedAstsFromSources eani availableSources.FilePaths loadFile parsedAsts 
        getParsedAstsFromSources eani availableSources.Urls downloadFile parsedAsts 

let private isCircular (parsedAsts:List<ParsedAst>) = 
    let l0 = Stack<ParsedAst>()
    let igrad = Dictionary<string,int>()
    parsedAsts |> Seq.map (fun pa -> 
        igrad.Add(pa.Id, pa.ReferencingAsts.Length)
        if pa.ReferencingAsts.Length = 0 then l0.Push(pa)
    ) |> ignore
    let mutable i = -1
    let mutable isCircular = false
    while not isCircular && i < parsedAsts.Count do
        i <- i + 1 
        isCircular <- l0.Count = 0 
        if not isCircular then
            let v = l0.Pop()
            v.TopologicalSorting <- i
            v.ReferencedAsts |> List.iter (fun name -> 
                igrad[name] <- igrad[name] - 1
                if igrad[name] = 0 then
                    l0.Push(parsedAsts.Find(fun pa -> pa.Id = name))
            )
    isCircular

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
            |> List.choose (fun x -> if List.contains x.Id node.ReferencingAsts then Some x else None)
            |> List.tryPick (dfs visited path)
    parsedAsts |> Seq.toList |> List.tryPick (dfs Set.empty [])


let private chainParsedAsts (alreadyLoaded:List<ParsedAst>) parsedAst (eani:EvalAliasedNamespaceIdentifier) = 
    // complement referenced asts 
    if not (List.contains eani.Name parsedAst.ReferencedAsts) then 
        parsedAst.ReferencedAsts <- parsedAst.ReferencedAsts @ [eani.Name]
    // complement referencing asts in the specific parsedAst even if it was already loaded
    let referencedPa = tryFindParsedAstByName eani.Name alreadyLoaded
    match referencedPa with
    | Some pa -> 
        if not (List.contains parsedAst.Id pa.ReferencingAsts) then 
            pa.ReferencingAsts <- pa.ReferencingAsts @ [parsedAst.Id]
    | None -> ()


let rearrangeList element list =
    let afterElement = list |> List.skipWhile ((<>) element)
    let beforeElement = list |> List.takeWhile ((<>) element)
    afterElement @ beforeElement


/// Parses the input at Uri and loads all referenced namespaces until
/// each of them was loaded. If a referenced namespace contains even more uses clauses,
/// their namespaces will also be loaded. The result is a list of ParsedAst objects.
let loadAllUsesClauses input (uri:Uri) fplLibUrl (parsedAsts:List<ParsedAst>) = 
    let currentName = addOrUpdateParsedAst input uri.LocalPath parsedAsts
    let mutable found = true

    while found do
        let loadedParsedAst = tryFindParsedAstLoaded parsedAsts
        match loadedParsedAst with
        | Some pa -> 
            // evaluate the EvalAliasedNamespaceIdentifier list of the ast
            pa.EANIList <- eval_uses_clause pa.Ast 
            pa.Status <- ParsedAstStatus.UsesClausesEvaluated
            findDuplicateAliases pa.EANIList |> ignore
            pa.EANIList
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
            let circularEaniReference = parsedAstThatStartsTheCycle.EANIList |> List.filter (fun eani -> eani.Name = circularReferencedName) |> List.head
            let diagnostic =
                    { 
                        Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter 
                        Diagnostic.Severity = DiagnosticSeverity.Error
                        Diagnostic.StartPos = circularEaniReference.StartPos
                        Diagnostic.EndPos = circularEaniReference.EndPos
                        Diagnostic.Code = NSP004 path
                        Diagnostic.Alternatives = None
                    }
            FplParser.parserDiagnostics.AddDiagnostic diagnostic

        | None -> ()
    parsedAsts

