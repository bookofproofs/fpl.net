﻿module FplInterpreterUsesClause
open System.Text.RegularExpressions
open System.Net.Http
open System.IO
open System.Security.Cryptography
open System.Text
open System.Collections.Generic
open System
open FplGrammarTypes
open FplParser
open ErrDiagnostics
open FParsec

type EvalAlias =  
    {
        StartPos: Position
        EndPos: Position
        AliasOrStar: string
    }

/// A record type to store all the necessary fields for parsed uses clauses in FPL code
type EvalAliasedNamespaceIdentifier = 
    { StartPos: Position
      EndPos: Position
      EvalAlias: EvalAlias
      PascalCaseIdList: string list }
    with
        member this.FileNamePattern = 
            let pascalCaseIdList = String.concat "." this.PascalCaseIdList
            match this.EvalAlias.AliasOrStar with
            | "*" -> 
                sprintf "%s*.fpl" pascalCaseIdList
            | _ -> 
                sprintf "%s.fpl" pascalCaseIdList
        member this.Name = 
            let concatenatedPascalCaseIds = String.concat "." this.PascalCaseIdList
            match this.EvalAlias.AliasOrStar with
            | "*" -> concatenatedPascalCaseIds
            | _ when this.EvalAlias.AliasOrStar <> "*" && this.EvalAlias.AliasOrStar <> "" -> this.EvalAlias.AliasOrStar
            | _ -> concatenatedPascalCaseIds

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

type ParsedAstStatus = 
    | Loaded
    | UsesClausesEvaluated

/// A record type to store all the necessary fields for parsed namespaces in FPL code
type ParsedAst =
    { 
        UriPath: string // source of the ast
        FplCode: string // source code of the ast
        Ast: Ast // parsed ast
        Checksum: string // checksum of the parsed ast
        Id: string // id of this ast giving the order in which it was parsed with other asts
        mutable TopologicalSorting: int // an order in which the ParsedAsts have to be interpreted to avoid undeclared identifiers (undefined if a circle was caused by uses clauses)
        mutable ReferencingAsts: string list // list of asts "referencing" this one with a uses clause
        mutable ReferencedAsts: string list // list of asts "referenced" by this one in a uses clause
        EANI: EvalAliasedNamespaceIdentifier // uses clause that as a first caused this ast to be loaded 
        mutable Status: ParsedAstStatus
    }

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

type SymbolTable =
    { ParsedAsts: List<ParsedAst> }
    
let private getParsedAstsFromSources 
    (eani:EvalAliasedNamespaceIdentifier) 
    (sources: seq<string>) (
    getContent: string -> EvalAliasedNamespaceIdentifier -> string) 
    (alreadyLoaded:List<ParsedAst>)
    parsedAst =
    let alreadyLoadedIds = alreadyLoaded |> Seq.map (fun pa -> pa.Id) |> Seq.toList
    let filteredSources =
        Seq.filter (fun (s:string) -> 
            let name = Path.GetFileNameWithoutExtension(s)
            not (List.contains name alreadyLoadedIds)
        ) sources

    // complement referenced asts 
    if not (List.contains eani.Name parsedAst.ReferencedAsts) then 
        parsedAst.ReferencedAsts <- parsedAst.ReferencedAsts @ [eani.Name]
    // complement referencing asts in the specific parsedAst even if it was already loaded
    if (List.contains eani.Name alreadyLoadedIds) then
        let referencedPa = alreadyLoaded.Find(fun pa -> pa.Id = eani.Name)
        if not (List.contains parsedAst.Id referencedPa.ReferencingAsts) then 
            referencedPa.ReferencingAsts <- referencedPa.ReferencingAsts @ [parsedAst.Id]

    filteredSources
    |> Seq.choose (fun fileLoc ->
        let fileContent = getContent fileLoc eani
        if fileContent <> "" then
            Some { 
                ParsedAst.UriPath = fileLoc
                ParsedAst.FplCode = fileContent
                ParsedAst.Ast = fplParser fileContent
                ParsedAst.Checksum = computeMD5Checksum fileContent
                ParsedAst.Id = eani.Name 
                ParsedAst.ReferencingAsts = [parsedAst.Id]
                ParsedAst.ReferencedAsts = []
                ParsedAst.TopologicalSorting = 0
                ParsedAst.EANI = eani 
                ParsedAst.Status = ParsedAstStatus.Loaded
            }
        else
            None
    ) 
    |> Seq.toList


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


/// Takes an `ast`, interprets it by extracting the uses clauses and looks for the files 
/// in the `currentPath` to be loaded and parsed to create even more ASTs.
/// Returns a list ParseAst objects or adds new diagnostics if the files were not found.
let findParsedAstsMatchingAliasedNamespaceIdentifier parsedAst (uri: Uri) (fplLibUrl: string) (alreadyLoaded:List<ParsedAst>) =
    let ad = FplParser.parserDiagnostics

    let parsedAstsMatchingAliasedNamespaceIdentifier (eani:EvalAliasedNamespaceIdentifier) =
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
            ad.AddDiagnostic diagnostic
            []
        else
            let astsFromFilePaths = getParsedAstsFromSources eani availableSources.FilePaths loadFile alreadyLoaded parsedAst
            let astsFromUrls = getParsedAstsFromSources eani availableSources.Urls downloadFile alreadyLoaded parsedAst
            astsFromFilePaths @ astsFromUrls

    let eaniList = eval_uses_clause parsedAst.Ast 
    findDuplicateAliases eaniList |> ignore

    eaniList 
    |> List.map (fun (eval:EvalAliasedNamespaceIdentifier) -> eval)
    |> List.collect parsedAstsMatchingAliasedNamespaceIdentifier

/// Creates an initial ParsedAst for the symbol table. 
/// We will put it in a collection, in which even more ParseAsts will 
/// be added if this ast contains some 'uses' clauses.
let private getInitiateParsedAst input (uri:Uri) = 
    let name = Path.GetFileNameWithoutExtension(uri.LocalPath)
    let initialEvaAlias = {
        EvalAlias.StartPos = Position ("",0,1,1)
        EvalAlias.EndPos = Position ("",0,1,1)
        EvalAlias.AliasOrStar = ""
    }
    let initialEvalAliasedNamespaceIdentifier = {   
        EvalAliasedNamespaceIdentifier.StartPos = Position ("",0,1,1)
        EvalAliasedNamespaceIdentifier.EndPos = Position ("",0,1,1)
        EvalAliasedNamespaceIdentifier.EvalAlias = initialEvaAlias
        EvalAliasedNamespaceIdentifier.PascalCaseIdList = name.Split('.') |> Array.toList
    }
    {
        ParsedAst.UriPath = uri.AbsolutePath
        ParsedAst.FplCode = input
        ParsedAst.Ast = fplParser input
        ParsedAst.Checksum = computeMD5Checksum input
        ParsedAst.Id = initialEvalAliasedNamespaceIdentifier.Name
        ParsedAst.ReferencingAsts = []
        ParsedAst.ReferencedAsts = []
        ParsedAst.TopologicalSorting = 0
        ParsedAst.EANI = initialEvalAliasedNamespaceIdentifier 
        ParsedAst.Status = ParsedAstStatus.Loaded
    }
    
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

/// Parses the input at Uri and loads all referenced namespaces until
/// each of them was loaded. If a referenced namespace contains even more uses clauses,
/// their namespaces will also be loaded. The result is a list of ParsedAst objects.
let loadAllUsesClauses input uri fplLibUrl = 
    let symbolTable = { ParsedAsts = List<ParsedAst>() }
    let evenMoreParsedAsts = List<ParsedAst>()
    evenMoreParsedAsts.Add(getInitiateParsedAst input uri)
    while evenMoreParsedAsts.Count > 0 do
        symbolTable.ParsedAsts.AddRange(evenMoreParsedAsts)
        evenMoreParsedAsts.Clear()
        symbolTable.ParsedAsts
        |> Seq.iter (fun pa -> 
            if pa.Status = ParsedAstStatus.Loaded then
                let newPa = findParsedAstsMatchingAliasedNamespaceIdentifier pa uri fplLibUrl symbolTable.ParsedAsts
                pa.Status <- ParsedAstStatus.UsesClausesEvaluated
                evenMoreParsedAsts.AddRange(newPa)
        )
        |> ignore
    if isCircular symbolTable.ParsedAsts then
        let circleStart = symbolTable.ParsedAsts.Find(fun pa -> pa.ReferencingAsts.Length = 0)
        let diagnostic =
                { 
                    Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter 
                    Diagnostic.Severity = DiagnosticSeverity.Error
                    Diagnostic.StartPos = circleStart.EANI.StartPos
                    Diagnostic.EndPos = circleStart.EANI.EndPos
                    Diagnostic.Code = NSP004 circleStart.Id
                    Diagnostic.Alternatives = None
                }
        FplParser.parserDiagnostics.AddDiagnostic diagnostic

    symbolTable

