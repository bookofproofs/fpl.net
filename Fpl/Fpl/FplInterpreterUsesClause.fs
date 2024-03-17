module FplInterpreterUsesClause
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

type FplInterpreterErrorCode =
    | NSP000 of string
    | NSP001 of string
    | NSP002 of string * string
    | NSP003 of string

let private getErroMsg = function  
    | NSP000 fileNamePattern -> sprintf "%s could not be loaded" fileNamePattern
    | NSP001 fileName -> sprintf "%s not found" fileName
    | NSP002 (url, innerErrMsg) -> sprintf "%s not downloadable: %s" url innerErrMsg
    | NSP003 alias -> sprintf "Alias %s appeared previously in this namespace" alias

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

/// A record type to store all the necessary fields for parsed namespaces in FPL code
type ParsedAst =
    { 
        UriPath: string // source of the ast
        FplCode: string // source code of the ast
        Ast: Ast // parsed ast
        Checksum: string // checksum of the parsed ast
        Id: int // id of this ast giving the order in which it was parsed with other asts
        mutable ReferencingAsts: int list // list of asts "referencing" this one with a uses clause
        mutable ReferencedAsts: int list // list of asts "referenced" by this one in a uses clause
        EANI: EvalAliasedNamespaceIdentifier // uses clause that as a first caused this ast to be loaded 
    }

let private downloadFile url (ad: Diagnostics) (e:EvalAliasedNamespaceIdentifier) =
    let client = new HttpClient()
    try
        async {
            let! data = client.GetStringAsync(Uri(url)) |> Async.AwaitTask
            return data
        } |> Async.RunSynchronously
    with
    | ex -> 
        let msg = { DiagnosticMessage.Value =getErroMsg (NSP002 (url, ex.Message)) }
        let code = { DiagnosticCode.Code = nameof(NSP002) }
        let diagnostic =
            { 
                Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter 
                Diagnostic.Severity = DiagnosticSeverity.Error
                Diagnostic.StartPos = e.StartPos
                Diagnostic.EndPos = e.EndPos
                Diagnostic.Message = msg
                Diagnostic.Code = code
            }
        ad.AddDiagnostic diagnostic 
        ""

let private loadFile fileName (ad: Diagnostics) (e:EvalAliasedNamespaceIdentifier) =
    try
        File.ReadAllText fileName
    with
    | :? FileNotFoundException -> 
        let msg = { DiagnosticMessage.Value = getErroMsg (NSP001 fileName) }
        let code = { DiagnosticCode.Code = nameof(NSP001) }
        let diagnostic =
            { 
                Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter 
                Diagnostic.Severity = DiagnosticSeverity.Error
                Diagnostic.StartPos = e.StartPos
                Diagnostic.EndPos = e.EndPos
                Diagnostic.Message = msg
                Diagnostic.Code = code
            }
        ad.AddDiagnostic diagnostic
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


let downloadLibMap (currentWebRepo: string) (ad: Diagnostics) (e:EvalAliasedNamespaceIdentifier) =
    let libMap = downloadFile (currentWebRepo + "/libmap.txt") ad e
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
let acquireSources (e:EvalAliasedNamespaceIdentifier) (uri: Uri) (fplLibUrl: string) (ad: Diagnostics) =
    let (directoryPath,libDirectoryPath) = createLibSubfolder uri
    let fileNamesInCurrDir = Directory.EnumerateFiles(directoryPath, e.FileNamePattern) |> Seq.toList
    let fileNamesInLibSubDir = Directory.EnumerateFiles(libDirectoryPath, e.FileNamePattern) |> Seq.toList
    let libMap = downloadLibMap fplLibUrl ad e
    let filesToDownload = findFilesInLibMapWithWildcard libMap e.FileNamePattern 
                            |> List.map (fun s -> fplLibUrl + "/" + s)
    FplSources(fileNamesInCurrDir @ fileNamesInLibSubDir @ filesToDownload)

/// computes an MD5 checksum of a string
let computeMD5Checksum (input: string) =
    let md5 = MD5.Create()
    let inputBytes = Encoding.ASCII.GetBytes(input)
    let hash = md5.ComputeHash(inputBytes)
    hash |> Array.map (fun b -> b.ToString("x2")) |> String.concat ""

   
let private getParsedAstsFromSources identifier (eani:EvalAliasedNamespaceIdentifier) (source: seq<string>) (getContent: string -> Diagnostics -> EvalAliasedNamespaceIdentifier -> string) =
    source
    |> Seq.choose (fun fileLoc ->
        let fileContent = getContent fileLoc ad eani
        if fileContent <> "" then
            Some { 
                UriPath = fileLoc
                FplCode = fileContent
                Ast = fplParser fileContent
                Checksum = computeMD5Checksum fileContent
                Id = identifier + 1
                ReferencingAsts = [identifier]
                ReferencedAsts = []
                EANI = eani 
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
                    Diagnostic.Message = { 
                        DiagnosticMessage.Value = getErroMsg (NSP003 alias.AliasOrStar)
                        }
                    Diagnostic.Code = { DiagnosticCode.Code = nameof(NSP003) }
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
let findParsedAstsMatchingAliasedNamespaceIdentifier (ident:int) ast (ad: Diagnostics) (uri: Uri) (fplLibUrl: string)  =
    let parsedAstsMatchingAliasedNamespaceIdentifier (eani:EvalAliasedNamespaceIdentifier) =
        let availableSources = acquireSources eani uri fplLibUrl ad
        if availableSources.NoneFound then 
            let msg = { DiagnosticMessage.Value = getErroMsg (NSP000 eani.FileNamePattern) }
            let code = { DiagnosticCode.Code = nameof(NSP000) }
            let diagnostic =
                { 
                    Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter 
                    Diagnostic.Severity = DiagnosticSeverity.Error
                    Diagnostic.StartPos = eani.StartPos
                    Diagnostic.EndPos = eani.EndPos
                    Diagnostic.Message = msg
                    Diagnostic.Code = code
                }
            ad.AddDiagnostic diagnostic
            []
        else
            let astsFromFilePaths = getParsedAstsFromSources ident eani availableSources.FilePaths loadFile
            let astsFromUrls = getParsedAstsFromSources ident eani availableSources.Urls downloadFile
            astsFromFilePaths @ astsFromUrls

    let eaniList = eval_uses_clause ast 
    findDuplicateAliases eaniList |> ignore

    eaniList 
    |> List.map (fun (eval:EvalAliasedNamespaceIdentifier) -> eval)
    |> List.collect parsedAstsMatchingAliasedNamespaceIdentifier


