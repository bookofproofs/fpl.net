module FplInterpreterUsesClause
open System.Text.RegularExpressions
open System.Net.Http
open System.IO
open System
open FplGrammarTypes
open FplParser
open ErrDiagnostics
open FParsec

type FplInterpreterErrorCode =
    | NSP000 of string
    | NSP001 of string
    | NSP002 of string * string

let private getErroMsg = function  
    | NSP000 fileNamePattern -> sprintf "%s could not be loaded" fileNamePattern
    | NSP001 fileName -> sprintf "%s not found" fileName
    | NSP002 (url, innerErrMsg) -> sprintf "%s not downloadable: %s" url innerErrMsg


/// A record type to store all the necessary fields for parsed namespaces in FPL code
type ParsedAst =
    { ParsedAst: string * Ast }
    with
        member this.Name = fst this.ParsedAst  // first element of the tuple 
        member this.Ast = snd this.ParsedAst  // second element of the tuple

/// A record type to store all the necessary fields for parsed uses clauses in FPL code
type EvalAliasedNamespaceIdentifier = 
    { StartPos: Position
      EndPos: Position
      AliasOrStar: string option
      PascalCaseIdList: string list }
    with
        member this.FileNamePattern = 
            let pascalCaseIdList = String.concat "." this.PascalCaseIdList
            match this.AliasOrStar with
            | Some "*" -> 
                sprintf "%s*.fpl" pascalCaseIdList
            | _ -> 
                sprintf "%s.fpl" pascalCaseIdList
        member this.Name = 
            let pascalCaseIdList = String.concat "." this.PascalCaseIdList
            match this.AliasOrStar with
            | Some "*" -> 
                pascalCaseIdList
            | Some s -> 
                s
            | _ -> 
                pascalCaseIdList

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
        let aliasOrStar = match optAst with
                          | Some (Ast.Alias (_, s)) -> Some s
                          | Some Ast.Star -> Some "*"
                          | _ -> None
        let pascalCaseIdList = match ast with
                               | Ast.NamespaceIdentifier (_, asts) -> 
                                   asts |> List.collect (function Ast.PascalCaseId s -> [s] | _ -> [])
                               | _ -> []
        [{ EvalAliasedNamespaceIdentifier.StartPos = pos1 
           EvalAliasedNamespaceIdentifier.EndPos = pos2 
           EvalAliasedNamespaceIdentifier.AliasOrStar = aliasOrStar
           EvalAliasedNamespaceIdentifier.PascalCaseIdList = pascalCaseIdList }]
    | _ -> []

let private downloadFile url (ad: Diagnostics) pos =
    let client = new HttpClient()
    try
        async {
            let! data = client.GetStringAsync(Uri(url)) |> Async.AwaitTask
            return data
        } |> Async.RunSynchronously
    with
    | ex -> 
        let msg = DiagnosticMessage(getErroMsg (NSP002 (url, ex.Message)))
        let code = { DiagnosticCode = nameof(NSP002) }
        let diagnostic =
            { Diagnostic = (DiagnosticEmitter.FplInterpreter, DiagnosticSeverity.Error, pos, msg, code) }
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
    let unescapedPath = Uri.UnescapeDataString(uri.AbsolutePath)
    let directoryPath = Path.GetDirectoryName(unescapedPath)
    let libDirectoryPath = Path.Combine(directoryPath, "lib")
    if not <| Directory.Exists(libDirectoryPath) then
        Directory.CreateDirectory(libDirectoryPath) |> ignore
    (directoryPath, libDirectoryPath)


let downloadLibMap (currentWebRepo: string) (ad: Diagnostics) =
    let libMap = downloadFile (currentWebRepo + "/libmap.txt") ad
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
            true
        with
        | :? ArgumentException -> false
    member this.Urls = List.filter this.IsUrl this.Strings
    member this.FilePaths = List.filter this.IsFilePath this.Strings
    member this.Length = this.Strings.Length

let acquireSources (e:EvalAliasedNamespaceIdentifier) (uri: Uri) (fplLibUrl: string) (ad: Diagnostics) =
    let (directoryPath,libDirectoryPath) = createLibSubfolder uri
    let fileNamesInCurrDir = Directory.EnumerateFiles(directoryPath, e.FileNamePattern) |> Seq.toList
    let fileNamesInLibSubDir = Directory.EnumerateFiles(libDirectoryPath, e.FileNamePattern) |> Seq.toList
    let libMap = downloadLibMap fplLibUrl ad e.StartPos
    let filesToDownload = findFilesInLibMapWithWildcard libMap e.FileNamePattern 
                            |> List.map (fun s -> fplLibUrl + "/" + s)
    FplSources(fileNamesInCurrDir @ fileNamesInLibSubDir @ filesToDownload)

/// Checks if the pattern for a uses clause was found in some sources. If no sources were found
/// a diagnostic will be emitted and the function returns false, otherwise no diagnostics are emitted and 
/// and false is returned.
let private sourcesFound (e:EvalAliasedNamespaceIdentifier) sourcesList = 
    let listEmpty = sourcesList |> Seq.tryFind (fun _ -> true) |> Option.isNone
    if listEmpty then
        let msg = DiagnosticMessage(getErroMsg (NSP000 e.FileNamePattern))
        let code = { DiagnosticCode = nameof(NSP000) }
        let diagnostic =
            { Diagnostic = (DiagnosticEmitter.FplInterpreter, DiagnosticSeverity.Error, e.StartPos, msg, code) }
        ad.AddDiagnostic diagnostic
        false
    else
        true
    
/// Takes an `ast`, interprets it by extracting the uses clauses and looks for the files 
/// in the `currentPath` to be loaded and parsed to create even more ASTs.
/// Returns a list ParseAst objects or adds new diagnostics if the files were not found.
let tryFindAndParseUsesClauses ast (ad: Diagnostics) (uri: Uri) (fplLibUrl: string)  =
    let parseFile (e:EvalAliasedNamespaceIdentifier) =
        let availableSources = acquireSources e uri fplLibUrl ad
        if (sourcesFound e availableSources.Strings) then
            availableSources.Strings
            |> Seq.choose (fun fileName ->
                try
                    let fileContent = File.ReadAllText fileName
                    let ast = fplParser fileContent
                    Some { ParsedAst = (fileName, ast) }
                with
                | :? FileNotFoundException -> 
                    let msg = DiagnosticMessage(getErroMsg (NSP001 fileName))
                    let code = { DiagnosticCode = nameof(NSP001) }
                    let diagnostic =
                        { Diagnostic = (DiagnosticEmitter.FplInterpreter, DiagnosticSeverity.Error, e.StartPos, msg, code) }
                    ad.AddDiagnostic diagnostic
                    None
            ) 
            |> Seq.toList
        else    
            []

    eval_uses_clause ast 
    |> List.map (fun (eval:EvalAliasedNamespaceIdentifier) -> eval)
    |> List.collect parseFile


