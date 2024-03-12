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
    | NSP002 of string

let private interpreterErrorMsg = function  
    | NSP000 fileNamePattern -> sprintf "%s could not be loaded" fileNamePattern
    | NSP001 fileName -> sprintf "%s not found" fileName


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

let private downloadFile url (ad: Diagnostics) =
    let client = new HttpClient()
    try
        async {
            let! data = client.GetStringAsync(Uri(url)) |> Async.AwaitTask
            return data
        } |> Async.RunSynchronously
    with
    | :? HttpRequestException as ex -> 
        printfn "An error occurred while downloading the file: %s" ex.Message; ""
    | ex -> printfn "An unexpected error occurred: %s" ex.Message; ""



let private wildcardToRegex (wildcard : string) =
    "^" + Regex.Escape(wildcard).Replace("\\*", ".*").Replace("\\?", ".") + "$"

let private findFilesWithWildcard filePath wildcard =
    let regexPattern = wildcardToRegex wildcard
    let regex = Regex(regexPattern, RegexOptions.IgnoreCase)
    File.ReadLines(filePath)
    |> Seq.filter regex.IsMatch
    |> Seq.toList

// Usage
let files = findFilesWithWildcard "sitemap.txt" "http://example.com/myfile*.txt"
files |> List.iter (printfn "%s")

let private createLibSubfolder (uri: Uri) =
    let unescapedPath = Uri.UnescapeDataString(uri.AbsolutePath)
    let directoryPath = Path.GetDirectoryName(unescapedPath)
    let libDirectoryPath = Path.Combine(directoryPath, "lib")
    if not <| Directory.Exists(libDirectoryPath) then
        Directory.CreateDirectory(libDirectoryPath) |> ignore
    (directoryPath, libDirectoryPath)


let acquireFiles (e:EvalAliasedNamespaceIdentifier) (uri: Uri) (currentWebRepo: string) (ad: Diagnostics) =
    let (directoryPath,libDirectoryPath) = createLibSubfolder uri
    let fileNamesInCurrDir = Directory.EnumerateFiles(directoryPath, e.FileNamePattern)
    let fileNamesInLibSubDir = Directory.EnumerateFiles(libDirectoryPath, e.FileNamePattern)
    fileNamesInCurrDir |> Seq.toList


/// Takes an `ast`, interprets it by extracting the uses clauses and looks for the files 
/// in the `currentPath` to be loaded and parsed to create even more ASTs.
/// Returns a list ParseAst objects or adds new diagnostics if the files were not found.
let tryFindAndParseUsesClauses ast (ad: Diagnostics) (uri: Uri) =
    let parseFile (e:EvalAliasedNamespaceIdentifier) =
        let (directoryPath,libDirectoryPath) = createLibSubfolder uri
        let fileNames = Directory.EnumerateFiles(directoryPath, e.FileNamePattern)
        let fileNamesEmpty = fileNames |> Seq.tryFind (fun _ -> true) |> Option.isNone
        if fileNamesEmpty then
            let msg = DiagnosticMessage(interpreterErrorMsg (NSP000 e.FileNamePattern))
            let code = { DiagnosticCode = (nameof(NSP000), "", msg.Value) }
            let diagnostic =
                { Diagnostic = (DiagnosticEmitter.FplInterpreter, DiagnosticSeverity.Error, e.StartPos, msg, code) }
            ad.AddDiagnostic diagnostic
        fileNames
        |> Seq.choose (fun fileName ->
            try
                let fileContent = File.ReadAllText fileName
                let ast = fplParser fileContent
                Some { ParsedAst = (fileName, ast) }
            with
            | :? FileNotFoundException -> 
                let msg = DiagnosticMessage($"{fileName} not found")
                let code = { DiagnosticCode = ("NSP000", msg.Value, msg.Value) }
                let diagnostic =
                    { Diagnostic = (DiagnosticEmitter.FplInterpreter, DiagnosticSeverity.Error, e.StartPos, msg, code) }
                ad.AddDiagnostic diagnostic
                None
        ) 
        |> Seq.toList

    eval_uses_clause ast 
    |> List.map (fun (eval:EvalAliasedNamespaceIdentifier) -> eval)
    |> List.collect parseFile


