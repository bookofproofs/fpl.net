module FplInterpreterUsesClause
open System.Text.RegularExpressions
open System.Net.Http
open System.IO
open FplGrammarTypes
open FplParser
open ErrDiagnostics
open FParsec

/// A record type to store all the necessary fields for parsed namespaces in FPL code
type ParsedAst =
    { ParsedAst: string * Ast }
    with
        member this.Name = fst this.ParsedAst  // first element of the tuple 
        member this.Ast = snd this.ParsedAst  // second element of the uple

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

// downloadFile "http://example.com/myfile.txt" "localfile.txt"
let downloadFile url filePath =
    let client = new HttpClient()
    try
        async {
            let! data = client.GetStringAsync(System.Uri(url)) |> Async.AwaitTask
            do! File.WriteAllTextAsync(filePath, data) |> Async.AwaitTask
        } |> Async.RunSynchronously
        printfn "File downloaded successfully."
    with
    | :? HttpRequestException as ex -> printfn "An error occurred while downloading the file: %s" ex.Message
    | :? IOException as ex -> printfn "An error occurred while writing to the file: %s" ex.Message
    | ex -> printfn "An unexpected error occurred: %s" ex.Message



let wildcardToRegex (wildcard : string) =
    "^" + Regex.Escape(wildcard).Replace("\\*", ".*").Replace("\\?", ".") + "$"

let findFilesWithWildcard filePath wildcard =
    let regexPattern = wildcardToRegex wildcard
    let regex = Regex(regexPattern, RegexOptions.IgnoreCase)
    File.ReadLines(filePath)
    |> Seq.filter regex.IsMatch
    |> Seq.toList

// Usage
let files = findFilesWithWildcard "sitemap.txt" "http://example.com/myfile*.txt"
files |> List.iter (printfn "%s")

/// Takes an AST, interprets it by extracting the uses clauses and looking for the files 
/// in the currentPath to be loaded and parsed to create even more ASTs.
/// Returns a list ParseAst objects or adds new diagnostics if the files were not found.
let tryFindAndParseUsesClauses ast (ad: Diagnostics) currentPath =
    let parseFile (e:EvalAliasedNamespaceIdentifier) =
        let fileNames = Directory.EnumerateFiles(currentPath, e.FileNamePattern)
        let fileNamesEmpty = fileNames |> Seq.tryFind (fun _ -> true) |> Option.isNone
        if fileNamesEmpty then
            let msg = DiagnosticMessage($"{e.FileNamePattern} not found")
            let code = DiagnosticCode("NSP000", "", msg.Value)
            let diagnostic =
                Diagnostic(DiagnosticEmitter.FplInterpreter, DiagnosticSeverity.Error, e.StartPos, msg, code)
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
                let code = DiagnosticCode("NSP000", msg.Value, msg.Value)
                let diagnostic =
                    Diagnostic(DiagnosticEmitter.FplInterpreter, DiagnosticSeverity.Error, e.StartPos, msg, code)
                ad.AddDiagnostic diagnostic
                None
        ) 
        |> Seq.toList

    eval_uses_clause ast 
    |> List.map (fun (eval:EvalAliasedNamespaceIdentifier) -> eval)
    |> List.collect parseFile


