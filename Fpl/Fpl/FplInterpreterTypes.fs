module FplInterpreterTypes
open System
open System.Text.RegularExpressions
open System.Security.Cryptography
open System.Collections.Generic
open System.Text
open System.IO
open FParsec
open FplGrammarTypes
open FplParser

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

type ParsedAstStatus = 
    | Loaded
    | UsesClausesEvaluated
    | Evaluated


type SortingProperties = 
    {
        mutable TopologicalSorting: int // an order in which the ParsedAsts have to be interpreted to avoid undeclared identifiers (undefined if a circle was caused by uses clauses)
        mutable ReferencingAsts: string list // list of asts "referencing" this one with a uses clause
        mutable ReferencedAsts: string list // list of asts "referenced" by this one in a uses clause
        mutable EANIList: EvalAliasedNamespaceIdentifier list // evaluated uses clauses found in the Ast 
    }
    with 
    member this.Reset() = 
        this.TopologicalSorting <- 0
        this.ReferencingAsts <- []
        this.ReferencedAsts <- []
        this.EANIList <- []

let computeMD5Checksum (input: string) =
    let md5 = MD5.Create()
    let inputBytes = Encoding.ASCII.GetBytes(input)
    let hash = md5.ComputeHash(inputBytes)
    hash |> Array.map (fun b -> b.ToString("x2")) |> String.concat ""

type ParsingProperties = 
    {
        mutable UriPath: string // source of the ast
        mutable FplSourceCode: string // source code of the ast
        mutable Ast: Ast // parsed ast
        mutable Checksum: string // checksum of the parsed ast
    }
    with
    member this.Reset (fplCode:string) (codeLoc:string) =
        let checksum = computeMD5Checksum fplCode
        if this.Checksum <> checksum then
            // if there ist a Parsed Ast with the same Name as the eani.Name 
            // and its checksum differs from the previous checksum 
            // then replace the ast, checksum, location, sourcecode, the 
            this.Ast <- fplParser fplCode
            this.UriPath <- codeLoc
            this.FplSourceCode <- fplCode
            this.Checksum <- checksum
            true
        else
            false

type FplBlockProperties = 
    {
        FplBlockIds: Dictionary<string,int>
    }
    with 
    member this.Reset() = 
        this.FplBlockIds.Clear()

/// A record type to store all the necessary fields for parsed namespaces in FPL code
type ParsedAst =
    { 
        Id: string // id of this ast giving the order in which it was parsed with other asts
        Parsing: ParsingProperties
        Sorting: SortingProperties
        FplBlocks: FplBlockProperties
        mutable Status: ParsedAstStatus
    }


/// A type that encapsulates the sources found for a uses clause 
/// and provides members to filter those from the file system and those from 
/// the web.
type FplSources(paths: string list) =
    member this.Paths = paths
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
    member this.Urls = List.filter this.IsUrl this.Paths
    member this.FilePaths = List.filter this.IsFilePath this.Paths
    member this.Length = this.Paths.Length
    member this.NoneFound = this.Paths.Length = 0

type EvalContext = 
    | ContextNone
    | InSignature of Positions

type SymbolTable =
    { 
        ParsedAsts: List<ParsedAst> 
        mutable EvaluationContext: EvalContext
        mutable Current: ParsedAst option
    }

