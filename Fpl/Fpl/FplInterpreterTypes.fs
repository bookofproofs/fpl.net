module FplInterpreterTypes
open FParsec
open FplGrammarTypes

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

/// A record type to store all the necessary fields for parsed namespaces in FPL code
type ParsedAst =
    { 
        Id: string // id of this ast giving the order in which it was parsed with other asts
        mutable UriPath: string // source of the ast
        mutable FplSourceCode: string // source code of the ast
        mutable Ast: Ast // parsed ast
        mutable Checksum: string // checksum of the parsed ast
        mutable TopologicalSorting: int // an order in which the ParsedAsts have to be interpreted to avoid undeclared identifiers (undefined if a circle was caused by uses clauses)
        mutable ReferencingAsts: string list // list of asts "referencing" this one with a uses clause
        mutable ReferencedAsts: string list // list of asts "referenced" by this one in a uses clause
        mutable EANIList: EvalAliasedNamespaceIdentifier list // evaluated uses clauses found in the Ast 
        mutable Status: ParsedAstStatus
    }

type SymbolTable =
    { ParsedAsts: List<ParsedAst> }