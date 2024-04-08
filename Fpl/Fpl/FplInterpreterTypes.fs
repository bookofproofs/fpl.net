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
    { StartPos: Position
      EndPos: Position
      AliasOrStar: string }

/// A record type to store all the necessary fields for parsed uses clauses in FPL code
type EvalAliasedNamespaceIdentifier =
    { StartPos: Position
      EndPos: Position
      EvalAlias: EvalAlias
      PascalCaseIdList: string list }

    member this.FileNamePattern =
        let pascalCaseIdList = String.concat "." this.PascalCaseIdList

        match this.EvalAlias.AliasOrStar with
        | "*" -> sprintf "%s*.fpl" pascalCaseIdList
        | _ -> sprintf "%s.fpl" pascalCaseIdList

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
    { mutable TopologicalSorting: int // an order in which the ParsedAsts have to be interpreted to avoid undeclared identifiers (undefined if a circle was caused by uses clauses)
      mutable ReferencingAsts: string list // list of asts "referencing" this one with a uses clause
      mutable ReferencedAsts: string list // list of asts "referenced" by this one in a uses clause
      mutable EANIList: EvalAliasedNamespaceIdentifier list } // evaluated uses clauses found in the Ast

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
    { mutable UriPath: string // source of the ast
      mutable FplSourceCode: string // source code of the ast
      mutable Ast: Ast // parsed ast
      mutable Checksum: string } // checksum of the parsed ast

    member this.Reset (fplCode: string) (codeLoc: string) =
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
    { FplBlockIds: Dictionary<string, int> }

    member this.Reset() = this.FplBlockIds.Clear()

/// A record type to store all the necessary fields for parsed namespaces in FPL code
type ParsedAst =
    { Id: string // id of this ast giving the order in which it was parsed with other asts
      Parsing: ParsingProperties
      Sorting: SortingProperties
      FplBlocks: FplBlockProperties
      mutable Status: ParsedAstStatus }


/// A type that encapsulates the sources found for a uses clause
/// and provides members to filter those from the file system and those from
/// the web.
type FplSources(paths: string list) =
    member this.Paths = paths

    member this.IsUrl(s: string) =
        let pattern = @"^https?://"
        Regex.IsMatch(s, pattern)

    member this.IsFilePath(s: string) =
        try
            Path.GetFullPath(s) |> ignore
            let pattern = @"^https?://"
            not (Regex.IsMatch(s, pattern))
        with :? ArgumentException ->
            false

    member this.Urls = List.filter this.IsUrl this.Paths
    member this.FilePaths = List.filter this.IsFilePath this.Paths
    member this.Length = this.Paths.Length
    member this.NoneFound = this.Paths.Length = 0




type FplType =
    | Object
    | Predicate
    | Template

type FplBlockType =
    | Variable
    | VariadicVariable
    | Expression
    | MandatoryProperty
    | OptionalProperty
    | Constructor
    | Class
    | Theorem
    | Lemma
    | Proposition
    | Corollary
    | Proof
    | Conjecture
    | Axiom
    | RuleOfInference
    | Premise
    | Conclusion
    | Predicate
    | FunctionalTerm
    | Theory
    | Root

type FplValue(name: string, blockType: FplBlockType, evalType: FplType, positions: Positions, parent: FplValue option) =
    let mutable _name = name
    let mutable _nameStartPos = Position("", 0, 1, 1)
    let mutable _nameEndPos = Position("", 0, 1, 1)
    let mutable _evalType = evalType
    let mutable _typeSignature = []
    let mutable _representation = ""
    let mutable _blockType = blockType
    let mutable _auxiliaryInfo = 0
    let _scope = System.Collections.Generic.Dictionary<string, FplValue>()

    /// Identifier of this FplValue that is unique in its scope
    member this.Name
        with get () = _name
        and set (value) = _name <- value

    /// Identifier of this FplValue that is unique in its scope
    member this.NameStartPos
        with get () = _nameStartPos
        and set (value) = _nameStartPos <- value

    /// Identifier of this FplValue that is unique in its scope
    member this.NameEndPos
        with get () = _nameEndPos
        and set (value) = _nameEndPos <- value

    /// Signature of this FplValue, for instance "predicate(object)"
    member this.TypeSignature
        with get () = _typeSignature
        and set (value:string list) = _typeSignature <- value

    /// Type of the FPL block with this FplValue
    member this.BlockType
        with get () = _blockType
        and set (value) = _blockType <- value

    /// The primary type of the FplValue
    member this.EvaluationType
        with get () = _evalType
        and set (value) = _evalType <- value

    /// A representation of the constructed object (if any)
    member this.FplRepresentation
        with get () = _representation
        and set (value) = _representation <- value

    /// An auxiliary storage that is used e.g. for remembering how many variables were declared when traversing the Ast recursively.
    member this.AuxiliaryInfo
        with get () = _auxiliaryInfo
        and set (value) = _auxiliaryInfo <- value

    /// Starting position of this FplValue
    member this.StartPos = fst positions
    /// Ending position of this FplValue
    member this.EndPos = snd positions
    /// Parent FplValue of this FplValue
    member this.Parent = parent
    /// A list of asserted predicates for this FplValue
    member this.AssertedPredicates = System.Collections.Generic.List<Ast>()
    /// A scope inside this FplValue
    member this.Scope = _scope

    /// A factory method for the evaluation of FPL theories
    static member CreateRoot() =
        new FplValue("", FplBlockType.Root, FplType.Predicate, (Position("", 0, 1, 1), Position("", 0, 1, 1)), None)

    /// Indicates if this FplValue is an FPL building block
    member this.IsFplBlock = 
        this.BlockType = FplBlockType.Axiom
        || this.BlockType = FplBlockType.Theorem 
        || this.BlockType = FplBlockType.Lemma 
        || this.BlockType = FplBlockType.Proposition 
        || this.BlockType = FplBlockType.Corollary 
        || this.BlockType = FplBlockType.Conjecture 
        || this.BlockType = FplBlockType.Proof 
        || this.BlockType = FplBlockType.RuleOfInference 
        || this.BlockType = FplBlockType.Predicate 
        || this.BlockType = FplBlockType.FunctionalTerm 
        || this.BlockType = FplBlockType.Class 

    /// Indicates if this FplValue is a variable
    member this.IsVariable = 
        this.BlockType = FplBlockType.Variable
        || this.BlockType = FplBlockType.VariadicVariable

    /// A factory method for the evaluation of Fpl class definitions
    static member CreateFplValue(positions: Positions, fplBlockType: FplBlockType, parent: FplValue) =
        match fplBlockType with
        | FplBlockType.Axiom
        | FplBlockType.Theorem
        | FplBlockType.Lemma
        | FplBlockType.Proposition
        | FplBlockType.Corollary
        | FplBlockType.Conjecture
        | FplBlockType.Premise
        | FplBlockType.Conclusion
        | FplBlockType.Proof
        | FplBlockType.RuleOfInference
        | FplBlockType.Expression
        | FplBlockType.Theory
        | FplBlockType.Predicate -> new FplValue("", fplBlockType, FplType.Predicate, positions, Some parent)
        | FplBlockType.Constructor
        | FplBlockType.FunctionalTerm
        | FplBlockType.Variable
        | FplBlockType.VariadicVariable
        | FplBlockType.MandatoryProperty
        | FplBlockType.OptionalProperty
        | FplBlockType.Class -> new FplValue("", fplBlockType, FplType.Object, positions, Some parent)
        | FplBlockType.Root -> raise (ArgumentException("Please use CreateRoot for creating the root instead"))

type EvalContext =
    | ContextNone
    | InTheory of FplValue
    | InSignature of FplValue
    | InBlock of FplValue

type SymbolTable =
    { ParsedAsts: List<ParsedAst>
      mutable CurrentContext: EvalContext
      Root: FplValue
    }
