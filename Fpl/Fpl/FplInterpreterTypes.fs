/// This module contains all types necessary to interpret FPL code (semantics)
module FplInterpreterTypes

open System
open System.Text.RegularExpressions
open System.Collections.Generic
open System.Text
open System.IO
open FParsec
open FplPrimitives
open FplGrammarTypes
open FplParser
open ErrDiagnostics
open FplInterpreterDiagnosticsEmitter
open Newtonsoft.Json

(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)

let toJson (keyValueList: string list) =
    match keyValueList with
    | [key; value] ->
        let dict = dict [ (key, value) ]
        JsonConvert.SerializeObject(dict, Formatting.None)
    | _ -> failwith "List must contain exactly two elements"

type EvalAlias =
    { StartPos: Position
      EndPos: Position
      AliasOrStar: string }

/// A record type to store all the necessary fields for parsed uses clauses in FPL code
type EvalAliasedNamespaceIdentifier =
    { StartPos: Position
      EndPos: Position
      EvalAlias: EvalAlias
      PascalCaseIdList: string list 
      DebugMode: bool}

    /// Creates an EvalAliasedNamespaceIdentifier with a string, alias or star, and positions.
    static member CreateEani(pascalCaseId: string, aliasOrStar: string, startPos, endPos, debugMode) =
        let evalAlias =
            { EvalAlias.StartPos = startPos
              EvalAlias.EndPos = endPos
              EvalAlias.AliasOrStar = aliasOrStar }

        { EvalAliasedNamespaceIdentifier.StartPos = startPos
          EvalAliasedNamespaceIdentifier.EndPos = endPos
          EvalAliasedNamespaceIdentifier.EvalAlias = evalAlias
          EvalAliasedNamespaceIdentifier.PascalCaseIdList = [ pascalCaseId ] 
          EvalAliasedNamespaceIdentifier.DebugMode = debugMode}

    /// Creates an EvalAliasedNamespaceIdentifier with a string list and a given EvalAlias and positions.
    static member CreateEani(pascalCaseIdList: string list, evalAlias: EvalAlias, startPos, endPos, debugMode) =
        { EvalAliasedNamespaceIdentifier.StartPos = startPos
          EvalAliasedNamespaceIdentifier.EndPos = endPos
          EvalAliasedNamespaceIdentifier.EvalAlias = evalAlias
          EvalAliasedNamespaceIdentifier.PascalCaseIdList = pascalCaseIdList 
          EvalAliasedNamespaceIdentifier.DebugMode = debugMode}

    /// Creates an EvalAliasedNamespaceIdentifier with a given Uri.
    static member CreateEani(uri: PathEquivalentUri, debugMode) =
        let pascalCaseId = uri.TheoryName
        let pos = Position("", 0, 1, 1)
        EvalAliasedNamespaceIdentifier.CreateEani(pascalCaseId, "*", pos, pos, debugMode)

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

    static member Create() =
        { SortingProperties.TopologicalSorting = 0
          SortingProperties.ReferencingAsts = []
          SortingProperties.ReferencedAsts = []
          SortingProperties.EANIList = [] }

/// A type that encapsulates the sources found for a uses clause
/// and provides members to filter those from the file system and those from
/// the web.
type FplSources(paths: PathEquivalentUri list, pathToLocalRegistry: string) =
    let _pathToLocalRegistry = pathToLocalRegistry
    /// All found paths for a uses clause, including those from the web.
    member this.Paths = paths

    /// Path to local copies of the registry.
    member this.PathToLocalRegistry = pathToLocalRegistry

    /// Returns all loaded FPL theories loaded by a uses clause grouped by name with lists of potential locations
    member this.Grouped =
        let fplTheories =
            this.Paths |> List.map (fun fp -> (Path.GetFileName fp.AbsolutePath, fp))

        fplTheories |> List.groupBy fst

    static member IsUrl(uri: PathEquivalentUri) =
        let pattern = "^https?:\/\/"
        Regex.IsMatch(uri.AbsoluteUri, pattern)

    static member IsFilePath(uri: PathEquivalentUri) =
        try
            Path.GetFullPath(uri.AbsoluteUri) |> ignore
            let pattern = "^https?:\/\/"
            not (Regex.IsMatch(uri.AbsoluteUri, pattern))
        with :? ArgumentException ->
            false

    member this.Urls = List.filter FplSources.IsUrl this.Paths
    member this.FilePaths = List.filter FplSources.IsFilePath this.Paths
    member this.Length = this.Paths.Length

    member this.GroupedWithPreferedSource =
        let result =
            let grouped = this.Grouped

            grouped
            |> List.collect (fun (fileName, paths) ->
                let pathType =
                    paths
                    |> List.map snd
                    |> List.tryFind (fun path ->
                        if
                            FplSources.IsFilePath(path)
                            && not (path.AbsolutePath.Contains("/lib/") || path.AbsolutePath.Contains(@"\lib\"))
                        then
                            true // the first source is the current directory
                        elif
                            FplSources.IsFilePath(path)
                            && (path.AbsolutePath.Contains("/lib/") || path.AbsolutePath.Contains(@"\lib\"))
                        then
                            true // the second is the lib subdirectory
                        else
                            true // the third is the Internet source
                    )

                let pathTypes =
                    List.map snd paths
                    |> List.map (fun path ->
                        if
                            FplSources.IsFilePath(path)
                            && (path.AbsolutePath.Contains("/lib/") || path.AbsolutePath.Contains(@"\lib\"))
                        then
                            "./lib"
                        elif FplSources.IsFilePath(path) then
                            "./"
                        else
                            "https")

                let theoryName = Path.GetFileNameWithoutExtension(fileName)

                match pathType with
                | Some path ->
                    let chosenPathType =
                        if
                            FplSources.IsFilePath(path)
                            && not (path.AbsolutePath.Contains("/lib/") || path.AbsolutePath.Contains(@"\lib\"))
                        then
                            "./"
                        elif
                            FplSources.IsFilePath(path)
                            && (path.AbsolutePath.Contains("/lib/") || path.AbsolutePath.Contains(@"\lib\"))
                        then
                            "./lib"
                        else
                            "https"

                    [ (fileName, path, chosenPathType, pathTypes, theoryName) ]
                | None -> [])

        result

    /// Checks if a filename has a pattern.
    static member HasPattern(fileName: string, pattern) =
        let wildcardToRegex (wildcard: string) =
            "^" + Regex.Escape(wildcard).Replace("\\*", ".*").Replace("\\?", ".") + "$"

        let regexPattern = wildcardToRegex pattern
        let regex = Regex(regexPattern, RegexOptions.IgnoreCase)
        regex.IsMatch(fileName)

    /// Finds all filenames in sources with a given pattern.
    member this.FindWithPattern(pattern: string) =
        this.GroupedWithPreferedSource
        |> List.filter (fun (fileName, _, _, _, _) -> FplSources.HasPattern(fileName, pattern))

type ParsingProperties =
    { mutable Uri: PathEquivalentUri // source of the ast
      mutable FplSourceCode: string // source code of the ast
      mutable Ast: Ast // parsed ast
      mutable Checksum: string } // checksum of the parsed ast

    /// Reset this ParsingProperties to its new location
    member this.Reset (fplCode: string) (uri: PathEquivalentUri) =
        let checksum = computeMD5Checksum fplCode

        if this.Checksum <> checksum then
            // if there is a Parsed Ast with the same Name as the eani.Name
            // and its checksum differs from the previous checksum
            // then replace the ast, checksum, location, source code, the
            this.Uri <- uri
            ad.ResetStream(uri)
            this.Ast <- fplParser fplCode
            this.FplSourceCode <- fplCode
            this.Checksum <- checksum
            true
        else
            false

    static member Create (fplCode: string) (uri: PathEquivalentUri) =
        ad.ResetStream(uri)

        { ParsingProperties.Uri = uri
          ParsingProperties.FplSourceCode = fplCode
          ParsingProperties.Ast = FplParser.fplParser fplCode
          ParsingProperties.Checksum = computeMD5Checksum fplCode }

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

/// A reference type to store a list of ParsedAsts
type ParsedAstList() =
    inherit System.Collections.Generic.List<ParsedAst>()
    let this = List<ParsedAst>()

    /// Finds some ParsedAst by identifier. Returns None if none was found.
    member this.TryFindAstById(identifier: string) =
        if this.Exists(fun pa -> pa.Id = identifier) then
            Some(this.Find(fun pa -> pa.Id = identifier))
        else
            None

    /// Finds some loaded ParsedAst. Returns None if none was found.
    member this.TryFindLoadedAst() =
        if this.Exists(fun pa -> pa.Status = ParsedAstStatus.Loaded) then
            Some(this.Find(fun pa -> pa.Status = ParsedAstStatus.Loaded))
        else
            None

    /// Returns a dictionary of <PathEquivalentUri,SourceCode> of all ParsedAsts in the list
    member this.DictionaryOfSUri2FplSourceCode() =
        let ret = System.Collections.Generic.Dictionary<PathEquivalentUri, string>()

        this
        |> Seq.iter (fun pa -> ret.TryAdd(pa.Parsing.Uri, pa.Parsing.FplSourceCode) |> ignore)

        ret

type FixType =
    | Infix of string * int
    | Postfix of string
    | Prefix of string
    | Symbol of string
    | NoFix

    member this.Type =
        match this with
        | Infix(symbol, precedence) -> sprintf "infix `%s` (with precedence `%i`)" symbol precedence
        | Postfix symbol -> sprintf "postfix `%s` " symbol
        | Prefix symbol -> sprintf "prefix `%s` " symbol
        | Symbol symbol -> sprintf "symbol `%s`" symbol
        | NoFix -> "no fix"

    member this.GetUserDefinedLiteral defaultSymbol =
        match this with
        | Infix(symbol, _) -> symbol 
        | Postfix symbol -> symbol
        | Prefix symbol -> symbol
        | Symbol symbol -> symbol
        | NoFix -> defaultSymbol

type SignatureType =
    | Name
    | Type
    | Mixed

/// Maximum number of calls allowed for an Fpl Node
let maxRecursion = 5

(*
    todo: 1) implement a function ToPL0Form transforming a predicative expresssion into a PL0 formula by replacing predicates with free pred variables
             possible applications: see 1a) 
    todo: 1a) implement a function ToTrueTable generating a true table out of ToPL0Form
             possible applications: see 2), 2a) 3) 4)
    todo: 2) implement a satifiability check to the Output of ToTrueTable
             possible applications: 
                issue error, if a formula of a theorem / axiom / conjecture is not satisfiable
                issue warning, if a subformula is not satisfiable to replace it by false
    todo: 2a) implement a tautology check to the output of ToTrueTable
             possible applications: 
                issue warning, if a formula of a theorem / axiom / conjecture is a tautology, because it could be replaced by a trivial true
                issue warning, if a subformula is a tautology to replace it by true
    todo: 3) implement a CanonicalDNF (disjunctive normal form) based on ToTrueTable with a sorted representation.
             possible applications:
                issue error, if in a proof there are two consecutive arguments aprev, anext whose outputs have the same ToTrueTables 
                    in terms of variables (its columns) that are not equivalent (have different rows)
    todo: 4) implement unit tests for all inference rules defined in Fpl.Commons checking if the respective premises and conclusions produce the same outputs of ToTrueTable.
             In this case, it is ensured that each inference rule in this library is a tautology. This is a required for 
             FPL to use inference rules as a Hilbert Calculus (see definition D.Hoffmann "Theoretische Informatik" 3rd. ed., p. 98)
             respectively inference rules with a premise being a predicate list: Here it is sufficient to check, if each rule 
             conserves the tautology property: If each predicate in a list is a tautology, so is the conclusion (see D.Hoffmann, "Theoretische Informatik", p. 104)
    todo: 5) ensure cleaned-up expressions by renaming variable with the same names in independent parts of the same formula expression.
             (see D.H. "Theoretische Informatik", 3rd. p. 119) 
             Implementation idea: This can be accomplished by moving the scope of variables declared in quantors to the containing FPL block, forcing renaming the variables by the end-user at coding-time of the formula. 
    todo: 6) issue error if arity-0 predicates are intrinsically defined, enforcing true or false (see D.H. "Theoretische Informatik", 3rd. p. 120) 
    todo: 7) write functions for normalizing predicative formulas (see D.H. "Th. Inf", 3rd. p. 122-123):
                NormalizeNeg - (uses cleaned-up expressions - see 5) replace impl, iif, xor, by and/or/not and move all negations from the beginning of non-atomar formula to its atomar sub formulas 
                NormalizePrenex - (uses the output of NormalizeNeg): move quantors from all subformulas the most outer quantor using fixed rules (see figure 3.35, p. 122)
                NormalizeSkolem - (uses the output of NormalizePrenex): eliminated all exists-quantors from the formula
                    there are two use cases: 
                        exists quantor is not proceeded by all quantors: - then just remove the ex quantor by replacing x <- u() with some intrinsic 0-ary function u()->tpl {intr} (some constant u fulfilling p)
                            for instance, 
                                ex x:tpl { p(x) } 
                            will be transformed to 
                                p(u())
                        exists quantor is proceeded by some all quantors: then remove the ex quantor by replacing x<-g_p(x1,x2) with some intrinsic n-ary function g(tpl1,tpl2)->tpl {intr} (some function fulfilling p, depending on proceeding variables bound by all quantors) 
                            for instance, 
                                all x1:tpl1, x2:tpl2, x:tpl {p(x1,x2,x)} 
                            will be transformed to  
                                all x1:tpl1, x2:tpl2 {p(x1,x2, g(x1, x2))} 
    todo: 8) write unit-test checking if FplValue.Type(SignatureType.Type) of expressions like p(u()) or all x1:tpl1, x2:tpl2 {p(x1,x2, g(x1, x2))} 
        includes full signatures of the functions u() and g(,), .i.e., including their mappings. This will later become necessary 
        to be able to recognize the satisfiability-equivalence of two NormalizeSkolem outputs (see 7)
        For the term "satisfiability-equivalence" see D.H. "Th. Inf", 3rd. p. 124
*)


type IVariable =
    abstract member IsSignatureVariable : bool with get, set
    abstract member IsInitialized : bool with get, set

[<AbstractClass>]
type FplValue(positions: Positions, parent: FplValue option) =
    let mutable _expressionType = FixType.NoFix
    let mutable _exprTypeAlreadySet = false
    let mutable _startPos = fst positions
    let mutable _endPos = snd positions
    let mutable _auxiliaryInfo = 0
    let mutable _arity = 0
    let mutable _fplId = ""
    let mutable _typeId = ""
    let mutable (_filePath: string option) = None
    let mutable _isIntrinsic = false

    let mutable _parent = parent
    let _scope = Dictionary<string, FplValue>()
    let _argList = List<FplValue>()
    let _valueList = List<FplValue>()

    /// A scope of this FplValue
    member this.Scope = _scope

    /// An argument list of this FplValue
    member this.ArgList = _argList

    /// ValueList of the FplValue.
    member this.ValueList = _valueList

    abstract member Clone: unit -> FplValue
    abstract member Copy : FplValue -> unit
    abstract member AssignParts: FplValue -> unit
    abstract member ShortName: string
    abstract member Name: string
    abstract member Represent: unit -> string

    /// An optional order in which this FplValue ist to be run after the symbol table is completely created.
    /// None means that it is not running but itself but called to be run from other FplValues.
    /// Some int means that it is running by itself after the creation of the symbol table. 
    /// Only theories in root, and axioms, theorems, lemmas, propositions, conjectures, and definitions of predicates and functional terms in theories run by themself and call all other types of FplValue to run.
    abstract member RunOrder: int option

    /// Generates a type string identifier or type-specific naming convention of this FplValue.
    abstract member Type: SignatureType -> string

    /// Embeds this FplValue in the SymbolTable by adding it to the Scope or as an argument of its predecessor in the SymbolTable.
    abstract member EmbedInSymbolTable: FplValue option -> unit

    /// Abstract member for running this FplValue. It has None or Some optional FplVariableStack as paramter.
    abstract member Run: FplVariableStack -> unit

    /// Indicates if this FplValue is an FPL building block.
    abstract member IsFplBlock: unit -> bool

    /// Indicates if this FplValue is an FPL building block, a property, or a constructor.
    abstract member IsBlock: unit -> bool

    /// Indicates if this FplValue is a class.
    abstract member IsClass: unit -> bool

    /// Indicates if this FplValue is a proof.
    abstract member IsProof: unit -> bool

    /// Indicates if this FplValue is a mapping.
    abstract member IsMapping: unit -> bool

    /// Clears the ValueList and adds the argument to it. Previous value(s), if any, get lost.
    abstract member SetValue: FplValue -> unit

    /// Clears the ValueList and adds the argument to it. Previous value(s), if any, get lost.
    abstract member SetValuesOf: FplValue -> unit

    /// A method used to issue diagnostics related to this FplValue and its structure retrieved during the FPL interpreter.
    abstract member CheckConsistency: unit -> unit

    (* Default implementations = everything is false, only the trues are overridden in derived classes *)
    default this.CheckConsistency() = ()
    
    default this.SetValue fv =
        this.ValueList.Clear()
        this.ValueList.Add(fv)

    default this.SetValuesOf fv =
        this.ValueList.Clear()
        if fv.ValueList.Count = 0 then
            // if fv has no values then it should be the value itself
            this.ValueList.Add(fv)
        else
            this.ValueList.AddRange(fv.ValueList)

    default this.IsFplBlock () = false
    default this.IsBlock () = false
    default this.IsClass () = false
    default this.IsProof () = false
    default this.IsMapping () = false
    
    default this.AssignParts (ret:FplValue) =
        ret.FplId <- this.FplId
        ret.TypeId <- this.TypeId
        ret.Arity <- this.Arity
        ret.AuxiliaryInfo <- this.AuxiliaryInfo
        ret.IsIntrinsic <- this.IsIntrinsic
        ret.ExpressionType <- this.ExpressionType

        this.Scope
        |> Seq.iter (fun (kvp:KeyValuePair<string, FplValue>) ->
            let value = kvp.Value.Clone()
            ret.Scope.Add(kvp.Key, value))

        this.ArgList
        |> Seq.iter (fun (fv1:FplValue) ->
            let value = fv1.Clone()
            ret.ArgList.Add(value))

        this.ValueList
        |> Seq.iter (fun (fv1:FplValue) ->
            let value = fv1.Clone()
            ret.ValueList.Add(value))

    /// TypeId of the FplValue.
    member this.TypeId
        with get () = _typeId
        and set (value) = _typeId <- value

    /// FplId of the FplValue.
    member this.FplId
        with get () = _fplId
        and set (value) = _fplId <- value

    /// FilePath of the FplValue.
    member this.FilePath
        with get () = _filePath
        and set (value) = _filePath <- value

    /// Type of the Expr
    member this.ExpressionType
        with get () = _expressionType
        and set (value) =
            if not _exprTypeAlreadySet then
                _expressionType <- value
                _exprTypeAlreadySet <- true
            else
                raise (
                    ArgumentException(
                        $"Type was already initialized with `{_expressionType.Type}`, cannot set it again with {value.Type}."
                    )
                )

    /// Starting position of this FplValue
    member this.StartPos
        with get () = _startPos
        and set (value) = _startPos <- value

    /// This FplValue's name's end position that can be different from its ending position
    member this.EndPos
        with get () = _endPos
        and set (value) = _endPos <- value

    /// An auxiliary storage that is used e.g. for remembering how many variables were declared when traversing the Ast recursively.
    member this.AuxiliaryInfo
        with get () = _auxiliaryInfo
        and set (value) = _auxiliaryInfo <- value

    /// An arity of this FplValue
    member this.Arity
        with get () = _arity
        and set (value) = _arity <- value

    /// Parent FplValue of this FplValue
    member this.Parent
        with get () = _parent
        and set (value) = _parent <- value

    /// Indicates if this FplValue is an intrinsically defined block
    member this.IsIntrinsic
        with get () = _isIntrinsic
        and set (value) = _isIntrinsic <- value

    /// Create a (possibly empty) list of all variables in the scope of this FplValue.
    member this.GetVariables() =
        this.Scope.Values
        |> Seq.filter (fun fv -> 
            fv.Name = PrimVariableL 
            || fv.Name = PrimVariableArrayL 
        )
        |> Seq.toList

    /// Create a (possibly empty) list of all properties in the scope of this FplValue.
    member this.GetProperties() =
        this.Scope.Values
        |> Seq.filter (fun fv -> 
            fv.Name = PrimMandatoryFunctionalTermL 
            || fv.Name = PrimMandatoryPredicateL 
        )
        |> Seq.toList

    /// Copies other FplValue to this one without changing its reference pointer.
    default this.Copy(other: FplValue) =
        this.FplId <- other.FplId

        this.TypeId <- other.TypeId
        this.Arity <- other.Arity
        this.AuxiliaryInfo <- other.AuxiliaryInfo
        this.IsIntrinsic <- other.IsIntrinsic

        this.Scope.Clear()
        other.Scope |> Seq.iter (fun kvp -> this.Scope.Add(kvp.Key, kvp.Value))

        this.ArgList.Clear()
        this.ArgList.AddRange(other.ArgList)

        this.ValueList.Clear()
        this.ValueList.AddRange(other.ValueList)

    /// Qualified starting position of this FplValue
    member this.QualifiedStartPos =
        let rec getFullName (fv: FplValue) (first: bool) =
            let fvType = fv.Type(SignatureType.Mixed)

            if fv.ShortName = PrimRoot then ""
            elif first then
                let starPosWithoutFileName =
                    $"(Ln: {fv.StartPos.Line}, Col: {fv.StartPos.Column})"

                if fv.ShortName = PrimTheory then
                    getFullName fv.Parent.Value false + fvType + starPosWithoutFileName
                else
                    getFullName fv.Parent.Value false + starPosWithoutFileName
            else if fv.ShortName = PrimTheory then
                getFullName fv.Parent.Value false + fvType
            else
                getFullName fv.Parent.Value false

        getFullName this true

    /// Calculcates this FplValue's ultimate block node (if such exists).
    /// The ultimate block node is the FPL building block's FplValue enclosing this FplValue (if such extists)
    member this.UltimateBlockNode = 
        let rec ultimateBlockNode (node:FplValue) =
            match node.Parent with
            | Some parent ->
                match parent.Name with
                | PrimRoot -> None
                | PrimTheoryL -> Some node
                | _ ->
                    ultimateBlockNode parent
            | None -> None
        ultimateBlockNode this

    /// Calculcates this FplValue's ultimate block node (if such exists).
    /// The next block node is either an FPL property (if such exists) 
    /// or the Fpl building block's FplValue enclosing this FplValue (if such exists).
    member this.NextBlockNode = 
        let rec nextBlockNode (node:FplValue) =
            match node.Name with 
            | LiteralCtorL
            | PrimMandatoryFunctionalTermL
            | PrimMandatoryPredicateL ->
                Some node
            | _ ->
                match node.Parent with
                | Some parent ->
                    match parent.Name with
                    | PrimRoot -> None
                    | PrimTheoryL -> Some node
                    | _ ->
                        nextBlockNode parent
                | None -> None
        nextBlockNode this

    /// Provides a name with an english article (an or a).
    member this.EnglishName = getEnglishName this.Name 

    /// Checks if a block named name is in the scope of the fplValue' parent.
    member this.InScopeOfParent name =
        let conflictInSiblingTheory (parent: FplValue) =
            // if the parent is a theory, look also for its sibling theories
            let (conflicts: ScopeSearchResult list) =
                let root = parent.Parent.Value

                root.Scope
                |> Seq.filter (fun siblingTheory ->
                    // look only for sibling theories
                    siblingTheory.Value <> parent)
                |> Seq.choose (fun siblingTheory ->
                    if siblingTheory.Value.Scope.ContainsKey(name) then
                        let foundConflict = siblingTheory.Value.Scope[name]
                        Some(ScopeSearchResult.Found foundConflict)
                    else
                        None)
                |> Seq.toList

            let res = conflicts

            if res.Length > 0 then
                conflicts.Head
            else
                ScopeSearchResult.NotFound

        match this.Parent with
        | Some parent ->
            if parent.Scope.ContainsKey(name) then
                let foundConflict = parent.Scope[name]
                ScopeSearchResult.Found foundConflict
            else if parent.ShortName = PrimTheory then
                conflictInSiblingTheory parent
            else
                ScopeSearchResult.NotFound
        | None -> ScopeSearchResult.NotApplicable


    member this.Debug (caller:string) =
        let rec getPath (fv:FplValue) =
            match fv.Parent with 
            | Some parent -> $"{getPath parent}>{fv.ShortName} {fv.Type SignatureType.Name}"
            | None -> $"{fv.ShortName}"
        if TestSharedConfig.TestConfig.DebugMode then 
            let logLine = $"{caller}:{getPath this}{Environment.NewLine}"
            let currDir = Directory.GetCurrentDirectory()
            File.AppendAllText(Path.Combine(currDir, "Debug.txt"), logLine)

/// A discriminated union type for wrapping search results in the Scope of an FplValue.
and ScopeSearchResult =
    | FoundAssociate of FplValue
    | FoundMultiple of string
    | FoundIncorrectBlock of string
    | Found of FplValue
    | NotFound
    | NotApplicable

/// This type implements the functionality needed to "run" FPL statements step-by-step
/// while managing the storage of variables and other evaluation-related information.
/// FPL uses a call-by-value approach when it comes to 
/// replacing parameters by a calling function with arguments.
and FplVariableStack() = 
    let mutable _inSignatureEvaluation = false
    let _stack = Stack<KeyValuePair<string, Dictionary<string,FplValue>>>()
    let _valueStack = Stack<FplValue>()
    let _assumedArguments = Stack<FplValue>()
    // positions of the caller to prevent some diagnostics of being shown at the wrong position 
    let mutable _callerStartPos = Position("", 0,0,0)
    let mutable _callerEndPos = Position("", 0,0,0)

    let mutable _nextRunOrder = 0


    /// Starting position of the caller
    member this.CallerStartPos
        with get () = _callerStartPos
        and set (value) = _callerStartPos <- value

    /// End position of the caller 
    member this.CallerEndPos
        with get () = _callerEndPos
        and set (value) = _callerEndPos <- value


    /// Returns the next available RunOrder to be stored, when inserting an FplValue into its parent.
    /// The need for this functionality is that sometimes, the block is inserted into the parent's scope, which is a dictionary.
    /// When running the nodes in the dictionary, their run order will ensure that they are being run in the the order they have bin inserted.
    /// This order is incremented and stored when specific FplValue when they are created.
    /// All FplValues can have either Some or None RunOrder.
    /// Those with Some RunOrder include e.g. the following building blocks: axioms, theorems, lemmas, propositions, proofs, corollaries, arguments in proofs.
    /// Those with None include all other types of FplValues. They do not run by their own. They are "called" by those with Some RunOrder.
    member this.GetNextAvailableFplBlockRunOrder = 
        _nextRunOrder <- _nextRunOrder + 1
        _nextRunOrder
    
    /// Indicates if this EvalStack is evaluating a signature on a FPL building block
    member this.InSignatureEvaluation
        with get () = _inSignatureEvaluation
        and set (value) = _inSignatureEvaluation <- value
        
    // The stack memory of the runner to store the variables of all run programs
    member this.Stack = _stack

    /// Copy the ValueList of the variadic ar to the ValueList of the variadic p
    /// by removing the previous values (if any) and
    /// inserting the clones of the elements.
    member this.ReplaceVariables (parameters:FplValue list) (arguments:FplValue list) =
        let replaceValues (p:FplValue) (ar:FplValue)  =
            (p.ValueList:List<FplValue>).Clear()
            let valueList = 
                match ar.Name with 
                | PrimRefL when ar.FplId = String.Empty ->
                    ar.ArgList |> Seq.toList
                | PrimRefL when ar.Scope.ContainsKey(ar.FplId) ->
                    ar.Scope.Values |> Seq.toList
                | _ -> ar.ValueList |> Seq.toList
                
            valueList
            |> List.iter (fun (fv:FplValue) ->
                match box fv with 
                | :? IVariable as var ->
                    p.SetValuesOf fv
                    if fv.ValueList.Count > 0 then 
                        var.IsInitialized <- true
                | _ ->
                    let fvClone = fv.Clone()
                    p.SetValue fvClone
                    match box p with
                    | :? IVariable as var -> var.IsInitialized <- true
                    | _ -> ()
            )

        let rec replace (pars:FplValue list) (args: FplValue list) = 
            match (pars, args) with
            | (p::ps, ar::ars) ->
                match p.Name , ar.Name with
                // p is variadic, ar is variadic 
                | PrimVariableArrayL, PrimVariableArrayL ->
                    replaceValues p ar
                    // continue replacing variables with the remaining lists
                    replace ps ars
                // p is variadic, ar is anything
                | PrimVariableArrayL, _ ->
                    replaceValues p ar              
                    // continue replacing variables with the original pars and the remaining ars list
                    replace pars ars
                // p is not variadic, ar is variadic 
                | PrimVariableL, PrimVariableArrayL -> ()
                 // p is not variadic, ar is anything but variadic 
                | PrimVariableL, _ ->
                    // otherwise, simply assign the argument's representation to the parameter's representation
                    replaceValues p ar
                    // continue replacing variables with the remaining lists
                    replace ps ars
                | _ , _ -> ()
            | (p::ps, []) -> ()
            | ([], ar::ars) -> ()
            | ([], []) -> ()
        replace parameters arguments

    /// Saves the clones (!) of the original scope variables of an FplValue block as a KeyValuePair to a stack memory.
    /// where the key is the block's FplId and the value is a dictionary of all scope variables.
    /// Returns a list of parameters of the called FplValue, i.e. its signature variables.
    /// Since the block's FplId is unique in the scope, all variables are stored in a separate scope.
    member this.SaveVariables (called:FplValue) = 
        // now process all scope variables and push by replacing them with their clones
        // and pushing the originals on the stack
        let toBeSavedScopeVariables = Dictionary<string, FplValue>()
        let pars = List<FplValue>()
        let vars = called.GetVariables()
        vars 
        |> List.iter (fun parOriginal -> 
            // save the clone of the original parameter variable
            let parClone = parOriginal.Clone()
            toBeSavedScopeVariables.Add(parOriginal.FplId, parClone)
            match box parOriginal with 
            | :? IVariable as parOrig when parOrig.IsSignatureVariable ->
                pars.Add(parOriginal)
            | _ -> ()
        )
        let kvp = KeyValuePair(called.FplId,toBeSavedScopeVariables)
        _stack.Push(kvp)
        pars |> Seq.toList

    /// Restores the scope variables of an FplValue block from the stack.
    member this.RestoreVariables (fvPar:FplValue) = 
        let blockVars = _stack.Pop()
        blockVars.Value
        |> Seq.iter (fun kvp -> 
            let orig = (fvPar.Scope:Dictionary<string, FplValue>)[kvp.Key] 
            orig.Copy(kvp.Value)
        )

    member this.EvalStack = _valueStack

    // Pops an FplValue from stack without propagating it's name and signature to the next FplValue on the stack.
    member this.Pop() = _valueStack.Pop()

    // Pops an FplValue from stack and propagates it's name and signature to the next FplValue on the stack.
    member this.PopEvalStack() = 
        let fv = _valueStack.Pop()
        if _valueStack.Count > 0 then
            let next = _valueStack.Peek()
            fv.EmbedInSymbolTable (Some next) 

    // Pushes an FplValue to the stack.
    member this.PushEvalStack fv = _valueStack.Push fv

    // Peeks an FplValue from the stack.
    member this.PeekEvalStack() = _valueStack.Peek()

    member this.LastAssumedArgument =
        if _assumedArguments.Count > 0 then
            Some (_assumedArguments.Peek())
        else 
            None

    member this.AssumeArgument assumption = _assumedArguments.Push (assumption)
    
    member this.RevokeLastArgument() = if _assumedArguments.Count > 0 then _assumedArguments.Pop() |> ignore

    // Clears stack.
    member this.ClearEvalStack() = 
        _valueStack.Clear()
        _assumedArguments.Clear()
        _stack.Clear()

// Create an FplValue list containing all Scopes of an FplNode
let rec flattenScopes (root: FplValue) =
    let rec helper (node: FplValue) (acc: FplValue list) =
        let newAcc = node :: acc
        node.Scope |> Seq.fold (fun acc kvp -> helper kvp.Value acc) newAcc

    helper root []

let stripLastDollarDigit (s: string) =
    let lastIndex = s.LastIndexOf('$')
    if lastIndex <> -1 then s.Substring(0, lastIndex) else s

let isVar (fv1:FplValue) =
    match fv1.Name with
    | PrimVariableL
    | PrimVariableArrayL -> true
    | _ -> false
    
let isSignatureVar (fv1:FplValue) = 
    match box fv1 with 
    | :? IVariable as var when var.IsSignatureVariable -> true
    | _ -> false

/// Qualified name of this FplValue
let qualifiedName (fplValue:FplValue)=
    let rec getFullName (fv: FplValue) (first: bool) =
        let fplValueType =
            match fv.Name with
            | LiteralLocL
            | PrimExclusiveOr 
            | PrimConjunction
            | PrimDisjunction 
            | PrimNegation
            | PrimImplication
            | PrimEquivalence 
            | PrimIsOperator 
            | PrimExtensionObj 
            | PrimDelegateEqualL 
            | PrimDelegateDecrementL 
            | PrimRefL -> fv.Type(SignatureType.Name)
            | LiteralCtorL
            | PrimBaseConstructorCall
            | PrimDefaultConstructor
            | PrimQuantorAll
            | PrimQuantorExists
            | PrimQuantorExistsN
            | PrimClassL
            | PrimPredicateL
            | PrimFuncionalTermL
            | PrimMandatoryPredicateL
            | PrimMandatoryFunctionalTermL -> fv.Type(SignatureType.Mixed)
            | _ -> fv.FplId

        match fv.Name with
        | PrimRoot -> ""
        | _ -> 


            if first then
                if fv.Parent.Value.Name = PrimRoot then
                    getFullName fv.Parent.Value false + fplValueType
                else if (isVar fv) && not (isVar fv.Parent.Value) then
                    fplValueType
                else
                    getFullName fv.Parent.Value false + "." + fplValueType
            elif fv.Parent.Value.Name = PrimRoot then
                getFullName fv.Parent.Value false + fplValueType
            elif (isVar fv) && not (isVar fv.Parent.Value) then
                fplValueType
            else
                getFullName fv.Parent.Value false + "." + fplValueType

    $"{fplValue.EnglishName} {getFullName fplValue true}"


/// Checks if an fv is provable. This will only be true if
/// it is a theorem, a lemma, a proposition, or a corollary
let isProvable (fv: FplValue) =
    match fv.Name with
    | LiteralThmL
    | LiteralLemL
    | LiteralPropL
    | LiteralCorL -> true
    | _ -> false

/// Checks if an fplValue is a conjecture or an axiom. This is used to decide whether or
/// not it is not provable.
let isAxiomOrConnjecture (fv:FplValue) = 
    match fv.Name with
    | LiteralConjL 
    | LiteralAxL -> true
    | _ -> false

let private getFplHead (fv:FplValue) (signatureType:SignatureType) =
    match signatureType with
            | SignatureType.Name 
            | SignatureType.Mixed -> fv.FplId
            | SignatureType.Type -> fv.TypeId

let private propagateSignatureType (signatureType:SignatureType) =
    match signatureType with
    | SignatureType.Mixed -> SignatureType.Type
    | _ -> signatureType 

/// Generates a string of parameters based on SignatureType
let private getParamTuple (fv:FplValue) (signatureType:SignatureType) =
        let propagate = propagateSignatureType signatureType
        fv.Scope
        |> Seq.filter (fun (kvp: KeyValuePair<string, FplValue>) ->
            isSignatureVar kvp.Value
            || (isVar fv) && not (kvp.Value.IsClass())
            || fv.IsMapping())
        |> Seq.map (fun (kvp: KeyValuePair<string, FplValue>) -> kvp.Value.Type(propagate))
        |> String.concat ", "

type FplTheory(theoryName, parent: FplValue, filePath: string, runOrder) as this =
    inherit FplValue((Position("",0,1,1), Position("",0,1,1)), Some parent)
    let _runOrder = runOrder

    do
        this.FilePath <- Some filePath
        this.FplId <- theoryName
        this.TypeId <- theoryName

    override this.Name = PrimTheoryL
    override this.ShortName = PrimTheory

    override this.Clone () =
        let ret = new FplTheory(this.FplId, this.Parent.Value, this.FilePath.Value, _runOrder)
        this.AssignParts(ret)
        ret

    override this.Type signatureType =
        match signatureType with
        | SignatureType.Name 
        | SignatureType.Mixed -> this.FplId
        | SignatureType.Type -> this.TypeId

    override this.Represent () = LiteralUndef

    /// The RunOrder in which this theory is to be executed.
    override this.RunOrder = Some _runOrder

    override this.EmbedInSymbolTable _ = 
        let next = this.Parent.Value
        // name conflicts of theories do not occur because of *.fpl file management 
        // and file-names being namespace names
        next.Scope.TryAdd(this.FplId, this) |> ignore

    /// Returns all Fpl Building Blocks that run on their own in this theory ordered by their RunOrder ascending.
    /// Only some of the building block run on their own in the theory, including axioms, theorems, lemmas, propositions, and conjectures.
    /// All other building blocks (e.g. rules of inferences, definitions of classes, etc.) are run when called by the first type of blocks.
    /// The RunOrder is set when creating the FplTheory during the parsing of the AST.
    member private this.OrderedBlocksRunningByThemselves =
        this.Scope.Values
        |> Seq.choose (fun block ->
            match block.RunOrder with
            | Some _ -> Some block
            | _ -> None)
        |> Seq.sortBy (fun block -> block.RunOrder.Value) 
        |> Seq.toList

    override this.Run variableStack = 
        this.Debug "Run"
        let blocks = this.OrderedBlocksRunningByThemselves
        blocks
        |> Seq.iter (fun block -> block.Run variableStack)        

/// Indicates if an FplValue is the root of the SymbolTable.
let isTheory (fv:FplValue) = 
    match fv with
    | :? FplTheory -> true
    | _ -> false

type FplRoot() =
    inherit FplValue((Position("", 0, 1, 1), Position("", 0, 1, 1)), None)
    override this.Name = PrimRoot
    override this.ShortName = PrimRoot

    override this.Clone () =
        let ret = new FplRoot()
        this.AssignParts(ret)
        ret

    override this.Type _ = String.Empty
    override this.Represent () = LiteralUndef

    override this.EmbedInSymbolTable _ = () 

    /// Returns all theories in the scope of this root ordered by their discovery time (parsing of the AST).
    /// This means that the theory with the lowest RunOrder comes first.
    member this.OrderedTheories =
        this.Scope.Values
        |> Seq.choose (fun item ->
            match item with
            | :? FplTheory as theory -> Some theory
            | _ -> None)
        |> Seq.sortBy (fun th -> th.RunOrder.Value) 

    override this.RunOrder = None

    override this.Run variableStack = 
        this.Debug "Run"
        this.OrderedTheories
        |> Seq.iter (fun theory -> theory.Run variableStack)        

// Returns the root node of any FplValue
let rec getRoot (fv:FplValue) =
    if fv.Name = PrimRoot then 
        fv :?> FplRoot
    else getRoot fv.Parent.Value
   
// Tries to add for statatement's domain or entity to its parent's for statement
let tryAddToParentForInStmt (fplValue:FplValue) =
    let identifier = fplValue.Type SignatureType.Name
    let parent = fplValue.Parent.Value

    if parent.ArgList.Count = 1 then
        let entityIdentifier = parent.ArgList[0].Type SignatureType.Name
        if entityIdentifier = identifier then 
            emitID027Diagnostics identifier fplValue.StartPos fplValue.EndPos
        else 
            parent.ArgList.Add fplValue
    else
        parent.ArgList.Add fplValue

// Tries to add an FPL block to its parent's scope using its FplId, or issues ID001 diagnostics if a conflict occurs
let tryAddToParentUsingFplId (fplValue:FplValue) =
    let identifier = fplValue.FplId
    let root = getRoot fplValue
    let conflicts = 
        root.OrderedTheories
        |> Seq.map (fun theory -> 
            theory.Scope
            |> Seq.filter (fun kvp -> kvp.Key = identifier)
            |> Seq.map (fun kvp -> kvp.Value)
        )
        |> Seq.concat
        |> Seq.toList

    if conflicts.Length > 0 then 
        emitID001Diagnostics identifier (conflicts.Head.QualifiedStartPos) fplValue.StartPos fplValue.EndPos
    else
        let parent = fplValue.Parent.Value
        parent.Scope.Add(identifier, fplValue)

// Tries to add an FPL block to its parent's scope using its mixed signature, or issues ID001 diagnostics if a conflict occurs
let tryAddToParentUsingMixedSignature (fplValue:FplValue) =
    let identifier = fplValue.Type SignatureType.Mixed
    let root = getRoot fplValue
    let conflicts = 
        root.OrderedTheories
        |> Seq.map (fun theory -> 
            theory.Scope
            |> Seq.filter (fun kvp -> kvp.Key = identifier)
            |> Seq.map (fun kvp -> kvp.Value)
        )
        |> Seq.concat
        |> Seq.toList

    if conflicts.Length > 0 then 
        emitID001Diagnostics identifier (conflicts.Head.QualifiedStartPos) fplValue.StartPos fplValue.EndPos
    else
        let parent = fplValue.Parent.Value
        parent.Scope.Add(identifier, fplValue)

// Tries to add a constructor or property to it's parent FPL block's scope using its mixed signature, or issues ID001 diagnostics if a conflict occurs
let tryAddSubBlockToFplBlock (fplValue:FplValue) =
    let identifier = fplValue.Type SignatureType.Mixed
    let parent = fplValue.Parent.Value
    if parent.Scope.ContainsKey(identifier) then 
        emitID001Diagnostics identifier (parent.Scope[identifier].QualifiedStartPos) fplValue.StartPos fplValue.EndPos
    else
        parent.Scope.Add(identifier, fplValue)   

// Tries to add an FPL block to its parent's scope using its typed signature, or issues ID001 diagnostics if a conflict occurs
let tryAddToParentUsingTypedSignature (fplValue:FplValue) =
    let identifier = fplValue.Type SignatureType.Type
    let root = getRoot fplValue
    let conflicts = 
        root.OrderedTheories
        |> Seq.map (fun theory -> 
            theory.Scope.Values
            |> Seq.filter (fun fv -> fv.Type SignatureType.Type = identifier)
        )
        |> Seq.concat
        |> Seq.toList

    if conflicts.Length > 0 then 
        let oldDiagnosticsStopped = ad.DiagnosticsStopped 
        ad.DiagnosticsStopped <- false
        emitID024Diagnostics identifier (conflicts.Head.QualifiedStartPos) fplValue.StartPos fplValue.EndPos
        ad.DiagnosticsStopped <- oldDiagnosticsStopped
    else
        let parent = fplValue.Parent.Value
        parent.Scope.Add(identifier, fplValue)

// Adds an expression to Parent's argument list
let addExpressionToParentArgList (fplValue:FplValue) =
    let parent = fplValue.Parent.Value
    match parent.Name with 
    | LiteralLocL ->
        let identifier = fplValue.FplId
        parent.FplId <- identifier
        parent.TypeId <- identifier
    | _ -> ()
    parent.ArgList.Add fplValue

// Add an expression to a reference
let addExpressionToReference (fplValue:FplValue) =
    let nextOpt = fplValue.Parent
    match nextOpt with
    | Some next when next.Name = PrimRefL && next.Scope.ContainsKey(".") -> ()
    | Some next when next.Name = PrimRefL && next.Scope.ContainsKey(next.FplId) ->
        let referenced = next.Scope.Values |> Seq.head
        match referenced.Name with 
        | PrimVariableArrayL ->
            next.ArgList.Add fplValue
        | _ ->
            next.FplId <- fplValue.FplId
            next.TypeId <- fplValue.TypeId
            next.Scope.TryAdd(fplValue.FplId, fplValue) |> ignore
    | Some next when next.Name = PrimRefL && 
        (
            fplValue.Name = PrimDelegateEqualL 
         || fplValue.Name = PrimDelegateDecrementL
         || fplValue.Name = PrimIntrinsicPred
         ) ->
        addExpressionToParentArgList fplValue 
    | Some next when next.Name = PrimRefL ->
        next.FplId <- fplValue.FplId
        next.TypeId <- fplValue.TypeId
        next.Scope.TryAdd(fplValue.FplId, fplValue) |> ignore
    | _ -> addExpressionToParentArgList fplValue 

/// Indicates if an FplValue is the root of the SymbolTable.
let isRoot (fv:FplValue) = 
    match fv with
    | :? FplRoot -> true
    | _ -> false

[<AbstractClass>]
type FplGenericPredicate(positions: Positions, parent: FplValue) as this =
    inherit FplValue(positions, Some parent)
    do 
        this.FplId <- PrimUndetermined
        this.TypeId <- LiteralPred

    override this.Represent () =
        let ret = 
            this.ValueList
            |> Seq.map (fun subfv -> subfv.Represent())
            |> String.concat ", "
        if ret = "" then
            PrimUndetermined
        else 
            ret

    override this.RunOrder = None

/// Implements the semantics of an FPL predicate prime predicate that is intrinsic.
/// It serves as a value for everything in FPL that is "predicative in nature". These can be predicates, theorem-like-statements, proofs or predicative expressions. The value can have one of three values in FPL: "true", LiteralFalse, and "undetermined". 
type FplIntrinsicPred(positions: Positions, parent: FplValue) =
    inherit FplGenericPredicate(positions, parent)

    override this.Name = PrimIntrinsicPred
    override this.ShortName = LiteralPred

    override this.Clone () =
        let ret = new FplIntrinsicPred((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type (signatureType:SignatureType) = 
        match signatureType with
            | SignatureType.Name 
            | SignatureType.Mixed -> this.FplId
            | SignatureType.Type -> this.TypeId
                    
    override this.Represent (): string = this.FplId

    override this.Run _ = 
        this.Debug "Run"

    override this.EmbedInSymbolTable _ = addExpressionToReference this


/// Tries to find a mapping of an FplValue
let rec getMapping (fv:FplValue) =
    match fv.Name with
    | LiteralSelf
    | LiteralParent 
    | PrimRefL ->
        if fv.Scope.ContainsKey(fv.FplId) then
            getMapping fv.Scope[fv.FplId]
        else
            None
    | _ ->
        fv.ArgList |> Seq.tryFind (fun fv -> fv.Name = PrimMappingL)

type IHasSignature =
    abstract member SignStartPos : Position with get, set
    abstract member SignEndPos : Position with get, set

type IHasDimensions =
    abstract member Dimension : int
    abstract member DimensionTypes : List<FplValue>
    abstract member SetType : string -> Position -> Position -> unit

[<AbstractClass>]
type FplGenericVariable(fplId, positions: Positions, parent: FplValue) as this =
    inherit FplValue(positions, Some parent)
    let mutable _isSignatureVariable = false
    let mutable _isInitialized = false
    let mutable _isBound = false
    let mutable _isUsed = false

    do 
        this.FplId <- fplId
        this.TypeId <- LiteralUndef

    /// Getter if this variable was used after its declaration.
    member this.IsUsed
        with get () = _isUsed

    /// Sets this variable to a used one .
    member this.SetIsUsed() =
        let rec setIsUsed (fv:FplValue) =
            fv.GetVariables()
            |> List.map (fun var -> var :?> FplGenericVariable)
            |> List.iter (fun var -> var.SetIsUsed())
        _isUsed <- true
        setIsUsed this        

    /// Getter if this variable is bound (by a quantor of otherwise).
    member this.IsBound
        with get () = _isBound

    /// Sets this variable to a bound one.
    member this.SetIsBound() =
        let rec setIsBound (fv:FplValue) =
            fv.GetVariables()
            |> List.map (fun var -> var :?> FplGenericVariable)
            |> List.iter (fun var -> var.SetIsBound())
        _isBound <- true
        setIsBound this        

    /// Indicates if this Variable is declared in the signature (true) or in the block (false).
    member this.IsSignatureVariable
        with get () = _isSignatureVariable
        and set (value) = 
            _isSignatureVariable <- value
            _isBound <- value // all signature variables are also bound

    /// Indicates if this FplValue is an initialized variable
    member this.IsInitialized
        with get () = _isInitialized
        and set (value) = 
            _isInitialized <- value
            _isBound <- value // all initialized variables are also bound

    interface IVariable with
        member this.IsSignatureVariable 
            with get () = this.IsSignatureVariable
            and set (value) = this.IsSignatureVariable <- value
        member this.IsInitialized 
            with get () = this.IsInitialized
            and set (value) = this.IsInitialized <- value

    override this.EmbedInSymbolTable nextOpt =
        this.CheckConsistency()
        let addToRuleOfInference (block:FplValue) = 
            if block.Scope.ContainsKey(this.FplId) then
                let oldDiagnosticsStopped = ad.DiagnosticsStopped 
                ad.DiagnosticsStopped <- false
                emitVAR03diagnostics this.FplId block.Scope[this.FplId].QualifiedStartPos this.StartPos this.EndPos false
                ad.DiagnosticsStopped <- oldDiagnosticsStopped
            else
                block.Scope.Add(this.FplId, this)

        let addToSimpleFplBlocksScope (block:FplValue) formulaConflict = 
            if block.Scope.ContainsKey(this.FplId) then
                emitVAR03diagnostics this.FplId block.Scope[this.FplId].QualifiedStartPos this.StartPos this.EndPos formulaConflict
            else
                block.Scope.Add(this.FplId, this)
        
        let addToPropertyOrConstructor (property:FplValue) formulaConflict = 
            let parentOfProperty = property.Parent.Value
            if property.Scope.ContainsKey(this.FplId) then
                emitVAR03diagnostics this.FplId property.Scope[this.FplId].QualifiedStartPos this.StartPos this.EndPos formulaConflict
            elif parentOfProperty.Scope.ContainsKey(this.FplId) then
                // check also the scope of the property's parent block
                emitVAR03diagnostics this.FplId parentOfProperty.Scope[this.FplId].QualifiedStartPos this.StartPos this.EndPos formulaConflict
            else
                property.Scope.Add(this.FplId, this)

        let addToProofOrCorolllary (proofOrCorollary:FplValue) = 
            let rec conflictInScope (node:FplValue) formulaConflict =
                if node.Scope.ContainsKey(this.FplId) then
                    emitVAR03diagnostics this.FplId node.Scope[this.FplId].QualifiedStartPos this.StartPos this.EndPos formulaConflict
                    true
                else 
                    let parent = node.Parent.Value
                    match parent.Name with
                    | LiteralCorL
                    | LiteralThmL
                    | LiteralLemL
                    | LiteralPropL
                    | LiteralConjL
                    | LiteralAxL ->
                        conflictInScope parent formulaConflict
                    | _ -> false

            if not (conflictInScope proofOrCorollary false) then
                proofOrCorollary.Scope.Add(this.FplId, this)

        let addToVariableOrQuantorOrMapping (variableOrQuantor:FplValue) =
            let rec conflictInScope (node:FplValue) formulaConflict =
                if node.Scope.ContainsKey(this.FplId) then
                    emitVAR03diagnostics this.FplId node.Scope[this.FplId].QualifiedStartPos this.StartPos this.EndPos formulaConflict
                    true
                else 
                    let parent = node.Parent.Value
                    match parent.Name with
                    | PrimRoot 
                    | PrimTheoryL -> false
                    | _ ->
                        conflictInScope parent formulaConflict
            
            if not (conflictInScope variableOrQuantor true) then
                variableOrQuantor.Scope.Add(this.FplId, this)
                let blockOpt = variableOrQuantor.UltimateBlockNode
                match blockOpt with
                | Some block -> block.Scope.Add(this.FplId, this)
                | None -> ()
            else
                variableOrQuantor.Scope.TryAdd(this.FplId, this) |> ignore

        match nextOpt with 
        | Some next when ( next.Name = PrimRefL 
                        || next.Name = PrimTranslationL ) ->
            next.FplId <- this.FplId
            next.TypeId <- this.TypeId
            tryAddToParentUsingFplId this
        | Some next when ( next.Name = LiteralAxL 
                        || next.Name = LiteralThmL 
                        || next.Name = LiteralLemL 
                        || next.Name = LiteralPropL 
                        || next.Name = LiteralConjL 
                        || next.Name = PrimClassL 
                        || next.Name = PrimFuncionalTermL
                        || next.Name = PrimPredicateL
                        || next.Name = PrimExtensionL ) ->
            addToSimpleFplBlocksScope next false
        | Some next when next.Name = PrimRuleOfInference ->
            addToRuleOfInference next
        | Some next when ( next.Name = LiteralCtorL
                        || next.Name = PrimMandatoryFunctionalTermL 
                        || next.Name = PrimMandatoryPredicateL) ->
            addToPropertyOrConstructor next false
        | Some next when (next.Name = LiteralPrfL 
                        || next.Name = LiteralCorL) ->
            addToProofOrCorolllary next
        | Some next when (next.Name = PrimVariableL
                        || next.Name = PrimVariableArrayL) ->
            addToVariableOrQuantorOrMapping next
        | Some next when next.Name = PrimMappingL ->
            this.SetIsBound() // mapping-Variables are bound
            addToVariableOrQuantorOrMapping next
        | Some next when next.Name = PrimQuantorAll || next.Name = PrimQuantorExists || next.Name = PrimQuantorExistsN ->  
            this.SetIsBound() // quantor-Variables are bound
            if next.Scope.ContainsKey(this.FplId) then
                emitVAR02diagnostics this.FplId this.StartPos this.EndPos
            elif next.Name = PrimQuantorExistsN && next.Scope.Count>0 then 
                emitVAR07diagnostics this.FplId this.StartPos this.EndPos
            elif this.Name = PrimVariableArrayL then 
                emitVAR08diagnostics this.StartPos this.EndPos
            else
                addToVariableOrQuantorOrMapping next
                
        | _ -> addExpressionToParentArgList this

    override this.Type signatureType =
        let pars = getParamTuple this signatureType
        let head = getFplHead this signatureType
        let propagate = propagateSignatureType signatureType

        match (pars, getMapping this) with
        | ("", None) -> head
        | ("", Some map) -> sprintf "%s() -> %s" head (map.Type(propagate))
        | (_, None) -> sprintf "%s(%s)" head pars
        | (_, Some map) -> sprintf "%s(%s) -> %s" head pars (map.Type(propagate))

    override this.Run _ = 
        this.Debug "Run"

    override this.RunOrder = None

    override this.Copy(other: FplValue) = 
        base.Copy(other)
        let otherVar = other :?> FplGenericVariable
        if otherVar.IsBound then 
            this.SetIsBound()
        if otherVar.IsUsed then 
            this.SetIsUsed()
        this.IsSignatureVariable <- otherVar.IsSignatureVariable 
        this.IsInitialized <- otherVar.IsInitialized


let checkVAR04Diagnostics (fv:FplValue) = 
    fv.GetVariables()
    |> List.map (fun var -> var :?> FplGenericVariable)
    |> List.filter(fun var -> not var.IsUsed)
    |> List.iter (fun var -> 
        emitVAR04diagnostics var.FplId var.StartPos var.EndPos
    )

[<AbstractClass>]
type FplGenericPredicateWithExpression(positions: Positions, parent: FplValue) =
    inherit FplGenericPredicate(positions, parent)
    let mutable _signStartPos = Position("", 0L, 0L, 0L)
    let mutable _signEndPos = Position("", 0L, 0L, 0L)

    member this.SignStartPos
        with get() = _signStartPos
        and set(value) = _signStartPos <- value

    member this.SignEndPos
        with get() = _signEndPos
        and set(value) = _signEndPos <- value

    interface IHasSignature with
        member this.SignStartPos 
            with get () = this.SignStartPos
            and set (value) = this.SignStartPos <- value
        member this.SignEndPos 
            with get () = this.SignEndPos
            and set (value) = this.SignEndPos <- value

    override this.Type signatureType = getFplHead this signatureType

    override this.CheckConsistency () = 
        base.CheckConsistency()
        checkVAR04Diagnostics this

            
[<AbstractClass>]
type FplGenericInheriting(positions: Positions, parent: FplValue) as this =
    inherit FplValue(positions, Some parent)
    let _inheritedVariables = Dictionary<string, FplValue>()
    let _inheritedProperties = Dictionary<string, FplValue>()
    // used to ensure that every clone of an inherited variable or property will preserve reference identity for referenced nodes 
    let _cloneMap = Dictionary<string, FplValue>() 

    member this.InheritVariables (fromBaseNode:FplValue) = 
        fromBaseNode.GetVariables()
        |> List.iter (fun var ->
            if _inheritedVariables.ContainsKey var.FplId then
                emitVAR06iagnostic var.FplId fromBaseNode.FplId (_inheritedVariables[var.FplId].FplId) true fromBaseNode.StartPos fromBaseNode.EndPos
            else
                // remember the variable and the base node it is from 
                _inheritedVariables.Add (var.FplId, fromBaseNode)

                // add the inherited variable to the scope of this
                if _cloneMap.ContainsKey var.FplId then 
                    this.Scope.TryAdd (var.FplId, _cloneMap[var.FplId]) |> ignore
                else
                    let clone = var.Clone() // todo: use Clone(_cloneMap) instead
                    _cloneMap.TryAdd (var.FplId, clone) |> ignore
                    this.Scope.TryAdd (var.FplId, clone) |> ignore
        )
    member this.InheritProperties (fromBaseNode:FplValue)  = 
        fromBaseNode.GetProperties()
        |> List.iter (fun prty ->
            let prtyName = prty.Type SignatureType.Mixed
            if _inheritedProperties.ContainsKey prtyName then
                emitSIG06iagnostic prtyName fromBaseNode.FplId (_inheritedProperties[prtyName].FplId) true fromBaseNode.StartPos fromBaseNode.EndPos
            else
                // remember the property and the base node it is from 
                _inheritedProperties.Add (prtyName, fromBaseNode)

                // add the inherited property to the scope of this
                if _cloneMap.ContainsKey prtyName then 
                    this.Scope.TryAdd (prtyName, _cloneMap[prtyName]) |> ignore
                else
                    let clone = prty.Clone() // todo: use Clone(_cloneMap) instead
                    _cloneMap.TryAdd (prtyName, clone) |> ignore
                    this.Scope.TryAdd (prtyName, clone) |> ignore
        )


type FplPredicateList(positions: Positions, parent: FplValue, runOrder) = 
    inherit FplValue(positions, Some parent)
    let _runOrder = runOrder
    override this.Name = LiteralPreL
    override this.ShortName = LiteralInf

    override this.Clone () =
        let ret = new FplPredicateList((this.StartPos, this.EndPos), this.Parent.Value, _runOrder)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = 
        this.ArgList
        |> Seq.map (fun fv -> fv.Type signatureType)
        |> String.concat ", "

    override this.Represent() = 
        this.ArgList
        |> Seq.map (fun fv -> fv.Represent())
        |> String.concat ", "

    override this.Run variableStack = 
        // todo implement run
        this.Debug "Run"


    override this.EmbedInSymbolTable _ = addExpressionToParentArgList this

    override this.RunOrder = Some _runOrder

type FplRuleOfInference(positions: Positions, parent: FplValue, runOrder) =
    inherit FplGenericPredicateWithExpression(positions, parent)
    let _runOrder = runOrder

    override this.Name = PrimRuleOfInference
    override this.ShortName = LiteralInf

    override this.Clone () =
        let ret = new FplRuleOfInference((this.StartPos, this.EndPos), this.Parent.Value, _runOrder)
        this.AssignParts(ret)
        ret

    override this.IsFplBlock () = true
    override this.IsBlock () = true    

    override this.Run variableStack = 
        // todo implement run
        this.Debug "Run"
        emitLG004diagnostic this.Name this.Arity this.StartPos this.EndPos

    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        tryAddToParentUsingFplId this

    override this.RunOrder = Some _runOrder

type FplInstance(positions: Positions, parent: FplValue) as this =
    inherit FplGenericInheriting(positions, parent)

    do
        this.FplId <- LiteralObj
        this.TypeId <- LiteralObj

    override this.Name = PrimInstanceL
    override this.ShortName = PrimInstance

    override this.Clone () =
        let ret = new FplInstance((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = 
        let head = getFplHead this signatureType 
        head

    override this.Represent () = 
        let head = this.TypeId

        // baseClasses = instances of all base classes
        let baseClasses =
            this.ArgList
            |> Seq.map (fun fv -> fv.Represent())
            |> String.concat ","
            |> fun body -> "\"base\":[" + body + "]"
            
        let vars =
            this.Scope.Values
            |> Seq.filter (fun fv -> 
                fv.Name = PrimVariableL 
                || fv.Name = PrimVariableArrayL
                )
            |> Seq.map (fun fv -> "{" + fv.FplId + ":{" + fv.Represent() + "}}")
            |> String.concat ","
            |> fun body -> "\"vars\":[" + body + "]"

        // only mandatory properties
        let prtys =
            this.Scope.Values
            |> Seq.filter (fun fv -> 
                fv.Name = PrimMandatoryFunctionalTermL 
                || fv.Name = PrimMandatoryPredicateL
                )
            |> Seq.map (fun fv -> $"\"{fv.Type SignatureType.Mixed}\"")
            |> String.concat $","
            |> fun body -> "\"prtys\":[" + body + "]"

        let body = 
            match head, baseClasses, vars, prtys with 
            | LiteralObj, _, _, _ 
            | LiteralObjL, _, _, _ ->
                "\"name\":\"" + head + "\""
            | _, _, _, _ ->
                "\"name\":\"" + head + "\"," +
                baseClasses + "," +
                vars + "," +
                prtys
        "{" + body + "}"

    override this.Run _ = 
        this.Debug "Run"

    override this.EmbedInSymbolTable _ = addExpressionToParentArgList this 

    override this.RunOrder = None

type FplBase(positions: Positions, parent: FplValue) =
    inherit FplValue(positions, Some parent)

    override this.Name = LiteralBase
    override this.ShortName = LiteralBase

    override this.Clone () =
        let ret = new FplBase((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type _ = this.FplId

    override this.Represent () = LiteralUndef

    override this.EmbedInSymbolTable _ = addExpressionToParentArgList this

    override this.Run variableStack = 
        this.Debug "Run"

    override this.RunOrder = None

    member this.BaseClass = 
        if this.Scope.Count > 0 then
            Some (this.Scope.Values |> Seq.head)
        else 
            None    

[<AbstractClass>]
type FplGenericConstructor(name, positions: Positions, parent: FplValue) as this =
    inherit FplValue(positions, Some parent)
    let mutable (_toBeConstructedClass:FplValue option) = None 

    do
        this.FplId <- name
        this.TypeId <- name

    override this.Name = PrimDefaultConstructor
    override this.ShortName = LiteralCtor

    override this.Represent () = 
        let head = this.TypeId

        // args = instances of all base classes
        let args =
            this.ArgList
            |> Seq.map (fun fv -> fv.Represent())
            |> String.concat $",{System.Environment.NewLine}"
        let vars =
            this.Scope.Values
            |> Seq.filter (fun fv -> 
                fv.Name = PrimVariableL 
                || fv.Name = PrimVariableArrayL
                )
            |> Seq.map (fun fv -> $"{fv.FplId} = {fv.Represent()}")
            |> String.concat $",{System.Environment.NewLine}"
        // only mandatory properties
        let prtys =
            this.Scope.Values
            |> Seq.filter (fun fv -> 
                fv.Name = PrimMandatoryFunctionalTermL 
                || fv.Name = PrimMandatoryPredicateL
                )
            |> Seq.map (fun fv -> $"{fv.FplId} = {fv.Represent()}")
            |> String.concat $",{System.Environment.NewLine}"

        match head, args, vars, prtys with 
        | LiteralObj, _, _, _ 
        | LiteralObjL, _, _, _ ->
            $"{head}()"
        | _, _, _, _ ->
            $"{head}({args});vars({vars});prtys({prtys})"
            
    member this.ToBeConstructedClass  
        with get () = _toBeConstructedClass
        and set (value) = _toBeConstructedClass <- value


    override this.Run variableStack = 
        this.Debug "Run"

        let rec createSubInstance (classDef:FplValue) (instance:FplValue) (baseInstance:FplValue)=
            classDef.ArgList
            |> Seq.filter (fun fv -> fv.Name = LiteralBase)
            |> Seq.map (fun fv -> fv :?> FplBase)
            |> Seq.map (fun fv -> fv.BaseClass)
            |> Seq.iter (fun baseClassOpt ->
                match baseClassOpt with
                | Some baseClass ->
                    let subInstance = new FplInstance((this.StartPos, this.EndPos), this)
                    subInstance.FplId <- baseClass.FplId
                    subInstance.TypeId <- subInstance.FplId
                    createSubInstance baseClass subInstance baseInstance
                    instance.ArgList.Add subInstance
                | _ -> ()
            )

        let instance = new FplInstance((this.StartPos, this.EndPos), this)
        match this.ToBeConstructedClass with
        | Some classDef -> 
            instance.FplId <- classDef.FplId
            instance.TypeId <- classDef.FplId
            this.ArgList 
            |> Seq.iter (fun fv ->
                fv.Run variableStack
            )
            createSubInstance classDef instance instance
        | None ->
            instance.FplId <- LiteralUndef
            instance.TypeId <- LiteralUndef
        this.SetValue instance

    member this.Instance = 
        if this.ValueList.Count = 1 then 
            Some (this.ValueList[0] :?> FplInstance)
        else
            None

    override this.RunOrder = None

/// This constructor is only used for creating instances of classes that have no declared constructors.
/// In FPL, such classes are "intrinsic". When the default constructor calls the constructor
/// of some base classes, it is only possible if those classes are also intrisic or have declared constructors
/// without parameters. 
type FplDefaultConstructor(name, positions: Positions, parent: FplValue) =
    inherit FplGenericConstructor(name, positions, parent)

    override this.Name = PrimDefaultConstructor
    override this.ShortName = LiteralCtor

    override this.Clone () =
        let ret = new FplDefaultConstructor(this.FplId, (this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType =
        match signatureType with
        | SignatureType.Name
        | SignatureType.Mixed -> $"{this.FplId}()" 
        | SignatureType.Type -> this.TypeId

    override this.Represent() = $"{this.TypeId}()" 

    override this.EmbedInSymbolTable nextOpt = 
        this.CheckConsistency()
        match nextOpt with 
        | Some next ->
            next.Scope.TryAdd(this.FplId, this) |> ignore
        | _ -> ()

type FplConstructor(positions: Positions, parent: FplValue) as this =
    inherit FplGenericConstructor(parent.FplId, positions, parent)
    let mutable _signStartPos = Position("", 0L, 0L, 0L)
    let mutable _signEndPos = Position("", 0L, 0L, 0L)
    let mutable _parentConstructorCalls = HashSet<string>()

    do 
        this.ToBeConstructedClass <- Some parent

    member this.SignStartPos
        with get() = _signStartPos
        and set(value) = _signStartPos <- value

    member this.SignEndPos
        with get() = _signEndPos
        and set(value) = _signEndPos <- value

    interface IHasSignature with
        member this.SignStartPos 
            with get () = this.SignStartPos
            and set (value) = this.SignStartPos <- value
        member this.SignEndPos 
            with get () = this.SignEndPos
            and set (value) = this.SignEndPos <- value

    override this.Name = LiteralCtorL
    override this.ShortName = LiteralCtor

    override this.Clone () =
        let ret = new FplConstructor((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    member this.ParentConstructorCalls = _parentConstructorCalls

    override this.IsBlock () = true

    override this.Type signatureType =
        let head = getFplHead this signatureType
        let paramT = getParamTuple this signatureType
        match signatureType with
        | SignatureType.Name
        | SignatureType.Mixed -> $"{head}({paramT})" 
        | SignatureType.Type -> head

    override this.Represent () = this.Type(SignatureType.Mixed)

    override this.CheckConsistency () = 
        base.CheckConsistency()
        // check if the constructor calls all necessary parent classes
        let parentClassOpt = this.UltimateBlockNode
        match parentClassOpt with
        | Some (:? FplClass as parentClass) ->
            parentClass.ArgList 
            |> Seq.iter (fun fv -> 
                if not (this.ParentConstructorCalls.Contains fv.FplId) then
                    emitID020Diagnostics fv.FplId fv.StartPos
            )
        | _ -> ()
        checkVAR04Diagnostics this

    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        tryAddSubBlockToFplBlock this

    member this.ParentClass = this.Parent.Value :?> FplClass

and FplClass(positions: Positions, parent: FplValue) as this =
    inherit FplGenericInheriting(positions, parent)
    let mutable _signStartPos = Position("", 0L, 0L, 0L)
    let mutable _signEndPos = Position("", 0L, 0L, 0L)

    do
        this.FplId <- LiteralObj
        this.TypeId <- LiteralObj

    member this.SignStartPos
        with get() = _signStartPos
        and set(value) = _signStartPos <- value

    member this.SignEndPos
        with get() = _signEndPos
        and set(value) = _signEndPos <- value

    interface IHasSignature with
        member this.SignStartPos 
            with get () = this.SignStartPos
            and set (value) = this.SignStartPos <- value
        member this.SignEndPos 
            with get () = this.SignEndPos
            and set (value) = this.SignEndPos <- value

    override this.Name = PrimClassL
    override this.ShortName = PrimClass

    override this.Clone () =
        let ret = new FplClass((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.IsFplBlock () = true
    override this.IsBlock () = true
    override this.IsClass () = true
    
    /// If this is a class definition, the function will return a list (possibly empty) list of all of its constructors.
    member this.GetConstructors() =
        this.Scope
        |> Seq.map (fun kvp -> kvp.Value)
        |> Seq.filter (fun fv -> fv.Name = LiteralCtorL)
        |> Seq.toList
    
    override this.Type signatureType =
        match signatureType with
        | SignatureType.Name 
        | SignatureType.Mixed -> this.FplId
        | SignatureType.Type -> this.TypeId

    override this.Represent () = $"dec {LiteralCl} {this.FplId}"

    override this.Run variableStack = 
        this.Debug "Run"
        this.GetProperties()
        |> List.iter (fun fv -> fv.Run variableStack)
        this.SetValue(new FplInstance((this.StartPos, this.EndPos), this))

    override this.CheckConsistency () = 
        base.CheckConsistency()
        checkVAR04Diagnostics this

    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        tryAddToParentUsingFplId this 

    override this.RunOrder = None

    member this.AddDefaultConstructor () = 
        let defaultConstructor = new FplDefaultConstructor(this.FplId, (this.StartPos, this.EndPos), this)
        defaultConstructor.EmbedInSymbolTable defaultConstructor.Parent
        defaultConstructor.ToBeConstructedClass <- Some this
    

type ICanBeCalledRecusively =
    abstract member CallCounter : int

type IReady =
    abstract member IsReady : bool

type IHasProof =
    abstract member HasProof : bool with get, set

[<AbstractClass>]
type FplGenericPredicateBlock(positions: Positions, parent: FplValue) =
    inherit FplGenericPredicateWithExpression(positions, parent)
    let mutable _isReady = false
    let mutable _callCounter = 0

    interface IReady with
        member _.IsReady = _isReady

    interface ICanBeCalledRecusively with
        member _.CallCounter = _callCounter

    override this.IsBlock () = true

    override this.Type signatureType = 
        let head = getFplHead this signatureType

        let paramT = getParamTuple this signatureType
        sprintf "%s(%s)" head paramT

    override this.Run variableStack = 
        this.Debug "Run"
        if not _isReady then
            _callCounter <- _callCounter + 1
            if _callCounter > maxRecursion then
                emitLG002diagnostic (this.Type(SignatureType.Name)) _callCounter this.StartPos this.EndPos
            else
                if this.IsIntrinsic then 
                    let undetermined = new FplIntrinsicPred((this.StartPos, this.EndPos), this)
                    this.SetValue undetermined
                else
                    this.ArgList
                    |> Seq.iter (fun fv -> 
                        fv.Run variableStack
                        this.SetValuesOf fv
                    )

            _isReady <- this.Arity = 0 

    override this.CheckConsistency () = 
        if not this.IsIntrinsic then // if not intrinsic, check variable usage
            checkVAR04Diagnostics this

let checkSIG02Diagnostics (root:FplValue) symbol precedence pos1 pos2 = 
    let precedences = Dictionary<int, FplValue>()
    let precedenceWasAlreadyThere precedence fv =
        if not (precedences.ContainsKey(precedence)) then
            precedences.Add(precedence, fv)
            false
        else
            true
    root.Scope
    |> Seq.map (fun kv -> kv.Value)
    |> Seq.iter (fun theory ->
        theory.Scope
        |> Seq.map (fun kv1 -> kv1.Value)
        |> Seq.iter (fun block ->
            match block.ExpressionType with
            | FixType.Infix(_, precedence) -> precedenceWasAlreadyThere precedence block |> ignore
            | _ -> ()))
    if precedences.ContainsKey(precedence) then
        let conflict = precedences[precedence].QualifiedStartPos
        emitSIG02Diagnostics symbol precedence conflict pos1 pos2


type FplPredicate(positions: Positions, parent: FplValue, runOrder) =
    inherit FplGenericPredicateBlock(positions, parent)
    let _runOrder = runOrder

    override this.Name = PrimPredicateL
    override this.ShortName = PrimPredicate

    override this.Clone () =
        let ret = new FplPredicate((this.StartPos, this.EndPos), this.Parent.Value, _runOrder)
        this.AssignParts(ret)
        ret

    override this.IsFplBlock () = true

    override this.CheckConsistency() = 
        base.CheckConsistency()
        match this.ExpressionType with
        | FixType.Infix _ when this.Arity <> 2 -> emitSIG00Diagnostics this.ExpressionType.Type 2 this.Arity this.SignStartPos this.SignEndPos
        | FixType.Prefix _ when this.Arity <> 1 -> emitSIG00Diagnostics this.ExpressionType.Type 1 this.Arity this.SignStartPos this.SignEndPos
        | FixType.Postfix _ when this.Arity <> 1 -> emitSIG00Diagnostics this.ExpressionType.Type 1 this.Arity this.SignStartPos this.SignEndPos
        | _ -> ()
        match this.ExpressionType with
        | FixType.Infix (symbol, precedence) -> checkSIG02Diagnostics (getRoot this) symbol precedence this.SignStartPos this.SignEndPos
        | _ -> ()

    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        tryAddToParentUsingMixedSignature this

    override this.Run variableStack = 
        this.Debug "Run"
        base.Run variableStack 
        this.GetProperties()
        |> List.iter (fun fv -> fv.Run variableStack)


    override this.RunOrder = Some _runOrder

type FplMandatoryPredicate(positions: Positions, parent: FplValue) =
    inherit FplGenericPredicateBlock(positions, parent)

    override this.Name = PrimMandatoryPredicateL
    override this.ShortName = PrimMandatoryPredicate

    override this.Clone () =
        let ret = new FplMandatoryPredicate((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.EmbedInSymbolTable _ = 
        base.CheckConsistency()
        tryAddSubBlockToFplBlock this

type FplAxiom(positions: Positions, parent: FplValue, runOrder) =
    inherit FplGenericPredicateWithExpression(positions, parent)
    let _runOrder = runOrder
    let mutable _isReady = false

    interface IReady with
        member _.IsReady = _isReady

    override this.Name = LiteralAxL
    override this.ShortName = LiteralAx

    override this.Clone () =
        let ret = new FplAxiom((this.StartPos, this.EndPos), this.Parent.Value, _runOrder)
        this.AssignParts(ret)
        ret

    override this.IsFplBlock () = true
    override this.IsBlock () = true

    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        tryAddToParentUsingFplId this

    override this.Run variableStack = 
        this.Debug "Run"
        if not _isReady then
            this.ArgList
            |> Seq.iter (fun fv -> 
                fv.Run variableStack
                this.SetValuesOf fv
            )
            _isReady <- this.Arity = 0 
            emitLG003diagnostic (this.Type(SignatureType.Name)) this.Name (this.Represent()) this.StartPos this.EndPos
            emitLG004diagnostic this.Name this.Arity this.StartPos this.EndPos

    override this.RunOrder = Some _runOrder

[<AbstractClass>]
type FplGenericTheoremLikeStmt(positions: Positions, parent: FplValue, runOrder) =
    inherit FplGenericPredicateWithExpression(positions, parent)
    let _runOrder = runOrder
    let mutable _isReady = false
    let mutable _hasProof = false

    override this.Name = LiteralThmL
    override this.ShortName = LiteralThm

    interface IReady with
        member _.IsReady = _isReady

    interface IHasProof with
        member this.HasProof
            with get (): bool = _hasProof
            and set (value) = _hasProof <- value
    
    override this.IsFplBlock () = true
    override this.IsBlock () = true

    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        tryAddToParentUsingFplId this 

    override this.Run variableStack = 
        this.Debug "Run"
        if not _isReady then
            this.ArgList
            |> Seq.iter (fun fv -> 
                fv.Run variableStack
                this.SetValuesOf fv
            )
            _isReady <- this.Arity = 0 
            emitLG003diagnostic (this.Type(SignatureType.Name)) this.Name (this.Represent()) this.StartPos this.EndPos
            emitLG004diagnostic this.Name this.Arity this.StartPos this.EndPos

            // evaluate all corollaries and proofs of the theorem-like statement
            this.Scope.Values
            |> Seq.filter (fun fv -> fv.Name = LiteralPrfL || fv.Name = LiteralCorL) 
            |> Seq.sortBy (fun block -> block.RunOrder.Value) 
            |> Seq.iter (fun fv -> 
                fv.Run variableStack
            )

        if not _hasProof then 
           emitPR007Diagnostics (this.Type(SignatureType.Name)) this.Name this.StartPos this.EndPos

    override this.RunOrder = Some _runOrder

type FplTheorem(positions: Positions, parent: FplValue, runOrder) =
    inherit FplGenericTheoremLikeStmt(positions, parent, runOrder)

    override this.Name = LiteralThmL
    override this.ShortName = LiteralThm

    override this.Clone () =
        let ret = new FplTheorem((this.StartPos, this.EndPos), this.Parent.Value, this.RunOrder.Value)
        this.AssignParts(ret)
        ret

type FplLemma(positions: Positions, parent: FplValue, runOrder) =
    inherit FplGenericTheoremLikeStmt(positions, parent, runOrder)

    override this.Name = LiteralLemL
    override this.ShortName = LiteralLem

    override this.Clone () =
        let ret = new FplLemma((this.StartPos, this.EndPos), this.Parent.Value, this.RunOrder.Value)
        this.AssignParts(ret)
        ret

type FplProposition(positions: Positions, parent: FplValue, runOrder) =
    inherit FplGenericTheoremLikeStmt(positions, parent, runOrder)

    override this.Name = LiteralPropL
    override this.ShortName = LiteralProp

    override this.Clone () =
        let ret = new FplProposition((this.StartPos, this.EndPos), this.Parent.Value, this.RunOrder.Value)
        this.AssignParts(ret)
        ret

type FplCorollary(positions: Positions, parent: FplValue, runOrder) =
    inherit FplGenericTheoremLikeStmt(positions, parent, runOrder)

    override this.Name = LiteralCorL
    override this.ShortName = LiteralCor

    override this.Clone () =
        let ret = new FplCorollary((this.StartPos, this.EndPos), this.Parent.Value, this.RunOrder.Value)
        this.AssignParts(ret)
        ret

    override this.EmbedInSymbolTable _ =
        /// Tries to find a theorem-like statement, a conjecture, or an axiom for a corollary
        /// and returns different cases of ScopeSearchResult, depending on different semantical error situations.
        let tryFindAssociatedBlockForCorollary (fplValue: FplValue) =
            match fplValue.Parent with
            | Some theory ->

                let flattenedScopes = flattenScopes theory.Parent.Value

                // The parent node of the proof is the theory. In its scope
                // we should find the theorem we are looking for.
                let buildingBlocksMatchingDollarDigitNameList =
                    // the potential theorem name of the corollary is the
                    // concatenated type signature of the name of the corollary
                    // without the last dollar digit
                    let potentialBlockName = stripLastDollarDigit (fplValue.Type(SignatureType.Mixed))

                    flattenedScopes
                    |> Seq.filter (fun fv -> fv.FplId = potentialBlockName)
                    |> Seq.toList

                let potentialBlockList =
                    buildingBlocksMatchingDollarDigitNameList
                    |> List.filter (fun fv -> isProvable fv || isAxiomOrConnjecture fv)

                let notPotentialBlockList =
                    buildingBlocksMatchingDollarDigitNameList
                    |> List.filter (fun fv ->
                        not (
                            isProvable fv || isAxiomOrConnjecture fv
                        ))

                if potentialBlockList.Length > 1 then
                    ScopeSearchResult.FoundMultiple(
                        potentialBlockList
                        |> List.map (fun fv -> sprintf "'%s' %s" fv.Name (fv.Type(SignatureType.Mixed)))
                        |> String.concat ", "
                    )
                elif potentialBlockList.Length > 0 then
                    let potentialTheorem = potentialBlockList.Head
                    ScopeSearchResult.FoundAssociate potentialTheorem
                elif notPotentialBlockList.Length > 0 then
                    let potentialOther = notPotentialBlockList.Head
                    ScopeSearchResult.FoundIncorrectBlock (qualifiedName potentialOther)
                else
                    ScopeSearchResult.NotFound
            | None -> ScopeSearchResult.NotApplicable


        match tryFindAssociatedBlockForCorollary this with
        | ScopeSearchResult.FoundAssociate potentialParent -> 
            // everything is ok, change the parent of the provable from theory to the found parent 
            this.Parent <- Some potentialParent
        | ScopeSearchResult.FoundIncorrectBlock incorrectBlock ->
            emitID005diagnostics this.FplId incorrectBlock this.StartPos this.EndPos
        | ScopeSearchResult.NotFound ->
            emitID006diagnostics this.FplId this.StartPos this.EndPos
        | _ -> ()
        tryAddToParentUsingFplId this


type FplConjecture(positions: Positions, parent: FplValue, runOrder) =
    inherit FplGenericPredicateWithExpression(positions, parent)
    let _runOrder = runOrder
    let mutable _isReady = false

    interface IReady with
        member _.IsReady = _isReady

    override this.Name = LiteralConjL
    override this.ShortName = LiteralConj

    override this.Clone () =
        let ret = new FplConjecture((this.StartPos, this.EndPos), this.Parent.Value, _runOrder)
        this.AssignParts(ret)
        ret

    override this.IsFplBlock () = true
    override this.IsBlock () = true

    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        tryAddToParentUsingFplId this 

    override this.Run variableStack = 
        this.Debug "Run"
        if not _isReady then
            this.ArgList
            |> Seq.iter (fun fv -> 
                fv.Run variableStack
                this.SetValuesOf fv
            )
            _isReady <- this.Arity = 0 
            emitLG004diagnostic this.Name this.Arity this.StartPos this.EndPos


    override this.RunOrder = Some _runOrder

[<AbstractClass>]
type FplGenericArgInference(positions: Positions, parent: FplValue) =
    inherit FplGenericPredicate(positions, parent)

    override this.Type signatureType =
        let head = getFplHead this signatureType
        head

    override this.EmbedInSymbolTable _ = addExpressionToParentArgList this 

[<AbstractClass>]
type FplGenericJustificationItem(positions: Positions, parent: FplValue) =
    inherit FplValue(positions, Some parent)

    override this.ShortName = PrimJustification

    override this.Type signatureType =
        let head = getFplHead this signatureType
        head

    override this.Represent() = this.Type(SignatureType.Name)

    override this.EmbedInSymbolTable _ = 
        let thisJustificationItemId = this.Type(SignatureType.Mixed)

        let alreadyAddedIdOpt = 
            this.Parent.Value.ArgList
            |> Seq.map (fun argJi -> argJi.Type(SignatureType.Mixed))
            |> Seq.tryFind (fun otherId -> otherId = thisJustificationItemId)
        match alreadyAddedIdOpt with
        | Some otherId ->
            emitPR004Diagnostics thisJustificationItemId otherId this.StartPos this.EndPos 
        | _ -> ()
        addExpressionToParentArgList this

    override this.RunOrder = None

    member this.ParentJustification = this.Parent.Value :?> FplJustification

    /// Returns Some FPL node that is referenced by this JustificationItem (if it could be found) or None
    member this.ReferencedJustification = 
        if this.Scope.Count > 0 then 
            Some (this.Scope.Values |> Seq.head)
        else
            None

    /// Returns a) Some expression inferred by the proceeding JustificationItem in the same proof argument.
    /// b) If there is no proceeding JustificationItem in the same proof argument, but there is a proceeding argument proof,
    ///    the function will return Some expression parsed from that proceeding proof argument. 
    ///    Note that this parsed expression might differ from the InferredExpression of the proceeding proof arguments last JustificationItem.
    /// c) If there is no proceeding JustificationItem and even no proceeding proof argument, the function will return None.
    member this.PreviousExpression = 
        if this.ArgList.Count > 0 then 
            Some this.ArgList[0]
        else
            None

    /// Returns Some expression that could be inferred by this JustificationItem based on its PreviousExpression and its ReferencedJustification.
    /// None will be returned if Such an InferredExpression could not be inferred
    member this.InferredExpression = 
        if this.ArgList.Count > 1 then 
            Some this.ArgList[1]
        else
            None

and FplJustificationItemByAx(positions: Positions, parent: FplValue) =
    inherit FplGenericJustificationItem(positions, parent)

    override this.Name = PrimJIByAx

    override this.Clone () =
        let ret = new FplJustificationItemByAx((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Run variableStack = 
        // todo implement Run
        this.Debug "Run"

and FplJustificationItemByDef(positions: Positions, parent: FplValue) =
    inherit FplGenericJustificationItem(positions, parent)

    override this.Name = PrimJIByDef

    override this.Clone () =
        let ret = new FplJustificationItemByDef((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Run variableStack = 
        // todo implement Run
        this.Debug "Run"

and FplJustificationItemByDefVar(positions: Positions, parent: FplValue) =
    inherit FplGenericJustificationItem(positions, parent)

    override this.Name = PrimJIByDefVar

    override this.Clone () =
        let ret = new FplJustificationItemByDefVar((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Run variableStack = 
        // todo implement Run
        this.Debug "Run"

and FplJustificationItemByConj(positions: Positions, parent: FplValue) =
    inherit FplGenericJustificationItem(positions, parent)

    override this.Name = PrimJIByConj

    override this.Clone () =
        let ret = new FplJustificationItemByConj((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Run variableStack = 
        // todo implement Run
        this.Debug "Run"

and FplJustificationItemByCor(positions: Positions, parent: FplValue) =
    inherit FplGenericJustificationItem(positions, parent)

    override this.Name = PrimJIByCor

    override this.Clone () =
        let ret = new FplJustificationItemByCor((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Run variableStack = 
        // todo implement Run
        this.Debug "Run"

and FplJustificationItemByInf(positions: Positions, parent: FplValue) =
    inherit FplGenericJustificationItem(positions, parent)

    override this.Name = PrimJIByInf

    override this.Clone () =
        let ret = new FplJustificationItemByInf((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Run variableStack = 
        // todo implement Run
        this.Debug "Run"

and FplJustificationItemByRefArgument(positions: Positions, parent: FplValue) =
    inherit FplGenericJustificationItem(positions, parent)

    override this.Name = PrimJIByRefArgument

    override this.Clone () =
        let ret = new FplJustificationItemByRefArgument((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Run variableStack = 
        // todo implement Run
        this.Debug "Run"

and FplJustificationItemByProofArgument(positions: Positions, parent: FplValue) =
    inherit FplGenericJustificationItem(positions, parent)

    override this.Name = PrimJIByProofArgument

    override this.Clone () =
        let ret = new FplJustificationItemByProofArgument((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Run variableStack = 
        // todo implement Run
        this.Debug "Run"


and FplJustificationItemByTheoremLikeStmt(positions: Positions, parent: FplValue) =
    inherit FplGenericJustificationItem(positions, parent)

    override this.Name = PrimJIByTheoremLikeStmt

    override this.Clone () =
        let ret = new FplJustificationItemByTheoremLikeStmt((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Run variableStack = 
        // todo implement Run
        this.Debug "Run"


and FplJustification(positions: Positions, parent: FplValue) =
    inherit FplGenericPredicate(positions, parent)

    override this.Name = PrimJustificationL
    override this.ShortName = PrimJustification

    override this.Clone () =
        let ret = new FplJustification((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType =
        let head = getFplHead this signatureType
        head

    override this.Run variableStack = 
        // todo implement Run
        this.Debug "Run"


    member this.GetOrderedJustificationItems =
        this.Scope.Values
            |> Seq.sortBy (fun fv -> fv.RunOrder)
            |> Seq.map (fun fv -> fv :?> FplGenericJustificationItem)
            |> Seq.toList

    override this.EmbedInSymbolTable _ = addExpressionToParentArgList this

    member this.ParentArgument = this.Parent.Value :?> FplArgument

and FplArgInferenceAssume(positions: Positions, parent: FplValue) =
    inherit FplGenericArgInference(positions, parent)

    override this.Name = PrimArgInfAssume
    override this.ShortName = PrimArgInf

    override this.Clone () =
        let ret = new FplArgInferenceAssume((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Run variableStack = 
        // todo implement Run
        this.Debug "Run"

    member this.ParentArgument = this.Parent.Value :?> FplArgument

and FplArgInferenceRevoke(positions: Positions, parent: FplValue) =
    inherit FplGenericArgInference(positions, parent)

    override this.Name = PrimArgInfRevoke
    override this.ShortName = PrimArgInf

    override this.Clone () =
        let ret = new FplArgInferenceRevoke((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Run variableStack = 
        // todo implement Run
        this.Debug "Run"

    member this.ParentArgument = this.Parent.Value :?> FplArgument

and FplArgInferenceTrivial(positions: Positions, parent: FplValue) =
    inherit FplGenericArgInference(positions, parent)

    override this.Name = PrimArgInfTrivial
    override this.ShortName = PrimArgInf

    override this.Clone () =
        let ret = new FplArgInferenceTrivial((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Run variableStack = 
        this.Debug "Run"
        let value = new FplIntrinsicPred((this.StartPos, this.EndPos), this) 
        value.FplId <- LiteralTrue
        this.SetValue value

    member this.ParentArgument = this.Parent.Value :?> FplArgument

and FplArgInferenceDerived(positions: Positions, parent: FplValue) =
    inherit FplGenericArgInference(positions, parent)

    override this.Name = PrimArgInfDerive
    override this.ShortName = PrimArgInf

    override this.Clone () =
        let ret = new FplArgInferenceDerived((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Run variableStack = 
        // todo implement run
        this.Debug "Run"

    member this.ParentArgument = this.Parent.Value :?> FplArgument

and FplArgument(positions: Positions, parent: FplValue, runOrder) =
    inherit FplGenericPredicate(positions, parent)
    let _runOrder = runOrder

    override this.Name = PrimArgL
    override this.ShortName = PrimArg

    override this.Clone () =
        let ret = new FplArgument((this.StartPos, this.EndPos), this.Parent.Value, _runOrder)
        this.AssignParts(ret)
        ret
    
    override this.Type signatureType =
        let head = getFplHead this signatureType
        head

    member this.Justification = 
        if this.ArgList.Count>0 then 
            let justification = this.ArgList[0]
            Some (justification :?> FplJustification)
        else
            None

    member this.ArgumentInference = 
        if this.ArgList.Count>1 then 
            let argInference = this.ArgList[1]
            Some (argInference :?> FplGenericArgInference)
        else
            None

    override this.Run variableStack =
        this.Debug "Run"
        // the argument has two elements, the justification and an argument inference
        let justificationOpt = this.Justification
        let argInferenceOpt = this.ArgumentInference

        match justificationOpt, argInferenceOpt with
        | Some justification, Some argInference -> 
            let orderdListJustifications = justification.GetOrderedJustificationItems
            orderdListJustifications
            |> List.iter (fun fv ->
                fv.Run variableStack
            )

        | _ -> ()
        (* todo: Enhance variableStack by the context in which this argument is being evaluated
            Here are some preliminary considerations: 
            1) The context should include 
                a) the argumentInference of the previous argument (if such exists) - the first argument doesn't have such a predecessor
                b) if the argument has more than justification, variableStack should store a list of applying the previous argumentInference from a) each justification sorted by their RunOrder 
                   so then next justification from the list can be applied to the last result from variableStack. 
                   The idea is that a list of justification could be "unzipped" in the FPL code by writing a sequence
                   of arguments, each having only a single justification from the original list. This unzipped FPL code should be semantically
                   the same as "zipping/hiding" the arguments by listing multiply justifications and only inferinig to the last argumentInference.
                c) possibly the structure of the to-be-proven predicate of the original theorem
            2) The evaluation of the argument should then handle the following cases
                a) whether or not the argumentInference of this argument is an FplAssume or FplRevoke 
                b) whether or not the justification is an axiom
                c) whether or not the justification is a rule of inference
                d) whether or not the justification is a by definition
        *)


    override this.EmbedInSymbolTable _ = 
        let (proof:FplProof) = this.ParentProof
        if proof.HasArgument (this.FplId) then 
            let conflict = proof.Scope[this.FplId]
            emitPR003Diagnostics this.FplId conflict.QualifiedStartPos this.StartPos this.EndPos 
        else 
            proof.Scope.Add(this.FplId, this)

    override this.RunOrder = Some _runOrder

    member this.ParentProof = this.Parent.Value :?> FplProof


and FplProof(positions: Positions, parent: FplValue, runOrder) =
    inherit FplGenericPredicateWithExpression(positions, parent)
    let _runOrder = runOrder
            
    override this.Name = LiteralPrfL
    override this.ShortName = LiteralPrf

    override this.Clone () =
        let ret = new FplProof((this.StartPos, this.EndPos), this.Parent.Value, _runOrder)
        this.AssignParts(ret)
        ret

    override this.IsFplBlock () = true
    override this.IsBlock () = true
    override this.IsProof (): bool = true

    override this.Type signatureType =
        let head = getFplHead this signatureType
        head

    member this.OrderedArguments =
        this.Scope.Values
        |> Seq.filter (fun fv -> fv.Name = PrimArgL)
        |> Seq.map (fun fv -> fv :?> FplArgument)
        |> Seq.sortBy (fun fv -> fv.RunOrder)

    member this.HasArgument argumentId = this.Scope.ContainsKey(argumentId)

    override this.Run variableStack = 
        this.Debug "Run"
        // tell the parent theorem-like statement that it has a proof
        let parent = this.Parent.Value 
        match box parent with 
        | :? IHasProof as parentWithProof ->
            parentWithProof.HasProof <- true
        | _ -> ()
        // evaluate the proof by evaluating all arguments according to their order in the FPL code
        let mutable allArgumentsEvaluateToTrue = true
        this.OrderedArguments
        |> Seq.iter (fun arg -> 
            arg.Run variableStack
            let argRepr = arg.Represent()
            allArgumentsEvaluateToTrue <- allArgumentsEvaluateToTrue && argRepr = LiteralTrue 
        )
        if not allArgumentsEvaluateToTrue then
            emitPR009Diagnostics this.StartPos this.StartPos

    override this.EmbedInSymbolTable _ = 
        /// Tries to find a theorem-like statement for a proof
        /// and returns different cases of ScopeSearchResult, depending on different semantical error situations.
        let tryFindAssociatedBlockForProof (fplValue: FplValue) =
            match fplValue.Parent with
            | Some theory ->

                let flattenedScopes = flattenScopes theory.Parent.Value

                let potentialProvableName = stripLastDollarDigit (fplValue.FplId)

                // The parent node of the proof is the theory. In its scope
                // we should find the theorem we are looking for.
                let buildingBlocksMatchingDollarDigitNameList =
                    // the potential block name of the proof is the
                    // concatenated type signature of the name of the proof
                    // without the last dollar digit
                    flattenedScopes |> List.filter (fun fv -> fv.FplId = potentialProvableName)

                let provableBlocklist =
                    buildingBlocksMatchingDollarDigitNameList
                    |> List.filter (fun fv -> isProvable fv)

                let notProvableBlocklist =
                    buildingBlocksMatchingDollarDigitNameList
                    |> List.filter (fun fv -> not (isProvable fv ))

                if provableBlocklist.Length > 1 then
                    ScopeSearchResult.FoundMultiple(
                        provableBlocklist
                        |> List.map (fun fv -> sprintf "'%s' %s" fv.Name (fv.Type(SignatureType.Mixed)))
                        |> String.concat ", "
                    )
                elif provableBlocklist.Length > 0 then
                    let potentialTheorem = provableBlocklist.Head
                    ScopeSearchResult.FoundAssociate potentialTheorem
                elif notProvableBlocklist.Length > 0 then
                    let potentialOther = notProvableBlocklist.Head

                    ScopeSearchResult.FoundIncorrectBlock(
                        sprintf "'%s' %s" potentialOther.Name (qualifiedName potentialOther)
                    )
                else
                    ScopeSearchResult.NotFound
            | None -> ScopeSearchResult.NotApplicable


        match tryFindAssociatedBlockForProof this with
        | ScopeSearchResult.FoundAssociate potentialParent -> 
            // everything is ok, change the parent of the provable from theory to the found parent 
            this.Parent <- Some potentialParent
        | ScopeSearchResult.FoundIncorrectBlock incorrectBlock ->
            emitID002Diagnostics this.FplId incorrectBlock this.StartPos this.EndPos
        | ScopeSearchResult.NotFound ->
            emitID003diagnostics this.FplId this.StartPos this.EndPos
        | _ -> ()
        tryAddToParentUsingFplId this

    override this.RunOrder = Some _runOrder


let getArgumentInProof (fv1:FplGenericJustificationItem) argName =
    let proof = 
        match fv1 with 
        | :? FplJustificationItemByProofArgument ->
            fv1.Scope.Values |> Seq.head :?> FplProof
        | _ ->
            let parent = fv1.ParentJustification
            let arg = parent.ParentArgument
            arg.ParentProof
    if proof.HasArgument argName then 
        Some proof.Scope[argName]
    else 
        None

type FplLocalization(positions: Positions, parent: FplValue) =
    inherit FplValue(positions, Some parent)

    override this.Name = LiteralLocL
    override this.ShortName = LiteralLoc

    override this.Clone () =
        let ret = new FplLocalization((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = 
        let head = getFplHead this signatureType
        let paramT =
            this.Scope
            |> Seq.filter (fun (kvp: KeyValuePair<string, FplValue>) -> isVar kvp.Value)
            |> Seq.map (fun (kvp: KeyValuePair<string, FplValue>) -> kvp.Value.Type(signatureType))
            |> String.concat ", "

        match paramT with
        | "" -> head
        | _ -> sprintf "%s(%s)" head paramT

    override this.Represent() = this.Type(SignatureType.Name)
        
    override this.IsBlock() = true

    override this.Run variableStack = 
        // todo implement run
        this.Debug "Run"

    override this.RunOrder = None

    override this.EmbedInSymbolTable _ = tryAddToParentUsingTypedSignature this

type FplTranslation(positions: Positions, parent: FplValue) =
    inherit FplValue(positions, Some parent)

    override this.Name = PrimTranslationL
    override this.ShortName = PrimTranslation

    override this.Clone () =
        let ret = new FplTranslation((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = 
        let head = getFplHead this signatureType
        let args =
            this.ArgList
            |> Seq.map (fun fv -> fv.Type(SignatureType.Name))
            |> String.concat " "

        sprintf "%s%s" head args

    override this.Represent () = this.FplId 

    override this.Run variableStack = 
        // todo implement run
        this.Debug "Run"

    override this.EmbedInSymbolTable _ = addExpressionToParentArgList this 

    override this.RunOrder = None

type FplLanguage(positions: Positions, parent: FplValue) =
    inherit FplValue(positions, Some parent)

    override this.Name = PrimLanguageL
    override this.ShortName = PrimLanguage

    override this.Clone () =
        let ret = new FplLanguage((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType =
        let head = getFplHead this signatureType
        head

    override this.Represent () = this.FplId

    override this.Run variableStack = 
        // todo implement run
        this.Debug "Run"

    override this.EmbedInSymbolTable _ = 
        let parent = this.Parent.Value
        if parent.Scope.ContainsKey(this.FplId) then 
            let conflict = parent.Scope[this.FplId]
            let oldDiagnosticsStopped = ad.DiagnosticsStopped
            ad.DiagnosticsStopped <- false
            emitID014Diagnostics this.FplId conflict.QualifiedStartPos this.StartPos this.EndPos 
            ad.DiagnosticsStopped <- oldDiagnosticsStopped
        else
            parent.Scope.Add(this.FplId, this)

    override this.RunOrder = None

let isLanguage (fv:FplValue) =
    match fv with
    | :? FplLanguage -> true
    | _ -> false

type FplAssertion(positions: Positions, parent: FplValue) =
    inherit FplValue(positions, Some parent)

    override this.Name = PrimAssertion
    override this.ShortName = LiteralAss

    override this.Clone () =
        let ret = new FplAssertion((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = this.FplId
        
    override this.Represent () = ""

    override this.Run variableStack = 
        // todo implement run
        this.Debug "Run"

    override this.EmbedInSymbolTable _ = addExpressionToParentArgList this 

    override this.RunOrder = None

type FplIntrinsicUndef(positions: Positions, parent: FplValue) as this =
    inherit FplValue(positions, Some parent)
    do 
        this.TypeId <- LiteralUndef
        this.FplId <- LiteralUndef

    override this.Name = PrimIntrinsicUndef
    override this.ShortName = LiteralUndef

    override this.Clone () =
        let ret = new FplIntrinsicUndef((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type (signatureType:SignatureType) = 
        getFplHead this signatureType
                    
    override this.Represent (): string = this.FplId

    override this.Run variableStack = 
        this.Debug "Run"

    override this.EmbedInSymbolTable _ = addExpressionToParentArgList this 

    override this.RunOrder = None

type ArgType = 
    | Brackets
    | Parentheses
    | Nothing

[<AbstractClass>]
type FplGenericReference(positions: Positions, parent: FplValue) =
    inherit FplValue(positions, Some parent)

    override this.Clone () = this // do not clone references to prevent stack overflow 

    override this.Run variableStack =
        this.Debug "Run"
        if this.Scope.Count > 0 then 
            let called = 
                this.Scope 
                |> Seq.map (fun kvp -> kvp.Value) 
                |> Seq.toList 
                |> List.head
            match called.Name with
            | LiteralCtorL
            | PrimBaseConstructorCall
            | PrimPredicateL
            | PrimFuncionalTermL
            | PrimMandatoryFunctionalTermL
            | PrimMandatoryPredicateL ->
                match box called with
                | :? ICanBeCalledRecusively as calledRecursively when calledRecursively.CallCounter > maxRecursion -> () // stop recursion
                | _ ->
                    let pars = variableStack.SaveVariables(called) 
                    let args = this.ArgList |> Seq.toList
                    // run all arguments before replacing variables with their values
                    args |> List.iter (fun arg -> arg.Run variableStack)
                    variableStack.ReplaceVariables pars args
                    // store the position of the caller
                    variableStack.CallerStartPos <- this.StartPos
                    variableStack.CallerEndPos <- this.EndPos
                    // run all statements of the called node
                    called.Run variableStack
                    this.SetValuesOf called
                    variableStack.RestoreVariables(called)
            | PrimDelegateDecrementL
            | PrimDelegateEqualL ->
                called.Run variableStack
                this.SetValuesOf called
            | _ -> ()
        elif this.ArgList.Count = 1 then
            let arg = this.ArgList[0]
            arg.Run variableStack

    override this.RunOrder = None

type FplReference(positions: Positions, parent: FplValue) =
    inherit FplGenericReference(positions, parent)
    let mutable _argType = ArgType.Nothing

    override this.Name = PrimRefL
    override this.ShortName = PrimRef

    override this.SetValue fv = 
        if this.Scope.ContainsKey(this.FplId) then
            let var = this.Scope[this.FplId]
            var.SetValue(fv)
        else
            base.SetValue(fv)

    /// Indicates if this Reference was followed by brackets, by parentheses, or by nothing in the FPL code.
    member this.ArgType
        with get () = _argType
        and set (value) = _argType <- value

    override this.Type signatureType =
        let headObj = 
            if this.Scope.Count > 0 && not (this.Scope.ContainsKey(".")) then 
                let ret = this.Scope.Values |> Seq.head
                match ret.Name with 
                | PrimExtensionObj 
                | LiteralParent 
                | LiteralSelf ->
                    if ret.Scope.Count > 0 then 
                        ret.Scope.Values |> Seq.head
                    else
                        ret   
                | _ -> ret
            else
                this
        let propagate = propagateSignatureType signatureType

        // The arguments are reserved for the arguments or the coordinates of the reference
        let args, argsCount =
            let ret = 
                this.ArgList
                |> Seq.map (fun fv -> fv.Type(propagate))
                |> String.concat ", "
            // fallback to variable params (and a possibly given mapping) if there are no arguments and the reference is a variable 
            match ret, headObj.Name with 
            | "", PrimVariableL -> 
                let countVarParams = 
                    headObj.Scope 
                    |> Seq.filter (fun (kvp: KeyValuePair<string, FplValue>) -> isSignatureVar kvp.Value || not (kvp.Value.IsClass()))
                    |> Seq.toList
                (getParamTuple headObj propagate), countVarParams.Length
            | _ -> ret, this.ArgList.Count

        let head = 
            let ret = 
                if headObj.Name = PrimExtensionL then 
                    headObj.Type signatureType
                elif headObj.ExpressionType.IsNoFix then
                    getFplHead headObj signatureType 
                else
                    headObj.ExpressionType.GetUserDefinedLiteral headObj.FplId
            match signatureType, ret, args with
            | SignatureType.Type, "", LiteralUndef -> ""
            | SignatureType.Type, "", "" -> LiteralUndef
            | _ -> ret
            
        let qualification =
            if this.Scope.ContainsKey(".") then
                Some(this.Scope["."])
            else
                None

        let fallBackFunctionalTerm =
            let varMappingOpt = getMapping headObj
            match varMappingOpt with 
            | Some varMapping ->
                match headObj.Name with 
                | PrimFuncionalTermL when signatureType = SignatureType.Type -> 
                    varMapping.Type propagate
                | PrimMandatoryFunctionalTermL when signatureType = SignatureType.Type -> 
                    varMapping.Type propagate
                | _ -> 
                    $"{head}({args}) -> {varMapping.Type propagate}"
            | None ->
                $"{head}({args})"

        match argsCount, this.ArgType, qualification with
        | 0, ArgType.Nothing, Some qual ->
            $"{head}.{qual.Type(propagate)}"
        | 0, ArgType.Brackets, Some qual ->
            $"{head}[].{qual.Type(propagate)}"
        | 0, ArgType.Parentheses, Some qual ->
            $"{head}().{qual.Type(propagate)}"
        | 0, ArgType.Nothing, None -> 
            match headObj.Name with 
            | PrimVariableArrayL -> $"{headObj.Type signatureType}"
            | _ -> head
        | 0, ArgType.Brackets, None ->
            $"{head}[]"
        | 0, ArgType.Parentheses, None ->
            fallBackFunctionalTerm
        | 1, ArgType.Nothing, Some qual -> 
            $"{head}{args}.{qual.Type(propagate)}"
        | 1, ArgType.Brackets, Some qual ->
            $"{head}[{args}].{qual.Type(propagate)}"
        | 1, ArgType.Parentheses, Some qual ->
            $"{head}({args}).{qual.Type(propagate)}"
        | 1, ArgType.Nothing, None -> 
            if this.FplId <> String.Empty then 
                fallBackFunctionalTerm
            else
                $"{head}{args}"
        | 1, ArgType.Brackets, None ->
            $"{head}[{args}]"
        | 1, ArgType.Parentheses, None ->
            $"{head}({args})"
        | _, ArgType.Nothing, Some qual -> 
            $"{head}({args}).{qual.Type(propagate)}"
        | _, ArgType.Brackets, Some qual ->
            $"{head}[{args}].{qual.Type(propagate)}"
        | _, ArgType.Parentheses, Some qual ->
            $"{head}({args}).{qual.Type(propagate)}"
        | _, ArgType.Nothing, None -> 
            $"{head}({args})"
        | _, ArgType.Brackets, None ->
            $"{head}[{args}]"
        | _, ArgType.Parentheses, None ->
            fallBackFunctionalTerm

    override this.Represent () = 
        if this.ValueList.Count = 0 then 
            if this.Scope.Count > 0 && not (this.Scope.ContainsKey(".")) then 
                (this.Scope.Values |> Seq.head).Represent()
            else

                let args, argsCount =
                    let ret = 
                        this.ArgList
                        |> Seq.map (fun fv -> fv.Represent())
                        |> String.concat ", "
                    ret, this.ArgList.Count

                let qualification =
                    if this.Scope.ContainsKey(".") then
                        Some(this.Scope["."])
                    else
                        None

                match argsCount, this.ArgType, qualification with
                | 0, ArgType.Nothing, Some qual ->
                    $"{LiteralUndef}.{qual.Represent()}"
                | 0, ArgType.Brackets, Some qual ->
                    $"{LiteralUndef}[].{qual.Represent()}"
                | 0, ArgType.Parentheses, Some qual ->
                    $"{LiteralUndef}().{qual.Represent()}"
                | 0, ArgType.Nothing, None -> 
                    $"{LiteralUndef}"
                | 0, ArgType.Brackets, None ->
                    $"{LiteralUndef}[]"
                | 0, ArgType.Parentheses, None ->
                    $"{LiteralUndef}()"
                | 1, ArgType.Nothing, Some qual -> 
                
                    $"{LiteralUndef}{args}.{qual.Represent()}"
                | 1, ArgType.Brackets, Some qual ->
                    $"{LiteralUndef}[{args}].{qual.Represent()}"
                | 1, ArgType.Parentheses, Some qual ->
                    $"{LiteralUndef}({args}).{qual.Represent()}"
                | 1, ArgType.Nothing, None -> 
                    if this.FplId <> String.Empty then 
                        $"{LiteralUndef}({args})"
                    else
                        $"{args}"
                | 1, ArgType.Brackets, None ->
                    $"{LiteralUndef}[{args}]"
                | 1, ArgType.Parentheses, None ->
                    $"{LiteralUndef}({args})"
                | _, ArgType.Nothing, Some qual -> 
                    $"{LiteralUndef}({args}).{qual.Represent()}"
                | _, ArgType.Brackets, Some qual ->
                    $"{LiteralUndef}[{args}].{qual.Represent()}"
                | _, ArgType.Parentheses, Some qual ->
                    $"{LiteralUndef}({args}).{qual.Represent()}"
                | _, ArgType.Nothing, None -> 
                    $"{LiteralUndef}({args})"
                | _, ArgType.Brackets, None ->
                    $"{LiteralUndef}[{args}]"
                | _, ArgType.Parentheses, None ->
                    $"{LiteralUndef}({args})"                
                
        else
            let subRepr = 
                this.ValueList
                |> Seq.filter (fun subfv -> subfv <> this) // prevent reference "self" being the value of itself causing an infinite loop
                |> Seq.map (fun subfv -> subfv.Represent())
                |> String.concat ", "
            if subRepr = String.Empty then 
                LiteralUndef
            else
                subRepr           
    override this.EmbedInSymbolTable nextOpt = 
       
        match nextOpt with 
        | Some next when next.Name = LiteralLocL -> 
            next.FplId <- this.FplId
            next.TypeId <- this.FplId
            next.EndPos <- this.EndPos
        | Some next when next.IsBlock() ->
            addExpressionToParentArgList this 
        | Some next when next.Scope.ContainsKey(".") -> 
            next.EndPos <- this.EndPos
        | Some next -> 
            addExpressionToParentArgList this
            next.EndPos <- this.EndPos
        | _ -> ()

    /// eturns the optional node referenced by this FplReference
    member this.TryReferenced = this.Scope.Values |> Seq.tryHead
            

/// Checks if a reference to a Symbol, Prefix, PostFix, or Infix exists
let checkSIG01Diagnostics (fv: FplValue)  =
    match fv with
    | :? FplReference ->
        // collect candidates to match this reference from all theories and
        // add them to fplValues's scope
        let expressionId = fv.FplId

        (getRoot fv).Scope
        |> Seq.map (fun kv -> kv.Value)
        |> Seq.iter (fun theory ->
            theory.Scope
            |> Seq.map (fun kv -> kv.Value)
            |> Seq.iter (fun block ->
                if expressionId = block.FplId then
                    let blockType = block.Type(SignatureType.Mixed)
                    fv.Scope.Add(blockType, block)
                    fv.TypeId <- block.TypeId
                else
                    let blockType = block.Type(SignatureType.Mixed)
                    match block.ExpressionType with
                    | FixType.Prefix symbol
                    | FixType.Symbol symbol
                    | FixType.Postfix symbol ->
                        if expressionId = symbol then
                            fv.Scope.Add(blockType, block)
                            fv.TypeId <- block.TypeId
                    | FixType.Infix(symbol, precedence) ->
                        if expressionId = symbol then
                            fv.Scope.Add(blockType, block)
                            fv.TypeId <- block.TypeId
                    | _ -> ()))

        if fv.Scope.Count = 0 then
            emitSIG01Diagnostics expressionId fv.StartPos fv.EndPos
    | _ -> ()

/// Gets the list of arguments of an FplValue if any
let getArguments (fv:FplValue) =
    fv.ArgList 
    |> Seq.toList

type FplMapping(positions: Positions, parent: FplValue) =
    inherit FplValue(positions, Some parent)
    let _dimensionTypes = new List<FplValue>()
    let mutable _dimensionTypesBeingSet = false
    let mutable _isArrayMapping = false
    let mutable (_toBeReturnedClass:FplValue option) = None

    /// If the mapping maps to classes or arrays of classes,
    /// this function will yield an optional reference to the corresponding class node.
    member this.ToBeReturnedClass
        with get() = _toBeReturnedClass
        and set(value) = _toBeReturnedClass <- value

    /// Sets this mapping to an array-typed mapping.
    member this.SetIsArray() = _isArrayMapping <- true

    member this.Dimension = _dimensionTypes.Count

    member this.DimensionTypes = _dimensionTypes

    /// Sets the during the symbol table construction.
    /// Because the type consists of a main type and index allowed-types, we use "Dimension being set" as a flag
    /// to decide which one to be set.
    member this.SetType (typeId:string) pos1 pos2 = 
        if not _dimensionTypesBeingSet then 
            this.TypeId <-
                if _isArrayMapping then 
                    $"*{typeId}"
                else
                    typeId
            _dimensionTypesBeingSet <- true
        else
            let indexAllowedType = FplMapping((pos1,pos2), this) 
            indexAllowedType.TypeId <- typeId
            this.DimensionTypes.Add indexAllowedType

    interface IHasDimensions with
        member this.Dimension = this.Dimension
        member this.DimensionTypes = this.DimensionTypes
        member this.SetType typeId pos1 pos2 = this.SetType typeId pos1 pos2

    override this.Name = PrimMappingL
    override this.ShortName = PrimMapping

    override this.Clone () =
        let ret = new FplMapping((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.IsMapping () = true

    override this.Type signatureType = 
        let pars = getParamTuple this signatureType
        let propagate = propagateSignatureType signatureType

        let myMapping = 
            if this.ArgList.Count > 0 then 
                let arg = this.ArgList[0]
                match arg with 
                | :? FplMapping ->
                    Some(arg)
                | _ -> None
            else
                None
        let mainType = 
            match (pars, myMapping) with
            | ("", None) -> this.TypeId
            | ("", Some map) -> sprintf "%s() -> %s" this.TypeId (map.Type(propagate))
            | (_, None) -> sprintf "%s(%s)" this.TypeId pars
            | (_, Some map) -> sprintf "%s(%s) -> %s" this.TypeId pars (map.Type(propagate))

        if not _isArrayMapping then
            mainType
        else
            let dimensionTypes = 
                this.DimensionTypes
                |> Seq.map (fun fv -> fv.Type signatureType)
                |> String.concat ","
            $"{mainType}[{dimensionTypes}]"


    override this.Represent() = $"dec {this.Type(SignatureType.Type)}"

    override this.Run _ = 
        this.Debug "Run"

    override this.EmbedInSymbolTable _ = addExpressionToParentArgList this 

    override this.RunOrder = None

type FplVariableArray(fplId, positions: Positions, parent: FplValue) =
    inherit FplGenericVariable(fplId, positions, parent)
    let _dimensionTypes = new List<FplValue>()
    let mutable _dimensionTypesBeingSet = false

    member this.Dimensionality = _dimensionTypes.Count

    member this.DimensionTypes = _dimensionTypes

    /// Sets the during the symbol table construction.
    /// Because the type consists of a main type and index allowed-types, we use "Dimension being set" as a flag
    /// to decide which one to be set.
    member this.SetType (typeId:string) pos1 pos2 = 
        if not _dimensionTypesBeingSet then 
            this.TypeId <- $"*{typeId}"
            _dimensionTypesBeingSet <- true
        else
            let indexAllowedType = FplMapping((pos1,pos2), this) 
            indexAllowedType.TypeId <- typeId
            this.DimensionTypes.Add indexAllowedType

    interface IHasDimensions with
        member this.Dimension = this.Dimensionality
        member this.DimensionTypes = this.DimensionTypes
        member this.SetType typeId pos1 pos2 = this.SetType typeId pos1 pos2

    override this.Name = PrimVariableArrayL

    override this.ShortName = PrimVariableArray

    override this.Clone () =
        let ret = new FplVariableArray(this.FplId, (this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        if this.IsBound then 
            ret.SetIsBound()
        if this.IsUsed then 
            ret.SetIsUsed()
        ret.IsSignatureVariable <- this.IsSignatureVariable 
        ret.IsInitialized <- this.IsInitialized

        this.DimensionTypes
        |> Seq.iter (fun (fv1:FplValue) ->
            let value = fv1.Clone()
            ret.DimensionTypes.Add(value))

        ret

    override this.Type signatureType =
        let mainType = base.Type signatureType

        let dimensionTypes = 
            this.DimensionTypes
            |> Seq.map (fun fv -> fv.Type signatureType)
            |> String.concat ","

        match signatureType with
        | SignatureType.Name -> this.FplId
        | _ -> $"{mainType}[{dimensionTypes}]"

    override this.Represent () = 
        if this.ValueList.Count = 0 then
            if this.IsInitialized then 
                // this case should never happen, because isInitializesVariable is a contradiction to ValueList.Count 0
                LiteralUndef
            else
                match this.TypeId with
                | LiteralUndef -> LiteralUndef
                | _ -> $"dec {this.Type SignatureType.Type}" 
        else
            let subRepr = 
                this.ValueList
                |> Seq.map (fun subfv -> subfv.Represent())
                |> String.concat ", "
            if this.IsInitialized then 
                subRepr
            else
                match this.TypeId with
                | LiteralUndef -> LiteralUndef
                | _ -> $"dec {this.Type(SignatureType.Type)}" 

    override this.CheckConsistency () = 
        base.CheckConsistency()

type FplVariable(fplId, positions: Positions, parent: FplValue) =
    inherit FplGenericVariable(fplId, positions, parent)


    override this.Name = PrimVariableL

    override this.ShortName = PrimVariable

    override this.Clone () =
        let ret = new FplVariable(this.FplId, (this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        if this.IsBound then 
            ret.SetIsBound()
        if this.IsUsed then 
            ret.SetIsUsed()
        ret.IsSignatureVariable <- this.IsSignatureVariable 
        ret.IsInitialized <- this.IsInitialized
        ret

    override this.SetValue fv =
        base.SetValue(fv)
        if fv.FplId <> LiteralUndef then
            this.IsInitialized <- true

    override this.Represent () = 
        if this.ValueList.Count = 0 then
            if this.IsInitialized then 
                // this case should never happen, because isInitializesVariable is a contradiction to ValueList.Count 0
                LiteralUndef
            else
                match this.TypeId with
                | LiteralUndef -> LiteralUndef
                | LiteralPred -> PrimUndetermined
                | _ -> $"dec {this.Type(SignatureType.Type)}" 
        else
            let subRepr = 
                this.ValueList
                |> Seq.map (fun subfv -> subfv.Represent())
                |> String.concat ", "
            if this.IsInitialized || this.IsBound then 
                subRepr
            else
                match this.TypeId with
                | LiteralUndef -> LiteralUndef
                | _ -> $"dec {this.Type(SignatureType.Type)}" 


/// Gets the list of parameters of an FplValue if any
let getParameters (fv:FplValue) =
    match fv.Name with
    | PrimVariableL ->
        fv.Scope.Values |> Seq.toList
    | PrimVariableArrayL ->
        match box fv with 
        | :? IHasDimensions as arr -> arr.DimensionTypes |> Seq.toList
        | _ -> []
    | PrimFuncionalTermL
    | PrimPredicateL
    | LiteralCtorL
    | PrimMandatoryPredicateL
    | PrimMandatoryFunctionalTermL ->
        fv.Scope.Values |> Seq.filter (fun fv -> isSignatureVar fv) |> Seq.toList
    | _ -> []

/// Checks, if an FplValue uses parentheses or brackets
let hasBracketsOrParentheses (fv:FplValue) = 
    match fv.Name with 
    | PrimVariableL ->
        let vars = fv.GetVariables()
        vars.Length > 0
    | PrimFuncionalTermL 
    | PrimPredicateL 
    | LiteralCtorL 
    | PrimDefaultConstructor 
    | PrimMandatoryPredicateL
    | PrimMandatoryFunctionalTermL -> true
    | PrimRefL -> 
        let refFv = fv :?> FplReference
        (refFv.ArgType = ArgType.Parentheses || refFv.ArgType = ArgType.Brackets)
    | _ -> false

/// Checks if the baseClassName is contained in the classRoot's base classes (it derives from).
/// If so, the function will produce Some path where path equals a string of base classes concatenated by ":".
/// The classRoot is required to have an FplValueType.Class.
let findInheritanceChains (baseNode: FplValue) =
    let distinctNames = HashSet<string>()
    let paths = Dictionary<string,string>() // collects all paths (keys) and errors (values)
    let predecessors = Dictionary<string,List<string>>() // inner dictionary = predecessors

    let rec findChains (bNode: FplValue) predecessorName accPath =
        let currName = bNode.FplId
        let newPath = 
            if accPath = String.Empty then 
                currName
            else
                $"{accPath}:{currName}" 
        match distinctNames.Contains currName with
        | true -> // a cross-inheritance between two paths or a cycle detected
            predecessors[currName].Add predecessorName
            if predecessors[currName].Count = 1 then 
                // a cycle detected since currNode had only one predecessor so far
                // and thus, it must be the first one
                paths[newPath] <- $"cycle detected" 
            else
                // a cross-inheritance
                let cross = predecessors[currName] |> Seq.distinct |> String.concat "` and `"
                if cross.Contains " and " then 
                    paths[newPath] <- $"cross-inheritance not supported, `{currName}` is base for `{cross}`."
                else 
                    paths[newPath] <- $"duplicate inheritance from `{currName}` detected." 
        | false -> // a node encountered the very first time
            // add node name to distinct names
            distinctNames.Add currName |> ignore
            // add predecessor to node name
            predecessors.Add (currName, List<string>())
            predecessors[currName].Add predecessorName
            match baseNode.Name, bNode.Name with 
            | PrimFuncionalTermL, PrimFuncionalTermL 
            | PrimClassL, PrimClassL ->
                bNode.ArgList
                |> Seq.filter (fun subNode -> subNode :? FplBase)
                |> Seq.iter (fun subNode ->
                    findChains subNode currName newPath 
                )
            | PrimFuncionalTermL, LiteralBase 
            | PrimClassL, LiteralBase ->
                if bNode.Scope.Count > 0 then
                    let nextBNode = bNode.Scope.Values |> Seq.head
                    let baseNodes = 
                        nextBNode.ArgList
                        |> Seq.filter (fun subNode -> subNode :? FplBase)
                        |> Seq.toList
                    if baseNodes.Length > 0 then 
                        baseNodes
                        |> List.iter (fun subNode ->
                            findChains subNode currName newPath 
                        )
                    elif paths.ContainsKey newPath then 
                        paths[newPath] <- $"duplicate inheritance detected, `{newPath}`." 
                    else
                        paths.Add (newPath, "ok")
                else 
                    if paths.ContainsKey newPath then 
                        paths[newPath] <- $"duplicate inheritance detected, `{newPath}`." 
                    else
                        paths.Add (newPath, "ok")
            | _ -> ()
            
    match baseNode.Name with 
    | PrimClassL
    | PrimFuncionalTermL -> ()
    | _ -> failwith ($"Expecting a class or a functional term node, got {baseNode.Name}")
    
    findChains baseNode "" ""
    if paths.Count = 0 then 
        distinctNames |> Seq.iter (fun s -> paths.Add (s, "ok"))
    paths


/// Checks if a node inherits from some type.
let inheritsFrom (node:FplValue) someType = 
    match node, someType with 
    | :? FplClass, "obj" -> true
    | _ -> 
        let inheritanceList = findInheritanceChains node 
        let inheritanceFound = 
            inheritanceList 
            |> Seq.filter (fun kvp -> 
                kvp.Value = "ok" && 
                (
                    kvp.Key.EndsWith $":{someType}" 
                || kvp.Key.Contains $":{someType}:"
                )
            )
            |> Seq.tryLast
        match inheritanceFound with 
        | Some _ -> true
        | None -> false

/// Tries to match parameters of an FplValue with its arguments recursively
let rec mpwa (args: FplValue list) (pars: FplValue list) =
    let matchClassInheritance (clOpt:FplValue option) (a:FplValue) aType (p:FplValue) pType = 
        match clOpt with 
        | Some cl -> 
            if inheritsFrom cl pType then 
                None
            else
                Some($"`{a.Type(SignatureType.Name)}:{aType}` matches neither `{p.Type(SignatureType.Name)}:{pType}` nor the base classes")
        | _ -> None

    match (args, pars) with
    | (a :: ars, p :: prs) ->
        let aType = a.Type SignatureType.Type
        let pType = p.Type SignatureType.Type

        if aType = pType then
            mpwa ars prs
        elif pType.StartsWith(LiteralTpl) || pType.StartsWith(LiteralTplL) then
            mpwa ars prs
        elif pType = $"*{aType}[{LiteralInd}]" then
            // only array parameters indexed with the FPL-inbuilt index type that also 
            // match the argument's type will accept variadic enumerations of such arguments 
            if ars.Length > 0 then 
                mpwa ars pars 
            else 
                None
        elif pType.StartsWith($"*{aType}[") then
            // array parameters with indexes that differ from the FPL-inbuilt index type  
            // or with multidimensional index types will not accept variadic enumerations of arguments
            // even if they have the same type used for the values of the array
            let aName = a.Type(SignatureType.Name)
            Some($"variadic enumeration of `{aName}:{aType}` does not match `{p.Type(SignatureType.Name)}:{pType}`, try `{aName}:{pType}` as argument or use parameter `{p.Type(SignatureType.Name)}:{p.TypeId}[{LiteralInd}]`")
        elif aType.StartsWith($"*{pType}[") && a.Name = PrimRefL then
            let refA = a :?> FplReference
            if refA.ArgType = ArgType.Brackets then 
                // some array elements matching parameter type
                None
            else
                Some($"Array type `{a.Type(SignatureType.Name)}:{aType}` does not match `{p.Type(SignatureType.Name)}:{pType}`")
        elif aType.Length > 0 && Char.IsUpper(aType[0]) && a.Name = PrimRefL && a.Scope.Count = 1 then
            let aReferencedNode = a.Scope.Values |> Seq.toList |> List.head
            if aReferencedNode.Scope.Count > 0 then
                let cl = aReferencedNode.Scope.Values |> Seq.head
                match cl with
                | :? FplClass ->
                    if inheritsFrom cl pType then 
                        mpwa ars prs
                    else
                        Some($"`{a.Type(SignatureType.Name)}:{aType}` neither matches `{p.Type(SignatureType.Name)}:{pType}` nor the base classes")
                | _ ->
                    // this case does not occur but we cover it for completeness reasons
                    Some($"`{a.Type(SignatureType.Name)}:{aType}` is undefined and doesn't match `{p.Type(SignatureType.Name)}:{pType}`")
            elif aReferencedNode.Name = PrimDefaultConstructor then 
                let defaultCtor = aReferencedNode :?> FplDefaultConstructor
                matchClassInheritance defaultCtor.ToBeConstructedClass a aType p pType
            elif aReferencedNode.Name = PrimFuncionalTermL || aReferencedNode.Name = PrimMandatoryFunctionalTermL then 
                let mapOpt = getMapping aReferencedNode
                let map = mapOpt.Value :?> FplMapping 
                matchClassInheritance map.ToBeReturnedClass a aType p pType
            else
                Some($"`{a.Type(SignatureType.Name)}:{aType}` is undefined and does not match `{p.Type(SignatureType.Name)}:{pType}`")
        elif aType.StartsWith(pType + "(") then
            None
        elif aType = LiteralPred && pType.StartsWith(LiteralPred) then
            None
        elif aType = LiteralFunc && pType.StartsWith(LiteralFunc) then
            None
        elif aType.StartsWith(LiteralFunc) then
            let someMap = getMapping a

            match someMap with
            | Some map -> mpwa [ map ] [ p ]
            | _ -> Some($"`{a.Type(SignatureType.Name)}:{aType}` does not match `{p.Type(SignatureType.Name)}:{pType}`")
        else
            Some($"`{a.Type(SignatureType.Name)}:{aType}` does not match `{p.Type(SignatureType.Name)}:{pType}`")
    | ([], p :: prs) ->
        let pType = p.Type(SignatureType.Type)
        match p with 
        | :? FplClass as cl ->
            let constructors = cl.GetConstructors()
            if constructors.Length = 0 then
                None
            else
                Some($"missing argument for `{p.Type(SignatureType.Name)}:{pType}`")
        | _ when p.Name = PrimVariableArrayL ->
            None
        | _ -> 
            Some($"missing argument for `{p.Type(SignatureType.Name)}:{pType}`")
    | (a :: [], []) ->
        let aType = a.Type(SignatureType.Type)
        Some($"no matching parameter for `{a.Type(SignatureType.Name)}:{aType}`")
    | (a :: ars, []) ->
        let aType = a.Type(SignatureType.Type)
        Some($"no matching parameter for `{a.Type(SignatureType.Name)}:{aType}`")
    | ([], []) -> None


/// Tries to match the arguments of `fva` FplValue with the parameters of the `fvp` FplValue and returns
/// Some(specific error message) or None, if the match succeeded.
let matchArgumentsWithParameters (fva: FplValue) (fvp: FplValue) =
    let parameters = getParameters fvp
    let arguments = getArguments fva

    let aHasBracketsOrParentheses = hasBracketsOrParentheses fva
    let pHasBracketsOrParentheses = hasBracketsOrParentheses fvp
        

    let argResult = 
        if aHasBracketsOrParentheses <> pHasBracketsOrParentheses && arguments.Length = 0 && parameters.Length = 0 then 
            Some($"calling and called nodes have mismatching use of parentheses")
        else
            mpwa arguments parameters

    match argResult with
    | Some aErr -> 
        match fvp.Name with 
        | PrimVariableArrayL ->
            Some($"{aErr} in {qualifiedName fvp}:{fvp.Type SignatureType.Type}")
        | _ -> 
            Some($"{aErr} in {qualifiedName fvp}")
    | None -> None

/// Tries to match the signatures of toBeMatched with the signatures of all candidates and accoumulates any
/// error messages in accResultList.
let rec checkCandidates (toBeMatched: FplValue) (candidates: FplValue list) (accResultList: string list) =
    match candidates with
    | [] -> (None, accResultList)
    | candidate :: candidates ->
        match matchArgumentsWithParameters toBeMatched candidate with
        | None -> (Some candidate, [])
        | Some errMsg -> checkCandidates toBeMatched candidates (accResultList @ [ errMsg ])

/// Checks if there is a candidate among the candiedates that matches the signature of a calling FplValue and returns this as an option.
let checkSIG04Diagnostics (calling:FplValue) (candidates: FplValue list) = 
    match checkCandidates calling candidates [] with
    | (Some candidate,_) -> Some candidate // no error occured
    | (None, errList) -> 
        emitSIG04Diagnostics (calling.Type SignatureType.Mixed) candidates.Length errList calling.StartPos calling.EndPos
        None

/// Checks if a reference to an array matches its dimensions (in terms of number and types)
let checkSIG08_SIG10Diagnostics (referenceToArray:FplValue) =
    let rec matchIndexesWithDimensions (refToArray:FplReference) =
        match refToArray.TryReferenced with
        | Some (:? FplVariableArray as varArray) ->
            let rec matchAllIndexes (indexes:FplValue list) (dims:FplValue list) dimNumber =
                match indexes, dims with
                | i::ixs, d::dms ->
                    match mpwa [i] [d] with
                    | Some errMsg ->
                        // type mismatch between dimension and index
                        emitSIG08diagnostics varArray.FplId i.FplId (i.Type SignatureType.Type) (d.Type SignatureType.Type) dimNumber i.StartPos i.EndPos 
                        matchAllIndexes ixs dms (dimNumber + 1) 
                    | _ -> matchAllIndexes ixs dms (dimNumber + 1) 
                | [], d::dms -> 
                    // missing index for dimension dimOrdinal
                    emitSIG09diagnostics varArray.FplId (d.Type SignatureType.Type) dimNumber d.StartPos d.EndPos
                    matchAllIndexes [] dms (dimNumber + 1) 
                | i::ixs, [] -> 
                    // array has less dimensions, index at dimOrdinal not supported
                    emitSIG10diagnostics varArray.FplId (i.FplId) dimNumber i.StartPos i.EndPos
                    matchAllIndexes ixs [] (dimNumber + 1)  
                | [], [] -> ()

            let dims = varArray.DimensionTypes |> Seq.toList
            let indexes = refToArray.ArgList |> Seq.toList
            matchAllIndexes indexes dims 1
        | _ -> ()
    match referenceToArray with 
    | :? FplReference as refToArray -> matchIndexesWithDimensions refToArray
    | _ -> ()


type FplBaseConstructorCall(positions: Positions, parent: FplValue) as this =
    inherit FplGenericReference(positions, parent)

    do 
        this.FplId <- LiteralObj
        this.TypeId <- LiteralObj

    override this.Name = PrimBaseConstructorCall
    override this.ShortName = PrimStmt

    override this.Type signatureType = 
        let head = getFplHead this signatureType
        let propagate = propagateSignatureType signatureType
        let args =
            this.ArgList
            |> Seq.map (fun fv -> fv.Type propagate)
            |> String.concat ", "
        sprintf "%s(%s)" head args

    override this.Represent () = LiteralUndef

    override this.CheckConsistency() = 
        base.CheckConsistency()

        // Check the base constructor call's id is the same as one of the classes this class is derived from,
        let outerClassOpt = this.UltimateBlockNode
        let enclosingConstructorOpt = this.NextBlockNode

        let registerParentConstructor() =
            match enclosingConstructorOpt with 
            | Some (:? FplConstructor as ctor) ->
                if ctor.ParentConstructorCalls.Contains(this.FplId) then 
                    // todo duplicate constructor call
                    emitID021Diagnostics this.FplId this.StartPos
                else
                    ctor.ParentConstructorCalls.Add this.FplId |> ignore
            | _ -> ()

        match outerClassOpt with
        | Some (:? FplClass as outerClass) ->
            let baseClassObjectOpt = 
                outerClass.ArgList 
                |> Seq.filter (fun pc -> pc.FplId = this.FplId)
                |> Seq.tryHead
                |> Option.map (fun (pc:FplValue) -> pc :?> FplBase)

            match baseClassObjectOpt with 
            | Some baseClassObject ->
                match baseClassObject.BaseClass with
                | Some baseClass ->
                    // now, try to match a constructor of the parentClass based on the signature of this base constructor call
                    match baseClass.IsIntrinsic, this.ArgList.Count with
                    | true, 0 ->
                        // call of a constructor of an intrinsic class (i.e., that is missing any constructor) with 0 paramters
                        // add "default constructor reference"
                        let defaultConstructor = new FplDefaultConstructor(baseClass.FplId, (this.StartPos, this.EndPos), this)
                        defaultConstructor.EmbedInSymbolTable defaultConstructor.Parent
                        defaultConstructor.ToBeConstructedClass <- Some baseClass
                        registerParentConstructor()
                    | true, _ ->
                        // the call uses parameters that are not possible for calling a non-existing constructor 
                        // obj() or an intrinsic class
                        emitID022Diagnostics baseClass.FplId this.StartPos this.EndPos
                    | false, _ ->
                        let parentClass = baseClass :?> FplClass
                        let candidates = parentClass.GetConstructors()
                        match checkSIG04Diagnostics this candidates with
                        | Some candidate ->
                            let name = candidate.Type SignatureType.Mixed
                            this.Scope.TryAdd(name, candidate) |> ignore
                        | None -> ()
                        registerParentConstructor()
                | None ->
                    // the base constructor call's id is not among the base classes this class is derived from
                    let candidates = outerClass.ArgList |> Seq.map (fun fv -> fv.FplId) |> Seq.sort |> String.concat ", "
                    emitID017Diagnostics this.FplId candidates this.StartPos this.EndPos
            | _ ->
                    emitID017Diagnostics this.FplId "" this.StartPos this.EndPos
                    registerParentConstructor()
        | _ ->
            // this case never happens, 
            // if so the bug will become apparent by failing to call the parent class constructor
            () 


    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        addExpressionToParentArgList this


/// Reference to "parent" using the FPL parent keyword. 
// It will point to a parent only inside FPL properties. Otherwise, it is undefined
type FplParent(positions: Positions, parent: FplValue) as this =
    inherit FplValue(positions, Some parent)

    do 
        this.FplId <- LiteralParent
        this.TypeId <- LiteralUndef

    override this.Name = LiteralParent
    override this.ShortName = LiteralParent

    override this.Clone () =
        let ret = new FplParent((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = 
        if this.Scope.Count > 1 then 
            (this.Scope.Values |> Seq.head).Type signatureType
        else 
            LiteralParent

    override this.Represent()= 
        if this.Scope.Count > 1 then 
            (this.Scope.Values |> Seq.head).Represent()
        else 
            LiteralUndef

    override this.Run variableStack = 
        // todo implement run
        this.Debug "Run"

    override this.EmbedInSymbolTable _ = addExpressionToReference this

    override this.RunOrder = None

/// Reference to "self" using the FPL self keyword. 
// It will point to the enclosing block inside FPL predicate definitions, functional terms, and properties. Otherwise, it is undefined.
type FplSelf(positions: Positions, parent: FplValue) as this =
    inherit FplValue(positions, Some parent)

    do 
        this.FplId <- LiteralSelf
        this.TypeId <- LiteralUndef

    override this.Name = LiteralSelf
    override this.ShortName = LiteralSelf

    override this.Clone () =
        let ret = new FplSelf((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = 
        if this.Scope.Count > 1 then 
            (this.Scope.Values |> Seq.head).Type signatureType
        else 
            LiteralSelf

    override this.Represent()= 
        if this.Scope.Count > 1 then 
            (this.Scope.Values |> Seq.head).Represent()
        else 
            LiteralUndef

    override this.Run variableStack = 
        // todo implement run
        this.Debug "Run"

    override this.EmbedInSymbolTable _ = addExpressionToReference this

    override this.RunOrder = None


/// Checks if an argument of an FplValue is a predicate and issues LG001Diagnostics if its not.
let checkArgPred (fv:FplValue) (arg:FplValue)  = 
    let argType = arg.Type SignatureType.Type
    let argName = arg.Type SignatureType.Name
    let repr = arg.Represent()
    match repr with
    | LiteralTrue
    | LiteralFalse 
    | PrimUndetermined -> () 
    | _ -> emitLG001Diagnostics argType argName fv.Name arg.StartPos arg.EndPos


/// Checks if an argument points to a free variable and if so, issues VAR09 diagnostics.
let checkFreeVar (arg:FplValue) = 
    if arg.Scope.ContainsKey(arg.FplId) then
        let ref = arg.Scope[arg.FplId]
        match ref with 
        | :? FplGenericVariable as var ->
            if not var.IsBound then 
                emitVAR09diagnostics var.FplId var.TypeId var.StartPos var.EndPos
        | _ -> ()

/// Implements the semantics of an FPL conjunction compound predicate.
type FplConjunction(positions: Positions, parent: FplValue) as this =
    inherit FplGenericPredicate(positions, parent)

    do 
        this.FplId <- LiteralAnd

    override this.Name = PrimConjunction
    override this.ShortName = LiteralAnd

    override this.Clone () =
        let ret = new FplConjunction((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = 
        let head = getFplHead this signatureType
        let args = 
            this.ArgList
            |> Seq.map (fun arg -> arg.Type(signatureType))
            |> String.concat ", "
        sprintf "%s(%s)" head args

    override this.Run variableStack =
        this.Debug "Run"
        let arg1 = this.ArgList[0]
        let arg2 = this.ArgList[1]
        arg1.Run variableStack
        arg2.Run variableStack
        let arg1Repr = arg1.Represent()
        let arg2Repr = arg2.Represent()
        let newValue =  new FplIntrinsicPred((this.StartPos, this.EndPos), this)
        newValue.FplId <-
            // FPL truth-table
            match (arg1Repr, arg2Repr) with
            | (LiteralFalse, _) 
            | (_, LiteralFalse)  -> 
                LiteralFalse
            | (LiteralTrue, LiteralTrue) -> 
                LiteralTrue
            | _ -> PrimUndetermined
        this.SetValue(newValue)

    override this.CheckConsistency() = 
        base.CheckConsistency() 
        let arg1 = this.ArgList[0]
        let arg2 = this.ArgList[1]
        checkArgPred this arg1
        checkArgPred this arg2
        checkFreeVar arg1
        checkFreeVar arg2


    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        addExpressionToParentArgList this


/// Implements the semantics of an FPL disjunction compound predicate.
type FplDisjunction(positions: Positions, parent: FplValue) as this =
    inherit FplGenericPredicate(positions, parent)

    do 
        this.FplId <- LiteralOr

    override this.Name = PrimDisjunction
    override this.ShortName = LiteralOr

    override this.Clone () =
        let ret = new FplDisjunction((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = 
        let head = getFplHead this signatureType
        let args = 
            this.ArgList
            |> Seq.map (fun arg -> arg.Type(signatureType))
            |> String.concat ", "
        sprintf "%s(%s)" head args

    override this.Run variableStack =
        this.Debug "Run"
        let arg1 = this.ArgList[0]
        let arg2 = this.ArgList[1]
        arg1.Run variableStack
        arg2.Run variableStack
        let arg1Repr = arg1.Represent()
        let arg2Repr = arg2.Represent()
        let newValue =  new FplIntrinsicPred((this.StartPos, this.EndPos), this)
        newValue.FplId <-
            // FPL truth-table
            match (arg1Repr, arg2Repr) with
            | (LiteralTrue, _) 
            | (_, LiteralTrue) -> 
                LiteralTrue
            | (LiteralFalse, LiteralFalse) -> 
                LiteralFalse
            | _ -> 
                PrimUndetermined
        this.SetValue(newValue)  
        
    override this.CheckConsistency() = 
        base.CheckConsistency() 
        let arg1 = this.ArgList[0]
        let arg2 = this.ArgList[1]
        checkArgPred this arg1
        checkArgPred this arg2
        checkFreeVar arg1
        checkFreeVar arg2


    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        addExpressionToParentArgList this

/// Implements the semantics of an FPL xor compound predicate.
type FplExclusiveOr(positions: Positions, parent: FplValue) as this =
    inherit FplGenericPredicate(positions, parent)

    do 
        this.FplId <- LiteralXor

    override this.Name = PrimExclusiveOr
    override this.ShortName = LiteralXor

    override this.Clone () =
        let ret = new FplExclusiveOr((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = 
        let head = getFplHead this signatureType
        let args = 
            this.ArgList
            |> Seq.map (fun arg -> arg.Type(signatureType))
            |> String.concat ", "
        sprintf "%s(%s)" head args

    override this.Run variableStack = 
        this.Debug "Run"
        let arg1 = this.ArgList[0]
        let arg2 = this.ArgList[1]
        arg1.Run variableStack
        arg2.Run variableStack
        let arg1Repr = arg1.Represent()
        let arg2Repr = arg2.Represent()
        let newValue =  new FplIntrinsicPred((this.StartPos, this.EndPos), this)

        newValue.FplId <- 
            // FPL truth-table
            match (arg1Repr, arg2Repr) with
            | (LiteralTrue, LiteralFalse) 
            | (LiteralFalse, LiteralTrue) -> 
                LiteralTrue
            | (LiteralTrue, LiteralTrue) 
            | (LiteralFalse, LiteralFalse) -> 
                LiteralFalse
            | _ -> 
                PrimUndetermined

        this.SetValue(newValue)  

    override this.CheckConsistency() = 
        base.CheckConsistency() 
        let arg1 = this.ArgList[0]
        let arg2 = this.ArgList[1]
        checkArgPred this arg1
        checkArgPred this arg2
        checkFreeVar arg1
        checkFreeVar arg2


    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        addExpressionToParentArgList this


/// Implements the semantics of an FPL negation compound predicate.
type FplNegation(positions: Positions, parent: FplValue) as this =
    inherit FplGenericPredicate(positions, parent)

    do 
        this.FplId <- LiteralNot

    override this.Name = PrimNegation
    override this.ShortName = LiteralNot

    override this.Clone () =
        let ret = new FplNegation((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = 
        let head = getFplHead this signatureType
        let args = 
            this.ArgList
            |> Seq.map (fun arg -> arg.Type(signatureType))
            |> String.concat ", "
        sprintf "%s(%s)" head args

    override this.Run variableStack =
        this.Debug "Run"
        let arg = this.ArgList[0]
        arg.Run variableStack
        let argRepr = arg.Represent()
        let newValue =  new FplIntrinsicPred((this.StartPos, this.EndPos), this)

        newValue.FplId <- 
            match argRepr with 
            // FPL truth-table
            | LiteralFalse -> LiteralTrue
            | LiteralTrue -> LiteralFalse
            | _ -> PrimUndetermined  

        this.SetValue newValue

    override this.CheckConsistency() = 
        base.CheckConsistency()
        let arg = this.ArgList[0]
        checkArgPred this arg
        checkFreeVar arg

    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        addExpressionToParentArgList this

/// Implements the semantics of an FPL implication compound predicate.
type FplImplication(positions: Positions, parent: FplValue) as this =
    inherit FplGenericPredicate(positions, parent)

    do 
        this.FplId <- LiteralImpl

    override this.Name = PrimImplication
    override this.ShortName = LiteralImpl

    override this.Clone () =
        let ret = new FplImplication((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = 
        let head = getFplHead this signatureType
        let args = 
            this.ArgList
            |> Seq.map (fun arg -> arg.Type(signatureType))
            |> String.concat ", "
        sprintf "%s(%s)" head args

    override this.Run _ = 
        this.Debug "Run"
        let arg1 = this.ArgList[0]
        let arg2 = this.ArgList[1]
        let arg1Repr = arg1.Represent()
        let arg2Repr = arg2.Represent()
        let newValue =  new FplIntrinsicPred((this.StartPos, this.EndPos), this)
        newValue.FplId <- 
            match (arg1Repr, arg2Repr) with
            // FPL truth-table
            | (LiteralTrue, LiteralFalse) -> LiteralFalse
            | (LiteralFalse, LiteralTrue) 
            | (LiteralFalse, LiteralFalse) 
            | (LiteralTrue, LiteralTrue) -> LiteralTrue
            | _ -> PrimUndetermined
        
        this.SetValue(newValue) 

    override this.CheckConsistency() = 
        base.CheckConsistency() 
        let arg1 = this.ArgList[0]
        let arg2 = this.ArgList[1]
        checkArgPred this arg1
        checkArgPred this arg2
        checkFreeVar arg1
        checkFreeVar arg2

    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        addExpressionToParentArgList this

/// Implements the semantics of an FPL equivalence compound predicate.
type FplEquivalence(positions: Positions, parent: FplValue) as this =
    inherit FplGenericPredicate(positions, parent)

    do 
        this.FplId <- LiteralIif

    override this.Name = PrimEquivalence
    override this.ShortName = LiteralIif

    override this.Clone () =
        let ret = new FplEquivalence((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = 
        let head = getFplHead this signatureType
        let args = 
            this.ArgList
            |> Seq.map (fun arg -> arg.Type(signatureType))
            |> String.concat ", "
        sprintf "%s(%s)" head args

    override this.Run variableStack = 
        this.Debug "Run"
        let arg1 = this.ArgList[0]
        let arg2 = this.ArgList[1]
        arg1.Run variableStack
        arg2.Run variableStack
        let arg1Repr = arg1.Represent()
        let arg2Repr = arg2.Represent()
        let newValue =  new FplIntrinsicPred((this.StartPos, this.EndPos), this)
        newValue.FplId <- 
            match (arg1Repr, arg2Repr) with
            // FPL truth-table
            | (LiteralTrue, LiteralTrue) 
            | (LiteralFalse, LiteralFalse) -> LiteralTrue
            | (LiteralFalse, LiteralTrue) 
            | (LiteralTrue, LiteralFalse) -> LiteralFalse
            | _ -> PrimUndetermined
        
        this.SetValue(newValue)

    override this.CheckConsistency() = 
        base.CheckConsistency() 
        let arg1 = this.ArgList[0]
        let arg2 = this.ArgList[1]
        checkArgPred this arg1
        checkArgPred this arg2
        checkFreeVar arg1
        checkFreeVar arg2

    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        addExpressionToParentArgList this


[<AbstractClass>]
type FplGenericDelegate(name, positions: Positions, parent: FplValue) as this =
    inherit FplValue(positions, Some parent)

    do 
        this.FplId <- name

    override this.Represent () =
        this.ValueList
        |> Seq.map (fun subfv -> subfv.Represent())
        |> String.concat ", "

    override this.EmbedInSymbolTable _ = addExpressionToReference this

    override this.RunOrder = None


/// Implements the semantics of an FPL equality.
type FplEquality(name, positions: Positions, parent: FplValue) as this =
    inherit FplGenericDelegate(name, positions, parent)

    do 
        this.TypeId <- LiteralPred

    override this.Name = PrimDelegateEqualL
    override this.ShortName = PrimDelegateEqual

    override this.Clone () =
        let ret = new FplEquality(this.FplId, (this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Copy(other) =
        base.Copy(other)
        this.TypeId <- LiteralPred

    override this.Type signatureType = 
        let head = getFplHead this signatureType
        let propagate = propagateSignatureType signatureType
        let args = 
            this.ArgList
            |> Seq.map (fun arg -> arg.Type(propagate))
            |> String.concat ", "

        sprintf "%s(%s)" head args

    override this.Run variableStack = 
        this.Debug "Run"
        if this.ArgList.Count <> 2 then 
            emitID013Diagnostics variableStack.CallerStartPos variableStack.CallerEndPos $"Predicate `=` takes 2 arguments, got {this.ArgList.Count}." 
        else

        let getActual (x:FplValue) = 
            if x.ArgList.Count > 0 then
                x.ArgList[0]
            else 
                x

        let a1 = getActual(this.ArgList[0])
        let b1 = getActual(this.ArgList[1])
        let a1Repr = a1.Represent()
        let b1Repr = b1.Represent()

        let newValue = new FplIntrinsicUndef((variableStack.CallerStartPos, variableStack.CallerEndPos), this.Parent.Value)
        match a1Repr with
        | LiteralUndef -> 
            emitID013Diagnostics variableStack.CallerStartPos variableStack.CallerEndPos "Predicate `=` cannot be evaluated because the left argument is undefined." 
            this.SetValue(newValue)
        | _ -> 
            match b1Repr with
            | LiteralUndef -> 
                emitID013Diagnostics variableStack.CallerStartPos variableStack.CallerEndPos "Predicate `=` cannot be evaluated because the right argument is undefined." 
                this.SetValue(newValue)
            | _ -> 
                let newValue = FplIntrinsicPred((variableStack.CallerStartPos, variableStack.CallerEndPos), this.Parent.Value)
                match a1Repr with
                | "dec pred"  
                | PrimUndetermined -> 
                    emitID013Diagnostics variableStack.CallerStartPos variableStack.CallerEndPos "Predicate `=` cannot be evaluated because the left argument is undetermined." 
                    this.SetValue(newValue)
                | _ -> 
                    match b1Repr with
                    | "dec pred"  
                    | PrimUndetermined -> 
                        emitID013Diagnostics variableStack.CallerStartPos variableStack.CallerEndPos "Predicate `=` cannot be evaluated because the right argument is undetermined." 
                        this.SetValue(newValue)
                    | _ -> 
                        let a1IsDeclared = a1Repr.Contains("dec ")
                        let b1IsDeclared = b1Repr.Contains("dec ")
                        newValue.FplId <- 
                            match a1IsDeclared, b1IsDeclared with
                            | false, false 
                            | true, true ->
                                $"{(a1Repr = b1Repr)}".ToLower()
                            | _ -> PrimUndetermined
                        this.SetValue(newValue)

/// Implements an object that is used to provide a representation of extensions in FPL.
type FplExtensionObj(positions: Positions, parent: FplValue) as this =
    inherit FplValue(positions, Some parent)

    do 
        this.TypeId <- LiteralObj


    override this.Name = PrimExtensionObj
    override this.ShortName = LiteralObj

    override this.Clone () =
        let ret = new FplExtensionObj((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = 
        let head = getFplHead this signatureType
        let propagate = propagateSignatureType signatureType

        let qualification =
            if this.Scope.ContainsKey(".") then
                Some(this.Scope["."])
            else
                None

        match (head, qualification) with
        | (_, Some qual) -> sprintf "%s.%s" head (qual.Type(propagate))
        | (_, None) -> sprintf "%s" head

    override this.Represent () = 
        let subRepr = 
            this.ValueList
            |> Seq.map (fun subfv -> subfv.Represent())
            |> String.concat ", "
        if subRepr = String.Empty then 
            this.FplId
        else
            subRepr

    override this.Run _ = 
        this.Debug "Run"

    override this.CheckConsistency () = 
        base.CheckConsistency()
        let matchReprId (fv1:FplValue) (identifier:string) = 
            let vars = fv1.GetVariables()
            if vars.Length > 0 then
                let mainVar = vars.Head
                let regex = Regex(mainVar.TypeId)
                regex.IsMatch(identifier)
            else
                false
        
        let candidatesFromScope =
            let root = getRoot this
            root.Scope
            |> Seq.map (fun theory ->
                theory.Value.Scope
                |> Seq.filter (fun kvp -> kvp.Value.Name = PrimExtensionL)
                |> Seq.map (fun kvp -> kvp.Value)
                |> Seq.filter (fun ext -> 
                    if matchReprId ext this.FplId && not (this.Scope.ContainsKey(this.FplId)) then 
                        // assign the reference FplValue fv only the first found match 
                        // even, if there are multiple extensions that would match it 
                        // (thus, the additional check for Scope.ContainsKey...)
                        this.Scope.Add(this.FplId, ext)
                        let mappingOpt = getMapping ext
                        match mappingOpt with
                        | Some mapping -> this.TypeId <- mapping.TypeId
                        | _ -> ()
                        true
                    else
                        false
                )
            )
            |> Seq.concat
            |> Seq.toList

        let candidates = 
            let parentExtension = this.NextBlockNode
            match parentExtension with 
            | Some ext when ext.Name = PrimExtensionL -> 
                if matchReprId ext this.FplId then
                    // if fv is inside an extension block, we add this block to the candidates
                    // so we can match patterns inside this extension block's definition referring to 
                    // its own pattern even if it is not yet fully parsed and analysed
                    candidatesFromScope @ [ext]
                else 
                    candidatesFromScope
            | _ -> candidatesFromScope

        if candidates.Length = 0 then 
            emitID018Diagnostics this.FplId this.StartPos this.EndPos

    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()    
        addExpressionToReference this

    override this.RunOrder = None

/// Implements the semantics of an FPL decrement delegate.
type FplDecrement(name, positions: Positions, parent: FplValue) as this =
    inherit FplGenericDelegate(name, positions, parent)

    do 
        this.TypeId <- "Nat"

    override this.Name = PrimDelegateDecrementL
    override this.ShortName = PrimDelegateDecrement

    override this.Clone () =
        let ret = new FplDecrement(this.FplId, (this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Copy(other) =
        base.Copy(other)

    override this.Type signatureType = 
        let head = getFplHead this signatureType
        match signatureType with
        | SignatureType.Type -> head
        | _ ->
            let propagate = propagateSignatureType signatureType
            let args = 
                this.ArgList
                |> Seq.map (fun arg -> arg.Type(propagate))
                |> String.concat ", "
            sprintf "%s(%s)" head args

    member private this.Diagnostic message = 
        let diagnostic =
            { 
                Diagnostic.Uri = ad.CurrentUri
                Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
                Diagnostic.Severity = DiagnosticSeverity.Error
                Diagnostic.StartPos = this.StartPos
                Diagnostic.EndPos = this.EndPos
                Diagnostic.Code = ID013 message
                Diagnostic.Alternatives = None 
            }
        ad.AddDiagnostic diagnostic

    override this.Represent () = 
        let subRepr = 
            this.ValueList
            |> Seq.map (fun subfv -> subfv.Type(SignatureType.Name))
            |> String.concat ", "
        if subRepr = String.Empty then 
            LiteralUndef
        else
            subRepr        

    override this.Run _ = 
        this.Debug "Run"
        if this.ArgList.Count <> 1 then 
            this.Diagnostic $"Decrement takes 1 arguments, got {this.ArgList.Count}." 
        else


        let newValue = FplExtensionObj((this.StartPos, this.EndPos), this.Parent.Value)

        let argPre = this.ArgList[0]
        let numericValue = 
            match argPre with
            | :? FplGenericVariable -> 
                argPre.Represent()
            | _ -> argPre.FplId

        let mutable n = 0
        System.Int32.TryParse(numericValue, &n) |> ignore
        let n' = n - 1
        newValue.FplId <- 
            if n' < 0 then 
                ""
            else
                string n'
        this.SetValue(newValue)

type FplFunctionalTerm(positions: Positions, parent: FplValue, runOrder) as this =
    inherit FplGenericInheriting(positions, parent)
    let mutable _signStartPos = Position("", 0L, 0L, 0L)
    let mutable _signEndPos = Position("", 0L, 0L, 0L)
    let _runOrder = runOrder
    let mutable _isReady = false
    let mutable _callCounter = 0

    do 
        this.FplId <- LiteralFunc
        this.TypeId <- LiteralFunc

    member this.SignStartPos
        with get() = _signStartPos
        and set(value) = _signStartPos <- value

    member this.SignEndPos
        with get() = _signEndPos
        and set(value) = _signEndPos <- value

    interface IHasSignature with
        member this.SignStartPos 
            with get () = this.SignStartPos
            and set (value) = this.SignStartPos <- value
        member this.SignEndPos 
            with get () = this.SignEndPos
            and set (value) = this.SignEndPos <- value

    interface IReady with
        member _.IsReady = _isReady

    override this.Name = PrimFuncionalTermL
    override this.ShortName = PrimFuncionalTerm

    override this.Clone () =
        let ret = new FplFunctionalTerm((this.StartPos, this.EndPos), this.Parent.Value, _runOrder)
        this.AssignParts(ret)
        ret

    override this.IsFplBlock () = true
    override this.IsBlock () = true

    override this.Type signatureType = 
        let head = getFplHead this signatureType
        let propagate = propagateSignatureType signatureType

        match getMapping this with
        | Some map ->
            let paramT = getParamTuple this signatureType
            sprintf "%s(%s) -> %s" head paramT (map.Type(propagate))
        | _ -> ""


    override this.CheckConsistency (): unit = 
        if not this.IsIntrinsic then // if not intrinsic, check variable usage
            checkVAR04Diagnostics this
        match this.ExpressionType with
        | FixType.Infix _ when this.Arity <> 2 -> emitSIG00Diagnostics this.ExpressionType.Type 2 this.Arity this.SignStartPos this.SignEndPos
        | FixType.Prefix _ when this.Arity <> 1 -> emitSIG00Diagnostics this.ExpressionType.Type 1 this.Arity this.SignStartPos this.SignEndPos
        | FixType.Postfix _ when this.Arity <> 1 -> emitSIG00Diagnostics this.ExpressionType.Type 1 this.Arity this.SignStartPos this.SignEndPos
        | _ -> ()
        match this.ExpressionType with
        | FixType.Infix (symbol, precedence) -> checkSIG02Diagnostics (getRoot this) symbol precedence this.SignStartPos this.SignEndPos
        | _ -> ()

    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        tryAddToParentUsingMixedSignature this

    override this.RunOrder = Some _runOrder

    override this.Represent () = 
        if this.ValueList.Count = 0 then 
            // since the function term has no value, it has no return statement
            // And the FPL syntax ensures that this can only be the case
            // if the Functional Term is intrinsic.
            // In this case, the "representation" of the function is
            // its declared mapping type
            let mapping = this.ArgList[0]
            $"dec {mapping.Type(SignatureType.Mixed)}"
        else
            let subRepr = 
                this.ValueList
                |> Seq.map (fun subfv -> subfv.Represent())
                |> String.concat ", "
            if subRepr = String.Empty then 
                LiteralUndef
            else
                subRepr

    override this.Run variableStack = 
        this.Debug "Run"
        if not _isReady then
            _callCounter <- _callCounter + 1
            if _callCounter > maxRecursion then
                emitLG002diagnostic (this.Type(SignatureType.Name)) _callCounter this.StartPos this.EndPos
            else
                this.ArgList
                |> Seq.iter (fun fv -> 
                    fv.Run variableStack
                    this.SetValuesOf fv
                )

                this.GetProperties()
                |> List.iter (fun fv -> fv.Run variableStack)

                _isReady <- this.Arity = 0 

/// Implements the semantics of the FPL is operator.
type FplIsOperator(positions: Positions, parent: FplValue) as this =
    inherit FplGenericPredicate(positions, parent)

    do 
        this.FplId <- LiteralIs

    override this.Name = PrimIsOperator
    override this.ShortName = LiteralIs

    override this.Clone () =
        let ret = new FplIsOperator((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = 
        let head = getFplHead this signatureType
        let args = 
            this.ArgList
            |> Seq.map (fun arg -> arg.Type(signatureType))
            |> String.concat ", "
        sprintf "%s(%s)" head args
        
    override this.Run _ = 
        this.Debug "Run"
        let operand = this.ArgList[0]
        let typeOfOperand = this.ArgList[1]
        let newValue = new FplIntrinsicPred((this.StartPos, this.EndPos), this)
        newValue.FplId <- 
            // FPL truth-table
            match operand with 
            | :? FplReference as op ->
                match mpwa [operand] [typeOfOperand] with
                | Some errMsg -> LiteralFalse
                | None -> LiteralTrue
            | _ -> LiteralFalse
        
        this.SetValue(newValue)  

    override this.EmbedInSymbolTable _ = addExpressionToParentArgList this

[<AbstractClass>]
type FplGenericQuantor(positions: Positions, parent: FplValue) =
    inherit FplGenericPredicate(positions, parent)

    override this.ShortName = PrimQuantor

    override this.Type signatureType =
        let head = getFplHead this signatureType

        let paramT =
            this.GetVariables()
            |> Seq.map (fun fv -> fv.Type(signatureType))
            |> String.concat ", "

        match paramT with
        | "" -> head
        | _ -> sprintf "%s(%s)" head paramT

    override this.CheckConsistency () = 
        base.CheckConsistency()
        this.GetVariables()
        |> List.map(fun var -> var :?> FplGenericVariable)
        |> List.filter(fun var -> not var.IsUsed)
        |> List.iter (fun var -> 
            emitVAR05diagnostics var.FplId var.StartPos var.EndPos
        )
        checkArgPred this (this.ArgList[0])

    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        // set all the variables of this quantor to bound ones
        this.GetVariables()
        |> List.map (fun var -> var :?> FplGenericVariable)
        |> List.iter (fun var -> var.SetIsBound())
        addExpressionToParentArgList this
    
    override this.Run variableStack = 
        this.Debug "Run"
        this.ArgList[0].Run variableStack
        let pred = new FplIntrinsicPred((this.StartPos, this.EndPos), this)
        this.SetValue pred


type FplQuantorAll(positions: Positions, parent: FplValue) as this =
    inherit FplGenericQuantor(positions, parent)

    do 
        this.FplId <- LiteralAll

    override this.Name = PrimQuantorAll

    override this.Clone () =
            let ret = new FplQuantorAll((this.StartPos, this.EndPos), this.Parent.Value)
            this.AssignParts(ret)
            ret

type FplQuantorExists(positions: Positions, parent: FplValue) as this =
    inherit FplGenericQuantor(positions, parent)

    do 
        this.FplId <- LiteralEx

    override this.Name = PrimQuantorExists

    override this.Clone () =
            let ret = new FplQuantorExists((this.StartPos, this.EndPos), this.Parent.Value)
            this.AssignParts(ret)
            ret


type FplQuantorExistsN(positions: Positions, parent: FplValue) as this =
    inherit FplGenericQuantor(positions, parent)

    do 
        this.FplId <- LiteralExN
        this.Arity <- 1


    override this.Name = PrimQuantorExistsN

    override this.Clone () =
            let ret = new FplQuantorExistsN((this.StartPos, this.EndPos), this.Parent.Value)
            this.AssignParts(ret)
            ret

type FplMandatoryFunctionalTerm(positions: Positions, parent: FplValue) as this =
    inherit FplValue(positions, Some parent)
    let mutable _signStartPos = Position("", 0L, 0L, 0L)
    let mutable _signEndPos = Position("", 0L, 0L, 0L)

    do 
        this.FplId <- LiteralFunc
        this.TypeId <- LiteralFunc

    override this.Name = PrimMandatoryFunctionalTermL
    override this.ShortName = PrimMandatoryFunctionalTerm

    member this.SignStartPos
        with get() = _signStartPos
        and set(value) = _signStartPos <- value

    member this.SignEndPos
        with get() = _signEndPos
        and set(value) = _signEndPos <- value

    interface IHasSignature with
        member this.SignStartPos 
            with get () = this.SignStartPos
            and set (value) = this.SignStartPos <- value
        member this.SignEndPos 
            with get () = this.SignEndPos
            and set (value) = this.SignEndPos <- value

    override this.Clone () =
        let ret = new FplMandatoryFunctionalTerm((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.IsBlock () = true

    override this.Type signatureType = 
        let head = getFplHead this signatureType
        let propagate = propagateSignatureType signatureType

        match getMapping this with
        | Some map ->
            let paramT = getParamTuple this signatureType
            sprintf "%s(%s) -> %s" head paramT (map.Type(propagate))
        | _ -> ""

    override this.Represent () = 
        if this.ValueList.Count = 0 then 
            // since the function term has no value, it has no return statement
            // And the FPL syntax ensures that this can only be the case
            // if the Functional Term is intrinsic.
            // In this case, the "representation" of the function is
            // its declared mapping type
            let mapping = this.ArgList[0]
            $"dec {mapping.Type(SignatureType.Mixed)}"
        else
            let subRepr = 
                this.ValueList
                |> Seq.map (fun subfv -> subfv.Represent())
                |> String.concat ", "
            if subRepr = String.Empty then 
                LiteralUndef
            else
                subRepr

    override this.EmbedInSymbolTable _ = 
        if not this.IsIntrinsic then // if not intrinsic, check variable usage
            checkVAR04Diagnostics this
        // set all signature variables of this block to bound ones
        this.GetVariables()
        |> List.map (fun var -> var :?> FplGenericVariable)
        |> List.filter (fun var -> var.IsSignatureVariable)
        |> List.iter (fun var -> var.SetIsBound())
        tryAddSubBlockToFplBlock this

    override this.RunOrder = None

    override this.Run variableStack = 
        // todo implement run
        this.Debug "Run"

type FplExtension(positions: Positions, parent: FplValue) =
    inherit FplValue(positions, Some parent)
    let mutable _signStartPos = Position("", 0L, 0L, 0L)
    let mutable _signEndPos = Position("", 0L, 0L, 0L)

    interface IHasSignature with
        member _.SignStartPos 
            with get (): Position = _signStartPos
            and set (value) = _signStartPos <- value
        member _.SignEndPos 
            with get (): Position = _signEndPos
            and set (value) = _signEndPos <- value

    override this.Name = PrimExtensionL 
    override this.ShortName = PrimExtension

    override this.Clone () =
        let ret = new FplExtension((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    // Returns a reference to the mapping of this extension
    member this.Mapping =
        let mapOpt = getMapping this
        match mapOpt with 
        | Some map -> map :?> FplMapping
        | None -> 
            let defaultMap = new FplMapping((this.StartPos, this.EndPos), this)
            defaultMap.FplId <- LiteralUndef
            defaultMap.TypeId <- LiteralUndef
            defaultMap


    override this.Type signatureType = 
        match signatureType with 
        | SignatureType.Name
        | SignatureType.Mixed -> $"{this.FplId} -> {this.Mapping.Type signatureType}" 
        | SignatureType.Type -> $"{this.Mapping.Type signatureType}"

    override this.IsBlock () = true

    override this.Represent () = this.FplId

    override this.Run variableStack = 
        // todo implement run
        this.Debug "Run"

    override this.EmbedInSymbolTable _ = tryAddToParentUsingMixedSignature this

    override this.RunOrder = None


let isExtension (fv:FplValue) =
    match fv with
    | :? FplExtension -> true
    | _ -> false


type FplIntrinsicInd(positions: Positions, parent: FplValue) as this =
    inherit FplValue(positions, Some parent)
    do 
        this.TypeId <- LiteralInd
        this.FplId <- LiteralInd


    override this.Name = PrimIntrinsicInd
    override this.ShortName = LiteralInd

    override this.Clone () =
        let ret = new FplIntrinsicInd((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type (signatureType:SignatureType) = 
        getFplHead this signatureType
                    
    override this.Represent (): string = this.FplId

    override this.Run _ = 
        this.Debug "Run"

    override this.EmbedInSymbolTable _ = addExpressionToParentArgList this 

    override this.RunOrder = None


type FplIntrinsicTpl(positions: Positions, parent: FplValue) as this =
    inherit FplValue(positions, Some parent)
    do
        this.TypeId <- LiteralTpl
        this.FplId <- LiteralTpl

    override this.Name = PrimIntrinsicTpl
    override this.ShortName = LiteralTpl

    override this.Clone () =
        let ret = new FplIntrinsicTpl((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type (signatureType:SignatureType) = 
        getFplHead this signatureType
                    
    override this.Represent (): string = this.FplId

    override this.Run _ = 
        this.Debug "Run"

    override this.EmbedInSymbolTable _ = addExpressionToParentArgList this 

    override this.RunOrder = None

[<AbstractClass>]
type FplGenericStmt(positions: Positions, parent: FplValue) =
    inherit FplValue(positions, Some parent)

    override this.ShortName = PrimStmt

    override this.Type signatureType = this.FplId
    override this.Represent () = ""

    override this.EmbedInSymbolTable _ = addExpressionToParentArgList this

    override this.RunOrder = None

/// Implements the return statement in FPL.
type FplReturn(positions: Positions, parent: FplValue) as this =
    inherit FplGenericStmt(positions, parent)

    do
        this.FplId <- LiteralUndef
        this.TypeId <- LiteralUndef

    override this.Name = PrimReturn

    override this.Clone () =
        let ret = new FplReturn((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = this.FplId
    override this.Represent () = this.FplId

    member private this.MatchWithMapping (fva: FplValue) (fvp: FplValue) =
        let targetMapping = 
            match fvp with 
            | :? FplMapping -> Some fvp
            | _ -> getMapping fvp
        match targetMapping with
        | Some tm -> mpwa [ fva ] [ tm ]
        | None -> None

    override this.Run variableStack =
        this.Debug "Run"
        let returnedReference = this.ArgList[0]
        let blockOpt = this.NextBlockNode
        match blockOpt with 
        | Some funTerm ->
            let mapTypeOpt = getMapping funTerm
            match mapTypeOpt with 
            | Some mapType ->
                match this.MatchWithMapping returnedReference mapType with
                | Some errMsg -> emitSIG03Diagnostics errMsg (mapType.Type(SignatureType.Type)) (returnedReference.StartPos) (returnedReference.EndPos)
                | _ -> 
                    match returnedReference with
                    | :? FplIntrinsicPred 
                    | :? FplIntrinsicTpl 
                    | :? FplIntrinsicInd 
                    | :? FplIntrinsicUndef ->
                        funTerm.SetValue returnedReference
                    | :? FplReference when returnedReference.Scope.ContainsKey(returnedReference.FplId) ->
                        let refValue = returnedReference.Scope[returnedReference.FplId]
                        refValue.Run variableStack
                        funTerm.SetValue refValue
                    | _ ->
                        let value = new FplIntrinsicUndef((this.StartPos, this.EndPos), this)
                        funTerm.SetValue(value)
            | _ -> () // does not occur
        | _ -> () // does not occur

type FplMapCaseSingle(positions: Positions, parent: FplValue) =
    inherit FplGenericStmt(positions, parent)

    override this.Name = PrimMapCaseSingle

    override this.Clone () =
        let ret = new FplMapCaseSingle((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = 
        getFplHead this signatureType

    override this.Represent () = 
        this.ValueList
        |> Seq.map (fun subfv -> subfv.Represent())
        |> String.concat ", "

    member this.GetCondition() = this.ArgList[0]
    member this.GetResult() = this.ArgList[1]

    override this.Run variableStack = 
        this.Debug "Run"
        let condition = this.GetCondition()
        let result = this.GetResult()
        condition.Run variableStack
        let condRepresent = condition.Represent()
        if condRepresent = "true" then
            this.SetValuesOf result
        else
            let undef = FplIntrinsicUndef((this.StartPos, this.EndPos), this.Parent.Value)
            this.SetValue(undef)

type FplMapCaseElse(positions: Positions, parent: FplValue) =
    inherit FplGenericStmt(positions, parent)

    override this.Name = PrimMapCaseElse

    override this.Clone () =
        let ret = new FplMapCaseElse((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = 
        getFplHead this signatureType

    override this.Represent () = LiteralUndef

    override this.Run variableStack = 
        // todo implement run
        this.Debug "Run"

type FplMapCases(positions: Positions, parent: FplValue) =
    inherit FplGenericStmt(positions, parent)

    override this.Name = PrimMapCases

    override this.Clone () =
        let ret = new FplMapCases((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = 
        getFplHead this signatureType

    override this.Represent () = 
        this.ValueList
        |> Seq.map (fun subfv -> subfv.Represent())
        |> String.concat ", "

    member this.GetConditionResultList() = 
        this.ArgList
        |> Seq.choose (fun item ->
            match item with
            | :? FplMapCaseSingle as condRes -> Some condRes
            | _ -> None)

    member this.GetElseResult() = this.ArgList[this.ArgList.Count-1]

    override this.Run _ = 
        this.Debug "Run"
        let resultLst = this.GetConditionResultList()
        let elseResult = this.GetElseResult()
        let findTrueCondition = 
            Seq.tryFind (fun (res:FplMapCaseSingle) -> res.GetCondition().Represent() = "true") resultLst
        match findTrueCondition with
        | Some found -> this.SetValuesOf (found.GetResult())
        | None -> this.SetValuesOf elseResult

type FplCases(positions: Positions, parent: FplValue) =
    inherit FplGenericStmt(positions, parent)

    override this.Name = PrimCases

    override this.Clone () =
        let ret = new FplCases((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = 
        getFplHead this signatureType

    override this.Represent () = LiteralUndef

    override this.Run variableStack = 
        // todo implement run
        this.Debug "Run"

type FplCaseSingle(positions: Positions, parent: FplValue) =
    inherit FplGenericStmt(positions, parent)

    override this.Name = PrimCaseSingle

    override this.Clone () =
        let ret = new FplCaseSingle((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = 
        getFplHead this signatureType

    override this.Represent () = LiteralUndef

    override this.Run variableStack = 
        // todo implement run
        this.Debug "Run"

type FplCaseElse(positions: Positions, parent: FplValue) =
    inherit FplGenericStmt(positions, parent)

    override this.Name = PrimCaseElse

    override this.Clone () =
        let ret = new FplCaseElse((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = 
        getFplHead this signatureType

    override this.Represent () = LiteralUndef

    override this.Run variableStack = 
        // todo implement run
        this.Debug "Run"

type FplForInStmt(positions: Positions, parent: FplValue) =
    inherit FplGenericStmt(positions, parent)

    override this.Name = PrimForInStmt

    override this.Clone () =
        let ret = new FplForInStmt((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = 
        getFplHead this signatureType

    override this.Represent () = LiteralUndef

    override this.Run variableStack = 
        // todo implement run
        this.Debug "Run"

type FplForInStmtEntity(positions: Positions, parent: FplValue) =
    inherit FplGenericStmt(positions, parent)

    override this.Name = PrimForInStmtEntity

    override this.Clone () =
        let ret = new FplForInStmtEntity((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = 
        if this.ArgList.Count = 1 then 
            getFplHead this.ArgList[0] signatureType
        else
            getFplHead this signatureType

    override this.Represent () = LiteralUndef

    override this.EmbedInSymbolTable _ = tryAddToParentForInStmt this

    override this.Run variableStack = 
        // todo implement run
        this.Debug "Run"

type FplForInStmtDomain(positions: Positions, parent: FplValue) =
    inherit FplGenericStmt(positions, parent)

    override this.Name = PrimForInStmtDomain

    override this.Clone () =
        let ret = new FplForInStmtDomain((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = 
        if this.ArgList.Count = 1 then 
            getFplHead this.ArgList[0] signatureType
        else
            getFplHead this signatureType

    override this.Represent () = LiteralUndef

    override this.EmbedInSymbolTable _ = tryAddToParentForInStmt this

    override this.Run variableStack = 
        // todo implement run
        this.Debug "Run"

/// Implements the assigment statement in FPL.
type FplAssignment(positions: Positions, parent: FplValue) as this =
    inherit FplGenericStmt(positions, parent)

    do
        this.FplId <- $"assign (ln {this.StartPos.Line})"
        this.TypeId <- LiteralUndef

    override this.Name = PrimAssignment

    override this.Clone () =
        let ret = new FplAssignment((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = 
        getFplHead this signatureType

    override this.Represent () = this.TypeId

    override this.CheckConsistency () = 
        match this.Assignee, this.AssignedValue with
        | Some (:? FplVariable as assignee), Some (assignedValue:FplValue) -> 
            let nameAssignee = assignee.Type SignatureType.Name
            let typeAssignee = assignee.Type SignatureType.Type
            let nameAssignedValue = assignedValue.Type SignatureType.Name
            let typeAssignedValue = assignedValue.Type SignatureType.Type 
            if nameAssignee = nameAssignedValue then
                emitLG005Diagnostics nameAssignedValue assignedValue.StartPos assignedValue.EndPos
            elif typeAssignee = LiteralObj && (assignedValue.Name = PrimDefaultConstructor || assignedValue.Name = LiteralCtorL) then
                ()
            elif typeAssignee = $"+{typeAssignedValue}" || typeAssignee = $"*{typeAssignedValue}" then 
                ()
            elif typeAssignee <> typeAssignedValue then 
                let referencedTypeOfVarOpt = assignee.Scope.Values |> Seq.tryHead
                match referencedTypeOfVarOpt with 
                | Some referencedTypeOfVar when (referencedTypeOfVar.Name = PrimClassL || referencedTypeOfVar.Name = PrimFuncionalTermL) -> 
                    match assignedValue with 
                    | :? FplIntrinsicUndef -> () // assignment of undef is always accepted
                    | :? FplGenericConstructor as constructor when constructor.ToBeConstructedClass.IsSome ->
                        if not (inheritsFrom constructor.ToBeConstructedClass.Value typeAssignee) then 
                            // issue SIG05 diagnostics if either no inheritance chain found 
                            emitSIG05Diagnostics typeAssignee typeAssignedValue this.ArgList[1].StartPos this.ArgList[1].EndPos 
                    | :? FplFunctionalTerm as funcTerm -> 
                        let mappingOpt = getMapping funcTerm 
                        match mappingOpt with 
                        | Some mapping ->
                            let newTypeAssignedValue = mapping.Type SignatureType.Type
                            if typeAssignee <> newTypeAssignedValue then 
                                // issue SIG05 diagnostics if the return type of an assigned function differs from type of assignee
                                emitSIG05Diagnostics typeAssignee newTypeAssignedValue this.ArgList[1].StartPos this.ArgList[1].EndPos
                        | _ -> ()
                    | _ -> 
                        emitSIG05Diagnostics typeAssignee typeAssignedValue this.ArgList[1].StartPos this.ArgList[1].EndPos
                | _ -> 
                    emitSIG05Diagnostics typeAssignee typeAssignedValue this.ArgList[1].StartPos this.ArgList[1].EndPos
            else   
                ()
        | Some (:? FplSelf as assignee), _ ->
            let ref = assignee.Scope.Values |> Seq.toList
            if ref.Length > 0 then 
                emitSIG07iagnostic (assignee.Type SignatureType.Name) (getEnglishName ref.Head.Name) assignee.Name (this.ArgList[0].StartPos) (this.ArgList[0].EndPos)
            else
                emitSIG07iagnostic (assignee.Type SignatureType.Name) "the type of self cound not be determined" assignee.Name (this.ArgList[0].StartPos) (this.ArgList[0].EndPos)
        | Some (:? FplParent as assignee), _ ->
            let ref = assignee.Scope.Values |> Seq.toList
            if ref.Length > 0 then 
                emitSIG07iagnostic (assignee.Type SignatureType.Name) (getEnglishName ref.Head.Name) assignee.Name (this.ArgList[0].StartPos) (this.ArgList[0].EndPos)
            else
                emitSIG07iagnostic (assignee.Type SignatureType.Name) "the type of parent cound not be determined" assignee.Name (this.ArgList[0].StartPos) (this.ArgList[0].EndPos)
        | Some (:? FplVariableArray as assignee), Some assignedValue ->
            let nameAssignee = this.ArgList[0].Type SignatureType.Name // get the signature of the array's reference
            let nameAssignedValue = this.ArgList[1].Type SignatureType.Name // get the signature of the array's reference
            if nameAssignee = nameAssignedValue then
                // an array has been assigned to itself
                emitLG005Diagnostics nameAssignedValue assignedValue.StartPos assignedValue.EndPos
        | Some (assignee), Some assignedValue ->
            let nameAssignee = assignee.Type SignatureType.Name
            let nameAssignedValue = assignedValue.Type SignatureType.Name
            if nameAssignee = nameAssignedValue then
                // something has been assigned to itself
                emitLG005Diagnostics nameAssignedValue assignedValue.StartPos assignedValue.EndPos
            else
                
                emitSIG07iagnostic (assignee.Type SignatureType.Name) $"type `{assignee.Type SignatureType.Type}`" assignee.Name (this.ArgList[0].StartPos) (this.ArgList[0].EndPos)
        | _ -> ()
        base.CheckConsistency()

    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        addExpressionToParentArgList this


    member this.Assignee:FplValue option =
        if this.ArgList.Count > 0 then 
            let candidate = this.ArgList[0]
            if candidate.Name = PrimRefL && candidate.Scope.ContainsKey(candidate.FplId) then 
                Some candidate.Scope[candidate.FplId]
            else
                Some candidate
        else
            None

    member this.AssignedValue =
        if this.ArgList.Count > 1 then 
            let candidate = this.ArgList[1]
            if candidate.Name = PrimRefL && candidate.Scope.ContainsKey(candidate.FplId) then 
                Some candidate.Scope[candidate.FplId]
            else
                Some candidate
        else
            None

    override this.Run variableStack =
        this.Debug "Run"
        match this.Assignee, this.AssignedValue with
        | Some assignee, Some assignedValue ->
            match assignedValue with 
            | :? FplGenericConstructor as ctor ->
                ctor.Run variableStack
                match ctor.Instance with 
                | Some instance ->
                    assignee.SetValue instance // set value to the created instance 
                    // reposition the instance in symbol table
                    instance.Parent <- Some assignee
                | None -> () // todo, issue diagnostics?
            | :? FplClass as classBlock ->
                emitID004diagnostics classBlock.FplId this.ArgList[1].StartPos this.ArgList[1].EndPos
            | _ ->
                if assignedValue = assignee then 
                    () // LG005 was already emitted when checking consistency but we want to prevent assigning something to itself to avoid infinite loops
                else
                    assignee.SetValuesOf assignedValue
                    match box assignee with
                    | :? IVariable as assigneeCast -> assigneeCast.IsInitialized <- true
                    | _ -> ()
        | _ -> ()

/// A string representation of an FplValue
let toString (fplValue:FplValue) = $"{fplValue.ShortName} {fplValue.Type(SignatureType.Name)}"

/// Checks if a variable is defined in the scope of block, if any
/// looking for it recursively, up the symbol tree.
let variableInBlockScopeByName (fplValue: FplValue) name withNestedVariableSearch =
    let rec firstBlockParent (fv: FplValue) =

        let qualifiedVar (fv1: FplValue) =
            let allVarsInScope = fv1.GetVariables()

            // try out all variables in scope
            let foundList =
                allVarsInScope
                |> Seq.map (fun (var: FplValue) ->
                    if var.Scope.ContainsKey name then
                        ScopeSearchResult.Found(var.Scope[name])
                    else
                        ScopeSearchResult.NotFound)
                |> Seq.filter (fun ssr -> ssr <> ScopeSearchResult.NotFound)
                |> Seq.toList

            if foundList.IsEmpty then
                firstBlockParent fv1.Parent.Value
            else
                foundList.Head
        if isTheory fv then 
            ScopeSearchResult.NotFound
        else
            match fv with 
            | :? FplTheorem 
            | :? FplLemma 
            | :? FplProposition 
            | :? FplCorollary
            | :? FplConjecture 
            | :? FplPredicate 
            | :? FplAxiom 
            | :? FplRuleOfInference -> 
                if fv.Scope.ContainsKey name then
                    ScopeSearchResult.Found(fv.Scope[name])
                elif fv.Parent.IsSome then
                    if withNestedVariableSearch then
                        match qualifiedVar fv with
                        | ScopeSearchResult.NotFound -> firstBlockParent fv.Parent.Value
                        | s -> s
                    else
                        firstBlockParent fv.Parent.Value
                else
                    ScopeSearchResult.NotFound
            | _ ->
                match fv with
                | :? FplConstructor
                | :? FplLocalization
                | :? FplGenericQuantor
                | :? FplMandatoryFunctionalTerm
                | :? FplMandatoryPredicate
                | :? FplProof
                | :? FplExtension
                | :? FplFunctionalTerm
                | :? FplClass ->
                    if fv.Scope.ContainsKey name then
                        ScopeSearchResult.Found(fv.Scope[name])
                    elif fv.Parent.IsSome then
                        if withNestedVariableSearch then
                            match qualifiedVar fv with
                            | ScopeSearchResult.NotFound -> firstBlockParent fv.Parent.Value
                            | s -> s
                        else
                            firstBlockParent fv.Parent.Value
                    else
                        ScopeSearchResult.NotFound
                | _ ->
                    if fv.Parent.IsSome then
                        firstBlockParent fv.Parent.Value
                    else
                        ScopeSearchResult.NotFound

    firstBlockParent fplValue

/// Tries to find a theorem-like statement, an axiom or a corollary
/// and returns different cases of ScopeSearchResult, depending on different semantical error situations.
let tryFindAssociatedBlockForJustificationItem (fvJi: FplGenericJustificationItem) (candidates:FplValue list) =
    let nameOfOther (fv:FplValue) = $"'{fv.Type(SignatureType.Name)} which is {getEnglishName fv.Name}'"
    match candidates.Length with
    | 1 ->  // exactly one candidate found
        let potentialCandidate = candidates.Head
        match fvJi, potentialCandidate with
        | :? FplJustificationItemByProofArgument, :? FplProof
        | :? FplJustificationItemByDef, :? FplClass
        | :? FplJustificationItemByDef, :? FplPredicate
        | :? FplJustificationItemByDef, :? FplFunctionalTerm
        | :? FplJustificationItemByConj, :? FplConjecture
        | :? FplJustificationItemByCor, :? FplCorollary
        | :? FplJustificationItemByAx, :? FplAxiom
        | :? FplJustificationItemByInf, :? FplRuleOfInference
        | :? FplJustificationItemByTheoremLikeStmt, :? FplTheorem 
        | :? FplJustificationItemByTheoremLikeStmt, :? FplProposition
        | :? FplJustificationItemByTheoremLikeStmt, :? FplLemma ->
            ScopeSearchResult.FoundAssociate potentialCandidate
        | _ ->
            ScopeSearchResult.FoundIncorrectBlock (nameOfOther potentialCandidate)
    | 0 -> ScopeSearchResult.NotFound
    | _ -> 
        // multiple candidates found
        ScopeSearchResult.FoundMultiple(
            candidates
            |> List.map (fun fv -> sprintf "'%s' %s" fv.Name (fv.Type(SignatureType.Mixed)))
            |> String.concat ", "
        )

type SymbolTable(parsedAsts: ParsedAstList, debug: bool, offlineMode: bool) =
    let _parsedAsts = parsedAsts
    let mutable _mainTheory = ""
    let _evalPath = Stack<string>()
    let _evalLog = List<string>()
    let _root = new FplRoot()
    let _debug = debug
    let _offlineMode = offlineMode

    /// Returns the current OfflineMode, with which the SymbolTable was created. 
    /// OfflineMode=True should not be used in production. If true, the unit tests will try to 
    /// get a local copy of Fpl libraries instead of trying to download them from the Internet.
    member this.OfflineMode
        with get () = _offlineMode

    /// Returns the current main theory.
    member this.MainTheory
        with get () = _mainTheory
        and set (value) = _mainTheory <- value

    /// Returns the evaluation root node of the symbol table.
    member this.Root = _root

    /// Returns the list of parsed asts
    member this.ParsedAsts = _parsedAsts

    /// Returns the path of the current recursive evaluation. The path is reversed, i.e. starting with the root ast name.
    /// This path can be used to avoid false positives of interpreter diagnostics by further narrowing the parsing context
    /// in which they occur.
    member this.EvalPath() =
        _evalPath |> Seq.toList |> List.rev |> String.concat "."

    /// Logs the current state transformation of the SymbolTable for debugging purposes.
    member this.Log(message) =
        if _debug then
            let enrichedMsg = sprintf "%s at %s" message (this.EvalPath())
            _evalLog.Add(enrichedMsg)

    /// Returns the logged state transformation of the SymbolTable (only non-empty of debug = true).
    member this.LoggedState =
        let log = _evalLog |> String.concat Environment.NewLine
        Environment.NewLine + log + Environment.NewLine

    /// Returns the string representation of all asts .
    member this.AstsToString =
        let res =
            _parsedAsts
            |> Seq.map (fun pa -> pa.Parsing.Ast.ToString())
            |> String.concat Environment.NewLine

        res

    /// Add the current ast name to the recursive evaluation path.
    member this.EvalPush(astName: string) = _evalPath.Push(astName)

    /// Remove the current ast name from the recursive evaluation path.
    member this.EvalPop() = _evalPath.Pop() |> ignore


    /// If there is a valid topological sorting, order the list descending by this ordering.
    member this.OrderAsts() =
        _parsedAsts.Sort(
            Comparer<ParsedAst>.Create(fun b a -> compare a.Sorting.TopologicalSorting b.Sorting.TopologicalSorting)
        )

    /// Serializes the symbol table as json
    member this.ToJson() =
        let sb = StringBuilder()
        let mutable currentPath = ""

        let rec createJson (root: FplValue) (sb: StringBuilder) level isLast preventInfinite =
            match root.FilePath with
            | Some path -> currentPath <- path
            | _ -> ()

            let indent, indentMinusOne =
                if _debug then
                    String(' ', level), String(' ', level - 1)
                else
                    String.Empty, String.Empty

            sb.AppendLine(indentMinusOne + "{") |> ignore
            let name = $"{root.Type(SignatureType.Name)}".Replace(@"\", @"\\")
            let fplTypeName = $"{root.Type(SignatureType.Type)}".Replace(@"\", @"\\")
            let mutable fplValueRepr = $"{root.Represent()}".Replace("\\", "\\\\")   // escape backslashes first
                                                            .Replace("\"", "\\\"")   // then escape double quotes

            if name = this.MainTheory then
                sb.AppendLine($"{indent}\"Name\": \"(Main) {name}\",") |> ignore
            else
                sb.AppendLine($"{indent}\"Name\": \"{name}\",") |> ignore

            sb.AppendLine($"{indent}\"Type\": \"{root.ShortName}\",") |> ignore
            sb.AppendLine($"{indent}\"FplValueType\": \"{fplTypeName}\",") |> ignore
            sb.AppendLine($"{indent}\"FplValueRepr\": \"{fplValueRepr}\",") |> ignore
            sb.AppendLine($"{indent}\"Line\": \"{root.StartPos.Line}\",") |> ignore
            sb.AppendLine($"{indent}\"Column\": \"{root.StartPos.Column}\",") |> ignore
            sb.AppendLine($"{indent}\"FilePath\": \"{currentPath}\",") |> ignore

            if preventInfinite then
                sb.AppendLine($"{indent}\"Scope\": [],") |> ignore
                sb.AppendLine($"{indent}\"ArgList\": [],") |> ignore
                sb.AppendLine($"{indent}\"ValueList\": []") |> ignore
            else
                sb.AppendLine($"{indent}\"Scope\": [") |> ignore
                let mutable counterScope = 0
                root.Scope
                |> Seq.iter (fun child ->
                    counterScope <- counterScope + 1
                    createJson
                        child.Value
                        sb
                        (level + 1)
                        (counterScope = root.Scope.Count)
                        (root.FplId = LiteralSelf || root.FplId = LiteralParent))
                sb.AppendLine($"{indent}],") |> ignore

                sb.AppendLine($"{indent}\"ArgList\": [") |> ignore
                let mutable argList = 0
                root.ArgList
                |> Seq.iter (fun child ->
                    argList <- argList + 1
                    createJson child sb (level + 1) (argList = root.ArgList.Count) false)
                sb.AppendLine($"{indent}],") |> ignore

                sb.AppendLine($"{indent}\"ValueList\": [") |> ignore
                let mutable valueList = 0
                root.ValueList
                |> Seq.iter (fun child ->
                    valueList <- valueList + 1
                    createJson child sb (level + 1) (valueList = root.ValueList.Count) false)
                sb.AppendLine($"{indent}]") |> ignore

            if isLast then
                sb.AppendLine(indentMinusOne + "}") |> ignore
            else
                sb.AppendLine(indentMinusOne + "},") |> ignore

        createJson this.Root sb 1 false false
        let res = sb.ToString().TrimEnd()

        if res.EndsWith(',') then
            res.Substring(0, res.Length - 1)
        else
            res

    /// Returns the uses dependencies of this symbol table needed e.g. for debugging purposes in the FPL language server.
    member this.UsesDependencies() =
        let sb = StringBuilder()
        sb.AppendLine() |> ignore
        sb.AppendLine("SymbolTable: ") |> ignore

        this.Root.Scope
        |> Seq.map (fun theory -> $"{theory.Value.Type(SignatureType.Mixed)} ({theory.Value.Scope.Count})")
        |> String.concat Environment.NewLine
        |> sb.AppendLine
        |> ignore

        sb.AppendLine("ParsedAsts: ") |> ignore

        this.ParsedAsts
        |> Seq.map (fun pa ->
            $"[{pa.Id}, {pa.Sorting.TopologicalSorting}, {pa.Sorting.ReferencedAsts}, {pa.Sorting.ReferencingAsts}]")
        |> String.concat Environment.NewLine
        |> sb.AppendLine
        |> ignore

        sb.ToString()

    /// Creates trace statistics needed e.g. for debugging purposes in the FPL language server.
    member this.TraceStatistics =
        let sb = StringBuilder()

        this.ParsedAsts
        |> Seq.iter (fun pa ->
            let paDiagnostics = ad.GetStreamDiagnostics(pa.Parsing.Uri)

            let statsDiags =
                paDiagnostics.Values
                |> Seq.groupBy (fun d -> $"{d.Emitter}({d.Code.Code})")
                |> Seq.map (fun (groupId, group) -> $"{groupId}:{Seq.length group}")
                |> String.concat ", "

            sb.AppendLine $"{pa.Id}(chksm {pa.Parsing.Checksum}): #total diags {paDiagnostics.Count}, {statsDiags}"
            |> ignore)

        sb.ToString()


/// Looks for all declared building blocks with a specific name.
let findCandidatesByName (st: SymbolTable) (name: string) withClassConstructors withCorollariesOrProofs =
    let pm = List<FplValue>()

    let rec flattenCorollariesAndProofs (tls:FplValue) =
        tls.Scope.Values
        |> Seq.iter (fun fv -> 
            match fv with
            | :? FplProof -> pm.Add(fv)
            | :? FplCorollary -> 
                pm.Add(fv)
                flattenCorollariesAndProofs fv
            | _ -> ()
        )
    let nameWithoutProofOrCorRef = 
        if withCorollariesOrProofs && name.Contains("$") then 
            let parts = name.Split('$')
            parts.[0] 
        else
            name
    let nameWithProofOrCorRef = 
        if withCorollariesOrProofs && not (name.Contains("$")) then 
            $"{name}$"
        else
            name

    if name.Length > 0 && System.Char.IsUpper(name[0]) then 
        st.Root.Scope // iterate all theories
        |> Seq.iter (fun theory ->
            theory.Value.Scope
            // filter only blocks starting with the same FplId as the reference
            |> Seq.map (fun kvp -> kvp.Value)
            |> Seq.filter (fun fv -> 
                fv.FplId = name 
                || fv.FplId = nameWithoutProofOrCorRef 
                || $"{fv.FplId}$".StartsWith nameWithProofOrCorRef)
            |> Seq.iter (fun (block: FplValue) ->
                pm.Add(block)

                if withClassConstructors && block.IsClass() then
                    block.Scope
                    |> Seq.map (fun kvp -> kvp.Value)
                    |> Seq.filter (fun (fv: FplValue) -> (fv.Name = LiteralCtorL || fv.Name = PrimDefaultConstructor))
                    |> Seq.iter (fun (fv: FplValue) -> pm.Add(fv))

                if withCorollariesOrProofs && (block :? FplGenericTheoremLikeStmt) then 
                    flattenCorollariesAndProofs block
            )
        )
        |> ignore

    pm |> Seq.toList

/// Looks for all declared properties or constructors (if any) that start with
/// the specific name within the building block, whose syntax tree the FplValue `fv` is part of.
let findCandidatesByNameInBlock (fv: FplValue) (name: string) =
    let rec findDefinition (fv1: FplValue) =
        if isTheory fv1 then
            ScopeSearchResult.NotFound
        else
            match fv1 with
            | :? FplPredicate -> ScopeSearchResult.Found(fv1)
            | _ ->
                match fv1 with
                | :? FplClass
                | :? FplFunctionalTerm -> ScopeSearchResult.Found(fv1)
                | _ ->
                    match fv1.Parent with
                    | Some parent -> findDefinition parent
                    | None -> ScopeSearchResult.NotFound

    match findDefinition fv with
    | ScopeSearchResult.Found candidate ->
        candidate.Scope
        |> Seq.filter (fun kvp -> kvp.Value.FplId = name)
        |> Seq.map (fun kvp -> kvp.Value)
        |> Seq.toList
    | _ -> []

let findCandidatesByNameInDotted (fv: FplValue) (name: string) =
    let rec findQualifiedEntity (fv1: FplValue) =
        match fv1 with
        | :? FplReference ->
            if fv1.Scope.ContainsKey(".") && fv1.Scope.Count > 1 then
                let result =
                    fv1.Scope
                    |> Seq.filter (fun kvp -> kvp.Key <> ".")
                    |> Seq.map (fun kvp -> kvp.Value)
                    |> Seq.toList
                    |> List.head

                ScopeSearchResult.Found(result)
            else
                match fv1.Parent with
                | Some parent -> findQualifiedEntity parent
                | None -> ScopeSearchResult.NotFound
        | _ -> ScopeSearchResult.NotFound

    match findQualifiedEntity fv with
    | ScopeSearchResult.Found candidate ->
        match candidate with
        | :? FplVariable ->
            if candidate.ArgList.Count > 0 then
                let (varType: FplValue) = candidate.ArgList[0]

                varType.Scope
                |> Seq.filter (fun kvp -> kvp.Value.FplId = name)
                |> Seq.map (fun kvp -> kvp.Value)
                |> Seq.toList
            else
                []
        | _ -> []
    | _ -> []

let rec getParentExtension (leaf: FplValue) =
    match leaf with
    | :? FplExtension ->
        Some leaf
    | _ -> 
        match leaf.Parent with
        | Some parent -> getParentExtension parent 
        | _ -> None

let searchExtensionByName (root: FplValue) identifier =
    let candidates =
        root.Scope
        |> Seq.map (fun theory ->
            theory.Value.Scope
            |> Seq.filter (fun kvp -> isExtension kvp.Value)
            |> Seq.map (fun kvp -> kvp.Value)
            |> Seq.filter (fun ext -> ext.FplId = identifier))
        |> Seq.concat
        |> Seq.toList

    if candidates.Length = 0 then
        ScopeSearchResult.NotFound
    else
        ScopeSearchResult.Found candidates.Head

