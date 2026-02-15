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


/// Checks if a string starts with a lower case character string
let checkStartsWithLowerCase (s:string) =
    if s.Length > 0 then 
        System.Char.IsLower(s[0])
    else
        false
(*
    TODO: 1) implement a function ToPL0Form transforming a predicative expression into a PL0 formula by replacing predicates with free pred variables
             possible applications: see 1a) 
    TODO: 1a) implement a function ToTrueTable generating a true table out of ToPL0Form
             possible applications: see 2), 2a) 3) 4)
    TODO: 2) implement a satisfiability check to the Output of ToTrueTable
             possible applications: 
                issue error, if a formula of a theorem / axiom / conjecture is not satisfiable
                issue warning, if a sub formula is not satisfiable to replace it by false
    TODO: 2a) implement a tautology check to the output of ToTrueTable
             possible applications: 
                issue warning, if a formula of a theorem / axiom / conjecture is a tautology, because it could be replaced by a trivial true
                issue warning, if a sub formula is a tautology to replace it by true
    TODO: 3) implement a CanonicalDNF (disjunctive normal form) based on ToTrueTable with a sorted representation.
             possible applications:
                issue error, if in a proof there are two consecutive arguments aprev, anext whose outputs have the same ToTrueTables 
                    in terms of variables (its columns) that are not equivalent (have different rows)
    TODO: 4) implement unit tests for all inference rules defined in Fpl.Commons checking if the respective premises and conclusions produce the same outputs of ToTrueTable.
             In this case, it is ensured that each inference rule in this library is a tautology. This is a required for 
             FPL to use inference rules as a Hilbert Calculus (see definition D.Hoffmann "Theoretische Informatik" 3rd. ed., p. 98)
             respectively inference rules with a premise being a predicate list: Here it is sufficient to check, if each rule 
             conserves the tautology property: If each predicate in a list is a tautology, so is the conclusion (see D.Hoffmann, "Theoretische Informatik", p. 104)
    TODO: 5) ensure cleaned-up expressions by renaming variable with the same names in independent parts of the same formula expression.
             (see D.H. "Theoretische Informatik", 3rd. p. 119) 
             Implementation idea: This can be accomplished by moving the scope of variables declared in quantors to the containing FPL block, forcing renaming the variables by the end-user at coding-time of the formula. 
    TODO: 6) issue error if arity-0 predicates are intrinsically defined, enforcing true or false (see D.H. "Theoretische Informatik", 3rd. p. 120) 
    TODO: 7) write functions for normalizing predicative formulas (see D.H. "Th. Inf", 3rd. p. 122-123):
                NormalizeNeg - (uses cleaned-up expressions - see 5) replace impl, iif, xor, by and/or/not and move all negations from the beginning of non-atomic formula to its atomic sub formulas 
                NormalizePrenex - (uses the output of NormalizeNeg): move quantors from all sub formulas the most outer quantor using fixed rules (see figure 3.35, p. 122)
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
    TODO: 8) write unit-test checking if FplValue.Type(SignatureType.Type) of expressions like p(u()) or all x1:tpl1, x2:tpl2 {p(x1,x2, g(x1, x2))} 
        includes full signatures of the functions u() and g(,), .i.e., including their mappings. This will later become necessary 
        to be able to recognize the satisfiability-equivalence of two NormalizeSkolem outputs (see 7)
        For the term "satisfiability-equivalence" see D.H. "Th. Inf", 3rd. p. 124
*)


type IVariable =
    abstract member IsSignatureVariable : bool with get, set
    abstract member IsInitialized : bool with get, set

/// The interface ISkolem is used to implement fixed but unknown objects of some type.
/// In general, the equality of two fixed but unknown objects of the same type cannot be determined, 
/// unless it is explicitly asserted (or explicitly negated) in the corresponding FPL theory.
/// However, once a SkolemName is established, the value will be treated like a fixed constant.
type ISkolem =
    abstract member SkolemName : string with get
    abstract member SetSkolemName: unit -> unit

type Debug =
    | Start
    | Stop

[<AbstractClass>]
type FplValue(positions: Positions, parent: FplValue option) =
    let mutable _expressionType = FixType.NoFix
    let mutable _argType = ArgType.Nothing
    let mutable _exprTypeAlreadySet = false
    let mutable _startPos = fst positions
    let mutable _endPos = snd positions
    let mutable _auxiliaryInfo = 0
    let mutable _arity = 0
    let mutable _fplId = ""
    let mutable _typeId = ""
    let mutable (_filePath: string option) = None
    let mutable _isIntrinsic = false
    let mutable (_errorOccurred: string option) = None
    let mutable (_value:FplValue option) = None
    let mutable _refersTo:FplValue option = None

    let mutable _parent = parent
    let _scope = Dictionary<string, FplValue>()
    let _argList = List<FplValue>()

    /// A scope of this FplValue
    member this.Scope = _scope

    /// An argument list of this FplValue
    member this.ArgList = _argList

    abstract member Clone: unit -> FplValue
    abstract member Copy : FplValue -> unit
    abstract member AssignParts: FplValue -> unit
    abstract member ShortName: string
    abstract member Name: string
    abstract member Represent: unit -> string

    /// An optional order in which this FplValue ist to be run after the symbol table is completely created.
    /// None means that it is not running but itself but called to be run from other FplValues.
    /// Some int means that it is running by itself after the creation of the symbol table. 
    /// Only theories in root, and axioms, theorems, lemmas, propositions, conjectures, and definitions of predicates and functional terms in theories run by themselves and call all other types of FplValue to run.
    abstract member RunOrder: int option

    /// Generates a type string identifier or type-specific naming convention of this FplValue.
    abstract member Type: SignatureType -> string

    /// Embeds this FplValue in the SymbolTable by adding it to the Scope or as an argument of its predecessor in the SymbolTable.
    abstract member EmbedInSymbolTable: FplValue option -> unit

    /// Abstract member for running this FplValue. It has None or Some optional FplVariableStack as parameter.
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
        this.Value <- Some fv

    default this.SetValuesOf fv =
        this.Value <-  fv.Value

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
        ret.ArgType <- this.ArgType
        ret.Value <- this.Value
        ret.RefersTo <- this.RefersTo

        this.Scope
        |> Seq.iter (fun (kvp:KeyValuePair<string, FplValue>) ->
            let value = kvp.Value.Clone()
            ret.Scope.Add(kvp.Key, value))

        this.ArgList
        |> Seq.iter (fun (fv1:FplValue) ->
            let value = fv1.Clone()
            ret.ArgList.Add(value))

        ret.Value <- this.Value 

    /// TypeId of the FplValue.
    member this.TypeId
        with get () = _typeId
        and set (value) = _typeId <- value

    /// Value of this FplValue
    member this.Value
        with get () = _value
        and set (value) = _value <- value

    /// The optional node this FplValue refers to 
    member this.RefersTo 
        with get () = _refersTo
        and set (value) = _refersTo <- value

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
            elif _expressionType.Type = value.Type then
                ()
            else
                raise (
                    ArgumentException(
                        $"Type was already initialized with `{_expressionType.Type}`, cannot set it again with {value.Type}."
                    )
                )

    /// Indicates if this FplValue has bracketed arguments or parameters, 
    /// parenthesized arguments or parameters, or no arguments or parameters
    member this.ArgType
        with get () = _argType
        and set (value) = _argType <- value

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

    /// Indicates if an interpreter error occurred during the construction of this FplReference
    member this.ErrorOccurred
        with get () = _errorOccurred
        and set (value) = 
            match _errorOccurred, value with
            | None, Some next -> _errorOccurred <- Some next // aggregate errors
            | Some prev, Some next -> _errorOccurred <- Some $"{prev}, {next}" // aggregate errors
            | _ -> ()

    /// Standard implementation of value string representation of all nodes in the symbol table
    /// constructed by the FPL interpreter. Returns 
    /// "None" - for all nodes in the symbol table that yield no value (e.g. statements).
    /// "undef" - for all nodes in the symbol table that have a type other from a predicate and usually yield a value (e.g. functional terms), but whose value could not be determined.
    /// "undetermined" - for all nodes in the symbol table that have a predicate type (e.g. theorems) but whose value could not be determined. 
    /// otherwise a string representation depending on type of the FPL node and its specific value.
    override this.Represent() = // done
        match this.Value with 
        | Some v -> v.Represent() 
        | _ -> PrimNone // If there is no value, return string "None"

    /// Create a (possibly empty) list of all variables in the scope of this FplValue.
    member this.GetVariables() =
        this.Scope.Values
        |> Seq.filter (fun fv -> 
            fv.Name = PrimVariableL 
            || fv.Name = PrimVariableArrayL 
        )
        |> Seq.sortBy(fun fv -> fv.RunOrder)
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
        this.ExpressionType <- other.ExpressionType
        this.ArgType <- other.ArgType
       

        this.Scope.Clear()
        other.Scope |> Seq.iter (fun kvp -> this.Scope.Add(kvp.Key, kvp.Value))

        this.ArgList.Clear()
        this.ArgList.AddRange(other.ArgList)

        this.Value <- other.Value
        this.RefersTo <- other.RefersTo

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

    /// Calculates this FplValue's ultimate block node (if such exists).
    /// The ultimate block node is the FPL building block's FplValue enclosing this FplValue (if such exists)
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

    /// Calculates this FplValue's ultimate block node (if such exists).
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


    member this.Debug (debugMode:Debug) =
        let rec getPath (fv:FplValue) =
            match fv.Parent with 
            | Some parent -> $"{getPath parent} # {fv.ShortName} {fv.Type SignatureType.Name}"
            | None -> $"{fv.ShortName}"
        let vars =
            this.GetVariables()
            |> List.map (fun var -> $"{var.FplId}={var.Represent()}")
            |> String.concat ", "
        if TestSharedConfig.TestConfig.DebugMode then 
            let logLine =
                match debugMode with
                | Debug.Start ->
                    $"Start:{getPath this}:[{this.Represent()}][{vars}]{Environment.NewLine}"
                | Debug.Stop ->
                    $"Stop :{getPath this}:[{this.Represent()}][{vars}]{Environment.NewLine}"
            let currDir = Directory.GetCurrentDirectory()
            File.AppendAllText(Path.Combine(currDir, "Debug.txt"), logLine)

/// a type wrapping the type of the FplValue 
and TypeNode = 
    | Simple of FplValue 
    | Array of FplValue
    | Nothing

/// a type wrapping the argument type of the FplValue 
and ArgType = 
    | Parentheses
    | Brackets
    | Nothing

/// A discriminated union type for wrapping search results in the Scope of an FplValue.
and ScopeSearchResult =
    | FoundAssociate of FplValue
    | FoundMultiple of string
    | FoundIncorrectBlock of string
    | Found of FplValue
    | NotFound
    | NotApplicable
and State() = 
    let _vars = Dictionary<string,FplValue option>()
    let mutable _value: FplValue option = None
    let mutable _refersTo: FplValue option = None

    /// The optional value of the called node before it was called
    member this.Value
        with get() = _value
        and set (value:FplValue option) = _value <- value


    /// The optional RefersTo of the called node before it was called
    member this.RefersTo
        with get() = _refersTo
        and set (value:FplValue option) = _refersTo <- value

    /// The dictionary of the variable values of the called node before it was called
    member this.VarValues = _vars

/// This type implements the functionality needed to "run" FPL statements step-by-step
/// while managing the storage of variables and other evaluation-related information.
/// FPL uses a call-by-value approach when it comes to 
/// replacing parameters by a calling function with arguments.
and FplVariableStack() = 
    let mutable _inSignatureEvaluation = false
    let _stateStack = Stack<KeyValuePair<string, State>>()
    let _valueStack = Stack<FplValue>()
    let _recursionCounters = Dictionary<string, int>()
    let _assumedArguments = Stack<FplValue>()
    // positions of the caller to prevent some diagnostics of being shown at the wrong position 
    let mutable _callerStartPos = Position("", 0,0,0)
    let mutable _callerEndPos = Position("", 0,0,0)
    let mutable _language = "tex" // the default language is tex, otherwise, it should be set in the FPL IDE extension

    let mutable _nextRunOrder = 0

    /// Current language choice of all localizations
    member this.CurrentLanguage
        with get () = _language
        and set (value) = _language <- value

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
    /// When running the nodes in the dictionary, their run order will ensure that they are being run in the order they have bin inserted.
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
    member this.Stack = _stateStack

    /// Copy the ValueList of the variadic ar to the ValueList of the variadic p
    /// by removing the previous values (if any) and
    /// inserting the clones of the elements.
    member this.ReplaceVariables (parameters:FplValue list) (arguments:FplValue list) =
        let replaceValues (p:FplValue) (ar:FplValue) =
            match ar.Value with 
            | Some v -> p.SetValue v
            | None -> p.Value <- None

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
    member this.SaveState (called:FplValue) = 
        // now process all scope variables and push by replacing them with their clones
        // and pushing the originals on the stack
        let toBeSavedState = new State()
        let pars = List<FplValue>()
        let vars = called.GetVariables()
        vars 
        |> List.iter (fun parOriginal -> 
            toBeSavedState.VarValues.Add(parOriginal.FplId, parOriginal.Value)
            match box parOriginal with 
            | :? IVariable as parOrig when parOrig.IsSignatureVariable ->
                pars.Add(parOriginal)
            | _ -> ()
        )
        toBeSavedState.Value <- called.Value
        toBeSavedState.RefersTo <- called.RefersTo
        let kvp = KeyValuePair(called.FplId,toBeSavedState)
        
        _stateStack.Push(kvp)
        pars |> Seq.toList

    /// Restores the state of a called FplValue block it had before it was called.
    member this.RestoreState (called:FplValue) = 
        let stateBeforeBeingCalled = _stateStack.Pop().Value
        stateBeforeBeingCalled.VarValues
        |> Seq.iter (fun kvp -> 
            let origVariable = (called.Scope:Dictionary<string, FplValue>)[kvp.Key] 
            let oldValue = kvp.Value
            origVariable.Value <- oldValue
        )
        called.Value <- stateBeforeBeingCalled.Value 
        called.RefersTo <- stateBeforeBeingCalled.RefersTo 


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
        _stateStack.Clear()

[<AbstractClass>]
type FplGenericHasNoValue(positions: Positions, parent: FplValue) =
    inherit FplValue(positions, Some parent)

    override this.Represent() = // done
        PrimNone
    override this.RunOrder = None


type IHasDotted = 
    abstract member DottedChild : FplValue option with get, set

/// Searches for a references in node symbol table. 
/// Will works properly only for nodes types that use their scope like FplReference, FplSelf, FplParent, FplForInStmtDomain, FplForInStmtEntity, FplVariable
let rec referencedNodeOpt (fv:FplValue) = 
    
    let refNodeOpt = 
        match box fv with 
        | :? IHasDotted as dotted when dotted.DottedChild.IsSome -> referencedNodeOpt dotted.DottedChild.Value
        | _ when fv.Name = PrimInstanceL -> Some fv
        | _ when fv.Name = PrimIntrinsicPred -> Some fv
        | _ when fv.Name = PrimIntrinsicInd -> Some fv
        | _ when fv.Name = PrimVariableL -> fv.RefersTo
        | _ -> fv.RefersTo
    match refNodeOpt with
    | Some refNode when refNode.Name = LiteralSelf -> refNode.RefersTo
    | Some refNode when refNode.Name = LiteralParent -> refNode.RefersTo
    | Some refNode when refNode.Name = PrimVariableL && refNode.Value.IsSome -> referencedNodeOpt refNode.Value.Value
    | _ -> refNodeOpt

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
    
let isDefinition (fv1:FplValue) =
    match fv1.Name with
    | PrimClassL
    | PrimPredicateL
    | PrimFunctionalTermL -> true
    | _ -> false

type IHasSignature =
    abstract member SignStartPos : Position with get, set
    abstract member SignEndPos : Position with get, set

let hasSignature (fv1:FplValue) =
    match box fv1 with
    | :? IHasSignature -> true
    | _ -> false

let isSignatureVar (fv1:FplValue) = 
    match box fv1 with 
    | :? IVariable as var when var.IsSignatureVariable -> true
    | _ -> false

/// Qualified name of this FplValue
let qualifiedName (fplValue:FplValue) determined =
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
            | PrimFunctionalTermL
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

    $"{getEnglishName fplValue.Name determined} {getFullName fplValue true}"


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

    override this.Type signatureType = getFplHead this signatureType

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
        this.Debug Debug.Start
        let blocks = this.OrderedBlocksRunningByThemselves
        blocks
        |> Seq.iter (fun block -> block.Run variableStack)        
        this.Debug Debug.Stop

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
        this.Debug Debug.Start
        this.OrderedTheories
        |> Seq.iter (fun theory -> theory.Run variableStack)        
        this.Debug Debug.Stop

// Returns the root node of any FplValue
let rec getRoot (fv:FplValue) =
    if fv.Name = PrimRoot then 
        fv :?> FplRoot
    else getRoot fv.Parent.Value
   
// Tries to add for statement's domain or entity to its parent's for statement
let tryAddToParentForInStmt (fplValue:FplValue) =
    let identifier = fplValue.Type SignatureType.Name
    let parent = fplValue.Parent.Value

    if parent.ArgList.Count = 1 then
        let entityIdentifier = parent.ArgList[0].Type SignatureType.Name
        if entityIdentifier = identifier then 
            fplValue.ErrorOccurred <- emitID027Diagnostics identifier fplValue.StartPos fplValue.EndPos
        else 
            parent.ArgList.Add fplValue
    else
        parent.ArgList.Add fplValue

// Tries to add a template to the ultimate block's scope, inside which it was used.
let tryAddTemplateToParent (templateNode:FplValue) =
    let identifier = templateNode.FplId
    let nextOpt = templateNode.UltimateBlockNode // the scope of all templates ís inside the ultimate block
    match nextOpt with 
    | Some next when not (next.Scope.ContainsKey identifier) -> 
        next.Scope.Add(identifier, templateNode)
        // correct the parent of the template
        templateNode.Parent <- Some next
    | Some next -> 
        // template was already added to the ultimate node
        let templateAlreadyInScope = next.Scope[identifier] // return the templateNode that was already added instead of the input 
        match templateNode.Parent with 
        | Some var -> 
            // Replace the variable's newly created template type by 
            // the template already in the scope of its ultimate node
            // making sure that only one template with the same identifier will store 
            // its actual type to issue SIG12 diagnostics properly
            var.RefersTo <- Some templateAlreadyInScope 
        | _ -> () // should never occur, since only the root has no parent
    | _ ->  
        // should never occur, since FPL's syntax 
        // does not allow template without UltimateBlocks
        ()

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
        fplValue.ErrorOccurred <- emitID001Diagnostics identifier (conflicts.Head.QualifiedStartPos) fplValue.StartPos fplValue.EndPos
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
        fplValue.ErrorOccurred <- emitID001Diagnostics identifier (conflicts.Head.QualifiedStartPos) fplValue.StartPos fplValue.EndPos
    else
        let parent = fplValue.Parent.Value
        parent.Scope.Add(identifier, fplValue)

// Tries to add a constructor or property to it's parent FPL block's scope using its mixed signature, or issues ID001 diagnostics if a conflict occurs
let tryAddSubBlockToFplBlock (fplValue:FplValue) =
    let identifier = fplValue.Type SignatureType.Mixed
    let parent = fplValue.Parent.Value
    if parent.Scope.ContainsKey(identifier) then 
        fplValue.ErrorOccurred <- emitID001Diagnostics identifier (parent.Scope[identifier].QualifiedStartPos) fplValue.StartPos fplValue.EndPos
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
        fplValue.ErrorOccurred <- emitID024Diagnostics identifier (conflicts.Head.QualifiedStartPos) fplValue.StartPos fplValue.EndPos
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
    match box nextOpt with 
    | :? IHasDotted as dc when dc.DottedChild.IsSome -> ()
    | _ ->
        match nextOpt with
        | Some next when next.Name = PrimRefL && next.RefersTo.IsSome ->
            let referenced = next.RefersTo.Value
            match referenced.Name with 
            | PrimVariableArrayL ->
                next.ArgList.Add fplValue
            | _ ->
                next.FplId <- fplValue.FplId
                next.TypeId <- fplValue.TypeId
                next.RefersTo <- Some fplValue
        | Some next when next.Name = PrimRefL && 
            (
                fplValue.Name = PrimDelegateEqualL 
             || fplValue.Name = PrimDelegateDecrementL
             ) ->
            addExpressionToParentArgList fplValue 
        | Some next when next.Name = PrimRefL ->
            next.FplId <- fplValue.FplId
            next.TypeId <- fplValue.TypeId
            next.RefersTo <- Some fplValue 
        | _ -> addExpressionToParentArgList fplValue 

/// Indicates if an FplValue is the root of the SymbolTable.
let isRoot (fv:FplValue) = 
    match fv with
    | :? FplRoot -> true
    | _ -> false

/// Implements the semantics of an FPL predicate prime predicate that is intrinsic.
/// It serves as a value for everything in FPL that is "predicative in nature". These can be predicates, theorem-like-statements, proofs or predicative expressions. The value can have one of three values in FPL: "true", LiteralFalse, and "undetermined". 
type FplIntrinsicPred(positions: Positions, parent: FplValue) as this =
    inherit FplGenericHasNoValue(positions, parent)
    do 
        this.FplId <- PrimUndetermined
        this.TypeId <- LiteralPred

    override this.Name = PrimIntrinsicPred
    override this.ShortName = LiteralPred

    override this.Clone () =
        let ret = new FplIntrinsicPred((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type (signatureType:SignatureType) = getFplHead this signatureType
                    
    override this.Represent() = // done
        this.FplId 

    override this.Run _ = 
        this.Debug Debug.Start
        // FplIntrinsicPred is a value of predicate closures and has no value on its own
        this.Debug Debug.Stop

    override this.EmbedInSymbolTable _ = addExpressionToReference this

    override this.RunOrder = None

[<AbstractClass>]
type FplGenericPredicate(positions: Positions, parent: FplValue) as this =
    inherit FplValue(positions, Some parent)
    do 
        this.FplId <- PrimUndetermined
        this.TypeId <- LiteralPred

    override this.RunOrder = None

/// Tries to find a mapping of an FplValue
let rec getMapping (fv:FplValue) =
    match fv.Name with
    | LiteralParent 
    | LiteralSelf -> 
        match fv.RefersTo with 
        | Some ref -> getMapping ref
        | None -> None
    | PrimRefL when fv.RefersTo.IsSome ->
        getMapping fv.RefersTo.Value
    | PrimRefL ->
        None
    | _ ->
        fv.ArgList |> Seq.tryFind (fun fv -> fv.Name = PrimMappingL)

type IHasDimensions =
    abstract member Dimensionality : int
    abstract member DimensionTypes : List<FplValue>
    abstract member SetType : string -> FplValue option -> Position -> Position -> unit

[<AbstractClass>]
type FplGenericInheriting(positions: Positions, parent: FplValue) =
    inherit FplValue(positions, Some parent)
    // used to ensure that every clone of FplGenericInheriting will preserve reference identity of inherited variables 
    let _inheritedVariables = Dictionary<string, List<FplValue>>()
    // used to ensure that every clone of FplGenericInheriting will preserve reference identity of inherited properties
    let _inheritedProperties = Dictionary<string, List<FplValue>>()

    /// Wraps an inherited object in a tuple together with the newFromNode it was from and stores this tuple with the keyOfInheritedObject in mapOfInheritedObjects. 
    /// Returns (Some oldFromNode, Some newFromNode) if some other tuple existed in the map, where oldFromNode will be the old base node that was overridden by newFromNode
    /// Returns (None, None) if no other tuple yet existed in the map
    member private this.OverrideInheritedObject keyOfInheritedObject (mapOfInheritedObjects:Dictionary<string, List<FplValue>>) (inheritedObject:FplValue) (newFromNode:FplValue) withCloning =
        let clone = 
            if withCloning then
                inheritedObject.Clone() // create a clone of new object to override the old one
            else
                inheritedObject
        // create a tuple (clone, fromBaseNode)
        let tuple = List<FplValue>()
        tuple.Add clone 
        tuple.Add newFromNode // and where it was from
        if mapOfInheritedObjects.ContainsKey keyOfInheritedObject then
            // replace the old reference
            let oldFromNode = mapOfInheritedObjects[keyOfInheritedObject][1]
            mapOfInheritedObjects[keyOfInheritedObject] <- tuple
            (Some oldFromNode.Name, Some (oldFromNode.Type SignatureType.Mixed), Some (newFromNode.Type SignatureType.Mixed))
        else
            // add a new reference
            mapOfInheritedObjects.Add(keyOfInheritedObject, tuple)
            (None, None, None)
            
    member this.InheritVariables (fromBaseNode:FplValue) = 
        fromBaseNode.GetVariables()
        |> List.iter (fun var ->
            match this.OverrideInheritedObject var.FplId _inheritedVariables var fromBaseNode true with
            | (Some typeName, Some oldFromNode, Some newFromNode) ->
                fromBaseNode.ErrorOccurred <- emitVAR06iagnostic var.FplId oldFromNode newFromNode typeName fromBaseNode.StartPos fromBaseNode.EndPos
            | _ ->
                ()
        )

    member this.InheritProperties (fromBaseNode:FplValue) = 
        fromBaseNode.GetProperties()
        |> List.iter (fun prty ->
            let prtyName = prty.Type SignatureType.Mixed
            match this.OverrideInheritedObject prtyName _inheritedProperties prty fromBaseNode true with
            | (Some typeName, Some oldFromNode, Some newFromNode) ->
                fromBaseNode.ErrorOccurred <- emitSIG06iagnostic prtyName oldFromNode newFromNode typeName fromBaseNode.StartPos fromBaseNode.EndPos
            | _ ->
                ()
        )

    override this.CheckConsistency() = 
        base.CheckConsistency()
        // check if own declared variables override the inherited ones
        this.GetVariables()
        |> Seq.iter (fun var -> 
            if _inheritedVariables.ContainsKey var.FplId then
                let oldFrom = _inheritedVariables[var.FplId][1]
                let oldFromNode = oldFrom.Type SignatureType.Mixed
                let newFromNode = this.Type SignatureType.Mixed
                let typeName = oldFrom.Name
                // override the old node
                let tuple = List<FplValue>()
                tuple.Add var // own scope variable
                tuple.Add this // the var is from this
                _inheritedVariables[var.FplId] <- tuple
                // emit VAR06, since the inner variable overrides some inherited var
                var.ErrorOccurred <- emitVAR06iagnostic var.FplId oldFromNode newFromNode typeName var.StartPos var.EndPos
        )
        // check if own declared properties override the inherited ones
        this.GetProperties()
        |> Seq.iter (fun prty -> 
            let prtyName = prty.Type SignatureType.Mixed
            if _inheritedProperties.ContainsKey prtyName then
                let oldFrom = _inheritedProperties[prtyName][1]
                let oldFromNode = oldFrom.Type SignatureType.Mixed
                let newFromNode = this.Type SignatureType.Mixed
                let typeName = oldFrom.Name
                // override the old node
                let tuple = List<FplValue>()
                tuple.Add prty // own scope property
                tuple.Add this // the property is from this
                _inheritedProperties[prtyName] <- tuple
                // emit SIG06, since the inner property overrides some inherited property
                prty.ErrorOccurred <- emitSIG06iagnostic prtyName oldFromNode newFromNode typeName prty.StartPos prty.EndPos
        )
        // add inherited variables, if they still do not exist in scope
        _inheritedVariables
        |> Seq.iter (fun kvp ->
            if this.Scope.ContainsKey(kvp.Key) then 
                () // VAR06 was already emitted
            else
                let var = kvp.Value[0]
                this.Scope.Add (kvp.Key, var)
        )
        // add inherited properties, if they still do not exist in scope
        _inheritedProperties
        |> Seq.iter (fun kvp ->
            if this.Scope.ContainsKey(kvp.Key) then 
                () // SIG06 was already emitted
            else
                let prty = kvp.Value[0]
                this.Scope.Add (kvp.Key, prty)
        )


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
                this.ErrorOccurred <- emitVAR03diagnostics this.FplId block.Scope[this.FplId].QualifiedStartPos this.StartPos this.EndPos
            else
                block.Scope.Add(this.FplId, this)

        let addToSimpleFplBlocksScope (block:FplValue) = 
            if block.Scope.ContainsKey(this.FplId) then
                this.ErrorOccurred <- emitVAR03diagnostics this.FplId block.Scope[this.FplId].QualifiedStartPos this.StartPos this.EndPos
            else
                block.Scope.Add(this.FplId, this)
        
        let addToPropertyOrConstructor (property:FplValue) = 
            let parentOfProperty = property.Parent.Value
            if property.Scope.ContainsKey(this.FplId) then
                this.ErrorOccurred <- emitVAR03diagnostics this.FplId property.Scope[this.FplId].QualifiedStartPos this.StartPos this.EndPos
            elif parentOfProperty.Scope.ContainsKey(this.FplId) then
                // check also the scope of the property's parent block
                this.ErrorOccurred <- emitVAR03diagnostics this.FplId parentOfProperty.Scope[this.FplId].QualifiedStartPos this.StartPos this.EndPos
            else
                property.Scope.Add(this.FplId, this)

        let addToProofOrCorolllary (proofOrCorollary:FplValue) = 
            let rec conflictInScope (node:FplValue) formulaConflict =
                if node.Scope.ContainsKey(this.FplId) then
                    this.ErrorOccurred <- emitVAR03diagnostics this.FplId node.Scope[this.FplId].QualifiedStartPos this.StartPos this.EndPos
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

        let addToQuantor (quantor:FplValue) =
            // issue VAR03, if the variable to be bound by the quantor was declared 
            // in the scope the quantor is placed in.
            let rec checkConfictInScope (node:FplValue) =
                if node.Scope.ContainsKey(this.FplId) then
                    this.ErrorOccurred <- emitVAR03diagnostics this.FplId node.Scope[this.FplId].QualifiedStartPos this.StartPos this.EndPos
                else 
                    let parent = node.Parent.Value
                    match parent.Name with
                    | PrimRoot 
                    | PrimTheoryL -> ()
                    | _ ->
                        checkConfictInScope parent 

            checkConfictInScope quantor
            quantor.Scope.TryAdd(this.FplId, this) |> ignore        
        
        let addToVariableOrMapping (variableOrMapping:FplValue) =
            let rec conflictInScope (node:FplValue) =
                if node.Scope.ContainsKey(this.FplId) then
                    this.ErrorOccurred <- emitVAR03diagnostics this.FplId node.Scope[this.FplId].QualifiedStartPos this.StartPos this.EndPos 
                    true
                else 
                    let parent = node.Parent.Value
                    match parent.Name with
                    | PrimRoot 
                    | PrimTheoryL -> false
                    | _ ->
                        conflictInScope parent
            
            if not (conflictInScope variableOrMapping) then
                variableOrMapping.Scope.Add(this.FplId, this)
                let blockOpt = variableOrMapping.UltimateBlockNode
                match blockOpt with
                | Some block -> block.Scope.Add(this.FplId, this)
                | None -> ()
            else
                variableOrMapping.Scope.TryAdd(this.FplId, this) |> ignore

        match nextOpt with 
        | Some next when next.Name = PrimRefL ->
            next.FplId <- this.FplId
            next.TypeId <- this.TypeId
            next.RefersTo <- Some this
        | Some next when ( next.Name = LiteralAxL 
                        || next.Name = LiteralThmL 
                        || next.Name = LiteralLemL 
                        || next.Name = LiteralPropL 
                        || next.Name = LiteralConjL 
                        || next.Name = PrimClassL 
                        || next.Name = PrimFunctionalTermL
                        || next.Name = PrimPredicateL
                        || next.Name = PrimExtensionL
                        || next.Name = PrimTranslationL
                        ) ->
            addToSimpleFplBlocksScope next
        | Some next when next.Name = PrimRuleOfInference ->
            addToRuleOfInference next
        | Some next when ( next.Name = LiteralCtorL
                        || next.Name = PrimMandatoryFunctionalTermL 
                        || next.Name = PrimMandatoryPredicateL) ->
            addToPropertyOrConstructor next
        | Some next when (next.Name = LiteralPrfL 
                        || next.Name = LiteralCorL) ->
            addToProofOrCorolllary next
        | Some next when (next.Name = PrimVariableL
                        || next.Name = PrimVariableArrayL) ->
            addToVariableOrMapping next
        | Some next when next.Name = PrimMappingL ->
            this.SetIsBound() // mapping-Variables are bound
            addToVariableOrMapping next
        | Some next when next.Name = PrimQuantorAll || next.Name = PrimQuantorExists || next.Name = PrimQuantorExistsN ->  
            this.SetIsBound() // quantor-Variables are bound
            if next.Scope.ContainsKey(this.FplId) then
                this.ErrorOccurred <- emitVAR02diagnostics this.FplId this.StartPos this.EndPos
            elif next.Name = PrimQuantorExistsN && next.Scope.Count>0 then 
                this.ErrorOccurred <- emitVAR07diagnostics this.FplId this.StartPos this.EndPos
            elif this.Name = PrimVariableArrayL then 
                this.ErrorOccurred <- emitVAR08diagnostics this.StartPos this.EndPos
            else
                addToQuantor next
                
        | _ -> addExpressionToParentArgList this

    override this.Type signatureType =
        let head = getFplHead this signatureType

        let pars = getParamTuple this signatureType
        let propagate = propagateSignatureType signatureType

        match this.ArgType, pars, getMapping this with
        | ArgType.Parentheses, "", None -> 
            if signatureType = SignatureType.Name then 
                head
            else
                $"{head}({pars})"
        | ArgType.Parentheses, "", Some map -> 
            if signatureType = SignatureType.Name then 
                head
            else
                $"{head}({pars}) -> {map.Type propagate}"
        | ArgType.Parentheses, _, None -> $"{head}({pars})"
        | ArgType.Parentheses, _, Some map -> $"{head}({pars}) -> {map.Type propagate}"
        | ArgType.Nothing, "", None -> head
        | ArgType.Nothing, "", Some map -> $"{head}() -> {map.Type propagate}" 
        | _, _, None -> sprintf "%s(%s)" head pars
        | _, _, Some map -> sprintf "%s(%s) -> %s" head pars (map.Type propagate)

    override this.Run _ = 
        this.Debug Debug.Start
        // FplGenericVariable has nothing to run
        this.Debug Debug.Stop

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
        this.IsInitialized <- otherVar.IsInitialized
        this.RefersTo <- otherVar.RefersTo

let checkVAR04Diagnostics (fv:FplValue) = 
    fv.GetVariables()
    |> List.map (fun var -> var :?> FplGenericVariable)
    |> List.filter(fun var -> not var.IsUsed)
    |> List.iter (fun var -> 
        var.ErrorOccurred <- emitVAR04diagnostics var.FplId var.StartPos var.EndPos
    )

let isArgPred (arg:FplValue) = 
    let argType = arg.Type SignatureType.Type
    (argType, argType.StartsWith(LiteralPred))

/// Checks if an argument of an FplValue is a predicate and issues LG001Diagnostics if its not.
let checkArgPred (fv:FplValue) (arg:FplValue)  = 
    match fv.UltimateBlockNode with 
    | Some node when node.Name = LiteralLocL -> () // skip this check for localizations
    | _ ->
        let argType, isPred = isArgPred (arg:FplValue) 
        if isPred then 
            () 
        else
            let argName = arg.Type SignatureType.Name
            fv.ErrorOccurred <- emitLG001Diagnostics argType argName fv.Name arg.StartPos arg.EndPos

/// Checks if a predicate expression is actually being interpreted as an predicate
let checkPredicateExpressionReturnsPredicate (fv:FplValue) =
    let exprOpt = fv.ArgList |> Seq.tryLast
    match exprOpt with 
    | Some expr -> checkArgPred fv expr
    | None -> ()

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
        checkPredicateExpressionReturnsPredicate this

type FplPredicateList(positions: Positions, parent: FplValue, runOrder) = 
    inherit FplGenericHasNoValue(positions, parent)
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

    override this.Run variableStack = 
        this.Debug Debug.Start
        // this line only makes sure that all Run is called recursively
        // FplPredicateList has no value its own
        this.ArgList |> Seq.map (fun fv -> fv.Run variableStack) |> ignore
        this.Debug Debug.Stop

    override this.EmbedInSymbolTable _ = addExpressionToParentArgList this

    override this.RunOrder = Some _runOrder

type FplRuleOfInference(positions: Positions, parent: FplValue, runOrder) as this =
    inherit FplGenericHasNoValue(positions, parent)
    let _runOrder = runOrder
    let mutable _signStartPos = Position("", 0L, 0L, 0L)
    let mutable _signEndPos = Position("", 0L, 0L, 0L)

    do
        this.FplId <- LiteralUndef
        this.TypeId <- LiteralUndef

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

    override this.Name = PrimRuleOfInference
    override this.ShortName = LiteralInf

    override this.Clone () =
        let ret = new FplRuleOfInference((this.StartPos, this.EndPos), this.Parent.Value, _runOrder)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = getFplHead this signatureType
    
    override this.IsFplBlock () = true
    override this.IsBlock () = true    

    override this.Run variableStack = 
        this.Debug Debug.Start
        // FplRuleOfReference does not have any Value
        this.Debug Debug.Stop

    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        checkVAR04Diagnostics this
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

    override this.Represent() = // done
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
        this.Debug Debug.Start
        // FplInstance is a value representation and has no value on its own
        this.Debug Debug.Stop

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

    override this.EmbedInSymbolTable _ = addExpressionToParentArgList this

    override this.Run variableStack = 
        this.Debug Debug.Start
        // FplBase no value on its own
        this.Debug Debug.Stop

    override this.RunOrder = None

[<AbstractClass>]
type FplGenericConstructor(name, positions: Positions, parent: FplValue) as this =
    inherit FplValue(positions, Some parent)
    let mutable (_toBeConstructedClass:FplValue option) = None 

    do
        this.FplId <- name
        this.TypeId <- name

    override this.Name = PrimDefaultConstructor
    override this.ShortName = LiteralCtor

    override this.Type signatureType =
        let head = getFplHead this signatureType
        let paramT = getParamTuple this signatureType
        match signatureType with
        | SignatureType.Name
        | SignatureType.Mixed -> $"{head}({paramT})" 
        | SignatureType.Type -> head
            
    member this.ToBeConstructedClass  
        with get () = _toBeConstructedClass
        and set (value) = _toBeConstructedClass <- value

    override this.Run variableStack = 
        this.Debug Debug.Start

        let rec createSubInstance (classDef:FplValue) (instance:FplValue) (baseInstance:FplValue)=
            classDef.ArgList
            |> Seq.filter (fun fv -> fv.Name = LiteralBase)
            |> Seq.map (fun fv -> fv :?> FplBase)
            |> Seq.map (fun fv -> fv.RefersTo)
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
        // the value of FplGenericConstructor is the created instance
        this.SetValue instance 
        this.Debug Debug.Stop


    member this.Instance =
        match this.Value with 
        | Some ref -> Some (ref :?> FplInstance)
        | _ -> None

    override this.RunOrder = None

/// This constructor is only used for creating instances of classes that have no declared constructors.
/// In FPL, such classes are "intrinsic". When the default constructor calls the constructor
/// of some base classes, it is only possible if those classes are also intrinsic or have declared constructors
/// without parameters. 
type FplDefaultConstructor(name, positions: Positions, parent: FplValue) =
    inherit FplGenericConstructor(name, positions, parent)

    override this.Name = PrimDefaultConstructor
    override this.ShortName = LiteralCtor

    override this.Clone () =
        let ret = new FplDefaultConstructor(this.FplId, (this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

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

    override this.CheckConsistency () = 
        base.CheckConsistency()
        // check if the constructor calls all necessary parent classes
        let parentClassOpt = this.UltimateBlockNode
        match parentClassOpt with
        | Some (:? FplClass as parentClass) ->
            parentClass.ArgList 
            |> Seq.iter (fun fv -> 
                if not (this.ParentConstructorCalls.Contains fv.FplId) then
                    fv.ErrorOccurred <- emitID020Diagnostics fv.FplId fv.StartPos
            )
        | _ -> ()
        checkVAR04Diagnostics this

    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        tryAddSubBlockToFplBlock this

    member this.ParentClass = this.Parent.Value :?> FplClass

and FplClass(positions: Positions, parent: FplValue, runOrder) as this =
    inherit FplGenericInheriting(positions, parent)
    let _runOrder = runOrder
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
        let ret = new FplClass((this.StartPos, this.EndPos), this.Parent.Value, _runOrder)
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
    
    override this.Type signatureType = getFplHead this signatureType

    override this.Run variableStack = 
        this.Debug Debug.Start
        // initialization of the stmts in the class and/or its constructors and/or properties not needed since
        // it will be done inside instances
        // FplClass has no value on their own
        this.Debug Debug.Stop

    override this.CheckConsistency () = 
        base.CheckConsistency()
        checkVAR04Diagnostics this

    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        tryAddToParentUsingFplId this 

    override this.RunOrder = Some _runOrder

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
        this.Debug Debug.Start
        if not _isReady then
            _callCounter <- _callCounter + 1
            if _callCounter > maxRecursion then
                this.ErrorOccurred <- emitLG002diagnostic (this.Type(SignatureType.Name)) _callCounter variableStack.CallerStartPos variableStack.CallerEndPos
            else
                if this.IsIntrinsic then 
                    let undetermined = new FplIntrinsicPred((this.StartPos, this.EndPos), this)
                    this.SetValue undetermined
                else
                    // run all statements and the last predicate in the FplPredicate
                    this.ArgList |> Seq.iter (fun fv1 -> fv1.Run variableStack) 
                    // Assign the value of the FplPredicate using the last predicate
                    let lastOpt = this.ArgList |> Seq.tryLast
                    match lastOpt with 
                    | Some last -> this.SetValuesOf last
                    | _ -> ()

            _callCounter <- _callCounter - 1
            _isReady <- this.Arity = 0
        this.Debug Debug.Stop


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
        precedences[precedence].ErrorOccurred <- emitSIG02Diagnostics symbol precedence conflict pos1 pos2

let signatureRepresent (fv:FplValue) = 
    let signatureVarRepresentations = 
        fv.GetVariables()
        |> List.map (fun var -> var:?>FplGenericVariable)
        |> List.filter (fun var -> var.IsSignatureVariable) 
        |> List.map (fun var -> var.Represent())
        |> String.concat ", "
    $"{fv.FplId}({signatureVarRepresentations})"

type FplPredicate(positions: Positions, parent: FplValue, runOrder) as this =
    inherit FplGenericInheriting(positions, parent)
    let _runOrder = runOrder
    let mutable _signStartPos = Position("", 0L, 0L, 0L)
    let mutable _signEndPos = Position("", 0L, 0L, 0L)
    let mutable _isReady = false
    let mutable _callCounter = 0

    do 
        this.FplId <- PrimUndetermined
        this.TypeId <- LiteralPred

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

    interface ICanBeCalledRecusively with
        member _.CallCounter = _callCounter

    override this.Name = PrimPredicateL
    override this.ShortName = PrimPredicate

    override this.Clone () =
        let ret = new FplPredicate((this.StartPos, this.EndPos), this.Parent.Value, _runOrder)
        this.AssignParts(ret)
        ret

    override this.IsFplBlock () = true

    override this.IsBlock () = true

    override this.CheckConsistency() = 
        base.CheckConsistency()
        match this.ExpressionType with
        | FixType.Infix _ when this.Arity <> 2 -> this.ErrorOccurred <- emitSIG00Diagnostics this.ExpressionType.Type 2 this.Arity this.SignStartPos this.SignEndPos
        | FixType.Prefix _ when this.Arity <> 1 -> this.ErrorOccurred <- emitSIG00Diagnostics this.ExpressionType.Type 1 this.Arity this.SignStartPos this.SignEndPos
        | FixType.Postfix _ when this.Arity <> 1 -> this.ErrorOccurred <- emitSIG00Diagnostics this.ExpressionType.Type 1 this.Arity this.SignStartPos this.SignEndPos
        | _ -> ()
        match this.ExpressionType with
        | FixType.Infix (symbol, precedence) -> checkSIG02Diagnostics (getRoot this) symbol precedence this.SignStartPos this.SignEndPos
        | _ -> ()
        checkPredicateExpressionReturnsPredicate this

    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        if not this.IsIntrinsic then // if not intrinsic, check variable usage
            checkVAR04Diagnostics this
        tryAddToParentUsingMixedSignature this
        
    override this.Type signatureType = 
        let head = getFplHead this signatureType

        let paramT = getParamTuple this signatureType
        sprintf "%s(%s)" head paramT

    override this.Run variableStack = 
        this.Debug Debug.Start
        if not _isReady then
            _callCounter <- _callCounter + 1
            if _callCounter > maxRecursion then
                this.ErrorOccurred <- emitLG002diagnostic (this.Type(SignatureType.Name)) _callCounter variableStack.CallerStartPos variableStack.CallerEndPos
            else
                if this.IsIntrinsic then 
                    let undetermined = new FplIntrinsicPred((this.StartPos, this.EndPos), this)
                    this.SetValue undetermined
                else
                    // run all statements and the last predicate in the FplPredicate
                    this.ArgList |> Seq.iter (fun fv1 -> fv1.Run variableStack) 
                    // Assign the value of the FplPredicate using the last predicate
                    let lastOpt = this.ArgList |> Seq.tryLast
                    match lastOpt with 
                    | Some last -> this.SetValuesOf last
                    | _ -> ()

            _callCounter <- _callCounter - 1
            _isReady <- this.Arity = 0        
            this.GetProperties() |> List.iter (fun fv -> fv.Run variableStack)
        this.Debug Debug.Stop

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

let runArgumentsOfGenericPredicateWithExpression (fv:FplValue) variableStack = 
    fv.ArgList
    |> Seq.iter (fun fv1 -> 
        fv1.Run variableStack
        if fv1.Value.IsNone then 
            fv.ErrorOccurred <- emitLG004diagnostic fv.Name fv1.StartPos fv1.EndPos
        else
            fv.SetValuesOf fv1
    )

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
        this.Debug Debug.Start
        if not _isReady then
            runArgumentsOfGenericPredicateWithExpression this variableStack
            _isReady <- true
            this.ErrorOccurred <- emitLG003diagnostic (this.Type(SignatureType.Name)) this.Name (this.Represent()) this.StartPos this.EndPos
        this.Debug Debug.Stop

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
        this.Debug Debug.Start
        if not _isReady then
            runArgumentsOfGenericPredicateWithExpression this variableStack
            _isReady <- true
            this.ErrorOccurred <- emitLG003diagnostic (this.Type(SignatureType.Name)) this.Name (this.Represent()) this.StartPos this.EndPos

            // evaluate all corollaries and proofs of the theorem-like statement
            this.Scope.Values
            |> Seq.filter (fun fv -> fv.Name = LiteralPrfL || fv.Name = LiteralCorL) 
            |> Seq.sortBy (fun block -> block.RunOrder.Value) 
            |> Seq.iter (fun fv -> 
                fv.Run variableStack
            )

        if not _hasProof then 
           this.ErrorOccurred <- emitPR007Diagnostics (this.Type(SignatureType.Name)) this.Name this.SignStartPos this.SignEndPos
        this.Debug Debug.Stop

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
                    ScopeSearchResult.FoundIncorrectBlock (qualifiedName potentialOther false)
                else
                    ScopeSearchResult.NotFound
            | None -> ScopeSearchResult.NotApplicable


        match tryFindAssociatedBlockForCorollary this with
        | ScopeSearchResult.FoundAssociate potentialParent -> 
            // everything is OK, change the parent of the provable from theory to the found parent 
            this.Parent <- Some potentialParent
        | ScopeSearchResult.FoundIncorrectBlock incorrectBlock ->
            this.ErrorOccurred <- emitID005diagnostics this.FplId incorrectBlock this.StartPos this.EndPos
        | ScopeSearchResult.NotFound ->
            this.ErrorOccurred <- emitID006diagnostics this.FplId this.StartPos this.EndPos
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
        this.Debug Debug.Start
        if not _isReady then
            runArgumentsOfGenericPredicateWithExpression this variableStack
            _isReady <- true
        this.Debug Debug.Stop


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
    inherit FplGenericPredicate(positions, parent)

    override this.ShortName = PrimJustification

    override this.Type signatureType =
        let head = getFplHead this signatureType
        head

    override this.EmbedInSymbolTable _ = 
        let thisJustificationItemId = this.Type(SignatureType.Mixed)

        let alreadyAddedIdOpt = 
            this.Parent.Value.ArgList
            |> Seq.map (fun argJi -> argJi.Type(SignatureType.Mixed))
            |> Seq.tryFind (fun otherId -> otherId = thisJustificationItemId)
        match alreadyAddedIdOpt with
        | Some otherId ->
            this.ErrorOccurred <- emitPR004Diagnostics thisJustificationItemId otherId this.StartPos this.EndPos 
        | _ -> ()
        addExpressionToParentArgList this

    member this.ParentJustification = this.Parent.Value :?> FplJustification

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

    override this.Run _ = 
        this.Debug Debug.Start
        match this.RefersTo with 
        | Some ref when ref.Value.IsSome ->
            let refType, isRefPred = isArgPred ref
            if isRefPred then 
                // because the ref must proceed "this" in FPL Code, it was already run
                // so we only need to copy its value into this
                this.SetValuesOf ref
            else
                // if there is a value but ref is not a predicate, 
                // set the value of "this" to undetermined
                let value = new FplIntrinsicPred((this.StartPos, this.EndPos), this) 
                this.SetValue value
                // and issue diagnostics saying that this requires a predicate
                let refName = ref.Type SignatureType.Name
                this.ErrorOccurred <- emitLG001Diagnostics refType refName this.Name ref.StartPos ref.EndPos
        | Some ref when ref.Value.IsNone ->
            // set the value of "this" to undetermined
            let value = new FplIntrinsicPred((this.StartPos, this.EndPos), this) 
            this.SetValue value
            // TODO issue diagnostics saying that a predicate expression was expected but has No value
            ()
        | _ -> 
            // set the value of "this" to undetermined
            let value = new FplIntrinsicPred((this.StartPos, this.EndPos), this) 
            this.SetValue value
            () // no diagnostics needed because covered by ID010, ID023, or PR001
        this.Debug Debug.Stop

and FplJustificationItemByAx(positions: Positions, parent: FplValue) =
    inherit FplGenericJustificationItem(positions, parent)

    override this.Name = PrimJIByAx

    override this.Clone () =
        let ret = new FplJustificationItemByAx((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

and FplJustificationItemByDef(positions: Positions, parent: FplValue) =
    inherit FplGenericJustificationItem(positions, parent)

    override this.Name = PrimJIByDef

    override this.Clone () =
        let ret = new FplJustificationItemByDef((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

and FplJustificationItemByDefVar(positions: Positions, parent: FplValue) =
    inherit FplGenericJustificationItem(positions, parent)

    override this.Name = PrimJIByDefVar

    override this.Clone () =
        let ret = new FplJustificationItemByDefVar((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

and FplJustificationItemByConj(positions: Positions, parent: FplValue) =
    inherit FplGenericJustificationItem(positions, parent)

    override this.Name = PrimJIByConj

    override this.Clone () =
        let ret = new FplJustificationItemByConj((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

and FplJustificationItemByCor(positions: Positions, parent: FplValue) =
    inherit FplGenericJustificationItem(positions, parent)

    override this.Name = PrimJIByCor

    override this.Clone () =
        let ret = new FplJustificationItemByCor((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

and FplJustificationItemByInf(positions: Positions, parent: FplValue) =
    inherit FplGenericJustificationItem(positions, parent)

    override this.Name = PrimJIByInf

    override this.Clone () =
        let ret = new FplJustificationItemByInf((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

and FplJustificationItemByRefArgument(positions: Positions, parent: FplValue) =
    inherit FplGenericJustificationItem(positions, parent)

    override this.Name = PrimJIByRefArgument

    override this.Clone () =
        let ret = new FplJustificationItemByRefArgument((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

and FplJustificationItemByProofArgument(positions: Positions, parent: FplValue) =
    inherit FplGenericJustificationItem(positions, parent)

    override this.Name = PrimJIByProofArgument

    override this.Clone () =
        let ret = new FplJustificationItemByProofArgument((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret


and FplJustificationItemByTheoremLikeStmt(positions: Positions, parent: FplValue) =
    inherit FplGenericJustificationItem(positions, parent)

    override this.Name = PrimJIByTheoremLikeStmt

    override this.Clone () =
        let ret = new FplJustificationItemByTheoremLikeStmt((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

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
        // TODO implement Run
        this.Debug Debug.Start
        let v = new FplIntrinsicPred((this.StartPos, this.EndPos), this)
        this.Value <- Some v
        this.Debug Debug.Stop


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
        // TODO implement Run, assume should return true if the assumption was possible
        this.Debug Debug.Start
        let v = new FplIntrinsicPred((this.StartPos, this.EndPos), this)
        this.Value <- Some v
        this.Debug Debug.Stop

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
        // TODO implement Run
        this.Debug Debug.Start
        let v = new FplIntrinsicPred((this.StartPos, this.EndPos), this)
        this.Value <- Some v
        this.Debug Debug.Stop

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
        this.Debug Debug.Start
        let value = new FplIntrinsicPred((this.StartPos, this.EndPos), this) 
        value.FplId <- LiteralTrue
        this.SetValue value
        this.Debug Debug.Stop

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
        // TODO implement run
        this.Debug Debug.Start
        let v = new FplIntrinsicPred((this.StartPos, this.EndPos), this)
        this.Value <- Some v
        this.Debug Debug.Stop

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
        this.Debug Debug.Start
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
        (* TODO: Enhance variableStack by the context in which this argument is being evaluated
            Here are some preliminary considerations: 
            1) The context should include 
                a) the argumentInference of the previous argument (if such exists) - the first argument doesn't have such a predecessor
                b) if the argument has more than justification, variableStack should store a list of applying the previous argumentInference from a) each justification sorted by their RunOrder 
                   so then next justification from the list can be applied to the last result from variableStack. 
                   The idea is that a list of justification could be "unzipped" in the FPL code by writing a sequence
                   of arguments, each having only a single justification from the original list. This unzipped FPL code should be semantically
                   the same as "zipping/hiding" the arguments by listing multiply justifications and only inferring to the last argumentInference.
                c) possibly the structure of the to-be-proven predicate of the original theorem
            2) The evaluation of the argument should then handle the following cases
                a) whether or not the argumentInference of this argument is an FplAssume or FplRevoke 
                b) whether or not the justification is an axiom
                c) whether or not the justification is a rule of inference
                d) whether or not the justification is a by definition
        *)
        let v = new FplIntrinsicPred((this.StartPos, this.EndPos), this)
        this.Value <- Some v

        this.Debug Debug.Stop



    override this.EmbedInSymbolTable _ = 
        let (proof:FplProof) = this.ParentProof
        if proof.HasArgument (this.FplId) then 
            let conflict = proof.Scope[this.FplId]
            this.ErrorOccurred <- emitPR003Diagnostics this.FplId conflict.QualifiedStartPos this.StartPos this.EndPos 
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
    override this.IsProof () = true

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
        this.Debug Debug.Start
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
            this.ErrorOccurred <- emitPR009Diagnostics this.StartPos this.StartPos
            let v = new FplIntrinsicPred((this.SignStartPos, this.SignEndPos), this)
            this.SetValue v
        else
            let v = new FplIntrinsicPred((this.SignStartPos, this.SignEndPos), this)
            v.FplId <- LiteralTrue
            this.SetValue v
        this.Debug Debug.Stop

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
                        sprintf "'%s' %s" potentialOther.Name (qualifiedName potentialOther false)
                    )
                else
                    ScopeSearchResult.NotFound
            | None -> ScopeSearchResult.NotApplicable


        match tryFindAssociatedBlockForProof this with
        | ScopeSearchResult.FoundAssociate potentialParent -> 
            // everything is OK, change the parent of the provable from theory to the found parent 
            this.Parent <- Some potentialParent
        | ScopeSearchResult.FoundIncorrectBlock incorrectBlock ->
            this.ErrorOccurred <- emitID002Diagnostics this.FplId incorrectBlock this.StartPos this.EndPos
        | ScopeSearchResult.NotFound ->
            this.ErrorOccurred <- emitID003diagnostics this.FplId this.StartPos this.EndPos
        | _ -> ()
        tryAddToParentUsingFplId this

    override this.RunOrder = Some _runOrder


let getArgumentInProof (fv1:FplGenericJustificationItem) argName =
    let proof = 
        match fv1 with 
        | :? FplJustificationItemByProofArgument when fv1.RefersTo.IsSome ->
            fv1.RefersTo.Value :?> FplProof
        | _ ->
            let parent = fv1.ParentJustification
            let arg = parent.ParentArgument
            arg.ParentProof
    if proof.HasArgument argName then 
        Some proof.Scope[argName]
    else 
        None

type FplLocalization(positions: Positions, parent: FplValue, runOrder) =
    inherit FplValue(positions, Some parent)
    let _runOrder = runOrder
    let mutable _currentLanguage = ""

    override this.Name = LiteralLocL
    override this.ShortName = LiteralLoc

    override this.Clone () =
        let ret = new FplLocalization((this.StartPos, this.EndPos), this.Parent.Value, _runOrder)
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

    override this.Represent() = // done
        if this.Scope.ContainsKey(_currentLanguage) then
            let language = this.Scope[_currentLanguage]
            language.Represent() // represent the current language
        else
            this.Type(SignatureType.Name) 
        
    override this.IsBlock() = true

    override this.Run variableStack = 
        this.Debug Debug.Start
        _currentLanguage <- variableStack.CurrentLanguage // remember current language for Represent()
        if not (this.Scope.ContainsKey(_currentLanguage)) then
            let expression = this.ArgList[0]
            this.ErrorOccurred <- emitST004diagnostics _currentLanguage expression.StartPos expression.EndPos
        this.Debug Debug.Stop

    override this.RunOrder = Some _runOrder

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

    override this.Represent() = // done
        this.FplId // represent according to string in the FplId of the translation term

    override this.Run variableStack = 
        this.Debug Debug.Start
        // no run necessary 
        this.Debug Debug.Stop

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

    override this.Represent() = // done
        // concatenate all translations of the language
        this.ArgList
        |> Seq.map (fun transl -> transl.Represent()) 
        |> String.concat " "

    override this.Run _ = 
        this.Debug Debug.Start
        // no run necessary 
        this.Debug Debug.Stop

    override this.EmbedInSymbolTable _ = 
        let parent = this.Parent.Value
        if parent.Scope.ContainsKey(this.FplId) then 
            let conflict = parent.Scope[this.FplId]
            this.ErrorOccurred <- emitID014Diagnostics this.FplId conflict.QualifiedStartPos this.StartPos this.EndPos 
        else
            parent.Scope.Add(this.FplId, this)

    override this.RunOrder = None

let isLanguage (fv:FplValue) =
    match fv with
    | :? FplLanguage -> true
    | _ -> false

[<AbstractClass>]
type FplGenericStmt(positions: Positions, parent: FplValue) =
    inherit FplGenericHasNoValue(positions, parent)

    override this.ShortName = PrimStmt

    override this.Type signatureType = this.FplId

    override this.EmbedInSymbolTable _ = addExpressionToParentArgList this

type FplAssertion(positions: Positions, parent: FplValue) =
    inherit FplGenericStmt(positions, parent)

    override this.Name = PrimAssertion
    override this.ShortName = LiteralAss

    override this.Clone () =
        let ret = new FplAssertion((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Run variableStack = 
        // TODO implement run
        this.Debug Debug.Start
        this.Debug Debug.Stop

    override this.RunOrder = None

type FplIntrinsicTpl(name, positions: Positions, parent: FplValue) as this =
    inherit FplGenericHasNoValue(positions, parent)

    do
        this.TypeId <- name
        this.FplId <- name

    override this.Name = PrimIntrinsicTpl
    override this.ShortName = LiteralTpl

    override this.Clone () =
        let ret = new FplIntrinsicTpl(this.TypeId, (this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type (signatureType:SignatureType) = 
        match this.RefersTo with
        | Some fv -> 
            // if the template was used, its representation is the 
            // type signature of how it was used
            fv.Type SignatureType.Type  
        | None -> 
            // otherwise, the representation defaults to the user-defined name of the template
            getFplHead this signatureType

    override this.Run _ = 
        this.Debug Debug.Start
        // A template has no value
        this.Debug Debug.Stop

    /// Sets a template usage and issues a specific diagnostics if a type conflict occurs.
    /// The caller decides with specific diagnostics to issue. If the diagnostics is unhandled, 
    /// GEN00 diagnostic will be issued as default.
    member this.TrySetTemplateUsage (fv:FplValue) diagnostic = 
        match this.RefersTo with 
        | None -> 
            this.RefersTo <- Some fv // if this template was not used, use it
        | Some templateUsage -> 
            // otherwise calculate the type signatures of the very first usage  
            let firstUsage = templateUsage.Type SignatureType.Type
            // and the current one
            let currentUsage = fv.Type SignatureType.Type
            // compare both
            match currentUsage with 
            | LiteralUndef -> () // Usage "undef" is always accepted
            | _  when firstUsage <> currentUsage ->
                // issue diagnostics, if inconsistent usage
                match diagnostic with 
                | "SIG12" -> this.ErrorOccurred <- emitSIG12diagnostics this.FplId currentUsage firstUsage (templateUsage.QualifiedStartPos) fv.StartPos fv.EndPos
                | "SIG13" -> this.ErrorOccurred <- emitSIG13diagnostics this.FplId currentUsage firstUsage (templateUsage.QualifiedStartPos) fv.StartPos fv.EndPos
                | _ -> emitUnexpectedErrorDiagnostics $"Unhandled diagnostic `{diagnostic}` in FplIntrinsicTpl.TrySetTemplateUsage."
            | _ -> () // equal usage is accepted

    override this.EmbedInSymbolTable _ = tryAddTemplateToParent this 

    override this.RunOrder = None

type FplIntrinsicUndef(positions: Positions, parent: FplValue) as this =
    inherit FplGenericHasNoValue(positions, parent)
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
                    
    override this.Represent() = // done
        LiteralUndef 

    override this.Run variableStack = 
        this.Debug Debug.Start
        // FplIntrinsicUndef is a value of not defined FPL objects and has no value on its own
        this.Debug Debug.Stop

    override this.EmbedInSymbolTable _ = addExpressionToReference this

    override this.RunOrder = None

[<AbstractClass>]
type FplGenericReference(positions: Positions, parent: FplValue) =
    inherit FplValue(positions, Some parent)
    
    override this.Clone () = this // do not clone references to prevent stack overflow 

    override this.Run variableStack =
        this.Debug Debug.Start
        let calledOpt = referencedNodeOpt this
        match calledOpt with 
        | Some called ->
            match called.Name with
            | LiteralCtorL
            | PrimDefaultConstructor
            | PrimBaseConstructorCall
            | PrimPredicateL
            | PrimFunctionalTermL
            | PrimMandatoryFunctionalTermL
            | PrimMandatoryPredicateL ->
                match box called, this.NextBlockNode with
                | :? ICanBeCalledRecusively as calledRecursively, Some blockNodeOfThis when 
                    Object.ReferenceEquals(blockNodeOfThis, called) && 
                    calledRecursively.CallCounter > maxRecursion -> () // stop recursion
                | _ ->
                    let pars = variableStack.SaveState(called) 
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
                    variableStack.RestoreState(called)
            | PrimExtensionObj 
            | PrimDelegateDecrementL
            | PrimDelegateEqualL ->
                called.Run variableStack
                this.SetValuesOf called
            | PrimInstanceL
            | PrimIntrinsicInd
            | PrimIntrinsicUndef
            | PrimIntrinsicTpl 
            | PrimExtensionObj
            | PrimIntrinsicPred ->
                this.SetValue called
            | _ -> ()
        | _ -> ()
        this.Debug Debug.Stop

    override this.RunOrder = None

/// Issue VAR10, if the formula in an FplValue uses 
/// quantor(s) and the variables bound by these quantor(s) are used elsewhere in the same formula
/// VAR10 => formula should be cleaned up by renaming the bound variables
let checkCleanedUpFormula (fv:FplValue) =
    let formulaCreationInSymbolTableCompleted (formula:FplValue) =
        match formula.Parent with 
        | Some parent ->
            match parent.Name with 
            | PrimConjunction
            | PrimDisjunction
            | PrimImplication
            | PrimEquivalence
            | PrimExclusiveOr
            | PrimNegation
            | PrimQuantorAll
            | PrimQuantorExists
            | PrimQuantorExistsN
            | PrimIsOperator
            | PrimRefL -> false
            | _ -> true
        | _ -> true

    let rec usedVariablesInFormula (formula:FplValue) = 
        let extractFromSubFormula (subFormula:FplValue) =
            subFormula.ArgList 
            |> Seq.map (fun subF -> usedVariablesInFormula subF)
            |> List.concat
        match formula.Name with 
        | PrimRefL when formula.RefersTo.IsSome ->
            match formula.RefersTo with
            | Some (:? FplGenericVariable) -> [formula] 
            | None when checkStartsWithLowerCase formula.FplId -> 
                [formula]  
            | _ -> extractFromSubFormula formula 
        | PrimQuantorAll
        | PrimQuantorExists
        | PrimQuantorExistsN -> (formula.Scope.Values |> Seq.toList) @ extractFromSubFormula formula.ArgList[0]  
        | _ ->
            extractFromSubFormula formula 

    let rec extractQuantors (formula:FplValue) =
        let extractFromSubFormula (subFormula:FplValue) =
            (subFormula.ArgList |> Seq.map (fun subF -> extractQuantors subF) |> List.concat)
        match formula.Name with 
        | PrimQuantorAll
        | PrimQuantorExists
        | PrimQuantorExistsN -> 
            [formula] @ extractFromSubFormula formula
        | _ -> 
            extractFromSubFormula formula

    let rec checkQuantors (formula:FplValue) =
        let varUsedInQuantor (varInFormula:FplValue) (quantor:FplValue) =
            let varLStart = varInFormula.StartPos.Line
            let varCStart = varInFormula.StartPos.Column
            let varLEnd = varInFormula.EndPos.Line
            let varCEnd = varInFormula.EndPos.Column
            let quantorLStart = quantor.StartPos.Line
            let quantorCStart = quantor.StartPos.Column
            let quantorLEnd = quantor.EndPos.Line
            let quantorCEnd = quantor.EndPos.Column
            (
               (quantorLStart < varLStart && quantorLEnd > varLEnd) // lines(quantor) contain lines(variable)
            || (quantorLStart = varLStart && quantorLEnd > varLEnd && quantorCStart <= varCStart ) // if start line(q) = start line line(v) && end line(q) > end line(v), compare starting columns
            || (quantorLStart = varLStart && quantorLEnd = varLEnd && quantorCStart <= varCStart && quantorCEnd >= varCEnd) // if line(q) = line(v) for start and end, compare starting and ending columns
            )

        let varIsBoundByQuantor (varInFormula:FplValue) (quantor:FplValue) =
            quantor.Scope.ContainsKey(varInFormula.FplId)

        let quantors = extractQuantors formula
        let usedVariables = usedVariablesInFormula formula
        if quantors.Length > 0 then 
            usedVariables
            |> List.iter(fun varInFormula ->
                quantors 
                |> List.iter (fun quantor ->
                    if varIsBoundByQuantor varInFormula quantor && 
                        not (varUsedInQuantor varInFormula quantor) then 
                        let quantorVar = quantor.Scope[varInFormula.FplId]
                        fv.ErrorOccurred <- emitVAR10diagnostics varInFormula.FplId varInFormula.QualifiedStartPos quantorVar.StartPos quantorVar.EndPos
                )
            )                
            
    if formulaCreationInSymbolTableCompleted fv then
        // here, this reference points to a formula, which is final in the symbol table
        checkQuantors fv


type FplReference(positions: Positions, parent: FplValue) =
    inherit FplGenericReference(positions, parent)
    let mutable _callCounter = 0 

    let mutable _dottedChild : FplValue option = None

    override this.Name = PrimRefL
    override this.ShortName = PrimRef

    /// The optional dotted child set when parsing a dotted reference 
    member this.DottedChild
        with get() = _dottedChild
        and set (value:FplValue option) = _dottedChild <- value

    interface IHasDotted with 
        member this.DottedChild 
            with get () = this.DottedChild
            and set (value) = this.DottedChild <- value

    override this.SetValue fv = 
        if this.Scope.ContainsKey(this.FplId) then
            let var = this.Scope[this.FplId]
            var.SetValue fv
            base.SetValue fv
        else
            base.SetValue fv

    override this.Type signatureType =
        let headObj = 
            match this.RefersTo with
            | Some ret -> ret
            | None -> this

        let propagate = propagateSignatureType signatureType

        // The arguments are reserved for the arguments or the coordinates of the reference
        let args, argsCount =
            let ret = 
                this.ArgList
                |> Seq.map (fun fv -> fv.Type(propagate))
                |> String.concat ", "
            ret, this.ArgList.Count

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
            | SignatureType.Type, _, _ -> headObj.TypeId
            | _ -> ret

        let fallBackFunctionalTerm =
            let varMappingOpt = getMapping headObj
            match varMappingOpt with 
            | Some varMapping ->
                match headObj.Name with 
                | PrimFunctionalTermL when signatureType = SignatureType.Type -> 
                    varMapping.Type propagate
                | PrimMandatoryFunctionalTermL when signatureType = SignatureType.Type -> 
                    varMapping.Type propagate
                | _ -> 
                    $"{head}({args}) -> {varMapping.Type propagate}"
            | None ->
                $"{head}({args})"

        let fallBackValueClosure =
            let varMappingOpt = getMapping headObj
            match varMappingOpt with 
            | Some varMapping ->
                match headObj.Name with 
                | PrimFunctionalTermL when signatureType = SignatureType.Type -> 
                    varMapping.Type propagate
                | PrimMandatoryFunctionalTermL when signatureType = SignatureType.Type -> 
                    varMapping.Type propagate
                | _ -> 
                    $"{head}({args}) -> {varMapping.Type propagate}"
            | None when signatureType = SignatureType.Type ->
                head
            | _ ->
                $"{head}({args})"

        match argsCount, this.ArgType, this.DottedChild with
            | 0, ArgType.Nothing, Some qualification when propagate = SignatureType.Type ->
                qualification.Type propagate
            | 0, ArgType.Nothing, Some qualification ->
                $"{head}.{qualification.Type propagate}"
            | 0, ArgType.Brackets, Some qualification ->
                $"{head}[].{qualification.Type propagate}"
            | 0, ArgType.Parentheses, Some qualification ->
                $"{head}().{qualification.Type propagate}"
            | 0, ArgType.Nothing, None -> 
                match headObj.Name with 
                | PrimVariableArrayL 
                | PrimPredicateL 
                | PrimMandatoryPredicateL 
                | PrimFunctionalTermL 
                | PrimMandatoryFunctionalTermL -> $"{headObj.Type signatureType}"
                | _ -> head
            | 0, ArgType.Brackets, None ->
                $"{head}[]"
            | 0, ArgType.Parentheses, None ->
                fallBackValueClosure
            | 1, ArgType.Nothing, None -> 
                if head <> String.Empty then 
                    $"{head}({args})"
                else
                    args
            | _, ArgType.Nothing, Some qualification -> 
                $"{head}({args}).{qualification.Type propagate}"
            | _, ArgType.Brackets, Some qualification ->
                $"{head}[{args}].{qualification.Type propagate}"
            | _, ArgType.Parentheses, Some qualification ->
                $"{head}({args}).{qualification.Type propagate}"
            | _, ArgType.Nothing, None -> 
                $"{head}({args})"
            | _, ArgType.Brackets, None ->
                $"{head}[{args}]"
            | _, ArgType.Parentheses, None ->
                fallBackValueClosure

    override this.Represent() = // done
        if _callCounter > maxRecursion then
            this.ErrorOccurred <- emitLG002diagnostic (this.Type(SignatureType.Name)) _callCounter this.StartPos this.EndPos
            LiteralUndef // fallback to undefined after infinite recursion (if any)
        else
            _callCounter <- _callCounter + 1
            let result = 
                match this.Value, this.DottedChild, this.RefersTo with 
                | _, Some dc, _ -> 
                    if not (Object.ReferenceEquals(dc, this)) then
                        // If the dotted child is not identical as "this",
                        // delegate the representation to dotted.
                        dc.Represent()
                    else
                        // Otherwise, fall back with dotted's "type representation" to prevent infinite loops
                        dc.Type SignatureType.Mixed
                | Some value, _, _ ->
                    if not (Object.ReferenceEquals(value,this)) then
                        // If the value is not identical as "this",
                        // delegate the representation to value.
                        value.Represent()
                    else
                        // Otherwise, fall back with "undef" to prevent infinite loops
                        LiteralUndef
                | _, _, Some refTo when refTo.Name = LiteralSelf && refTo.ErrorOccurred.IsSome ->
                    // infinite loop or other error in self detected
                    // fallback to undefined
                    LiteralUndef 
                | _, _, Some refTo ->
                    if not (Object.ReferenceEquals(refTo,this)) then
                        // If refTo is not identical as "this",
                        // delegate the representation to refTo.
                        refTo.Represent()
                    else
                        refTo.Type SignatureType.Mixed
                | _, _, _ ->
                    this.Type SignatureType.Mixed
            _callCounter <- _callCounter - 1
            result

    override this.CheckConsistency () = 
        base.CheckConsistency()
        checkCleanedUpFormula this

    override this.EmbedInSymbolTable nextOpt = 
        this.CheckConsistency()
        match nextOpt with 
        | Some next when next.IsBlock() ->
            addExpressionToParentArgList this 
        | Some next when next.Name = PrimForInStmtDomainL -> 
            next.RefersTo <- Some this
        | Some (:? FplReference as next) when next.DottedChild.IsSome -> 
            next.EndPos <- this.EndPos
        | Some next when (next.Name = PrimMapCaseElseL || next.Name = PrimMapCaseSingleL) -> 
            addExpressionToParentArgList this
            next.TypeId <- this.TypeId
            next.EndPos <- this.EndPos
        | Some next -> 
            addExpressionToParentArgList this
            next.EndPos <- this.EndPos
        | _ -> ()            

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
            |> Seq.filter (fun fv1 -> isDefinition fv1)
            |> Seq.iter (fun block ->
                match block.ExpressionType with
                | FixType.Prefix symbol
                | FixType.Symbol symbol
                | FixType.Postfix symbol ->
                    if expressionId = symbol then
                        fv.RefersTo <- Some block 
                        fv.TypeId <- block.TypeId
                | FixType.Infix(symbol, precedence) ->
                    if expressionId = symbol then
                        fv.RefersTo <- Some block 
                        fv.TypeId <- block.TypeId
                | _ -> ()))

        if fv.RefersTo.IsNone then
            fv.ErrorOccurred <- emitSIG01Diagnostics expressionId fv.StartPos fv.EndPos
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

    /// Sets this mapping to an array-typed mapping.
    member this.SetIsArray() = _isArrayMapping <- true

    member this.Dimensionality = _dimensionTypes.Count

    member this.DimensionTypes = _dimensionTypes

    /// Sets the during the symbol table construction.
    /// Because the type consists of a main type and index allowed-types, we use "Dimension being set" as a flag
    /// to decide which one to be set.
    member this.SetType (typeId:string) (typeNodeOpt:FplValue option) pos1 pos2 = 
        if not _dimensionTypesBeingSet then 
            this.TypeId <-
                if _isArrayMapping then 
                    $"*{typeId}"
                else
                    typeId
            this.RefersTo <- typeNodeOpt 
            _dimensionTypesBeingSet <- true
        else
            let indexAllowedType = FplMapping((pos1,pos2), this) 
            indexAllowedType.TypeId <- typeId
            indexAllowedType.RefersTo <- typeNodeOpt 
            this.DimensionTypes.Add indexAllowedType

    interface IHasDimensions with
        member this.Dimensionality = _dimensionTypes.Count
        member this.DimensionTypes = _dimensionTypes
        member this.SetType typeId typeNodeOpt pos1 pos2 = this.SetType typeId typeNodeOpt pos1 pos2

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
            match this.ArgType, myMapping with
            | ArgType.Parentheses, None -> $"{this.TypeId}({pars})"
            | _, None -> this.TypeId
            | _, Some map -> $"{this.TypeId}({pars}) -> {map.Type(propagate)}" 

        if not _isArrayMapping then
            mainType
        else
            let dimensionTypes = 
                this.DimensionTypes
                |> Seq.map (fun fv -> fv.Type signatureType)
                |> String.concat ","
            $"{mainType}[{dimensionTypes}]"

    override this.Represent() = // done
        // a fall back value representation for intrinsic functional terms
        $"dec {this.Type(SignatureType.Type)}"

    override this.Run _ = 
        this.Debug Debug.Start
        // FplMapping has nothing to do in run
        this.Debug Debug.Stop

    override this.EmbedInSymbolTable _ = addExpressionToParentArgList this 

    override this.RunOrder = None

type FplVariableArray(fplId, positions: Positions, parent: FplValue) =
    inherit FplGenericVariable(fplId, positions, parent)
    let _dimensionTypes = new List<FplValue>()
    let mutable _dimensionTypesBeingSet = false
    let _valueKeys = new Dictionary<string,int>() // used to store the keys of all values
    let _valueList = List<FplValue>()

    member this.Dimensionality = _dimensionTypes.Count

    member this.DimensionTypes = _dimensionTypes

    member this.ValueKeys = _valueKeys

    /// Sets the during the symbol table construction.
    /// Because the type consists of a main type and index allowed-types, we use "Dimension being set" as a flag
    /// to decide which one to be set.
    member this.SetType (typeId:string) (typeNodeOpt:FplValue option) pos1 pos2 = 
        if not _dimensionTypesBeingSet then 
            this.TypeId <- $"*{typeId}"
            // TODO prefer RefersTo over Scope when storing the type node of the variable array 
            match typeNodeOpt with 
            | Some typeNode -> this.Scope.TryAdd(typeId, typeNode) |> ignore
            | _ -> ()
            _dimensionTypesBeingSet <- true
        else
            let indexAllowedType = FplMapping((pos1,pos2), this) 
            indexAllowedType.TypeId <- typeId
            indexAllowedType.RefersTo <- typeNodeOpt
            this.DimensionTypes.Add indexAllowedType

    interface IHasDimensions with
        member this.Dimensionality = _dimensionTypes.Count
        member this.DimensionTypes = _dimensionTypes
        member this.SetType typeId typeNodeOpt pos1 pos2 = this.SetType typeId typeNodeOpt pos1 pos2

    override this.Name = PrimVariableArrayL

    override this.ShortName = PrimVariableArray

    member this.CopyValueKeys (ret:FplVariableArray) = 
        this.ValueKeys.Clear()
        this.ValueKeys
            |> Seq.iter (fun kvp ->
                ret.ValueKeys.Add(kvp.Key, kvp.Value)
            )

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

        this.CopyValueKeys ret

        ret

    /// ValueList of the FplVariableArray.
    member this.ValueList = _valueList

    member this.AssignValueToCoordinates (coordinates:FplValue seq) (value:FplValue) =
        this.IsInitialized <- true
        let coordinatesKey = 
            coordinates
            |> Seq.map (fun fv -> fv.Represent())
            |> String.concat "|"
        if this.ValueKeys.ContainsKey coordinatesKey then 
           let index = this.ValueKeys[coordinatesKey]
           // a value with this coordinates already exists, and we replace it by the new one
           this.ValueList[index] <- value
        else
            // a value with this coordinates does not exist yet. We ann the value 
            this.ValueList.Add value
            // and store the index of the new coordinatesKey
            this.ValueKeys.Add (coordinatesKey, this.ValueList.Count-1)

    override this.Type signatureType =
        let mainType = base.Type signatureType

        let dimensionTypes = 
            this.DimensionTypes
            |> Seq.map (fun fv -> fv.Type signatureType)
            |> String.concat ","

        match signatureType with
        | SignatureType.Name -> this.FplId
        | _ -> $"{mainType}[{dimensionTypes}]"

    override this.Represent() = // done
        if this.ValueList.Count = 0 then
            if this.IsInitialized then 
                // this case should never happen, because isInitializesVariable is a contradiction to ValueList.Count 0
                LiteralUndef
            else
                match this.TypeId with
                | LiteralUndef -> LiteralUndef
                | _ -> $"dec {this.Type SignatureType.Type}"
        else
            // ensure canonical order of keys
            let sortedKeys = 
                let sortByCoordinates (items: string seq) =
                    let parseCoord (coord: string) =
                        if coord.StartsWith "$" then
                            // Numeric coordinate: return Left(int)
                            let n = coord.Substring(1) |> int
                            Choice1Of2 n
                        else
                            // Alphabetic coordinate: return Right(string)
                            Choice2Of2 coord

                    let keyOf (s: string) =
                        s.Split('|')
                        |> Array.map parseCoord
                        |> Array.toList

                    items
                    |> Seq.sortBy keyOf
                sortByCoordinates this.ValueKeys.Keys 
            
            let subRepr = 
                sortedKeys
                |> Seq.map (fun coordinatesKey -> 
                    let index = this.ValueKeys[coordinatesKey]
                    let valueRepr = this.ValueList[index].Represent()
                    $"[{coordinatesKey}]->{valueRepr}"
                )
                |> String.concat ", "
            if this.IsInitialized then 
                subRepr
            else
                match this.TypeId with
                | LiteralUndef -> LiteralUndef
                | _ -> $"dec {this.Type(SignatureType.Type)}" 

    override this.CheckConsistency () = 
        base.CheckConsistency()

    
    override this.SetValue fv =
        base.SetValue fv
        if fv.FplId <> LiteralUndef then
            this.IsInitialized <- true
        match fv with 
        | :? FplVariableArray as arr -> arr.CopyValueKeys this
        | _ -> ()

    override this.SetValuesOf fv =
        base.SetValuesOf fv
        if fv.FplId <> LiteralUndef then
            this.IsInitialized <- true
        match fv with 
        | :? FplVariableArray as arr -> arr.CopyValueKeys this
        | _ -> ()

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
        base.SetValue fv
        if fv.FplId <> LiteralUndef then
            this.IsInitialized <- true
            match this.RefersTo with 
            | Some (:? FplIntrinsicTpl as tpl) -> tpl.TrySetTemplateUsage fv (SIG12("", "", "", "").Code)
            | _ -> ()
                

    override this.SetValuesOf fv =
        base.SetValuesOf fv
        if fv.FplId <> LiteralUndef then
            this.IsInitialized <- true

    override this.Represent() = // done
        match this.Value with 
        | None ->
            match this.TypeId with
            | LiteralUndef -> LiteralUndef
            | LiteralPred -> PrimUndetermined
            | _ -> $"dec {this.Type(SignatureType.Type)}" 
        | Some ref ->
            let subRepr = ref.Represent()
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
        fv.GetVariables()
    | PrimVariableArrayL ->
        match box fv with 
        | :? IHasDimensions as arr -> arr.DimensionTypes |> Seq.toList
        | _ -> []
    | PrimFunctionalTermL
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
    | PrimFunctionalTermL 
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
            | PrimFunctionalTermL, PrimFunctionalTermL 
            | PrimClassL, PrimClassL ->
                bNode.ArgList
                |> Seq.filter (fun subNode -> subNode :? FplBase)
                |> Seq.iter (fun subNode ->
                    findChains subNode currName newPath 
                )
            | PrimFunctionalTermL, LiteralBase 
            | PrimClassL, LiteralBase ->
                match bNode.RefersTo with 
                | Some nextBNode ->
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
                | None ->
                    if paths.ContainsKey newPath then 
                        paths[newPath] <- $"duplicate inheritance detected, `{newPath}`." 
                    else
                        paths.Add (newPath, "ok")
            | _ -> ()
            
    match baseNode.Name with 
    | PrimClassL
    | PrimPredicateL
    | PrimFunctionalTermL -> ()
    | _ -> failwith ($"Expecting a class, a functional term, or a predicate node, got {baseNode.Name}")
    
    findChains baseNode "" ""
    if paths.Count = 0 then 
        distinctNames |> Seq.iter (fun s -> paths.Add (s, "ok"))
    paths


/// Checks if a node inherits from some type (or is already that type).
let inheritsFrom (node:FplValue) someType = 
    match node, someType with 
    | :? FplClass, "obj" -> true
    | :? FplClass, _  when node.FplId = someType -> true
    | _ -> 
        let inheritanceList = findInheritanceChains node 
        let inheritanceFound = 
            inheritanceList 
            |> Seq.filter (fun kvp -> 
                kvp.Value = "ok" && 
                (
                   kvp.Key = someType 
                || kvp.Key.EndsWith $":{someType}" 
                || kvp.Key.Contains $":{someType}:"
                )
            )
            |> Seq.tryLast
        match inheritanceFound with 
        | Some _ -> true
        | None -> false

type MatchingMode =
    | Assignment
    | Signature

type Parameter =
    | Consumed
    | NotConsumed

/// Determines if the FplValue has parentheses and has an upper case FplId
let isCallByValue (fv:FplValue) =
    match fv.ArgType with 
    | ArgType.Parentheses when isUpper fv.FplId -> true
    | _ -> false

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
        sprintf "%s" head

    override this.Represent() = // done 
        let enclosingBlockOpt = this.UltimateBlockNode
        match enclosingBlockOpt, this.RefersTo with 
        | Some enclosingBlock, Some ref when Object.ReferenceEquals(enclosingBlock, ref) ->
            this.FplId // when this occurs inside its own extension definition, return FplId
        | _, None ->
            this.FplId // when this has no occurs inside its own extension definition, return FplId
        | _, _ when this.Value.IsSome ->
            this.Value.Value.Represent()
        | _, _ ->
            this.FplId // in all other cases return FplId

    override this.Run variableStack = 
        this.Debug Debug.Start
        match this.RefersTo with 
        | Some extensionDefinition ->
            let pars = variableStack.SaveState(extensionDefinition)
            if pars.Length = 1 then
                let extVar = pars[0]
                extVar.SetValue this
            else
                () // should never occur since extensionDefinition has syntactically always only one parameters
            // store the position of the caller
            variableStack.CallerStartPos <- this.StartPos
            variableStack.CallerEndPos <- this.EndPos
            // run all statements of the called node
            extensionDefinition.Run variableStack
            match extensionDefinition.Value with 
            | Some v when Object.ReferenceEquals(this, v) -> 
                // we have the case that an extension definition evaluated to the same FplExtensionObj 
                // it was referred from. In this case, we have to prevent the FplExtensionObj to be set with itself as its value
                // we create a new Extension Obj
                let v = new FplExtensionObj((this.StartPos, this.EndPos), this)
                v.FplId <- this.FplId
                v.TypeId <- this.TypeId
                this.SetValue v
            | _ ->
                this.SetValuesOf extensionDefinition
            variableStack.RestoreState(extensionDefinition)
        | _ ->
            let v = new FplIntrinsicUndef((this.StartPos, this.EndPos), this)
            // fall back to undef, if this does not refer to any extension definition
            this.SetValue v
        this.Debug Debug.Stop


    override this.CheckConsistency () = 
        base.CheckConsistency()
        let matchReprId (fv1:FplValue) (identifier:string) = 
            let vars = fv1.GetVariables()
            if vars.Length > 0 then
                let mainVar = vars.Head
                // starts always with "obj:", we retrieve only the regex string after it
                let retrieveRegex = mainVar.TypeId.Substring(4) 
                let regex = Regex(retrieveRegex)
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
                        // assign the reference FplValue only the first found match 
                        // even, if there are multiple extensions that would match it 
                        // (thus, the additional check for Scope.ContainsKey...)
                        this.RefersTo <- Some ext
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
                    // its own pattern even if it is not yet fully parsed and analyzed
                    candidatesFromScope @ [ext]
                else 
                    candidatesFromScope
            | _ -> candidatesFromScope

        if candidates.Length = 0 then 
            this.ErrorOccurred <- emitID018Diagnostics this.FplId this.StartPos this.EndPos

    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()    
        addExpressionToReference this

    override this.RunOrder = None

type FplExtension(positions: Positions, parent: FplValue, runOrder) =
    inherit FplValue(positions, Some parent)
    let _runOrder = runOrder
    let mutable _callCounter = 0
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
        let ret = new FplExtension((this.StartPos, this.EndPos), this.Parent.Value, _runOrder)
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

    override this.Run variableStack = 
        this.Debug Debug.Start
        _callCounter <- _callCounter + 1
        if _callCounter > maxRecursion then
            this.ErrorOccurred <- emitLG002diagnostic (this.Type(SignatureType.Name)) _callCounter variableStack.CallerStartPos variableStack.CallerEndPos
        else
            if this.ArgList.Count = 0 then 
                let v = FplIntrinsicUndef((this.StartPos, this.EndPos), this)
                this.SetValue v
            else
                this.ArgList
                |> Seq.iter (fun fv -> 
                    fv.Run variableStack
                    this.SetValuesOf fv
                )
        _callCounter <- _callCounter - 1
        this.Debug Debug.Stop

    override this.EmbedInSymbolTable _ = tryAddToParentUsingMixedSignature this

    override this.RunOrder = Some _runOrder

let private errMsgStandard aName aType pName pType = Some $"`{aName}:{aType}` doesn't match `{pName}:{pType}`"
let private errMsgMissingArgument pName pType = Some $"Missing argument for `{pName}:{pType}`"
let private errMsgMissingParameter aName aType = Some $"No matching parameter for `{aName}:{aType}`"
let private errMsgCallByRefToClass aName classType = Some $"Assignee `{aName}` references to a class `{classType}`. Consider initializing `{aName}` with a class constructor `{classType}(...)`."
let private errMsgDirectClassAssignment classType = Some $"Assignee is a class `{classType}`. Use a class constructor `{classType}(...)` instead."

let private matchClassInheritance (clOpt:FplValue option) aName aType (pName:string) (pType:string) = 
    let pTypeSimple =
        if pType.StartsWith("*") then 
            let ret = pType.Substring(1).Split("[")
            ret[0]
        else
            pType
    match clOpt with 
    | Some cl -> 
        if inheritsFrom cl pTypeSimple then 
            None
        else
            Some $"`{aName}:{aType}` neither matches `{pName}:{pType}` nor the base classes"
    | _ -> 
        Some($"{LiteralUndefL} `{aName}:{aType}` doesn't match `{pName}:{pType}`")


let private matchByTypeStringRepresentation (a:FplValue) aName (aType:string) aTypeName (p:FplValue) pName (pType:string) pTypeName mode = 

    match mode with 
    | _ when aType = pType ->
        None, Parameter.Consumed
    | _ when aType = LiteralUndef ->
        None, Parameter.Consumed // undef matches any type
    | _ when pType.StartsWith(LiteralTpl) || pType.StartsWith(LiteralTplL) ->
        None, Parameter.Consumed // tpl accepts everything: TODO: really?
    | _ when pType.StartsWith($"*{LiteralTpl}") || pType.StartsWith($"*{LiteralTplL}") ->
        None, Parameter.Consumed // tpl arrays accepts everything: TODO: really?
    | MatchingMode.Assignment when aType = LiteralUndef ->
        None, Parameter.Consumed // undef can always be assigned
    | MatchingMode.Assignment when pType.StartsWith($"*{aType}[") && pTypeName = PrimVariableArrayL ->
        None, Parameter.Consumed // assignee array accepting assigned value
    | MatchingMode.Signature when pType.StartsWith($"*{aType}[{LiteralInd}]") ->
        None, Parameter.NotConsumed // 1D arrays matching input type with ind as index accept variadic enumerations
    | _ when pType.StartsWith($"*{aType}[") ->
        // array parameters with indexes that differ from the FPL-inbuilt index type  
        // or with multidimensional index types will not accept variadic enumerations of arguments
        // even if they have the same type used for the values of the array
        Some $"variadic enumeration of `{aName}:{aType}` doesn't match `{pName}:{pType}`, try `{aName}:{pType}` as argument or use parameter `{pName}:{p.TypeId}[{LiteralInd}]`", Parameter.Consumed
    | _ when aType.StartsWith($"*{pType}[") && aTypeName = PrimRefL ->
        let refA = a :?> FplReference
        if refA.ArgType = ArgType.Brackets then 
            // some array elements matching parameter type
            None, Parameter.Consumed
        else
            Some $"Array type `{aName}:{aType}` doesn't match `{pName}:{pType}`", Parameter.Consumed
    | _ when isUpper aType && aTypeName = PrimRefL && a.RefersTo.IsSome ->
        let aReferencedNode = a.RefersTo.Value
        if aReferencedNode.RefersTo.IsSome then
            let aRef = aReferencedNode.RefersTo.Value
            match aRef with
            | :? FplClass ->
                matchClassInheritance (Some aRef) aName aType pName pType, Parameter.Consumed 
            | :? FplExtension as extension ->
                let map = extension.Mapping 
                matchClassInheritance map.RefersTo aName aType pName pType, Parameter.Consumed  
            | _ ->
                // this case does not occur but we cover it for completeness reasons
                Some $"undefined `{aName}:{aType}` doesn't match `{pName}:{pType}`", Parameter.Consumed
        elif aReferencedNode.Name = PrimDefaultConstructor || aReferencedNode.Name = LiteralCtorL then 
            let ctor = aReferencedNode :?> FplGenericConstructor
            matchClassInheritance ctor.ToBeConstructedClass aName aType pName pType, Parameter.Consumed
        elif aReferencedNode.Name = PrimFunctionalTermL || aReferencedNode.Name = PrimMandatoryFunctionalTermL then 
            let mapOpt = getMapping aReferencedNode
            let map = mapOpt.Value :?> FplMapping 
            matchClassInheritance map.RefersTo aName aType pName pType, Parameter.Consumed
        elif aReferencedNode.Name = PrimVariableL then 
            matchClassInheritance aReferencedNode.RefersTo aName aType pName pType, Parameter.Consumed
        else
            Some $"undefined `{aName}:{aType}` doesn't match `{pName}:{pType}`", Parameter.Consumed
    | _ when aType.StartsWith(pType + "(") ->
        None, Parameter.Consumed
    | _ when aType.StartsWith(LiteralPred) && pType = LiteralPred ->
        None, Parameter.Consumed
    | _ when aType.StartsWith(LiteralFunc) && pType = LiteralFunc->
        None, Parameter.Consumed
    | MatchingMode.Assignment when aTypeName = PrimVariableL ->
        let clOpt = a.Scope.Values |> Seq.tryHead
        match clOpt with 
        | Some (:? FplClass) -> matchClassInheritance clOpt aName aType pName pType, Parameter.Consumed
        | _ -> errMsgStandard aName aType pName pType, Parameter.Consumed
    | MatchingMode.Assignment when aTypeName = PrimDefaultConstructor || aTypeName = LiteralCtorL ->
        let ctor = a :?> FplGenericConstructor
        matchClassInheritance ctor.ToBeConstructedClass aName aType pName pType, Parameter.Consumed
    | _ when pTypeName = PrimFunctionalTermL || pTypeName = PrimMandatoryFunctionalTermL ->
        let mappingOpt = getMapping p 
        match mappingOpt with 
        | Some mapping ->
            let newTypeAssignedValue = mapping.Type SignatureType.Type
            if aType <> newTypeAssignedValue then 
                errMsgStandard aName aType pName pType, Parameter.Consumed
            else 
                None, Parameter.Consumed
        | None -> None, Parameter.Consumed
    | _ ->
        errMsgStandard aName aType pName pType, Parameter.Consumed

let private isPredWithParentheses (fv:FplValue) =
    match fv.ArgType with 
    | ArgType.Parentheses when fv.TypeId.StartsWith(LiteralPred) -> true
    | _ -> false

let private isPredWithoutParentheses (fv:FplValue) =
    match fv.ArgType with 
    | ArgType.Nothing when fv.TypeId = LiteralPred -> true
    | _ -> false

let private isFuncWithParentheses (fv:FplValue) =
    match fv.ArgType with 
    | ArgType.Parentheses when fv.TypeId.StartsWith(LiteralFunc) -> true
    | _ -> false

let private isFuncWithoutParentheses (fv:FplValue) =
    match fv.ArgType with 
    | ArgType.Nothing when fv.TypeId = LiteralFunc -> true
    | _ -> false

/// Checks if an FplValue is a reference to a variable that points to a class, and at the same time is marked as 'initialized' and still does not any values.
/// (this is the convention flagging that a variable has been assigned to its class instead of the constructor of the class generating an instance value).
/// If the function returns a non-empty string, it contains the identifier of the referenced class (that has not been instantiated).
let private getCallByReferenceToClass (fv:FplValue) =
    match fv.RefersTo with 
    | Some refNode ->
        match refNode with 
        | :? FplGenericVariable as var when var.IsInitialized && var.Value.IsNone ->
            // reference fv points to an initialized variable without values 
            match var.RefersTo with
            | Some fv1 when fv1.Name = PrimClassL -> fv1.TypeId // and the variable points to a class
            | _ -> String.Empty
        | _ -> String.Empty
    | None ->
        String.Empty
               
let rec private isCallByReference (fv:FplValue) =
    match fv with 
    | :? FplReference as ref when ref.DottedChild.IsSome ->
        isCallByReference ref.DottedChild.Value // evaluate dotted reference instead
    | _ ->
        match fv.ArgType with 
        | ArgType.Nothing when isUpper fv.FplId -> true
        | ArgType.Nothing -> true
        | _ -> false

let private getNames (fv:FplValue) = 
    let fvName = fv.Type SignatureType.Name
    let fvType = fv.Type SignatureType.Type
    let fvTypeName = fv.Name
    fvName, fvType, fvTypeName

let rec private matchTwoTypes (a:FplValue) (p:FplValue) (mode:MatchingMode) =
    let aName, aType, aTypeName = getNames a 
    let pName, pType, pTypeName = getNames p

    match aTypeName, pTypeName with 
    | PrimClassL, PrimVariableL when mode = MatchingMode.Assignment ->
        errMsgDirectClassAssignment aType, Parameter.Consumed
    | PrimRefL, PrimVariableL
    | PrimRefL, PrimMappingL ->
        let aIsCallByReference = isCallByReference a
        let callByReferenceToClass = getCallByReferenceToClass a
        let refNodeOpt = referencedNodeOpt a
        if callByReferenceToClass <> String.Empty then 
            errMsgCallByRefToClass aName callByReferenceToClass, Parameter.Consumed
        elif aIsCallByReference && isPredWithParentheses p then 
            // match a call by reference with pred with parameters
            match refNodeOpt with 
            | Some refNode when refNode.Name = PrimPredicateL ->
                matchTwoTypes refNode p mode // match signatures with parameters
            | Some refNode when refNode.Name = PrimIntrinsicUndef -> 
                None, Parameter.Consumed // mapping pred(...) accepting undef
            | Some refNode when refNode.Name = PrimMandatoryPredicateL ->
                matchTwoTypes refNode p mode // match signatures with parameters
            | Some refNode when refNode.Name = PrimVariableL && refNode.TypeId = LiteralPred ->
                matchTwoTypes refNode p mode // match signatures with parameters
            | Some refNode ->
                // a node was referenced but is not a predicate
                Some $"The return type of {qualifiedName refNode true} doesn't match `{pType}`.", Parameter.Consumed
            | _ ->
                // in all other cases, 
                errMsgStandard aName aType pName pType, Parameter.Consumed
        elif aIsCallByReference && isPredWithoutParentheses p then
            // match a not-by-value-reference with pred mapping without parameters
            match refNodeOpt with 
            | Some refNode when refNode.Name = PrimIntrinsicPred ->
                None, Parameter.Consumed // pred accepting intrinsic predicates
            | Some refNode when refNode.Name = PrimIntrinsicUndef -> 
                None, Parameter.Consumed // mapping pred accepting undef
            | Some refNode when refNode.Name = PrimPredicateL ->
                None, Parameter.Consumed // pred accepting predicate nodes
            | Some refNode when refNode.Name = PrimMandatoryPredicateL ->
                None, Parameter.Consumed // pred accepting predicate properties
            | Some refNode when refNode.Name = LiteralAxL ->
                None, Parameter.Consumed // pred accepting axioms
            | Some (:? FplGenericTheoremLikeStmt as refNode) ->
                None, Parameter.Consumed // pred accepting theorem-like statements
            | Some refNode when refNode.Name = LiteralConjL ->
                None, Parameter.Consumed // pred accepting conjectures
            | Some refNode when refNode.Name = PrimVariableL && refNode.TypeId = LiteralPred ->
                None, Parameter.Consumed // pred accepting pred variables
            | Some refNode when refNode.Name = PrimExtensionObj ->
                matchTwoTypes refNode p mode // match signatures with parameters
            | Some refNode ->
                // a node was referenced not a predicate node
                Some $"The return type of {qualifiedName refNode true} doesn't match mapping type `{pType}`.", Parameter.Consumed
            | _ ->
                // in all other cases, error
                errMsgStandard aName aType pName pType, Parameter.Consumed
        elif aIsCallByReference && isFuncWithParentheses p then
            // match a not-by-value-reference with func mapping with parameters
            match refNodeOpt with 
            | Some refNode when refNode.Name = PrimIntrinsicUndef -> 
                None, Parameter.Consumed // mapping func(...)->.. accepting undef
            | Some refNode when refNode.Name = PrimFunctionalTermL ->
                matchTwoTypes refNode p mode // match signatures with parameters
            | Some refNode when refNode.Name = PrimMandatoryFunctionalTermL ->
                matchTwoTypes refNode p mode // match signatures with parameters
            | Some refNode when refNode.Name = PrimVariableL && refNode.TypeId = LiteralFunc ->
                matchTwoTypes refNode p mode // match signatures with parameters
            | Some refNode ->
                // a node was referenced but is not a functional term block
                Some $"The return type of {qualifiedName refNode true} does not match mapping type `{pType}`.", Parameter.Consumed
            | _ ->
                // in all other cases, error
                errMsgStandard aName aType pName pType, Parameter.Consumed
        elif aIsCallByReference && isFuncWithoutParentheses p then 
            // match a not-by-value-reference with func mapping with parameters
            match refNodeOpt with 
            | Some refNode when refNode.Name = PrimIntrinsicUndef -> 
                None, Parameter.Consumed // mapping func accepting undef
            | Some refNode when refNode.Name = PrimFunctionalTermL ->
                None, Parameter.Consumed // func accepting functional term nodes
            | Some refNode when refNode.Name = PrimMandatoryFunctionalTermL ->
                None, Parameter.Consumed // func accepting functional term properties
            | Some refNode when refNode.Name = PrimVariableL && refNode.TypeId = LiteralFunc ->
                None, Parameter.Consumed // func accepting func variables
            | Some refNode when refNode.Name = PrimExtensionObj ->
                matchTwoTypes refNode p mode // match signatures with parameters
            | Some refNode ->
                // a node was referenced but is not a functional term block
                Some $"The return type of {qualifiedName refNode true} does not match expected mapping type `{pType}`.", Parameter.Consumed
            | _ ->
                // in all other cases, error
                Some $"Return type of `{aName}:{aType}` does not match expected mapping type `{pType}`.", Parameter.Consumed
        elif aIsCallByReference && pTypeName = PrimMappingL then 
            let map = p :?> FplMapping
            match map.RefersTo, refNodeOpt with
            | Some def, Some refNode when refNode.Name = PrimInstanceL -> 
                matchTwoTypes a def mode
            | Some _, Some refNode when refNode.Name = PrimIntrinsicUndef -> 
                None, Parameter.Consumed // definition accepting undef
            | Some def, Some (:? FplGenericVariable as refNode) when not refNode.IsInitialized  -> 
                matchTwoTypes a def mode
            | Some def, Some (:? FplExtensionObj as extObj) -> 
                matchTwoTypes extObj def mode
            | None, Some refNode when map.TypeId = LiteralObj && refNode.Name = PrimInstanceL -> 
                None, Parameter.Consumed // obj accepting instance
            | None, Some (:? FplGenericVariable as refNode) when map.TypeId = LiteralObj && not refNode.IsInitialized -> 
                let refNodeOpt1 = referencedNodeOpt refNode
                match refNodeOpt1 with 
                | Some (:? FplClass as cl) -> None, Parameter.Consumed // obj accepting instance
                | _ when refNode.TypeId = LiteralObj && aType = pType -> None, Parameter.Consumed // obj accepting obj variable
                | _ -> errMsgStandard aName aType pName pType, Parameter.Consumed
            | None, Some refNode when refNode.Name = PrimIntrinsicUndef -> 
                None, Parameter.Consumed // anything accepting undef
            | None, Some refNode -> 
                matchTwoTypes refNode map mode
            | None, None when aType = pType && isUpper aType -> 
                Some $"`{aName}:{aType}` matches the expected type `{pType}` but the type is undefined.", Parameter.Consumed
            | None, None when aType = pType -> 
                None, Parameter.Consumed // obj accepting obj, ind accepting ind, pred accepting pred, func accepting func
            | _, _ -> 
                errMsgStandard aName aType pName pType, Parameter.Consumed
        else 
            matchByTypeStringRepresentation a aName aType aTypeName p pName pType pTypeName mode
    | _ ,_ -> 
        matchByTypeStringRepresentation a aName aType aTypeName p pName pType pTypeName mode


/// Tries to match a list of arguments with a list of parameters by their type recursively.
/// The comparison depends on MatchingMode.
let rec mpwa (args: FplValue list) (pars: FplValue list) mode =
    match (args, pars) with
    | (a :: ars, p :: prs) ->
        match matchTwoTypes (a:FplValue) (p:FplValue) mode with
        | Some errMsg, _ -> Some errMsg
        | None, Parameter.Consumed -> mpwa ars prs mode
        | None, Parameter.NotConsumed -> mpwa ars pars mode // handle variadic parameters
    | ([], p :: prs) ->
        let pName, pType, pTypeName = getNames p
        match p with 
        | :? FplClass as cl ->
            let constructors = cl.GetConstructors()
            if constructors.Length = 0 then
                None
            else
                errMsgMissingArgument pName pType
        | _ when pTypeName = PrimVariableArrayL ->
            None
        | _ -> 
            errMsgMissingArgument pName pType
    | (a :: _, []) ->
        let aName, aType, aTypeName = getNames a
        errMsgMissingParameter aName aType
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
            Some $"calling `{fva.Type SignatureType.Name}` and called `{fvp.Type SignatureType.Name}` nodes have mismatching use of parentheses"
        else
            mpwa arguments parameters MatchingMode.Signature

    match argResult with
    | Some aErr -> 
        match fvp.Name with 
        | PrimVariableArrayL ->
            Some($"{aErr} in {qualifiedName fvp true}:{fvp.Type SignatureType.Type}")
        | _ -> 
            Some($"{aErr} in {qualifiedName fvp true}")
    | None -> None

/// Tries to match the signatures of toBeMatched with the signatures of all candidates and accumulates any
/// error messages in accResultList.
let rec checkCandidates (toBeMatched: FplValue) (candidates: FplValue list) (accResultList: string list) =
    match candidates with
    | [] -> (None, accResultList)
    | candidate :: candidates ->
        match matchArgumentsWithParameters toBeMatched candidate with
        | None -> (Some candidate, [])
        | Some errMsg -> checkCandidates toBeMatched candidates (accResultList @ [ errMsg ])

/// Checks if there is a candidate among the candidates that matches the signature of a calling FplValue and returns this as an option.
let checkSIG04Diagnostics (calling:FplValue) (candidates: FplValue list) = 
    if candidates.Length = 0 then
        None
    else
        match checkCandidates calling candidates [] with
        | (Some candidate,_) -> Some candidate // no error occurred
        | (None, errList) -> 
            let errListStr = 
                errList 
                |> List.mapi (fun i s -> 
                    if errList.Length > 1 then 
                        sprintf "%d) %s" (i + 1) s
                    else
                        sprintf "%s" s
                )
                |> String.concat ", "
            calling.ErrorOccurred <- emitSIG04Diagnostics (calling.Type SignatureType.Mixed) candidates.Length errListStr calling.StartPos calling.EndPos
            None

/// Checks if a reference to an array matches its dimensions (in terms of number and types)
let checkSIG08_SIG10Diagnostics (referenceToArray:FplValue) =
    let rec matchIndexesWithDimensions (refToArray:FplReference) =
        match refToArray.RefersTo with
        | Some (:? FplVariableArray as varArray) ->
            let rec matchAllIndexes (indexes:FplValue list) (dims:FplValue list) dimNumber =
                match indexes, dims with
                | i::ixs, d::dms ->
                    match mpwa [i] [d] MatchingMode.Signature with
                    | Some errMsg ->
                        // type mismatch between dimension and index
                        refToArray.ErrorOccurred <- emitSIG08diagnostics varArray.FplId i.FplId (i.Type SignatureType.Type) (d.Type SignatureType.Type) dimNumber i.StartPos i.EndPos 
                        matchAllIndexes ixs dms (dimNumber + 1) 
                    | _ -> matchAllIndexes ixs dms (dimNumber + 1) 
                | [], d::dms -> 
                    // missing index for dimension dimOrdinal
                    refToArray.ErrorOccurred <- emitSIG09diagnostics varArray.FplId (d.Type SignatureType.Type) dimNumber d.StartPos d.EndPos
                    matchAllIndexes [] dms (dimNumber + 1) 
                | i::ixs, [] -> 
                    // array has less dimensions, index at dimOrdinal not supported
                    refToArray.ErrorOccurred <- emitSIG10diagnostics varArray.FplId (i.FplId) dimNumber i.StartPos i.EndPos
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

    override this.CheckConsistency() = 
        base.CheckConsistency()

        // Check the base constructor call's id is the same as one of the classes this class is derived from,
        let outerClassOpt = this.UltimateBlockNode
        let enclosingConstructorOpt = this.NextBlockNode

        let registerParentConstructor() =
            match enclosingConstructorOpt with 
            | Some (:? FplConstructor as ctor) ->
                if ctor.ParentConstructorCalls.Contains(this.FplId) then 
                    // issue duplicate constructor call diagnostics
                    this.ErrorOccurred <- emitID021Diagnostics this.FplId this.StartPos
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
                match baseClassObject.RefersTo with
                | Some baseClass ->
                    // now, try to match a constructor of the parentClass based on the signature of this base constructor call
                    match baseClass.IsIntrinsic, this.ArgList.Count with
                    | true, 0 ->
                        // call of a constructor of an intrinsic class (i.e., that is missing any constructor) with 0 parameters
                        // add "default constructor reference"
                        let defaultConstructor = new FplDefaultConstructor(baseClass.FplId, (this.StartPos, this.EndPos), this)
                        defaultConstructor.EmbedInSymbolTable defaultConstructor.Parent
                        defaultConstructor.ToBeConstructedClass <- Some baseClass
                        registerParentConstructor()
                    | true, _ ->
                        // the call uses parameters that are not possible for calling a non-existing constructor 
                        // obj() or an intrinsic class
                        this.ErrorOccurred <- emitID022Diagnostics baseClass.FplId this.StartPos this.EndPos
                    | false, _ ->
                        let parentClass = baseClass :?> FplClass
                        let constructors = parentClass.GetConstructors()
                        match checkSIG04Diagnostics this constructors with
                        | Some ctor ->
                            let name = ctor.Type SignatureType.Mixed
                            this.Scope.TryAdd(name, ctor) |> ignore
                        | None -> ()
                        registerParentConstructor()
                | None ->
                    // the base constructor call's id is not among the base classes this class is derived from
                    let candidates = outerClass.ArgList |> Seq.map (fun fv -> fv.FplId) |> Seq.sort |> String.concat ", "
                    this.ErrorOccurred <- emitID017Diagnostics this.FplId candidates this.StartPos this.EndPos
            | _ ->
                    this.ErrorOccurred <- emitID017Diagnostics this.FplId "" this.StartPos this.EndPos
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
    let mutable _callCounter = 0

    do 
        this.FplId <- LiteralParent
        this.TypeId <- LiteralUndef

    override this.Name = LiteralParent
    override this.ShortName = LiteralParent

    override this.Clone() = this // do not clone FplParent to prevent stack overflow 

    override this.Type signatureType = 
        match this.RefersTo with 
        | Some ref -> ref.Type signatureType
        | _ -> LiteralParent

    override this.Represent() = // done
        match this.RefersTo with 
        | Some ref -> 
            if _callCounter > maxRecursion then
                this.ErrorOccurred <- emitLG002diagnostic (this.Type(SignatureType.Name)) _callCounter this.StartPos this.EndPos
                LiteralUndef
            else
                _callCounter <- _callCounter + 1
                let result = ref.Represent()
                _callCounter <- _callCounter - 1
                result
        | _ -> LiteralUndef

    override this.Run variableStack = 
        this.Debug Debug.Start
        // FplParent has no value, unless it has a representable RefersTo
        this.Debug Debug.Stop

    override this.EmbedInSymbolTable _ = addExpressionToReference this

    override this.RunOrder = None

/// Reference to "self" using the FPL self keyword. 
// It will point to the enclosing block inside FPL predicate definitions, functional terms, and properties. Otherwise, it is undefined.
type FplSelf(positions: Positions, parent: FplValue) as this =
    inherit FplValue(positions, Some parent)
    let mutable _callCounter = 0

    do 
        this.FplId <- LiteralSelf
        this.TypeId <- LiteralUndef

    override this.Name = LiteralSelf
    override this.ShortName = LiteralSelf

    override this.Clone() = this // do not clone FplSelf to prevent stack overflow 

    override this.Type signatureType = 
        match this.RefersTo with 
        | Some ref -> ref.Type signatureType
        | _ -> LiteralSelf

    override this.Represent() = // done
        match this.RefersTo with 
        | Some ref -> 
            if _callCounter > maxRecursion then
                this.ErrorOccurred <- emitLG002diagnostic (this.Type(SignatureType.Name)) _callCounter this.StartPos this.EndPos
                LiteralUndef
            else
                _callCounter <- _callCounter + 1
                let result = ref.Represent()
                _callCounter <- _callCounter - 1
                result
        | _ -> LiteralUndef

    override this.Run variableStack = 
        this.Debug Debug.Start
        // FplSelf has no value, unless it has a representable RefersTo
        this.Debug Debug.Stop

    override this.EmbedInSymbolTable _ = addExpressionToReference this

    override this.RunOrder = None

/// Checks if an argument points to a free variable and if so, issues VAR09 diagnostics.
let checkFreeVar (arg:FplValue) = 
    match arg.RefersTo with 
    | Some ref ->
        match ref, ref.UltimateBlockNode with 
        | :? FplGenericVariable as var, Some node when node.Name <> PrimRuleOfInference && node.Name <> LiteralLocL && not var.IsBound ->
            var.ErrorOccurred <- emitVAR09diagnostics var.FplId var.TypeId var.StartPos var.EndPos
        | _ -> ()
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
        this.Debug Debug.Start
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
        this.Debug Debug.Stop

    override this.CheckConsistency() = 
        base.CheckConsistency() 
        let arg1 = this.ArgList[0]
        let arg2 = this.ArgList[1]
        checkArgPred this arg1
        checkArgPred this arg2
        checkFreeVar arg1
        checkFreeVar arg2
        checkCleanedUpFormula this


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
        this.Debug Debug.Start
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
        this.Debug Debug.Stop
        
    override this.CheckConsistency() = 
        base.CheckConsistency() 
        let arg1 = this.ArgList[0]
        let arg2 = this.ArgList[1]
        checkArgPred this arg1
        checkArgPred this arg2
        checkFreeVar arg1
        checkFreeVar arg2
        checkCleanedUpFormula this

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
        this.Debug Debug.Start
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
        this.Debug Debug.Stop

    override this.CheckConsistency() = 
        base.CheckConsistency() 
        let arg1 = this.ArgList[0]
        let arg2 = this.ArgList[1]
        checkArgPred this arg1
        checkArgPred this arg2
        checkFreeVar arg1
        checkFreeVar arg2
        checkCleanedUpFormula this


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
        this.Debug Debug.Start
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
        this.Debug Debug.Stop

    override this.CheckConsistency() = 
        base.CheckConsistency()
        let arg = this.ArgList[0]
        checkArgPred this arg
        checkFreeVar arg
        checkCleanedUpFormula this

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
        this.Debug Debug.Start
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
        this.Debug Debug.Stop
        
        this.SetValue(newValue) 

    override this.CheckConsistency() = 
        base.CheckConsistency() 
        let arg1 = this.ArgList[0]
        let arg2 = this.ArgList[1]
        checkArgPred this arg1
        checkArgPred this arg2
        checkFreeVar arg1
        checkFreeVar arg2
        checkCleanedUpFormula this

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
        this.Debug Debug.Start
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
        this.Debug Debug.Stop

    override this.CheckConsistency() = 
        base.CheckConsistency() 
        let arg1 = this.ArgList[0]
        let arg2 = this.ArgList[1]
        checkArgPred this arg1
        checkArgPred this arg2
        checkFreeVar arg1
        checkFreeVar arg2
        checkCleanedUpFormula this

    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        addExpressionToParentArgList this


[<AbstractClass>]
type FplGenericDelegate(name, positions: Positions, parent: FplValue) as this =
    inherit FplValue(positions, Some parent)

    do 
        this.FplId <- name

    override this.EmbedInSymbolTable _ = addExpressionToReference this

    override this.RunOrder = None


/// Implements the semantics of an FPL equality.
type FplEquality(name, positions: Positions, parent: FplValue) as this =
    inherit FplGenericDelegate(name, positions, parent)

    do 
        this.FplId <- $"{LiteralDel}{PrimDelegateEqual}"
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
        this.Debug Debug.Start
        if this.ArgList.Count <> 2 then 
            this.ErrorOccurred <- emitID013Diagnostics $"Predicate `=` takes 2 arguments, got {this.ArgList.Count}." variableStack.CallerStartPos variableStack.CallerEndPos 
        else

            let a = this.ArgList[0]
            let b = this.ArgList[1]
            let aType = a.Type SignatureType.Type
            let bType = b.Type SignatureType.Type
            let aRepr = a.Represent()
            let bRepr = b.Represent()

            let newValue = new FplIntrinsicPred((variableStack.CallerStartPos, variableStack.CallerEndPos), this.Parent.Value)
            match aRepr with
            | LiteralUndef -> 
                this.ErrorOccurred <- emitID013Diagnostics "Predicate `=` cannot be evaluated because the left argument is undefined." variableStack.CallerStartPos variableStack.CallerEndPos 
                this.SetValue(newValue)
            | _ -> 
                match bRepr with
                | LiteralUndef -> 
                    this.ErrorOccurred <- emitID013Diagnostics "Predicate `=` cannot be evaluated because the right argument is undefined." variableStack.CallerStartPos variableStack.CallerEndPos 
                    this.SetValue(newValue)
                | _ when aRepr = "dec tpl" && bRepr = "dec tpl" -> 
                    this.SetValue(newValue) // undetermined
                | _ when aType<>bType -> 
                    newValue.FplId <- LiteralFalse // if the compared arguments have different types, then unequal
                    this.SetValue(newValue)
                | _ -> 
                    match aRepr with
                    | "dec pred"  
                    | PrimUndetermined -> 
                        this.ErrorOccurred <- emitID013Diagnostics "Predicate `=` cannot be evaluated because the left argument is undetermined." variableStack.CallerStartPos variableStack.CallerEndPos 
                        this.SetValue(newValue)
                    | _ -> 
                        match bRepr with
                        | "dec pred"  
                        | PrimUndetermined -> 
                            this.ErrorOccurred <- emitID013Diagnostics "Predicate `=` cannot be evaluated because the right argument is undetermined." variableStack.CallerStartPos variableStack.CallerEndPos 
                            this.SetValue(newValue)
                        | _ -> 
                            let a1IsDeclared = aRepr.Contains("dec ")
                            let b1IsDeclared = bRepr.Contains("dec ")
                            newValue.FplId <- 
                                match a1IsDeclared, b1IsDeclared with
                                | false, false 
                                | true, true ->
                                    $"{(aRepr = bRepr)}".ToLower()
                                | _ -> PrimUndetermined
                            this.SetValue(newValue)
        this.Debug Debug.Stop

/// Implements the semantics of an FPL decrement delegate.
type FplDecrement(name, positions: Positions, parent: FplValue) as this =
    inherit FplGenericDelegate(name, positions, parent)

    do 
        this.TypeId <- LiteralObj

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

    override this.Run _ = 
        this.Debug Debug.Start
        if this.ArgList.Count <> 1 then 
            this.ErrorOccurred <- emitID013Diagnostics $"Decrement takes 1 arguments, got {this.ArgList.Count}." this.StartPos this.EndPos
            let newValue = FplIntrinsicUndef((this.StartPos, this.EndPos), this)
            this.SetValue newValue
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
                    LiteralUndef
                else
                    string n'
            this.SetValue(newValue)
        this.Debug Debug.Stop

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
                    
    override this.Represent() = // done
        match this.FplId with
        | LiteralInd -> $"dec {this.TypeId}"
        | _ -> this.FplId

    override this.Run _ = 
        this.Debug Debug.Start
        // no Run needed for FplIntrinsicInd
        this.Debug Debug.Stop

    override this.EmbedInSymbolTable _ = addExpressionToReference this

    override this.RunOrder = None

let runIntrinsicFunction (fv:FplValue) variableStack =
    
    let mapOpt = getMapping fv
    match mapOpt with
    | Some (:? FplMapping as map) ->
        match map.RefersTo with 
        | Some cl when map.Dimensionality = 0 ->
            // a class type without an array
            // TODO filter by constructors
            let defaultCtor = cl.Scope.Values |> Seq.head :?> FplGenericConstructor
            defaultCtor.Run variableStack
            match defaultCtor.Instance with 
            | Some instance ->
                fv.SetValue instance // set value to the created instance 
                // reposition the instance in symbol table
                instance.Parent <- Some fv
            | None -> () // TODO, should not occur issue diagnostics?
        | Some cl when map.Dimensionality > 0 ->
            fv.SetValue map // set value to the map
        | None when map.Dimensionality > 0 ->
            fv.SetValue map // set value to the map
        | _ ->
            fv.SetValue map // set value to the map
    | _ ->
        let undef = new FplIntrinsicUndef((fv.StartPos, fv.EndPos), fv)
        fv.SetValue undef

let private getFunctionalTermRepresent (fv:FplValue) =
    match fv.Value with 
    | None ->
        // since the function term has no value, it has no return statement
        // And the FPL syntax ensures that this can only be the case
        // if the Functional Term is intrinsic.
        // In this case, the "representation" of the function is
        // its declared mapping type
        let mapping = fv.ArgList[0]
        $"dec {mapping.Type(SignatureType.Mixed)}"
    | Some ref -> ref.Represent()    

type FplFunctionalTerm(positions: Positions, parent: FplValue, runOrder) as this =
    inherit FplGenericInheriting(positions, parent)
    let mutable _signStartPos = Position("", 0L, 0L, 0L)
    let mutable _signEndPos = Position("", 0L, 0L, 0L)
    let _runOrder = runOrder
    let mutable _isReady = false
    let mutable _callCounter = 0
    let mutable _skolemName = ""

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

    member this.SkolemName = _skolemName
    member this.SetSkolemName() = _skolemName <- signatureRepresent this

    interface ISkolem with
        member this.SkolemName = this.SkolemName 
        member this.SetSkolemName() = this.SetSkolemName() 

    override this.Name = PrimFunctionalTermL
    override this.ShortName = PrimFunctionalTerm

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
        base.CheckConsistency()
        if not this.IsIntrinsic then // if not intrinsic, check variable usage
            checkVAR04Diagnostics this
        match this.ExpressionType with
        | FixType.Infix _ when this.Arity <> 2 -> this.ErrorOccurred <- emitSIG00Diagnostics this.ExpressionType.Type 2 this.Arity this.SignStartPos this.SignEndPos
        | FixType.Prefix _ when this.Arity <> 1 -> this.ErrorOccurred <- emitSIG00Diagnostics this.ExpressionType.Type 1 this.Arity this.SignStartPos this.SignEndPos
        | FixType.Postfix _ when this.Arity <> 1 -> this.ErrorOccurred <- emitSIG00Diagnostics this.ExpressionType.Type 1 this.Arity this.SignStartPos this.SignEndPos
        | _ -> ()
        match this.ExpressionType with
        | FixType.Infix (symbol, precedence) -> checkSIG02Diagnostics (getRoot this) symbol precedence this.SignStartPos this.SignEndPos
        | _ -> ()

    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        tryAddToParentUsingMixedSignature this

    override this.RunOrder = Some _runOrder

    override this.Represent() = // done
        if _callCounter > maxRecursion then
            this.ErrorOccurred <- emitLG002diagnostic (this.Type(SignatureType.Name)) _callCounter this.StartPos this.EndPos
            LiteralUndef
        else
            _callCounter <- _callCounter + 1
            let result = 
                if this.IsIntrinsic then
                    this.SkolemName
                else
                    getFunctionalTermRepresent this
            _callCounter <- _callCounter - 1
            result

    override this.Run variableStack = 
        this.Debug Debug.Start
        if not _isReady then
            _callCounter <- _callCounter + 1
            if _callCounter > maxRecursion then
                this.ErrorOccurred <- emitLG002diagnostic (this.Type(SignatureType.Name)) _callCounter variableStack.CallerStartPos variableStack.CallerEndPos
            else
                if this.IsIntrinsic then 
                    this.SetSkolemName()
                    runIntrinsicFunction this variableStack
                else
                    this.ArgList
                    |> Seq.iter (fun fv -> 
                        fv.Run variableStack
                        this.SetValuesOf fv
                    )

                this.GetProperties()
                |> List.iter (fun fv -> fv.Run variableStack)
            _callCounter <- _callCounter - 1
            _isReady <- this.Arity = 0 
        this.Debug Debug.Stop

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
        this.Debug Debug.Start
        let operand = this.ArgList[0]
        let typeOfOperand = this.ArgList[1]
        let newValue = new FplIntrinsicPred((this.StartPos, this.EndPos), this)
        let evaluateIsOperator =
            // FPL truth-table
            match operand with 
            | :? FplReference as op ->
                match mpwa [operand] [typeOfOperand] MatchingMode.Signature with
                | Some errMsg -> LiteralFalse
                | None -> LiteralTrue
            | _ -> LiteralFalse
        
        newValue.FplId <- evaluateIsOperator
        this.SetValue(newValue)  
        this.Debug Debug.Stop

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
            var.ErrorOccurred <- emitVAR05diagnostics var.FplId var.StartPos var.EndPos
        )
        checkArgPred this (this.ArgList[0])
        checkCleanedUpFormula this

    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        // set all the variables of this quantor to bound ones
        this.GetVariables()
        |> List.map (fun var -> var :?> FplGenericVariable)
        |> List.iter (fun var -> var.SetIsBound())
        addExpressionToParentArgList this
    
    override this.Run variableStack = 
        this.Debug Debug.Start
        this.ArgList[0].Run variableStack
        let pred = new FplIntrinsicPred((this.StartPos, this.EndPos), this)
        this.SetValue pred
        this.Debug Debug.Stop


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
    let mutable _isReady = false
    let mutable _callCounter = 0
    let mutable _skolemName = ""

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

    interface IReady with
        member _.IsReady = _isReady

    member this.SkolemName = _skolemName
    member this.SetSkolemName() = _skolemName <- signatureRepresent this

    interface ISkolem with
        member this.SkolemName = this.SkolemName 
        member this.SetSkolemName() = this.SetSkolemName() 

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

    override this.Represent() = // done
        if _callCounter > maxRecursion then
            this.ErrorOccurred <- emitLG002diagnostic (this.Type(SignatureType.Name)) _callCounter this.StartPos this.EndPos
            LiteralUndef
        else
            _callCounter <- _callCounter + 1
            let result = 
                if this.IsIntrinsic then
                    this.SkolemName
                else
                    getFunctionalTermRepresent this
            _callCounter <- _callCounter - 1
            result

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
        this.Debug Debug.Start
        if not _isReady then
            _callCounter <- _callCounter + 1
            if _callCounter > maxRecursion then
                this.ErrorOccurred <- emitLG002diagnostic (this.Type(SignatureType.Name)) _callCounter variableStack.CallerStartPos variableStack.CallerEndPos
            else
                if this.IsIntrinsic then 
                    this.SetSkolemName()
                    runIntrinsicFunction this variableStack
                else
                    this.ArgList
                    |> Seq.iter (fun fv -> 
                        fv.Run variableStack
                        this.SetValuesOf fv
                    )
            _callCounter <- _callCounter - 1
            _isReady <- this.Arity = 0 
        this.Debug Debug.Stop

let isExtension (fv:FplValue) =
    match fv with
    | :? FplExtension -> true
    | _ -> false


/// Implements the return statement in FPL.
type FplReturn(positions: Positions, parent: FplValue) as this =
    inherit FplValue(positions, Some parent)

    do
        this.FplId <- LiteralRet
        this.TypeId <- LiteralUndef

    override this.Name = PrimReturn
    override this.ShortName = PrimStmt

    override this.Clone () =
        let ret = new FplReturn((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = this.FplId

    override this.EmbedInSymbolTable _ = addExpressionToParentArgList this

    override this.RunOrder = None

    override this.Run variableStack =
        this.Debug Debug.Start
        let returnedReference = this.ArgList[0]
        let blockOpt = this.NextBlockNode
        match blockOpt with 
        | Some funTerm ->
            let mapTypeOpt = getMapping funTerm
            match mapTypeOpt with 
            | Some mapType ->
                match mpwa [ returnedReference ] [ mapType ] MatchingMode.Assignment with
                | Some errMsg -> returnedReference.ErrorOccurred <- emitSIG03Diagnostics errMsg (mapType.Type(SignatureType.Type)) (returnedReference.StartPos) (returnedReference.EndPos)
                | _ -> 
                    match returnedReference with
                    | :? FplIntrinsicPred 
                    | :? FplIntrinsicTpl 
                    | :? FplIntrinsicInd 
                    | :? FplIntrinsicUndef ->
                        this.SetValue returnedReference
                    | :? FplReference when returnedReference.RefersTo.IsSome ->
                        let refValue = returnedReference.RefersTo.Value
                        refValue.Run variableStack
                        match refValue with 
                        | :? FplGenericConstructor ->
                            this.SetValuesOf refValue
                        | :? FplVariable as var when var.Value.IsSome ->
                            this.SetValuesOf refValue
                        | _ -> this.SetValue refValue
                    | _ ->
                        let value = new FplIntrinsicUndef((this.StartPos, this.EndPos), this)
                        this.SetValue value
            | _ -> 
                // should syntactically not occur that a functional term has no mapping
                // in this case return undef
                let value = new FplIntrinsicUndef((this.StartPos, this.EndPos), this)
                this.SetValue value
        | _ -> 
            // should syntactically not occur that a return statement occurs in something else
            // then a functional term
            // in this case return undef
            let value = new FplIntrinsicUndef((this.StartPos, this.EndPos), this)
            this.SetValue value
        this.Debug Debug.Stop

type FplMapCaseSingle(positions: Positions, parent: FplValue) as this =
    inherit FplValue(positions, Some parent)
    do 
        this.FplId <- PrimMapCaseSingle

    override this.Name = PrimMapCaseSingleL
    override this.ShortName = PrimStmt

    override this.Clone () =
        let ret = new FplMapCaseSingle((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = 
        getFplHead this signatureType

    member this.GetCondition() = this.ArgList[0]
    member this.GetResult() = this.ArgList[1]

    override this.CheckConsistency() = 
        base.CheckConsistency()
        checkArgPred this (this.GetCondition())

    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        addExpressionToParentArgList this

    override this.RunOrder = None

    override this.Run variableStack = 
        this.Debug Debug.Start
        let result = this.GetResult()
        result.Run variableStack
        this.SetValuesOf result
        this.Debug Debug.Stop

type FplMapCaseElse(positions: Positions, parent: FplValue) as this =
    inherit FplValue(positions, Some parent)
    do 
        this.FplId <- PrimMapCaseElse

    override this.Name = PrimMapCaseElseL
    override this.ShortName = PrimStmt

    override this.Clone () =
        let ret = new FplMapCaseElse((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = 
        getFplHead this signatureType

    override this.EmbedInSymbolTable _ = addExpressionToParentArgList this

    override this.RunOrder = None

    override this.Run variableStack = 
        this.Debug Debug.Start
        let contentOfElsResult = this.ArgList |> Seq.head
        contentOfElsResult.Run variableStack
        this.SetValuesOf contentOfElsResult
        this.Debug Debug.Stop

type FplMapCases(positions: Positions, parent: FplValue) as this =
    inherit FplValue(positions, Some parent)
    let _consistentCaseType = new FplIntrinsicTpl("", positions, parent)
    let _reachableCases = new HashSet<string>()

    do 
        this.FplId <- LiteralMapCases

    override this.Name = PrimMapCasesL
    override this.ShortName = PrimStmt

    override this.Clone () =
        let ret = new FplMapCases((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = 
        getFplHead this signatureType

    member this.GetConditionResultList() = 
        this.ArgList
        |> Seq.choose (fun item ->
            match item with
            | :? FplMapCaseSingle as condRes -> Some condRes
            | _ -> None)
        |> Seq.toList

    member this.GetMapElse() = 
        this.ArgList |> Seq.last

    member private this.CheckAllResultsForEqualType() =
        // check if all results have the same type
        this.GetConditionResultList()
        |> Seq.map (fun conditionResultPair -> conditionResultPair.GetResult())
        |> Seq.iter (fun result -> _consistentCaseType.TrySetTemplateUsage result (SIG13("", "", "", "").Code))
        // check also else result
        _consistentCaseType.TrySetTemplateUsage (this.GetMapElse()) (SIG13("", "", "", "").Code)
        match _consistentCaseType.ErrorOccurred with
        | Some errMsg -> 
            // Since there were proceeding errors regarding inconsistent Type Ids of some branches
            // set the TypeId of this FplMapCases to undefined
            this.TypeId <- LiteralUndef  
         | _ ->
            // Set the TypeId of this FplMapCases to the consistent TypeId found for all of its branches
            let typeOfAllBranches = _consistentCaseType.RefersTo.Value
            this.TypeId <- typeOfAllBranches.TypeId


    member private this.CheckAllCasesForBeingReachable() =
        _reachableCases.Clear()
        this.GetConditionResultList()
        |> Seq.map (fun conditionResultPair -> conditionResultPair.GetCondition())
        |> Seq.iter (fun condition -> 
            let conditionSignature = condition.Type SignatureType.Name
            if _reachableCases.Add(conditionSignature) then 
                () // signature added
            else
                // signature was already added
                this.ErrorOccurred <- emitSIG14diagnostics condition.StartPos condition.EndPos
                
        )

    override this.CheckConsistency() = 
        base.CheckConsistency()
        this.CheckAllResultsForEqualType()
        this.CheckAllCasesForBeingReachable()


    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        addExpressionToParentArgList this

    override this.RunOrder = None

    override this.Run variableStack = 
        this.Debug Debug.Start
        let resultLst = this.GetConditionResultList()
        let mapElse = this.GetMapElse()
        let firstMapCaseWithTrueConditionOpt = 
            resultLst
            |> Seq.tryFind(fun mapCaseSingle -> 
                let condition = mapCaseSingle.GetCondition()
                condition.Run variableStack
                condition.Represent() = LiteralTrue
            )
        match firstMapCaseWithTrueConditionOpt with
        | Some firstMapCaseWithTrueCondition -> 
            firstMapCaseWithTrueCondition.Run variableStack
            let resOfFound = firstMapCaseWithTrueCondition.GetResult()
            this.SetValuesOf resOfFound
        | None -> 
            mapElse.Run variableStack
            this.SetValuesOf mapElse
        this.Debug Debug.Stop

type FplCaseSingle(positions: Positions, parent: FplValue) as this =
    inherit FplGenericStmt(positions, parent)
    do 
        this.FplId <- PrimCaseSingle

    override this.Name = PrimCaseSingleL

    override this.Clone () =
        let ret = new FplCaseSingle((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = 
        getFplHead this signatureType

    member this.GetCondition() = this.ArgList[0]
    member this.StmtsAfterCondition() = this.ArgList |> Seq.tail

    override this.CheckConsistency() = 
        base.CheckConsistency()
        checkArgPred this (this.GetCondition())

    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        addExpressionToParentArgList this

    override this.Run variableStack = 
        this.Debug Debug.Start
        this.StmtsAfterCondition()
        |> Seq.iter (fun stmt -> stmt.Run variableStack)
        this.Debug Debug.Stop

type FplCaseElse(positions: Positions, parent: FplValue) as this =
    inherit FplGenericStmt(positions, parent)
    do 
        this.FplId <- PrimCaseElse

    override this.Name = PrimCaseElseL

    override this.Clone () =
        let ret = new FplCaseElse((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = 
        getFplHead this signatureType

    override this.Run variableStack = 
        this.Debug Debug.Start
        this.ArgList 
        |> Seq.iter (fun stmt -> stmt.Run variableStack)
        this.Debug Debug.Stop

type FplCases(positions: Positions, parent: FplValue) as this =
    inherit FplGenericStmt(positions, parent)
    let _reachableCases = new HashSet<string>()
    do 
        this.FplId <- LiteralCases

    override this.Name = PrimCasesL

    override this.Clone () =
        let ret = new FplCases((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = 
        getFplHead this signatureType

    member this.GetConditionResultList() = 
        this.ArgList
        |> Seq.choose (fun item ->
            match item with
            | :? FplCaseSingle as condRes -> Some condRes
            | _ -> None)
        |> Seq.toList

    member this.GetElseStmt() = this.ArgList |> Seq.last

    member private this.CheckAllCasesForBeingReachable() =
        _reachableCases.Clear()
        this.GetConditionResultList()
        |> Seq.map (fun conditionResultPair -> conditionResultPair.GetCondition())
        |> Seq.iter (fun condition -> 
            let conditionSignature = condition.Type SignatureType.Name
            if _reachableCases.Add(conditionSignature) then 
                () // signature added
            else
                // signature was already added
                this.ErrorOccurred <- emitSIG14diagnostics condition.StartPos condition.EndPos
                
        )

    override this.CheckConsistency() = 
        base.CheckConsistency()
        this.CheckAllCasesForBeingReachable()

    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        addExpressionToParentArgList this

    override this.Run variableStack = 
        this.Debug Debug.Start
        let resultLst = this.GetConditionResultList()
        let elseStmt = this.GetElseStmt()
        let firstCaseWithTrueConditionOpt = 
            resultLst
            |> Seq.tryFind(fun caseSingle -> 
                let condition = caseSingle.GetCondition()
                condition.Run variableStack
                condition.Represent() = LiteralTrue
            )
        match firstCaseWithTrueConditionOpt with
        | Some firstCaseWithTrueCondition -> 
            firstCaseWithTrueCondition.Run variableStack
        | None -> 
            elseStmt.Run variableStack
        this.Debug Debug.Stop

type FplForEnumeratorType = 
    | ArrayElements
    | Predicative
    | Error

type FplForInStmt(positions: Positions, parent: FplValue) as this =
    inherit FplGenericStmt(positions, parent)
    do 
        this.FplId <- LiteralFor

    override this.Name = PrimForInStmtL

    override this.Clone () =
        let ret = new FplForInStmt((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = 
        getFplHead this signatureType

    member this.Entity =
        if this.ArgList.Count > 0 then 
            this.ArgList[0].RefersTo
        else 
            None

    member this.Domain =
        if this.ArgList.Count > 1 then 
            this.ArgList[1].RefersTo 
        else 
            None

    member this.Body =
        // the body of the for statement starts after the entity and after the domain
        if this.ArgList.Count > 2 then 
            this.ArgList |> Seq.tail |> Seq.tail |> Seq.toList
        else
            []

    member this.GetEnumerator() =
        match this.Domain with
        | Some (:? FplVariableArray as domain) ->
            (FplForEnumeratorType.ArrayElements, domain.ValueList |> Seq.toList)
        | Some domain ->
            this.ErrorOccurred <- emitST005diagnostics (domain.Type SignatureType.Name) domain.Name this.ArgList[1].StartPos this.ArgList[1].EndPos
            (FplForEnumeratorType.Error, [])
        | _ ->
            this.ErrorOccurred <- emitST005diagnostics "missing" PrimNone this.StartPos this.StartPos
            (FplForEnumeratorType.Error, [])
            
    override this.Run variableStack = 
        this.Debug Debug.Start
        match this.Entity, this.GetEnumerator() with
        | Some entity, (FplForEnumeratorType.ArrayElements, lst) ->
            lst
            |> List.iter (fun lstElement ->
                // TODO: check type compatibility of entity accepting lstElement
                entity.Value <- Some lstElement
                this.Body
                |> List.iter (fun stmt ->
                    stmt.Run variableStack
                )
            )
        | _, _ -> ()
        this.Debug Debug.Stop

type FplForInStmtEntity(positions: Positions, parent: FplValue) as this =
    inherit FplGenericStmt(positions, parent)
    do 
        this.FplId <- PrimForInStmtEntity

    override this.Name = PrimForInStmtEntityL

    override this.Clone () =
        let ret = new FplForInStmtEntity((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = 
        let entityOpt = referencedNodeOpt this
        match entityOpt with 
        | Some entity -> entity.Type signatureType
        | _ -> getFplHead this signatureType

    override this.EmbedInSymbolTable _ = tryAddToParentForInStmt this

    override this.Run variableStack = 
        // TODO implement run
        this.Debug Debug.Start
        this.Debug Debug.Stop

type FplForInStmtDomain(positions: Positions, parent: FplValue) as this =
    inherit FplGenericStmt(positions, parent)
    do 
        this.FplId <- PrimForInStmtDomain

    override this.Name = PrimForInStmtDomainL

    override this.Clone () =
        let ret = new FplForInStmtDomain((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = 
        let domainOpt = referencedNodeOpt this
        match domainOpt with 
        | Some domain -> domain.Type signatureType
        | _ -> getFplHead this signatureType
    override this.EmbedInSymbolTable _ = tryAddToParentForInStmt this

    override this.Run variableStack = 
        // TODO implement run
        this.Debug Debug.Start
        this.Debug Debug.Stop

/// Implements the assignment statement in FPL.
type FplAssignment(positions: Positions, parent: FplValue) as this =
    inherit FplGenericStmt(positions, parent)

    do
        this.FplId <- PrimAssignment
        this.TypeId <- LiteralUndef

    override this.Name = PrimAssignmentL

    override this.Clone () =
        let ret = new FplAssignment((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = 
        getFplHead this signatureType

    override this.CheckConsistency () = 
        base.CheckConsistency()
        let checkTypes (assignee:FplValue) (assignedValue:FplValue) =
            let nameAssignee = assignee.Type SignatureType.Name
            let nameAssignedValue = assignedValue.Type SignatureType.Name
            if nameAssignee = nameAssignedValue then
                this.ErrorOccurred <- emitLG005Diagnostics nameAssignedValue assignedValue.StartPos assignedValue.EndPos
            else
                // assignee is to be treated as parameter, the assignedValue as argument
                match mpwa [assignedValue] [assignee] MatchingMode.Assignment with
                | Some errMsg ->
                    this.ErrorOccurred <- emitSIG05Diagnostics errMsg this.ArgList[1].StartPos this.ArgList[1].EndPos
                | _ -> ()
                
        let checkErrorOccuredInReference (fv:FplValue) = 
            match fv with
            | :? FplReference as ref -> 
                this.ErrorOccurred <- ref.ErrorOccurred 
            | _ -> ()
        // remember proceeding errors of references used in the assignment (if any)
        checkErrorOccuredInReference this.ArgList[0]
        checkErrorOccuredInReference this.ArgList[1]
        match this.Assignee, this.AssignedValue with
        | Some (:? FplVariable as assignee), Some (assignedValue:FplValue) when assignedValue.Name = PrimClassL ->
            assignee.IsInitialized <- true
            checkTypes assignee assignedValue
        | Some (:? FplVariable as assignee), Some (assignedValue:FplValue) when (assignedValue.Name = PrimFunctionalTermL || assignedValue.Name = PrimMandatoryFunctionalTermL) && isCallByValue this.ArgList[1] ->
            let mapOpt = getMapping assignedValue
            match mapOpt with 
            | Some map -> checkTypes this.ArgList[0] map
            | _ -> checkTypes assignee assignedValue
        | Some (:? FplVariable as assignee), Some (assignedValue:FplValue) -> 
            checkTypes assignee this.ArgList[1] 
        | Some (:? FplVariableArray as assignee), Some assignedValue ->
           checkTypes assignee assignedValue 
        | Some (:? FplSelf as assignee), _ ->
            match assignee.RefersTo with 
            | Some ref -> 
                this.ErrorOccurred <- emitSIG07iagnostic (assignee.Type SignatureType.Name) (getEnglishName ref.Name false) assignee.Name (this.ArgList[0].StartPos) (this.ArgList[0].EndPos)
            | None ->
                this.ErrorOccurred <- emitSIG07iagnostic (assignee.Type SignatureType.Name) "the type of self could not be determined" assignee.Name (this.ArgList[0].StartPos) (this.ArgList[0].EndPos)
        | Some (:? FplParent as assignee), _ ->
            match assignee.RefersTo with 
            | Some ref -> 
                this.ErrorOccurred <- emitSIG07iagnostic (assignee.Type SignatureType.Name) (getEnglishName ref.Name false) assignee.Name (this.ArgList[0].StartPos) (this.ArgList[0].EndPos)
            | None ->
                this.ErrorOccurred <- emitSIG07iagnostic (assignee.Type SignatureType.Name) "the type of parent could not be determined" assignee.Name (this.ArgList[0].StartPos) (this.ArgList[0].EndPos)
        | Some (assignee), Some assignedValue ->
            let nameAssignee = assignee.Type SignatureType.Name
            let nameAssignedValue = assignedValue.Type SignatureType.Name
            if nameAssignee = nameAssignedValue then
                // something has been assigned to itself
                this.ErrorOccurred <- emitLG005Diagnostics nameAssignedValue assignedValue.StartPos assignedValue.EndPos
            else
                this.ErrorOccurred <- emitSIG07iagnostic (assignee.Type SignatureType.Name) $"type `{assignee.Type SignatureType.Type}`" assignee.Name (this.ArgList[0].StartPos) (this.ArgList[0].EndPos)
        | _ -> ()

    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        addExpressionToParentArgList this

    member private this.GetAssignmentArg no =
        if this.ArgList.Count > 1 then 
            let candidate = this.ArgList[no]
            match candidate with 
            | :? FplReference as ref ->
                match ref.DottedChild with 
                | Some dc -> dc.RefersTo
                | None when ref.RefersTo.IsSome -> ref.RefersTo
                | _ -> Some candidate
            | _ ->
                Some candidate
        else
            None

    member this.Assignee:FplValue option = this.GetAssignmentArg 0

    member this.AssignedValue = this.GetAssignmentArg 1

    override this.Run variableStack =
        this.Debug Debug.Start
        match this.ErrorOccurred, this.Assignee, this.AssignedValue with
        | Some errCode, Some (:? FplGenericVariable as assignee), _ ->
            emitST003diagnostics errCode this.ArgList[1].StartPos this.ArgList[1].EndPos
        | Some errCode, Some assignee, _ ->
            emitST003diagnostics errCode this.ArgList[1].StartPos this.ArgList[1].EndPos
        | None, Some (:? FplVariable as assignee), Some (:? FplGenericConstructor as assignedValue) ->
            assignedValue.Run variableStack
            match assignedValue.Instance with 
            | Some instance ->
                assignee.SetValue instance // set value to the created instance 
                // reposition the instance in symbol table
                instance.Parent <- Some assignee
            | None -> () // TODO, issue diagnostics?
        | None, Some (:? FplVariable as assignee), Some (:? FplIntrinsicInd as assignedValue) ->
            assignee.SetValue assignedValue
        | None, Some (:? FplVariable as assignee), Some (:? FplIntrinsicPred as assignedValue) ->
            assignee.SetValue assignedValue
        | None, Some (:? FplVariableArray as assignee), Some (:? FplGenericConstructor as assignedValue) ->
            assignedValue.Run variableStack
            match assignedValue.Instance with 
            | Some instance ->
                assignee.AssignValueToCoordinates this.ArgList[0].ArgList instance // set value to the created instance 
                // reposition the instance in symbol table
                instance.Parent <- Some assignee
            | None -> () // TODO, issue diagnostics?
        | None, Some (:? FplVariableArray as assignee), Some assignedValue ->
            assignee.AssignValueToCoordinates this.ArgList[0].ArgList assignedValue // set value to the created instance 
        | None, Some assignee, Some assignedValue ->
            assignedValue.Run variableStack
            assignee.SetValuesOf assignedValue
        | _ -> ()
        this.Debug Debug.Stop

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
    let nameOfOther (fv:FplValue) = $"'{fv.Type(SignatureType.Name)} which is {getEnglishName fv.Name false}'"
    match candidates.Length with
    | 1 ->  // exactly one candidate found
        let potentialCandidate = candidates.Head
        match fvJi, potentialCandidate with
        | :? FplJustificationItemByProofArgument, :? FplProof
        | :? FplJustificationItemByDef, :? FplClass
        | :? FplJustificationItemByDef, :? FplPredicate
        | :? FplJustificationItemByDef, :? FplFunctionalTerm
        | :? FplJustificationItemByDef, :? FplGenericVariable
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

    /// Returns the string representation of all asts .
    member this.AstsToString =
        let res =
            _parsedAsts
            |> Seq.map (fun pa -> pa.Parsing.Ast.ToString())
            |> String.concat Environment.NewLine

        res

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
                match root.Value with
                | Some ref -> createJson ref sb (level + 1) true false
                | None -> ()
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

    if isUpper name then 
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
            | :? FplPredicate 
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
        | :? FplReference as ref when ref.DottedChild.IsSome -> 
            ScopeSearchResult.Found(ref.DottedChild.Value)
        | :? FplReference -> 
            match fv1.Parent with
            | Some parent -> findQualifiedEntity parent
            | None -> ScopeSearchResult.NotFound
        | _ -> ScopeSearchResult.NotFound

    match findQualifiedEntity fv with
    | ScopeSearchResult.Found candidate ->
        match candidate with
        | :? FplVariable as var ->
            // prefer variable value over its referred type node
            Option.orElse var.Value var.RefersTo |> Option.toList
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

