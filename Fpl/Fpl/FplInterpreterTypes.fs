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
open ErrDiagnostics

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

    /// Creates an EvalAliasedNamespaceIdentifier with a string, alias or star, and positions.
    static member CreateEani (pascalCaseId:string, aliasOrStar: string, startPos, endPos) =
        let evalAlias = {
                EvalAlias.StartPos = startPos
                EvalAlias.EndPos = endPos
                EvalAlias.AliasOrStar = aliasOrStar
            }

        { EvalAliasedNamespaceIdentifier.StartPos = startPos
          EvalAliasedNamespaceIdentifier.EndPos = endPos
          EvalAliasedNamespaceIdentifier.EvalAlias = evalAlias
          EvalAliasedNamespaceIdentifier.PascalCaseIdList = [ pascalCaseId ] }

    /// Creates an EvalAliasedNamespaceIdentifier with a string list and a given EvalAlias and positions.
    static member CreateEani (pascalCaseIdList:string list, evalAlias:EvalAlias, startPos, endPos) = 
        { EvalAliasedNamespaceIdentifier.StartPos = startPos
          EvalAliasedNamespaceIdentifier.EndPos = endPos
          EvalAliasedNamespaceIdentifier.EvalAlias = evalAlias
          EvalAliasedNamespaceIdentifier.PascalCaseIdList = pascalCaseIdList }

    /// Creates an EvalAliasedNamespaceIdentifier with a given Uri.
    static member CreateEani(uri:PathEquivalentUri) = 
        let pascalCaseId = uri.TheoryName
        let pos = Position("", 0, 1, 1)
        EvalAliasedNamespaceIdentifier.CreateEani(pascalCaseId, "*", pos, pos)

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
        {
            SortingProperties.TopologicalSorting = 0
            SortingProperties.ReferencingAsts = []
            SortingProperties.ReferencedAsts = []
            SortingProperties.EANIList = [] 
        }

let computeMD5Checksum (input: string) =
    let md5 = MD5.Create()
    let inputBytes = Encoding.ASCII.GetBytes(input)
    let hash = md5.ComputeHash(inputBytes)
    hash |> Array.map (fun b -> b.ToString("x2")) |> String.concat ""


/// A type that encapsulates the sources found for a uses clause
/// and provides members to filter those from the file system and those from
/// the web.
type FplSources(paths: PathEquivalentUri list, pathToLocalRegistry:string) =
    let _pathToLocalRegistry = pathToLocalRegistry
    /// All found paths for a uses clause, including those from the web.
    member this.Paths = paths

    /// Path to local copies of the registry.
    member this.PathToLocalRegistry = pathToLocalRegistry

    /// Returns all loaded FPL theories loaded by a uses clause grouped by name with lists of potential locations
    member this.Grouped 
        with get () = 
            let fplTheories = 
                this.Paths
                |> List.map (fun fp -> (Path.GetFileName fp.AbsolutePath, fp))
            fplTheories
            |> List.groupBy fst
    
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



    member this.GroupedWithPreferedSource 
        with get () = 
            let result = 
                let grouped = this.Grouped
                grouped
                |> List.collect (fun (fileName, paths) -> 
                    let pathType =
                        paths
                        |> List.map snd
                        |> List.tryFind (fun path -> 
                            if FplSources.IsFilePath(path) && not (path.AbsolutePath.Contains("/lib/") || path.AbsolutePath.Contains(@"\lib\")) then
                                true // the first source is the current directory
                            elif FplSources.IsFilePath(path) && (path.AbsolutePath.Contains("/lib/") || path.AbsolutePath.Contains(@"\lib\")) then 
                                true // the second is the lib subdirectory 
                            else
                                true // the third is the internet source
                        )

                    let pathTypes =
                        List.map snd paths
                        |> List.map (fun path -> 
                            if FplSources.IsFilePath(path) && (path.AbsolutePath.Contains("/lib/") || path.AbsolutePath.Contains(@"\lib\")) then
                                "./lib"
                            elif  FplSources.IsFilePath(path) then 
                                "./"
                            else
                                "https"
                        )                    
                    let theoryName = 
                        Path.GetFileNameWithoutExtension(fileName)
                    match pathType with
                    | Some path -> 
                        let chosenPathType = 
                            if FplSources.IsFilePath(path) && not (path.AbsolutePath.Contains("/lib/") || path.AbsolutePath.Contains(@"\lib\")) then
                                "./"
                            elif FplSources.IsFilePath(path) && (path.AbsolutePath.Contains("/lib/") || path.AbsolutePath.Contains(@"\lib\")) then 
                                "./lib"
                            else
                                "https"
                        [(fileName, path, chosenPathType, pathTypes, theoryName)]
                    | None -> []          
                )
            result

    /// Checks if a filename has a pattern.
    static member HasPattern(fileName:string, pattern) = 
        let wildcardToRegex (wildcard : string) =
            "^" + Regex.Escape(wildcard).Replace("\\*", ".*").Replace("\\?", ".") + "$"
        let regexPattern = wildcardToRegex pattern
        let regex = Regex(regexPattern, RegexOptions.IgnoreCase)
        regex.IsMatch(fileName)
    
    /// Finds all filenames in sources with a given pattern.
    member this.FindWithPattern(pattern:string) = 
        this.GroupedWithPreferedSource
        |> List.filter (fun (fileName, _, _, _, _) ->
            FplSources.HasPattern(fileName, pattern)
        )

type ParsingProperties =
    { mutable Uri: PathEquivalentUri // source of the ast
      mutable FplSourceCode: string // source code of the ast
      mutable Ast: Ast // parsed ast
      mutable Checksum: string } // checksum of the parsed ast

    /// Reset this ParsingProperties to its new location
    member this.Reset (fplCode: string) (uri: PathEquivalentUri) =
        let checksum = computeMD5Checksum fplCode

        if this.Checksum <> checksum then
            // if there ist a Parsed Ast with the same Name as the eani.Name
            // and its checksum differs from the previous checksum
            // then replace the ast, checksum, location, sourcecode, the
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
        {
            ParsingProperties.Uri = uri
            ParsingProperties.FplSourceCode = fplCode
            ParsingProperties.Ast = FplParser.fplParser fplCode
            ParsingProperties.Checksum = computeMD5Checksum fplCode
        }

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
    member this.TryFindAstById(identifier:string) = 
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

    /// Returns a dictionof <PathEquivalentUri,SourceCode> of all ParsedAsts in the list
    member this.DictionaryOfSUri2FplSourceCode()  = 
        let ret = System.Collections.Generic.Dictionary<PathEquivalentUri,string>()
        this
        |> Seq.iter (fun pa -> ret.TryAdd(pa.Parsing.Uri,pa.Parsing.FplSourceCode) |> ignore)
        ret 

type FplValueType =
    | Variable
    | VariadicVariableMany
    | VariadicVariableMany1
    | MandatoryPredicate
    | OptionalPredicate
    | MandatoryFunctionalTerm
    | OptionalFunctionalTerm
    | Constructor
    | Class 
    | Object
    | Theorem
    | Localization
    | Lemma
    | Proposition
    | Corollary
    | Proof
    | Conjecture
    | Axiom
    | RuleOfInference
    | Quantor
    | Predicate
    | FunctionalTerm
    | Reference
    | Theory
    | Argument
    | Justification
    | ArgInference
    | Translation
    | Mapping
    | Root
    member private this.UnqualifiedName = 
        match this with
            // parser error messages
            | Variable -> "variable"
            | VariadicVariableMany -> "zero-or-more variable"
            | VariadicVariableMany1 -> "one-or-more variable"
            | MandatoryPredicate -> "predicate property"
            | OptionalPredicate -> "optional predicate property"
            | MandatoryFunctionalTerm -> "functional term property"
            | OptionalFunctionalTerm -> "optional functional term property"
            | Constructor -> "constructor"
            | Class -> "class definition"
            | Object -> "object"
            | Localization -> "localization"
            | Theorem -> "theorem"
            | Lemma -> "lemma"
            | Proposition -> "proposition"
            | Corollary -> "corollary"
            | Proof -> "proof"
            | Conjecture -> "conjecture"
            | Axiom -> "axiom"
            | RuleOfInference -> "rule of inference"
            | Quantor -> "quantor"
            | Predicate -> "predicate definition"
            | FunctionalTerm -> "functional term definition"
            | Reference -> "reference"
            | Theory -> "theory"
            | Argument -> "argument"
            | Justification -> "justification"
            | ArgInference -> "argument inference"
            | Translation -> "translation"
            | Mapping -> "mapping"
            | Root -> "root"
    member private this.Article = 
        match this with
        | OptionalPredicate
        | OptionalFunctionalTerm
        | Object 
        | Argument 
        | ArgInference 
        | Axiom -> "an"
        | _ -> "a"

    member this.Name = this.Article + " " + this.UnqualifiedName

    member this.ShortName = 
        match this with
            // parser error messages
            | Variable -> "var"
            | VariadicVariableMany -> "*var"
            | VariadicVariableMany1 -> "+var"
            | MandatoryPredicate 
            | OptionalPredicate 
            | MandatoryFunctionalTerm 
            | OptionalFunctionalTerm -> "prop"
            | Constructor -> "ctor"
            | Class -> "cl"
            | Object -> "obj"
            | Localization -> "loc"
            | Theorem -> "thm"
            | Lemma -> "lem"
            | Proposition -> "prop"
            | Corollary -> "cor"
            | Proof -> "prf"
            | Conjecture -> "conj"
            | Axiom -> "ax"
            | RuleOfInference -> "inf"
            | Quantor -> "qtr"
            | Predicate -> "pred"
            | FunctionalTerm -> "func"
            | Reference -> "ref"
            | Theory -> "th"
            | Argument -> "arg"
            | Justification -> "just"
            | ArgInference -> "ainf"
            | Translation -> "trsl"
            | Mapping -> "map"
            | Root -> "root"

type FplPredicate =
    | True
    | False
    | Undetermined
    
type FplLanguageConstruct =
    | Function
    | Class
    | Extension

type FixType = 
    | Infix of string * int
    | Postfix of string 
    | Prefix of string 
    | NoFix
    member this.Type = 
        match this with 
        | Infix (symbol,precedence) -> sprintf "infix `%s` (with precedence `%i`)" symbol precedence
        | Postfix symbol -> sprintf "postfix `%s` " symbol
        | Prefix symbol -> sprintf "prefix `%s` " symbol
        | NoFix -> "no fix"

type FplRepresentation = 
    | PredRepr of FplPredicate 
    | ObjRepr of string
    | Localization of FplValue * string
    | LangRepr of FplLanguageConstruct
    | Index of uint
    | Undef
    member this.String() = 
        match this with
        | PredRepr _ -> "predicate"
        | ObjRepr _ -> "object"
        | Localization _ -> "localization"
        | Index _ -> "index"
        | LangRepr FplLanguageConstruct.Class -> "class"
        | LangRepr FplLanguageConstruct.Extension -> "extension"
        | LangRepr FplLanguageConstruct.Function -> "function"
        | Undef -> "undefined"
and ScopeSearchResult = 
    | FoundAssociate of FplValue 
    | FoundMultiple of string
    | FoundIncorrectBlock of string
    | Found of FplValue
    | NotFound
    | NotApplicable

and FplValue(name:string, blockType: FplValueType, positions: Positions, parent: FplValue option) =
    let mutable _expressionType = FixType.NoFix
    let mutable _exprTypeAlreadySet = false 
    let mutable _nameStartPos = fst positions
    let mutable _nameEndPos = snd positions
    let mutable _representation = FplRepresentation.Undef
    let mutable _blockType = blockType
    let mutable _auxiliaryInfo = 0
    let mutable _arity = 0
    let mutable _fplId = ""
    let mutable _typeId = ""
    let mutable _hasBrackets = false
    let mutable _isSignatureVariable = false

    let mutable _parent = parent
    let _auxiliaryUniqueChilds = HashSet<string>()
    let _scope = System.Collections.Generic.Dictionary<string, FplValue>()
    let _valueList = System.Collections.Generic.List<FplValue>()

    /// Indicates if this FplValue's Scope or ValueList can be treated as bracketed coordinates or as parenthesized parameters.
    member this.HasBrackets 
        with get () = _hasBrackets
        and set (value) = _hasBrackets <- value

    /// TypeId of the FplValue.
    member this.TypeId 
        with get () = _typeId
        and set (value) = _typeId <- value

    /// NameId of the FplValue.
    member this.FplId 
        with get () = _fplId
        and set (value) = _fplId <- value

    /// Type of the Expr
    member this.ExpressionType
        with get () = _expressionType
        and set (value) = 
            if not _exprTypeAlreadySet then 
                    _expressionType <- value
                    _exprTypeAlreadySet <- true
            else
                raise (ArgumentException($"Type was already initialized with `{_expressionType.Type}`, cannot set it again with {value.Type}."))

    /// Type of the FPL block within this FplValue
    member this.BlockType
        with get () = _blockType
        and set (value) = _blockType <- value

    /// Name of the FPL block type within this FplValue
    member this.BlockTypeName
        with get () = _blockType.Name

    /// Short name of the FPL block type within this FplValue
    member this.BlockTypeShortName
        with get () = _blockType.ShortName

    /// Starting position of this FplValue
    member this.NameStartPos 
        with get () = _nameStartPos
        and set (value) = _nameStartPos <- value

    /// This FplValue's name's end position that can be different from its endig position
    member this.NameEndPos
        with get () = _nameEndPos
        and set (value) = _nameEndPos <- value

    /// A representation of the constructed object (if any)
    member this.FplRepresentation
        with get () = _representation
        and set (value) = _representation <- value

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

    /// A list of asserted predicates for this FplValue
    member this.AssertedPredicates = System.Collections.Generic.List<Ast>()

    /// A scope inside this FplValue
    member this.Scope = _scope

    /// A value list inside this FplValue
    member this.ValueList = _valueList

    /// Type Identifier of this FplValue 
    member this.Type (isSignature:bool) =
        let paramTuple() = 
            this.Scope
            |> Seq.filter (fun (kvp: KeyValuePair<string,FplValue>) -> kvp.Value.IsSignatureVariable)
            |> Seq.map (fun (kvp: KeyValuePair<string,FplValue>) -> 
                kvp.Value.Type(isSignature))
            |> String.concat ", "
        let mapping =
            if this.ValueList.Count>0 && this.ValueList[0].BlockType = FplValueType.Mapping then
                Some (this.ValueList[0])
            else   
                None
        let argumentTuple() = 
            this.ValueList
            |> Seq.map (fun fv -> fv.Type(false))
            |> String.concat ", "
        let idRec() =
            match this.BlockType with
            | FplValueType.Theory 
            | FplValueType.Proof 
            | FplValueType.Class ->
                this.FplId
            | FplValueType.Theorem 
            | FplValueType.Lemma 
            | FplValueType.Proposition
            | FplValueType.Conjecture 
            | FplValueType.RuleOfInference
            | FplValueType.Predicate
            | FplValueType.Corollary 
            | FplValueType.Constructor 
            | FplValueType.OptionalPredicate 
            | FplValueType.MandatoryPredicate 
            | FplValueType.Axiom ->
                sprintf "%s(%s)" this.FplId (paramTuple())
            | FplValueType.OptionalFunctionalTerm 
            | FplValueType.MandatoryFunctionalTerm
            | FplValueType.FunctionalTerm ->
                match mapping with 
                | Some map -> 
                    sprintf "%s(%s) -> %s" this.FplId (paramTuple()) (map.Type(isSignature))
                | _ -> ""
            | FplValueType.Mapping 
            | FplValueType.Variable 
            | FplValueType.Argument 
            | FplValueType.VariadicVariableMany 
            | FplValueType.VariadicVariableMany1 ->
                let subType = paramTuple()
                if isSignature then 
                    match (subType, mapping) with
                    | ("",_) -> 
                        this.TypeId 
                    | (_,None) -> 
                        if this.HasBrackets then 
                            sprintf "%s[%s]" this.TypeId subType
                        else
                            sprintf "%s(%s)" this.TypeId subType
                    | (_,Some map) ->
                        if this.HasBrackets then 
                            sprintf "%s[%s] -> %s" this.TypeId subType (map.Type(isSignature))
                        else
                            sprintf "%s(%s) -> %s" this.TypeId subType (map.Type(isSignature))
                else
                    this.FplId 
            | FplValueType.Reference ->
                let args = argumentTuple()
                match (args, this.FplId) with
                | ("","") -> "???" // this case should never occur after full evaluation
                | ("",_) -> this.FplId
                | (_,"") -> sprintf "%s()" args
                | (_,_) -> 
                    if args<>"" then
                        if this.HasBrackets then 
                            sprintf "%s[%s]" this.FplId args
                        else
                            sprintf "%s(%s)" this.FplId args
                    else
                        this.FplId
            | _ -> ""
        idRec()

    /// Indicates if this FplValue is an FPL building block.
    static member IsFplBlock(fplValue:FplValue) = 
        match fplValue.BlockType with 
        | FplValueType.Axiom
        | FplValueType.Theorem 
        | FplValueType.Lemma 
        | FplValueType.Proposition 
        | FplValueType.Corollary 
        | FplValueType.Conjecture 
        | FplValueType.Proof 
        | FplValueType.RuleOfInference 
        | FplValueType.Predicate 
        | FplValueType.FunctionalTerm 
        | FplValueType.Class 
        | FplValueType.Localization -> true
        | _ -> false

    /// Indicates if this FplValue is a definition
    static member IsDefinition(fplValue:FplValue) = 
        match fplValue.BlockType with 
        | FplValueType.Predicate 
        | FplValueType.FunctionalTerm 
        | FplValueType.Class -> true
        | _ -> false

    /// Indicates if this FplValue is an root of the symbol table.
    static member IsRoot(fplValue:FplValue) = 
        fplValue.BlockType = FplValueType.Root

    /// Qualified name of this FplValue 
    member this.QualifiedName
        with get () = 
            let rec getFullName (fplValue:FplValue) (first:bool) =
                let fplValueType = fplValue.Type(false)
                if fplValue.BlockType = FplValueType.Root then
                    ""
                elif first then 
                    if FplValue.IsRoot(fplValue.Parent.Value) then 
                        getFullName fplValue.Parent.Value false + fplValueType 
                    else
                        if FplValue.IsVariable(fplValue) && not (FplValue.IsVariable(fplValue.Parent.Value)) then
                            fplValueType
                        else
                            getFullName fplValue.Parent.Value false + "." + fplValueType 
                else
                    if FplValue.IsRoot(fplValue.Parent.Value) then 
                        getFullName fplValue.Parent.Value false + fplValueType
                    else
                        if FplValue.IsVariable(fplValue) && not (FplValue.IsVariable(fplValue.Parent.Value)) then
                            fplValueType
                        else
                            getFullName fplValue.Parent.Value false + "." + fplValueType
            getFullName this true 

    /// Indicates if this FplValue is a constructor.
    static member IsConstructor(fplValue:FplValue) = 
        fplValue.BlockType = FplValueType.Constructor

    /// Indicates if this FplValue is a class.
    static member IsClass(fplValue:FplValue) = 
        match fplValue.BlockType with
        | FplValueType.Class (_) -> true
        | _ -> false

    /// Indicates if this FplValue is a proof.
    static member IsProof(fplValue:FplValue) = 
        fplValue.BlockType = FplValueType.Proof

    /// Indicates if this FplValue is a corollary.
    static member IsCorollary(fplValue:FplValue) = 
        fplValue.BlockType = FplValueType.Corollary

    /// Indicates if this FplValue is a property.
    static member IsProperty(fplValue:FplValue) = 
        fplValue.BlockType = FplValueType.MandatoryFunctionalTerm
        || fplValue.BlockType = FplValueType.OptionalFunctionalTerm
        || fplValue.BlockType = FplValueType.MandatoryPredicate
        || fplValue.BlockType = FplValueType.OptionalPredicate

    /// Indicates if this FplValue is a functional term.
    static member IsFunctionalTerm(fplValue:FplValue) = 
        fplValue.BlockType = FplValueType.MandatoryFunctionalTerm
        || fplValue.BlockType = FplValueType.OptionalFunctionalTerm
        || fplValue.BlockType = FplValueType.FunctionalTerm

    /// Indicates if this FplValue is a constructor or a property
    static member IsConstructorOrProperty(fplValue:FplValue)  = 
        FplValue.IsConstructor(fplValue) || FplValue.IsProperty(fplValue)

    /// Indicates if this FplValue is a constructor or a property
    static member IsProofOrCorollary(fplValue:FplValue) = 
        FplValue.IsProof(fplValue) || FplValue.IsCorollary(fplValue)

    /// Indicates if this FplValue is a constructor or a theory
    static member IsTheory (fplValue:FplValue) = 
        fplValue.BlockType = FplValueType.Theory


    /// Indicates if this FplValue is a block, a property, or a constructor.
    static member IsBlock(fplValue:FplValue) = 
        FplValue.IsFplBlock(fplValue) 
        || FplValue.IsProperty(fplValue) 
        || FplValue.IsConstructor(fplValue)


    /// Qualified starting position of this FplValue
    member this.QualifiedStartPos = 
        let rec getFullName (fplValue:FplValue) (first:bool) =
            let fplValueType = fplValue.Type(false)
            if fplValue.BlockType = FplValueType.Root then
                ""
            elif first then 
                let starPosWithoutFileName = $"(Ln: {fplValue.NameStartPos.Line}, Col: {fplValue.NameStartPos.Column})"
                if FplValue.IsTheory(fplValue) then 
                    getFullName fplValue.Parent.Value false + fplValueType + starPosWithoutFileName
                else
                    getFullName fplValue.Parent.Value false + starPosWithoutFileName
            else
                if FplValue.IsTheory(fplValue) then 
                    getFullName fplValue.Parent.Value false + fplValueType 
                else
                    getFullName fplValue.Parent.Value false 

        getFullName this true


    /// Indicates if this FplValue is a variable.
    static member IsVariable(fplValue:FplValue) = 
        fplValue.BlockType = FplValueType.Variable
        || fplValue.BlockType = FplValueType.VariadicVariableMany
        || fplValue.BlockType = FplValueType.VariadicVariableMany1

    /// Indicates if this FplValue is a variable declared in the signature (true) or in the block (false).
    member this.IsSignatureVariable
        with get () = 
            if FplValue.IsVariable(this) then
                _isSignatureVariable
            else 
                false
        and set (value) = 
            if FplValue.IsVariable(this) then
                _isSignatureVariable <- value
            else 
                raise (ArgumentException(sprintf "Cannot set IsSignatureVariable for non-variable %s" this.BlockType.ShortName))

    /// Indicates if this FplValue is a reference
    static member IsReference(fplValue:FplValue) = 
        fplValue.BlockType = FplValueType.Reference

    /// Indicates if this FplValue is a variadic *variable.
    static member IsVariadicVariableMany(fplValue:FplValue) = 
        fplValue.BlockType = FplValueType.VariadicVariableMany

    /// Indicates if this FplValue is a variadic +variable.
    static member IsVariadicVariableMany1(fplValue:FplValue) = 
        fplValue.BlockType = FplValueType.VariadicVariableMany1

    /// Checks if a block is in the scope of its parent, by the name of the block. 
    static member InScopeOfParent(fplValue:FplValue) name = 
        let conflictInSiblingTheory (parent:FplValue) = 
            // if the parent is a theory, look also for its sibling theories
            let (conflicts:ScopeSearchResult list) = 
                let root = parent.Parent.Value
                root.Scope
                |> Seq.filter (fun siblingTheory ->
                    // look only for sibling theories 
                    siblingTheory.Value <> parent
                )
                |> Seq.choose (fun siblingTheory ->
                        if siblingTheory.Value.Scope.ContainsKey(name) then
                            let foundConflict = siblingTheory.Value.Scope[name]
                            Some (ScopeSearchResult.Found foundConflict)
                        else
                            None
                )
                |> Seq.toList
            let res = conflicts 
            if res.Length > 0 then 
                conflicts.Head
            else
                ScopeSearchResult.NotFound

        match fplValue.Parent with
        | Some parent ->
            if parent.Scope.ContainsKey(name) then
                let foundConflict = parent.Scope[name]
                ScopeSearchResult.Found foundConflict 
            else 
                if FplValue.IsTheory(parent) then 
                    conflictInSiblingTheory parent
                else
                    ScopeSearchResult.NotFound
        | None -> ScopeSearchResult.NotApplicable

    /// Checks if a variable is defined in the scope of block, if any
    /// looking for it recursively, up the symbol tree.
    static member VariableInBlockScopeByName(fplValue:FplValue) name = 
        let rec firstBlockParent (fv:FplValue) =
            if fv.Scope.ContainsKey name then
                if FplValue.IsProperty(fv) then
                    ScopeSearchResult.Found (fv.Scope[name])
                elif FplValue.IsConstructor(fv) then 
                    ScopeSearchResult.Found (fv.Scope[name])
                elif FplValue.IsProof(fv) then 
                    ScopeSearchResult.Found (fv.Scope[name])
                elif FplValue.IsCorollary(fv) then 
                    ScopeSearchResult.Found (fv.Scope[name])
                elif FplValue.IsFplBlock(fv) then 
                    ScopeSearchResult.Found (fv.Scope[name])
                elif FplValue.IsTheory(fv) then
                    ScopeSearchResult.NotFound
                elif fv.Parent.IsSome then 
                    firstBlockParent fv.Parent.Value
                else
                    ScopeSearchResult.NotFound
            else
                if fv.Parent.IsSome then 
                    firstBlockParent fv.Parent.Value
                else
                    ScopeSearchResult.NotFound

        firstBlockParent fplValue

    /// Checks if an fplValue is provable. This will only be true if 
    /// it is a theorem, a lemma, a proposition, or a corollary
    static member IsProvable(fplValue:FplValue) = 
        fplValue.BlockType = FplValueType.Theorem
        || fplValue.BlockType = FplValueType.Corollary
        || fplValue.BlockType = FplValueType.Lemma
        || fplValue.BlockType = FplValueType.Proposition

    /// A factory method for the evaluation of FPL theories
    static member CreateRoot() =
        let root = new FplValue("", FplValueType.Root, (Position("", 0, 1, 1), Position("", 0, 1, 1)), None)
        root

    /// A factory method for the FPL primitive Object
    static member CreateObject(pos1,pos2) =
        let obj = new FplValue("obj", FplValueType.Object, (pos1, pos2), None)
        obj.FplRepresentation <- FplRepresentation.ObjRepr "obj"
        obj

    /// A factory method for the evaluation of Fpl class definitions
    static member CreateFplValue(positions: Positions, fplBlockType: FplValueType, parent: FplValue) =
        match fplBlockType with
        | FplValueType.Axiom
        | FplValueType.Theorem
        | FplValueType.Lemma
        | FplValueType.Proposition
        | FplValueType.Corollary
        | FplValueType.Proof
        | FplValueType.Predicate 
        | FplValueType.RuleOfInference
        | FplValueType.Quantor
        | FplValueType.Mapping
        | FplValueType.Conjecture -> 
            let ret = new FplValue("", fplBlockType, positions, Some parent)
            ret.FplRepresentation <- FplRepresentation.PredRepr FplPredicate.Undetermined
            ret
        | FplValueType.Constructor -> 
            let ret = new FplValue("", fplBlockType, positions, Some parent)
            ret.FplRepresentation <- FplRepresentation.ObjRepr "obj"
            ret
        | FplValueType.Theory
        | FplValueType.MandatoryPredicate
        | FplValueType.OptionalPredicate
        | FplValueType.Reference 
        | FplValueType.FunctionalTerm
        | FplValueType.Variable
        | FplValueType.VariadicVariableMany
        | FplValueType.VariadicVariableMany1
        | FplValueType.MandatoryFunctionalTerm
        | FplValueType.Localization
        | FplValueType.Argument
        | FplValueType.Justification
        | FplValueType.ArgInference
        | FplValueType.Translation
        | FplValueType.OptionalFunctionalTerm -> new FplValue("", fplBlockType, positions, Some parent)
        | FplValueType.Class -> 
            let ret = new FplValue("", fplBlockType, positions, Some parent)
            ret.FplRepresentation <- FplRepresentation.LangRepr FplLanguageConstruct.Class
            ret
        | FplValueType.Root -> raise (ArgumentException("Please use CreateRoot for creating the root instead"))
        | FplValueType.Object -> raise (ArgumentException("Please use CreateObject for creating a primitive object instead"))

    /// Clears this FplValue
    member this.Reset() = 
        let rec clearAll (root:FplValue) =
            root.ValueList
            |> Seq.iter (fun child ->
                clearAll child
            )
            root.ValueList.Clear()
            root.Scope
            |> Seq.iter (fun child ->
                clearAll child.Value
            )
            root.Scope.Clear()
        clearAll this

    /// A string representation of this FplValue
    override this.ToString() = 
        $"{this.BlockTypeShortName} {this.Type(false)}"

// create an FplValue list containing all Scopes of the theory
let rec flattenScopes (root: FplValue) =
    let rec helper (node: FplValue) (acc: FplValue list) =
        let newAcc = node :: acc
        node.Scope
        |> Seq.fold (fun acc kvp -> helper kvp.Value acc) newAcc
    helper root []

let stripLastDollarDigit (s: string) =
    let lastIndex = s.LastIndexOf('$')
    if lastIndex <> -1 then
        s.Substring(0, lastIndex)
    else
        s

/// Tries to find a theorem-like statement for a proof 
/// and returns different cases of ScopeSearchResult, depending on different semantical error situations. 
let tryFindAssociatedBlockForProof (fplValue:FplValue) = 
    if fplValue.BlockType = FplValueType.Proof then
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
                flattenedScopes
                |> List.filter (fun fv ->
                    fv.Type(true).StartsWith(potentialProvableName + "(") || fv.Type(true) = potentialProvableName
                )

            let provableBlocklist = 
                buildingBlocksMatchingDollarDigitNameList
                |> List.filter (fun fv ->
                    FplValue.IsProvable(fv)
                )
            let notProvableBlocklist = 
                buildingBlocksMatchingDollarDigitNameList
                |> List.filter (fun fv ->
                    not (FplValue.IsProvable(fv))
                )
            if provableBlocklist.Length > 1 then
                ScopeSearchResult.FoundMultiple (provableBlocklist |> List.map (fun fv -> sprintf "%s %s" fv.BlockType.Name (fv.Type(true))) |> String.concat ", ")
            elif provableBlocklist.Length > 0 then 
                let potentialTheorem = provableBlocklist.Head
                ScopeSearchResult.FoundAssociate potentialTheorem
            elif notProvableBlocklist.Length > 0 then 
                let potentialOther = notProvableBlocklist.Head
                ScopeSearchResult.FoundIncorrectBlock (sprintf "%s %s" potentialOther.BlockTypeName potentialOther.QualifiedName)
            else
                ScopeSearchResult.NotFound
        | None -> ScopeSearchResult.NotApplicable
    else
        ScopeSearchResult.NotApplicable

/// Tries to find a therem-like statement, a conjecture, or an axiom for a corollary 
/// and returns different cases of ScopeSearchResult, depending on different semantical error situations. 
let tryFindAssociatedBlockForCorollary(fplValue:FplValue) = 

    if fplValue.BlockType = FplValueType.Corollary then
        match fplValue.Parent with
        | Some theory ->
            
            let flattenedScopes = flattenScopes theory.Parent.Value

            // The parent node of the proof is the theory. In its scope 
            // we should find the theorem we are looking for.
            let buildingBlocksMatchingDollarDigitNameList = 
                // the potential theorem name of the corollary is the 
                // concatenated type signature of the name of the corollary 
                // without the last dollar digit
                let potentialBlockName = 
                    stripLastDollarDigit (fplValue.Type(true))
                flattenedScopes
                |> Seq.filter (fun fv -> 
                    let fvType = fv.Type(true)
                    fvType.StartsWith(potentialBlockName + "(") || fvType = potentialBlockName 
                )
                |> Seq.toList
            let potentialBlockList = 
                buildingBlocksMatchingDollarDigitNameList
                |> List.filter (fun fv ->
                    FplValue.IsProvable(fv) 
                    || fv.BlockType = FplValueType.Conjecture
                    || fv.BlockType = FplValueType.Axiom
                )
            let notPotentialBlockList = 
                buildingBlocksMatchingDollarDigitNameList
                |> List.filter (fun fv ->
                    not (FplValue.IsProvable(fv) 
                    || fv.BlockType = FplValueType.Conjecture
                    || fv.BlockType = FplValueType.Axiom)
                )
            if potentialBlockList.Length > 1 then
                ScopeSearchResult.FoundMultiple (potentialBlockList |> List.map (fun fv -> sprintf "%s %s" fv.BlockType.Name (fv.Type(true))) |> String.concat ", ")
            elif potentialBlockList.Length > 0 then 
                let potentialTheorem = potentialBlockList.Head
                ScopeSearchResult.FoundAssociate potentialTheorem
            elif notPotentialBlockList.Length > 0 then 
                let potentialOther = notPotentialBlockList.Head
                ScopeSearchResult.FoundIncorrectBlock potentialOther.QualifiedName
            else
                ScopeSearchResult.NotFound
        | None -> ScopeSearchResult.NotApplicable
    else
        ScopeSearchResult.NotApplicable



type SymbolTable(parsedAsts:ParsedAstList, debug:bool) =
    let _parsedAsts = parsedAsts
    let mutable _mainTheory = ""
    let _evalPath = Stack<string>()
    let _evalLog = List<string>()
    let _root = FplValue.CreateRoot()
    let _debug = debug

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
        _evalPath 
        |> Seq.toList 
        |> List.rev 
        |> String.concat "."

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
        let res = _parsedAsts
                    |> Seq.map(fun pa -> pa.Parsing.Ast.ToString())
                    |> String.concat Environment.NewLine
        res

    /// Add the current ast name to the recursive evaluation path.
    member this.EvalPush(astName:string) = 
        _evalPath.Push(astName)

    /// Remove the current ast name from the recursive evaluation path.
    member this.EvalPop() = 
        _evalPath.Pop() |> ignore


    /// If there is a valid topological sorting, order the list descending by this ordering.
    member this.OrderAsts() =
        _parsedAsts.Sort(
            Comparer<ParsedAst>.Create(fun b a -> compare a.Sorting.TopologicalSorting b.Sorting.TopologicalSorting)
        )

    /// Serializes the symbol table as json
    member this.ToJson() = 
        let sb = StringBuilder()
        let rec createJson (root:FplValue) (sb:StringBuilder) level isLast =
            let indent = String(' ', level)
            sb.AppendLine(String(' ', level - 1) + "{") |> ignore
            if root.Type(false) = this.MainTheory then
                sb.AppendLine($"{indent}\"Name\": \"Main> {root.Type(false)}\",") |> ignore
            else
                sb.AppendLine($"{indent}\"Name\": \"{root.Type(true)}\",") |> ignore
            sb.AppendLine($"{indent}\"Type\": \"{root.BlockType.ShortName}\",") |> ignore
            sb.AppendLine($"{indent}\"Scope\": [") |> ignore
            let mutable counterScope = 0
            root.Scope
            |> Seq.iter (fun child ->
                counterScope <- counterScope + 1
                createJson child.Value sb (level + 1) (counterScope = root.Scope.Count)
            )
            sb.AppendLine($"{indent}],") |> ignore
            sb.AppendLine($"{indent}\"ValueList\": [") |> ignore

            let mutable valueList = 0
            root.ValueList
            |> Seq.iter (fun child ->
                valueList <- valueList + 1
                createJson child sb (level + 1) (valueList = root.ValueList.Count)
            )
            sb.AppendLine($"{indent}]") |> ignore
            if isLast then
                sb.AppendLine(String(' ', level - 1) + "}") |> ignore
            else
                sb.AppendLine(String(' ', level - 1) + "},") |> ignore
        createJson this.Root sb 1 false
        let res = sb.ToString().TrimEnd()
        if res.EndsWith(',') then 
            res.Substring(0,res.Length - 1)
        else
            res

    /// Returns the uses dependencies of this symbol table needed e.g. for debugging purposes in the FPL language server.
    member this.UsesDependencies() =
        let sb = StringBuilder()
        sb.AppendLine() |> ignore
        sb.AppendLine("SymbolTable: ") |> ignore
        this.Root.Scope
        |> Seq.map (fun theory -> 
            $"{theory.Value.Type(false)} ({theory.Value.Scope.Count})"
        )
        |> String.concat Environment.NewLine
        |> sb.AppendLine
        |> ignore

        sb.AppendLine("ParsedAsts: ") |> ignore
        this.ParsedAsts
        |> Seq.map (fun pa -> 
            $"[{pa.Id}, {pa.Sorting.TopologicalSorting}, {pa.Sorting.ReferencedAsts}, {pa.Sorting.ReferencingAsts}]"
        )
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
                |> Seq.map (fun (groupId,group) -> $"{groupId}:{Seq.length group}")
                |> String.concat ", "
            sb.AppendLine $"{pa.Id}(chksm {pa.Parsing.Checksum}): #total diags {paDiagnostics.Count}, {statsDiags}" |> ignore
        )
        sb.ToString()


/// Looks for all declared building blocks with a specific name.
let findCandidatesByName (st:SymbolTable) (name:string) =
    let pm = List<FplValue>()
    st.Root.Scope // iterate all theories
    |> Seq.iter (fun theory -> 
        theory.Value.Scope
        // filter only blocks starting with the same FplId as the reference
        |> Seq.filter (fun fv -> fv.Value.FplId = name) 
        |> Seq.iter (fun block -> pm.Add(block.Value)) 
    ) |> ignore
    pm |> Seq.toList


let rec checkCandidates (toBeMatchedTypeSignature: string) (candidates: FplValue list) accResult =
    /// Compares two strings and returns a tuple of (a,b,i) where i is None, 
    /// if both lists are identical. If i = Some index, then a and b contain strings that are not equal at that index.
    let findFirstMismatchPosition (str1:string) (str2:string) =
        if str1.Length > str2.Length then
            (str1[str1.Length-1].ToString(),"",Some(str1.Length-1))
        elif str1.Length < str2.Length then 
            ("", str2[str2.Length-1].ToString(),Some(str2.Length-1))
        else
            let rec loop i =
                if i >= str1.Length || i >= str2.Length then
                    None
                elif str1.[i] <> str2.[i] then
                    Some i
                else
                    loop (i + 1)
            match loop 0 with
            | Some ind -> 
                (str1[ind].ToString(),str2[ind].ToString(),Some(ind))
            | None -> ("","",None)

    // matches the TypeSignature of every candidate with a toBeMatchedTypeSignature
    match candidates with
    | [] -> (accResult, None) // all candidates mismatch toBeMatchedTypeSignature
    | x :: xs ->
        match findFirstMismatchPosition toBeMatchedTypeSignature (x.Type(true)) with
        | ("", "", None) -> (accResult, Some x) // there is a candidate that matches toBeMatchedTypeSignature
        | ("", elem2, Some index) -> 
            checkCandidates toBeMatchedTypeSignature xs $"missing token `{elem2}` after matching {index}" // first reason for mismatch
        | (elem1, "", Some index) -> 
            checkCandidates toBeMatchedTypeSignature xs $"superfluous token `{elem1}` after matching {index}" // second reason for mismatch
        | (elem1, elem2, Some index) -> 
            checkCandidates toBeMatchedTypeSignature xs $"`{elem1}` with `{elem2}` at position {index}" // third reason for mismatch
        | _ -> checkCandidates toBeMatchedTypeSignature xs accResult // accumulate reasons

/// Tries to match the signature of a reference FplValue with some overload. 
/// Returns a tuple (a,b,c) where 
/// a = a string indicating the first mismatching argument that couldn't be matched,
/// b = a list of candidates that were identified to match the reference,
/// c = Some or None candidate that was matched.
let tryMatchSignatures (st:SymbolTable) (reference:FplValue) = 
    let candidates = findCandidatesByName st (reference.FplId)

    if candidates.IsEmpty then 
        ("", candidates, None)
    else
        let accResult, matchedCandidate = checkCandidates (reference.Type(true)) candidates ""
        (accResult, candidates, matchedCandidate)

/// Tries to match the signatures of two types.
/// Returns a tuple (a,b,c) where 
/// a = a string indicating the first mismatching argument that couldn't be matched,
/// b = a list of candidates that were identified to match the reference,
/// c = Some or None candidate that was matched.
let tryMatchTypes (st:SymbolTable) (typeFplValue:FplValue) name = 
    let candidates = findCandidatesByName st name

    let stripFunctionalTermTypeSignature (typeFplValue:FplValue) = 
        match typeFplValue.BlockType with
        | FplValueType.MandatoryFunctionalTerm  
        | FplValueType.OptionalFunctionalTerm  
        | FplValueType.FunctionalTerm -> 
            let mapping = 
                if typeFplValue.ValueList.Count>0 then
                    Some(typeFplValue.ValueList[0])
                else
                    None
            match mapping with
            | Some map -> map.Type(true)
            | None -> typeFplValue.Type(true)
        | _ -> 
            typeFplValue.Type(true)

    let toBeCheckedSignature = stripFunctionalTermTypeSignature typeFplValue
    if candidates.IsEmpty then 
        ("", candidates, None)
    else
        let accResult, matchedCandidate = checkCandidates toBeCheckedSignature candidates ""
        (accResult, candidates, matchedCandidate)
