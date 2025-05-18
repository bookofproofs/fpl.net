module FplInterpreterTypes

open System
open System.Text.RegularExpressions
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
    | Language
    | Translation
    | Mapping
    | Stmt
    | Assertion
    | Extension
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
            | Language -> "language"
            | Translation -> "translation"
            | Mapping -> "mapping"
            | Stmt -> "statement"
            | Assertion -> "assertion"
            | Extension -> "extension"
            | Root -> "root"
    member private this.Article = 
        match this with
        | OptionalPredicate
        | OptionalFunctionalTerm
        | Object 
        | Argument 
        | Assertion 
        | ArgInference 
        | Extension 
        | Axiom -> "an"
        | _ -> "a"

    member this.Name = this.Article + " " + this.UnqualifiedName

    member this.ShortName = 
        match this with
            // parser error messages
            | Variable -> "var"
            | VariadicVariableMany -> "*var"
            | VariadicVariableMany1 -> "+var"
            | MandatoryPredicate -> "mpred"
            | OptionalPredicate  -> "opred"
            | MandatoryFunctionalTerm -> "mfunc"
            | OptionalFunctionalTerm -> "ofunc"
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
            | Language -> "lang"
            | Translation -> "trsl"
            | Mapping -> "map"
            | Stmt -> "stmt"
            | Assertion -> "ass"
            | Extension -> "ext"
            | Root -> "root"

type FixType = 
    | Infix of string * int
    | Postfix of string 
    | Prefix of string 
    | Symbol of string
    | NoFix
    member this.Type = 
        match this with 
        | Infix (symbol,precedence) -> sprintf "infix `%s` (with precedence `%i`)" symbol precedence
        | Postfix symbol -> sprintf "postfix `%s` " symbol
        | Prefix symbol -> sprintf "prefix `%s` " symbol
        | Symbol symbol -> sprintf "symbol `%s`" symbol
        | NoFix -> "no fix"

type SignatureType = 
    | Name
    | Type
    | Mixed
    | Repr


type ScopeSearchResult = 
    | FoundAssociate of FplValue 
    | FoundMultiple of string
    | FoundIncorrectBlock of string
    | Found of FplValue
    | NotFound
    | NotApplicable

and FplValue(blockType: FplValueType, positions: Positions, parent: FplValue option) =
    let mutable _expressionType = FixType.NoFix
    let mutable _exprTypeAlreadySet = false 
    let mutable _nameStartPos = fst positions
    let mutable _nameEndPos = snd positions
    let mutable _blockType = blockType
    let mutable _auxiliaryInfo = 0
    let mutable _arity = 0
    let mutable _fplId = ""
    let mutable _typeId = ""
    let mutable (_filePath:string option) = None
    let mutable _reprId = "undef"
    let mutable _hasBrackets = false
    let mutable _isIntrinsic = false
    let mutable _isSignatureVariable = false

    let mutable _parent = parent
    let _scope = Dictionary<string, FplValue>()
    let _valueList = List<FplValue>()
    let _assertedPredicates = List<FplValue>()

    /// Indicates if this FplValue's Scope or ValueList can be treated as bracketed coordinates or as parenthesized parameters.
    member this.HasBrackets 
        with get () = _hasBrackets
        and set (value) = _hasBrackets <- value

    /// TypeId of the FplValue.
    member this.TypeId 
        with get () = _typeId
        and set (value) = _typeId <- value

    /// ReprId of the FplValue.
    member this.ReprId 
        with get () = _reprId
        and set (value) = _reprId <- value

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
    member this.AssertedPredicates = _assertedPredicates

    /// A scope inside this FplValue
    member this.Scope = _scope

    /// A value list inside this FplValue
    member this.ValueList = _valueList

    /// Tries to find a mapping of this FplValue
    member this.Mapping =
        match this.BlockType with
        | FplValueType.Reference ->
            if this.Scope.ContainsKey(this.FplId) then
                this.Scope[this.FplId].Mapping
            else
                None
        | _ -> 
            if this.ValueList.Count>0 && this.ValueList[0].BlockType = FplValueType.Mapping then
                Some (this.ValueList[0])
            else   
                None

    /// Type Identifier of this FplValue 
    member this.Type (isSignature:SignatureType) =
        match (this.BlockType, this.Scope.ContainsKey(this.FplId)) with 
        | (FplValueType.Reference, true) -> 
            // delegate the type identifier to the referenced entitity
            let val1 = this.Scope[this.FplId]
            val1.Type(isSignature)
        | (FplValueType.Stmt, _) 
        | (FplValueType.Extension, _) -> 
            this.FplId
        | _ -> 
            let head = 
                match (this.BlockType, this.Scope.ContainsKey(this.FplId)) with 
                | (FplValueType.Reference, true) -> 
                    match isSignature with
                    | SignatureType.Name -> this.Scope[this.FplId].Type(SignatureType.Name)
                    | SignatureType.Type -> _typeId
                    | SignatureType.Mixed -> _fplId
                    | SignatureType.Repr -> _reprId
                | (FplValueType.Mapping, _) -> 
                    _typeId
                | _ -> 
                    match isSignature with
                    | SignatureType.Name -> _fplId
                    | SignatureType.Type -> _typeId
                    | SignatureType.Mixed -> _fplId
                    | SignatureType.Repr -> _reprId
            match (isSignature, this.TypeId) with
            | (SignatureType.Repr, _) when (this.BlockType = VariadicVariableMany1 || this.BlockType = VariadicVariableMany) -> 
                let variadicContent = 
                    this.ValueList
                    |> Seq.map (fun fv -> fv.Type(isSignature))
                    |> String.concat ", "
                $"{this.Type(SignatureType.Type)}" + "{" + variadicContent + "}"
            | (SignatureType.Repr, _) when this.BlockType = Variable ->
                let vOpt = this.GetValue
                match vOpt with
                | Some (v:FplValue) -> v.Type(isSignature)
                | None -> head
            | (SignatureType.Repr, typeId) when typeId = "pred" || typeId.StartsWith "pred(" || typeId.StartsWith "pred$" -> 
                _reprId
            | (SignatureType.Repr, typeId) when typeId = "func" || typeId.StartsWith "func(" -> 
                _reprId
            | (SignatureType.Repr, typeId) when typeId = "undef" || typeId = "ind" -> 
                _reprId
            | (SignatureType.Repr, typeId) when this.BlockType = FplValueType.Class -> 
                _reprId
            | _ -> 

                let propagate = 
                    match isSignature with
                    | SignatureType.Name 
                    | SignatureType.Type 
                    | SignatureType.Repr -> isSignature
                    | SignatureType.Mixed -> SignatureType.Type

                let paramTuple() = 
                    this.Scope
                    |> Seq.filter (fun (kvp: KeyValuePair<string,FplValue>) -> kvp.Value.IsSignatureVariable || FplValue.IsVariable(this) || this.BlockType = FplValueType.Mapping)
                    |> Seq.map (fun (kvp: KeyValuePair<string,FplValue>) -> 
                        kvp.Value.Type(propagate))
                    |> String.concat ", "

                let idRec() =
                    match this.BlockType with
                    | FplValueType.Theory 
                    | FplValueType.Proof 
                    | FplValueType.Argument 
                    | FplValueType.Language 
                    | FplValueType.Object 
                    | FplValueType.Class ->
                        head
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
                        let paramT = paramTuple()
                        sprintf "%s(%s)" head paramT
                    | FplValueType.Quantor ->
                        let paramT = 
                            this.Scope
                            |> Seq.filter (fun (kvp: KeyValuePair<string,FplValue>) -> FplValue.IsVariable(kvp.Value))
                            |> Seq.map (fun (kvp: KeyValuePair<string,FplValue>) -> 
                                kvp.Value.Type(isSignature))
                            |> String.concat ", "
                        match paramT with
                        | "" -> head
                        | _ -> sprintf "%s(%s)" head paramT
                    | FplValueType.Localization -> 
                        let paramT = 
                            this.Scope
                            |> Seq.filter (fun (kvp: KeyValuePair<string,FplValue>) -> FplValue.IsVariable(kvp.Value))
                            |> Seq.map (fun (kvp: KeyValuePair<string,FplValue>) -> 
                                kvp.Value.Type(isSignature))
                            |> String.concat ", "
                        match paramT with
                        | "" -> head
                        | _ -> sprintf "%s(%s)" head paramT
                    | FplValueType.OptionalFunctionalTerm 
                    | FplValueType.MandatoryFunctionalTerm
                    | FplValueType.FunctionalTerm ->
                        match this.Mapping with 
                        | Some map -> 
                            let paramT = paramTuple()
                            sprintf "%s(%s) -> %s" head paramT (map.Type(propagate))
                        | _ -> ""
                    | FplValueType.Translation ->
                        let args =
                            this.ValueList
                            |> Seq.filter(fun fv -> fv.BlockType <> FplValueType.Stmt && fv.BlockType <> FplValueType.Assertion)
                            |> Seq.map (fun fv -> fv.Type(SignatureType.Name))
                            |> String.concat ""
                        sprintf "%s%s" head args
                    | FplValueType.Mapping 
                    | FplValueType.Variable 
                    | FplValueType.VariadicVariableMany 
                    | FplValueType.VariadicVariableMany1 ->
                        let pars = paramTuple()
                        match (pars, this.Mapping) with
                        | ("",None) -> 
                            head 
                        | ("",Some map) ->
                            sprintf "%s() -> %s" head (map.Type(propagate))
                        | (_,None) -> 
                            if this.HasBrackets then 
                                sprintf "%s[%s]" head pars
                            else
                                sprintf "%s(%s)" head pars
                        | (_,Some map) ->
                            if this.HasBrackets then 
                                sprintf "%s[%s] -> %s" head pars (map.Type(propagate))
                            else
                                sprintf "%s(%s) -> %s" head pars (map.Type(propagate))
                    | FplValueType.Reference ->
                        let qualification = 
                            if this.Scope.ContainsKey(".") then 
                                Some(this.Scope["."])
                            else 
                                None
                        // The arguments are reserved for the arguments or the coordinates of the reference
                        // If the argument tuple equals "???", an empty argument or coordinates list has occurred
                        let args =
                            let a = 
                                this.ValueList
                                |> Seq.filter(fun fv -> fv.BlockType <> FplValueType.Stmt && fv.BlockType <> FplValueType.Assertion)
                                |> Seq.map (fun fv -> fv.Type(propagate))
                                |> String.concat ", "
                            a
                        match (head, args, qualification) with
                        | (_, "", Some qual) ->
                            sprintf "%s.%s" head (qual.Type(propagate))
                        | (_, "???", Some qual) -> 
                            if this.HasBrackets then 
                                sprintf "%s[].%s" head (qual.Type(propagate))
                            else
                                sprintf "%s().%s" head (qual.Type(propagate))
                        | (_, _, Some qual) -> 
                            if this.HasBrackets then 
                                sprintf "%s[%s].%s" head args (qual.Type(propagate))
                            else
                                sprintf "%s(%s).%s" head args (qual.Type(propagate))
                        | ("???", _, None) -> 
                            sprintf "%s" head
                        | ("", _, None) -> 
                            sprintf "%s" args
                        | (_, "", None) -> 
                             sprintf "%s" head 
                        | (_, "???", None) -> 
                            if this.HasBrackets then 
                                sprintf "%s[]" head 
                            else
                                sprintf "%s()" head 
                        | (_, _, None) -> 
                            if this.HasBrackets then 
                                sprintf "%s[%s]" head args 
                            elif head = "bydef." then
                                sprintf "%s%s" head args
                            else
                                sprintf "%s(%s)" head args 
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
        | FplValueType.Class -> true
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
            let rec getFullName (fv:FplValue) (first:bool) =
                let fplValueType = 
                    match fv.BlockType with
                    | FplValueType.Localization
                    | FplValueType.Reference -> 
                        fv.Type(SignatureType.Name)
                    | FplValueType.Localization 
                    | FplValueType.Constructor
                    | _ when FplValue.IsBlock(fv) -> 
                        fv.Type(SignatureType.Mixed)
                    | FplValueType.Quantor -> 
                        fv.Type(SignatureType.Mixed)
                    | _ -> 
                        fv.FplId
                if fv.BlockType = FplValueType.Root then
                    ""
                elif first then 
                    if FplValue.IsRoot(fv.Parent.Value) then 
                        getFullName fv.Parent.Value false + fplValueType 
                    else
                        if FplValue.IsVariable(fv) && not (FplValue.IsVariable(fv.Parent.Value)) then
                            fplValueType
                        else
                            getFullName fv.Parent.Value false + "." + fplValueType 
                else
                    if FplValue.IsRoot(fv.Parent.Value) then 
                        getFullName fv.Parent.Value false + fplValueType
                    else
                        if FplValue.IsVariable(fv) && not (FplValue.IsVariable(fv.Parent.Value)) then
                            fplValueType
                        else
                            getFullName fv.Parent.Value false + "." + fplValueType
            getFullName this true 
   
    /// Returns Some value of the FplValue taking into account if this 
    /// the value can be accessed directly or if the 
    /// FplValue is a Reference to a variable. In the latter case, 
    /// the value of the referenced variable is returned instead.
    member this.GetValue =
        match this.BlockType with
        | FplValueType.Reference when this.Scope.ContainsKey(this.FplId) ->
            let var = this.Scope[this.FplId]
            if var.ValueList.Count>0 then 
                Some var.ValueList[0]
            else
                Some var
        | FplValueType.Reference when this.FplId <> "" ->
            Some this
        | FplValueType.Reference when this.ValueList.Count = 0 ->
            Some this
        | _ when this.ValueList.Count>0 ->
            Some this.ValueList[0]
        | _ -> 
            None
    /// Indicates if this FplValue is a class.
    static member IsClass(fplValue:FplValue) = 
        fplValue.BlockType = FplValueType.Class

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
        match fplValue.BlockType with 
        | FplValueType.Constructor -> true
        | _ -> FplValue.IsProperty(fplValue)

    /// Indicates if this FplValue is a constructor or a property
    static member IsProofOrCorollary(fplValue:FplValue) = 
        FplValue.IsProof(fplValue) || FplValue.IsCorollary(fplValue)

    /// Indicates if this FplValue is a constructor or a theory
    static member IsTheory (fplValue:FplValue) = 
        fplValue.BlockType = FplValueType.Theory


    /// Indicates if this FplValue is a block, a property, or a constructor.
    static member IsBlock(fplValue:FplValue) = 
        match fplValue.BlockType with
        | FplValueType.Constructor -> true
        | _ -> 
            FplValue.IsFplBlock(fplValue) || FplValue.IsProperty(fplValue) 


    /// Qualified starting position of this FplValue
    member this.QualifiedStartPos = 
        let rec getFullName (fplValue:FplValue) (first:bool) =
            let fplValueType = fplValue.Type(SignatureType.Mixed)
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

    /// Indicates if this FplValue is an intrinsically defined block
    member this.IsIntrinsic
        with get () = _isIntrinsic
        and set (value) = _isIntrinsic <- value

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
    static member VariableInBlockScopeByName(fplValue:FplValue) name withNestedVariableSearch = 
        let rec firstBlockParent (fv:FplValue) =
            
            let qualifiedVar (fv1:FplValue) =
                let allVarsInScope = fv1.GetVariables()
                
                // try out all variables in scope
                let foundList = 
                    allVarsInScope
                    |> Seq.map (fun (var:FplValue) -> 
                        if var.Scope.ContainsKey name then 
                            ScopeSearchResult.Found (var.Scope[name])
                        else
                            ScopeSearchResult.NotFound
                    )
                    |> Seq.filter (fun ssr -> ssr <> ScopeSearchResult.NotFound)
                    |> Seq.toList

                if foundList.IsEmpty then 
                    firstBlockParent fv1.Parent.Value 
                else 
                    foundList.Head 

            match fv.BlockType with
            | FplValueType.Constructor  
            | FplValueType.Localization  
            | FplValueType.Quantor 
            | FplValueType.MandatoryFunctionalTerm
            | FplValueType.OptionalFunctionalTerm
            | FplValueType.MandatoryPredicate
            | FplValueType.OptionalPredicate
            | FplValueType.Proof
            | FplValueType.Corollary
            | FplValueType.Axiom
            | FplValueType.Theorem 
            | FplValueType.Lemma 
            | FplValueType.Proposition 
            | FplValueType.Corollary 
            | FplValueType.Conjecture 
            | FplValueType.Proof 
            | FplValueType.RuleOfInference 
            | FplValueType.Extension 
            | FplValueType.Predicate 
            | FplValueType.FunctionalTerm 
            | FplValueType.Class -> 
                if fv.Scope.ContainsKey name then
                    ScopeSearchResult.Found (fv.Scope[name])
                elif fv.Parent.IsSome then 
                    if withNestedVariableSearch then 
                        match qualifiedVar fv with 
                        | ScopeSearchResult.NotFound ->
                            firstBlockParent fv.Parent.Value 
                        | s -> s
                    else
                        firstBlockParent fv.Parent.Value 
                else
                    ScopeSearchResult.NotFound
            | FplValueType.Theory ->
                ScopeSearchResult.NotFound
            | _ ->                         
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
        let root = new FplValue(FplValueType.Root, (Position("", 0, 1, 1), Position("", 0, 1, 1)), None)
        root.ReprId <- ""
        root.TypeId <- ""
        root.FplId <- ""
        root

    /// A factory method for the evaluation of FPL theories
    static member CreateTheory(positions: Positions, parent: FplValue, filePath:string) =
        let th = new FplValue(FplValueType.Theory, positions, Some parent)
        th.FilePath <- Some filePath
        th

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
        | FplValueType.Conjecture -> 
            let ret = new FplValue(fplBlockType, positions, Some parent)
            ret.ReprId <- "undetermined"
            ret.TypeId <- "pred"
            ret
        | FplValueType.Mapping -> 
            let ret = new FplValue(fplBlockType, positions, Some parent)
            ret
        | FplValueType.Constructor -> 
            let ret = new FplValue(fplBlockType, positions, Some parent)
            ret.ReprId <- "obj"
            ret.TypeId <- "pred"
            ret
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
        | FplValueType.Language
        | FplValueType.Translation
        | FplValueType.Stmt
        | FplValueType.Assertion
        | FplValueType.Extension
        | FplValueType.OptionalFunctionalTerm -> new FplValue(fplBlockType, positions, Some parent)
        | FplValueType.Class -> 
            let ret = new FplValue(fplBlockType, positions, Some parent)
            ret.ReprId <- "class"
            ret
        | FplValueType.Object -> 
            let ret = new FplValue(fplBlockType, positions, Some parent)
            ret.ReprId <- "obj"
            ret.TypeId <- "obj"
            ret.FplId <- "obj"
            ret
        | FplValueType.Root -> raise (ArgumentException("Please use CreateRoot for creating the root instead."))
        | FplValueType.Theory -> raise (ArgumentException("Please use CreateTheory for creating the theories instead."))

    /// A string representation of this FplValue
    override this.ToString() = 
        $"{this.BlockTypeShortName} {this.Type(SignatureType.Name)}"

    /// Create a (possibly empty) list of all variables in the scope of this FplValue.
    /// If the FplValue is itself a variable, it will be included in the list.
    member this.GetVariables() = 
        let rec collectVariables (fv: FplValue) =
            let mutable result = []
            if FplValue.IsVariable(fv) then
                result <- fv :: result
            for kvp in fv.Scope do
                result <- result @ collectVariables kvp.Value
            result
        collectVariables this

    /// Returns Some or none FplValue being the enclosing class block of a node inside a class.
    member this.GetClassBlock() = 
        let rec getClassBlock (fv:FplValue) = 
            match fv.BlockType with
            | FplValueType.Root -> None
            | FplValueType.Class -> Some fv
            | _ -> 
                match fv.Parent with
                | Some parent -> getClassBlock parent 
                | _ -> None
        getClassBlock this

    /// If this is a class definition, the function will return a list (possibly empty) list of all of its constructors.
    member this.GetConstructors() = 
        this.Scope
        |> Seq.map (fun kvp -> kvp.Value)
        |> Seq.filter (fun fv -> fv.BlockType = FplValueType.Constructor)
        |> Seq.toList

    /// If this is a definition, the function will return a (possibly empty) list of all of its properties.
    member this.GetProperties() = 
        this.Scope
        |> Seq.map (fun kvp -> kvp.Value)
        |> Seq.filter (fun fv -> fv.BlockType = FplValueType.MandatoryFunctionalTerm 
                                                || fv.BlockType = FplValueType.OptionalFunctionalTerm
                                                || fv.BlockType = FplValueType.MandatoryPredicate 
                                                || fv.BlockType = FplValueType.OptionalPredicate )
        |> Seq.toList

    /// Copies other FplValue to this one without changing its reference pointer.
    member this.Copy(other:FplValue) = 
        this.ReprId <- other.ReprId
        this.FplId <- other.FplId
        if other.IsSignatureVariable then 
            this.IsSignatureVariable <- other.IsSignatureVariable
        this.TypeId <- other.TypeId
        this.Arity <- other.Arity
        this.AuxiliaryInfo <- other.AuxiliaryInfo
        this.HasBrackets <- other.HasBrackets
        this.IsIntrinsic <- other.IsIntrinsic

    /// Clones this FplValue.
    member this.Clone() = 
        let rec recClone (fv:FplValue) =
            let ret = 
                match fv.BlockType with
                | FplValueType.Root -> 
                    FplValue.CreateRoot()
                | FplValueType.Theory ->
                    FplValue.CreateTheory((fv.NameStartPos, fv.NameEndPos),fv.Parent.Value, fv.FilePath.Value)
                | _ ->
                    FplValue.CreateFplValue((fv.NameStartPos, fv.NameEndPos),fv.BlockType, fv.Parent.Value)
            ret.ReprId <- fv.ReprId
            ret.FplId <- fv.FplId
            if fv.IsSignatureVariable then 
                ret.IsSignatureVariable <- fv.IsSignatureVariable
            ret.TypeId <- fv.TypeId
            ret.Arity <- fv.Arity
            ret.AuxiliaryInfo <- fv.AuxiliaryInfo
            ret.HasBrackets <- fv.HasBrackets
            ret.IsIntrinsic <- fv.IsIntrinsic
            ret.ExpressionType <- fv.ExpressionType
            fv.Scope 
            |> Seq.iter (fun kvp -> 
                let value = recClone kvp.Value
                ret.Scope.Add(kvp.Key, value)
            )
            fv.ValueList 
            |> Seq.iter (fun fv1 -> 
                let value = recClone fv1
                ret.ValueList.Add(value)
            )
            fv.AssertedPredicates
            |> Seq.iter (fun fv1 -> 
                // asserted predicates do not have to be cloned
                ret.ValueList.Add(fv1) 
            )
            ret
        recClone this

// Create an FplValue list containing all Scopes of an FplNode
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
                |> List.filter (fun fv -> fv.FplId = potentialProvableName)

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
                ScopeSearchResult.FoundMultiple (provableBlocklist |> List.map (fun fv -> sprintf "%s %s" fv.BlockType.Name (fv.Type(SignatureType.Mixed))) |> String.concat ", ")
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
                    stripLastDollarDigit (fplValue.Type(SignatureType.Mixed))
                flattenedScopes
                |> Seq.filter (fun fv -> fv.FplId = potentialBlockName)
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
                ScopeSearchResult.FoundMultiple (potentialBlockList |> List.map (fun fv -> sprintf "%s %s" fv.BlockType.Name (fv.Type(SignatureType.Mixed))) |> String.concat ", ")
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

/// Checks if the baseClassName is contained in the classRoot's base classes (it derives from).
/// If so, the function will produce Some path where path equals a string of base classes concatenated by ":".
let rec findClassInheritanceChain (classRoot: FplValue) (baseClassName: string) =
    let rootType = classRoot.Type(SignatureType.Type)
    if rootType = baseClassName then
        Some(rootType)
    else
        classRoot.ValueList
        |> Seq.collect (fun child ->
            match findClassInheritanceChain child baseClassName with
            | Some path -> [ rootType + ":" + path ]
            | None -> [])
        |> Seq.tryLast

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
        let mutable currentPath = ""
        let rec createJson (root:FplValue) (sb:StringBuilder) level isLast preventInfinite =
            match root.FilePath with
            | Some path -> currentPath <- path
            | _ -> ()
            let indent, indentMinusOne = 
                if _debug then 
                    String(' ', level), String(' ', level - 1)
                else
                    String.Empty, String.Empty

            sb.AppendLine(indentMinusOne + "{") |> ignore
            let name = root.Type(SignatureType.Name).Replace(@"\",@"\\")
            let fplTypeName = root.Type(SignatureType.Type).Replace(@"\",@"\\")
            let fplValueRepr = root.Type(SignatureType.Repr).Replace(@"\",@"\\")
            if name = this.MainTheory then
                sb.AppendLine($"{indent}\"Name\": \"(Main) {name}\",") |> ignore
            else
                sb.AppendLine($"{indent}\"Name\": \"{name}\",") |> ignore
            sb.AppendLine($"{indent}\"Type\": \"{root.BlockType.ShortName}\",") |> ignore
            sb.AppendLine($"{indent}\"FplValueType\": \"{fplTypeName}\",") |> ignore
            sb.AppendLine($"{indent}\"FplValueRepr\": \"{fplValueRepr}\",") |> ignore
            sb.AppendLine($"{indent}\"Line\": \"{root.NameStartPos.Line}\",") |> ignore
            sb.AppendLine($"{indent}\"Column\": \"{root.NameStartPos.Column}\",") |> ignore
            sb.AppendLine($"{indent}\"FilePath\": \"{currentPath}\",") |> ignore
            if preventInfinite then 
                sb.AppendLine($"{indent}\"Scope\": [],") |> ignore
                sb.AppendLine($"{indent}\"ValueList\": []") |> ignore
            else
                sb.AppendLine($"{indent}\"Scope\": [") |> ignore

                let mutable counterScope = 0
                root.Scope
                |> Seq.iter (fun child ->
                    counterScope <- counterScope + 1
                    createJson child.Value sb (level + 1) (counterScope = root.Scope.Count) (root.FplId = "self" || root.FplId = "parent")
                )
                sb.AppendLine($"{indent}],") |> ignore
                sb.AppendLine($"{indent}\"ValueList\": [") |> ignore

                let mutable valueList = 0
                root.ValueList
                |> Seq.iter (fun child ->
                    valueList <- valueList + 1
                    createJson child sb (level + 1) (valueList = root.ValueList.Count) false
                )
                sb.AppendLine($"{indent}]") |> ignore
            if isLast then
                sb.AppendLine(indentMinusOne + "}") |> ignore
            else
                sb.AppendLine(indentMinusOne + "},") |> ignore
        createJson this.Root sb 1 false false
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
            $"{theory.Value.Type(SignatureType.Mixed)} ({theory.Value.Scope.Count})"
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
let findCandidatesByName (st:SymbolTable) (name:string) withClassConstructors =
    let pm = List<FplValue>()
    st.Root.Scope // iterate all theories
    |> Seq.iter (fun theory -> 
        theory.Value.Scope
        // filter only blocks starting with the same FplId as the reference
        |> Seq.map (fun kvp -> kvp.Value) 
        |> Seq.filter (fun fv -> fv.FplId = name) 
        |> Seq.iter (fun (block:FplValue) -> 
            pm.Add(block)
            if withClassConstructors 
                && block.BlockType = FplValueType.Class then 
                block.Scope
                |> Seq.map (fun kvp -> kvp.Value) 
                |> Seq.filter (fun (fv:FplValue) -> fv.BlockType = FplValueType.Constructor)
                |> Seq.iter (fun (fv:FplValue) -> pm.Add(fv))
        ) 
    ) |> ignore
    pm |> Seq.toList


/// Looks for all declared properties or constructors (if any) that start with 
/// the specific name within the building block, whose syntax tree the FplValue `fv` is part of. 
let findCandidatesByNameInBlock (fv:FplValue) (name:string) = 
    let rec findDefinition (fv1:FplValue) =
        match fv1.BlockType with
        | FplValueType.Theory ->
            ScopeSearchResult.NotFound
        | FplValueType.Class
        | FplValueType.Predicate
        | FplValueType.FunctionalTerm ->
            ScopeSearchResult.Found (fv1)
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

let findCandidatesByNameInDotted (fv:FplValue) (name:string) = 
    let rec findQualifiedEntity (fv1:FplValue) =
        match fv1.BlockType with
        | FplValueType.Reference ->
            if fv1.Scope.ContainsKey(".") && fv1.Scope.Count>1 then 
                let result = 
                    fv1.Scope
                    |> Seq.filter (fun kvp -> kvp.Key <> ".")
                    |> Seq.map (fun kvp -> kvp.Value)
                    |> Seq.toList
                    |> List.head
                ScopeSearchResult.Found (result)
            else
                match fv1.Parent with 
                | Some parent -> findQualifiedEntity parent
                | None -> ScopeSearchResult.NotFound
        | _ -> ScopeSearchResult.NotFound
    match findQualifiedEntity fv with
    | ScopeSearchResult.Found candidate -> 
        match candidate.BlockType with
        | FplValueType.Variable -> 
            if candidate.ValueList.Count>0 then 
                let (varType:FplValue) = candidate.ValueList[0]
                varType.Scope
                |> Seq.filter (fun kvp -> kvp.Value.FplId = name)
                |> Seq.map (fun kvp -> kvp.Value)
                |> Seq.toList
            else    
                []
        | _ -> []    
    | _ -> []

/// Searches in the counter-th parent predecessor of fv and checks if it is a definition block.
/// Returns ScopeSearchResult.NotFound if no such block was reached.
/// Returns ScopeSearchResult.FoundIncorrectBlock if the reached block is not a definition block.
/// Returns ScopeSearchResult.Found if a definition block was reached.
let rec nextDefinition (fv:FplValue) counter = 
    let blocks = Stack<FplValue>()
    match fv.BlockType with 
    | FplValueType.Axiom
    | FplValueType.Theorem 
    | FplValueType.Lemma 
    | FplValueType.Proposition 
    | FplValueType.Corollary 
    | FplValueType.Conjecture 
    | FplValueType.Proof 
    | FplValueType.Localization 
    | FplValueType.RuleOfInference -> 
        let name = $"{fv.BlockType.Name} {fv.Type(SignatureType.Name)}"
        ScopeSearchResult.FoundIncorrectBlock name
    | FplValueType.Theory -> 
        let name = 
            if blocks.Count > 0 then 
                let fv1 = blocks.Peek()
                $"{fv1.BlockType.Name} {fv1.Type(SignatureType.Name)}"
            else
                "(no block found)"
        ScopeSearchResult.FoundIncorrectBlock name
    | FplValueType.MandatoryPredicate  
    | FplValueType.OptionalPredicate
    | FplValueType.MandatoryFunctionalTerm  
    | FplValueType.OptionalFunctionalTerm 
    | FplValueType.Predicate  
    | FplValueType.FunctionalTerm 
    | FplValueType.Class -> 
        blocks.Push(fv)
        if counter <= 0 then 
            ScopeSearchResult.Found fv
        else
            let next = fv.Parent
            match next with
            | Some parent ->
                nextDefinition parent (counter - 1)
            | None -> 
                let name = 
                    if blocks.Count > 0 then 
                        let fv1 = blocks.Peek()
                        $"{fv1.BlockType.Name} {fv1.Type(SignatureType.Name)}"
                    else
                        "(no block found)"
                ScopeSearchResult.FoundMultiple name
    | _ -> 
        let next = fv.Parent
        match next with
        | Some parent -> nextDefinition parent counter
        | None -> ScopeSearchResult.NotFound


let rec mpwa (args:FplValue list) (pars:FplValue list) = 
    match (args, pars) with
    | (a::ars, p::prs) -> 
        let aType = a.Type(SignatureType.Type)
        let pType = p.Type(SignatureType.Type)
        if aType = pType then
            mpwa ars prs
        elif pType.StartsWith("tpl") || pType.StartsWith("template") then
            mpwa ars prs
        elif pType = $"*{aType}" || pType.StartsWith("*") && aType = "???" then 
            if ars.Length > 0 then 
                mpwa ars pars
            else
                None
        elif pType.StartsWith("+") && aType = "???" then 
            Some($"() does not match `{p.Type(SignatureType.Name)}:{pType}`")
        elif pType = $"+{aType}" then 
            if ars.Length > 0 then 
                mpwa ars pars
            else
                None
        elif aType.Length > 0 && Char.IsUpper(aType[0]) && a.BlockType = FplValueType.Reference && a.Scope.Count = 1 then
            let var = a.Scope.Values |> Seq.toList |> List.head
            if var.ValueList.Count = 1 then 
                let cl = var.ValueList[0]
                match cl.BlockType with 
                | FplValueType.Class ->
                    let inheritanceList = findClassInheritanceChain cl pType
                    match inheritanceList with 
                    | Some str -> 
                        mpwa ars prs
                    | None -> 
                        Some($"`{a.Type(SignatureType.Name)}:{aType}` neither matches `{p.Type(SignatureType.Name)}:{pType}` nor the base classes")
                | _ -> 
                    // this case does not occur but for we cover it for completeness reasons
                    Some($"`{a.Type(SignatureType.Name)}:{aType}` is undefined and does'nt match `{p.Type(SignatureType.Name)}:{pType}`")
            else
                Some($"`{a.Type(SignatureType.Name)}:{aType}` is undefined and does not match `{p.Type(SignatureType.Name)}:{pType}`")
        elif aType.StartsWith(pType + "(") then
            None
        elif aType = "???" && pType <> "???" then
            Some($"`()` does not match `{p.Type(SignatureType.Name)}:{pType}`")
        elif aType.StartsWith("func") then 
            let someMap = a.Mapping
            match someMap with 
            | Some map -> mpwa [map] [p]
            | _ -> Some($"`{a.Type(SignatureType.Name)}:{aType}` does not match `{p.Type(SignatureType.Name)}:{pType}`")
        else
            Some($"`{a.Type(SignatureType.Name)}:{aType}` does not match `{p.Type(SignatureType.Name)}:{pType}`")
    | ([], p::prs) -> 
            let pType = p.Type(SignatureType.Type)
            Some($"missing argument for `{p.Type(SignatureType.Name)}:{pType}`")
    | (a::[], []) -> 
        if a.FplId = "???" then 
            None
        else
            let aType = a.Type(SignatureType.Type)
            Some($"no matching paramater for `{a.Type(SignatureType.Name)}:{aType}`")
    | (a::ars, []) -> 
            let aType = a.Type(SignatureType.Type)
            Some($"no matching paramater for `{a.Type(SignatureType.Name)}:{aType}`")
    | ([],[]) -> None

/// Tries to match the arguments of `fva` FplValue with the parameters of the `fvp` FplValue and returns 
/// Some(specific error message) or None, if the match succeeded.
let matchArgumentsWithParameters (fva:FplValue) (fvp:FplValue) =
    let parameters = 
        match fvp.BlockType with
        | FplValueType.Axiom
        | FplValueType.Theorem
        | FplValueType.Lemma
        | FplValueType.Proposition
        | FplValueType.Corollary
        | FplValueType.Predicate 
        | FplValueType.FunctionalTerm 
        | FplValueType.RuleOfInference
        | FplValueType.Conjecture
        | FplValueType.Constructor
        | FplValueType.MandatoryPredicate
        | FplValueType.MandatoryFunctionalTerm
        | FplValueType.OptionalPredicate
        | FplValueType.OptionalFunctionalTerm -> 
            fvp.Scope.Values |> Seq.filter (fun fv -> fv.IsSignatureVariable) |> Seq.toList
        | _ -> 
            fvp.Scope.Values |> Seq.toList
    let arguments = fva.ValueList |> Seq.toList
    
    let stdMsg = $"{fvp.QualifiedName}"
    let argResult = mpwa arguments parameters
    match argResult with 
    | Some aErr -> Some($"{aErr} in {stdMsg}")
    | None -> None

let matchWithMapping (fva:FplValue) (fvp:FplValue) =
    let targetMapping = fvp.Mapping
    match targetMapping with
    | Some tm -> mpwa [fva] [tm]
    | None -> Some($"Btest")

/// Tries to match the signatures of toBeMatched with the signatures of all candidates and accoumulates any 
/// error messages in accResultList.
let rec checkCandidates (toBeMatched: FplValue) (candidates: FplValue list) (accResultList:string list) = 
    match candidates with 
    | [] -> (None, accResultList)
    | candidate::candidates -> 
        match matchArgumentsWithParameters toBeMatched candidate with
        | None -> (Some candidate, [])
        | Some errMsg -> 
            checkCandidates toBeMatched candidates (accResultList @ [errMsg])

let rec filterTreePathByBlockType (leaf:FplValue) (typ:FplValueType) = 
    if leaf.BlockType = typ then 
        Some leaf
    else
        match leaf.Parent with
        | Some parent -> filterTreePathByBlockType parent typ
        | _-> None

let searchExtensionByName (root:FplValue) identifier = 
    let candidates =
        root.Scope
        |> Seq.map (fun theory ->
            theory.Value.Scope
            |> Seq.filter (fun kvp -> kvp.Value.BlockType = FplValueType.Extension)
            |> Seq.map (fun kvp -> 
            kvp.Value)
            |> Seq.filter (fun ext -> ext.FplId = identifier)
        )
        |> Seq.concat
        |> Seq.toList
    if candidates.Length = 0 then
        ScopeSearchResult.NotFound
    else 
        ScopeSearchResult.Found candidates.Head

/// Copy the variables and properties of a parent class into a derived class.
let copyParentToDerivedClass (parentClass:FplValue) (derivedClass:FplValue) =
    let shadowedVars = List<string>()
    let shadowedProperties = List<string>()
    let parentVariables = parentClass.GetVariables()
    parentVariables 
    |> List.iter (fun parentVar -> 
        if derivedClass.Scope.ContainsKey(parentVar.FplId) then 
            shadowedVars.Add(parentVar.FplId)
    )
    (shadowedVars,shadowedProperties)
