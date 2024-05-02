module FplInterpreterTypes

open System
open System.Text.RegularExpressions
open System.Security.Cryptography
open System.Collections.Generic
open System.Text
open System.IO
open FParsec
open Newtonsoft.Json
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
    static member CreateEani(uri:System.Uri) = 
        let pascalCaseId = Path.GetFileNameWithoutExtension(uri.LocalPath)
        let pos = Position(uri.LocalPath, 0, 1, 1)
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
type FplSources(paths: string list) =
    /// All found paths for a uses clause, including those from the web.
    member this.Paths = paths

    /// Returns all loaded FPL theories loaded by a uses clause grouped by name with lists of potential locations
    member this.Grouped 
        with get () = 
            let fplTheories = 
                this.Paths
                |> List.map (fun fp -> (Path.GetFileName fp, fp))
            fplTheories
            |> List.groupBy fst
    
    static member IsUrl(s: string) =
        let pattern = @"^https?://"
        Regex.IsMatch(s, pattern)

    static member IsFilePath(s: string) =
        try
            Path.GetFullPath(s) |> ignore
            let pattern = @"^https?://"
            not (Regex.IsMatch(s, pattern))
        with :? ArgumentException ->
            false

    /// Uri of this ParsingProperties
    static member EscapedUri(path:string) = 
        let pathNew = Uri.UnescapeDataString(path.Replace("\\","/"))
        if FplSources.IsFilePath(pathNew) then  
            Uri($"{pathNew}")
        else
            Uri($"fplregistry://{pathNew}")

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
                            if FplSources.IsFilePath(path) && not (path.Contains("/lib/") || path.Contains(@"\lib\")) then
                                true // the first source is the current directory
                            elif FplSources.IsFilePath(path) && (path.Contains("/lib/") || path.Contains(@"\lib\")) then 
                                true // the second is the lib subdirectory 
                            else
                                true // the third is the internet source
                        )

                    let pathTypes =
                        List.map snd paths
                        |> List.map (fun path -> 
                            if FplSources.IsFilePath(path) && (path.Contains("/lib/") || path.Contains(@"\lib\")) then
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
                            if FplSources.IsFilePath(path) && not (path.Contains("/lib/") || path.Contains(@"\lib\")) then
                                "./"
                            elif FplSources.IsFilePath(path) && (path.Contains("/lib/") || path.Contains(@"\lib\")) then 
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
    { mutable UriPath: string // source of the ast
      mutable FplSourceCode: string // source code of the ast
      mutable Ast: Ast // parsed ast
      mutable Checksum: string } // checksum of the parsed ast

    /// Reset this ParsingProperties to its new location
    member this.Reset (fplCode: string) (codeLoc: string) =
        let checksum = computeMD5Checksum fplCode

        if this.Checksum <> checksum then
            // if there ist a Parsed Ast with the same Name as the eani.Name
            // and its checksum differs from the previous checksum
            // then replace the ast, checksum, location, sourcecode, the
            this.UriPath <- codeLoc
            this.Ast <- fplParser (FplSources.EscapedUri(codeLoc)) fplCode
            this.FplSourceCode <- fplCode
            this.Checksum <- checksum
            true
        else
            false

    static member Create(fileLoc, fileContent) = 
        {
            ParsingProperties.UriPath = fileLoc
            ParsingProperties.FplSourceCode = fileContent
            ParsingProperties.Ast = FplParser.fplParser (FplSources.EscapedUri(fileLoc)) fileContent
            ParsingProperties.Checksum = computeMD5Checksum fileContent
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

type FplType =
    | Object 
    | Predicate 
    | FunctionalTerm 
    | Template 
    | Index
    | Localization

type FplBlockType =
    | Variable
    | VariadicVariableMany
    | VariadicVariableMany1
    | Expression
    | MandatoryPredicate
    | OptionalPredicate
    | MandatoryFunctionalTerm
    | OptionalFunctionalTerm
    | Constructor
    | Class 
    | Theorem
    | Localization
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
    member private this.UnqualifiedName = 
        match this with
            // parser error messages
            | Variable -> "variable"
            | VariadicVariableMany -> "zero-or-more variable"
            | VariadicVariableMany1 -> "one-or-more variable"
            | Expression -> "expression"
            | MandatoryPredicate -> "predicate property"
            | OptionalPredicate -> "optional predicate property"
            | MandatoryFunctionalTerm -> "functional term property"
            | OptionalFunctionalTerm -> "optional functional term property"
            | Constructor -> "constructor"
            | Class -> "class definition"
            | Localization -> "localization"
            | Theorem -> "theorem"
            | Lemma -> "lemma"
            | Proposition -> "proposition"
            | Corollary -> "corollary"
            | Proof -> "proof"
            | Conjecture -> "conjecture"
            | Axiom -> "axiom"
            | RuleOfInference -> "rule of inference"
            | Premise -> "premise"
            | Conclusion -> "conclusion"
            | Predicate -> "predicate definition"
            | FunctionalTerm -> "functional term definition"
            | Theory -> "theory"
            | Root -> "root"
    member private this.Article = 
        match this with
        | OptionalPredicate
        | OptionalFunctionalTerm
        | Expression 
        | Axiom -> "an"
        | _ -> "a"

    member this.Name = this.Article + " " + this.UnqualifiedName


type ScopeSearchResult = 
    | FoundAssociate of string 
    | FoundMultiple of string
    | FoundIncorrectBlock of string
    | Found of FplValue
    | NotFound
    | NotApplicable
and FplValue(name: string, blockType: FplBlockType, evalType: FplType, positions: Positions, parent: FplValue option) =
    let mutable _name = name
    let mutable _nameFinal = false
    let mutable _nameEndPos = Position("", 0, 1, 1)
    let mutable _evalType = evalType
    let mutable _typeSignature = []
    let mutable _representation = ""
    let mutable _blockType = blockType
    let mutable _auxiliaryInfo = 0
    let mutable _parent = parent
    let _auxiliaryUniqueChilds = HashSet<string>()
    let _scope = System.Collections.Generic.Dictionary<string, FplValue>()
    let _valueList = System.Collections.Generic.List<FplValue>()

    /// Identifier of this FplValue that is unique in its scope.
    member this.Name
        with get () = _name
        and set (value) = 
            if _nameFinal then 
                raise (ArgumentException($"Cannot set readonly Name {_name} again since it has been finally evaluated."))
            else
                _name <- value

    /// Indicates, if the Name has been finally determined during the evaluation process.
    /// If true, the Name property becomes immutable.
    member this.NameIsFinal
        with get () = _nameFinal
        and set (value) = 
            if _nameFinal then 
                raise (ArgumentException($"Cannot change the readonly NameIsFinal property since it has been finally evaluated."))
            else
                _nameFinal <- value

    /// Type of the FPL block within this FplValue
    member this.BlockType
        with get () = _blockType
        and set (value) = _blockType <- value

    /// Name of the FPL block type within this FplValue
    member this.BlockTypeName
        with get () = _blockType.Name

    /// This FplValue's name's end position that can be different from its endig position
    member this.NameEndPos
        with get () = _nameEndPos
        and set (value) = _nameEndPos <- value

    /// Signature of this FplValue, for instance "predicate(object)"
    member this.TypeSignature
        with get () = _typeSignature
        and set (value:string list) = _typeSignature <- value

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

    /// Am aixiliary storage that is used e.g. for remembering the names of already processed variables when traversing the Ast recursively.
    member this.AuxiliaryUniqueChilds = _auxiliaryUniqueChilds

    /// Starting position of this FplValue
    member this.StartPos = fst positions

    /// Ending position of this FplValue
    member this.EndPos = snd positions
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

    /// Indicates if this FplValue is an FPL building block.
    static member IsFplBlock(fplValue:FplValue) = 
        match fplValue.BlockType with 
        | FplBlockType.Axiom
        | FplBlockType.Theorem 
        | FplBlockType.Lemma 
        | FplBlockType.Proposition 
        | FplBlockType.Corollary 
        | FplBlockType.Conjecture 
        | FplBlockType.Proof 
        | FplBlockType.RuleOfInference 
        | FplBlockType.Predicate 
        | FplBlockType.FunctionalTerm 
        | FplBlockType.Class(_) 
        | FplBlockType.Localization -> true
        | _ -> false

    /// Indicates if this FplValue is a definition
    static member IsDefinition(fplValue:FplValue) = 
        match fplValue.BlockType with 
        | FplBlockType.Predicate 
        | FplBlockType.FunctionalTerm 
        | FplBlockType.Class(_) -> true
        | _ -> false

    /// Indicates if this FplValue is an root of the symbol table.
    static member IsRoot(fplValue:FplValue) = 
        fplValue.BlockType = FplBlockType.Root

    /// Qualified name of this FplValue 
    member this.QualifiedName
        with get () = 
            let rec getFullName (fplValue:FplValue) (first:bool) =
                if fplValue.BlockType = FplBlockType.Root then
                    ""
                elif first then 
                    if FplValue.IsRoot(fplValue.Parent.Value) then 
                        getFullName fplValue.Parent.Value false + fplValue.Name 
                    else
                        if FplValue.IsVariable(fplValue) && not (FplValue.IsVariable(fplValue.Parent.Value)) then
                            fplValue.Name
                        else
                            getFullName fplValue.Parent.Value false + "." + fplValue.Name 
                else
                    if FplValue.IsRoot(fplValue.Parent.Value) then 
                        getFullName fplValue.Parent.Value false + fplValue.Name 
                    else
                        if FplValue.IsVariable(fplValue) && not (FplValue.IsVariable(fplValue.Parent.Value)) then
                            fplValue.Name
                        else
                            getFullName fplValue.Parent.Value false + "." + fplValue.Name
            getFullName this true 

    /// Indicates if this FplValue is a constructor.
    static member IsConstructor(fplValue:FplValue) = 
        fplValue.BlockType = FplBlockType.Constructor

    /// Indicates if this FplValue is a class.
    static member IsClass(fplValue:FplValue) = 
        match fplValue.BlockType with
        | FplBlockType.Class (_) -> true
        | _ -> false

    /// Indicates if this FplValue is a proof.
    static member IsProof(fplValue:FplValue) = 
        fplValue.BlockType = FplBlockType.Proof

    /// Indicates if this FplValue is a corollary.
    static member IsCorollary(fplValue:FplValue) = 
        fplValue.BlockType = FplBlockType.Corollary

    /// Indicates if this FplValue is a property.
    static member IsProperty(fplValue:FplValue) = 
        fplValue.BlockType = FplBlockType.MandatoryFunctionalTerm
        || fplValue.BlockType = FplBlockType.OptionalFunctionalTerm
        || fplValue.BlockType = FplBlockType.MandatoryPredicate
        || fplValue.BlockType = FplBlockType.OptionalPredicate

    /// Indicates if this FplValue is a functional term.
    static member IsFunctionalTerm(fplValue:FplValue) = 
        fplValue.BlockType = FplBlockType.MandatoryFunctionalTerm
        || fplValue.BlockType = FplBlockType.OptionalFunctionalTerm
        || fplValue.BlockType = FplBlockType.FunctionalTerm

    /// Indicates if this FplValue is a constructor or a property
    static member IsConstructorOrProperty(fplValue:FplValue)  = 
        FplValue.IsConstructor(fplValue) || FplValue.IsProperty(fplValue)

    /// Indicates if this FplValue is a constructor or a property
    static member IsProofOrCorollary(fplValue:FplValue) = 
        FplValue.IsProof(fplValue) || FplValue.IsCorollary(fplValue)

    /// Indicates if this FplValue is a constructor or a theory
    static member IsTheory (fplValue:FplValue) = 
        fplValue.BlockType = FplBlockType.Theory

    /// Qualified starting position of this FplValue
    member this.QualifiedStartPos = 
        let rec getFullName (fplValue:FplValue) (first:bool) =
            if fplValue.BlockType = FplBlockType.Root then
                ""
            elif first then 
                if FplValue.IsRoot(fplValue.Parent.Value) then 
                    getFullName fplValue.Parent.Value false + fplValue.Name + fplValue.StartPos.ToString() 
                else
                    getFullName fplValue.Parent.Value false + fplValue.StartPos.ToString() 
            else
                if FplValue.IsRoot(fplValue.Parent.Value) then 
                    getFullName fplValue.Parent.Value false + fplValue.Name 
                else
                    getFullName fplValue.Parent.Value false 
        getFullName this true


    /// Indicates if this FplValue is a variable.
    static member IsVariable(fplValue:FplValue) = 
        fplValue.BlockType = FplBlockType.Variable
        || fplValue.BlockType = FplBlockType.VariadicVariableMany
        || fplValue.BlockType = FplBlockType.VariadicVariableMany1

    /// Indicates if this FplValue is a variadic * variable.
    static member IsVariadicVariableMany(fplValue:FplValue) = 
        fplValue.BlockType = FplBlockType.VariadicVariableMany

    /// Indicates if this FplValue is a variadic + variable.
    static member IsVariadicVariableMany1(fplValue:FplValue) = 
        fplValue.BlockType = FplBlockType.VariadicVariableMany1

    /// Checks if a block is in the scope of its parent 
    static member InScopeOfParent(fplValue:FplValue) name = 
        let conflictInSiblingTheory (parent:FplValue) = 
            // if the parent is as theory, look also for its sibling theories
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

    /// Checks if a variable defined in the scope of a constructor or a property
    /// was already defined in the scope of its parent definition. 
    static member ConstructorOrPropertyVariableInOuterScope(fplValue:FplValue) =
        if (FplValue.IsVariable(fplValue)) then 
            match fplValue.Parent with
            | Some parent ->
                if (FplValue.IsConstructorOrProperty(parent)) then 
                     if parent.Parent.Value.Scope.ContainsKey fplValue.Name then
                        ScopeSearchResult.Found (parent.Parent.Value.Scope[fplValue.Name])
                     else
                        ScopeSearchResult.NotFound
                else
                    ScopeSearchResult.NotApplicable
            | None -> ScopeSearchResult.NotApplicable
        else
            ScopeSearchResult.NotApplicable

    /// Checks if a variable defined in the scope of a proof
    /// was already defined in the scope of its parent definition. 
    static member ProofVariableInOuterScope(fplValue:FplValue) =
        if (FplValue.IsVariable(fplValue)) then 
            match fplValue.Parent with
            | Some parent ->
                if (FplValue.IsProofOrCorollary(parent)) then 
                     if parent.Parent.Value.Scope.ContainsKey fplValue.Name then
                        ScopeSearchResult.Found (parent.Parent.Value.Scope[fplValue.Name])
                     else
                        ScopeSearchResult.NotFound
                else
                    ScopeSearchResult.NotApplicable
            | None -> ScopeSearchResult.NotApplicable
        else
            ScopeSearchResult.NotApplicable
    
    /// Checks if a variable defined in the scope of a corollary 
    /// was already defined in the scope of its parent definition. 
    static member CorollaryVariableInOuterScope(fplValue:FplValue) =
        if (FplValue.IsVariable(fplValue)) then 
            match fplValue.Parent with
            | Some parent ->
                if (FplValue.IsProofOrCorollary(parent)) then 
                     if parent.Parent.Value.Scope.ContainsKey fplValue.Name then
                        ScopeSearchResult.Found (parent.Parent.Value.Scope[fplValue.Name])
                     else
                        ScopeSearchResult.NotFound
                else
                    ScopeSearchResult.NotApplicable
            | None -> ScopeSearchResult.NotApplicable
        else
            ScopeSearchResult.NotApplicable

    /// Checks if an fplValue is provable. This will only be true if 
    /// it is a theorem, a lemma, a proposition, or a corollary
    static member IsProvable(fplValue:FplValue) = 
        fplValue.BlockType = FplBlockType.Theorem
        || fplValue.BlockType = FplBlockType.Corollary
        || fplValue.BlockType = FplBlockType.Lemma
        || fplValue.BlockType = FplBlockType.Proposition

    /// Tries to find a therem-like statement for a proof 
    /// and returns different cases of ScopeSearchResult, depending on different semantical error situations. 
    static member TryFindAssociatedBlockForProof(fplValue:FplValue) = 
        if fplValue.BlockType = FplBlockType.Proof then
            match fplValue.Parent with
            | Some theory ->
                // The parent node of the proof is the theory. In its scope 
                // we should find the theorem we are looking for.
                let buildingBlocksMatchingDollarDigitNameList = 
                    // the potential block name of the proof is the 
                    // concatenated type signature of the name of the proof 
                    // without the last dollar digit
                    let potentialBlockName = 
                        let positionButLast = fplValue.TypeSignature.Length
                        if positionButLast < 0 then
                            failwith "Type signature of a proof to short."
                        else
                            fplValue.TypeSignature |> List.take (positionButLast - 1) |> String.concat ""
                    theory.Scope
                    |> Seq.filter (fun keyValuePair -> 
                        keyValuePair.Key.StartsWith(potentialBlockName + "(") || keyValuePair.Key = potentialBlockName
                    )
                    |> Seq.toList
                let potentialBlockList = 
                    buildingBlocksMatchingDollarDigitNameList
                    |> List.filter (fun keyValuePair ->
                        FplValue.IsProvable(keyValuePair.Value)
                    )
                let notPotentialBlockList = 
                    buildingBlocksMatchingDollarDigitNameList
                    |> List.filter (fun keyValuePair ->
                        not (FplValue.IsProvable(keyValuePair.Value))
                    )
                if potentialBlockList.Length > 1 then
                    ScopeSearchResult.FoundMultiple (potentialBlockList |> List.map (fun kv -> kv.Value.BlockType.Name + " " + kv.Value.Name) |> String.concat ", ")
                elif potentialBlockList.Length > 0 then 
                    let potentialTheorem = potentialBlockList.Head
                    ScopeSearchResult.FoundAssociate potentialTheorem.Value.Name
                elif notPotentialBlockList.Length > 0 then 
                    let potentialOther = notPotentialBlockList.Head
                    ScopeSearchResult.FoundIncorrectBlock potentialOther.Value.QualifiedName
                else
                    ScopeSearchResult.NotFound
            | None -> ScopeSearchResult.NotApplicable
        else
            ScopeSearchResult.NotApplicable

    /// Tries to find a therem-like statement, a conjecture, or an axiom for a corollary 
    /// and returns different cases of ScopeSearchResult, depending on different semantical error situations. 
    static member TryFindAssociatedBlockForCorollary(fplValue:FplValue) = 

        if fplValue.BlockType = FplBlockType.Corollary then
            match fplValue.Parent with
            | Some theory ->
                // The parent node of the proof is the theory. In its scope 
                // we should find the theorem we are looking for.
                let buildingBlocksMatchingDollarDigitNameList = 
                    // the potential theorem name of the corollary is the 
                    // concatenated type signature of the name of the corollary 
                    // without the last dollar digit
                    let potentialBlockName = 
                        try
                            let positionParenthesis = fplValue.TypeSignature |> List.findIndex ((=) "(")
                            if positionParenthesis >= 2 then
                                fplValue.TypeSignature |> List.take (positionParenthesis - 1) |> String.concat ""
                            else    
                                failwith "Corollary has a malformed type signature, could not find the opening parenthesis at least at the 2th position."
                        with ex -> raise (ArgumentException(ex.Message))
                    theory.Scope
                    |> Seq.filter (fun keyValuePair -> 
                        keyValuePair.Key.StartsWith(potentialBlockName + "(") || keyValuePair.Key = potentialBlockName 
                    )
                    |> Seq.toList
                let potentialBlockList = 
                    buildingBlocksMatchingDollarDigitNameList
                    |> List.filter (fun keyValuePair ->
                        FplValue.IsProvable(keyValuePair.Value) 
                        || keyValuePair.Value.BlockType = FplBlockType.Conjecture
                        || keyValuePair.Value.BlockType = FplBlockType.Axiom
                    )
                let notPotentialBlockList = 
                    buildingBlocksMatchingDollarDigitNameList
                    |> List.filter (fun keyValuePair ->
                        not (FplValue.IsProvable(keyValuePair.Value) 
                        || keyValuePair.Value.BlockType = FplBlockType.Conjecture
                        || keyValuePair.Value.BlockType = FplBlockType.Axiom)
                    )
                if potentialBlockList.Length > 1 then
                    ScopeSearchResult.FoundMultiple (potentialBlockList |> List.map (fun kv -> kv.Value.BlockType.Name + " " + kv.Value.Name) |> String.concat ", ")
                elif potentialBlockList.Length > 0 then 
                    let potentialTheorem = potentialBlockList.Head
                    ScopeSearchResult.FoundAssociate potentialTheorem.Value.Name
                elif notPotentialBlockList.Length > 0 then 
                    let potentialOther = notPotentialBlockList.Head
                    ScopeSearchResult.FoundIncorrectBlock potentialOther.Value.QualifiedName
                else
                    ScopeSearchResult.NotFound
            | None -> ScopeSearchResult.NotApplicable
        else
            ScopeSearchResult.NotApplicable

    /// A factory method for the evaluation of FPL theories
    static member CreateRoot() =
        let root = new FplValue("", FplBlockType.Root, FplType.Object, (Position("", 0, 1, 1), Position("", 0, 1, 1)), None)
        root.NameIsFinal <- true
        root

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
        | FplBlockType.MandatoryPredicate
        | FplBlockType.OptionalPredicate
        | FplBlockType.Predicate -> new FplValue("", fplBlockType, FplType.Predicate, positions, Some parent)
        | FplBlockType.Constructor
        | FplBlockType.FunctionalTerm
        | FplBlockType.Variable
        | FplBlockType.VariadicVariableMany
        | FplBlockType.VariadicVariableMany1
        | FplBlockType.MandatoryFunctionalTerm
        | FplBlockType.OptionalFunctionalTerm
        | FplBlockType.Class(_) -> new FplValue("", fplBlockType, FplType.Object, positions, Some parent)
        | FplBlockType.Root -> raise (ArgumentException("Please use CreateRoot for creating the root instead"))
        | FplBlockType.Localization -> new FplValue("", fplBlockType, FplType.Localization, positions, Some parent)

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

 
type EvalContext =
    | ContextNone
    | InTheory of FplValue
    | InSignature of FplValue
    | InBlock of FplValue
    | InPropertySignature of FplValue
    | InPropertyBlock of FplValue
    | InConstructorSignature of FplValue
    | InConstructorBlock of FplValue
    | NamedVarDeclarationInBlock of FplValue

type SymbolTable(parsedAsts:ParsedAstList, debug:bool) =
    let _parsedAsts = parsedAsts
    let mutable _currentContext = EvalContext.ContextNone
    let _evalPath = Stack<string>()
    let _root = FplValue.CreateRoot()
    let _debug = debug

    /// Sets and gets the current evaluation context.
    member this.CurrentContext
        with get () = _currentContext
        and set (value) = _currentContext <- value

    /// Returns the evaluation root node of the symbol table.
    member this.Root = _root

    /// Returns the list of parsed asts
    member this.ParsedAsts = _parsedAsts

    /// Returns the path of the current recursive evaluation. The path is reversed, i.e. starting with the root ast name.
    member this.EvalPath() = 
        _evalPath 
        |> Seq.toList 
        |> List.rev 
        |> String.concat "."

    /// Add the current ast name to the recursive evaluation path.
    member this.EvalPush(astName:string) = 
        _evalPath.Push(astName)
        if debug then
            let path = this.EvalPath()
            System.Console.WriteLine(path)
            if path = "Error" then 
                raise(Exception("Test error message"))

    /// Remove the current ast name from the recursive evaluation path.
    member this.EvalPop() = 
        _evalPath.Pop() |> ignore


    /// If there is a valid topological sorting, order the list descending by this ordering.
    member this.OrderAsts() =
        _parsedAsts.Sort(
            Comparer<ParsedAst>.Create(fun b a -> compare a.Sorting.TopologicalSorting b.Sorting.TopologicalSorting)
        )

    /// serializes the symbol table as json
    member this.ToJson() = 
        let sb = StringBuilder()
        let rec createJson (root:FplValue) (sb:StringBuilder) level isLast =
            let indent = String(' ', level)
            sb.AppendLine(String(' ', level - 1) + "{") |> ignore
            sb.AppendLine($"{indent}\"Name\": \"{root.Name}\",") |> ignore
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
