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
open FplInterpreterDiagnosticsEmitterPre

(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)

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

type SignatureType =
    | Name
    | Type
    | Mixed

/// Maximum number of calls allowed for an Fpl Node
let maxRecursion = 5

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
    let mutable _hasBrackets = false
    let mutable _isIntrinsic = false
    let mutable _isInitializedVariable = false
    let mutable _isSignatureVariable = false

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
    
    /// Adds this FplValue to its parent's ArgList, if such a Parent exists.
    abstract member TryAddToParentsArgList: unit -> unit

    /// Adds this FplValue to its parent's Scope, if such a Parent exists
    abstract member TryAddToParentsScope: unit -> unit

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

    /// Indicates if this FplValue is a variable or some variadic variable.
    abstract member IsVariable: unit -> bool

    /// Indicates if this FplValue is a variable or some variadic variable.
    abstract member IsVariadic: unit -> bool

    /// Indicates if this FplValue is a mapping.
    abstract member IsMapping: unit -> bool

    /// Clears the ValueList and adds the argument to it. Previous value(s), if any, get lost.
    abstract member SetValue: FplValue -> unit

    /// Clears the ValueList and adds the argument to it. Previous value(s), if any, get lost.
    abstract member SetValuesOf: FplValue -> unit

    (* Default implementations = everything is false, only the trues are overridden in derived classes *)
    override this.SetValue fv =
        this.ValueList.Clear()
        this.ValueList.Add(fv)

    override this.SetValuesOf fv =
        this.ValueList.Clear()
        if fv.ValueList.Count = 0 then
            // if fv has no values then it should be the value itself
            this.ValueList.Add(fv)
        else
            this.ValueList.AddRange(fv.ValueList)

    override this.IsFplBlock () = false
    override this.IsBlock () = false
    override this.IsClass () = false
    override this.IsProof () = false
    override this.IsVariable () = false
    override this.IsVariadic () = false
    override this.IsMapping () = false
    
    override this.AssignParts (ret:FplValue) =
        ret.FplId <- this.FplId

        if this.IsSignatureVariable then
            ret.IsSignatureVariable <- this.IsSignatureVariable

        ret.TypeId <- this.TypeId
        ret.Arity <- this.Arity
        ret.AuxiliaryInfo <- this.AuxiliaryInfo
        ret.HasBrackets <- this.HasBrackets
        ret.IsIntrinsic <- this.IsIntrinsic
        ret.ExpressionType <- this.ExpressionType
        ret.IsInitializedVariable <- this.IsInitializedVariable

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

    /// Indicates if this FplValue's Scope or ArgList can be treated as bracketed coordinates or as parenthesized parameters.
    member this.HasBrackets
        with get () = _hasBrackets
        and set (value) = _hasBrackets <- value

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

    /// Indicates if this FplValue is a variable declared in the signature (true) or in the block (false).
    member this.IsSignatureVariable
        with get () =
            if this.IsVariable() then
                _isSignatureVariable
            else
                false
        and set (value) =
            if this.IsVariable() then
                _isSignatureVariable <- value
            else
                raise (
                    ArgumentException(
                        sprintf "Cannot set IsSignatureVariable for non-variable %s" this.ShortName
                    )
                )

    /// Starting position of this FplValue
    member this.StartPos
        with get () = _startPos
        and set (value) = _startPos <- value

    /// This FplValue's name's end position that can be different from its endig position
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

    /// Indicates if this FplValue is an initialized variable
    member this.IsInitializedVariable
        with get () = _isInitializedVariable
        and set (value) = _isInitializedVariable <- value

    /// Indicates if this FplValue is an intrinsically defined block
    member this.IsIntrinsic
        with get () = _isIntrinsic
        and set (value) = _isIntrinsic <- value

    /// Create a (possibly empty) list of all variables in the scope of this FplValue.
    /// If the FplValue is itself a variable, it will be included in the list.
    member this.GetVariables() =
        let rec collectVariables (fv: FplValue) =
            let mutable result = []

            if fv.IsVariable() then
                result <- fv :: result

            for kvp in fv.Scope do
                result <- result @ collectVariables kvp.Value

            result

        collectVariables this

    /// Copies other FplValue to this one without changing its reference pointer.
    member this.Copy(other: FplValue) =
        this.FplId <- other.FplId

        if other.IsSignatureVariable then
            this.IsSignatureVariable <- other.IsSignatureVariable

        this.TypeId <- other.TypeId
        this.Arity <- other.Arity
        this.AuxiliaryInfo <- other.AuxiliaryInfo
        this.HasBrackets <- other.HasBrackets
        this.IsIntrinsic <- other.IsIntrinsic
        this.IsInitializedVariable <- other.IsInitializedVariable

        this.Scope.Clear()
        other.Scope |> Seq.iter (fun kvp -> this.Scope.Add(kvp.Key, kvp.Value))

        this.ArgList.Clear()
        this.ArgList.AddRange(other.ArgList)

        this.ValueList.Clear()
        this.ValueList.AddRange(other.ValueList)

    override this.TryAddToParentsArgList () = 
        match this.Parent with 
        | Some parent -> parent.ArgList.Add(this)
        | _ -> ()
              

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

    /// Adds the FplValue to it's parent's Scope.
    override this.TryAddToParentsScope () = 
        let next = this.Parent.Value
        let identifier = 
            if this.IsBlock() then 
                this.Type(SignatureType.Mixed)
            elif this.IsVariable() then 
                this.FplId
            else
                this.Type(SignatureType.Name)
        match this.InScopeOfParent identifier with
        | ScopeSearchResult.Found conflict -> 
            match this.ShortName with
            | PrimVariable 
            | PrimVariableMany 
            | PrimVariableMany1 -> // variable
                ()
            | _ ->
                emitID001Diagnostics (this.Type(SignatureType.Type)) conflict.QualifiedStartPos this.StartPos this.EndPos 
        | _ -> 
            next.Scope.Add(identifier,this)

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
    let _classCounters = Dictionary<string,FplValue option>()
    let _stack = Stack<KeyValuePair<string, Dictionary<string,FplValue>>>()
    let _valueStack = Stack<FplValue>()
    let _assumedArguments = Stack<FplValue>()

    let mutable _nextRunOrder = 0
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

    /// In the context of a class being evaluated, this dictionary provides a dictionary
    /// of potential calls of parent classes (=Key). The optional FplValue values become some values
    /// if the a particular call was found. 
    /// This dictionary is used to emit ID020/ID021 diagnostics. If a class A inherits from class B but doesn't call its base constructor
    /// ID020 diagnostics will be emitted. If a class A inherits from class B and calls its base constructor more than once
    /// ID021 diagnostics will be emitted.
    member this.ParentClassCalls = _classCounters
    
    /// Resets the counters of th ID020 diagnostics evaluation.
    member this.ParentClassCountersInitialize() = 
        _classCounters |> Seq.iter (fun kvp -> _classCounters[kvp.Key] <- None)
        
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
                let fvClone = fv.Clone()
                p.ValueList.Add(fvClone)
                p.IsInitializedVariable <- true
            )

        let rec replace (pars:FplValue list) (args: FplValue list) = 
            match (pars, args) with
            | (p::ps, ar::ars) ->
                match p.IsVariadic(), ar.IsVariadic() with
                // p is variadic, ar is variadic 
                | true, true ->
                    replaceValues p ar
                    // continue replacing variables with the remaining lists
                    replace ps ars
                // p is variadic, ar is anything
                | true, _ ->
                    replaceValues p ar              
                    // continue replacing variables with the original pars and the remaining ars list
                    replace pars ars
                // p is not variadic, ar is variadic 
                | false, true -> ()
                 // p is not variadic, ar is anything but variadic 
                | false, _ ->
                    // otherwise, simply assign the argument's representation to the parameter's representation
                    replaceValues p ar
                    // continue replacing variables with the remaining lists
                    replace ps ars
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
        called.Scope
        |> Seq.filter (fun (kvp:KeyValuePair<string,FplValue>) -> kvp.Value.IsVariable()) 
        |> Seq.iter (fun paramKvp -> 
            // save the clone of the original parameter variable
            let parOriginal = paramKvp.Value
            let parClone = parOriginal.Clone()
            toBeSavedScopeVariables.Add(paramKvp.Key, parClone)
            if paramKvp.Value.IsSignatureVariable then 
                pars.Add(parOriginal)
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
        _classCounters.Clear()
    
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
let private getParamTuple (fv:FplValue)  (signatureType:SignatureType) =
        let propagate = propagateSignatureType signatureType
        fv.Scope
        |> Seq.filter (fun (kvp: KeyValuePair<string, FplValue>) ->
            kvp.Value.IsSignatureVariable
            || fv.IsVariable() && not (kvp.Value.IsClass())
            || fv.IsMapping())
        |> Seq.map (fun (kvp: KeyValuePair<string, FplValue>) -> kvp.Value.Type(propagate))
        |> String.concat ", "

type FplTheory(positions: Positions, parent: FplValue, filePath: string, runOrder) as this =
    inherit FplValue(positions, Some parent)
    let _runOrder = runOrder

    do
        this.FilePath <- Some filePath

    override this.Name = PrimTheoryL
    override this.ShortName = PrimTheory

    override this.Clone () =
        let ret = new FplTheory((this.StartPos, this.EndPos), this.Parent.Value, this.FilePath.Value, _runOrder)
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

    override this.EmbedInSymbolTable _ = this.TryAddToParentsScope()

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
    override this.TryAddToParentsArgList () = () 

    override this.EmbedInSymbolTable _ = () 

    /// Returns all theories in the scope of this root ordered by their discovery time (parsing of the AST).
    /// This means that the theory with the lowest RunOrder comes first.
    member private this.OrderedTheories =
        this.Scope.Values
        |> Seq.choose (fun item ->
            match item with
            | :? FplTheory as theory -> Some theory
            | _ -> None)
        |> Seq.sortBy (fun th -> th.RunOrder.Value) 

    override this.RunOrder = None

    override this.Run variableStack = 
        this.OrderedTheories
        |> Seq.iter (fun theory -> theory.Run variableStack)        


/// Indicates if an FplValue is the root of the SymbolTable.
let isRoot (fv:FplValue) = 
    match fv with
    | :? FplRoot -> true
    | _ -> false



[<AbstractClass>]
type FplGenericPredicate(positions: Positions, parent: FplValue) as this =
    inherit FplValue(positions, Some parent)
    do 
        this.FplId <- LiteralUndetermined
        this.TypeId <- LiteralPred

    override this.Represent () =
        this.ValueList
        |> Seq.map (fun subfv -> subfv.Represent())
        |> String.concat ", "

    override this.EmbedInSymbolTable nextOpt = 
        match nextOpt with 
        | Some next when next.Name = PrimJustificationL -> 
            this.TryAddToParentsScope()
        | Some next when next.Name = LiteralLocL -> 
            next.FplId <- this.FplId
            next.TypeId <- this.TypeId
            next.EndPos <- this.EndPos
        | Some next when next.IsBlock() || next.Name = PrimArgL ->
            this.TryAddToParentsArgList() 
        | Some next -> 
            this.TryAddToParentsArgList()
            next.EndPos <- this.EndPos
        | _ -> ()

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

    override this.Run _ = ()

    override this.EmbedInSymbolTable _ = this.TryAddToParentsArgList() 


type IHasSignature =
    abstract member SignStartPos : Position with get, set
    abstract member SignEndPos : Position with get, set

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
            
[<AbstractClass>]
type FplGenericObject(positions: Positions, parent: FplValue) as this =
    inherit FplValue(positions, Some parent)

    do
        this.FplId <- LiteralObj
        this.TypeId <- LiteralObj

    override this.RunOrder = None

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
        ()

    override this.EmbedInSymbolTable _ = this.TryAddToParentsArgList() 

    override this.RunOrder = Some _runOrder

type FplRuleOfInference(positions: Positions, parent: FplValue, runOrder) =
    inherit FplGenericPredicateWithExpression(positions, parent)
    let _runOrder = runOrder

    override this.Name = $"rule of {LiteralInfL}"
    override this.ShortName = LiteralInf

    override this.Clone () =
        let ret = new FplRuleOfInference((this.StartPos, this.EndPos), this.Parent.Value, _runOrder)
        this.AssignParts(ret)
        ret

    override this.IsFplBlock () = true
    override this.IsBlock () = true    

    override this.Run variableStack = 
        // todo implement run
        emitLG004diagnostic this.Name this.Arity this.StartPos this.EndPos

    override this.EmbedInSymbolTable _ = this.TryAddToParentsScope() 

    override this.RunOrder = Some _runOrder

type FplInstance(positions: Positions, parent: FplValue) =
    inherit FplGenericObject(positions, parent)

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
        let subRepr = 
            this.ValueList
            |> Seq.map (fun subfv -> subfv.Represent())
            |> String.concat ", "
        if subRepr = String.Empty then 
            LiteralUndef
        else
            subRepr

    override this.Run _ = ()

    override this.EmbedInSymbolTable _ = this.TryAddToParentsArgList() 

type FplConstructor(positions: Positions, parent: FplValue) =
    inherit FplGenericObject(positions, parent)
    let mutable _signStartPos = Position("", 0L, 0L, 0L)
    let mutable _signEndPos = Position("", 0L, 0L, 0L)

    interface IHasSignature with
        member this.SignStartPos 
            with get (): Position = _signStartPos
            and set (value) = _signStartPos <- value
        member this.SignEndPos 
            with get (): Position = _signEndPos
            and set (value) = _signEndPos <- value

    override this.Name = LiteralCtorL
    override this.ShortName = LiteralCtor

    override this.Clone () =
        let ret = new FplConstructor((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.IsBlock () = true

    override this.Type signatureType =
        let head = getFplHead this signatureType
        let paramT = getParamTuple this signatureType
        sprintf "%s(%s)" head paramT

    override this.Represent () = this.Type(SignatureType.Mixed)

    override this.Run _ = 
        this.SetValue(new FplInstance((this.StartPos, this.EndPos), this))

    override this.EmbedInSymbolTable _ = this.TryAddToParentsScope() 

    member this.ParentClass = this.Parent.Value :?> FplClass

and FplClass(positions: Positions, parent: FplValue) =
    inherit FplGenericObject(positions, parent)
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

    override this.Run _ = 
        this.SetValue(new FplInstance((this.StartPos, this.EndPos), this))


    override this.EmbedInSymbolTable _ = this.TryAddToParentsScope() 

    override this.RunOrder = None


/// Returns Some or none FplValue being the enclosing class block of a node inside a class.
let rec getClassBlock (fv: FplValue) =
    match fv with
    | :? FplClass -> Some fv
    | _ ->
        match fv.Parent with
        | Some parent -> getClassBlock parent
        | _ -> None

type FplIntrinsicObj(positions: Positions, parent: FplValue) =
    inherit FplGenericObject(positions, parent)

    override this.Name = PrimIntrinsicObj
    override this.ShortName = LiteralObj

    override this.Clone () =
        let ret = new FplIntrinsicObj((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type (signatureType:SignatureType) = 
        match signatureType with
            | SignatureType.Name 
            | SignatureType.Mixed -> this.FplId
            | SignatureType.Type -> this.TypeId

    override this.Represent (): string = this.FplId

    override this.Run variableStack = 
        this.SetValue (new FplInstance((this.StartPos, this.EndPos), this.Parent.Value))

    override this.EmbedInSymbolTable _ = this.TryAddToParentsArgList() 

let isIntrinsicObj (fv1:FplValue) = 
    match fv1 with
    | :? FplIntrinsicObj -> true
    | _ -> false

/// Checks if the baseClassName is contained in the classRoot's base classes (it derives from).
/// If so, the function will produce Some path where path equals a string of base classes concatenated by ":".
/// The classRoot is required to have an FplValueType.Class.
let rec findClassInheritanceChain (classRoot: FplValue) (baseClassName: string) =
    let rootType = classRoot.Type(SignatureType.Type)

    match classRoot with
    | :? FplClass
    | :? FplIntrinsicObj ->
        if rootType = baseClassName then
            Some(rootType)
        else
            classRoot.ArgList
            |> Seq.collect (fun child ->
                match findClassInheritanceChain child baseClassName with
                | Some path -> [ rootType + ":" + path ]
                | None -> [])
            |> Seq.tryLast
    | _ -> 
        None

type ICanBeCalledRecusively =
    abstract member CallCounter : int

type IReady =
    abstract member IsReady : bool

type IHasProof =
    abstract member HasProof : bool with get, set

type FplPredicate(positions: Positions, parent: FplValue, runOrder) =
    inherit FplGenericPredicateWithExpression(positions, parent)
    let _runOrder = runOrder
    let mutable _isReady = false
    let mutable _callCounter = 0

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

    override this.Type signatureType = 
        let head = getFplHead this signatureType

        let paramT = getParamTuple this signatureType
        sprintf "%s(%s)" head paramT

    override this.IsFplBlock () = true
    override this.IsBlock () = true

    override this.EmbedInSymbolTable _ = this.TryAddToParentsScope() 

    override this.Run variableStack = 
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
                _isReady <- this.Arity = 0 

    override this.RunOrder = Some _runOrder

type FplMandatoryPredicate(positions: Positions, parent: FplValue) =
    inherit FplGenericPredicateWithExpression(positions, parent)

    override this.Name = PrimMandatoryPredicateL
    override this.ShortName = PrimMandatoryPredicate

    override this.Clone () =
        let ret = new FplMandatoryPredicate((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.IsBlock () = true

    override this.EmbedInSymbolTable _ = this.TryAddToParentsScope() 

    override this.Run variableStack = 
        // todo implement run
        ()

    override this.Type signatureType = 
        let head = getFplHead this signatureType

        let paramT = getParamTuple this signatureType
        sprintf "%s(%s)" head paramT

type FplOptionalPredicate(positions: Positions, parent: FplValue) =
    inherit FplGenericPredicateWithExpression(positions, parent)

    override this.Name = PrimOptionalPredicateL
    override this.ShortName = PrimOptionalPredicate

    override this.Clone () =
        let ret = new FplOptionalPredicate((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.IsBlock () = true

    override this.Type signatureType = 
        let head = getFplHead this signatureType

        let paramT = getParamTuple this signatureType
        sprintf "%s(%s)" head paramT

    override this.EmbedInSymbolTable _ = this.TryAddToParentsScope() 

    override this.Run variableStack = 
        // todo implement run
        ()


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

    override this.EmbedInSymbolTable _ = this.TryAddToParentsScope() 

    override this.Run variableStack = 
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

    override this.EmbedInSymbolTable _ = this.TryAddToParentsScope() 

    override this.Run variableStack = 
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

    override this.EmbedInSymbolTable _ = this.TryAddToParentsScope() 

    override this.Run variableStack = 
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

    override this.EmbedInSymbolTable _ = this.TryAddToParentsArgList() 

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
            
        this.TryAddToParentsArgList() 

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

and FplJustificationItemByDef(positions: Positions, parent: FplValue) =
    inherit FplGenericJustificationItem(positions, parent)

    override this.Name = PrimJIByDef

    override this.Clone () =
        let ret = new FplJustificationItemByDef((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Run variableStack = () // todo implement Run

and FplJustificationItemByDefVar(positions: Positions, parent: FplValue) =
    inherit FplGenericJustificationItem(positions, parent)

    override this.Name = PrimJIByDefVar

    override this.Clone () =
        let ret = new FplJustificationItemByDefVar((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Run variableStack = () // todo implement Run

and FplJustificationItemByRefArgument(positions: Positions, parent: FplValue) =
    inherit FplGenericJustificationItem(positions, parent)

    override this.Name = PrimJIByRefArgument

    override this.Clone () =
        let ret = new FplJustificationItemByRefArgument((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Run variableStack = () // todo implement Run

and FplJustificationItemByProofArgument(positions: Positions, parent: FplValue) =
    inherit FplGenericJustificationItem(positions, parent)

    override this.Name = PrimJIByProofArgument

    override this.Clone () =
        let ret = new FplJustificationItemByProofArgument((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Run variableStack = () // todo implement Run

and FplJustificationItemByAx(positions: Positions, parent: FplValue) =
    inherit FplGenericJustificationItem(positions, parent)

    override this.Name = PrimJIByAx

    override this.Clone () =
        let ret = new FplJustificationItemByAx((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Run variableStack = () // todo implement Run

and FplJustificationItemByInf(positions: Positions, parent: FplValue) =
    inherit FplGenericJustificationItem(positions, parent)

    override this.Name = PrimJIByInf

    override this.Clone () =
        let ret = new FplJustificationItemByInf((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Run variableStack = () // todo implement Run

and FplJustificationItemByTheoremLikeStmt(positions: Positions, parent: FplValue) =
    inherit FplGenericJustificationItem(positions, parent)

    override this.Name = PrimJIByTheoremLikeStmt

    override this.Clone () =
        let ret = new FplJustificationItemByTheoremLikeStmt((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Run variableStack = () // todo implement Run

and FplJustificationItemByCor(positions: Positions, parent: FplValue) =
    inherit FplGenericJustificationItem(positions, parent)

    override this.Name = PrimJIByCor

    override this.Clone () =
        let ret = new FplJustificationItemByCor((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Run variableStack = () // todo implement Run

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
        // todo implement run
        ()

    member this.GetOrderedJustificationItems =
        this.Scope.Values
            |> Seq.sortBy (fun fv -> fv.RunOrder)
            |> Seq.map (fun fv -> fv :?> FplGenericJustificationItem)
            |> Seq.toList

    override this.EmbedInSymbolTable _ = this.TryAddToParentsArgList() 

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
        // todo implement run
        ()

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
        // todo implement run
        ()

    member this.ParentArgument = this.Parent.Value :?> FplArgument

and FplArgInferenceTrivial(positions: Positions, parent: FplValue) =
    inherit FplGenericArgInference(positions, parent)

    override this.Name = PrimArgInfTrivial
    override this.ShortName = PrimArgInf

    override this.Clone () =
        let ret = new FplArgInferenceTrivial((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Run _ = 
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
        ()

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

    override this.EmbedInSymbolTable _ = this.TryAddToParentsScope() 

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
            |> Seq.filter (fun (kvp: KeyValuePair<string, FplValue>) -> kvp.Value.IsVariable())
            |> Seq.map (fun (kvp: KeyValuePair<string, FplValue>) -> kvp.Value.Type(signatureType))
            |> String.concat ", "

        match paramT with
        | "" -> head
        | _ -> sprintf "%s(%s)" head paramT

    override this.Represent() = this.Type(SignatureType.Name)
        
    override this.IsBlock() = true

    override this.Run variableStack = 
        // todo implement run
        ()

    override this.RunOrder = None

    override this.EmbedInSymbolTable _ = this.TryAddToParentsScope() 

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
            |> String.concat ""

        sprintf "%s%s" head args

    override this.Represent () = this.FplId 

    override this.Run variableStack = 
        // todo implement run
        ()

    override this.EmbedInSymbolTable _ = this.TryAddToParentsArgList() 

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
        ()

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
        ()

    override this.EmbedInSymbolTable _ = this.TryAddToParentsArgList() 

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

    override this.Run _ = ()

    override this.EmbedInSymbolTable _ = this.TryAddToParentsArgList() 

    override this.RunOrder = None

type FplReference(positions: Positions, parent: FplValue) =
    inherit FplValue(positions, Some parent)

    override this.Name = PrimRefL
    override this.ShortName = PrimRef

    override this.Clone () = this // do not clone references to prevent stack overflow 

    override this.SetValue fv = 
        if this.Scope.ContainsKey(this.FplId) then
            let var = this.Scope[this.FplId]
            var.SetValue(fv)
        else
            base.SetValue(fv)

    override this.Type signatureType =
        if this.Scope.ContainsKey(this.FplId) then
            // delegate the type identifier to the referenced entity
            let val1 = this.Scope[this.FplId]
            val1.Type(signatureType)
        else
            let head = getFplHead this signatureType
            let propagate = propagateSignatureType signatureType

            let qualification =
                if this.Scope.ContainsKey(".") then
                    Some(this.Scope["."])
                else
                    None
            // The arguments are reserved for the arguments or the coordinates of the reference
            // If the argument tuple equals "???", an empty argument or coordinates list has occurred
            let args =
                this.ArgList
                |> Seq.map (fun fv -> fv.Type(propagate))
                |> String.concat ", "

            match (head, args, qualification) with
            | (_, "", Some qual) -> sprintf "%s.%s" head (qual.Type(propagate))
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
            | ("???", _, None) -> sprintf "%s" head
            | ("", _, None) -> sprintf "%s" args
            | (_, "", None) -> sprintf "%s" head
            | (_, "???", None) ->
                if this.HasBrackets then
                    sprintf "%s[]" head
                else
                    sprintf "%s()" head
            | (_, _, None) ->
                if this.HasBrackets then sprintf "%s[%s]" head args
                else sprintf "%s(%s)" head args


    override this.Represent () = 
        if this.ValueList.Count = 0 then 
            if this.Scope.ContainsKey(this.FplId) && this.Scope[this.FplId].IsVariable() then
                this.Scope[this.FplId].Represent()
            else
                let args = 
                    this.ArgList
                    |> Seq.map (fun arg -> arg.Represent())
                    |> String.concat ", "

                let qualification =
                    if this.Scope.ContainsKey(".") then
                        Some(this.Scope["."])
                    else
                        None

                match (this.FplId, args, qualification) with
                | (_, "", Some qual) -> sprintf "%s.%s" LiteralUndef (qual.Represent())
                | (_, "???", Some qual) ->
                    if this.HasBrackets then
                        sprintf "%s[].%s" LiteralUndef (qual.Represent())
                    else
                        sprintf "%s().%s" LiteralUndef (qual.Represent())
                | (_, _, Some qual) ->
                    if this.HasBrackets then
                        sprintf "%s[%s].%s" LiteralUndef args (qual.Represent())
                    else
                        sprintf "%s(%s).%s" LiteralUndef args (qual.Represent())
                | ("???", _, None) -> "()" 
                | ("", _, None) -> sprintf "%s" args
                | (_, "()", None) -> sprintf "%s()" LiteralUndef
                | (_, "", None) -> sprintf "%s" LiteralUndef
                | (_, "???", None) ->
                    if this.HasBrackets then
                        sprintf "%s[]" LiteralUndef
                    else
                        sprintf "%s()" LiteralUndef
                | (_, _, None) ->
                    if this.HasBrackets then sprintf "%s[%s]" LiteralUndef args
                    else sprintf "%s(%s)" LiteralUndef args
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

    override this.Run variableStack =
        if this.Scope.Count > 0 then 
            let called = 
                this.Scope 
                |> Seq.map (fun kvp -> kvp.Value) 
                |> Seq.toList 
                |> List.head
            if called.IsBlock() then
                match box called with
                | :? ICanBeCalledRecusively as calledRecursively when calledRecursively.CallCounter > maxRecursion -> () // stop recursion
                | _ ->
                    let pars = variableStack.SaveVariables(called) 
                    let args = this.ArgList |> Seq.toList
                    variableStack.ReplaceVariables pars args
                    // run all statements of the called node
                    called.Run variableStack
                    this.SetValuesOf called
                    variableStack.RestoreVariables(called)
        elif this.ArgList.Count = 1 then
            let arg = this.ArgList[0]
            arg.Run variableStack

    override this.EmbedInSymbolTable nextOpt = 
        match nextOpt with 
        | Some next when next.Name = LiteralLocL -> 
            next.FplId <- this.FplId
            next.TypeId <- this.TypeId
            next.EndPos <- this.EndPos
        | Some next when next.IsBlock() ->
            this.TryAddToParentsArgList() 
        | Some next when next.Scope.ContainsKey(".") -> 
            next.EndPos <- this.EndPos
        | Some next -> 
            this.TryAddToParentsArgList()
            next.EndPos <- this.EndPos
        | _ -> ()

    override this.RunOrder = None

let isReference (fv:FplValue) =
    match fv with
    | :? FplReference -> true
    | _ -> false

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
            | _ -> LiteralUndetermined
        this.SetValue(newValue)


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
                LiteralUndetermined
        this.SetValue(newValue)  


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
                LiteralUndetermined

        this.SetValue(newValue)  

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

    override this.Run _ = 
        let arg = this.ArgList[0]
        let argRepr = arg.Represent()
        let newValue =  new FplIntrinsicPred((this.StartPos, this.EndPos), this)

        newValue.FplId <- 
            match argRepr with 
            // FPL truth-table
            | LiteralFalse -> LiteralTrue
            | LiteralTrue -> LiteralFalse
            | _ -> LiteralUndetermined  

        this.SetValue(newValue)  

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
            | _ -> LiteralUndetermined
        
        this.SetValue(newValue) 

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
            | _ -> LiteralUndetermined
        
        this.SetValue(newValue)

/// Implements the semantics of an FPL equality.
type FplEquality(positions: Positions, parent: FplValue) as this =
    inherit FplGenericPredicate(positions, parent)

    do 
        this.FplId <- $"{LiteralDel}."
        this.TypeId <- LiteralPred

    override this.Name = PrimEqualityL
    override this.ShortName = PrimEquality

    override this.Clone () =
        let ret = new FplEquality((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    member this.Copy(other) =
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

    override this.Run _ = 
        if this.ArgList.Count <> 2 then 
            emitID013Diagnostics this.StartPos this.EndPos $"Predicate `=` takes 2 arguments, got {this.ArgList.Count}." 
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

        let newValue = new FplIntrinsicUndef((this.StartPos, this.EndPos), this.Parent.Value)
        match a1Repr with
        | LiteralUndef -> 
            emitID013Diagnostics this.StartPos this.EndPos "Predicate `=` cannot be evaluated because the left argument is undefined." 
            this.SetValue(newValue)
        | _ -> 
            match b1Repr with
            | LiteralUndef -> 
                emitID013Diagnostics this.StartPos this.EndPos "Predicate `=` cannot be evaluated because the right argument is undefined." 
                this.SetValue(newValue)
            | _ -> 
                let newValue = FplIntrinsicPred((this.StartPos, this.EndPos), this.Parent.Value)
                match a1Repr with
                | "dec pred"  
                | LiteralUndetermined -> 
                    emitID013Diagnostics this.StartPos this.EndPos "Predicate `=` cannot be evaluated because the left argument is undetermined." 
                    this.SetValue(newValue)
                | _ -> 
                    match b1Repr with
                    | "dec pred"  
                    | LiteralUndetermined -> 
                        emitID013Diagnostics this.StartPos this.EndPos "Predicate `=` cannot be evaluated because the right argument is undetermined." 
                        this.SetValue(newValue)
                    | _ -> 
                        let a1IsDeclared = a1Repr.Contains("dec ")
                        let b1IsDeclared = b1Repr.Contains("dec ")
                        newValue.FplId <- 
                            match a1IsDeclared, b1IsDeclared with
                            | false, false 
                            | true, true ->
                                $"{(a1Repr = b1Repr)}".ToLower()
                            | _ -> LiteralUndetermined
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
        let args = 
            this.ArgList
            |> Seq.map (fun arg -> arg.Type(propagate))
            |> String.concat ", "

        let qualification =
            if this.Scope.ContainsKey(".") then
                Some(this.Scope["."])
            else
                None

        match (head, args, qualification) with
        | (_, "", Some qual) -> sprintf "%s.%s" head (qual.Type(propagate))
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
        | ("???", _, None) -> sprintf "%s" head
        | ("", _, None) -> sprintf "%s" args
        | (_, "", None) -> sprintf "%s" head
        | (_, "???", None) ->
            if this.HasBrackets then
                sprintf "%s[]" head
            else
                sprintf "%s()" head
        | (_, _, None) ->
            if this.HasBrackets then sprintf "%s[%s]" head args
            elif head = "bydef." then sprintf "%s%s" head args
            else sprintf "%s(%s)" head args


    override this.Represent () = 
        let subRepr = 
            this.ValueList
            |> Seq.map (fun subfv -> subfv.Represent())
            |> String.concat ", "
        if subRepr = String.Empty then 
            LiteralUndef
        else
            subRepr

    override this.Run _ = ()

    override this.EmbedInSymbolTable nextOpt = 
        match nextOpt with
        | Some next when next.Scope.ContainsKey(".") -> ()
        | _ -> this.TryAddToParentsArgList() 

    override this.RunOrder = None

/// Returns Some argument of the FplValue depending of the type of it.
let getArgument (fv:FplValue) =
    match fv with
    | :? FplReference when fv.Scope.ContainsKey(fv.FplId) ->
        let refValue = fv.Scope[fv.FplId]
        // if the reference value itself contains value(s) and is not a class, 
        // return this value. 
        // Exceptions: 
        // 1) if refValue is a class, its "arg list" means something else - namely parent classes. In this case we only want to return the main class
        // 2) if refValue is a constructor, its "arg list" means something else - namely the calls to some base classes' constructors classes. In this case we only want to return the main constructor
        if refValue.ArgList.Count > 0 && not (refValue.IsClass()) && (refValue.Name <> LiteralCtorL) then
            Some refValue.ArgList[0] // return existing values except of classes, because those denoted their parent classes
        else 
            Some refValue 
    | :? FplReference when fv.FplId <> "" -> Some fv
    | :? FplReference when fv.ArgList.Count = 0 -> Some fv
    | _ when fv.ArgList.Count > 0 -> Some fv.ArgList[0]
    | _ -> None

/// Implements the semantics of an FPL decrement delegate.
type FplDecrement(positions: Positions, parent: FplValue) as this =
    inherit FplValue(positions, Some parent)

    do 
        this.FplId <- $"{LiteralDel}."
        this.TypeId <- LiteralObj

    override this.Name = PrimDecrementL
    override this.ShortName = PrimDecrement

    override this.Clone () =
        let ret = new FplDecrement((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    member this.Copy(other) =
        base.Copy(other)
        this.TypeId <- LiteralObj

    override this.Type signatureType = 
        let head = getFplHead this signatureType
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
        if this.ArgList.Count <> 1 then 
            this.Diagnostic $"Decrement takes 1 arguments, got {this.ArgList.Count}." 
        else


        let newValue = FplExtensionObj((this.StartPos, this.EndPos), this.Parent.Value)

        let argPre = this.ArgList[0]
        let argOpt = getArgument argPre
        let numericValue = 
            match argOpt with
            | Some arg when arg.IsVariable() -> 
                arg.Represent()
            | Some arg -> arg.FplId
            | None -> argPre.FplId

        let mutable n = 0
        System.Int32.TryParse(numericValue, &n) |> ignore
        let n' = n - 1
        newValue.FplId <- 
            if n' < 0 then 
                ""
            else
                string n'
        this.SetValue(newValue)

    override this.EmbedInSymbolTable _ = this.TryAddToParentsArgList()

    override this.RunOrder = None

type FplMapping(positions: Positions, parent: FplValue) =
    inherit FplValue(positions, Some parent)

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

        match (pars, myMapping) with
        | ("", None) -> this.TypeId
        | ("", Some map) -> sprintf "%s() -> %s" this.TypeId (map.Type(propagate))
        | (_, None) ->
            if this.HasBrackets then
                sprintf "%s[%s]" this.TypeId pars
            else
                sprintf "%s(%s)" this.TypeId pars
        | (_, Some map) ->
            if this.HasBrackets then
                sprintf "%s[%s] -> %s" this.TypeId pars (map.Type(propagate))
            else
                sprintf "%s(%s) -> %s" this.TypeId pars (map.Type(propagate))

    override this.Represent() = $"dec {this.Type(SignatureType.Type)}"

    override this.Run _ = ()

    override this.EmbedInSymbolTable _ = this.TryAddToParentsArgList() 

    override this.RunOrder = None

/// Tries to find a mapping of an FplValue
let rec getMapping (fv:FplValue) =
    match fv with
    | :? FplReference ->
        if fv.Scope.ContainsKey(fv.FplId) then
            getMapping fv.Scope[fv.FplId]
        else
            None
    | _ ->
        if fv.ArgList.Count > 0 then 
            let arg = fv.ArgList[0]
            match arg with 
            | :? FplMapping ->
                Some(arg)
            | _ -> None
        else
            None

/// Tries to match parameters of an FplValue with its arguments recursively
let rec mpwa (args: FplValue list) (pars: FplValue list) =
    match (args, pars) with
    | (a :: ars, p :: prs) ->
        let aType = a.Type(SignatureType.Type)
        let pType = p.Type(SignatureType.Type) 

        if aType = pType then
            mpwa ars prs
        elif pType.StartsWith(LiteralTpl) || pType.StartsWith("template") then
            mpwa ars prs
        elif pType = $"*{aType}" || pType.StartsWith("*") && aType = "???" then
            if ars.Length > 0 then mpwa ars pars else None
        elif pType.StartsWith("+") && aType = "???" then
            Some($"() does not match `{p.Type(SignatureType.Name)}:{pType}`")
        elif pType = $"+{aType}" then
            if ars.Length > 0 then mpwa ars pars else None
        elif
            aType.Length > 0
            && Char.IsUpper(aType[0])
            && isReference a
            && a.Scope.Count = 1
        then
            let var = a.Scope.Values |> Seq.toList |> List.head

            if var.Scope.ContainsKey(var.FplId) then
                let cl = var.Scope[var.FplId]

                match cl with
                | :? FplClass ->
                    let inheritanceList = findClassInheritanceChain cl pType

                    match inheritanceList with
                    | Some str -> mpwa ars prs
                    | None ->
                        Some(
                            $"`{a.Type(SignatureType.Name)}:{aType}` neither matches `{p.Type(SignatureType.Name)}:{pType}` nor the base classes"
                        )
                | _ ->
                    // this case does not occur but for we cover it for completeness reasons
                    Some(
                        $"`{a.Type(SignatureType.Name)}:{aType}` is undefined and doesn't match `{p.Type(SignatureType.Name)}:{pType}`"
                    )
            else
                Some(
                    $"`{a.Type(SignatureType.Name)}:{aType}` is undefined and does not match `{p.Type(SignatureType.Name)}:{pType}`"
                )
        elif aType.StartsWith(pType + "(") then
            None
        elif aType = "???" && pType <> "???" then
            Some($"`()` does not match `{p.Type(SignatureType.Name)}:{pType}`")
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
        | _ -> 
            Some($"missing argument for `{p.Type(SignatureType.Name)}:{pType}`")
    | (a :: [], []) ->
        if a.FplId = "???" then
            None
        else
            let aType = a.Type(SignatureType.Type)
            Some($"no matching parameter for `{a.Type(SignatureType.Name)}:{aType}`")
    | (a :: ars, []) ->
        let aType = a.Type(SignatureType.Type)
        Some($"no matching parameter for `{a.Type(SignatureType.Name)}:{aType}`")
    | ([], []) -> None

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
        let operand = this.ArgList[0]
        let typeOfOperand = this.ArgList[1]
        let newValue = new FplIntrinsicPred((this.StartPos, this.EndPos), this)
        newValue.FplId <- 
            // FPL truth-table
            match mpwa [operand] [typeOfOperand] with
            | Some errMsg -> LiteralFalse
            | None -> LiteralTrue
        
        this.SetValue(newValue)  

[<AbstractClass>]
type FplGenericQuantor(positions: Positions, parent: FplValue) =
    inherit FplGenericPredicate(positions, parent)

    override this.ShortName = PrimQuantor

    override this.Type signatureType =
        let head = getFplHead this signatureType

        let paramT =
            this.Scope
            |> Seq.filter (fun (kvp: KeyValuePair<string, FplValue>) -> kvp.Value.IsVariable())
            |> Seq.map (fun (kvp: KeyValuePair<string, FplValue>) -> kvp.Value.Type(signatureType))
            |> String.concat ", "

        match paramT with
        | "" -> head
        | _ -> sprintf "%s(%s)" head paramT

    override this.EmbedInSymbolTable _ = this.TryAddToParentsArgList() 
    
type FplQuantorAll(positions: Positions, parent: FplValue) as this =
    inherit FplGenericQuantor(positions, parent)

    do 
        this.FplId <- LiteralAll

    override this.Name = PrimQuantorAll

    override this.Clone () =
            let ret = new FplQuantorAll((this.StartPos, this.EndPos), this.Parent.Value)
            this.AssignParts(ret)
            ret

    override this.Run _ = () // todo implement run

type FplQuantorExists(positions: Positions, parent: FplValue) as this =
    inherit FplGenericQuantor(positions, parent)

    do 
        this.FplId <- LiteralEx

    override this.Name = PrimQuantorExists

    override this.Clone () =
            let ret = new FplQuantorExists((this.StartPos, this.EndPos), this.Parent.Value)
            this.AssignParts(ret)
            ret

    override this.Run _ = () // todo implement run

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

    override this.Run _ = () // todo implement run

type FplVariable(positions: Positions, parent: FplValue) =
    inherit FplValue(positions, Some parent)
    let mutable _variadicType = String.Empty // "" = variable, "many" = many, "many1" = many1 
    override this.Name = 
        match _variadicType with
        | "many" -> PrimVariableManyL
        | "many1"-> PrimVariableMany1L
        | _ -> PrimVariableL
    override this.ShortName = 
        match _variadicType with
        | "many" -> PrimVariableMany
        | "many1"-> PrimVariableMany1
        | _ -> PrimVariable

    member this.SetToMany() = 
        if _variadicType = String.Empty then
            _variadicType <- "many"
        else 
            failwith($"The variadic type was already set to {_variadicType}.")

    member this.SetToMany1() = 
        if _variadicType = String.Empty then
            _variadicType <- "many1"
        else 
            failwith($"The variadic type was already set to {_variadicType}.")

    override this.IsVariadic() = _variadicType <> String.Empty

    member this.IsMany = _variadicType = "many"
    member this.IsMany1 = _variadicType = "many1"


    override this.Clone () =
        let ret = new FplVariable((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.IsVariable () = true

    override this.SetValue fv =
        base.SetValue(fv)
        if fv.FplId <> LiteralUndef then
            this.IsInitializedVariable <- true

    override this.Type signatureType =
        let pars = getParamTuple this signatureType
        let head = getFplHead this signatureType
        let propagate = propagateSignatureType signatureType

        match (pars, getMapping this) with
        | ("", None) -> head
        | ("", Some map) -> sprintf "%s() -> %s" head (map.Type(propagate))
        | (_, None) ->
            if this.HasBrackets then
                sprintf "%s[%s]" head pars
            else
                sprintf "%s(%s)" head pars
        | (_, Some map) ->
            if this.HasBrackets then
                sprintf "%s[%s] -> %s" head pars (map.Type(propagate))
            else
                sprintf "%s(%s) -> %s" head pars (map.Type(propagate))

    override this.Represent () = 
        if this.ValueList.Count = 0 then
            if this.IsInitializedVariable then 
                // this case should never happen, because isInitializesVariable is a contradiction to ValueList.Count 0
                LiteralUndef
            else
                match this.TypeId with
                | LiteralUndef -> LiteralUndef
                | _ -> 
                    if this.IsVariadic() then
                        $"dec {this.Type(SignatureType.Type)}[]" 
                    else
                        $"dec {this.Type(SignatureType.Type)}" 
        else
            let subRepr = 
                this.ValueList
                |> Seq.map (fun subfv -> subfv.Represent())
                |> String.concat ", "
            if this.IsInitializedVariable then 
                subRepr
            else
                match this.TypeId with
                | LiteralUndef -> LiteralUndef
                | _ -> 
                    if this.IsVariadic() then
                        $"dec {this.Type(SignatureType.Type)}[]" 
                    else
                        $"dec {this.Type(SignatureType.Type)}" 

    override this.Run _ = ()

    override this.EmbedInSymbolTable nextOpt =
        match nextOpt with 
        | Some next when next.IsBlock() ->
            this.TryAddToParentsScope()
        | Some next when next.IsVariable() ->
            this.TryAddToParentsScope()
        | Some next when next.Name = PrimMappingL || next.Name = "quantor" ->  
            this.TryAddToParentsScope()
        | _ ->
            this.TryAddToParentsArgList()

    override this.RunOrder = None

[<AbstractClass>]
type FplGenericFunctionalTerm(positions: Positions, parent: FplValue) as this =
    inherit FplValue(positions, Some parent)
    let mutable _signStartPos = Position("", 0L, 0L, 0L)
    let mutable _signEndPos = Position("", 0L, 0L, 0L)

    do 
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
            // since the FunctionTerm has no value, it has no return statement
            // And the FPL syntax ensures that this can only be the case
            // if the Functional Term is intrinsic.
            // In this case, the "representation" of the function is
            // its declared mapping type
            let mapping = this.ArgList |> Seq.head 
            mapping.Represent()
        else
            let subRepr = 
                this.ValueList
                |> Seq.map (fun subfv -> subfv.Represent())
                |> String.concat ", "
            if subRepr = String.Empty then 
                LiteralUndef
            else
                subRepr

type FplFunctionalTerm(positions: Positions, parent: FplValue, runOrder) =
    inherit FplGenericFunctionalTerm(positions, parent)
    let _runOrder = runOrder
    let mutable _isReady = false
    let mutable _callCounter = 0

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

    override this.EmbedInSymbolTable _ = this.TryAddToParentsScope() 

    override this.RunOrder = Some _runOrder

    override this.Run variableStack = 
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
                _isReady <- this.Arity = 0 


type FplMandatoryFunctionalTerm(positions: Positions, parent: FplValue) =
    inherit FplGenericFunctionalTerm(positions, parent)

    override this.Name = PrimMandatoryFunctionalTermL
    override this.ShortName = PrimMandatoryFunctionalTerm

    override this.Clone () =
        let ret = new FplMandatoryFunctionalTerm((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.IsBlock () = true

    override this.EmbedInSymbolTable _ = this.TryAddToParentsScope() 

    override this.RunOrder = None

    override this.Run variableStack = 
        // todo implement run
        ()

type FplOptionalFunctionalTerm(positions: Positions, parent: FplValue) =
    inherit FplGenericFunctionalTerm(positions, parent)

    override this.Name = PrimOptionalFunctionalTermL
    override this.ShortName = PrimOptionalFunctionalTerm

    override this.Clone () =
        let ret = new FplOptionalFunctionalTerm((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.IsBlock () = true

    override this.EmbedInSymbolTable _ = this.TryAddToParentsScope() 

    override this.RunOrder = None

    override this.Run variableStack = 
        // todo implement run
        ()

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

    override this.Type signatureType = this.FplId

    override this.IsBlock () = true

    override this.Represent () = this.FplId

    override this.Run variableStack = 
        // todo implement run
        ()

    override this.EmbedInSymbolTable _ = this.TryAddToParentsScope() 

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

    override this.Run _ = ()

    override this.EmbedInSymbolTable _ = this.TryAddToParentsArgList() 

    override this.RunOrder = None

type FplIntrinsicFunc(positions: Positions, parent: FplValue) as this =
    inherit FplValue(positions, Some parent)
    do
        this.TypeId <- LiteralFunc
        this.FplId <- LiteralFunc

    override this.Name = PrimIntrinsicFunc
    override this.ShortName = LiteralFunc

    override this.Clone () =
        let ret = new FplIntrinsicFunc((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type (signatureType:SignatureType) = 
        getFplHead this signatureType
                    
    override this.Represent (): string = this.FplId

    override this.Run _ = () 

    override this.EmbedInSymbolTable _ = this.TryAddToParentsArgList() 

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

    override this.Run _ = () 

    override this.EmbedInSymbolTable _ = this.TryAddToParentsArgList() 

    override this.RunOrder = None

[<AbstractClass>]
type FplGenericStmt(positions: Positions, parent: FplValue) =
    inherit FplValue(positions, Some parent)

    override this.ShortName = PrimStmt

    override this.Type signatureType = this.FplId
    override this.Represent () = ""

    override this.EmbedInSymbolTable _ = this.TryAddToParentsArgList() 

    override this.RunOrder = None

/// Implements the return statement in FPL.
type FplReturn(positions: Positions, parent: FplValue) as this =
    inherit FplGenericStmt(positions, parent)

    do
        this.FplId <- LiteralRet
        this.TypeId <- LiteralUndef

    override this.Name = PrimReturn

    override this.Clone () =
        let ret = new FplReturn((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = this.FplId
    override this.Represent () = this.FplId

    member private this.MatchWithMapping (fva: FplValue) (fvp: FplValue) =
        let targetMapping = getMapping fvp

        match targetMapping with
        | Some tm -> mpwa [ fva ] [ tm ]
        | None -> Some($"Btest")

    override this.Run _ =
        let returnedReference = this.ArgList[0]
        let mapType = this.Parent.Value
        match this.MatchWithMapping returnedReference mapType with
        | Some errMsg -> emitSIG03Diagnostics errMsg (mapType.Type(SignatureType.Type)) (returnedReference.StartPos) (returnedReference.EndPos)
        | _ -> 
            let returnedValueOpt = getArgument returnedReference
            match returnedValueOpt with
            | Some returnedValue -> 
                if returnedValue.ValueList.Count > 0 then
                    this.SetValuesOf returnedValue
                else
                    // todo diagnostics returns uninitialized value
                    let value = new FplIntrinsicUndef((this.StartPos, this.EndPos), this)
                    this.SetValue(value)
            | _ -> 
                // add an undefined value since there was no argument of the returnedValue
                let value = new FplIntrinsicUndef((this.StartPos, this.EndPos), this)
                this.SetValue(value)

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

    member private this.CheckSIG05Diagnostics (assignee:FplValue) (toBeAssignedValue: FplValue) = 
        let valueOpt = getArgument toBeAssignedValue
        match valueOpt with
        | Some value when value.IsClass() ->
            let chainOpt = findClassInheritanceChain value assignee.TypeId
            match chainOpt with
            | None ->
                // issue SIG05 diagnostics if either no inheritance chain found 
                emitSIG05Diagnostics (assignee.Type(SignatureType.Type)) (value.Type(SignatureType.Type)) toBeAssignedValue.StartPos toBeAssignedValue.EndPos
            | _ -> () // inheritance chain found (no SIG05 diagnostics)
        | Some value when (value.Name = LiteralCtorL) ->
            // find a class inheritance chain for the constructor's class (which is stored in its parent value)
            let chainOpt = findClassInheritanceChain value.Parent.Value assignee.TypeId
            match chainOpt with
            | None ->
                // issue SIG05 diagnostics if either no inheritance chain found 
                emitSIG05Diagnostics (assignee.Type(SignatureType.Type)) (value.Type(SignatureType.Type)) toBeAssignedValue.StartPos toBeAssignedValue.EndPos
            | _ -> () // inheritance chain found (no SIG05 diagnostics)
        | Some value when assignee.TypeId <> value.TypeId ->
            // Issue SIG05 diagnostics if value is not a constructor and not a class and still the types are not the same 
            emitSIG05Diagnostics (assignee.Type(SignatureType.Type)) (value.Type(SignatureType.Type)) toBeAssignedValue.StartPos toBeAssignedValue.EndPos
        | Some value when assignee.TypeId = value.TypeId ->
            // Issue no SIG05 diagnostics if value is not a constructor and not a class but the types match
            ()
        | _ ->
            // Issue SIG05 diagnostics if there is (for some reason) no value of the toBeAssignedValue 
            emitSIG05Diagnostics (assignee.Type(SignatureType.Type)) (toBeAssignedValue.Type(SignatureType.Type)) toBeAssignedValue.StartPos toBeAssignedValue.EndPos

    member this.Assignee =
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
        match this.Assignee, this.AssignedValue with
        | Some assignee, Some assignedValue ->
            let nameAssignee = assignee.Type(SignatureType.Name)
            let nameAssignedValue = assignedValue.Type(SignatureType.Name)
            if nameAssignee = nameAssignedValue then
                emitLG005Diagnostics nameAssignedValue assignedValue.StartPos assignedValue.EndPos
            else
                this.CheckSIG05Diagnostics assignee assignedValue
                assignedValue.Run variableStack
                assignee.SetValuesOf assignedValue
                match assignee with
                | :? FplVariable -> assignee.IsInitializedVariable <- true
                | _ -> ()
        | _ -> ()

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
        ()

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
        ()

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
        ()

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
        ()

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
        ()

type FplForInStmtEntity(positions: Positions, parent: FplValue) =
    inherit FplGenericStmt(positions, parent)

    override this.Name = PrimForInStmtEntity

    override this.Clone () =
        let ret = new FplForInStmtEntity((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = 
        getFplHead this signatureType

    override this.Represent () = LiteralUndef

    override this.Run variableStack = 
        // todo implement run
        ()

type FplForInStmtDomain(positions: Positions, parent: FplValue) =
    inherit FplGenericStmt(positions, parent)

    override this.Name = PrimForInStmtDomain

    override this.Clone () =
        let ret = new FplForInStmtDomain((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = 
        getFplHead this signatureType

    override this.Represent () = LiteralUndef

    override this.Run variableStack = 
        // todo implement run
        ()

type FplConstructorCall(positions: Positions, parent: FplValue) as this =
    inherit FplGenericStmt(positions, parent)

    do 
        this.FplId <- LiteralObj
        this.TypeId <- LiteralObj

    override this.Name = PrimConstructorCall

    override this.Clone () =
        let ret = new FplConstructorCall((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = 
        getFplHead this signatureType

    override this.Represent () = LiteralUndef

    override this.Run variableStack = 
        // todo implement run
        ()

/// A string representation of an FplValue
let toString (fplValue:FplValue) = $"{fplValue.ShortName} {fplValue.Type(SignatureType.Name)}"

/// Qualified name of this FplValue
let qualifiedName (fplValue:FplValue)=
    let rec getFullName (fv: FplValue) (first: bool) =
        let fplValueType =
            match fv with
            | :? FplLocalization
            | :? FplExclusiveOr 
            | :? FplConjunction
            | :? FplDisjunction 
            | :? FplNegation 
            | :? FplImplication 
            | :? FplEquivalence 
            | :? FplIsOperator 
            | :? FplExtensionObj 
            | :? FplEquality 
            | :? FplReference -> fv.Type(SignatureType.Name)
            | :? FplLocalization
            | :? FplConstructor
            | _ when fv.IsBlock() -> fv.Type(SignatureType.Mixed)
            | :? FplGenericQuantor -> fv.Type(SignatureType.Mixed)
            | _ -> fv.FplId

        match fv with
        | :? FplRoot -> ""
        | _ -> 
            if first then
                if isRoot fv.Parent.Value then
                    getFullName fv.Parent.Value false + fplValueType
                else if fv.IsVariable() && not (fv.Parent.Value.IsVariable()) then
                    fplValueType
                else
                    getFullName fv.Parent.Value false + "." + fplValueType
            else if isRoot fv.Parent.Value then
                getFullName fv.Parent.Value false + fplValueType
            else if fv.IsVariable() && not (fv.Parent.Value.IsVariable()) then
                fplValueType
            else
                getFullName fv.Parent.Value false + "." + fplValueType

    getFullName fplValue true

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
                | :? FplOptionalFunctionalTerm
                | :? FplMandatoryPredicate
                | :? FplOptionalPredicate
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

// Create an FplValue list containing all Scopes of an FplNode
let rec flattenScopes (root: FplValue) =
    let rec helper (node: FplValue) (acc: FplValue list) =
        let newAcc = node :: acc
        node.Scope |> Seq.fold (fun acc kvp -> helper kvp.Value acc) newAcc

    helper root []

let stripLastDollarDigit (s: string) =
    let lastIndex = s.LastIndexOf('$')
    if lastIndex <> -1 then s.Substring(0, lastIndex) else s

/// Checks if an fv is provable. This will only be true if
/// it is a theorem, a lemma, a proposition, or a corollary
let isProvable (fv: FplValue) =
    match fv with
    | :? FplTheorem
    | :? FplLemma
    | :? FplProposition
    | :? FplCorollary -> true
    | _ -> false

/// Checks if an fplValue is a conjecture or an axiom. This is used to decide whether or
/// not it is not provable.
let isAxiomOrConnjecture (fv:FplValue) = 
    match fv with
    | :? FplConjecture 
    | :? FplAxiom -> true
    | _ -> false

/// Tries to find a theorem-like statement for a proof
/// and returns different cases of ScopeSearchResult, depending on different semantical error situations.
let tryFindAssociatedBlockForProof (fplValue: FplValue) =
    match fplValue with
    | :? FplProof ->
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
                flattenedScopes |> List.filter (fun fv -> fv.FplId = potentialProvableName || fv.FplId = $"@{potentialProvableName}")

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
    | _ ->
        ScopeSearchResult.NotApplicable

/// Tries to find a theorem-like statement, a conjecture, or an axiom for a corollary
/// and returns different cases of ScopeSearchResult, depending on different semantical error situations.
let tryFindAssociatedBlockForCorollary (fplValue: FplValue) =

    match fplValue with 
    | :? FplCorollary ->
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
    | _ ->
        ScopeSearchResult.NotApplicable

/// Tries to find a theorem-like statement, an axiom or a corollary
/// and returns different cases of ScopeSearchResult, depending on different semantical error situations.
let tryFindAssociatedBlockForJustificationItem (fvJi: FplGenericJustificationItem) (candidates:FplValue list) =
    let nameOfOther (fv:FplValue) =
        if isEnglishAn fv.Name then 
            $"'{fv.Type(SignatureType.Name)} which is an {fv.Name}'"
        else 
            $"'{fv.Type(SignatureType.Name)} which is a {fv.Name}"
    

    match candidates.Length with
    | 1 ->  // exactly one candidate found
        let potentialCandidate = candidates.Head
        match fvJi, potentialCandidate with
        | :? FplJustificationItemByProofArgument, :? FplProof
        | :? FplJustificationItemByDef, :? FplClass
        | :? FplJustificationItemByDef, :? FplPredicate
        | :? FplJustificationItemByDef, :? FplFunctionalTerm
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
            let fplValueRepr = $"{root.Represent()}".Replace(@"\", @"\\")

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
            if parts.Length > 0 then 
                parts.[0] 
            else 
                ""
        else
            name
    let nameWithProofOrCorRef = 
        if withCorollariesOrProofs && not (name.Contains("$")) then 
            $"{name}$"
        else
            name

    st.Root.Scope // iterate all theories
    |> Seq.iter (fun theory ->
        theory.Value.Scope
        // filter only blocks starting with the same FplId as the reference
        |> Seq.map (fun kvp -> kvp.Value)
        |> Seq.filter (fun fv -> fv.FplId = name || fv.FplId = nameWithoutProofOrCorRef || fv.FplId.StartsWith(nameWithProofOrCorRef))
        |> Seq.iter (fun (block: FplValue) ->
            pm.Add(block)

            if withClassConstructors && block.IsClass() then
                block.Scope
                |> Seq.map (fun kvp -> kvp.Value)
                |> Seq.filter (fun (fv: FplValue) -> (fv.Name = LiteralCtorL))
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

/// Searches in the counter-th parent predecessor of fv and checks if it is a definition block.
/// Returns ScopeSearchResult.NotFound if no such block was reached.
/// Returns ScopeSearchResult.FoundIncorrectBlock if the reached block is not a definition block.
/// Returns ScopeSearchResult.Found if a definition block was reached.
let rec nextDefinition (fv: FplValue) counter =
    let blocks = Stack<FplValue>()

    if isTheory fv then
        let name =
            if blocks.Count > 0 then
                let fv1 = blocks.Peek()
                $"'{fv1.Name}' {fv1.Type(SignatureType.Name)}"
            else
                "(no block found)"

        ScopeSearchResult.FoundIncorrectBlock name
    else
        match fv with
        | :? FplTheorem
        | :? FplLemma
        | :? FplProposition
        | :? FplCorollary
        | :? FplConjecture
        | :? FplAxiom 
        | :? FplRuleOfInference 
        | :? FplProof
        | :? FplLocalization ->
            let name = $"'{fv.Name}' {fv.Type(SignatureType.Name)}"
            ScopeSearchResult.FoundIncorrectBlock name
        | :? FplPredicate ->
                blocks.Push(fv)

                if counter <= 0 then
                    ScopeSearchResult.Found fv
                else
                    let next = fv.Parent

                    match next with
                    | Some parent -> nextDefinition parent (counter - 1)
                    | None ->
                        let name =
                            if blocks.Count > 0 then
                                let fv1 = blocks.Peek()
                                $"'{fv1.Name}' {fv.Type(SignatureType.Name)}"
                            else
                                "(no block found)"

                        ScopeSearchResult.FoundMultiple name
        | :? FplMandatoryPredicate
        | :? FplOptionalPredicate
        | :? FplMandatoryFunctionalTerm
        | :? FplOptionalFunctionalTerm
        | :? FplFunctionalTerm
        | :? FplClass ->
            blocks.Push(fv)

            if counter <= 0 then
                ScopeSearchResult.Found fv
            else
                let next = fv.Parent

                match next with
                | Some parent -> nextDefinition parent (counter - 1)
                | None ->
                    let name =
                        if blocks.Count > 0 then
                            let fv1 = blocks.Peek()
                            $"'{fv1.Name}' {fv.Type(SignatureType.Name)}"
                        else
                            "(no block found)"

                    ScopeSearchResult.FoundMultiple name
        | _ ->
            let next = fv.Parent

            match next with
            | Some parent -> nextDefinition parent counter
            | None -> ScopeSearchResult.NotFound

/// Tries to match the arguments of `fva` FplValue with the parameters of the `fvp` FplValue and returns
/// Some(specific error message) or None, if the match succeeded.
let matchArgumentsWithParameters (fva: FplValue) (fvp: FplValue) =
    let parameters =
        match fvp with
        | :? FplTheorem
        | :? FplLemma
        | :? FplProposition
        | :? FplCorollary
        | :? FplConjecture
        | :? FplPredicate
        | :? FplAxiom
        | :? FplRuleOfInference ->
            fvp.Scope.Values |> Seq.filter (fun fv -> fv.IsSignatureVariable) |> Seq.toList
        | _ ->
            match fvp with
            | :? FplFunctionalTerm
            | :? FplConstructor
            | :? FplMandatoryPredicate
            | :? FplMandatoryFunctionalTerm
            | :? FplOptionalPredicate
            | :? FplOptionalFunctionalTerm ->
                fvp.Scope.Values |> Seq.filter (fun fv -> fv.IsSignatureVariable) |> Seq.toList
            | _ -> fvp.Scope.Values |> Seq.toList

    let arguments = fva.ArgList |> Seq.toList

    let stdMsg = $"{qualifiedName fvp}"
    let argResult = mpwa arguments parameters

    match argResult with
    | Some aErr -> Some($"{aErr} in {stdMsg}")
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

/// Copy the variables and properties of a parent class into a derived class.
let copyParentToDerivedClass (parentClass: FplValue) (derivedClass: FplValue) =
    let shadowedVars = List<string>()
    let shadowedProperties = List<string>()
    let parentVariables = parentClass.GetVariables()

    parentVariables
    |> List.iter (fun parentVar ->
        if derivedClass.Scope.ContainsKey(parentVar.FplId) then
            shadowedVars.Add(parentVar.FplId))

    (shadowedVars, shadowedProperties)
