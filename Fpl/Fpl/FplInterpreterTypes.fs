/// This module contains all types necessary to interpret FPL code (semantics)
module FplInterpreterTypes

open System
open System.Text.RegularExpressions
open System.Collections.Generic
open System.Text
open System.IO
open FParsec
open FplGrammarCommons
open FplGrammarTypes
open FplParser
open ErrDiagnostics

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
                            true // the third is the internet source
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


type FplBlockType =
    | Todo
    | Variable
    | VariadicVariableMany
    | VariadicVariableMany1
    | Class
    | Constructor
    | FunctionalTerm
    | MandatoryFunctionalTerm
    | OptionalFunctionalTerm
    | Predicate
    | MandatoryPredicate
    | OptionalPredicate
    | Proposition
    | Conjecture
    | Corollary
    | Proof
    | Argument
    | Justification
    | ArgInference
    | Localization
    | Translation
    | Language
    | Reference
    | Quantor
    | Mapping
    | Stmt
    | Assertion
    | Extension
    | Instance
    | IntrinsicInd
    | IntrinsicObj
    | IntrinsicPred
    | IntrinsicUndef
    | IntrinsicFunc
    | IntrinsicTpl

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

[<AbstractClass>]
type FplValue(blockType: FplBlockType, positions: Positions, parent: FplValue option) =
    let mutable _expressionType = FixType.NoFix
    let mutable _exprTypeAlreadySet = false
    let mutable _startPos = fst positions
    let mutable _endPos = snd positions
    let mutable _blockType = blockType
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
    let _assertedPredicates = List<FplValue>()

    /// A list of asserted predicates for this FplValue
    member this.AssertedPredicates = _assertedPredicates

    /// A scope of this FplValue
    member this.Scope = _scope

    /// An argument list of this FplValue
    member this.ArgList = _argList

    /// ValueList of the FplValue.
    member this.ValueList = _valueList

    abstract member Instantiate: unit -> FplValue option // not all FplValues can be instantiated, e.g. a theorem cannot be instantiated
    abstract member Clone: unit -> FplValue
    abstract member AssignParts: FplValue -> unit
    abstract member ShortName: string
    abstract member Name: string
    abstract member Represent: unit -> string
    abstract member Type: SignatureType -> string
    
    /// Adds this FplValue to it's parent's ArgList, if such a Parent exists.
    abstract member TryAddToParentsArgList: unit -> unit

    /// Indicates if this FplValue is an root of the symbol table.
    abstract member IsRoot: unit -> bool

    /// Indicates if this FplValue is a constructor or a theory
    abstract member IsTheory: unit -> bool

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

    (* Default implementations = everything is false, only the trues are overridden in derived classes *)
    override this.IsRoot () = false
    override this.IsTheory () = false
    override this.IsFplBlock () = false
    override this.IsBlock () = false
    override this.IsClass (): bool = false
    override this.IsProof (): bool = false
    override this.IsVariable (): bool = false
    
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

        this.AssertedPredicates
        |> Seq.iter (fun (fv1:FplValue) ->
            // asserted predicates do not have to be cloned
            ret.ArgList.Add(fv1))
            

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

    /// Type of the FPL block within this FplValue
    member this.FplBlockType
        with get () = _blockType
        and set (value) = _blockType <- value

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

    /// Tries to find a mapping of this FplValue
    member this.Mapping =
        match this.FplBlockType with
        | FplBlockType.Reference ->
            if this.Scope.ContainsKey(this.FplId) then
                this.Scope[this.FplId].Mapping
            else
                None
        | _ ->
            if this.ArgList.Count > 0 && this.ArgList[0].FplBlockType = FplBlockType.Mapping then
                Some(this.ArgList[0])
            else
                None

    /// Returns Some argument of the FplValue depending of the type of its 
    /// FplBlockType. 
    member this.GetArgument =
        match this.FplBlockType with
        | FplBlockType.Reference when this.Scope.ContainsKey(this.FplId) ->
            let refValue = this.Scope[this.FplId]
            // if the reference value itself contains value(s) and is not a class, 
            // return this value. 
            // Exceptions: 
            // 1) if refValue is a class, its "arg list" means something else - namely parent classes. In this case we only want to return the main class
            // 2) if refValue is a constructor, its "arg list" means something else - namely the calls to some base classes' constructors classes. In this case we only want to return the main constructor
            if refValue.ArgList.Count > 0 && not (refValue.IsClass()) && refValue.FplBlockType <> FplBlockType.Constructor then
                Some refValue.ArgList[0] // return existing values except of classes, because those denoted their parent classes
            else 
                Some refValue 
        | FplBlockType.Reference when this.FplId <> "" -> Some this
        | FplBlockType.Reference when this.ArgList.Count = 0 -> Some this
        | FplBlockType.Stmt when this.FplId = "bas" && this.ArgList.Count > 0 -> 
            let test = this.ArgList[0]
            // in case of a base.obj() constructor call
            if test.ArgList.Count = 2 && 
                test.ArgList[0].FplBlockType = FplBlockType.IntrinsicObj && 
                test.ArgList[1].FplBlockType = FplBlockType.Reference &&
                test.ArgList[1].FplId = "???" then
                // return an FplValue inbuilt Object 
                Some  test.ArgList[0] 
            elif test.ArgList.Count > 0 then
               Some test 
            else
                None
        | _ when this.ArgList.Count > 0 -> Some this.ArgList[0]
        | _ -> None

    /// Sets the value of this FplValue taking into account if this
    /// FplValue is a Reference to a variable.
    member this.SetValue(fplValue) =
        match this.FplBlockType with
        | FplBlockType.Reference when this.Scope.ContainsKey(this.FplId) ->
            let var = this.Scope[this.FplId]
            var.ValueList.Clear()
            var.ValueList.Add(fplValue)
        | FplBlockType.Variable -> 
            this.ValueList.Clear()
            this.ValueList.Add(fplValue)
            this.IsInitializedVariable <- true
        | _ -> 
            this.ValueList.Clear()
            this.ValueList.Add(fplValue)

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

    /// Returns Some or none FplValue being the enclosing class block of a node inside a class.
    member this.GetClassBlock() =
        let rec getClassBlock (fv: FplValue) =
            match fv.FplBlockType with
            | FplBlockType.Class -> Some fv
            | _ ->
                match fv.Parent with
                | Some parent -> getClassBlock parent
                | _ -> None

        getClassBlock this

    /// If this is a class definition, the function will return a list (possibly empty) list of all of its constructors.
    member this.GetConstructors() =
        this.Scope
        |> Seq.map (fun kvp -> kvp.Value)
        |> Seq.filter (fun fv -> fv.FplBlockType = FplBlockType.Constructor)
        |> Seq.toList

    /// 
    member this.GetParamTuple(signatureType:SignatureType) =
        let propagate =
            match signatureType with
            | SignatureType.Mixed -> SignatureType.Type
            | _ -> signatureType
        this.Scope
        |> Seq.filter (fun (kvp: KeyValuePair<string, FplValue>) ->
            kvp.Value.IsSignatureVariable
            || this.IsVariable() && not (kvp.Value.IsClass())
            || this.FplBlockType = FplBlockType.Mapping)
        |> Seq.map (fun (kvp: KeyValuePair<string, FplValue>) -> kvp.Value.Type(propagate))
        |> String.concat ", "

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

    override this.TryAddToParentsArgList () = 
        match this.Parent with 
        | Some parent -> parent.ArgList.Add(this)
        | _ -> ()

    /// Generates a type string identifier or type-specific naming convention of this FplValue.
    override this.Type(isSignature: SignatureType)  = 
        match (this.FplBlockType, this.Scope.ContainsKey(this.FplId)) with
        | (FplBlockType.Reference, true) ->
            // delegate the type identifier to the referenced entity
            let val1 = this.Scope[this.FplId]
            val1.Type(isSignature)
        | (FplBlockType.Stmt, _)
        | (FplBlockType.Extension, _) -> this.FplId
        | _ ->
            let head =
                match (this.FplBlockType, this.Scope.ContainsKey(this.FplId)) with
                | (FplBlockType.Reference, true) ->
                    match isSignature with
                    | SignatureType.Name -> this.Scope[this.FplId].Type(SignatureType.Name)
                    | SignatureType.Mixed -> this.FplId
                    | SignatureType.Type -> this.TypeId
                | (FplBlockType.Mapping, _) -> this.TypeId
                | _ ->
                    match isSignature with
                    | SignatureType.Name 
                    | SignatureType.Mixed -> this.FplId
                    | SignatureType.Type -> this.TypeId

            let propagate =
                match isSignature with
                | SignatureType.Mixed -> SignatureType.Type
                | _ -> isSignature 

            let idRec () =
                match this.FplBlockType with
                | FplBlockType.Proof
                | FplBlockType.Argument
                | FplBlockType.Language
                | FplBlockType.IntrinsicObj
                | FplBlockType.IntrinsicInd
                | FplBlockType.IntrinsicPred
                | FplBlockType.IntrinsicFunc
                | FplBlockType.IntrinsicTpl
                | FplBlockType.IntrinsicUndef
                | FplBlockType.Class -> head
                | FplBlockType.Constructor
                | FplBlockType.OptionalPredicate
                | FplBlockType.MandatoryPredicate ->
                    let paramT = this.GetParamTuple(isSignature)
                    sprintf "%s(%s)" head paramT
                | FplBlockType.Quantor ->
                    let paramT =
                        this.Scope
                        |> Seq.filter (fun (kvp: KeyValuePair<string, FplValue>) -> kvp.Value.IsVariable())
                        |> Seq.map (fun (kvp: KeyValuePair<string, FplValue>) -> kvp.Value.Type(isSignature))
                        |> String.concat ", "

                    match paramT with
                    | "" -> head
                    | _ -> sprintf "%s(%s)" head paramT
                | FplBlockType.Localization ->
                    let paramT =
                        this.Scope
                        |> Seq.filter (fun (kvp: KeyValuePair<string, FplValue>) -> kvp.Value.IsVariable())
                        |> Seq.map (fun (kvp: KeyValuePair<string, FplValue>) -> kvp.Value.Type(isSignature))
                        |> String.concat ", "

                    match paramT with
                    | "" -> head
                    | _ -> sprintf "%s(%s)" head paramT
                | FplBlockType.OptionalFunctionalTerm
                | FplBlockType.MandatoryFunctionalTerm
                | FplBlockType.FunctionalTerm ->
                    match this.Mapping with
                    | Some map ->
                        let paramT = this.GetParamTuple(isSignature)
                        sprintf "%s(%s) -> %s" head paramT (map.Type(propagate))
                    | _ -> ""
                | FplBlockType.Instance ->
                    let args =
                        this.ArgList
                        |> Seq.map (fun fv -> fv.Type(isSignature))
                        |> String.concat ","
                    if args <> String.Empty then
                        sprintf "%s:%s" head args
                    else
                        head
                | FplBlockType.Translation ->
                    let args =
                        this.ArgList
                        |> Seq.filter (fun fv ->
                            fv.FplBlockType <> FplBlockType.Stmt && fv.FplBlockType <> FplBlockType.Assertion)
                        |> Seq.map (fun fv -> fv.Type(SignatureType.Name))
                        |> String.concat ""

                    sprintf "%s%s" head args
                | FplBlockType.Mapping
                | FplBlockType.Variable
                | FplBlockType.VariadicVariableMany
                | FplBlockType.VariadicVariableMany1 ->
                    let pars = this.GetParamTuple(isSignature)

                    match (pars, this.Mapping) with
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
                | FplBlockType.Reference ->
                    let qualification =
                        if this.Scope.ContainsKey(".") then
                            Some(this.Scope["."])
                        else
                            None
                    // The arguments are reserved for the arguments or the coordinates of the reference
                    // If the argument tuple equals "???", an empty argument or coordinates list has occurred
                    let args =
                        this.ArgList
                        |> Seq.filter (fun fv ->
                            fv.FplBlockType <> FplBlockType.Stmt && fv.FplBlockType <> FplBlockType.Assertion)
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
                        elif head = "bydef." then sprintf "%s%s" head args
                        else sprintf "%s(%s)" head args
                | _ -> ""

            idRec ()

    /// Generates a representation of this FplValue.
    override this.Represent() = 
        let rec children (fv:FplValue) isFirst = 
            match (isFirst, fv.ValueList.Count = 0) with
            | (true, true) -> 
                match fv.FplBlockType with
                | FplBlockType.FunctionalTerm -> 
                    // since the FunctionTerm has no value, it has no return statement
                    // And the FPL syntax ensures that this can only be the case
                    // if the Functional Term is intrinsic.
                    // In this case, the "representation" of the function is
                    // its declared mapping type
                    let mapping = fv.ArgList |> Seq.head 
                    $"dec {mapping.Represent()}"              
                | _ -> 
                    literalUndef
            | (false, false) 
            | (true, false) ->
                let subRepr = 
                    fv.ValueList
                    |> Seq.map (fun subfv -> 
                        children subfv false 
                    )
                    |> String.concat ", "
                if subRepr = String.Empty then
                    ""
                else 
                    $"{fv.FplId}({subRepr})"
            | (false, true) -> fv.FplId

        children this true                     
    

type FplRoot() =
    inherit FplValue(FplBlockType.Todo, (Position("", 0, 1, 1), Position("", 0, 1, 1)), None)
    override this.Name = "a root"
    override this.ShortName = "root"
    override this.Instantiate () = None
    override this.Clone () =
        let ret = new FplRoot()
        this.AssignParts(ret)
        ret
    override this.IsRoot () = true

    override this.Type(_:SignatureType) = String.Empty
    override this.Represent () = literalUndef
    override this.TryAddToParentsArgList () = () 

type FplTheory(positions: Positions, parent: FplValue, filePath: string) as this =
    inherit FplValue(FplBlockType.Todo, positions, Some parent)
    do
        this.FilePath <- Some filePath

    override this.Name = "a theory"
    override this.ShortName = "th"
    override this.Instantiate () = None
    override this.Clone () =
        let ret = new FplTheory((this.StartPos, this.EndPos), this.Parent.Value, this.FilePath.Value)
        this.AssignParts(ret)
        ret

    override this.IsTheory () = true

    override this.Type(signatureType:SignatureType) =
        match signatureType with
        | SignatureType.Name 
        | SignatureType.Mixed -> this.FplId
        | SignatureType.Type -> this.TypeId

    override this.Represent () = literalUndef


[<AbstractClass>]
type FplGenericPredicate(blockType: FplBlockType, positions: Positions, parent: FplValue) as this =
    inherit FplValue(blockType, positions, Some parent)
    do 
        this.FplId <- literalUndetermined
        this.TypeId <- literalPred

    override this.Instantiate () = None
    override this.Represent (): string = 
        this.ValueList
        |> Seq.map (fun subfv -> subfv.Represent())
        |> String.concat ", "


[<AbstractClass>]
type FplGenericPredicateWithExpression(blockType: FplBlockType, positions: Positions, parent: FplValue) =
    inherit FplGenericPredicate(blockType, positions, parent)

    override this.Type(signatureType:SignatureType) = 
        let paramT = this.GetParamTuple(signatureType)
        let head = 
            match signatureType with
                    | SignatureType.Name 
                    | SignatureType.Mixed -> this.FplId
                    | SignatureType.Type -> this.TypeId
        sprintf "%s(%s)" head paramT
            
[<AbstractClass>]
type FplGenericObject(blockType: FplBlockType, positions: Positions, parent: FplValue) as this =
    inherit FplValue(blockType, positions, Some parent)

    do
        this.FplId <- literalObj
        this.TypeId <- literalObj

        //let rec getInstance (previous:FplValue) =
        //    let (inst:FplValue) = createFplValue((fplValue.StartPos,fplValue.EndPos), FplBlockType.Instance, previous)
        //    inst.FplId <- fplValue.FplId
        //    inst.TypeId <- fplValue.TypeId
        //    let (constructors: FplValue list) = fplValue.GetConstructors()
        //    previous.ArgList
        //    |> Seq.iter (fun next -> 
        //        inst.ArgList.Add (createInstance next)
        //    )
        //    inst
        //match this.FplBlockType with
        //| FplBlockType.IntrinsicObj
        //| FplBlockType.Constructor
        //| FplBlockType.Class ->
        //    getInstance this
        //| FplBlockType.Stmt when fplValue.FplId = "bas" ->
        //    // in case of a base class constructor call (that resides inside this that is a constructor)
        //    // identify the 
        //    let baseClassOpt = fplValue.GetArgument
        //    match baseClassOpt with
        //    | Some (baseClass:FplValue) -> 
        //        getInstance baseClass
        //    | _ -> failwith ($"Cannot create an instance of a base class, missing constructor {fplValue.Type(SignatureType.Mixed)}") 
        //| _ -> 
        //    createRoot() // todo
        //    //failwith ($"Cannot create an instance of a non-class {this.Type(SignatureType.Mixed)}")    

type FplRuleOfInference(positions: Positions, parent: FplValue) =
    inherit FplGenericPredicateWithExpression(FplBlockType.Todo, positions, parent)

    override this.Name = "a rule of inference"
    override this.ShortName = literalInf

    override this.Clone () =
        let ret = new FplRuleOfInference((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret
    override this.IsFplBlock () = true
    override this.IsBlock () = true    

type FplVariable(positions: Positions, parent: FplValue) =
    inherit FplValue(FplBlockType.Variable, positions, Some parent)
    let mutable _variadicType = String.Empty // "" = variable, "many" = many, "many1" = many1 
    override this.Name = "a variable"
    override this.ShortName = "var"

    member this.SetToMany() = 
        if _variadicType = String.Empty then
            _variadicType <- "many"
            this.FplBlockType <- FplBlockType.VariadicVariableMany
        else 
            failwith($"The variadic type was already set to {_variadicType}.")

    member this.SetToMany1() = 
        if _variadicType = String.Empty then
            _variadicType <- "many1"
            this.FplBlockType <- FplBlockType.VariadicVariableMany1
        else 
            failwith($"The variadic type was already set to {_variadicType}.")

    member this.IsVariadic = _variadicType <> String.Empty

    member this.IsMany = _variadicType = "many"
    member this.IsMany1 = _variadicType = "many1"


    override this.Clone () =
        let ret = new FplVariable((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Instantiate () = None

    override this.IsVariable () = true

    override this.Represent () = 
        if this.ValueList.Count = 0 then
            if this.IsInitializedVariable then 
                // this case should never happen, because isInitializesVariable is a contradiction to ValueList.Count 0
                literalUndef
            else
                match this.TypeId with
                | FplGrammarCommons.literalUndef -> literalUndef
                | _ -> 
                    if this.IsVariadic then
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
                | FplGrammarCommons.literalUndef -> literalUndef
                | _ -> 
                    if this.IsVariadic then
                        $"dec {this.Type(SignatureType.Type)}[]" 
                    else
                        $"dec {this.Type(SignatureType.Type)}" 




type FplInstance(positions: Positions, parent: FplValue) =
    inherit FplGenericObject(FplBlockType.Instance, positions, parent)

    override this.Name = "an instance"
    override this.ShortName = "inst"

    override this.Clone () =
        let ret = new FplInstance((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Instantiate () = None

type FplClass(positions: Positions, parent: FplValue) =
    inherit FplGenericObject(FplBlockType.Class, positions, parent)

    override this.Name = "a class definition"
    override this.ShortName = "def cl"

    override this.Clone () =
        let ret = new FplClass((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Instantiate () = 
        Some (new FplInstance((this.StartPos, this.EndPos), this.Parent.Value))

    override this.IsFplBlock () = true
    override this.IsBlock () = true
    override this.IsClass () = true
    override this.Represent () = $"dec {literalCl} {this.FplId}"

type FplConstructor(positions: Positions, parent: FplValue) =
    inherit FplGenericObject(FplBlockType.Constructor, positions, parent)

    override this.Name = "a constructor"
    override this.ShortName = literalCtor

    override this.Clone () =
        let ret = new FplConstructor((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Instantiate () = 
        Some (new FplInstance((this.StartPos, this.EndPos), this))

    override this.IsBlock () = true

type FplFunctionalTerm(positions: Positions, parent: FplValue) =
    inherit FplValue(FplBlockType.FunctionalTerm, positions, Some parent)

    override this.Name = "a functional term definition"
    override this.ShortName = "def func"

    override this.Clone () =
        let ret = new FplFunctionalTerm((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Instantiate () = None

    override this.IsFplBlock () = true
    override this.IsBlock () = true

type FplMandatoryFunctionalTerm(positions: Positions, parent: FplValue) =
    inherit FplValue(FplBlockType.MandatoryFunctionalTerm, positions, Some parent)

    override this.Name = "a functional term property"
    override this.ShortName = "mfunc"

    override this.Clone () =
        let ret = new FplMandatoryFunctionalTerm((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Instantiate () = None

    override this.IsBlock () = true

type FplOptionalFunctionalTerm(positions: Positions, parent: FplValue) =
    inherit FplValue(FplBlockType.OptionalFunctionalTerm, positions, Some parent)

    override this.Name = "an optional functional term property"
    override this.ShortName = "ofunc"

    override this.Clone () =
        let ret = new FplOptionalFunctionalTerm((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Instantiate () = None

    override this.IsBlock () = true

type FplPredicate(positions: Positions, parent: FplValue) =
    inherit FplGenericPredicateWithExpression(FplBlockType.Predicate, positions, parent)

    override this.Name = "a predicate definition"
    override this.ShortName = "def pred"

    override this.Clone () =
        let ret = new FplPredicate((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.IsFplBlock () = true
    override this.IsBlock () = true

type FplMandatoryPredicate(positions: Positions, parent: FplValue) =
    inherit FplGenericPredicate(FplBlockType.MandatoryPredicate, positions, parent)

    override this.Name = "a predicate property"
    override this.ShortName = "mpred"

    override this.Clone () =
        let ret = new FplMandatoryPredicate((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.IsBlock () = true


type FplOptionalPredicate(positions: Positions, parent: FplValue) =
    inherit FplGenericPredicate(FplBlockType.OptionalPredicate, positions, parent)

    override this.Name = "an optional predicate property"
    override this.ShortName = "opred"

    override this.Clone () =
        let ret = new FplOptionalPredicate((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.IsBlock () = true

type FplAxiom(positions: Positions, parent: FplValue) =
    inherit FplGenericPredicateWithExpression(FplBlockType.Todo, positions, parent)

    override this.Name = "an axiom"
    override this.ShortName = literalAx

    override this.Clone () =
        let ret = new FplAxiom((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.IsFplBlock () = true
    override this.IsBlock () = true

type FplTheorem(positions: Positions, parent: FplValue) =
    inherit FplGenericPredicateWithExpression(FplBlockType.Todo, positions, parent)

    override this.Name = "a theorem"
    override this.ShortName = literalThm

    override this.Clone () =
        let ret = new FplTheorem((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.IsFplBlock () = true
    override this.IsBlock () = true

type FplLemma(positions: Positions, parent: FplValue) =
    inherit FplGenericPredicateWithExpression(FplBlockType.Todo, positions, parent)

    override this.Name = "a lemma"
    override this.ShortName = literalLem

    override this.Clone () =
        let ret = new FplLemma((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.IsFplBlock () = true
    override this.IsBlock () = true

type FplProposition(positions: Positions, parent: FplValue) =
    inherit FplGenericPredicateWithExpression(FplBlockType.Proposition, positions, parent)

    override this.Name = "a proposition"
    override this.ShortName = literalProp

    override this.Clone () =
        let ret = new FplProposition((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.IsFplBlock () = true
    override this.IsBlock () = true

type FplConjecture(positions: Positions, parent: FplValue) =
    inherit FplGenericPredicateWithExpression(FplBlockType.Conjecture, positions, parent)

    override this.Name = "a conjecture"
    override this.ShortName = literalConj

    override this.Clone () =
        let ret = new FplConjecture((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.IsFplBlock () = true
    override this.IsBlock () = true

type FplCorollary(positions: Positions, parent: FplValue) =
    inherit FplGenericPredicateWithExpression(FplBlockType.Corollary, positions, parent)

    override this.Name = "a corollary"
    override this.ShortName = literalCor

    override this.Clone () =
        let ret = new FplCorollary((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.IsFplBlock () = true
    override this.IsBlock () = true

type FplProof(positions: Positions, parent: FplValue) =
    inherit FplGenericPredicate(FplBlockType.Proof, positions, parent)

    override this.Name = "a proof"
    override this.ShortName = literalPrf

    override this.Clone () =
        let ret = new FplProof((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.IsFplBlock () = true
    override this.IsBlock () = true
    override this.IsProof (): bool = true

type FplArgument(positions: Positions, parent: FplValue) =
    inherit FplGenericPredicate(FplBlockType.Argument, positions, parent)

    override this.Name = "an argument"
    override this.ShortName = "arg"

    override this.Clone () =
        let ret = new FplArgument((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

type FplJustification(positions: Positions, parent: FplValue) =
    inherit FplGenericPredicate(FplBlockType.Justification, positions, parent)

    override this.Name = "a justification"
    override this.ShortName = "just"

    override this.Clone () =
        let ret = new FplJustification((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Instantiate () = None

type FplArgInference(positions: Positions, parent: FplValue) =
    inherit FplGenericPredicate(FplBlockType.ArgInference, positions, parent)

    override this.Name = "an argument inference"
    override this.ShortName = "ainf"

    override this.Clone () =
        let ret = new FplArgInference((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

type FplLocalization(positions: Positions, parent: FplValue) =
    inherit FplValue(FplBlockType.Localization, positions, Some parent)

    override this.Name = "a localization"
    override this.ShortName = literalLoc

    override this.Clone () =
        let ret = new FplLocalization((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Instantiate () = None

    override this.Represent() = this.Type(SignatureType.Name)
        

type FplTranslation(positions: Positions, parent: FplValue) =
    inherit FplValue(FplBlockType.Translation, positions, Some parent)

    override this.Name = "a translation"
    override this.ShortName = "trsl"

    override this.Clone () =
        let ret = new FplTranslation((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Instantiate () = None

type FplLanguage(positions: Positions, parent: FplValue) =
    inherit FplValue(FplBlockType.Language, positions, Some parent)

    override this.Name = "a language"
    override this.ShortName = "lang"

    override this.Clone () =
        let ret = new FplLanguage((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Instantiate () = None

type FplReference(positions: Positions, parent: FplValue) =
    inherit FplValue(FplBlockType.Reference, positions, Some parent)

    override this.Name = "a reference"
    override this.ShortName = "ref"

    override this.Clone () =
        let ret = new FplReference((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Instantiate () = None

    override this.Represent (): string = 
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
                | (_, "", Some qual) -> sprintf "%s.%s" literalUndef (qual.Represent())
                | (_, "???", Some qual) ->
                    if this.HasBrackets then
                        sprintf "%s[].%s" literalUndef (qual.Represent())
                    else
                        sprintf "%s().%s" literalUndef (qual.Represent())
                | (_, _, Some qual) ->
                    if this.HasBrackets then
                        sprintf "%s[%s].%s" literalUndef args (qual.Represent())
                    else
                        sprintf "%s(%s).%s" literalUndef args (qual.Represent())
                | ("???", _, None) -> "()" 
                | ("", _, None) -> sprintf "%s" args
                | (_, "()", None) -> sprintf "%s()" literalUndef
                | (_, "", None) -> sprintf "%s" literalUndef
                | (_, "???", None) ->
                    if this.HasBrackets then
                        sprintf "%s[]" literalUndef
                    else
                        sprintf "%s()" literalUndef
                | (_, _, None) ->
                    if this.HasBrackets then sprintf "%s[%s]" literalUndef args
                    elif this.FplId = $"{literalByDef}." then sprintf "%s %s" literalByDef args
                    else sprintf "%s(%s)" literalUndef args

        else
            let subRepr = 
                this.ValueList
                |> Seq.map (fun subfv -> subfv.Represent())
                |> String.concat ", "
            if subRepr = String.Empty then 
                literalUndef
            else
                subRepr

type FplQuantor(positions: Positions, parent: FplValue) =
    inherit FplGenericPredicate(FplBlockType.Quantor, positions, parent)

    override this.Name = "a quantor"
    override this.ShortName = "qtr"

    override this.Clone () =
        let ret = new FplQuantor((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

type FplMapping(positions: Positions, parent: FplValue) =
    inherit FplValue(FplBlockType.Mapping, positions, Some parent)

    override this.Name = "a mapping"
    override this.ShortName = "map"

    override this.Clone () =
        let ret = new FplMapping((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Instantiate () = None

    override this.Represent() = $"dec {this.Type(SignatureType.Type)}"

type FplStmt(positions: Positions, parent: FplValue) =
    inherit FplValue(FplBlockType.Stmt, positions, Some parent)

    override this.Name = "a statement"
    override this.ShortName = "stmt"

    override this.Clone () =
        let ret = new FplStmt((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret
    override this.Instantiate () =
        if this.FplId = "bas" then
            // in case of a base class constructor call (that resides inside this that is a constructor)
            // identify the 
            let baseClassOpt = this.GetArgument
            match baseClassOpt with
            | Some (baseClass:FplValue) when baseClass.FplBlockType = FplBlockType.Class -> 
                Some (new FplInstance((this.StartPos, this.EndPos), baseClass.Parent.Value))
            | _ -> failwith ($"Cannot create an instance of a base class, missing constructor {this.Type(SignatureType.Mixed)}") 
        else
            None

type FplAssertion(positions: Positions, parent: FplValue) =
    inherit FplValue(FplBlockType.Assertion, positions, Some parent)

    override this.Name = "an assertion"
    override this.ShortName = literalAss

    override this.Clone () =
        let ret = new FplAssertion((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Instantiate () = None

type FplExtension(positions: Positions, parent: FplValue) =
    inherit FplValue(FplBlockType.Extension, positions, Some parent)

    override this.Name = "an extension"
    override this.ShortName = "def ext"

    override this.Clone () =
        let ret = new FplExtension((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Instantiate () = None

type FplIntrinsicInd(positions: Positions, parent: FplValue) as this =
    inherit FplValue(FplBlockType.IntrinsicInd, positions, Some parent)
    do 
        this.TypeId <- literalInd
        this.FplId <- literalInd


    override this.Name = "an intrinsic index"
    override this.ShortName = literalInd

    override this.Clone () =
        let ret = new FplIntrinsicInd((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Instantiate () = None

    override this.Represent (): string = this.FplId

type FplIntrinsicObj(positions: Positions, parent: FplValue) =
    inherit FplGenericObject(FplBlockType.IntrinsicObj, positions, parent)

    override this.Name = "an intrinsic object"
    override this.ShortName = literalObj

    override this.Clone () =
        let ret = new FplIntrinsicObj((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Instantiate () = 
        Some (new FplInstance((this.StartPos, this.EndPos), this.Parent.Value))

    override this.Represent (): string = this.FplId

type FplIntrinsicPred(positions: Positions, parent: FplValue) =
    inherit FplGenericPredicate(FplBlockType.IntrinsicPred, positions, parent)

    override this.Name = "an intrinsic predicate"
    override this.ShortName = literalPred

    override this.Clone () =
        let ret = new FplIntrinsicPred((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Represent (): string = this.FplId

type FplIntrinsicUndef(positions: Positions, parent: FplValue) as this =
    inherit FplValue(FplBlockType.IntrinsicUndef, positions, Some parent)
    do 
        this.TypeId <- literalUndef
        this.FplId <- literalUndef

    override this.Name = "an intrinsic undefined"
    override this.ShortName = literalUndef

    override this.Clone () =
        let ret = new FplIntrinsicUndef((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Instantiate () = None

    override this.Represent (): string = this.FplId

type FplIntrinsicFunc(positions: Positions, parent: FplValue) as this =
    inherit FplValue(FplBlockType.IntrinsicFunc, positions, Some parent)
    do
        this.TypeId <- literalFunc
        this.FplId <- literalFunc

    override this.Name = "an intrinsic functional term"
    override this.ShortName = literalFunc

    override this.Clone () =
        let ret = new FplIntrinsicFunc((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Instantiate () = None

    override this.Represent (): string = this.FplId

type FplIntrinsicTpl(positions: Positions, parent: FplValue) as this =
    inherit FplValue(FplBlockType.IntrinsicTpl, positions, Some parent)
    do
        this.TypeId <- literalTpl
        this.FplId <- literalTpl

    override this.Name = "an intrinsic template"
    override this.ShortName = literalTpl

    override this.Clone () =
        let ret = new FplIntrinsicTpl((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Instantiate () = None

    override this.Represent (): string = this.FplId

/// A discriminated union type for wrapping search results in the Scope of an FplValue.
type ScopeSearchResult =
    | FoundAssociate of FplValue
    | FoundMultiple of string
    | FoundIncorrectBlock of string
    | Found of FplValue
    | NotFound
    | NotApplicable

/// Qualified starting position of this FplValue
let qualifiedStartPos (fplValue:FplValue) =
    let rec getFullName (fv: FplValue) (first: bool) =
        let fvType = fv.Type(SignatureType.Mixed)

        if fv.IsRoot() then ""
        elif first then
            let starPosWithoutFileName =
                $"(Ln: {fv.StartPos.Line}, Col: {fv.StartPos.Column})"

            if fv.IsTheory() then
                getFullName fv.Parent.Value false + fvType + starPosWithoutFileName
            else
                getFullName fv.Parent.Value false + starPosWithoutFileName
        else if fv.IsTheory() then
            getFullName fv.Parent.Value false + fvType
        else
            getFullName fv.Parent.Value false

    getFullName fplValue true


/// A string representation of an FplValue
let toString (fplValue:FplValue) = $"{fplValue.ShortName} {fplValue.Type(SignatureType.Name)}"

/// Qualified name of this FplValue
let qualifiedName (fplValue:FplValue)=
    let rec getFullName (fv: FplValue) (first: bool) =
        let fplValueType =
            match fv.FplBlockType with
            | FplBlockType.Localization
            | FplBlockType.Reference -> fv.Type(SignatureType.Name)
            | FplBlockType.Localization
            | FplBlockType.Constructor
            | _ when fv.IsBlock() -> fv.Type(SignatureType.Mixed)
            | FplBlockType.Quantor -> fv.Type(SignatureType.Mixed)
            | _ -> fv.FplId

        match fv with
        | :? FplRoot -> ""
        | _ -> 
            if first then
                if fv.Parent.Value.IsRoot() then
                    getFullName fv.Parent.Value false + fplValueType
                else if fv.IsVariable() && not (fv.Parent.Value.IsVariable()) then
                    fplValueType
                else
                    getFullName fv.Parent.Value false + "." + fplValueType
            else if fv.Parent.Value.IsRoot() then
                getFullName fv.Parent.Value false + fplValueType
            else if fv.IsVariable() && not (fv.Parent.Value.IsVariable()) then
                fplValueType
            else
                getFullName fv.Parent.Value false + "." + fplValueType

    getFullName fplValue true


/// Checks if a block named name is in the scope of the fplValue' parent.
let inScopeOfParent (fplValue: FplValue) name =
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

    match fplValue.Parent with
    | Some parent ->
        if parent.Scope.ContainsKey(name) then
            let foundConflict = parent.Scope[name]
            ScopeSearchResult.Found foundConflict
        else if parent.IsTheory() then
            conflictInSiblingTheory parent
        else
            ScopeSearchResult.NotFound
    | None -> ScopeSearchResult.NotApplicable

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
        if fv.IsTheory() then 
            ScopeSearchResult.NotFound
        else
            match fv with 
            | :? FplTheorem 
            | :? FplLemma 
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
                match fv.FplBlockType with
                | FplBlockType.Constructor
                | FplBlockType.Localization
                | FplBlockType.Quantor
                | FplBlockType.MandatoryFunctionalTerm
                | FplBlockType.OptionalFunctionalTerm
                | FplBlockType.MandatoryPredicate
                | FplBlockType.OptionalPredicate
                | FplBlockType.Proof
                | FplBlockType.Corollary
                | FplBlockType.Proposition
                | FplBlockType.Corollary
                | FplBlockType.Conjecture
                | FplBlockType.Proof
                | FplBlockType.Extension
                | FplBlockType.Predicate
                | FplBlockType.FunctionalTerm
                | FplBlockType.Class ->
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
    if fplValue.FplBlockType = FplBlockType.Proof then
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
                    |> List.map (fun fv -> sprintf "%s %s" fv.Name (fv.Type(SignatureType.Mixed)))
                    |> String.concat ", "
                )
            elif provableBlocklist.Length > 0 then
                let potentialTheorem = provableBlocklist.Head
                ScopeSearchResult.FoundAssociate potentialTheorem
            elif notProvableBlocklist.Length > 0 then
                let potentialOther = notProvableBlocklist.Head

                ScopeSearchResult.FoundIncorrectBlock(
                    sprintf "%s %s" potentialOther.Name (qualifiedName potentialOther)
                )
            else
                ScopeSearchResult.NotFound
        | None -> ScopeSearchResult.NotApplicable
    else
        ScopeSearchResult.NotApplicable

/// Tries to find a theorem-like statement, a conjecture, or an axiom for a corollary
/// and returns different cases of ScopeSearchResult, depending on different semantical error situations.
let tryFindAssociatedBlockForCorollary (fplValue: FplValue) =

    if fplValue.FplBlockType = FplBlockType.Corollary then
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
                    |> List.map (fun fv -> sprintf "%s %s" fv.Name (fv.Type(SignatureType.Mixed)))
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
    else
        ScopeSearchResult.NotApplicable

/// Checks if the baseClassName is contained in the classRoot's base classes (it derives from).
/// If so, the function will produce Some path where path equals a string of base classes concatenated by ":".
/// The classRoot is required to have an FplValueType.Class.
let rec findClassInheritanceChain (classRoot: FplValue) (baseClassName: string) =
    let rootType = classRoot.Type(SignatureType.Type)

    match classRoot.FplBlockType with
    | FplBlockType.Class
    | FplBlockType.IntrinsicObj ->
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
                        (root.FplId = literalSelf || root.FplId = literalParent))
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
let findCandidatesByName (st: SymbolTable) (name: string) withClassConstructors =
    let pm = List<FplValue>()

    st.Root.Scope // iterate all theories
    |> Seq.iter (fun theory ->
        theory.Value.Scope
        // filter only blocks starting with the same FplId as the reference
        |> Seq.map (fun kvp -> kvp.Value)
        |> Seq.filter (fun fv -> fv.FplId = name)
        |> Seq.iter (fun (block: FplValue) ->
            pm.Add(block)

            if withClassConstructors && block.FplBlockType = FplBlockType.Class then
                block.Scope
                |> Seq.map (fun kvp -> kvp.Value)
                |> Seq.filter (fun (fv: FplValue) -> fv.FplBlockType = FplBlockType.Constructor)
                |> Seq.iter (fun (fv: FplValue) -> pm.Add(fv))))
    |> ignore

    pm |> Seq.toList


/// Looks for all declared properties or constructors (if any) that start with
/// the specific name within the building block, whose syntax tree the FplValue `fv` is part of.
let findCandidatesByNameInBlock (fv: FplValue) (name: string) =
    let rec findDefinition (fv1: FplValue) =
        if fv1.IsTheory() then
            ScopeSearchResult.NotFound
        else
            match fv1.FplBlockType with
            | FplBlockType.Class
            | FplBlockType.Predicate
            | FplBlockType.FunctionalTerm -> ScopeSearchResult.Found(fv1)
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
        match fv1.FplBlockType with
        | FplBlockType.Reference ->
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
        match candidate.FplBlockType with
        | FplBlockType.Variable ->
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

    if fv.IsTheory() then
        let name =
            if blocks.Count > 0 then
                let fv1 = blocks.Peek()
                $"{fv1.Name} {fv1.Type(SignatureType.Name)}"
            else
                "(no block found)"

        ScopeSearchResult.FoundIncorrectBlock name
    else
        match fv with
        | :? FplTheorem
        | :? FplLemma
        | :? FplAxiom 
        | :? FplRuleOfInference -> 
            let name = $"{fv.Name} {fv.Type(SignatureType.Name)}"
            ScopeSearchResult.FoundIncorrectBlock name
        | _ ->
            match fv.FplBlockType with
            | FplBlockType.Proposition
            | FplBlockType.Corollary
            | FplBlockType.Conjecture
            | FplBlockType.Proof
            | FplBlockType.Localization ->
                let name = $"{fv.Name} {fv.Type(SignatureType.Name)}"
                ScopeSearchResult.FoundIncorrectBlock name
            | FplBlockType.MandatoryPredicate
            | FplBlockType.OptionalPredicate
            | FplBlockType.MandatoryFunctionalTerm
            | FplBlockType.OptionalFunctionalTerm
            | FplBlockType.Predicate
            | FplBlockType.FunctionalTerm
            | FplBlockType.Class ->
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
                                $"{fv1.Name} {fv.Type(SignatureType.Name)}"
                            else
                                "(no block found)"

                        ScopeSearchResult.FoundMultiple name
            | _ ->
                let next = fv.Parent

                match next with
                | Some parent -> nextDefinition parent counter
                | None -> ScopeSearchResult.NotFound

/// Tries to match parameters of an FplValue with its arguments recursively
let rec mpwa (args: FplValue list) (pars: FplValue list) =
    match (args, pars) with
    | (a :: ars, p :: prs) ->
        let aType = a.Type(SignatureType.Type)
        let pType = p.Type(SignatureType.Type) 

        if aType = pType then
            mpwa ars prs
        elif pType.StartsWith(literalTpl) || pType.StartsWith("template") then
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
            && a.FplBlockType = FplBlockType.Reference
            && a.Scope.Count = 1
        then
            let var = a.Scope.Values |> Seq.toList |> List.head

            if var.Scope.ContainsKey(var.FplId) then
                let cl = var.Scope[var.FplId]

                match cl.FplBlockType with
                | FplBlockType.Class ->
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
        elif aType.StartsWith(literalFunc) then
            let someMap = a.Mapping

            match someMap with
            | Some map -> mpwa [ map ] [ p ]
            | _ -> Some($"`{a.Type(SignatureType.Name)}:{aType}` does not match `{p.Type(SignatureType.Name)}:{pType}`")
        else
            Some($"`{a.Type(SignatureType.Name)}:{aType}` does not match `{p.Type(SignatureType.Name)}:{pType}`")
    | ([], p :: prs) ->
        let pType = p.Type(SignatureType.Type)
        if p.IsClass() then
            let constructors = p.GetConstructors()
            if constructors.Length = 0 then
                None
            else
                Some($"missing argument for `{p.Type(SignatureType.Name)}:{pType}`")
        else
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

/// Tries to match the arguments of `fva` FplValue with the parameters of the `fvp` FplValue and returns
/// Some(specific error message) or None, if the match succeeded.
let matchArgumentsWithParameters (fva: FplValue) (fvp: FplValue) =
    let parameters =
        match fvp with
        | :? FplTheorem
        | :? FplLemma
        | :? FplAxiom
        | :? FplRuleOfInference ->
            fvp.Scope.Values |> Seq.filter (fun fv -> fv.IsSignatureVariable) |> Seq.toList
        | _ ->
            match fvp.FplBlockType with
            | FplBlockType.Proposition
            | FplBlockType.Corollary
            | FplBlockType.Predicate
            | FplBlockType.FunctionalTerm
            | FplBlockType.Conjecture
            | FplBlockType.Constructor
            | FplBlockType.MandatoryPredicate
            | FplBlockType.MandatoryFunctionalTerm
            | FplBlockType.OptionalPredicate
            | FplBlockType.OptionalFunctionalTerm ->
                fvp.Scope.Values |> Seq.filter (fun fv -> fv.IsSignatureVariable) |> Seq.toList
            | _ -> fvp.Scope.Values |> Seq.toList

    let arguments = fva.ArgList |> Seq.toList

    let stdMsg = $"{qualifiedName fvp}"
    let argResult = mpwa arguments parameters

    match argResult with
    | Some aErr -> Some($"{aErr} in {stdMsg}")
    | None -> None

let matchWithMapping (fva: FplValue) (fvp: FplValue) =
    let targetMapping = fvp.Mapping

    match targetMapping with
    | Some tm -> mpwa [ fva ] [ tm ]
    | None -> Some($"Btest")

/// Tries to match the signatures of toBeMatched with the signatures of all candidates and accoumulates any
/// error messages in accResultList.
let rec checkCandidates (toBeMatched: FplValue) (candidates: FplValue list) (accResultList: string list) =
    match candidates with
    | [] -> (None, accResultList)
    | candidate :: candidates ->
        match matchArgumentsWithParameters toBeMatched candidate with
        | None -> (Some candidate, [])
        | Some errMsg -> checkCandidates toBeMatched candidates (accResultList @ [ errMsg ])

let rec filterTreePathByBlockType (leaf: FplValue) (typ: FplBlockType) =
    if leaf.FplBlockType = typ then
        Some leaf
    else
        match leaf.Parent with
        | Some parent -> filterTreePathByBlockType parent typ
        | _ -> None

let searchExtensionByName (root: FplValue) identifier =
    let candidates =
        root.Scope
        |> Seq.map (fun theory ->
            theory.Value.Scope
            |> Seq.filter (fun kvp -> kvp.Value.FplBlockType = FplBlockType.Extension)
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
