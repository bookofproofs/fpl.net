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
    let _assertedPredicates = List<FplValue>()

    /// A list of asserted predicates for this FplValue
    member this.AssertedPredicates = _assertedPredicates

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

    /// Generates a type string identifier or type-specific naming convention of this FplValue.
    abstract member Type: SignatureType -> string
    
    /// Adds this FplValue to it's parent's ArgList, if such a Parent exists.
    abstract member TryAddToParentsArgList: unit -> unit

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

        this.AssertedPredicates.Clear()
        this.AssertedPredicates.AddRange(other.AssertedPredicates)

    override this.TryAddToParentsArgList () = 
        match this.Parent with 
        | Some parent -> parent.ArgList.Add(this)
        | _ -> ()
              
/// This type implements the functionality needed to "run" FPL statements step-by-step
/// while managing the storage of variables. FPL uses a call-by-value approach when it comes to 
/// replacing parameters by a calling function with arguments.
and FplVariableStack() = 
    let _stack = Stack<KeyValuePair<string, Dictionary<string,FplValue>>>()

    // The stack memory of the runner to store the variables of all run programs
    member this.Stack = _stack

    /// Copy the ValueList of the variadic ar to the ValueList of the variadic p
    /// by removing the previous values (if any) and
    /// inserting the clones of the elements.
    member this.ReplaceVariables (parameters:FplValue list) (arguments:FplValue list) =
        let replaceValues (p:FplValue) (ar:FplValue)  =
            (p.ValueList:List<FplValue>).Clear()
            ar.ValueList
            |> Seq.iter (fun (fv:FplValue) ->
                let fvClone = fv.Clone()
                p.ValueList.Add(fvClone)
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

type FplRoot() =
    inherit FplValue((Position("", 0, 1, 1), Position("", 0, 1, 1)), None)
    override this.Name = "root"
    override this.ShortName = "root"

    override this.Clone () =
        let ret = new FplRoot()
        this.AssignParts(ret)
        ret

    override this.Type _ = String.Empty
    override this.Represent () = literalUndef
    override this.TryAddToParentsArgList () = () 
    override this.Run variableStack = ()

/// Indicates if an FplValue is the root of the SymbolTable.
let isRoot (fv:FplValue) = 
    match fv with
    | :? FplRoot -> true
    | _ -> false

type FplTheory(positions: Positions, parent: FplValue, filePath: string) as this =
    inherit FplValue(positions, Some parent)
    do
        this.FilePath <- Some filePath

    override this.Name = "theory"
    override this.ShortName = "th"

    override this.Clone () =
        let ret = new FplTheory((this.StartPos, this.EndPos), this.Parent.Value, this.FilePath.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType =
        match signatureType with
        | SignatureType.Name 
        | SignatureType.Mixed -> this.FplId
        | SignatureType.Type -> this.TypeId

    override this.Represent () = literalUndef
    override this.Run variableStack = ()


/// Indicates if an FplValue is the root of the SymbolTable.
let isTheory (fv:FplValue) = 
    match fv with
    | :? FplTheory -> true
    | _ -> false

[<AbstractClass>]
type FplGenericPredicate(positions: Positions, parent: FplValue) as this =
    inherit FplValue(positions, Some parent)
    do 
        this.FplId <- literalUndetermined
        this.TypeId <- literalPred

    override this.Represent (): string = 
        this.ValueList
        |> Seq.map (fun subfv -> subfv.Represent())
        |> String.concat ", "

/// Implements the semantics of an FPL predicate prime predicate that is intrinsic.
/// It serves as a value for everything in FPL that is "predicative in nature". These can be predicates, theorem-like-statements, proofs or predicative expressions. The value can have one of three values in FPL: "true", literalFalse, and "undetermined". 
type FplIntrinsicPred(positions: Positions, parent: FplValue) =
    inherit FplGenericPredicate(positions, parent)

    override this.Name = $"{literalIntrL} {literalPredL}"
    override this.ShortName = literalPred

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

[<AbstractClass>]
type FplGenericPredicateWithExpression(positions: Positions, parent: FplValue) =
    inherit FplGenericPredicate(positions, parent)

    override this.Type signatureType = 
        let head = getFplHead this signatureType

        let paramT = getParamTuple this signatureType
        sprintf "%s(%s)" head paramT
            

    override this.Run variableStack = 
        raise (NotImplementedException())

[<AbstractClass>]
type FplGenericObject(positions: Positions, parent: FplValue) as this =
    inherit FplValue(positions, Some parent)

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
    inherit FplGenericPredicateWithExpression(positions, parent)

    override this.Name = $"rule of {literalInfL}"
    override this.ShortName = literalInf

    override this.Clone () =
        let ret = new FplRuleOfInference((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret
    override this.IsFplBlock () = true
    override this.IsBlock () = true    

    override this.Run variableStack = 
        raise (NotImplementedException())

type FplInstance(positions: Positions, parent: FplValue) =
    inherit FplGenericObject(positions, parent)

    override this.Name = "instance"
    override this.ShortName = "inst"

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
            literalUndef
        else
            subRepr

    override this.Run variableStack = ()

type FplConstructor(positions: Positions, parent: FplValue) =
    inherit FplGenericObject(positions, parent)

    override this.Name = literalCtorL
    override this.ShortName = literalCtor

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


let isConstructor (fv:FplValue) =
    match fv with
    | :? FplConstructor -> true
    | _ -> false

type FplClass(positions: Positions, parent: FplValue) =
    inherit FplGenericObject(positions, parent)

    override this.Name = $"{literalClL} {literalDefL}"
    override this.ShortName = $"{literalDef} {literalCl}"

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
        |> Seq.filter (fun fv -> isConstructor fv)
        |> Seq.toList
    
    override this.Type signatureType =
        match signatureType with
        | SignatureType.Name 
        | SignatureType.Mixed -> this.FplId
        | SignatureType.Type -> this.TypeId

    override this.Represent () = $"dec {literalCl} {this.FplId}"

    override this.Run _ = 
        this.SetValue(new FplInstance((this.StartPos, this.EndPos), this))


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

    override this.Name = $"{literalIntrL} {literalObjL}"
    override this.ShortName = literalObj

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

type FplPredicate(positions: Positions, parent: FplValue) =
    inherit FplGenericPredicateWithExpression(positions, parent)

    override this.Name = $"{literalPredL} {literalDefL}"
    override this.ShortName = $"{literalDef} {literalPred}"

    override this.Clone () =
        let ret = new FplPredicate((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.IsFplBlock () = true
    override this.IsBlock () = true


type FplMandatoryPredicate(positions: Positions, parent: FplValue) =
    inherit FplGenericPredicateWithExpression(positions, parent)

    override this.Name = $"{literalPredL} property"
    override this.ShortName = $"m{literalPred}"

    override this.Clone () =
        let ret = new FplMandatoryPredicate((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.IsBlock () = true

type FplOptionalPredicate(positions: Positions, parent: FplValue) =
    inherit FplGenericPredicateWithExpression(positions, parent)

    override this.Name = $"{literalOptL} {literalPredL} property"
    override this.ShortName = $"o{literalPred}"

    override this.Clone () =
        let ret = new FplOptionalPredicate((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.IsBlock () = true

type FplAxiom(positions: Positions, parent: FplValue) =
    inherit FplGenericPredicateWithExpression(positions, parent)

    override this.Name = literalAxL
    override this.ShortName = literalAx

    override this.Clone () =
        let ret = new FplAxiom((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.IsFplBlock () = true
    override this.IsBlock () = true



type FplTheorem(positions: Positions, parent: FplValue) =
    inherit FplGenericPredicateWithExpression(positions, parent)

    override this.Name = literalThmL
    override this.ShortName = literalThm

    override this.Clone () =
        let ret = new FplTheorem((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.IsFplBlock () = true
    override this.IsBlock () = true

type FplLemma(positions: Positions, parent: FplValue) =
    inherit FplGenericPredicateWithExpression(positions, parent)

    override this.Name = literalLemL
    override this.ShortName = literalLem

    override this.Clone () =
        let ret = new FplLemma((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.IsFplBlock () = true
    override this.IsBlock () = true

type FplProposition(positions: Positions, parent: FplValue) =
    inherit FplGenericPredicateWithExpression(positions, parent)

    override this.Name = literalPropL
    override this.ShortName = literalProp

    override this.Clone () =
        let ret = new FplProposition((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.IsFplBlock () = true
    override this.IsBlock () = true

type FplConjecture(positions: Positions, parent: FplValue) =
    inherit FplGenericPredicateWithExpression(positions, parent)

    override this.Name = literalConjL
    override this.ShortName = literalConj

    override this.Clone () =
        let ret = new FplConjecture((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.IsFplBlock () = true
    override this.IsBlock () = true

type FplCorollary(positions: Positions, parent: FplValue) =
    inherit FplGenericPredicateWithExpression(positions, parent)

    override this.Name = literalCorL
    override this.ShortName = literalCor

    override this.Clone () =
        let ret = new FplCorollary((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.IsFplBlock () = true
    override this.IsBlock () = true

type FplProof(positions: Positions, parent: FplValue) =
    inherit FplGenericPredicate(positions, parent)

    override this.Name = literalPrfL
    override this.ShortName = literalPrf

    override this.Clone () =
        let ret = new FplProof((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.IsFplBlock () = true
    override this.IsBlock () = true
    override this.IsProof (): bool = true

    override this.Type signatureType =
        let head = getFplHead this signatureType
        head

    override this.Run variableStack = 
        raise (NotImplementedException())

type FplArgument(positions: Positions, parent: FplValue) =
    inherit FplGenericPredicate(positions, parent)

    override this.Name = "argument"
    override this.ShortName = "arg"

    override this.Clone () =
        let ret = new FplArgument((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret
    
    override this.Type signatureType =
        let head = getFplHead this signatureType
        head

    override this.Run variableStack = 
        raise (NotImplementedException())

let isArgument (fv:FplValue) = 
    match fv with
    | :? FplArgument -> true
    | _ -> false

type FplJustification(positions: Positions, parent: FplValue) =
    inherit FplGenericPredicate(positions, parent)

    override this.Name = "justification"
    override this.ShortName = "just"

    override this.Clone () =
        let ret = new FplJustification((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType =
        let head = getFplHead this signatureType
        head

    override this.Run variableStack = 
        raise (NotImplementedException())

type FplArgInference(positions: Positions, parent: FplValue) =
    inherit FplGenericPredicate(positions, parent)

    override this.Name = "argument inference"
    override this.ShortName = "ainf"

    override this.Clone () =
        let ret = new FplArgInference((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType =
        let head = getFplHead this signatureType
        head

    override this.Run variableStack = 
        raise (NotImplementedException())

type FplLocalization(positions: Positions, parent: FplValue) =
    inherit FplValue(positions, Some parent)

    override this.Name = literalLocL
    override this.ShortName = literalLoc

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
        
    override this.Run variableStack = 
        raise (NotImplementedException())

type FplTranslation(positions: Positions, parent: FplValue) =
    inherit FplValue(positions, Some parent)

    override this.Name = "translation"
    override this.ShortName = "trsl"

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
        raise (NotImplementedException())

type FplLanguage(positions: Positions, parent: FplValue) =
    inherit FplValue(positions, Some parent)

    override this.Name = "language"
    override this.ShortName = "lang"

    override this.Clone () =
        let ret = new FplLanguage((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType =
        let head = getFplHead this signatureType
        head

    override this.Represent () = this.FplId

    override this.Run variableStack = 
        raise (NotImplementedException())

let isLanguage (fv:FplValue) =
    match fv with
    | :? FplLanguage -> true
    | _ -> false

type FplAssertion(positions: Positions, parent: FplValue) =
    inherit FplValue(positions, Some parent)

    override this.Name = "assertion"
    override this.ShortName = literalAss

    override this.Clone () =
        let ret = new FplAssertion((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = this.FplId
        
    override this.Represent () = ""

    override this.Run variableStack = 
        raise (NotImplementedException())

type FplIntrinsicUndef(positions: Positions, parent: FplValue) as this =
    inherit FplValue(positions, Some parent)
    do 
        this.TypeId <- literalUndef
        this.FplId <- literalUndef

    override this.Name = $"{literalIntrL} {literalUndefL}"
    override this.ShortName = literalUndef

    override this.Clone () =
        let ret = new FplIntrinsicUndef((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type (signatureType:SignatureType) = 
        getFplHead this signatureType
                    
    override this.Represent (): string = this.FplId

    override this.Run _ = ()

type FplReference(positions: Positions, parent: FplValue) =
    inherit FplValue(positions, Some parent)

    override this.Name = "reference"
    override this.ShortName = "ref"

    override this.Clone () =
        let ret = new FplReference((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

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
                elif head = "bydef." then sprintf "%s%s" head args
                else sprintf "%s(%s)" head args


    override this.Represent (): string = 
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


    override this.Run variableStack =
        if this.Scope.Count > 0 then 
            let called = 
                this.Scope 
                |> Seq.map (fun kvp -> kvp.Value) 
                |> Seq.toList 
                |> List.head
            if called.IsBlock() then
                let pars = variableStack.SaveVariables(called) 
                let args = this.ArgList |> Seq.toList
                variableStack.ReplaceVariables pars args
                let lastRepr = new FplRoot()
                // run all statements of the called node
                called.ArgList
                |> Seq.iter (fun fv -> 
                    fv.Run variableStack
                )
                this.SetValuesOf called
                variableStack.RestoreVariables(called)
        elif this.ArgList.Count = 1 then
            let arg = this.ArgList[0]
            this.SetValuesOf arg
        else
            let undef = new FplIntrinsicUndef((this.StartPos, this.EndPos), this)
            this.SetValue(undef)

let isReference (fv:FplValue) =
    match fv with
    | :? FplReference -> true
    | _ -> false

/// Implements the semantics of an FPL conjunction compound predicate.
type FplConjunction(positions: Positions, parent: FplValue) as this =
    inherit FplGenericPredicate(positions, parent)

    do 
        this.FplId <- literalAnd

    override this.Name = "conjunction"
    override this.ShortName = literalAnd

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

    override this.Run _ = 
        let arg1 = this.ArgList[0]
        let arg2 = this.ArgList[1]
        let arg1Repr = arg1.Represent()
        let arg2Repr = arg2.Represent()
        let newValue =  new FplIntrinsicPred((this.StartPos, this.EndPos), this)
        newValue.FplId <-
            // FPL truth-table
            match (arg1Repr, arg2Repr) with
            | (FplGrammarCommons.literalFalse, _) 
            | (_, FplGrammarCommons.literalFalse)  -> 
                FplGrammarCommons.literalFalse
            | (FplGrammarCommons.literalTrue, FplGrammarCommons.literalTrue) -> 
                literalTrue
            | _ -> literalUndetermined
        this.SetValue(newValue)

/// Implements the semantics of an FPL disjunction compound predicate.
type FplDisjunction(positions: Positions, parent: FplValue) as this =
    inherit FplGenericPredicate(positions, parent)

    do 
        this.FplId <- literalOr

    override this.Name = "disjunction"
    override this.ShortName = literalOr

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

    override this.Run _ = 
        let arg1 = this.ArgList[0]
        let arg2 = this.ArgList[1]
        let arg1Repr = arg1.Represent()
        let arg2Repr = arg2.Represent()
        let newValue =  new FplIntrinsicPred((this.StartPos, this.EndPos), this)
        newValue.FplId <-
            // FPL truth-table
            match (arg1Repr, arg2Repr) with
            | (FplGrammarCommons.literalTrue, _) 
            | (_, FplGrammarCommons.literalTrue) -> 
                literalTrue
            | (FplGrammarCommons.literalFalse, FplGrammarCommons.literalFalse) -> 
                literalFalse
            | _ -> 
                literalUndetermined
        this.SetValue(newValue)  

/// Implements the semantics of an FPL xor compound predicate.
type FplExclusiveOr(positions: Positions, parent: FplValue) as this =
    inherit FplGenericPredicate(positions, parent)

    do 
        this.FplId <- literalXor

    override this.Name = "exclusive disjunction"
    override this.ShortName = literalXor

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

    override this.Run _ = 
        let arg1 = this.ArgList[0]
        let arg2 = this.ArgList[1]
        let arg1Repr = arg1.Represent()
        let arg2Repr = arg2.Represent()
        let newValue =  new FplIntrinsicPred((this.StartPos, this.EndPos), this)

        newValue.FplId <- 
            // FPL truth-table
            match (arg1Repr, arg2Repr) with
            | (FplGrammarCommons.literalTrue, FplGrammarCommons.literalFalse) 
            | (FplGrammarCommons.literalFalse, FplGrammarCommons.literalTrue) -> 
                literalTrue
            | (FplGrammarCommons.literalTrue, FplGrammarCommons.literalTrue) 
            | (FplGrammarCommons.literalFalse, FplGrammarCommons.literalFalse) -> 
                literalFalse
            | _ -> 
                literalUndetermined

        this.SetValue(newValue)  


/// Implements the semantics of an FPL negation compound predicate.
type FplNegation(positions: Positions, parent: FplValue) as this =
    inherit FplGenericPredicate(positions, parent)

    do 
        this.FplId <- literalNot

    override this.Name = "negation"
    override this.ShortName = literalNot

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
            | FplGrammarCommons.literalFalse -> literalTrue
            | FplGrammarCommons.literalTrue -> literalFalse
            | _ -> literalUndetermined  

        this.SetValue(newValue)  

/// Implements the semantics of an FPL implication compound predicate.
type FplImplication(positions: Positions, parent: FplValue) as this =
    inherit FplGenericPredicate(positions, parent)

    do 
        this.FplId <- literalImpl

    override this.Name = "implication"
    override this.ShortName = literalImpl

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
            | (FplGrammarCommons.literalTrue, FplGrammarCommons.literalFalse) -> literalFalse
            | (FplGrammarCommons.literalFalse, FplGrammarCommons.literalTrue) 
            | (FplGrammarCommons.literalFalse, FplGrammarCommons.literalFalse) 
            | (FplGrammarCommons.literalTrue, FplGrammarCommons.literalTrue) -> literalTrue
            | _ -> literalUndetermined
        
        this.SetValue(newValue)  

/// Implements the semantics of an FPL equivalence compound predicate.
type FplEquivalence(positions: Positions, parent: FplValue) as this =
    inherit FplGenericPredicate(positions, parent)

    do 
        this.FplId <- literalIif

    override this.Name = "equivalence"
    override this.ShortName = literalIif

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

    override this.Run _ = 
        let arg1 = this.ArgList[0]
        let arg2 = this.ArgList[1]
        let arg1Repr = arg1.Represent()
        let arg2Repr = arg2.Represent()
        let newValue =  new FplIntrinsicPred((this.StartPos, this.EndPos), this)
        newValue.FplId <- 
            match (arg1Repr, arg2Repr) with
            // FPL truth-table
            | (FplGrammarCommons.literalTrue, FplGrammarCommons.literalTrue) 
            | (FplGrammarCommons.literalFalse, FplGrammarCommons.literalFalse) -> literalTrue
            | (FplGrammarCommons.literalFalse, FplGrammarCommons.literalTrue) 
            | (FplGrammarCommons.literalTrue, FplGrammarCommons.literalFalse) -> literalFalse
            | _ -> literalUndetermined
        
        this.SetValue(newValue)  

/// Implements the semantics of an FPL equality.
type FplEquality(positions: Positions, parent: FplValue) as this =
    inherit FplGenericPredicate(positions, parent)

    do 
        this.FplId <- $"{literalDel}."
        this.TypeId <- literalPred

    override this.Name = "equality"
    override this.ShortName = "="

    override this.Clone () =
        let ret = new FplEquality((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    member this.Copy(other) =
        base.Copy(other)
        this.TypeId <- literalPred

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
        | FplGrammarCommons.literalUndef -> 
            emitID013Diagnostics this.StartPos this.EndPos "Predicate `=` cannot be evaluated because the left argument is undefined." 
            this.SetValue(newValue)
        | _ -> 
            match b1Repr with
            | FplGrammarCommons.literalUndef -> 
                emitID013Diagnostics this.StartPos this.EndPos "Predicate `=` cannot be evaluated because the right argument is undefined." 
                this.SetValue(newValue)
            | _ -> 
                let newValue = FplIntrinsicPred((this.StartPos, this.EndPos), this.Parent.Value)
                match a1Repr with
                | "dec pred"  
                | FplGrammarCommons.literalUndetermined -> 
                    emitID013Diagnostics this.StartPos this.EndPos "Predicate `=` cannot be evaluated because the left argument is undetermined." 
                    this.SetValue(newValue)
                | _ -> 
                    match b1Repr with
                    | "dec pred"  
                    | FplGrammarCommons.literalUndetermined -> 
                        emitID013Diagnostics this.StartPos this.EndPos "Predicate `=` cannot be evaluated because the right argument is undetermined." 
                        this.SetValue(newValue)
                    | _ -> 
                        newValue.FplId <- $"{(a1Repr = b1Repr)}" 
                        this.SetValue(newValue)

/// Implements an object that is used to provide a representation of extensions in FPL.
type FplExtensionObj(positions: Positions, parent: FplValue) as this =
    inherit FplValue(positions, Some parent)

    do 
        this.TypeId <- literalObj


    override this.Name = $"{literalExtL} {literalObjL}"
    override this.ShortName = literalObj

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
            literalUndef
        else
            subRepr

    override this.Run _ = ()


/// Implements the semantics of an FPL decrement delegate.
type FplDecrement(positions: Positions, parent: FplValue) as this =
    inherit FplValue(positions, Some parent)

    do 
        this.FplId <- $"{literalDel}."

    override this.Name = "decrement"
    override this.ShortName = "decr"

    override this.Clone () =
        let ret = new FplEquality((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    member this.Copy(other) =
        base.Copy(other)
        this.TypeId <- literalObj

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
            literalUndef
        else
            subRepr        

    override this.Run _ = 
        if this.ArgList.Count <> 1 then 
            this.Diagnostic $"Decrement takes 1 arguments, got {this.ArgList.Count}." 
        else


        let arg = this.ArgList[0]

        let newValue = FplExtensionObj((this.StartPos, this.EndPos), this.Parent.Value)
        let n = int arg.FplId
        let n' = n - 1
        newValue.FplId <- 
            if n' < 0 then 
                ""
            else
                string n'
        this.SetValue(newValue)

type FplMapping(positions: Positions, parent: FplValue) =
    inherit FplValue(positions, Some parent)

    override this.Name = "mapping"
    override this.ShortName = "map"

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
        elif aType.StartsWith(literalFunc) then
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
        this.FplId <- literalIs

    override this.Name = "is operator"
    override this.ShortName = literalIs

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
            | Some errMsg -> literalFalse
            | None -> literalTrue
        
        this.SetValue(newValue)  

type FplQuantor(positions: Positions, parent: FplValue) =
    inherit FplGenericPredicate(positions, parent)

    override this.Name = "quantor"
    override this.ShortName = "qtr"

    override this.Clone () =
        let ret = new FplQuantor((this.StartPos, this.EndPos), this.Parent.Value)
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

    override this.Run _ = 
        raise (NotImplementedException())

type FplVariable(positions: Positions, parent: FplValue) =
    inherit FplValue(positions, Some parent)
    let mutable _variadicType = String.Empty // "" = variable, "many" = many, "many1" = many1 
    override this.Name = 
        match _variadicType with
        | "many" -> "zero-or-more variable"
        | "many1"-> "one-or-more variable"
        | _ -> "variable"
    override this.ShortName = 
        match _variadicType with
        | "many" -> "*var"
        | "many1"-> "+var"
        | _ -> "var"

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
                literalUndef
            else
                match this.TypeId with
                | FplGrammarCommons.literalUndef -> literalUndef
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
                | FplGrammarCommons.literalUndef -> literalUndef
                | _ -> 
                    if this.IsVariadic() then
                        $"dec {this.Type(SignatureType.Type)}[]" 
                    else
                        $"dec {this.Type(SignatureType.Type)}" 

    override this.Run _ = ()


[<AbstractClass>]
type FplGenericFunctionalTerm(positions: Positions, parent: FplValue) =
    inherit FplValue(positions, Some parent)

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
                literalUndef
            else
                subRepr

    override this.Run variableStack = 
        raise (NotImplementedException())

type FplFunctionalTerm(positions: Positions, parent: FplValue) =
    inherit FplGenericFunctionalTerm(positions, parent)

    override this.Name = $"functional term {literalDefL}"
    override this.ShortName = $"{literalDef} {literalFunc}"

    override this.Clone () =
        let ret = new FplFunctionalTerm((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.IsFplBlock () = true
    override this.IsBlock () = true

type FplMandatoryFunctionalTerm(positions: Positions, parent: FplValue) =
    inherit FplGenericFunctionalTerm(positions, parent)

    override this.Name = "functional term property"
    override this.ShortName = $"m{literalFunc}"

    override this.Clone () =
        let ret = new FplMandatoryFunctionalTerm((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.IsBlock () = true

type FplOptionalFunctionalTerm(positions: Positions, parent: FplValue) =
    inherit FplGenericFunctionalTerm(positions, parent)

    override this.Name = $"{literalOptL} functional term property"
    override this.ShortName = $"o{literalFunc}"

    override this.Clone () =
        let ret = new FplOptionalFunctionalTerm((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.IsBlock () = true

type FplExtension(positions: Positions, parent: FplValue) =
    inherit FplValue(positions, Some parent)

    override this.Name = literalExtL
    override this.ShortName = $"{literalDef} {literalExt}"

    override this.Clone () =
        let ret = new FplExtension((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = this.FplId

    override this.Represent () = this.FplId

    override this.Run variableStack = 
        raise (NotImplementedException())

let isExtension (fv:FplValue) =
    match fv with
    | :? FplExtension -> true
    | _ -> false

type FplIntrinsicInd(positions: Positions, parent: FplValue) as this =
    inherit FplValue(positions, Some parent)
    do 
        this.TypeId <- literalInd
        this.FplId <- literalInd


    override this.Name = $"{literalIntrL} {literalIndL}"
    override this.ShortName = literalInd

    override this.Clone () =
        let ret = new FplIntrinsicInd((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type (signatureType:SignatureType) = 
        getFplHead this signatureType
                    
    override this.Represent (): string = this.FplId

    override this.Run _ = ()

type FplIntrinsicFunc(positions: Positions, parent: FplValue) as this =
    inherit FplValue(positions, Some parent)
    do
        this.TypeId <- literalFunc
        this.FplId <- literalFunc

    override this.Name = $"{literalIntrL} functional term"
    override this.ShortName = literalFunc

    override this.Clone () =
        let ret = new FplIntrinsicFunc((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type (signatureType:SignatureType) = 
        getFplHead this signatureType
                    
    override this.Represent (): string = this.FplId

    override this.Run _ = () 


type FplIntrinsicTpl(positions: Positions, parent: FplValue) as this =
    inherit FplValue(positions, Some parent)
    do
        this.TypeId <- literalTpl
        this.FplId <- literalTpl

    override this.Name = $"{literalIntrL} {literalTplL}"
    override this.ShortName = literalTpl

    override this.Clone () =
        let ret = new FplIntrinsicTpl((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type (signatureType:SignatureType) = 
        getFplHead this signatureType
                    
    override this.Represent (): string = this.FplId

    override this.Run _ = () 

type FplStmt(positions: Positions, parent: FplValue) =
    inherit FplValue(positions, Some parent)

    override this.Name = "statement"
    override this.ShortName = "stmt"

    override this.Clone () =
        let ret = new FplStmt((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    //override this.Instantiate () =
    //    if this.FplId = "bas" then
    //        // in case of a base class constructor call (that resides inside this that is a constructor)
    //        // identify the 

    //        let baseClassOpt = 
    //            if this.ArgList.Count > 0 then
    //                let test = this.ArgList[0]
    //                // in case of a base.obj() constructor call
    //                if test.ArgList.Count = 2 && 
    //                    isIntrinsicObj test.ArgList[0] && 
    //                    isReference test.ArgList[1] &&
    //                    test.ArgList[1].FplId = "???" then
    //                    // return an FplValue inbuilt Object 
    //                    Some  test.ArgList[0] 
    //                elif test.ArgList.Count > 0 then
    //                    Some test 
    //                else
    //                    None
    //            else 
    //                None

    //        match baseClassOpt with
    //        | Some (baseClass:FplValue) when baseClass.IsClass() -> 
    //            Some (new FplInstance((this.StartPos, this.EndPos), baseClass.Parent.Value))
    //        | _ -> failwith ($"Cannot create an instance of a base class, missing constructor {this.Type(SignatureType.Mixed)}") 
    //    else
    //        None

    override this.Type signatureType = this.FplId
    override this.Represent () = ""

    override this.Run variableStack = 
        raise (NotImplementedException())

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
        if refValue.ArgList.Count > 0 && not (refValue.IsClass()) && not (isConstructor refValue) then
            Some refValue.ArgList[0] // return existing values except of classes, because those denoted their parent classes
        else 
            Some refValue 
    | :? FplReference when fv.FplId <> "" -> Some fv
    | :? FplReference when fv.ArgList.Count = 0 -> Some fv
    | _ when fv.ArgList.Count > 0 -> Some fv.ArgList[0]
    | _ -> None

/// Implements the return statement in FPL.
type FplReturn(positions: Positions, parent: FplValue) as this =
    inherit FplValue(positions, Some parent)

    do
        this.FplId <- literalRet
        this.TypeId <- literalUndef

    override this.Name = $"{literalRetL} statement"
    override this.ShortName = "stmt"

    override this.Clone () =
        let ret = new FplReturn((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    //override this.Instantiate () =
    //    if this.FplId = "bas" then
    //        // in case of a base class constructor call (that resides inside this that is a constructor)
    //        // identify the 

    //        let baseClassOpt = 
    //            if this.ArgList.Count > 0 then
    //                let test = this.ArgList[0]
    //                // in case of a base.obj() constructor call
    //                if test.ArgList.Count = 2 && 
    //                    isIntrinsicObj test.ArgList[0] && 
    //                    isReference test.ArgList[1] &&
    //                    test.ArgList[1].FplId = "???" then
    //                    // return an FplValue inbuilt Object 
    //                    Some  test.ArgList[0] 
    //                elif test.ArgList.Count > 0 then
    //                    Some test 
    //                else
    //                    None
    //            else 
    //                None

        //    match baseClassOpt with
        //    | Some (baseClass:FplValue) when baseClass.IsClass() -> 
        //        Some (new FplInstance((this.StartPos, this.EndPos), baseClass.Parent.Value))
        //    | _ -> failwith ($"Cannot create an instance of a base class, missing constructor {this.Type(SignatureType.Mixed)}") 
        //else
        //    None

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
    inherit FplValue(positions, Some parent)

    do
        this.FplId <- $"assign (ln {this.StartPos.Line})"
        this.TypeId <- literalUndef

    override this.Name = $"assigment statement"
    override this.ShortName = "stmt"

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
        | Some value when isConstructor value ->
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

    override this.Run variableStack = 
        let assigneeReferenceOpt = getArgument this.ArgList[0]
        let assignedValueOpt = getArgument this.ArgList[1]
        match assigneeReferenceOpt, assignedValueOpt with
        | Some assignee, Some assignedValue ->
            this.CheckSIG05Diagnostics assignee assignedValue
            assignedValue.Run variableStack
            assignee.SetValuesOf assignedValue
            match assignee with
            | :? FplVariable -> assignee.IsInitializedVariable <- true
            | _ -> ()
        | _ -> ()

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

        if isRoot fv then ""
        elif first then
            let starPosWithoutFileName =
                $"(Ln: {fv.StartPos.Line}, Col: {fv.StartPos.Column})"

            if isTheory fv then
                getFullName fv.Parent.Value false + fvType + starPosWithoutFileName
            else
                getFullName fv.Parent.Value false + starPosWithoutFileName
        else if isTheory fv then
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
            | :? FplQuantor -> fv.Type(SignatureType.Mixed)
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
        else if isTheory parent then
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
                | :? FplQuantor
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

            if withClassConstructors && block.IsClass() then
                block.Scope
                |> Seq.map (fun kvp -> kvp.Value)
                |> Seq.filter (fun (fv: FplValue) -> isConstructor fv)
                |> Seq.iter (fun (fv: FplValue) -> pm.Add(fv))))
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
