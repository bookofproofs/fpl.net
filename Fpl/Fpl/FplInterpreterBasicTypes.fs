/// This module contains all abstract types used by the FplInterpreter classes

(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)
module FplInterpreterBasicTypes
open System
open System.Collections.Generic
open FplPrimitives
open FplGrammarTypes

open FParsec

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
let maxRecursion = 15

/// Checks if a string starts with a lower case character string
let checkStartsWithLowerCase (s:string) =
    if s.Length > 0 then 
        System.Char.IsLower(s[0])
    else
        false

type IVariable =
    abstract member IsSignatureVariable : bool with get, set
    abstract member IsInitialized : bool with get, set
    abstract member IsBound : bool with get

/// The interface IConstant is used to implement fixed but unknown objects of some type.
/// In general, the equality of two fixed but unknown objects of the same type cannot be determined, 
/// unless it is explicitly asserted (or explicitly negated) in the corresponding FPL theory.
/// However, once a ConstantName is established, the value will be treated like a fixed constant.
type IConstant =
    abstract member ConstantName : string with get
    abstract member SetConstantName: unit -> unit

type IHasSignature =
    abstract member SignStartPos : Position with get, set
    abstract member SignEndPos : Position with get, set

type ICanBeCalledRecusively =
    abstract member CallCounter : int

type IReady =
    abstract member IsReady : bool

type IHasProof =
    abstract member HasProof : bool with get, set

let stripLastDollarDigit (s: string) =
    let lastIndex = s.LastIndexOf('$')
    if lastIndex <> -1 then s.Substring(0, lastIndex) else s



[<AbstractClass>]
type FplGenericNode(positions: Positions, parent: FplGenericNode option) =
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
    let mutable _refersTo:FplGenericNode option = None

    let mutable _parent = parent
    let _scope = Dictionary<string, FplGenericNode>()
    let _argList = List<FplGenericNode>()

    /// A scope of this FplValue
    member this.Scope = _scope

    /// An argument list of this FplValue
    member this.ArgList = _argList

    abstract member Clone: unit -> FplGenericNode
    abstract member Copy : FplGenericNode -> unit
    abstract member AssignParts: FplGenericNode -> unit
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
    abstract member EmbedInSymbolTable: FplGenericNode option -> unit

    /// Abstract member for running this FplValue. 
    abstract member Run: unit -> unit

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

    /// A method used to issue diagnostics related to this FplValue and its structure retrieved during the FPL interpreter.
    abstract member CheckConsistency: unit -> unit

    (* Default implementations = everything is false, only the trues are overridden in derived classes *)
    default this.CheckConsistency() = ()
    
    default this.IsFplBlock () = false
    default this.IsBlock () = false
    default this.IsClass () = false
    default this.IsProof () = false
    default this.IsMapping () = false
    
    default this.AssignParts (ret:FplGenericNode) =
        ret.FplId <- this.FplId
        ret.TypeId <- this.TypeId
        ret.Arity <- this.Arity
        ret.AuxiliaryInfo <- this.AuxiliaryInfo
        ret.IsIntrinsic <- this.IsIntrinsic
        ret.ExpressionType <- this.ExpressionType
        ret.ArgType <- this.ArgType
        ret.RefersTo <- this.RefersTo

        this.Scope
        |> Seq.iter (fun (kvp:KeyValuePair<string, FplGenericNode>) ->
            let value = kvp.Value.Clone()
            ret.Scope.Add(kvp.Key, value))

        this.ArgList
        |> Seq.iter (fun (fv1:FplGenericNode) ->
            let value = fv1.Clone()
            ret.ArgList.Add(value))

    /// TypeId of the FplValue.
    member this.TypeId
        with get () = _typeId
        and set (value) = _typeId <- value

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

    /// A value string representation of all nodes in the symbol table
    /// constructed by the FPL interpreter. Returns 
    /// "None" - for all nodes in the symbol table that yield no value (e.g. statements).
    /// "undef" - for all nodes in the symbol table that yield a value, but whose value is undefined.
    /// "undetermined" - for all nodes in the symbol table that yield a value, but whose value could not be determined. 
    /// otherwise a string representation depending on type of the FPL node and its specific value.
    override this.Represent() = // done
        PrimNone

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
    default this.Copy(other: FplGenericNode) =
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

        this.RefersTo <- other.RefersTo

    /// Qualified starting position of this FplValue
    member this.QualifiedStartPos =
        let rec getFullName (fv: FplGenericNode) (first: bool) =
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
        let rec ultimateBlockNode (node:FplGenericNode) =
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
        let rec nextBlockNode (node:FplGenericNode) =
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
        let conflictInSiblingTheory (parent: FplGenericNode) =
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

/// a type wrapping the argument type of the FplValue 
and ArgType = 
    | Parentheses
    | Brackets
    | Nothing

/// A discriminated union type for wrapping search results in the Scope of an FplValue.
and ScopeSearchResult =
    | FoundAssociate of FplGenericNode
    | FoundMultiple of string
    | FoundIncorrectBlock of FplGenericNode
    | Found of FplGenericNode
    | NotFound
    | NotApplicable

[<AbstractClass>]
type FplGenericIsValue(positions: Positions, parent: FplGenericNode) =
    inherit FplGenericNode(positions, Some parent)

    override this.Represent() = // done
        PrimNone
    override this.RunOrder = None

[<AbstractClass>]
type FplGenericIsAction(positions: Positions, parent: FplGenericNode) =
    inherit FplGenericNode(positions, Some parent)

    override this.Represent() = // done
        PrimNone
    override this.RunOrder = None


/// Implements the semantics of a default undetermined value 
/// that has the required type of the consumer FplValue. If this the value of this consumer cannot be determined during the interpretation, 
/// its value will be set to this undermined value compatible with the consumer type.
/// FplUndetermined is not to be confused with FplIntrinsicUndef that is used for declaring partial mappings in FPL.
type FplUndetermined(typeId:string, positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericIsValue(positions, parent)
    do 
        this.FplId <- PrimUndetermined
        this.TypeId <- typeId

    override this.Name = PrimUndeterminedL
    override this.ShortName = PrimUndetermined

    override this.Clone () =
        let ret = new FplUndetermined(this.TypeId, (this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type (signatureType:SignatureType) = 
        match signatureType with 
        | SignatureType.Type -> this.TypeId
        | _ -> this.FplId
                    
    override this.Represent() = // done
        this.FplId 

    override this.Run() = 
        // run is not neccessary, since this node is are never referenced in the FPL syntax
        // Instead, we use them internally as default value of FplGenericHasValue
        ()

    override this.EmbedInSymbolTable _ = 
        // the embedding is not neccessary, since this node is are never referenced in the FPL syntax
        // Instead, we use them internally as default value of FplGenericHasValue
        () 

    override this.RunOrder = None

[<AbstractClass>]
type FplGenericHasValue(positions: Positions, parent: FplGenericNode) =
    inherit FplGenericNode(positions, Some parent)
    let mutable (_value:FplGenericNode option) = None

    /// Value of this FplValue
    member this.Value
        with get () = _value
        and set (value) = _value <- value

    /// Sets the value of this FplGenericHasValue to the provided value
    abstract member SetValue: FplGenericNode -> unit

    /// Sets the value of this FplGenericHasValue to the value of the provided node
    abstract member SetValueOf: FplGenericHasValue -> unit

    /// Sets the value of this FplGenericHasValue to the default value, based on its type
    abstract member SetDefaultValue: unit -> unit

    default this.SetValue fv =
        this.Value <- Some fv

    default this.SetValueOf fv =
        this.Value <-  fv.Value

    default this.SetDefaultValue() =
        this.SetValue (new FplUndetermined(this.TypeId, (this.StartPos, this.EndPos), this))

    override this.AssignParts (ret:FplGenericNode) = 
        base.AssignParts ret
        match ret with 
        | :? FplGenericHasValue as retWithValue ->
            retWithValue.Value <- this.Value
        | _ -> ()

    override this.Copy other =
        base.Copy other
        match other with 
        | :? FplGenericHasValue as otherWithValue ->
            this.Value <- otherWithValue.Value
        | _ -> ()

    override this.Represent() = // done
        match this.Value with 
        | Some v -> v.Represent() 
        | _ -> PrimNone // If there is no value, return string "None"

// Create an FplValue list containing all Scopes of an FplNode
let rec flattenScopes (root: FplGenericNode) =
    let rec helper (node: FplGenericNode) (acc: FplGenericNode list) =
        let newAcc = node :: acc
        node.Scope |> Seq.fold (fun acc kvp -> helper kvp.Value acc) newAcc

    helper root []
    
let getFplHead (fv:FplGenericNode) (signatureType:SignatureType) =
    match signatureType with
            | SignatureType.Name 
            | SignatureType.Mixed -> fv.FplId
            | SignatureType.Type -> fv.TypeId

let propagateSignatureType (signatureType:SignatureType) =
    match signatureType with
    | SignatureType.Mixed -> SignatureType.Type
    | _ -> signatureType 

/// Creates a concatenated string represenation based on a sequence of FplValues.
let signatureSep sep (coordinates:FplGenericNode seq) signatureType =
    coordinates
    |> Seq.map (fun fv -> fv.Type signatureType)
    |> String.concat sep

/// Tries to find a mapping of an FplValue
let rec getMapping (fv:FplGenericNode) =
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

/// Creates a concatenated string represenation based on a sequence of FplValues.
let representationSep sep (coordinates:FplGenericNode seq) =
    coordinates 
    |> Seq.map (fun fv -> fv.Represent())
    |> String.concat sep

/// A string representation of an FplValue
let toString (fplValue:FplGenericNode) = $"{fplValue.ShortName} {fplValue.Type(SignatureType.Name)}"

type IHasDotted = 
    abstract member DottedChild : FplGenericNode option with get, set

type ValidityReason =
    | IsAxiom of FplGenericNode
    | IsAsserted of FplGenericNode
    | IsAssumed of FplGenericNode
    | IsInferred of FplGenericNode
    | IsInferredFromRevocation of FplGenericNode
    | Error 

type ValidStatement =
    { Node: FplGenericNode
      ValidityReason: ValidityReason
      StatementExpression: string}

type IInferrable =
    abstract member GetInferrableExpression : ValidStatement with get
