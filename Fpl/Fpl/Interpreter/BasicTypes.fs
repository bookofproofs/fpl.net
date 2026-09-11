/// <summary>
/// This module contains all abstract types used by nodes that are listed in the symbol table of the FPL interpreter.
/// </summary>

(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)
module Fpl.Interpreter.BasicTypes
open System.Collections.Generic
open Fpl.Primitives
open Fpl.Parser.Types

open FParsec

/// <summary>
/// Discriminated union representing the fixity of an operator or literal in FPL.
/// </summary>
/// <remarks>
/// Cases carry the user-visible symbol and precedence where applicable.
/// </remarks>
type FixType =
    | Infix of string * int
    | Postfix of string
    | Prefix of string
    | Symbol of string
    | Paren
    | NoFix

    /// <summary>
    /// Human-readable representation of the fixity suitable for messages.
    /// </summary>
    member this.Type =
        match this with
        | Infix(symbol, precedence) -> sprintf "infix `%s` (with precedence `%i`)" symbol precedence
        | Postfix symbol -> sprintf "postfix `%s` " symbol
        | Prefix symbol -> sprintf "prefix `%s` " symbol
        | Symbol symbol -> sprintf "symbol `%s`" symbol
        | Paren -> "parens"
        | NoFix -> "no fix"

    /// <summary>
    /// Return the user-defined literal for this fixity or a default symbol when not applicable.
    /// </summary>
    /// <param name="defaultSymbol">Fallback symbol to return for non-user-defined fixities.</param>
    /// <returns>User-defined literal or <paramref name="defaultSymbol"/>.</returns>
    member this.GetUserDefinedLiteral defaultSymbol =
        match this with
        | Infix(symbol, _) -> symbol 
        | Postfix symbol -> symbol
        | Prefix symbol -> symbol
        | Symbol symbol -> symbol
        | Paren -> defaultSymbol
        | NoFix -> defaultSymbol

/// <summary>
/// Kind of signature requested when resolving a node's identity.
/// </summary>
type SignatureType =
    | Name
    | Type
    | Mixed

/// <summary>
/// Maximum allowed recursion depth for interpreter node calls.
/// </summary>
let maxRecursion = 15

/// <summary>
/// Check whether the provided string starts with a lowercase character.
/// </summary>
/// <param name="s">Input string to test.</param>
/// <returns>True when the first character exists and is lowercase; otherwise false.</returns>
let checkStartsWithLowerCase (s:string) =
    if s.Length > 0 then 
        System.Char.IsLower(s[0])
    else
        false

/// <summary>
/// Represents a mutable variable abstraction observed by the interpreter.
/// </summary>
type IVariable =
    abstract member IsSignatureVariable : bool with get, set
    abstract member IsInitialized : bool with get, set
    abstract member IsBound : bool with get

/// <summary>
/// Interface for constants that represent fixed but unknown objects in the interpreter.
/// </summary>
type IConstant =
    abstract member ConstantName : string with get
    abstract member SetConstantName: unit -> unit

/// <summary>
/// Interface for nodes that carry signature position information.
/// </summary>
type IHasSignature =
    abstract member SignStartPos : Position with get, set
    abstract member SignEndPos : Position with get, set

/// <summary>
/// Interface for nodes that can be tracked for recursive calls.
/// </summary>
type ICanBeCalledRecusively =
    abstract member CallCounter : int

/// <summary>
/// Simple interpretation readiness marker interface.
/// </summary>
type IReady =
    abstract member IsReady : bool

/// <summary>
/// Interface indicating the node carries a proof.
/// </summary>
type IHasProof =
    abstract member HasProof : bool with get, set

/// <summary>
/// Remove the trailing dollar-suffixed digit from an identifier, if present.
/// </summary>
/// <param name="s">Identifier to strip.</param>
/// <returns>Identifier without trailing dollar-digit suffix.</returns>
let stripLastDollarDigit (s: string) =
    let lastIndex = s.LastIndexOf('$')
    if lastIndex <> -1 then s.Substring(0, lastIndex) else s


/// <summary>
/// Base abstract class for all interpreter symbol-table nodes.
/// </summary>
/// <param name="positions">Tuple of start and end positions for the node.</param>
/// <param name="parent">Optional parent node in the symbol tree.</param>
[<AbstractClass>]
type FplGenericNode(positions: Positions, parent: FplGenericNode option) =
    let mutable _expressionType = FixType.NoFix
    let mutable _argType = ArgType.Nothing
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

    /// <summary>Scope dictionary containing children keyed by name.</summary>
    member this.Scope = _scope

    /// <summary>Argument list of this node.</summary>
    member this.ArgList = _argList

    /// <summary>Create a deep clone of this node.</summary>
    abstract member Clone: unit -> FplGenericNode

    /// <summary>Copy contents from another node into this node.</summary>
    /// <param name="other">Source node to copy from.</param>
    abstract member Copy : FplGenericNode -> unit

    /// <summary>Assign common parts from another node into this node.</summary>
    /// <param name="other">Source node providing parts to assign.</param>
    abstract member AssignParts: FplGenericNode -> unit

    /// <summary>Short name used when printing this node (e.g. in error messages).</summary>
    abstract member ShortName: string

    /// <summary>Formal name of the node.</summary>
    abstract member Name: string

    /// <summary>String representation (serialization) of the node that can be used for equality comparison.</summary>
    abstract member Represent: unit -> string

    /// <summary>Optional run order used by interpreter execution scheduling.</summary>
    abstract member RunOrder: int option

    /// <summary>Resolve and return the type identifier according to the requested signature kind.</summary>
    /// <param name="signatureType">Kind of signature to resolve.</param>
    /// <returns>Type identifier or name depending on <paramref name="signatureType"/>.</returns>
    abstract member Type: SignatureType -> string

    /// <summary>Embed this node into the symbol table, optionally under a provided predecessor.</summary>
    /// <param name="predecessor">Optional predecessor node used for embedding.</param>
    abstract member EmbedInSymbolTable: FplGenericNode option -> unit

    /// <summary>Execute any interpretation actions associated with this node.</summary>
    abstract member Run: unit -> unit

    /// <summary>Query whether this node is a block, property, or constructor.</summary>
    abstract member IsFplBlock: unit -> bool

    /// <summary>Query whether this node is an FPL building block.</summary>
    abstract member IsBlock: unit -> bool

    /// <summary>Query whether this node represents a class.</summary>
    abstract member IsClass: unit -> bool

    /// <summary>Query whether this node represents a proof.</summary>
    abstract member IsProof: unit -> bool

    /// <summary>Query whether this node represents a mapping.</summary>
    abstract member IsMapping: unit -> bool

    /// <summary>Perform consistency checks and emit diagnostics if necessary.</summary>
    abstract member CheckConsistency: unit -> unit

    /// <summary>Default implementation of consistency checks = no op.</summary>
    default this.CheckConsistency() = ()

    /// <summary>Collect variables present in this node's scope.</summary>
    /// <returns>List of variable nodes.</returns>
    abstract member GetVariables: unit -> FplGenericNode list

    /// Create a (possibly empty) list of all variables in the scope of this FplGenericNode.
    default this.GetVariables() =
        this.Scope.Values
        |> Seq.filter (fun fv -> 
            fv.Name = PrimVariableL 
            || fv.Name = PrimVariableArrayL 
        )
        |> Seq.sortBy(fun fv -> fv.RunOrder)
        |> Seq.toList


    
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

    /// <summary>Type identifier for the node.</summary>
    member this.TypeId
        with get () = _typeId
        and set (value) = _typeId <- value

    /// <summary>Optional node this node refers to.</summary>
    member this.RefersTo 
        with get () = _refersTo
        and set (value) = _refersTo <- value

    /// <summary>FPL identifier of the node.</summary>
    member this.FplId
        with get () = _fplId
        and set (value) = _fplId <- value

    /// <summary>Optional file path associated with the node.</summary>
    member this.FilePath
        with get () = _filePath
        and set (value) = _filePath <- value

    /// <summary>Fixity/expression type of the node.</summary>
    member this.ExpressionType
        with get () = _expressionType
        and set (value) = _expressionType <- value

    /// <summary>Argument style for the node (parentheses/brackets/nothing).</summary>
    member this.ArgType
        with get () = _argType
        and set (value) = _argType <- value

    /// <summary>Parsed start position of the node.</summary>
    member this.StartPos
        with get () = _startPos
        and set (value) = _startPos <- value

    /// <summary>Parsed end position of the node.</summary>
    member this.EndPos
        with get () = _endPos
        and set (value) = _endPos <- value

    /// <summary>Auxiliary integer information used by traversals.</summary>
    member this.AuxiliaryInfo
        with get () = _auxiliaryInfo
        and set (value) = _auxiliaryInfo <- value

    /// <summary>Arity of the node.</summary>
    member this.Arity
        with get () = _arity
        and set (value) = _arity <- value

    /// <summary>Parent node in the symbol tree.</summary>
    member this.Parent
        with get () = _parent
        and set (value) = _parent <- value

    /// <summary>Whether this node is intrinsically defined by the system.</summary>
    member this.IsIntrinsic
        with get () = _isIntrinsic
        and set (value) = _isIntrinsic <- value

    /// <summary>Aggregated error message recorded for this node, if any.</summary>
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

    /// Create a (possibly empty) list of all properties in the scope of this FplGenericNode.
    member this.GetProperties() =
        this.Scope.Values
        |> Seq.filter (fun fv -> 
            fv.Name = PrimMandatoryFunctionalTermL 
            || fv.Name = PrimMandatoryPredicateL 
        )
        |> Seq.toList

    /// Copies other FplGenericNode to this one without changing its reference pointer.
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

    /// <summary>Qualified starting position string used for diagnostics and messages.</summary>
    member this.QualifiedStartPos =
        let rec getFullName (fv: FplGenericNode) (first: bool) =
            let fvType = fv.Type(SignatureType.Mixed)

            if fv.ShortName = PrimRoot then ""
            elif first then
                let starPosWithoutFileName =
                    // the column positioning is 0-based, we want to print it 1-based
                    let col =
                        match fv.Name with
                        | PrimTheoryL -> fv.StartPos.Column
                        | _ -> fv.StartPos.Column + (int64)1 
                    $"(Ln: {fv.StartPos.Line}, Col: {col})"

                if fv.ShortName = PrimTheory then
                    getFullName fv.Parent.Value false + fvType + starPosWithoutFileName
                else
                    getFullName fv.Parent.Value false + starPosWithoutFileName
            else if fv.ShortName = PrimTheory then
                getFullName fv.Parent.Value false + fvType
            else
                getFullName fv.Parent.Value false

        getFullName this true

    /// <summary>Compute the ultimate enclosing block node for this node.</summary>
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

    /// <summary>Compute the next block node (property or enclosing building block) for this node.</summary>
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

    /// <summary>Check whether a block named <paramref name="name"/> exists in the parent scope or sibling theories.</summary>
    /// <param name="name">Name to search for.</param>
    /// <returns>Scope search result describing found/not-found/ambiguous state.</returns>
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

/// <summary>
/// Argument style for node parameter lists.
/// </summary>
and ArgType = 
    | Parentheses
    | Brackets
    | Nothing

/// <summary>
/// Result of a name/scope lookup within a node's Scope.
/// </summary>
and ScopeSearchResult =
    | FoundAssociate of FplGenericNode
    | FoundMultiple of string
    | FoundIncorrectBlock of FplGenericNode
    | Found of FplGenericNode
    | NotFound
    | NotApplicable

/// <summary>
/// Abstract class for nodes that represent values.
/// </summary>
[<AbstractClass>]
type FplGenericIsValue(positions: Positions, parent: FplGenericNode) =
    inherit FplGenericNode(positions, Some parent)

    /// <summary>Override providing a non-value representation.</summary>
    override this.Represent() = // done
        PrimNone

    /// <summary>Value nodes do not participate in runs by default.</summary>
    override this.RunOrder = None

/// <summary>
/// Abstract class for nodes that represent actions.
/// </summary>
[<AbstractClass>]
type FplGenericIsAction(positions: Positions, parent: FplGenericNode) =
    inherit FplGenericNode(positions, Some parent)

    override this.Represent() = // done
        PrimNone
    override this.RunOrder = None

/// <summary>
/// Node representing an undetermined default value compatible with a requested type.
/// </summary>
/// <param name="typeId">Type identifier that the undetermined value must match.</param>
/// <param name="positions">Start/end positions for the synthetic node.</param>
/// <param name="parent">Parent node in the symbol table.</param>
type FplUndetermined(typeId:string, positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericIsValue(positions, parent)
    do 
        this.FplId <- LiteralUndet
        this.TypeId <- typeId

    /// <summary>Formal name returned by this node.</summary>
    override this.Name = LiteralUndetL

    /// <summary>Short name used for printing.</summary>
    override this.ShortName = LiteralUndet

    /// <summary>Create a deep clone of this undetermined node.</summary>
    override this.Clone () =
        let ret = new FplUndetermined(this.TypeId, (this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    /// <summary>Resolve type or name depending on signature mode.</summary>
    override this.Type (signatureType:SignatureType) = 
        match signatureType with 
        | SignatureType.Type -> this.TypeId
        | _ -> this.FplId
                    
    /// <summary>String representation of the undetermined value.</summary>
    override this.Represent() = // done
        this.FplId 

    /// <summary>Run is a no-op for undetermined synthetic nodes.</summary>
    override this.Run() = 
        // run is not necessary, since this node is are never referenced in the FPL syntax
        // Instead, we use them internally as default value of FplGenericHasValue
        ()

    /// <summary>Embedding is a no-op for synthetic undetermined nodes.</summary>
    override this.EmbedInSymbolTable _ = 
        // the embedding is not necessary, since this node is are never referenced in the FPL syntax
        // Instead, we use them internally as default value of FplGenericHasValue
        () 

    /// <summary>Undetermined nodes do not participate in runs by default.</summary>
    override this.RunOrder = None

/// <summary>
/// Abstract base class for nodes that may hold a value.
/// </summary>
[<AbstractClass>]
type FplGenericHasValue(positions: Positions, parent: FplGenericNode) =
    inherit FplGenericNode(positions, Some parent)
    let mutable (_value:FplGenericNode option) = None

    /// <summary>Optional value currently held by the node.</summary>
    member this.Value
        with get () = _value
        and set (value) = _value <- value

    /// <summary>Assign an explicit value node to this node.</summary>
    abstract member SetValue: FplGenericNode -> unit

    /// <summary>Adopt the value from another <c>FplGenericHasValue</c> instance.</summary>
    abstract member SetValueOf: FplGenericHasValue -> unit

    /// <summary>Set the node's value to a default undetermined value according to its type.</summary>
    abstract member SetDefaultValue: unit -> unit

    /// <summary>Assign an explicit value node to this node.</summary>
    default this.SetValue fv =
        this.Value <- Some fv

    /// <summary>Adopt the value from another <c>FplGenericHasValue</c> instance.</summary>
    default this.SetValueOf fv =
        this.Value <-  fv.Value

    /// <summary>Set the node's value to a default undetermined value according to its type.</summary>
    default this.SetDefaultValue() =
        this.SetValue (new FplUndetermined(this.TypeId, (this.StartPos, this.EndPos), this))

    /// <summary>Overrides AssignParts to propagate stored value into clones.</summary>
    override this.AssignParts (ret:FplGenericNode) = 
        base.AssignParts ret
        match ret with 
        | :? FplGenericHasValue as retWithValue ->
            retWithValue.Value <- this.Value
        | _ -> ()

    /// <summary>Overrides copy to preserve the value field.</summary>
    override this.Copy other =
        base.Copy other
        match other with 
        | :? FplGenericHasValue as otherWithValue ->
            this.Value <- otherWithValue.Value
        | _ -> ()

    /// <summary>Representation delegates to the contained value if present.</summary>
    override this.Represent() = // done
        match this.Value with 
        | Some v -> v.Represent() 
        | _ -> PrimNone // If there is no value, return string "None"

/// <summary>
/// Flatten the scopes of a node into a list containing the root and all descendants.
/// </summary>
/// <param name="root">Root node whose scopes will be flattened.</param>
/// <returns>List containing the provided node and all nodes reachable via Scope traversal.</returns>
let rec flattenScopes (root: FplGenericNode) =
    let rec helper (node: FplGenericNode) (acc: FplGenericNode list) =
        let newAcc = node :: acc
        node.Scope |> Seq.fold (fun acc kvp -> helper kvp.Value acc) newAcc

    helper root []
    
/// <summary>
/// Return the appropriate head string for the given node according to requested signature kind.
/// </summary>
/// <param name="fv">Node to query.</param>
/// <param name="signatureType">Requested signature representation.</param>
/// <returns>Either the node's FPL identifier or its type identifier.</returns>
let getFplHead (fv:FplGenericNode) (signatureType:SignatureType) =
    match signatureType with
            | SignatureType.Name 
            | SignatureType.Mixed -> fv.FplId
            | SignatureType.Type -> fv.TypeId

/// <summary>
/// Normalize a propagated signature type: <c>Mixed</c> becomes <c>Type</c>.
/// </summary>
let propagateSignatureType (signatureType:SignatureType) =
    match signatureType with
    | SignatureType.Mixed -> SignatureType.Type
    | _ -> signatureType 

/// <summary>
/// Create a concatenated signature string from a sequence of nodes using the provided separator.
/// </summary>
/// <param name="sep">Separator string used between elements.</param>
/// <param name="coordinates">Sequence of nodes contributing parts.</param>
/// <param name="signatureType">Signature kind to request from each node.</param>
/// <returns>Concatenated signature string.</returns>
let signatureSep sep (coordinates:FplGenericNode seq) signatureType =
    coordinates
    |> Seq.map (fun fv -> fv.Type signatureType)
    |> String.concat sep

/// <summary>
/// Attempt to find a mapping node associated with the provided node (follows references).
/// </summary>
/// <param name="fv">Node to inspect for a mapping.</param>
/// <returns>Optionally the mapping node when found.</returns>
let rec getMapping (fv:FplGenericNode) =
    match fv.Name with
    | LiteralParent 
    | LiteralSelf -> 
        match fv.RefersTo with 
        | Some ref -> getMapping ref
        | None -> None
    | PrimRefL when fv.RefersTo.IsSome ->
        getMapping fv.RefersTo.Value
    | PrimIsOperator 
    | PrimRefL ->
        None
    | _ ->
        fv.ArgList |> Seq.tryFind (fun fv -> fv.Name = PrimMappingL)

/// <summary>
/// Create a concatenated representation string from a sequence of nodes using the provided separator.
/// </summary>
/// <param name="sep">Separator string.</param>
/// <param name="coordinates">Sequence of nodes to represent.</param>
/// <returns>Concatenated representation string.</returns>
let representationSep sep (coordinates:FplGenericNode seq) =
    coordinates 
    |> Seq.map (fun fv -> fv.Represent())
    |> String.concat sep

/// <summary>
/// Single-line printable description of a node consisting of its short name and name-type.
/// </summary>
/// <param name="FplGenericNode">Node to describe.</param>
/// <returns>Single-line description.</returns>
let toString (FplGenericNode:FplGenericNode) = $"{FplGenericNode.ShortName} {FplGenericNode.Type(SignatureType.Name)}"

/// <summary>
/// Interface for nodes that expose a dotted child reference used by dotted-notation lookups.
/// </summary>
type IHasDotted = 
    abstract member DottedChild : FplGenericNode option with get, set

/// <summary>
/// Reasons used to explain why a statement is considered valid or derived.
/// </summary>
type ValidityReason =
    | IsAxiom of string
    | IsAxiomAssertion of string
    | IsRuleOfInference of string * string
    | IsTheorem of string
    | IsDerived of string
    | IsDerivedRevoke of string * string
    | IsDerivedAssumed of string
    | Error 


/// <summary>
/// Container combining a node and its validity reason.
/// </summary>
type ValidStatement =
    {
        Node: FplGenericNode
        ValidityReason: ValidityReason
    }

/// <summary>
/// Interface for nodes that can expose a validity expression and reason.
/// </summary>
type IValid =
    abstract member ValidExpression : ValidStatement with get
