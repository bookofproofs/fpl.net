(* Copyright (c) 2021+ bookofproofs See LICENSE in the project root for license terms. *)

/// <summary>
/// This module contains top-level classes of the symbol table,
/// including root and theories in the Fpl.Interpreter namespace.
/// </summary>
module Fpl.Interpreter.SymbolTable.Types1.TopLevel
open System
open System.Text
open FParsec
open Fpl.Primitives
open Fpl.Interpreter.BasicTypes
open Fpl.Interpreter.Helpers.Debug
open TestSharedConfig

/// <summary>
/// Represents a theory in the symbol table. A theory is a top-level container for
/// FPL building blocks (axioms, theorems, lemmas, etc.) and has an associated
/// execution order used when running the interpreter.
/// </summary>
/// <param name="theoryName">Identifier used for the theory (also used for TypeId).</param>
/// <param name="parent">Parent node in the symbol table hierarchy.</param>
/// <param name="filePath">Path of the source file where the theory is declared.</param>
/// <param name="runOrder">Numeric order indicating when the theory is executed.</param>
/// <remarks>
/// The theory embeds itself into the root node. Only some kinds of building
/// blocks inside a theory run independently; others are invoked by those blocks.
/// </remarks>
type FplTheory(theoryName, parent: FplGenericNode, filePath: string, runOrder) as this =
    inherit FplGenericNode((Position("",0,1,1), Position("",0,1,1)), Some parent)
    let _runOrder = runOrder

    do
        this.FilePath <- Some filePath
        this.FplId <- theoryName
        this.TypeId <- theoryName

    /// <summary>
    /// Human-readable long name for the node (used by the interpreter).
    /// </summary>
    override this.Name = PrimTheoryL

    /// <summary>
    /// Short, symbolic name for the node kind.
    /// </summary>
    override this.ShortName = PrimTheory

    /// <summary>
    /// Creates a deep copy of this theory node.
    /// </summary>
    /// <returns>A cloned <see cref="FplGenericNode"/> representing the same theory.</returns>
    override this.Clone () =
        let ret = new FplTheory(this.FplId, this.Parent.Value, this.FilePath.Value, _runOrder)
        this.AssignParts(ret)
        ret

    /// <summary>
    /// Returns the signature representation for the requested <c>SignatureType</c>.
    /// </summary>
    /// <param name="signatureType">The signature view to return (e.g. Name, Type).</param>
    /// <returns>A string representing the requested signature view of the node.</returns>
    override this.Type signatureType = getFplHead this signatureType

    /// <summary>
    /// The run order in which this theory should be executed among other theories embedded in the root node.
    /// </summary>
    /// <returns>An optional integer; <c>Some</c> contains the order, <c>None</c> otherwise.</returns>
    override this.RunOrder = Some _runOrder

    /// <summary>
    /// Adds this theory to its parent's scope (root node's scope) in the symbol table.
    /// </summary>
    /// <param name="_">Unused parameter; embedding uses the theory's parent.</param>
    /// <remarks>
    /// Name conflicts are not expected because file and namespace management guarantees uniqueness.
    /// </remarks>
    override this.EmbedInSymbolTable _ = 
        let next = this.Parent.Value
        // name conflicts of theories do not occur because of *.fpl file management 
        // and file-names being namespace names
        next.Scope.TryAdd(this.FplId, this) |> ignore

    /// <summary>
    /// Returns all building blocks in this theory that run independently, ordered by their RunOrder.
    /// </summary>
    /// <remarks>
    /// This member is private and used internally when running a theory.
    /// </remarks>
    member private this.OrderedBlocksRunningByThemselves =
        this.Scope.Values
        |> Seq.choose (fun block ->
            match block.RunOrder with
            | Some _ -> Some block
            | _ -> None)
        |> Seq.sortBy (fun block -> block.RunOrder.Value) 
        |> Seq.toList

    /// <summary>
    /// Execute this theory: runs all independent building blocks in RunOrder ascending.
    /// </summary>
    override this.Run() = 
        StaticDebug.Debug(this,Debug.Start) 
        let blocks = this.OrderedBlocksRunningByThemselves
        blocks
        |> Seq.iter (fun block -> block.Run())        
        StaticDebug.Debug(this,Debug.Stop) 

/// <summary>
/// Represents the root node of the evaluation symbol table. The root contains
/// all discovered theories and provides top-level operations such as running
/// all theories in the proper order.
/// </summary>
type FplRoot() =
    inherit FplGenericNode((Position("", 0, 1, 1), Position("", 0, 1, 1)), None)

    /// <summary>
    /// Human-readable long name for the root node.
    /// </summary>
    override this.Name = PrimRoot

    /// <summary>
    /// Short name for the root node kind.
    /// </summary>
    override this.ShortName = PrimRoot

    /// <summary>
    /// Returns a shallow clone of the root.
    /// </summary>
    /// <returns>The same root instance (clone returns the root itself).</returns>
    override this.Clone () = this

    /// <summary>
    /// Returns the signature for the requested signature type for the root node.
    /// </summary>
    /// <param name="_">SignatureType parameter (ignored for root).</param>
    /// <returns>An empty string for the root.</returns>
    override this.Type _ = String.Empty

    /// <summary>
    /// Embedding the root into a symbol table is a no-op.
    /// </summary>
    override this.EmbedInSymbolTable _ = () 

    /// <summary>
    /// Returns all theories discovered under the root ordered by their RunOrder (discovery order).
    /// </summary>
    member this.OrderedTheories =
        this.Scope.Values
        |> Seq.choose (fun item ->
            match item with
            | :? FplTheory as theory -> Some theory
            | _ -> None)
        |> Seq.sortBy (fun th -> th.RunOrder.Value) 

    /// <summary>
    /// The root has no RunOrder.
    /// </summary>
    override this.RunOrder = None

    /// <summary>
    /// Executes all theories registered under the root using their individual run order.
    /// </summary>
    override this.Run() = 
        StaticDebug.Debug(this,Debug.Start)
        this.OrderedTheories
        |> Seq.iter (fun theory -> theory.Run())        
        StaticDebug.Debug(this,Debug.Stop)

    /// <summary>
    /// Clears the root's argument list and scope.
    /// </summary>
    member this.Clear() =
        this.ArgList.Clear()
        this.Scope.Clear()

/// <summary>
/// Represents the in-memory symbol table used by the interpreter. Holds the root node,
/// current main theory identifier, and an evaluation counter used for re-evaluations.
/// </summary>
type SymbolTable() =
    let mutable _mainTheory = ""
    let mutable _evalCounter = 0
    let _root = new FplRoot()

    /// <summary>
    /// Gets or sets the current main theory identifier that is used as a s starting point to create the run order.
    /// </summary>
    /// <remarks>
    /// The main theory is used as the entry point for the FPL interpreter. All other theories imported by the main theory will be interpreted.
    /// in the order they were imported. This process works by recursion and accounts for the "self-containment" requirement of
    /// mathematical theories (see Proof-of-Concept paper "Formal Proving Language (FPL)", A. Piotrowski, 2021)
    /// </remarks>
    member this.MainTheory
        with get () = _mainTheory
        and set (value) = _mainTheory <- value

    /// <summary>
    /// Gets or sets the number of times this symbol table has been re-evaluated.
    /// </summary>
    member this.EvalCounter
        with get () = _evalCounter
        and set (value) = _evalCounter <- value

    /// <summary>
    /// Returns the evaluation root node of the symbol table.
    /// </summary>
    member this.Root = _root

    /// <summary>
    /// Serializes the symbol table to a JSON-like representation.
    /// </summary>
    /// <returns>A string containing the serialized representation of the symbol table.</returns>
    /// <remarks>
    /// The resulting JSON-like string escapes backslashes and double quotes and walks the
    /// symbol table graph while guarding against infinite cycles.
    /// </remarks>
    member this.ToJson() =
        let sb = StringBuilder()
        let mutable currentPath = ""

        let rec createJson (root: FplGenericNode) (sb: StringBuilder) level isLast preventInfinite =
            match root.FilePath with
            | Some path -> currentPath <- path
            | _ -> ()

            let indent, indentMinusOne =
                if TestConfig.DebugModeInterpreter then
                    String(' ', level), String(' ', level - 1)
                else
                    String.Empty, String.Empty

            sb.AppendLine(indentMinusOne + "{") |> ignore
            let name = $"{root.Type(SignatureType.Name)}".Replace(@"\", @"\\")
            let fplTypeName = $"{root.Type(SignatureType.Type)}".Replace(@"\", @"\\")
            let fplValueRepr = $"{root.Represent()}".Replace("\\", "\\\\")   // escape backslashes first
                                                            .Replace("\"", "\\\"")   // then escape double quotes

            if name = this.MainTheory then
                sb.AppendLine($"{indent}\"Name\": \"(Main) {name}\",") |> ignore
            else
                sb.AppendLine($"{indent}\"Name\": \"{name}\",") |> ignore

            let refersTo =
                match root.RefersTo with
                | Some ref -> $"{ref.Name} {ref.Type SignatureType.Mixed}" 
                | None -> ""

            sb.AppendLine($"{indent}\"Type\": \"{root.ShortName}\",") |> ignore
            sb.AppendLine($"{indent}\"FplValueType\": \"{fplTypeName}\",") |> ignore
            sb.AppendLine($"{indent}\"FplValueRepr\": \"{fplValueRepr}\",") |> ignore
            sb.AppendLine($"{indent}\"FplRefersTo\": \"{refersTo}\",") |> ignore
            sb.AppendLine($"{indent}\"Line\": \"{root.StartPos.Line}\",") |> ignore
            sb.AppendLine($"{indent}\"Column\": \"{root.StartPos.Column}\",") |> ignore
            sb.AppendLine($"{indent}\"FilePath\": \"{currentPath}\",") |> ignore

            if preventInfinite then
                sb.AppendLine($"{indent}\"Scope\": [],") |> ignore
                sb.AppendLine($"{indent}\"ArgList\": [],") |> ignore
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

    /// <summary>
    /// Clears the internal root and resets the main theory identifier.
    /// </summary>
    member this.Clear() =
        _root.Clear() 
        _mainTheory <- ""

