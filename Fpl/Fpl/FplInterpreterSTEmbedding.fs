/// This module contains functions helping the FplInterpreter
/// to embed nodes into the symbol table 

(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)
module FplInterpreterSTEmbedding
open System
open FParsec
open FplPrimitives
open FplInterpreterUtils
open FplInterpreterDiagnosticsEmitter
open FplInterpreterBasicTypes
open FplInterpreterGlobals




type FplTheory(theoryName, parent: FplGenericNode, filePath: string, runOrder) as this =
    inherit FplGenericNode((Position("",0,1,1), Position("",0,1,1)), Some parent)
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

    override this.Run() = 
        debug this Debug.Start 
        let blocks = this.OrderedBlocksRunningByThemselves
        blocks
        |> Seq.iter (fun block -> block.Run())        
        debug this Debug.Stop 

/// Indicates if an FplValue is the root of the SymbolTable.
let isTheory (fv:FplGenericNode) = 
    match fv with
    | :? FplTheory -> true
    | _ -> false

type FplRoot() =
    inherit FplGenericNode((Position("", 0, 1, 1), Position("", 0, 1, 1)), None)
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

    override this.Run() = 
        debug this Debug.Start
        this.OrderedTheories
        |> Seq.iter (fun theory -> theory.Run())        
        debug this Debug.Stop

// Returns the root node of any FplValue
let rec getRoot (fv:FplGenericNode) =
    if fv.Name = PrimRoot then 
        fv :?> FplRoot
    else getRoot fv.Parent.Value
   

/// Indicates if an FplValue is the root of the SymbolTable.
let isRoot (fv:FplGenericNode) = 
    match fv with
    | :? FplRoot -> true
    | _ -> false


// Tries to add for statement's domain or entity to its parent's for statement
let tryAddToParentForInStmt (fplValue:FplGenericNode) =
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
let tryAddTemplateToParent (templateNode:FplGenericNode) =
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
let tryAddToParentUsingFplId (fplValue:FplGenericNode) =
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
let tryAddToParentUsingMixedSignature (fplValue:FplGenericNode) =
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
let tryAddSubBlockToFplBlock (fplValue:FplGenericNode) =
    let identifier = fplValue.Type SignatureType.Mixed
    let parent = fplValue.Parent.Value
    if parent.Scope.ContainsKey(identifier) then 
        fplValue.ErrorOccurred <- emitID001Diagnostics identifier (parent.Scope[identifier].QualifiedStartPos) fplValue.StartPos fplValue.EndPos
    else
        parent.Scope.Add(identifier, fplValue)   

// Tries to add an FPL block to its parent's scope using its typed signature, or issues ID001 diagnostics if a conflict occurs
let tryAddToParentUsingTypedSignature (fplValue:FplGenericNode) =
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
let addExpressionToParentArgList (fplValue:FplGenericNode) =
    let parent = fplValue.Parent.Value
    match parent.Name with 
    | LiteralLocL ->
        let identifier = fplValue.FplId
        parent.FplId <- identifier
        parent.TypeId <- identifier
    | _ -> ()
    parent.ArgList.Add fplValue

type IHasDotted = 
    abstract member DottedChild : FplGenericNode option with get, set

// Add an expression to a reference
let addExpressionToReference (fplValue:FplGenericNode) =
    let nextOpt = fplValue.Parent
    match box nextOpt with 
    | :? IHasDotted as dc when dc.DottedChild.IsSome -> ()
    | _ ->
        match nextOpt with
        | Some next when next.Name = PrimRefL && next.RefersTo.IsSome ->
            let referenced = next.RefersTo.Value
            
            match referenced.Name with 
            | PrimVariableArrayL 
            | LiteralParent 
            | LiteralSelf ->
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
            next.FplId <- fplValue.FplId
            next.TypeId <- fplValue.TypeId
            next.RefersTo <- Some fplValue
        | Some next when next.Name = PrimRefL ->
            next.FplId <- fplValue.FplId
            next.TypeId <- fplValue.TypeId
            next.RefersTo <- Some fplValue 
            next.ErrorOccurred <- fplValue.ErrorOccurred
        | _ -> addExpressionToParentArgList fplValue 
