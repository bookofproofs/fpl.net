(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)

/// <summary>
/// Module containing intrinsic types used by the FPL interpreter's symbol table.
/// </summary>
/// <remarks>
/// This module defines intrinsic value and action node types that represent
/// built-in FPL primitives such as templates, undef, predicate literals false and true,
/// and helper instance/statement abstractions used internally by the interpreter.
/// </remarks>
module Fpl.Interpreter.SymbolTable.Types2.Intrinsic
open System
open FParsec
open Fpl.Parser.Types
open Fpl.Primitives
open Fpl.Errors.Emitter
open Fpl.Interpreter.BasicTypes
open Fpl.Interpreter.Helpers.Basic
open Fpl.Interpreter.Helpers.Debug


/// <summary>
/// Represents an intrinsic template placeholder used by the interpreter to model
/// user-declared templates (type variables) in FPL.
/// </summary>
/// <param name="name">User provided name of the template.</param>
/// <param name="positions">Start and end positions in the source for diagnostics.</param>
/// <param name="parent">Parent node in the AST / symbol table.</param>
/// <remarks>
/// The template can be assigned a usage via <see cref="TrySetTemplateUsage"/> which
/// enforces consistent usage of the template within the same ultimate block scope.
/// </remarks>
type FplIntrinsicTpl(name, positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericIsValue(positions, parent)
    
    do
        this.TypeId <- name
        this.FplId <- name

    override this.Name = PrimIntrinsicTpl
    override this.ShortName = LiteralTpl

    /// <summary>
    /// Creates a deep copy of this intrinsic template node.
    /// </summary>
    /// <returns>A new <c>FplIntrinsicTpl</c> with the same identifying data and assigned parts.</returns>
    override this.Clone () =
        let ret = new FplIntrinsicTpl(this.TypeId, (this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    /// <summary>
    /// Returns the type representation for the template depending on its usage.
    /// </summary>
    /// <param name="signatureType">Determines how the head is rendered (type vs value).</param>
    /// <returns>
    /// If the template has a referenced usage, returns the usage's type signature;
    /// otherwise returns the template's user-defined head.
    /// </returns>
    override this.Type (signatureType:SignatureType) = 
        match this.RefersTo with
        | Some fv -> 
            // if the template was used, its representation is the 
            // type signature of how it was used
            fv.Type SignatureType.Type  
        | None -> 
            // otherwise, the representation defaults to the user-defined name of the template
            getFplHead this signatureType

    /// <summary>
    /// No runtime action for a template; templates are compile-time/type-level constructs.
    /// </summary>
    /// <returns>Unit.</returns>
    override this.Run() = 
        // A template has no value
        ()

    /// <summary>
    /// Sets a concrete usage for this template and emits diagnostics if a type conflict is detected.
    /// </summary>
    /// <param name="fv">The node representing the concrete usage to refer this template to.</param>
    /// <param name="diagnostic">
    /// A diagnostic identifier that decides which specific diagnostic to emit on conflict
    /// (e.g. "SIG12" or "SIG13"). If an unknown diagnostic id is passed, an unexpected error
    /// diagnostic is emitted.
    /// </param>
    /// <remarks>
    /// If the template has not been used before it is bound to <paramref name="fv"/>.
    /// If already bound and both usages occur inside the same ultimate block, the method
    /// compares signature types and emits the corresponding diagnostics on mismatch.
    /// This method does not throw exceptions for diagnostics; it sets <c>ErrorOccurred</c>
    /// and emits diagnostics via the emitter helpers. Unexpected diagnostic identifiers
    /// will cause an "unexpected error" diagnostic to be emitted but not thrown.
    /// </remarks>
    member this.TrySetTemplateUsage (fv:FplGenericNode) diagnostic = 
        match this.RefersTo with 
        | None ->
            // if this template was not used, use it
            this.RefersTo <- Some fv 
        | Some templateUsage ->
            match templateUsage.UltimateBlockNode, fv.UltimateBlockNode with
            | Some block1, Some block2 when Object.ReferenceEquals(block1, block2) ->
                // test only usages within the scope of the same UltimateBlockNode
                // (since all other usages are not usages are out of scope)
                // otherwise calculate the type signatures of the very first usage  
                let firstUsage = templateUsage.Type SignatureType.Type
                // and the current one
                let currentUsage = fv.Type SignatureType.Type
                // compare both
                match currentUsage with 
                | LiteralUndef -> () // Usage "undef" is always accepted
                | _  when firstUsage <> currentUsage ->
                    // issue diagnostics, if inconsistent usage
                    let diagnosticPosition =
                        match fv.Parent with
                        | Some par when par.Name = PrimAssignmentL ->
                            (par.ArgList[1].StartPos, par.ArgList[1].EndPos)
                        | Some par ->
                            (par.StartPos, par.EndPos)
                        | _ -> // never happens since Parent is set in all those cases
                            (Position("",0,0,0), Position("",0,0,0))

                    match diagnostic with 
                    | "SIG12" -> this.ErrorOccurred <- emitSIG12Diagnostics this.FplId currentUsage firstUsage (templateUsage.Parent.Value.QualifiedStartPos) (fst diagnosticPosition) (snd diagnosticPosition)
                    | "SIG13" -> this.ErrorOccurred <- emitSIG13Diagnostics PrimMapCasesL currentUsage firstUsage (templateUsage.Parent.Value.QualifiedStartPos) (fst diagnosticPosition) (snd diagnosticPosition)
                    | _ -> emitUnexpectedErrorDiagnostics $"Unhandled diagnostic `{diagnostic}` in FplIntrinsicTpl.TrySetTemplateUsage."
                | _ -> () // equal usage is accepted
            | _, _ -> () // equal usage is accepted

    /// <summary>
    /// Embeds this template into the parent symbol table using the helper.
    /// </summary>
    /// <param name="_">Unused parameter required by the base contract.</param>
    /// <returns>Result of the embedding helper, typically unit.</returns>
    override this.EmbedInSymbolTable _ = tryAddTemplateToParent this 

    override this.RunOrder = None

/// <summary>
/// Represents an undefined/unknown literal in FPL (`undef`).
/// </summary>
/// <param name="positions">Start and end positions for diagnostics.</param>
/// <param name="parent">Parent AST/symbol table node.</param>
/// <remarks>
/// This node denotes an undefined value and participates in type representation
/// and symbol table embedding as an expression reference.
/// </remarks>
type FplIntrinsicUndef(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericIsValue(positions, parent)
    do 
        this.TypeId <- LiteralUndef
        this.FplId <- LiteralUndef

    override this.Name = PrimIntrinsicUndef
    override this.ShortName = LiteralUndef

    /// <summary>
    /// Creates a copy of the `undefined` intrinsic node.
    /// </summary>
    /// <returns>A cloned <c>FplIntrinsicUndef</c> instance.</returns>
    override this.Clone () =
        let ret = new FplIntrinsicUndef((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    /// <summary>
    /// Returns the head representation for this `undefined` node .
    /// </summary>
    /// <param name="signatureType">How to render the head (type vs value).</param>
    /// <returns>String representing the undefined head according to <paramref name="signatureType"/>.</returns>
    override this.Type (signatureType:SignatureType) = 
        getFplHead this signatureType
                    
    /// <summary>
    /// Returns the canonical representation of the `undefined` node.
    /// </summary>
    /// <returns>Always returns <c>LiteralUndef</c>.</returns>
    override this.Represent() = // done
        LiteralUndef 

    /// <summary>
    /// No runtime action for `undefined` values; they have no runtime value.
    /// </summary>
    override this.Run() = 
        // FplIntrinsicUndef is a value of not defined FPL objects and has no value on its own
        ()

    /// <summary>
    /// Embeds this `undefined` node as an expression reference in the parent.
    /// </summary>
    override this.EmbedInSymbolTable _ = addExpressionToReference this

    override this.RunOrder = None

/// <summary>
/// Intrinsic representing the FPL predicate `true` predicate node.
/// </summary>
/// <param name="positions">Start/end positions used for diagnostics.</param>
/// <param name="parent">The parent AST/node in the symbol table.</param>
type FplIntrinsicTrue(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericIsValue(positions, parent)
    do 
        this.FplId <- LiteralTrue
        this.TypeId <- LiteralPred

    override this.Name = PrimTrue
    override this.ShortName = LiteralPred

    /// <summary>
    /// Clone the intrinsic `true` predicate node.
    /// </summary>
    /// <returns>A cloned instance of <c>FplIntrinsicTrue</c>.</returns>
    override this.Clone () =
        let ret = new FplIntrinsicTrue((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    /// <summary>
    /// Returns the head representation for the intrinsic `true` predicate.
    /// </summary>
    /// <param name="signatureType">Rendering mode for the head.</param>
    /// <returns>Head string according to <paramref name="signatureType"/>.</returns>
    override this.Type (signatureType:SignatureType) = getFplHead this signatureType
                    
    /// <summary>
    /// Returns the FPL identifier for the intrinsic `true` predicate literal.
    /// </summary>
    override this.Represent() = // done
        this.FplId 

    /// <summary>
    /// No runtime action required for the intrinsic `true` predicate.
    /// </summary>
    override this.Run() = 
        // FplIntrinsicTrue is a value of predicate closures and has no value on its own
        ()

    /// <summary>
    /// Embeds the intrinsic `true` predicate as an expression reference into the symbol table.
    /// </summary>
    override this.EmbedInSymbolTable _ = addExpressionToReference this

    override this.RunOrder = None

/// <summary>
/// Intrinsic representing the FPL predicate `false` predicate node.
/// </summary>
/// <param name="positions">Start/end positions used for diagnostics.</param>
/// <param name="parent">The parent AST/node in the symbol table.</param>
type FplIntrinsicFalse(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericIsValue(positions, parent)
    do 
        this.FplId <- LiteralFalse
        this.TypeId <- LiteralPred

    override this.Name = PrimFalse
    override this.ShortName = LiteralPred

    /// <summary>
    /// Clone the intrinsic `false` predicate node.
    /// </summary>
    /// <returns>A cloned instance of <c>FplIntrinsicFalse</c>.</returns>
    override this.Clone () =
        let ret = new FplIntrinsicTrue((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    /// <summary>
    /// Returns the head representation for the intrinsic `false` predicate.
    /// </summary>
    /// <param name="signatureType">Rendering mode for the head.</param>
    /// <returns>Head string according to <paramref name="signatureType"/>.</returns>
    override this.Type (signatureType:SignatureType) = getFplHead this signatureType
                    
    /// <summary>
    /// Returns the FPL identifier for the intrinsic `false` predicate literal.
    /// </summary>
    override this.Represent() = // done
        this.FplId 

    /// <summary>
    /// No runtime action required for the intrinsic `false` predicate.
    /// </summary>
    override this.Run() = 
        // FplIntrinsicFalse is a value of predicate closures and has no value on its own
        ()

    /// <summary>
    /// Embeds the intrinsic `false` predicate as an expression reference into the symbol table.
    /// </summary>
    override this.EmbedInSymbolTable _ = addExpressionToReference this

    override this.RunOrder = None

/// <summary>
/// Intrinsic symbol table node representing an index-typed value in FPL.
/// </summary>
/// <param name="positions">Start and end positions for diagnostics.</param>
/// <param name="parent">Parent node in the AST / symbol table.</param>
/// <remarks>
/// The representation may show a declared type when the <c>FplId</c> is <c>LiteralInd</c>.
/// </remarks>
type FplIntrinsicInd(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericIsValue(positions, parent)

    do 
        this.TypeId <- LiteralInd
        this.FplId <- LiteralInd

    override this.Name = PrimIntrinsicInd
    override this.ShortName = LiteralInd

    /// <summary>
    /// Clone the intrinsic 'ind' node.
    /// </summary>
    /// <returns>A cloned <c>FplIntrinsicInd</c> instance.</returns>
    override this.Clone () =
        let ret = new FplIntrinsicInd((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    /// <summary>
    /// Returns the head representation for this intrinsic according to the requested signature type.
    /// </summary>
    /// <param name="signatureType">Rendering mode for the head.</param>
    /// <returns>Head string for the intrinsic indicator.</returns>
    override this.Type (signatureType:SignatureType) = 
        getFplHead this signatureType
                    
    /// <summary>
    /// Returns a textual representation for the intrinsic which is typically a numeric value of the index; If the value is still unset, it will include its declared type.
    /// </summary>
    /// <returns>A human-readable representation of the intrinsic.</returns>
    override this.Represent() = // done
        match this.FplId with
        | LiteralInd -> $"dec {this.TypeId}"
        | _ -> this.FplId

    /// <summary>
    /// No runtime action for this intrinsic node.
    /// </summary>
    override this.Run() = 
        // no Run needed for FplIntrinsicInd
        ()

    /// <summary>
    /// Embeds this intrinsic into the parent as an expression reference.
    /// </summary>
    override this.EmbedInSymbolTable _ = addExpressionToReference this

    override this.RunOrder = None

/// <summary>
/// Represents an instance literal used internally to denote object instances and carry a type id.
/// </summary>
/// <param name="typeId">The type identifier of the instance.</param>
/// <param name="positions">Source positions for diagnostics.</param>
/// <param name="parent">Parent AST/symbol node.</param>
/// <remarks>
/// <c>FplInstance</c> nodes represent user-defined objects. They are created by the interpreter and not
/// emitted directly from the FPL parser. They serve as default values for nodes that hold values.
/// </remarks>
type FplInstance(typeId:string, positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericIsValue(positions, parent)

    do
        this.FplId <- LiteralObj
        this.TypeId <- typeId
 
    override this.Name = PrimInstanceL
    override this.ShortName = PrimInstance

    /// <summary>
    /// Clone this instance node preserving the type id.
    /// </summary>
    /// <returns>A cloned <c>FplInstance</c> with the same type id.</returns>
    override this.Clone () =
        let ret = new FplInstance(this.TypeId, (this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    /// <summary>
    /// Returns the representation for this instance depending on <paramref name="signatureType"/>.
    /// </summary>
    /// <param name="signatureType">Rendering mode for the head.</param>
    /// <returns>Head string according to the signature type.</returns>
    override this.Type signatureType = 
        //match signatureType with 
        //| SignatureType.Type -> this.TypeId
        //| _ -> this.FplId
        let head = getFplHead this signatureType 
        head

    /// <summary>
    /// Returns the FPL identifier representing this instance literal.
    /// </summary>
    /// <returns>Always returns <c>LiteralObj</c>.</returns>
    override this.Represent() = // done
        this.FplId

    /// <summary>
    /// Instances have no runtime action; they are used as internal value representations.
    /// </summary>
    override this.Run() = 
        // run is not neccessary, since this node is are never referenced in the FPL syntax
        // Instead, we use them internally as default value of FplGenericHasValue
        // FplInstance is a value representation and has no value on its own
        ()

    /// <summary>
    /// Embeds the instance as an expression argument in the parent.
    /// </summary>
    override this.EmbedInSymbolTable _ = 
        //// the embedding is not neccessary, since this node is are never referenced in the FPL syntax
        //// Instead, we use them internally as default value of FplGenericHasValue
        //() 
        addExpressionToParentArgList this 

    override this.RunOrder = None

/// <summary>
/// Abstract base class for statement-like actions in the interpreter that do not carry a value.
/// </summary>
/// <param name="positions">Start/end positions for diagnostics.</param>
/// <param name="parent">Parent AST/symbol node.</param>
/// <remarks>
/// Statements are actions and thus inherit from <c>FplGenericIsAction</c>. They provide
/// a common short-name and default embedding behavior used by derived statement nodes.
/// </remarks>
[<AbstractClass>]
type FplGenericStmt(positions: Positions, parent: FplGenericNode) =
    inherit FplGenericIsAction(positions, parent)

    override this.ShortName = PrimStmt

    override this.Type _ = this.FplId

    /// <summary>
    /// By default, statements embed themselves into their parent's argument list.
    /// </summary>
    override this.EmbedInSymbolTable _ = addExpressionToParentArgList this

/// <summary>
/// Abstract base class for predicate nodes that carry a value (predicative expressions).
/// </summary>
/// <param name="positions">Start/end positions for diagnostics.</param>
/// <param name="parent">Parent AST/symbol node.</param>
/// <remarks>
/// Predicates default to an undetermined predicate value; their Run implementation
/// initializes that default value.
/// </remarks>
[<AbstractClass>]
type FplGenericPredicate(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericHasValue(positions, parent)
    do 
        this.FplId <- LiteralTrue
        this.TypeId <- LiteralPred

    override this.RunOrder = None

    /// <summary>
    /// Sets the default predicate value when a predicate node is run.
    /// </summary>
    /// <remarks>
    /// Derived predicate implementations may override this behavior but should call
    /// the base behaviour if they rely on the default initialization sequence.
    /// </remarks>
    override this.Run() = 
        StaticDebug.Debug(this,Debug.Start) 
        // the default value of predicates is an undetermined predicate
        this.SetDefaultValue()
        StaticDebug.Debug(this,Debug.Stop)
