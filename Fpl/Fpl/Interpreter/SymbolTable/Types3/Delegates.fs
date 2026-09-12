(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 
*)

/// <summary>
/// Module containing delegate-like symbol-table node types used by the FPL interpreter,
/// including equality and numeric delegates.
/// </summary>
/// <remarks>
/// Delegates implement runtime semantics for special operations expressed as FPL delegates.
/// This module includes:
/// - <c>FplGenericDelegate</c>: abstract base for delegate nodes.
/// - <c>FplEquality</c>: implements predicate equality semantics.
/// - <c>FplDecrement</c>: implements a numeric decrement delegate.
/// Diagnostics referenced: ID013, ID013, etc. See emitter usages for details.
/// </remarks>
module Fpl.Interpreter.SymbolTable.Types3.Delegates
open Fpl.Primitives
open Fpl.Parser.Types
open Fpl.Errors.Emitter
open Fpl.Interpreter.BasicTypes
open Fpl.Interpreter.Helpers.Checks
open Fpl.Interpreter.Helpers.Basic
open Fpl.Interpreter.Helpers.Debug
open Fpl.Interpreter.SymbolTable.Storage.Heap
open Fpl.Interpreter.SymbolTable.Types2.Intrinsic
open Fpl.Interpreter.SymbolTable.Types2.Variables
open Fpl.Interpreter.SymbolTable.Types2.References
open Fpl.Interpreter.SymbolTable.Types3.Extensions

/// <summary>
/// Abstract base class for delegate nodes in the FPL symbol-table.
/// </summary>
/// <param name="name">Identifier used as the delegate's internal FPL id.</param>
/// <param name="positions">Source positions (start,end) used for diagnostics.</param>
/// <param name="parent">Parent symbol-table node.</param>
/// <remarks>
/// Derived delegates typically override <c>Run</c>, <c>Type</c> or <c>CheckConsistency</c>
/// to implement their specific behavior. Delegates are not executed in a fixed run order
/// (RunOrder = None) by default.
/// </remarks>
[<AbstractClass>]
type FplGenericDelegate(name, positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericHasValue(positions, parent)

    do 
        this.FplId <- name

    override this.RunOrder = None

/// <summary>
/// Implements the semantics of the equality delegate (predicate <c>=</c>).
/// </summary>
/// <param name="name">Delegate name (unused; constructed with equality id).</param>
/// <param name="positions">Source positions used for diagnostics.</param>
/// <param name="parent">Parent node.</param>
/// <remarks>
/// The equality delegate enforces arity of two arguments (emits ID013 when violated).
/// At runtime it evaluates argument representations and types to determine
/// True/False/Undetermined semantics and emits appropriate diagnostics for undefined
/// or undetermined operands.
/// </remarks>
/// <exceptions>
/// <exception>Emits ID013 diagnostics when argument count is not 2 or when evaluation
/// encounters undefined/undetermined arguments.</exception>
/// </exceptions>
type FplEquality(name, positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericDelegate(name, positions, parent)

    do 
        this.FplId <- $"{LiteralDel}{PrimDelegateEqual}"
        this.TypeId <- LiteralPred

    override this.Name = PrimDelegateEqualL
    override this.ShortName = PrimDelegateEqual

    /// <summary>
    /// Create a shallow clone of the equality node preserving parts and positions.
    /// </summary>
    /// <returns>New <c>FplEquality</c> instance.</returns>
    override this.Clone () =
        let ret = new FplEquality(this.FplId, (this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    /// <summary>
    /// Copy relevant state from another node into this instance.
    /// </summary>
    /// <param name="other">Node to copy from.</param>
    override this.Copy(other) =
        base.Copy(other)
        this.TypeId <- LiteralPred

    /// <summary>
    /// Produce a textual signature for the equality delegate according to the requested signature type.
    /// </summary>
    /// <param name="signatureType">Requested signature formatting.</param>
    /// <returns>String describing the delegate head and its argument list.</returns>
    override this.Type signatureType = 
        let head = getFplHead this signatureType
        let propagate = propagateSignatureType signatureType
        let args = signatureSep ", " this.ArgList propagate
        sprintf "%s(%s)" head args

    /// <summary>
    /// Check arity and run base consistency checks.
    /// </summary>
    /// <remarks>
    /// Emits ID013 diagnostic when the delegate does not have exactly two arguments.
    /// </remarks>
    override this.CheckConsistency (): unit = 
        if this.ArgList.Count <> 2 then 
            this.ErrorOccurred <- emitID013Diagnostics $"Predicate `=` takes 2 arguments, got {this.ArgList.Count}." heap.Helper.CallerStartPos heap.Helper.CallerEndPos 
        base.CheckConsistency()
    
    /// <summary>
    /// Embed equality expression into parent reference lists after consistency checks.
    /// </summary>
    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        addExpressionToReference this

    /// <summary>
    /// Evaluate the equality predicate at runtime.
    /// </summary>
    /// <remarks>
    /// Behavior:
    /// - If errors exist or the node is used inside a quantifier, the predicate becomes undetermined.
    /// - If operand types differ, result is False.
    /// - If operand representations are equal and defined, result is True; otherwise False.
    /// - Emits diagnostics (ID013) for undefined/undetermined operands.
    /// </remarks>
    override this.Run() = 
        StaticDebug.Debug(this,Debug.Start)
        match this.ErrorOccurred with 
        | Some err ->
            this.SetDefaultValue()
        | _ ->
            if isInQuantifier this then 
                this.SetDefaultValue()
            else

                let a = this.ArgList[0]
                let b = this.ArgList[1]
                let aType = a.Type SignatureType.Type
                let bType = b.Type SignatureType.Type
                let aRepr = a.Represent()
                let bRepr = b.Represent()

                match aRepr with
                | LiteralUndef -> 
                    this.ErrorOccurred <- emitID013Diagnostics "Predicate `=` cannot be evaluated: left argument is undefined." heap.Helper.CallerStartPos heap.Helper.CallerEndPos 
                    this.SetDefaultValue()
                | _ -> 
                    match bRepr with
                    | LiteralUndef -> 
                        this.ErrorOccurred <- emitID013Diagnostics "Predicate `=` cannot be evaluated: right argument is undefined." heap.Helper.CallerStartPos heap.Helper.CallerEndPos 
                        this.SetDefaultValue()
                    | _ when aType<>bType -> 
                        // if the compared arguments have different types, then unequal
                        this.SetValue (new FplIntrinsicFalse((heap.Helper.CallerStartPos, heap.Helper.CallerEndPos), this.Parent.Value))
                    | _ when aType = bType && aRepr = LiteralUndet && bRepr = LiteralUndet -> 
                        this.SetDefaultValue()
                    | _ -> 
                        match aRepr with
                        | LiteralUndet -> 
                            this.ErrorOccurred <- emitID013Diagnostics "Predicate `=` cannot be evaluated: left argument is undetermined." heap.Helper.CallerStartPos heap.Helper.CallerEndPos 
                            this.SetDefaultValue()
                        | _ -> 
                            match bRepr with
                            | LiteralUndet -> 
                                this.ErrorOccurred <- emitID013Diagnostics "Predicate `=` cannot be evaluated: right argument is undetermined." heap.Helper.CallerStartPos heap.Helper.CallerEndPos 
                                this.SetDefaultValue()
                            | _ when aRepr = bRepr -> 
                                this.SetValue (new FplIntrinsicTrue((heap.Helper.CallerStartPos, heap.Helper.CallerEndPos), this.Parent.Value))
                            | _ -> 
                                this.SetValue (new FplIntrinsicFalse((heap.Helper.CallerStartPos, heap.Helper.CallerEndPos), this.Parent.Value))
        StaticDebug.Debug(this,Debug.Stop)

/// <summary>
/// Implements the semantics of a decrement delegate that subtracts one from an integer-like value.
/// This delegate is a convenience delegate for user-defined FPL extensions injecting the syntax of potentially infinitely many new symbols that might represent natural numbers. The latter has to be user-defined in FPL.
/// This decrement delegate makes use of the well-ordering principle of natural numbers, so that recursive calls of extensions starting, e.g. with the symbol "3", will finally terminate at the smallest value possible that is "0".
/// </summary>
/// <param name="name">Delegate identifier.</param>
/// <param name="positions">Source positions used for diagnostics.</param>
/// <param name="parent">Parent node.</param>
/// <remarks>
/// The delegate expects a single argument of type <c>PrimDigits</c>. It tries to interpret
/// the argument's representation as an integer, decrements it by one and returns a new
/// <c>FplExtensionObj</c> wrapping the numeric string representation. If the result would be
/// negative an overflow-like condition is treated as undetermined (default value).
/// </remarks>
/// <exceptions>
/// <exception>Emits ID013 diagnostics when the argument count or type is invalid.</exception>
/// </exceptions>
type FplDecrement(name, positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericDelegate(name, positions, parent)

    do 
        this.TypeId <- PrimDigits

    override this.Name = PrimDelegateDecrementL
    override this.ShortName = PrimDelegateDecrement

    /// <summary>
    /// Create a shallow clone of the decrement node preserving parts and positions.
    /// </summary>
    /// <returns>New <c>FplDecrement</c> instance.</returns>
    override this.Clone () =
        let ret = new FplDecrement(this.FplId, (this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Copy(other) =
        base.Copy(other)

    /// <summary>
    /// Produce textual signature for this delegate according to requested signature type.
    /// </summary>
    /// <param name="signatureType">Requested signature formatting.</param>
    /// <returns>Signature string describing head and argument list.</returns>
    override this.Type signatureType = 
        let head = getFplHead this signatureType
        match signatureType with
        | SignatureType.Type -> head
        | _ ->
            let propagate = propagateSignatureType signatureType
            let args = signatureSep ", " this.ArgList propagate
            sprintf "%s(%s)" head args


    /// <summary>
    /// Validate arity and argument type for decrement delegate.
    /// </summary>
    /// <remarks>
    /// Expects exactly one argument of type <c>PrimDigits</c>; otherwise emits ID013 diagnostics.
    /// </remarks>
    override this.CheckConsistency() =
        if this.ArgList.Count <> 1 then 
            this.ErrorOccurred <- emitID013Diagnostics $"Decrement expects 1 argument but received {this.ArgList.Count}." this.StartPos this.EndPos
        else
            let arg = this.ArgList[0]
            let argType = arg.Type SignatureType.Type 
            if argType <> PrimDigits then 
                this.ErrorOccurred <- emitID013Diagnostics $"Decrement requires an argument of type `{PrimDigits}`; got `{argType}`." arg.StartPos arg.EndPos
        base.CheckConsistency()
    
    /// <summary>
    /// Embed this delegate into parent reference lists after consistency checks.
    /// </summary>
    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        addExpressionToReference this

    /// <summary>
    /// Execute the decrement operation at runtime.
    /// </summary>
    /// <remarks>
    /// The method:
    /// - Evaluates the single argument and determines its textual numeric representation.
    /// - Attempts to parse and decrement the integer value.
    /// - On success returns a new <c>FplExtensionObj</c> whose <c>FplId</c> is the decremented numeric string.
    /// - On parse failure or negative result, returns the default/undetermined value.
    /// </remarks>
    override this.Run() = 
        StaticDebug.Debug(this,Debug.Start)
        match this.ErrorOccurred with
        | Some err ->
            this.SetDefaultValue()
        | _ ->
            let newValue = FplExtensionObj((this.StartPos, this.EndPos), this.Parent.Value)
            newValue.TypeId <- PrimDigits
            let argPre = this.ArgList[0]
            argPre.Run()
            let numericValue = 
                match argPre with
                | :? FplGenericVariable -> 
                    argPre.Represent()
                | :? FplReference when argPre.RefersTo.IsSome ->
                    match argPre.RefersTo.Value with
                    | :? FplGenericVariable as argPreVar -> 
                        argPreVar.Represent()
                    | _ -> argPre.FplId
                | _ -> argPre.FplId

            let mutable n = 0
            System.Int32.TryParse(numericValue, &n) |> ignore
            let n' = n - 1
            if n' < 0 then 
                // TODO issue diagnostics overflow Decrement
                this.SetDefaultValue()
            else
                newValue.FplId <- string n'
                this.SetValue newValue
        StaticDebug.Debug(this,Debug.Stop)


