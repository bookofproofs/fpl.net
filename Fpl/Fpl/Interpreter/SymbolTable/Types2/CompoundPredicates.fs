(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)

/// <summary>
/// Module containing symbol-table node implementations for compound predicate semantics
/// used by the FPL interpreter.
/// </summary>
/// <remarks>
/// This module implements conjunction, disjunction, exclusive-or, negation,
/// implication and equivalence predicate nodes. Each node provides type-signature
/// rendering, runtime evaluation via <c>Run</c>, consistency checks and symbol-table
/// embedding behavior used by the interpreter.
/// </remarks>
module Fpl.Interpreter.SymbolTable.Types2.CompoundPredicates
open Fpl.Primitives
open Fpl.Parser.Types
open Fpl.Interpreter.BasicTypes
open Fpl.Interpreter.Helpers.Checks
open Fpl.Interpreter.Helpers.Basic
open Fpl.Interpreter.Helpers.Debug
open Fpl.Interpreter.SymbolTable.Types2.Intrinsic


/// <summary>
/// Implements the semantics of an FPL conjunction compound predicate (logical AND).
/// </summary>
/// <param name="positions">Start and end positions used for diagnostics.</param>
/// <param name="parent">Parent AST/symbol table node.</param>
/// <remarks>
/// The node evaluates its two arguments and sets its value according to the FPL truth table:
/// - false if any argument is false
/// - true if both arguments are true
/// - undetermined otherwise
/// </remarks>
type FplConjunction(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericPredicate(positions, parent)

    do 
        this.FplId <- LiteralAnd

    override this.Name = PrimConjunction
    override this.ShortName = LiteralAnd

    /// <summary>
    /// Creates a deep copy of this conjunction node preserving positions and parent.
    /// </summary>
    /// <returns>A new <c>FplConjunction</c> with assigned parts copied.</returns>
    override this.Clone () =
        let ret = new FplConjunction((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    /// <summary>
    /// Returns the textual/type head for a conjunction using infix notation "∧".
    /// </summary>
    /// <param name="signatureType">Specifies how the head should be rendered.</param>
    /// <returns>Formatted head string for the conjunction.</returns>
    override this.Type signatureType = getNotationTwoArgs this "∧" signatureType LiteralPred

    /// <summary>
    /// Evaluates the conjunction node at runtime according to FPL semantics.
    /// </summary>
    /// <returns>Unit; the node's value is set via <c>SetValue</c> or <c>SetDefaultValue</c>.</returns>
    /// <remarks>
    /// Evaluation runs both argument nodes before deciding the result and sets the
    /// node's value to an intrinsic true/false or default undetermined predicate.
    /// </remarks>
    override this.Run() =
        StaticDebug.Debug(this,Debug.Start)
        let arg1 = this.ArgList[0]
        let arg2 = this.ArgList[1]
        arg1.Run()
        arg2.Run()
        let arg1Repr = arg1.Represent()
        let arg2Repr = arg2.Represent()
        // FPL truth-table
        match (arg1Repr, arg2Repr) with
        | (LiteralFalse, _) 
        | (_, LiteralFalse)  ->
            let newValue =  new FplIntrinsicFalse((this.StartPos, this.EndPos), this)
            this.SetValue newValue
        | (LiteralTrue, LiteralTrue) -> 
            let newValue =  new FplIntrinsicTrue((this.StartPos, this.EndPos), this)
            this.SetValue newValue
        | _ -> 
            this.SetDefaultValue()
        StaticDebug.Debug(this,Debug.Stop)

    /// <summary>
    /// Performs semantic consistency checks for the conjunction node.
    /// </summary>
    /// <remarks>
    /// Checks include predicate-argument validation, free-variable and signature-variable
    /// constraints and formula cleanup checks using helper functions.
    /// </remarks>
    /// <exceptions>
    /// <exception>
    /// This method does not raise managed exceptions for diagnostics; it may emit
    /// diagnostics via helper emitters and affect node diagnostic state.
    /// </exception>
    /// </exceptions>
    override this.CheckConsistency() = 
        base.CheckConsistency() 
        let arg1 = this.ArgList[0]
        let arg2 = this.ArgList[1]
        checkArgPred this arg1
        checkArgPred this arg2
        checkFreeAndNotSignatureVar arg1
        checkFreeAndNotSignatureVar arg2
        checkCleanedUpFormula this


    /// <summary>
    /// Embeds the conjunction node into the parent's argument list after checking consistency.
    /// </summary>
    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        addExpressionToParentArgList this


/// <summary>
/// Implements the semantics of an FPL disjunction compound predicate (logical OR).
/// </summary>
/// <param name="positions">Start and end positions used for diagnostics.</param>
/// <param name="parent">Parent AST/symbol table node.</param>
type FplDisjunction(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericPredicate(positions, parent)

    do 
        this.FplId <- LiteralOr

    override this.Name = PrimDisjunction
    override this.ShortName = LiteralOr

    /// <summary>
    /// Creates a deep copy of this disjunction node.
    /// </summary>
    /// <returns>A new <c>FplDisjunction</c> with copied parts.</returns>
    override this.Clone () =
        let ret = new FplDisjunction((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    /// <summary>
    /// Returns the textual/type head for a disjunction using infix notation "∨".
    /// </summary>
    /// <param name="signatureType">Specifies how the head should be rendered.</param>
    override this.Type signatureType = getNotationTwoArgs this "∨" signatureType LiteralPred

    /// <summary>
    /// Evaluates the disjunction node at runtime according to FPL semantics.
    /// </summary>
    /// <remarks>
    /// Sets the node value to true if any argument is true, false if both are false,
    /// or leaves it undetermined otherwise.
    /// </remarks>
    override this.Run() =
        StaticDebug.Debug(this,Debug.Start)
        let arg1 = this.ArgList[0]
        let arg2 = this.ArgList[1]
        arg1.Run()
        arg2.Run()
        let arg1Repr = arg1.Represent()
        let arg2Repr = arg2.Represent()
        // FPL truth-table
        match (arg1Repr, arg2Repr) with
        | (LiteralTrue, _) 
        | (_, LiteralTrue) -> 
            let newValue =  new FplIntrinsicTrue((this.StartPos, this.EndPos), this)
            this.SetValue newValue
        | (LiteralFalse, LiteralFalse) -> 
            let newValue =  new FplIntrinsicFalse((this.StartPos, this.EndPos), this)
            this.SetValue newValue
        | _ -> 
            this.SetDefaultValue()
        StaticDebug.Debug(this,Debug.Stop)
        
    /// <summary>
    /// Performs semantic consistency checks for the disjunction node.
    /// </summary>
    override this.CheckConsistency() = 
        base.CheckConsistency() 
        let arg1 = this.ArgList[0]
        let arg2 = this.ArgList[1]
        checkArgPred this arg1
        checkArgPred this arg2
        checkFreeAndNotSignatureVar arg1
        checkFreeAndNotSignatureVar arg2
        checkCleanedUpFormula this

    /// <summary>
    /// Embeds the disjunction node into the parent's argument list after checking consistency.
    /// </summary>
    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        addExpressionToParentArgList this

/// <summary>
/// Implements the semantics of an FPL exclusive-or compound predicate (logical XOR).
/// </summary>
/// <param name="positions">Start and end positions for diagnostics.</param>
/// <param name="parent">Parent AST/symbol table node.</param>
type FplExclusiveOr(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericPredicate(positions, parent)

    do 
        this.FplId <- LiteralXor

    override this.Name = PrimExclusiveOr
    override this.ShortName = LiteralXor

    /// <summary>
    /// Creates a deep copy of this exclusive-or node.
    /// </summary>
    /// <returns>A cloned <c>FplExclusiveOr</c> instance.</returns>
    override this.Clone () =
        let ret = new FplExclusiveOr((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    /// <summary>
    /// Returns the textual/type head for an exclusive-or using infix notation "⩡".
    /// </summary>
    /// <param name="signatureType">Specifies how the head should be rendered.</param>
    override this.Type signatureType = getNotationTwoArgs this "⩡" signatureType LiteralPred

    /// <summary>
    /// Evaluates the exclusive-or node at runtime according to FPL semantics.
    /// </summary>
    override this.Run() = 
        StaticDebug.Debug(this,Debug.Start)
        let arg1 = this.ArgList[0]
        let arg2 = this.ArgList[1]
        arg1.Run()
        arg2.Run()
        let arg1Repr = arg1.Represent()
        let arg2Repr = arg2.Represent()
        // FPL truth-table
        match (arg1Repr, arg2Repr) with
        | (LiteralTrue, LiteralFalse) 
        | (LiteralFalse, LiteralTrue) -> 
            let newValue =  new FplIntrinsicTrue((this.StartPos, this.EndPos), this)
            this.SetValue newValue
        | (LiteralTrue, LiteralTrue) 
        | (LiteralFalse, LiteralFalse) -> 
            let newValue =  new FplIntrinsicFalse((this.StartPos, this.EndPos), this)
            this.SetValue newValue
        | _ -> 
            this.SetDefaultValue()

        StaticDebug.Debug(this,Debug.Stop)

    /// <summary>
    /// Performs semantic consistency checks for the exclusive-or node.
    /// </summary>
    override this.CheckConsistency() = 
        base.CheckConsistency() 
        let arg1 = this.ArgList[0]
        let arg2 = this.ArgList[1]
        checkArgPred this arg1
        checkArgPred this arg2
        checkFreeAndNotSignatureVar arg1
        checkFreeAndNotSignatureVar arg2
        checkCleanedUpFormula this


    /// <summary>
    /// Embeds the exclusive-or node into the parent's argument list after checking consistency.
    /// </summary>
    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        addExpressionToParentArgList this


/// <summary>
/// Implements the semantics of an FPL negation compound predicate (logical NOT).
/// </summary>
/// <param name="positions">Start and end positions for diagnostics.</param>
/// <param name="parent">Parent AST/symbol table node.</param>
/// <remarks>
/// The type representation renders a negated argument head and the runtime behavior
/// flips intrinsic true/false results of its single argument.
/// </remarks>
type FplNegation(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericPredicate(positions, parent)

    do 
        this.FplId <- LiteralNot

    override this.Name = PrimNegation
    override this.ShortName = LiteralNot

    /// <summary>
    /// Creates a deep copy of this negation node.
    /// </summary>
    /// <returns>A cloned <c>FplNegation</c> instance.</returns>
    override this.Clone () =
        let ret = new FplNegation((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    /// <summary>
    /// Returns the type/textual head for a negation node.
    /// </summary>
    /// <param name="signatureType">Specifies how the head should be rendered.</param>
    /// <returns>Negated head string or literal predicate when <c>SignatureType.Type</c> is requested.</returns>
    override this.Type signatureType =
        match signatureType with
        | SignatureType.Type -> LiteralPred
        | _ ->
            let argRepr =
                if this.ArgList.Count>0 then
                    let arg = this.ArgList[0]
                    if isSimpleExpression arg then
                        $"{arg.Type signatureType}"
                    else
                        $"({arg.Type signatureType})"
                else
                    LiteralUndet
            $"¬{argRepr}"

    /// <summary>
    /// Evaluates the negation node at runtime, flipping true/false of the argument.
    /// </summary>
    override this.Run() =
        StaticDebug.Debug(this,Debug.Start)
        let arg = this.ArgList[0]
        arg.Run()
        let argRepr = arg.Represent()
        match argRepr with 
        // FPL truth-table
        | LiteralFalse -> 
            let newValue =  new FplIntrinsicTrue((this.StartPos, this.EndPos), this)
            this.SetValue newValue
        | LiteralTrue -> 
            let newValue =  new FplIntrinsicFalse((this.StartPos, this.EndPos), this)
            this.SetValue newValue
        | _ -> 
            this.SetDefaultValue()

        StaticDebug.Debug(this,Debug.Stop)

    /// <summary>
    /// Performs semantic consistency checks for the negation node.
    /// </summary>
    override this.CheckConsistency() = 
        base.CheckConsistency()
        let arg = this.ArgList[0]
        checkArgPred this arg
        checkFreeAndNotSignatureVar arg
        checkCleanedUpFormula this

    /// <summary>
    /// Embeds the negation node into the parent's argument list after checking consistency.
    /// </summary>
    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        addExpressionToParentArgList this

/// <summary>
/// Implements the semantics of an FPL implication compound predicate (logical IMPLIES).
/// </summary>
/// <param name="positions">Start and end positions for diagnostics.</param>
/// <param name="parent">Parent AST/symbol table node.</param>
type FplImplication(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericPredicate(positions, parent)

    do 
        this.FplId <- LiteralImpl

    override this.Name = PrimImplication
    override this.ShortName = LiteralImpl

    /// <summary>
    /// Creates a deep copy of this implication node.
    /// </summary>
    /// <returns>A cloned <c>FplImplication</c> instance.</returns>
    override this.Clone () =
        let ret = new FplImplication((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    /// <summary>
    /// Returns the textual/type head for implication using infix notation "⇒".
    /// </summary>
    /// <param name="signatureType">Specifies how the head should be rendered.</param>
    override this.Type signatureType = getNotationTwoArgs this "⇒" signatureType LiteralPred

    /// <summary>
    /// Evaluates the implication node at runtime using the FPL truth table.
    /// </summary>
    override this.Run() = 
        StaticDebug.Debug(this,Debug.Start)
        let arg1 = this.ArgList[0]
        let arg2 = this.ArgList[1]
        let arg1Repr = arg1.Represent()
        let arg2Repr = arg2.Represent()
        match (arg1Repr, arg2Repr) with
        // FPL truth-table
        | (LiteralTrue, LiteralFalse) -> 
            let newValue =  new FplIntrinsicFalse((this.StartPos, this.EndPos), this)
            this.SetValue newValue
        | (LiteralFalse, LiteralTrue) 
        | (LiteralFalse, LiteralFalse) 
        | (LiteralTrue, LiteralTrue) -> 
            let newValue =  new FplIntrinsicTrue((this.StartPos, this.EndPos), this)
            this.SetValue newValue
        | _ -> 
            this.SetDefaultValue()
        
        StaticDebug.Debug(this,Debug.Stop)

    /// <summary>
    /// Performs semantic consistency checks for the implication node.
    /// </summary>
    override this.CheckConsistency() = 
        base.CheckConsistency() 
        let arg1 = this.ArgList[0]
        let arg2 = this.ArgList[1]
        checkArgPred this arg1
        checkArgPred this arg2
        checkFreeAndNotSignatureVar arg1
        checkFreeAndNotSignatureVar arg2
        checkCleanedUpFormula this

    /// <summary>
    /// Embeds the implication node into the parent's argument list after checking consistency.
    /// </summary>
    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        addExpressionToParentArgList this

/// <summary>
/// Implements the semantics of an FPL equivalence compound predicate (if and only if).
/// </summary>
/// <param name="positions">Start and end positions for diagnostics.</param>
/// <param name="parent">Parent AST/symbol table node.</param>
type FplEquivalence(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericPredicate(positions, parent)

    do 
        this.FplId <- LiteralIif

    override this.Name = PrimEquivalence
    override this.ShortName = LiteralIif

    /// <summary>
    /// Creates a deep copy of this equivalence node.
    /// </summary>
    /// <returns>A cloned <c>FplEquivalence</c> instance.</returns>
    override this.Clone () =
        let ret = new FplEquivalence((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    /// <summary>
    /// Returns the textual/type head for equivalence using infix notation "⇔".
    /// </summary>
    /// <param name="signatureType">Specifies how the head should be rendered.</param>
    override this.Type signatureType = getNotationTwoArgs this "⇔" signatureType LiteralPred

    /// <summary>
    /// Evaluates the equivalence node at runtime according to FPL semantics.
    /// </summary>
    override this.Run() = 
        StaticDebug.Debug(this,Debug.Start)
        let arg1 = this.ArgList[0]
        let arg2 = this.ArgList[1]
        arg1.Run()
        arg2.Run()
        let arg1Repr = arg1.Represent()
        let arg2Repr = arg2.Represent()
        match (arg1Repr, arg2Repr) with
        // FPL truth-table
        | (LiteralTrue, LiteralTrue) 
        | (LiteralFalse, LiteralFalse) -> 
            let newValue =  new FplIntrinsicTrue((this.StartPos, this.EndPos), this)
            this.SetValue newValue
        | (LiteralFalse, LiteralTrue) 
        | (LiteralTrue, LiteralFalse) -> 
            let newValue =  new FplIntrinsicFalse((this.StartPos, this.EndPos), this)
            this.SetValue newValue
        | _ -> 
            this.SetDefaultValue()

        StaticDebug.Debug(this,Debug.Stop)

    /// <summary>
    /// Performs semantic consistency checks for the equivalence node.
    /// </summary>
    override this.CheckConsistency() = 
        base.CheckConsistency() 
        let arg1 = this.ArgList[0]
        let arg2 = this.ArgList[1]
        checkArgPred this arg1
        checkArgPred this arg2
        checkFreeAndNotSignatureVar arg1
        checkFreeAndNotSignatureVar arg2
        checkCleanedUpFormula this

    /// <summary>
    /// Embeds the equivalence node into the parent's argument list after checking consistency.
    /// </summary>
    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        addExpressionToParentArgList this
