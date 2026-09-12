(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)
/// <summary>
/// Module containing symbol-table node implementation for the FPL <c>is</c> operator.
/// </summary>
/// <remarks>
/// The <c>is</c> operator tests whether an operand matches a given type/mapping according
/// to the interpreter's type-matching rules. Evaluation uses <c>FplTypeMatcher.MatchPwA</c>
/// and yields intrinsic predicate values (<c>true</c>/<c>false</c>).
/// Diagnostics are emitted via emitter helpers; runtime evaluation does not throw for semantic diagnostics.
/// </remarks>
module Fpl.Interpreter.SymbolTable.Types3.IsOperator
open Fpl.Primitives
open Fpl.Parser.Types
open Fpl.Interpreter.BasicTypes
open Fpl.Interpreter.Helpers.Basic
open Fpl.Interpreter.Helpers.Debug
open Fpl.Interpreter.SymbolTable.Types2.Intrinsic
open Fpl.Interpreter.SymbolTable.Types2.References
open Fpl.Interpreter.SymbolTable.TypeMatching



/// <summary>
/// Implements the semantics of the FPL <c>is</c> operator (type-test).
/// </summary>
/// <param name="positions">Tuple of start and end positions in the source used for diagnostics.</param>
/// <param name="parent">Parent AST / symbol-table node.</param>
/// <remarks>
/// The node expects two arguments: the operand to be tested (typically a reference)
/// and the type/mapping expression to test against. During <c>Run</c>, it sets the node's
/// value to an intrinsic predicate <c>true</c> or <c>false</c> depending on the match result.
/// </remarks>
type FplIsOperator(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericPredicate(positions, parent)

    do 
        this.FplId <- LiteralIs

    /// <summary>
    /// The element name used for pattern matching and diagnostics.
    /// </summary>
    override this.Name = PrimIsOperator
    override this.ShortName = LiteralIs

    /// <summary>
    /// Create a deep copy of this <c>FplIsOperator</c> node preserving positional and parent information.
    /// </summary>
    /// <returns>A new <c>FplIsOperator</c> instance with copied parts.</returns>
    override this.Clone () =
        let ret = new FplIsOperator((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    /// <summary>
    /// Returns the textual/type head representation of the operator using infix notation "is".
    /// </summary>
    /// <param name="signatureType">Requested signature rendering mode.</param>
    /// <returns>Formatted head string for the operator according to <paramref name="signatureType"/>.</returns>
    override this.Type signatureType = getNotationTwoArgs this "is" signatureType LiteralPred
        
    /// <summary>
    /// Evaluate the <c>is</c> operator at runtime.
    /// </summary>
    /// <returns>Unit; the evaluated intrinsic predicate value is assigned to the node via <c>SetValue</c>.</returns>
    /// <remarks>
    /// If the left operand is a reference, the matcher <c>FplTypeMatcher.MatchPwA</c> is invoked
    /// to compare the operand against the provided type expression. The node's value is set to
    /// intrinsic true on match and intrinsic false on mismatch. Non-reference operands result in false.
    /// </remarks>
    override this.Run() = 
        StaticDebug.Debug(this,Debug.Start)
        let operand = this.ArgList[0]
        let typeOfOperand = this.ArgList[1]
        // FPL truth-table
        match operand with 
        | :? FplReference as op ->
            match FplTypeMatcher.MatchPwA [operand] [typeOfOperand] with
            | Some _errMsg -> 
                let newValue =  new FplIntrinsicFalse((this.StartPos, this.EndPos), this)
                this.SetValue newValue
            | None -> 
                let newValue =  new FplIntrinsicTrue((this.StartPos, this.EndPos), this)
                this.SetValue newValue
        | _ -> 
            let newValue =  new FplIntrinsicFalse((this.StartPos, this.EndPos), this)
            this.SetValue newValue
        
        StaticDebug.Debug(this,Debug.Stop)

    /// <summary>
    /// Embed this operator node into the parent's argument list in the symbol table.
    /// </summary>
    /// <param name="_">Unused parameter required by the base contract.</param>
    /// <remarks>
    /// Embedding records the node in the parent's argument list; signature/consistency checks occur elsewhere.
    /// </remarks>
    override this.EmbedInSymbolTable _ = addExpressionToParentArgList this
