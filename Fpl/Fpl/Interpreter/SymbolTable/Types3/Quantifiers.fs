(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)
/// <summary>
/// Module containing symbol-table node implementations for FPL quantifiers.
/// </summary>
/// <remarks>
/// Implements generic and concrete quantifier nodes used by the interpreter (universal,
/// existential and existential-unique). Nodes provide signature rendering, consistency
/// checks, embedding into the symbol table and runtime evaluation semantics for quantifiers.
/// Diagnostics are emitted via emitter helpers and nodes generally do not throw on semantic errors.
/// </remarks>
module Fpl.Interpreter.SymbolTable.Types3.Quantifiers
open Fpl.Primitives
open Fpl.Parser.Types
open Fpl.Errors.Emitter
open Fpl.Interpreter.BasicTypes
open Fpl.Interpreter.Helpers.Checks
open Fpl.Interpreter.Helpers.Basic
open Fpl.Interpreter.Helpers.Debug
open Fpl.Interpreter.SymbolTable.Types2.Intrinsic
open Fpl.Interpreter.SymbolTable.Types2.Variables


/// <summary>
/// Abstract base class for quantifier nodes (common behavior for universal and existential quantifiers).
/// </summary>
/// <param name="positions">Tuple with start and end positions in the source for diagnostics.</param>
/// <param name="parent">Parent node in the symbol table.</param>
/// <remarks>
/// Provides default type rendering for predicate signatures, consistency checks that emit VAR05
/// diagnostics for unused bound variables and helper embedding/run semantics used by concrete quantifiers.
/// </remarks>
/// <returns>Instance of a derived <c>FplGenericQuantifier</c>.</returns>
[<AbstractClass>]
type FplGenericQuantifier(positions: Positions, parent: FplGenericNode) =
    inherit FplGenericPredicate(positions, parent)

    override this.ShortName = PrimQuantifier

    /// <summary>
    /// Returns the textual/type representation for the quantifier depending on the requested signature mode.
    /// </summary>
    /// <param name="signatureType">Requested signature rendering mode (Name/Mixed/Type).</param>
    /// <returns>Formatted string representing the quantifier (head, bound variables and body).</returns>
    /// <remarks>
    /// - When <c>SignatureType.Type</c> is requested, the function returns a predicate head optionally
    ///   followed by the types of unbound signature variables.
    /// - For display forms, the quantifier symbol and grouped bound variables are rendered and the
    ///   quantified body is included in braces.
    /// </remarks>
    override this.Type signatureType =
        match signatureType with
        | SignatureType.Type ->
            let head = LiteralPred
            let paramT =
                this.GetVariables()
                |> List.filter (fun var -> box var :? FplVariable)
                |> List.map (fun var -> box var :?> FplVariable)
                |> List.filter (fun var -> not var.IsBound)
                |> List.map (fun var -> $"{var.Type SignatureType.Type}")
                |> String.concat ", "

            match paramT with
            | "" -> head
            | _ -> sprintf "%s(%s)" head paramT
        | _ ->
            let head =
                match this.Name with
                | PrimQuantifierAll -> "∀"
                | PrimQuantifierExists -> "∃"
                | PrimQuantifierExistsN -> $"{this.FplId}".Replace("exn$1","∃!").Replace("exn$","∃!")
                | _ -> ""
            let boundVars =
                this.GetVariables()
                |> List.choose (function
                    | :? FplVariable as v when v.IsBound -> Some v
                    | _ -> None)
                |> List.groupBy (fun v -> v.Type SignatureType.Type)
                |> List.sortBy fst   // sort groups by type
                |> List.map (fun (typ, vars) ->
                    let names =
                        vars
                        |> List.sortBy (fun v -> v.FplId)   // sort names inside each group
                        |> List.map (fun v -> v.FplId)
                        |> String.concat ", "
                    $"{names}:{typ}")
                |> String.concat ", "

            let body =
                if this.ArgList.Count>0 then
                    this.ArgList[0].Type signatureType
                else
                    ""
            $"{head} {boundVars} " + "{" + body + "}"

    /// <summary>
    /// Perform semantic consistency checks for the quantifier node.
    /// </summary>
    /// <remarks>
    /// Emits:
    /// - VAR05 diagnostics for variables declared in the quantifier that are never used;
    /// - ensures the quantifier argument is a predicate expression and that the formula is cleaned up.
    /// Diagnostics are recorded on nodes using emitter helpers and not thrown.
    /// </remarks>
    override this.CheckConsistency () = 
        base.CheckConsistency()
        this.GetVariables()
        |> List.map(fun var -> var :?> FplGenericVariable)
        |> List.filter(fun var -> not var.IsUsed)
        |> List.iter (fun var -> 
            var.ErrorOccurred <- emitVAR05Diagnostics var.FplId var.StartPos var.EndPos
        )
        checkArgPred this (this.ArgList[0])
        checkCleanedUpFormula this

    /// <summary>
    /// Embed the quantifier into the parent symbol table as an expression argument.
    /// </summary>
    /// <param name="_">Unused parameter required by the base contract.</param>
    /// <returns>Unit; embedding calls helpers to attach the node to the parent's arg list.</returns>
    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        addExpressionToParentArgList this
    
    /// <summary>
    /// Runtime semantics for the quantifier: evaluate the quantified body and set this node to default value.
    /// </summary>
    /// <remarks>
    /// The quantifier runs its first argument (the quantified formula) and then sets a default
    /// (undetermined) value for the quantifier node.
    /// </remarks>
    override this.Run() = 
        StaticDebug.Debug(this,Debug.Start)
        this.ArgList[0].Run()
        this.SetDefaultValue()
        StaticDebug.Debug(this,Debug.Stop)

/// <summary>
/// Concrete universal quantifier node ("all").
/// </summary>
/// <param name="positions">Start and end positions for diagnostics.</param>
/// <param name="parent">Parent symbol-table node.</param>
/// <returns>Instance of <c>FplQuantifierAll</c>.</returns>
type FplQuantifierAll(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericQuantifier(positions, parent)

    do 
        this.FplId <- LiteralAll

    override this.Name = PrimQuantifierAll

    /// <summary>
    /// Clone a universal quantifier node preserving positions and parent.
    /// </summary>
    override this.Clone () =
            let ret = new FplQuantifierAll((this.StartPos, this.EndPos), this.Parent.Value)
            this.AssignParts(ret)
            ret

/// <summary>
/// Concrete existential quantifier node ("exists").
/// </summary>
/// <param name="positions">Start and end positions for diagnostics.</param>
/// <param name="parent">Parent symbol-table node.</param>
/// <returns>Instance of <c>FplQuantifierExists</c>.</returns>
type FplQuantifierExists(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericQuantifier(positions, parent)

    do 
        this.FplId <- LiteralEx

    override this.Name = PrimQuantifierExists

    /// <summary>
    /// Clone an existential quantifier node preserving positions and parent.
    /// </summary>
    override this.Clone () =
            let ret = new FplQuantifierExists((this.StartPos, this.EndPos), this.Parent.Value)
            this.AssignParts(ret)
            ret

/// <summary>
/// Concrete existential-unique quantifier node ("exists exactly one").
/// </summary>
/// <param name="positions">Start and end positions for diagnostics.</param>
/// <param name="parent">Parent symbol-table node.</param>
/// <remarks>
/// This quantifier variant sets its arity to 1 (expecting an additional uniqueness parameter).
/// </remarks>
/// <returns>Instance of <c>FplQuantifierExistsN</c>.</returns>
type FplQuantifierExistsN(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericQuantifier(positions, parent)

    do 
        this.FplId <- LiteralExN
        this.Arity <- 1


    override this.Name = PrimQuantifierExistsN

    /// <summary>
    /// Clone an existential-unique quantifier node preserving positions and parent.
    /// </summary>
    override this.Clone () =
            let ret = new FplQuantifierExistsN((this.StartPos, this.EndPos), this.Parent.Value)
            this.AssignParts(ret)
            ret

