(* Copyright (c) 2021+ bookofproofs See LICENSE in the project root for license terms. *)

/// <summary>
/// Module that defines symbol-table nodes used to model and interpret rules of inference
/// and predicate lists in the FPL interpreter.
/// </summary>
/// <remarks>
/// Nodes in this module represent inference-related constructs such as predicate lists
/// and rules of inference. They provide symbol-table embedding, signature handling,
/// runtime registration in the valid-statement store and diagnostic emission. Diagnostics
/// are emitted via emitter helpers; semantic errors are recorded on nodes rather than thrown.
/// </remarks>
module Fpl.Interpreter.SymbolTable.Types3.RulesOfInferences
open FParsec
open Fpl.Primitives
open Fpl.Parser.Types
open Fpl.Interpreter.BasicTypes
open Fpl.Interpreter.Helpers.Basic
open Fpl.Interpreter.Helpers.Debug
open Fpl.Interpreter.SymbolTable.Storage.Heap
open Fpl.Interpreter.SymbolTable.Storage.Util
open Fpl.Interpreter.SymbolTable.Types2.Variables


/// <summary>
/// Represents a list of predicates used as the premise of a rule of inference.
/// </summary>
/// <param name="positions">Start and end positions in the source used for diagnostics.</param>
/// <param name="parent">Parent node in the symbol table that will contain the predicate list.</param>
/// <param name="runOrder">Execution ordering index used when running predicate lists.</param>
/// <remarks>
/// A predicate list is an action node with no intrinsic value; its Run implementation
/// ensures that contained predicate arguments are executed. It participates in symbol-table
/// embedding as an expression argument.
/// </remarks>
type FplPredicateList(positions: Positions, parent: FplGenericNode, runOrder) = 
    inherit FplGenericIsAction(positions, parent)
    let _runOrder = runOrder

    /// <summary>
    /// The symbolic name used for this node kind.
    /// </summary>
    override this.Name = LiteralPreL
    override this.ShortName = LiteralInf

    /// <summary>
    /// Create a shallow copy of the predicate list node preserving positions and parent.
    /// </summary>
    /// <returns>A cloned <c>FplPredicateList</c> instance.</returns>
    override this.Clone () =
        let ret = new FplPredicateList((this.StartPos, this.EndPos), this.Parent.Value, _runOrder)
        this.AssignParts(ret)
        ret

    /// <summary>
    /// Render the type/signature of the predicate list by joining argument signatures.
    /// </summary>
    /// <param name="signatureType">Requested signature rendering mode.</param>
    /// <returns>Joined signature string of contained predicates.</returns>
    override this.Type signatureType = signatureSep ", " this.ArgList signatureType

    /// <summary>
    /// Run all contained predicates to ensure side-effects and evaluations occur.
    /// </summary>
    /// <returns>Unit.</returns>
    /// <remarks>
    /// Predicate lists do not produce a value; this Run implementation simply invokes
    /// Run on each argument to ensure any nested evaluation is performed.
    /// </remarks>
    override this.Run() = 
        StaticDebug.Debug(this,Debug.Start)
        // ensure that all Run calls are executed recursively
        this.ArgList |> Seq.map (fun fv -> fv.Run()) |> ignore
        StaticDebug.Debug(this,Debug.Stop)

    /// <summary>
    /// Embed this predicate list into the parent's argument list.
    /// </summary>
    override this.EmbedInSymbolTable _ = addExpressionToParentArgList this

    /// <summary>
    /// Execution order index for this predicate list node.
    /// </summary>
    override this.RunOrder = Some _runOrder


/// <summary>
/// Represents a rule of inference definition node in the symbol table.
/// </summary>
/// <param name="positions">Start and end source positions used for diagnostics.</param>
/// <param name="parent">Parent AST/symbol node that will contain the rule.</param>
/// <param name="runOrder">Execution ordering index used for registration and running related constructs.</param>
/// <remarks>
/// A rule of inference typically contains a premise (predicate list) and a conclusion.
/// Registering a rule places it into the valid-statement store; embedding performs
/// signature/variable checks before adding it to the parent scope.
/// </remarks>
type FplRuleOfInference(positions: Positions, parent: FplGenericNode, runOrder) as this =
    inherit FplGenericIsAction(positions, parent)
    let _runOrder = runOrder
    let mutable _signStartPos = Position("", 0L, 0L, 0L)
    let mutable _signEndPos = Position("", 0L, 0L, 0L)

    do
        this.FplId <- LiteralUndef
        this.TypeId <- LiteralUndef

    /// <summary>
    /// Signature start position for diagnostics.
    /// </summary>
    member this.SignStartPos
        with get() = _signStartPos
        and set(value) = _signStartPos <- value

    /// <summary>
    /// Signature end position for diagnostics.
    /// </summary>
    member this.SignEndPos
        with get() = _signEndPos
        and set(value) = _signEndPos <- value

    interface IHasSignature with
        member this.SignStartPos 
            with get () = this.SignStartPos
            and set (value) = this.SignStartPos <- value
        member this.SignEndPos 
            with get () = this.SignEndPos
            and set (value) = this.SignEndPos <- value

    /// <summary>
    /// The premise of the rule, if present, is a predicate list inside the argument list.
    /// </summary>
    /// <returns>Optionally the <c>FplPredicateList</c> node used as premise.</returns>
    member this.Premise =
        this.ArgList
        |> Seq.tryFind (fun fv -> fv :? FplPredicateList)

    /// <summary>
    /// The conclusion node of the rule, typically the last argument.
    /// </summary>
    /// <returns>Optionally the conclusion node.</returns>
    member this.Conclusion =
        this.ArgList |> Seq.tryLast 

    /// <summary>
    /// Produce a ValidStatement describing the rule (premise → conclusion).
    /// </summary>
    /// <returns>A <c>ValidStatement</c> describing this rule's representation.</returns>
    member this.ValidExpression =
        let validityReason =
            match this.Premise, this.Conclusion with
            | Some pre, Some con ->
                let preExpr = pre.Type SignatureType.Name
                let conExpr = con.Type SignatureType.Name
                ValidityReason.IsRuleOfInference (preExpr,conExpr)
            | _ -> ValidityReason.Error // fallback if premise/conclusion are empty

        {
            ValidStatement.Node = this
            ValidStatement.ValidityReason = validityReason
        }

    interface IValid with
        member this.ValidExpression
            with get () = this.ValidExpression


    override this.Name = PrimRuleOfInference
    override this.ShortName = LiteralInf

    /// <summary>
    /// Create a shallow copy of this rule-of-inference node preserving positions and parent.
    /// </summary>
    /// <returns>A cloned <c>FplRuleOfInference</c> instance.</returns>
    override this.Clone () =
        let ret = new FplRuleOfInference((this.StartPos, this.EndPos), this.Parent.Value, _runOrder)
        this.AssignParts(ret)
        ret

    /// <summary>
    /// Return the textual/type head for the rule-of-inference node.
    /// </summary>
    /// <param name="signatureType">Requested signature rendering mode.</param>
    /// <returns>Head string according to <paramref name="signatureType"/>.</returns>
    override this.Type signatureType = getFplHead this signatureType
    
    override this.IsFplBlock () = true
    override this.IsBlock () = true    

    /// <summary>
    /// Run the rule-of-inference: register it in the valid-statement store.
    /// </summary>
    /// <returns>Unit.</returns>
    /// <remarks>
    /// Registration places the rule into the interpreter's valid-statement repository so it can be used
    /// when validating proofs and deriving conclusions.
    /// </remarks>
    override this.Run() = 
        StaticDebug.Debug(this,Debug.Start)
        heap.ValidStmtStore.RegisterExpression this |> ignore
        StaticDebug.Debug(this,Debug.Stop)

    /// <summary>
    /// Embed the rule into the parent symbol table after performing consistency checks.
    /// </summary>
    /// <remarks>
    /// Embedding executes variable-usage diagnostics (VAR04) and attaches the rule to the parent scope
    /// using its FPL identifier.
    /// </remarks>
    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        checkVAR04Diagnostics this
        tryAddToParentUsingFplId this

    /// <summary>
    /// Execution ordering index for this rule node.
    /// </summary>
    override this.RunOrder = Some _runOrder

