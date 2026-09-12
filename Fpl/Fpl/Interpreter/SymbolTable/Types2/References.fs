(* Copyright (c) 2021+ bookofproofs See LICENSE in the project root for license terms. *)

/// <summary>
/// Module containing helper functions and reference node implementations that resolve
/// and evaluate references to other nodes in the symbol table.
/// </summary>
/// <remarks>
/// The reference semantics include resolving dotted names, delegated evaluation for
/// callable nodes, variable replacement using the heap state, and special handling
/// for extension objects. Diagnostics are emitted via the emitter helpers; methods
/// typically do not throw for semantic diagnostics.
/// </remarks>
module Fpl.Interpreter.SymbolTable.Types2.References
open System
open Fpl.Parser.Types
open Fpl.Primitives
open Fpl.Errors.Emitter
open Fpl.Interpreter.BasicTypes
open Fpl.Interpreter.Helpers.Checks
open Fpl.Interpreter.Helpers.Basic
open Fpl.Interpreter.Helpers.Debug
open Fpl.Interpreter.SymbolTable.Storage.Heap
open Fpl.Interpreter.SymbolTable.Types2.Variables

/// <summary>
/// Resolve the ultimate referenced node for a given node by following <c>RefersTo</c>,
/// dotted-child chains and certain intrinsic node kinds.
/// </summary>
/// <param name="fv">Node to search from.</param>
/// <returns>
/// Optionally the resolved target node. Returns <c>None</c> when no reference target exists.
/// </returns>
/// <remarks>
/// Works correctly for node types that utilize their scope (for example
/// <c>FplReference</c>, <c>FplSelf</c>, <c>FplParent</c>, <c>FplForInStmtDomain</c>,
/// <c>FplForInStmtEntity</c>, <c>FplVariable</c>).
/// Special handling unwraps <c>self</c> and <c>parent</c> indirections.
/// </remarks>
let rec referencedNodeOpt (fv:FplGenericNode) = 
    
    let refNodeOpt = 
        match box fv with 
        | :? IHasDotted as dotted when dotted.DottedChild.IsSome -> referencedNodeOpt dotted.DottedChild.Value
        | _ when fv.Name = PrimInstanceL -> Some fv
        | _ when fv.Name = PrimTrue -> Some fv
        | _ when fv.Name = PrimFalse -> Some fv
        | _ when fv.Name = PrimIntrinsicInd -> Some fv
        | _ when fv.Name = PrimVariableL -> fv.RefersTo
        | _ -> fv.RefersTo
    match refNodeOpt with
    | Some refNode when refNode.Name = LiteralSelf -> refNode.RefersTo
    | Some refNode when refNode.Name = LiteralParent -> refNode.RefersTo
    | _ -> refNodeOpt

/// <summary>
/// Abstract base for reference nodes that represent a reference to another symbol-table node.
/// </summary>
/// <param name="positions">Source positions used for diagnostics.</param>
/// <param name="parent">Parent node in the symbol table.</param>
/// <remarks>
/// This type implements evaluation semantics that handle callable references (with variable
/// replacement via the heap), extension objects and plain value references. It intentionally
/// avoids cloning to prevent stack overflow for recursive reference graphs.
/// </remarks>
[<AbstractClass>]
type FplGenericReference(positions: Positions, parent: FplGenericNode) =
    inherit FplGenericHasValue(positions, parent)
    
    /// <summary>
    /// Do not clone references to prevent recursion / stack overflow.
    /// </summary>
    override this.Clone () = this // do not clone references to prevent stack overflow 

    /// <summary>
    /// Invoke <paramref name="called"/> with variable replacement from this reference's arguments.
    /// </summary>
    /// <param name="called">Target routine or callable value to invoke.</param>
    /// <remarks>
    /// Uses the heap state to save/replace variables, stores caller positions for debugging,
    /// runs the called node and restores heap state. If arguments are not determined,
    /// the called node is treated as undetermined.
    /// </remarks>
    member private this.RunWithVariableReplacement (called:FplGenericHasValue) =
        match box called, this.NextBlockNode with
        | :? ICanBeCalledRecusively as calledRecursively, Some blockNodeOfThis when 
            Object.ReferenceEquals(blockNodeOfThis, called) && 
            calledRecursively.CallCounter > maxRecursion -> () // stop recursion
        | _ ->
            let allArgumentsHaveDeterminedValues, args =
                let args = ResizeArray()

                let allOk =
                    this.ArgList
                    // lazy: stops as soon as one argument is not determined
                    |> Seq.forall (fun arg ->
                        arg.Run()
                        args.Add arg
                        isDetermined arg
                    )

                allOk, List.ofSeq args

            // run subroutines only if all arguments have defined values
            if allArgumentsHaveDeterminedValues then 
                let pars = heap.State.SaveState(called) 
                heap.State.ReplaceVariables pars args
                // store the position of the caller
                heap.Helper.CallerStartPos <- this.StartPos
                heap.Helper.CallerEndPos <- this.EndPos
                // run all statements of the called node
                called.Run()
                // Correct the during the Run() created value of called (if any) has the same parent as this (i.e., the reference pointing to called).
                // This ensures that checks (like those for SIG12 diagnostics) work correctly, since they depend on the UltimateBlockNode,
                // in which a value is embedded in the symbol table. The UltimateBlockNode of the value changes with the its correct parent.
                match called.Value with
                | Some v -> v.Parent <- this.Parent
                | _ -> ()
                this.SetValueOf called
                heap.State.RestoreState called
            else
                called.SetDefaultValue()
                this.SetValueOf called

    /// <summary>
    /// Evaluate an extension object in the context of this reference, performing variable replacement.
    /// </summary>
    /// <param name="extensionObj">Extension object node to evaluate or copy as a value.</param>
    /// <remarks>
    /// If the extension object is being evaluated outside its own extension block this delegates
    /// evaluation to the extension's callable representation; otherwise the extension object is
    /// treated as a value and stored directly to the reference.
    /// </remarks>
    member private this.RunExtensionWithVariableReplacement (extensionObj:FplGenericNode)=
        match extensionObj.UltimateBlockNode, extensionObj.RefersTo with
        | Some enclosingNode, Some (:? FplGenericHasValue as calledExtension) when not (Object.ReferenceEquals(enclosingNode, calledExtension)) ->
            // if the extension object is called outside its own extension
            // delegate its evaluation to this extension
            let pars = heap.State.SaveState(calledExtension) 
            let args = [extensionObj]
            heap.State.ReplaceVariables pars args
            heap.Helper.CallerStartPos <- extensionObj.StartPos
            heap.Helper.CallerEndPos <- extensionObj.EndPos
            calledExtension.Run()
            
            // and store the value of the extension to this reference
            this.SetValueOf calledExtension
            heap.State.RestoreState(calledExtension)
        | _ ->
            // otherwise (i.e., inside the extensionObj's extension), 
            // treat the extensionObj as a value and store this value to the reference
            this.SetValue extensionObj

    /// <summary>
    /// Evaluate the reference and set its value according to the resolved target semantics.
    /// </summary>
    /// <remarks>
    /// Handles:
    /// - callable nodes (with or without parameters),
    /// - variables and arrays,
    /// - extension objects,
    /// - parenthesized expressions delegated to the child argument.
    /// The method uses heap-based variable replacement when invoking callables.
    /// </remarks>
    override this.Run() =
        StaticDebug.Debug(this,Debug.Start)
        let calledOpt = referencedNodeOpt this
        match calledOpt with 
        | Some (:? FplGenericHasValue as called) when isCallableWithParams called ->
            this.RunWithVariableReplacement called 
        | Some (:? FplGenericHasValue as called) when isCallableWithoutParams called ->
            called.Run()
            this.SetValueOf called
        | Some (:? FplGenericHasValue as called) ->
            match called.Name with
            | PrimVariableL ->
                called.Run()
                this.SetValueOf called
            | PrimDelegateEqualL
            | PrimDelegateDecrementL ->
                this.RunWithVariableReplacement called
                this.SetValueOf called
            | PrimVariableArrayL ->
                this.SetValue called
            | _ -> ()
        | Some (:? FplGenericIsValue as called) when called.Name = PrimExtensionObj ->
            this.RunExtensionWithVariableReplacement (called :> FplGenericNode)
        | Some (:? FplGenericIsValue as called) ->
            this.SetValue called
        | None when this.ExpressionType.IsParen ->
            // delegate parenthesized arguments to the contents of the parentheses
            // this has always a single argument due to symbol table structure of Ast.Parens
            let arg = this.ArgList[0]
            match arg with
            | :? FplGenericHasValue as arg1 ->
                arg1.Run()
                this.SetValueOf arg1
            | _ -> ()
        | _ -> ()
        StaticDebug.Debug(this,Debug.Stop)

    /// <summary>
    /// References do not participate in a separate run ordering.
    /// </summary>
    override this.RunOrder = None

/// <summary>
/// Concrete reference node used to represent ordinary references in the symbol table.
/// </summary>
/// <param name="positions">Source positions for diagnostics.</param>
/// <param name="parent">Parent AST/symbol table node.</param>
type FplReference(positions: Positions, parent: FplGenericNode) =
    inherit FplGenericReference(positions, parent)
    let mutable _callCounter = 0 

    let mutable _dottedChild : FplGenericNode option = None

    override this.Name = PrimRefL
    override this.ShortName = PrimRef

    /// <summary>
    /// Optional dotted child set during parsing for dotted-qualified references.
    /// </summary>
    member this.DottedChild
        with get() = _dottedChild
        and set (value:FplGenericNode option) = _dottedChild <- value

    interface IHasDotted with 
        member this.DottedChild 
            with get () = this.DottedChild
            and set (value) = this.DottedChild <- value

    /// <summary>
    /// When the reference points to a variable, propagate the set to the variable as well.
    /// </summary>
    /// <param name="fv">Value to assign to the referenced target.</param>
    override this.SetValue fv = 
        match this.RefersTo with
        | Some (:? FplGenericVariable as var) when var.Name = PrimVariableL ->
            var.SetValue fv
            base.SetValue fv
        | _ ->
            base.SetValue fv

    /// <summary>
    /// Compute the type/head rendering for this reference according to requested signature form.
    /// </summary>
    /// <param name="signatureType">Requested signature rendering mode.</param>
    /// <returns>Rendered head/type string for the reference.</returns>
    /// <remarks>
    /// Takes account of dotted qualifications, mapping fallbacks, fixed notation (prefix/suffix/infix), and special-case nodes.
    /// </remarks>
    override this.Type signatureType =
        let headObj = 
            match this.RefersTo with
            | Some ret when ret.Name = LiteralSelf && ret.RefersTo.IsSome -> ret.RefersTo.Value
            | Some ret when ret.Name = LiteralParent && ret.RefersTo.IsSome -> ret.RefersTo.Value
            | Some ret when ret.Name = PrimDelegateDecrementL && ret.RefersTo.IsSome -> ret.RefersTo.Value
            | Some ret -> ret
            | _ -> this

        let propagate = propagateSignatureType signatureType

        // The arguments are reserved for the arguments or the coordinates of the reference
        let args, argsCount =
            let ret = signatureSep ", " this.ArgList propagate
            ret, this.ArgList.Count

        let head = 
            let ret = 
                if headObj.Name = PrimExtensionL || headObj.Name = PrimDelegateDecrementL then 
                    headObj.Type signatureType
                elif headObj.ExpressionType.IsNoFix then
                    getFplHead headObj signatureType 
                else
                    headObj.ExpressionType.GetUserDefinedLiteral headObj.FplId
            match signatureType, ret, args with
            | SignatureType.Type, "", LiteralUndef -> ""
            | SignatureType.Type, "", "" -> LiteralUndef
            | SignatureType.Type, _, _ -> headObj.TypeId
            | _ -> ret

        let fallBackValueClosure =
            let varMappingOpt = getMapping headObj
            match varMappingOpt with 
            | Some varMapping ->
                match headObj.Name with 
                | PrimFunctionalTermL when signatureType = SignatureType.Type -> 
                    varMapping.Type propagate
                | PrimMandatoryFunctionalTermL when signatureType = SignatureType.Type -> 
                    varMapping.Type propagate
                | PrimExtensionL when signatureType = SignatureType.Type -> 
                    varMapping.Type propagate
                | _ -> 
                    $"{head}({args})"
            | None when signatureType = SignatureType.Type ->
                head
            | _ ->
                $"{head}({args})"

        let prefixNotation() =
            match argsCount, this.ArgType, this.DottedChild with
                | 0, ArgType.Nothing, Some qualification when propagate = SignatureType.Type ->
                    qualification.Type propagate
                | 0, ArgType.Nothing, Some qualification ->
                    $"{head}.{qualification.Type propagate}"
                | 0, ArgType.Brackets, Some qualification ->
                    $"{head}[].{qualification.Type propagate}"
                | 0, ArgType.Parentheses, Some qualification ->
                    $"{head}().{qualification.Type propagate}"
                | 0, ArgType.Nothing, None -> 
                    match headObj.Name with 
                    | PrimVariableArrayL 
                    | PrimPredicateL 
                    | PrimMandatoryPredicateL 
                    | PrimFunctionalTermL 
                    | PrimExtensionObj 
                    | PrimMandatoryFunctionalTermL -> $"{headObj.Type signatureType}"
                    | _ -> head
                | 0, ArgType.Brackets, None ->
                    $"{head}[]"
                | 0, ArgType.Parentheses, None ->
                    fallBackValueClosure
                | 1, ArgType.Nothing, None -> 
                    if head <> String.Empty then 
                        $"{head}({args})"
                    else
                        args
                | _, ArgType.Nothing, Some qualification -> 
                    $"{head}({args}).{qualification.Type propagate}"
                | _, ArgType.Brackets, Some qualification ->
                    $"{head}[{args}].{qualification.Type propagate}"
                | _, ArgType.Parentheses, Some qualification ->
                    $"{head}({args}).{qualification.Type propagate}"
                | _, ArgType.Nothing, None -> 
                    $"{head}({args})"
                | _, ArgType.Brackets, None ->
                    $"{head}[{args}]"
                | _, ArgType.Parentheses, None ->
                    fallBackValueClosure

        match signatureType, headObj.ExpressionType with
        | SignatureType.Type, _ ->
            prefixNotation()
        | _, FixType.Infix (symbol, _) ->
            getNotationTwoArgs this symbol signatureType (headObj.Type SignatureType.Type)
        | _, FixType.Symbol symbol -> symbol
        | _, FixType.Prefix symbol ->
            if isSimpleExpression this.ArgList[0] then
                $"{symbol}{args}"
            else
                $"{symbol}({args})"
        | _, FixType.Postfix symbol ->
            if isSimpleExpression this.ArgList[0] then
                $"{args}{symbol}"
            else
                $"({args}){symbol}"
        | _, FixType.Paren ->
            $"({args})"
        | _, _ ->
            prefixNotation()


    /// <summary>
    /// Compute a representation for the reference. This method guards against infinite recursion.
    /// </summary>
    /// <returns>String representation for the reference's current value or name.</returns>
    /// <remarks>
    /// Uses an internal call counter to detect recursion and emits LG002 diagnostics when a
    /// recursion limit is exceeded. It delegates representation to dotted child, referred-to nodes,
    /// or the node's Type as fallback.
    /// </remarks>
    override this.Represent() = // done
        if _callCounter > maxRecursion then
            this.ErrorOccurred <- emitLG002Diagnostics (this.Type(SignatureType.Name)) _callCounter this.StartPos this.EndPos
            LiteralUndet // fallback to undefined after infinite recursion (if any)
        else
            _callCounter <- _callCounter + 1
            let result =
                if this.ExpressionType.IsParen then
                    // delegate parenthesized arguments to the contents of the parentheses
                    // this has always a single argument due to symbol table structure of Ast.Parens
                    this.ArgList[0].Represent() 
                else
                    match this.Value, this.DottedChild, this.RefersTo with 
                    | _, Some dc, _ -> 
                        if not (Object.ReferenceEquals(dc, this)) then
                            // If the dotted child is not identical as "this",
                            // delegate the representation to dotted.
                            dc.Represent()
                        else
                            // Otherwise, fall back with dotted's "type representation" to prevent infinite loops
                            dc.Type SignatureType.Mixed
                    | Some value, _, _ ->
                        if not (Object.ReferenceEquals(value,this)) then
                            // If the value is not identical as "this",
                            // delegate the representation to value.
                            value.Represent()
                        else
                            // Otherwise, fall back with "undef" to prevent infinite loops
                            LiteralUndet
                    | _, _, Some refTo when refTo.Name = LiteralSelf && refTo.ErrorOccurred.IsSome ->
                        // infinite loop or other error in self detected
                        // fallback to undefined
                        LiteralUndet 
                    | _, _, Some refTo ->
                        if not (Object.ReferenceEquals(refTo,this)) then
                            // If refTo is not identical as "this",
                            // delegate the representation to refTo.
                            refTo.Represent()
                        else
                            refTo.Type SignatureType.Name // default to the name of the expression
                    | _, _, _ ->
                        this.Type SignatureType.Name // default to the name of the expression
            _callCounter <- _callCounter - 1
            result

    /// <summary>
    /// Run basic consistency checks for reference nodes.
    /// </summary>
    /// <remarks>
    /// Delegates to base checks and applies <c>checkCleanedUpFormula</c>.
    /// Diagnostics (if any) are recorded on the node and emitted via helper emitters.
    /// </remarks>
    override this.CheckConsistency () = 
        base.CheckConsistency()
        checkCleanedUpFormula this

    /// <summary>
    /// Embed the reference into the symbol table according to the context node.
    /// </summary>
    /// <param name="nextOpt">The context node that follows this reference.</param>
    /// <remarks>
    /// Handles block insertion, for-in domain referencing, dotted child end position
    /// propagation and map case context. Adjusts <c>EndPos</c> where appropriate.
    /// </remarks>
    override this.EmbedInSymbolTable nextOpt = 
        this.CheckConsistency()
        match nextOpt with 
        | Some next when next.IsBlock() ->
            addExpressionToParentArgList this 
        | Some next when next.Name = PrimForInStmtDomainL -> 
            next.RefersTo <- Some this
        | Some (:? FplReference as next) when next.DottedChild.IsSome -> 
            next.EndPos <- this.EndPos
        | Some next when (next.Name = PrimMapCaseElseL || next.Name = PrimMapCaseSingleL) -> 
            addExpressionToParentArgList this
            next.TypeId <- this.TypeId
            next.EndPos <- this.EndPos
        | Some next -> 
            addExpressionToParentArgList this
            next.EndPos <- this.EndPos
        | _ -> ()

    /// <summary>
    /// Return variables contained in the referenced node, if the reference points to a variable or variable array.
    /// </summary>
    /// <returns>List of variable nodes collected from the referred-to target; empty list otherwise.</returns>
    override this.GetVariables () =
        match this.RefersTo with
        | Some ref when ref.Name = PrimVariableL -> ref.GetVariables()
        | Some ref when ref.Name = PrimVariableArrayL -> ref.GetVariables()
        //| Some ref -> getSignatureVars ref
        | _ -> []

/// <summary>
/// Find candidate nodes by name when a dotted qualification exists.
/// </summary>
/// <param name="fv">Starting node (typically a reference) to search from.</param>
/// <param name="name">Name of the candidate to find.</param>
/// <returns>
/// A list of candidate nodes whose name matches and are accessible via the dotted qualification.
/// </returns>
/// <remarks>
/// For dotted-qualified references this attempts to locate the qualified entity and,
/// when it is a variable, returns its value or referred type node preferring the variable value.
/// </remarks>
let findCandidatesByNameInDotted (fv: FplGenericNode) (name: string) =
    let rec findQualifiedEntity (fv1: FplGenericNode) =
        match fv1 with
        | :? FplReference as ref when ref.DottedChild.IsSome -> 
            ScopeSearchResult.Found(ref.DottedChild.Value)
        | :? FplReference -> 
            match fv1.Parent with
            | Some parent -> findQualifiedEntity parent
            | None -> ScopeSearchResult.NotFound
        | _ -> ScopeSearchResult.NotFound

    match findQualifiedEntity fv with
    | ScopeSearchResult.Found candidate ->
        match candidate with
        | :? FplVariable as var ->
            // prefer variable value over its referred type node
            Option.orElse var.Value var.RefersTo |> Option.toList
        | _ -> []
    | _ -> []

