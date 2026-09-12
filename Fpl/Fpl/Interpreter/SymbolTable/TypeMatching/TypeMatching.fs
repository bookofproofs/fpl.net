(* Copyright (c) 2021+ bookofproofs See LICENSE in the project root for license terms. *)

/// <summary>
/// Utilities to compare and match FPL types used by the interpreter's symbol table.
/// </summary>
/// <remarks>
/// This module implements helpers for extracting parameters/arguments from symbol-table
/// nodes, computing distinct free variables of expressions, and matching argument lists
/// and individual types according to FPL's complex type and inheritance rules. Diagnostics
/// are emitted via the emitter helpers; the functions return optional error information
/// rather than throwing exceptions for semantic diagnostics.
/// </remarks>
module Fpl.Interpreter.SymbolTable.TypeMatching
open System
open System.Collections.Generic
open Fpl.Errors.Emitter
open Fpl.Primitives
open Fpl.Errors.Messages
open Fpl.Interpreter.BasicTypes
open Fpl.Interpreter.Helpers.Checks
open Fpl.Interpreter.SymbolTable.Types2.Variables
open Fpl.Interpreter.SymbolTable.Types2.References
open Fpl.Interpreter.SymbolTable.Types2.Definitions


/// <summary>
/// Get the parameter list for an FPL node if that node exposes parameters/signature variables.
/// </summary>
/// <param name="fv">Node whose parameters should be retrieved.</param>
/// <returns>List of parameter nodes; empty list if none.</returns>
/// <remarks>
/// Handles variable nodes, variable arrays (via IHasDimensions), functional/predicate blocks,
/// constructors and mandatory variants. For other nodes an empty list is returned.
/// </remarks>
let getParameters (fv:FplGenericNode) =
    match fv.Name with
    | PrimVariableL ->
        fv.GetVariables()
    | PrimVariableArrayL ->
        match box fv with 
        | :? IHasDimensions as arr -> arr.DimensionTypes |> Seq.toList
        | _ -> []
    | PrimExtensionL
    | PrimFunctionalTermL
    | PrimPredicateL
    | LiteralCtorL
    | PrimMandatoryPredicateL
    | PrimMandatoryFunctionalTermL ->
        fv.Scope.Values |> Seq.filter (fun fv -> isSignatureVar fv) |> Seq.toList
    | _ -> []

/// <summary>
/// Get the argument list for an FPL node.
/// </summary>
/// <param name="fv">Node whose argument list should be retrieved.</param>
/// <returns>Argument nodes as a list.</returns>
/// <remarks>
/// For references that point to a variable and have no own arguments, this falls back
/// to the referenced variable's parameters.
/// </remarks>
let getArguments (fv:FplGenericNode) =
    match fv.Name, fv.RefersTo with
    | PrimRefL, Some var when var.Name = PrimVariableL && fv.ArgList.Count = 0 ->
        // fallback to variable parameters,
        // if reference points to a variable and has no own arguments
        getParameters var
    | _ ->
        fv.ArgList 
        |> Seq.toList

/// <summary>
/// Compute the distinct set of variables used in an expression.
/// </summary>
/// <param name="expr">Expression node to analyze.</param>
/// <returns>List of distinct variable nodes referenced by the expression.</returns>
/// <remarks>
/// The function traverses the expression and collects variable references,
/// avoiding duplicates by variable identifier.
/// </remarks>
let getDistinctVarsOfExpression (expr:FplGenericNode) =
    let seen = new HashSet<string>()
    let rec getDVoE (expr1:FplGenericNode) acc = 
        match expr1.Name with
        | PrimRefL when expr1.RefersTo.IsSome ->
            match expr1.RefersTo with
            | Some (:? FplVariable as var) ->
                if seen.Contains var.FplId then
                    acc
                else
                    seen.Add var.FplId |> ignore
                    acc @ [var]
            | _ -> acc
        | _ -> 
            let args = getArguments expr1
            args
            |> List.map (fun arg -> getDVoE arg acc)
            |> List.concat
    getDVoE expr []

/// <summary>
/// Determine whether the node uses parentheses or brackets in its signature/usage.
/// </summary>
/// <param name="fv">Node to inspect.</param>
/// <returns><c>true</c> when parentheses or brackets are present; otherwise <c>false</c>.</returns>
/// <remarks>
/// This helper inspects variables, functional terms, predicates, constructors and references.
/// It is used to detect inconsistencies in call/parameter styles.
/// </remarks>
let hasBracketsOrParentheses (fv:FplGenericNode) = 
    match fv.Name with 
    | PrimVariableL when fv.ArgType = ArgType.Nothing ->
        let vars = fv.GetVariables()
        vars.Length > 0
    | PrimVariableL when fv.ArgType <> ArgType.Nothing -> true
    | PrimFunctionalTermL 
    | PrimPredicateL 
    | LiteralCtorL 
    | PrimDefaultConstructor 
    | PrimMandatoryPredicateL
    | PrimMandatoryFunctionalTermL -> true
    | PrimRefL -> 
        let refFv = fv :?> FplReference
        (refFv.ArgType = ArgType.Parentheses || refFv.ArgType = ArgType.Brackets)
    | _ -> false

let private errExprMismatchOpenFormulasWrapper (aOriginal:FplGenericNode) (aOpenFormula:FplGenericNode) (aFreeVars:FplGenericNode list) (pOriginal:FplGenericNode) (pOpenFormula:FplGenericNode) (pFreeVars:FplGenericNode list) = 
    let aName = aOriginal.Type SignatureType.Name
    let aOpenFormulaType = aOpenFormula.Type SignatureType.Type
    let pName = pOriginal.Type SignatureType.Name
    let pOpenFormulaType = pOpenFormula.Type SignatureType.Type

    /// <summary>
    /// Create comma-separated string from signature names of nodes.
    /// </summary>
    let lstToString (lst:FplGenericNode list) =
        lst
        |> List.map (fun fv -> fv.Type SignatureType.Name)
        |> String.concat ", "

    let openClosedStr (lstFreeVars:FplGenericNode list) =
        if lstFreeVars.Length > 0 then
            $"an open formula with the free variables `{lstToString lstFreeVars}`"
        else
            "a closed formula"
    let aVarsOpenClosedStr = openClosedStr aFreeVars
    let pVarsOpenClosedStr = openClosedStr pFreeVars
    errExprMismatchOpenFormulas aName aVarsOpenClosedStr aOpenFormulaType pName pVarsOpenClosedStr pOpenFormulaType 


/// <summary>
/// Find inheritance chains for a definition node (class, predicate, or functional term).
/// </summary>
/// <param name="baseNode">Definition node to analyze.</param>
/// <returns>
/// A dictionary mapping encountered node names (or path strings) to either "ok" or an error message describing cycles/cross-inheritance.
/// </returns>
/// <remarks>
/// The returned dictionary can be inspected to determine whether a given type is in the inheritance closure.
/// The function throws if the provided node is not a definition node.
/// </remarks>
/// <exceptions>
/// <exception cref="System.Exception">Thrown when baseNode is not a class, predicate or functional term definition.</exception>
/// </exceptions>
let findInheritanceChains (baseNode: FplGenericNode) =
    let distinctNames = HashSet<string>()
    let paths = Dictionary<string,string>() // collects all paths (keys) and errors (values)
    let predecessors = Dictionary<string,List<string>>() // inner dictionary = predecessors

    let rec findChains (bNode: FplGenericNode) predecessorName accPath =
        let currName = bNode.FplId
        let newPath = 
            if accPath = String.Empty then 
                currName
            else
                $"{accPath}:{currName}" 
        match distinctNames.Contains currName with
        | true -> // a cross-inheritance between two paths or a cycle detected
            predecessors[currName].Add predecessorName
            if predecessors[currName].Count = 1 then 
                // a cycle detected since currNode had only one predecessor so far
                // and thus, it must be the first one
                paths[newPath] <- errTypeMismatchInheritanceCycle 
            else
                // a cross-inheritance
                let cross = predecessors[currName] |> Seq.distinct |> String.concat "` and `"
                if cross.Contains " and " then 
                    paths[newPath] <- errTypeMismatchInheritanceCrossing currName cross
                else 
                    paths[newPath] <- errTypeMismatchInheritanceDuplicate currName
        | false -> // a node encountered the very first time
            // add node name to distinct names
            distinctNames.Add currName |> ignore
            // add predecessor to node name
            predecessors.Add (currName, List<string>())
            predecessors[currName].Add predecessorName
            match baseNode.Name, bNode.Name with 
            | PrimPredicateL, PrimPredicateL 
            | PrimFunctionalTermL, PrimFunctionalTermL 
            | PrimClassL, PrimClassL ->
                bNode.ArgList
                |> Seq.filter (fun subNode -> subNode :? FplBase)
                |> Seq.iter (fun subNode ->
                    findChains subNode currName newPath 
                )
            | PrimPredicateL, LiteralBase 
            | PrimFunctionalTermL, LiteralBase 
            | PrimClassL, LiteralBase ->
                match bNode.RefersTo with 
                | Some nextBNode ->
                    let baseNodes = 
                        nextBNode.ArgList
                        |> Seq.filter (fun subNode -> subNode :? FplBase)
                        |> Seq.toList
                    if baseNodes.Length > 0 then 
                        baseNodes
                        |> List.iter (fun subNode ->
                            findChains subNode currName newPath 
                        )
                    elif paths.ContainsKey newPath then 
                        paths[newPath] <- errTypeMismatchInheritanceDuplicate newPath 
                    else
                        paths.Add (newPath, "ok")
                | None ->
                    if paths.ContainsKey newPath then 
                        paths[newPath] <- errTypeMismatchInheritanceDuplicate newPath 
                    else
                        paths.Add (newPath, "ok")
            | _ -> ()
            
    match baseNode.Name with 
    | PrimClassL
    | PrimPredicateL
    | PrimFunctionalTermL -> ()
    | _ -> failwith (errTypeMismatchInheritanceFromNonDefinition baseNode.Name)
    
    findChains baseNode "" ""
    if paths.Count = 0 then 
        distinctNames |> Seq.iter (fun s -> paths.Add (s, "ok"))
    paths

/// <summary>
/// Check whether a node inherits from a given type name (or is that type).
/// </summary>
/// <param name="node">Node to check.</param>
/// <param name="someType">Type identifier to test.</param>
/// <returns><c>true</c> when the node inherits from or equals the given type identifier.</returns>
/// <remarks>
/// Accepts "obj" as the root of class inheritance. Uses <see cref="findInheritanceChains"/> to search closures.
/// </remarks>
let inheritsFrom (node:FplGenericNode) someType = 
    match node, someType with 
    | :? FplClass, "obj" -> true
    | :? FplClass, _  when node.FplId = someType -> true
    | _ -> 
        let inheritanceList = findInheritanceChains node 
        let inheritanceFound = 
            inheritanceList 
            |> Seq.filter (fun kvp -> 
                kvp.Value = "ok" && 
                (
                   kvp.Key = someType 
                || kvp.Key.EndsWith $":{someType}" 
                || kvp.Key.Contains $":{someType}:"
                )
            )
            |> Seq.tryLast
        match inheritanceFound with 
        | Some _ -> true
        | None -> false

type Parameter =
    | Consumed
    | NotConsumed

let private errWrongReturnType aName aType pType (p:FplGenericNode) =
    let pBlockOpt = p.UltimateBlockNode
    let blockName = 
        match pBlockOpt with 
        | Some block -> block.Name
        | _ -> "unidentified block" // should never occur
    errTypeMismatchReturnType aName aType pType blockName

let private matchClassInheritance (clOpt:FplGenericNode option) aName aType (pName:string) (pType:string) = 
    let pTypeSimple =
        if pType.StartsWith("*") then 
            let ret = pType.Substring(1).Split("[")
            ret[0]
        else
            pType
    match clOpt with 
    | Some cl -> 
        if inheritsFrom cl pTypeSimple then 
            None
        else
            errTypeMismatchInheritanceWrongBase aName aType pName pType
    | _ -> 
        errTypeMismatchInheritanceUndetermined aName aType pName pType

let private matchByTypeStringRepresentation (a:FplGenericNode) aName (aType:string) aTypeName (p:FplGenericNode) pName (pType:string) pTypeName = 

    if aType = pType then
        None, Parameter.Consumed
    elif aType = LiteralUndef then
        None, Parameter.Consumed // undef matches any type
    elif pType.StartsWith(LiteralTpl) || pType.StartsWith(LiteralTplL) then
        None, Parameter.Consumed // tpl accepts everything: TODO: really?
    elif pType.StartsWith($"*{LiteralTpl}") || pType.StartsWith($"*{LiteralTplL}") then
        None, Parameter.Consumed // tpl arrays accepts everything: TODO: really?
    elif aType = LiteralUndef then
        None, Parameter.Consumed // undef can always be assigned
    elif pType.StartsWith($"*{aType}[{LiteralInd}]") && p.ArgType <> ArgType.Brackets then
        None, Parameter.NotConsumed // 1D arrays matching input type with ind as index accept variadic enumerations
    elif pType.StartsWith($"*{aType}[") && p.ArgType <> ArgType.Brackets then
        // array parameters with indexes that differ from the FPL-inbuilt index type  
        // or with multidimensional index types will not accept variadic enumerations of arguments
        // even if they have the same type used for the values of the array
        errTypeMismatchVariadic aName aType pName pType p.TypeId, Parameter.Consumed
    elif pType.StartsWith($"*{aType}[") && p.ArgType = ArgType.Brackets then
        // array parameters with indexes that differ from the FPL-inbuilt index type  
        // or with multidimensional index types will not accept variadic enumerations of arguments
        // even if they have the same type used for the values of the array
        None, Parameter.NotConsumed // 1D arrays matching input type in assignments (ArgType.Brackets reference being assigned to a value)
    elif aType.StartsWith($"*{pType}[") && aTypeName = PrimRefL then
        let refA = a :?> FplReference
        if refA.ArgType = ArgType.Brackets then 
            // some array elements matching parameter type
            None, Parameter.Consumed
        else
            errTypeMismatchStandard aName aType pName pType, Parameter.Consumed
    elif pType.StartsWith($"{aType}:") && aType = LiteralObj then 
        None, Parameter.Consumed // extension type matching object type
    elif isUpper aType && aTypeName = PrimRefL && a.RefersTo.IsSome then
        let aReferencedNode = a.RefersTo.Value
        if aReferencedNode.RefersTo.IsSome then
            let aRef = aReferencedNode.RefersTo.Value
            match aRef.Name with
            | PrimClassL ->
                matchClassInheritance (Some aRef) aName aType pName pType, Parameter.Consumed 
            | PrimExtensionL ->
                let map = (getMapping aRef).Value
                matchClassInheritance map.RefersTo aName aType pName pType, Parameter.Consumed  
            | _ ->
                // this case does should not occur but we cover it as a fallback case
                errTypeMismatchUndefined aName pName pType, Parameter.Consumed
        elif aReferencedNode.Name = PrimDefaultConstructor || aReferencedNode.Name = LiteralCtorL then 
            let ctor = aReferencedNode :?> FplGenericConstructor
            matchClassInheritance ctor.ToBeConstructedClass aName aType pName pType, Parameter.Consumed
        elif aReferencedNode.Name = PrimFunctionalTermL || aReferencedNode.Name = PrimMandatoryFunctionalTermL then 
            let mapOpt = getMapping aReferencedNode
            let map = mapOpt.Value :?> FplMapping 
            matchClassInheritance map.RefersTo aName aType pName pType, Parameter.Consumed
        elif aReferencedNode.Name = PrimVariableL then 
            matchClassInheritance aReferencedNode.RefersTo aName aType pName pType, Parameter.Consumed
        else
            errTypeMismatchUndefined aName pName pType, Parameter.Consumed
    elif aType.StartsWith(pType + "(") then
        None, Parameter.Consumed
    elif aType.StartsWith(LiteralPred) && pType = LiteralPred then
        None, Parameter.Consumed
    elif aType.StartsWith(LiteralFunc) && pType = LiteralFunc then
        None, Parameter.Consumed
    elif aTypeName = PrimVariableL then
        let clOpt = a.Scope.Values |> Seq.tryHead
        match clOpt with 
        | Some (:? FplClass) -> matchClassInheritance clOpt aName aType pName pType, Parameter.Consumed
        | _ -> errTypeMismatchStandard aName aType pName pType, Parameter.Consumed
    elif aTypeName = PrimDefaultConstructor || aTypeName = LiteralCtorL then
        let ctor = a :?> FplGenericConstructor
        matchClassInheritance ctor.ToBeConstructedClass aName aType pName pType, Parameter.Consumed
    elif pTypeName = PrimFunctionalTermL || pTypeName = PrimMandatoryFunctionalTermL then
        let mappingOpt = getMapping p 
        match mappingOpt with 
        | Some mapping ->
            let newTypeAssignedValue = mapping.Type SignatureType.Type
            if aType <> newTypeAssignedValue then 
                errTypeMismatchStandard aName aType pName pType, Parameter.Consumed
            else 
                None, Parameter.Consumed
        | None -> None, Parameter.Consumed
    elif a.Parent.IsSome && a.Parent.Value.Name = PrimReturn then 
        errWrongReturnType aName aType pType p, Parameter.Consumed
    else
        errTypeMismatchStandard aName aType pName pType, Parameter.Consumed

let private isPredWithParentheses (fv:FplGenericNode) =
    match fv.ArgType with 
    | ArgType.Parentheses when fv.TypeId.StartsWith(LiteralPred) -> true
    | _ -> false

let private isPredWithoutParentheses (fv:FplGenericNode) =
    match fv.ArgType with 
    | ArgType.Nothing when fv.TypeId = LiteralPred -> true
    | _ -> false

let private isFuncWithParentheses (fv:FplGenericNode) =
    match fv.ArgType with 
    | ArgType.Parentheses when fv.TypeId.StartsWith(LiteralFunc) -> true
    | _ -> false

let private isFuncWithoutParentheses (fv:FplGenericNode) =
    match fv.ArgType with 
    | ArgType.Nothing when fv.TypeId = LiteralFunc -> true
    | _ -> false

/// <summary>
/// Determine whether a reference refers to a variable that points to a class instance-construction intent.
/// </summary>
/// <param name="fv">Reference or node to inspect.</param>
/// <returns>
/// The identifier of the referenced class when the convention indicates a call-by-reference to a class;
/// otherwise an empty string.
/// </returns>
/// <remarks>
/// The interpreter uses the IsInitialized flag combined with missing value to indicate that a variable
/// refers to a class itself (not a constructed instance). This helper discovers that convention.
/// </remarks>
let private getCallByReferenceToClass (fv:FplGenericNode) =
    match fv.RefersTo with 
    | Some refNode ->
        match refNode with 
        | :? FplGenericVariable as var when var.IsInitialized && var.Value.IsNone ->
            // reference fv points to an initialized variable without values 
            match var.RefersTo with
            | Some fv1 when fv1.Name = PrimClassL -> fv1.TypeId // and the variable points to a class
            | _ -> String.Empty
        | _ -> String.Empty
    | None ->
        String.Empty
               
let rec private isCallByReference (fv:FplGenericNode) =
    match fv with 
    | :? FplReference as ref when ref.DottedChild.IsSome ->
        isCallByReference ref.DottedChild.Value // evaluate dotted reference instead
    | _ ->
        match fv.ArgType with 
        | ArgType.Nothing when isUpper fv.FplId -> true
        | ArgType.Nothing -> true
        | _ -> false

let private getNames (fv:FplGenericNode) = 
    let fvName = fv.Type SignatureType.Name
    let fvType = fv.Type SignatureType.Type
    let fvTypeName = fv.Name
    fvName, fvType, fvTypeName


/// <summary>
/// Class-based entry point for complex type matching algorithms.
/// </summary>
/// <remarks>
/// The class groups mutually recursive matching helpers as members so they can refer to
/// each other even when declared later in the source file.
/// </remarks>
type FplTypeMatcher() =

    /// <summary>
    /// Attempt to match the argument list of a calling node to the parameters of a called node.
    /// </summary>
    /// <param name="fva">Calling node (argument provider).</param>
    /// <param name="fvp">Called node (parameter provider).</param>
    /// <returns>
    /// None when the match succeeds; Some error message string when the match fails.
    /// </returns>
    /// <remarks>
    /// Issues special fallbacks for variable parameters and attaches contextual location information
    /// to produced error messages.
    /// </remarks>
    static member MatchArgumentsWithParameters (fva: FplGenericNode) (fvp: FplGenericNode) =
        let parameters = getParameters fvp
        let arguments = getArguments fva

        let aHasBracketsOrParentheses = hasBracketsOrParentheses fva
        let pHasBracketsOrParentheses = hasBracketsOrParentheses fvp

        // Compute the initial result: either the special parentheses/brackets mismatch error
        // or the recursive arguments-vs-parameters match.
        let baseResult = 
            if aHasBracketsOrParentheses <> pHasBracketsOrParentheses && arguments.Length = 0 && parameters.Length = 0 then 
                Some $"The calling node `{fva.Type SignatureType.Name}` and the called node `{fvp.Type SignatureType.Name}` use parenthesized parameters inconsistently."
            else
                FplTypeMatcher.MatchPwA arguments parameters 

        // Helper to attach location/context to an error and to handle the special
        // fallback used when the parameter is a variable: try matching the whole
        // caller `fva` against the variable parameter `fvp`.
        let formatErrorWithContext err =
            match fvp.Name with
            | PrimVariableArrayL ->
                Some($"{err} Location: {qualifiedNameSimple fvp}:{fvp.Type SignatureType.Type}")
            | PrimVariableL when fvp.ArgType = ArgType.Parentheses ->
                // Fallback: attempt to match `fva` directly as a single argument against the variable parameter.
                match FplTypeMatcher.MatchPwA [ fva ] [ fvp ] with
                | Some fallbackErr -> Some fallbackErr
                | None -> None
            | PrimVariableL ->
                // Fallback: attempt to match `fva` directly as a single argument against the variable parameter.
                match FplTypeMatcher.MatchPwA [ fva ] [ fvp ] with
                | Some fallbackErr -> Some $"{err}; {fallbackErr}"
                | None -> None
            | _ ->
                Some($"{err} Location: {qualifiedNameSimple fvp}")

        match baseResult with
        | Some err -> formatErrorWithContext err
        | None -> None

    /// <summary>
    /// Recursively match an argument list against a parameter list.
    /// </summary>
    /// <param name="args">Arguments as a list of nodes.</param>
    /// <param name="pars">Parameters as a list of nodes.</param>
    /// <returns>None when match succeeds; Some error message when it fails.</returns>
    /// <remarks>
    /// Handles variadic/consumption semantics via the Parameter discriminant returned by individual matches.
    /// </remarks>
    static member MatchPwA (args: FplGenericNode list) (pars: FplGenericNode list) =
        let rec mpwa (args: FplGenericNode list) (pars: FplGenericNode list) =
            match (args, pars) with
            | (a :: ars, p :: prs) ->
                match FplTypeMatcher.MatchTwoTypes (a:FplGenericNode) (p:FplGenericNode) with
                | Some errMsg, _ -> Some errMsg
                | None, Parameter.Consumed -> mpwa ars prs
                | None, Parameter.NotConsumed -> mpwa ars pars // handle variadic parameters
            | ([], p :: prs) ->  
                let pName, pType, pTypeName = getNames p
                match p with 
                | :? FplClass as cl ->
                    let constructors = cl.GetConstructors()
                    if constructors.Length = 0 then
                        None
                    else
                        errTypeMismatchMissingArgument pName pType
                | _ when pTypeName = PrimVariableArrayL ->
                    None
                | _ when p.ArgType = ArgType.Brackets ->
                    // when p is an indexed array and being assigned a value, 
                    // do not expect missing arguments
                    None
                | _ -> 
                    errTypeMismatchMissingArgument pName pType
            | (a :: _, []) ->
                let aName, aType, aTypeName = getNames a
                errTypeMismatchMissingParameter aName aType
            | ([], []) -> None
        mpwa args pars

    static member private MatchTwoTypes (a:FplGenericNode) (p:FplGenericNode) =
        let rec matchTwoTypes (a:FplGenericNode) (p:FplGenericNode) =
            let aName, aType, aTypeName = getNames a 
            let pName, pType, pTypeName = getNames p

            match aTypeName, pTypeName with 
            | PrimRefL, _ when a.ExpressionType.IsParen ->
                // delegate parenthesized arguments to the contents of the parentheses
                // a has always a single argument due to symbol table structure of Ast.Parens
                matchTwoTypes a.ArgList[0] p 
            | PrimClassL, PrimClassL 
            | PrimClassL, PrimVariableL ->
                errTypeMismatchClassValueNotAllowed aType, Parameter.Consumed
            | PrimVariableL, PrimMappingL
            | PrimRefL, PrimVariableL
            | PrimRefL, PrimMappingL ->
                let aIsCallByReference = isCallByReference a
                let callByReferenceToClass = getCallByReferenceToClass a
                let refNodeOpt = referencedNodeOpt a
                if callByReferenceToClass <> String.Empty then 
                    errTypeMismatchClassValueNotAllowed callByReferenceToClass, Parameter.Consumed
                elif aIsCallByReference && isPredWithParentheses p then 
                    // match a call by reference with pred with parameters
                    match refNodeOpt with 
                    | Some refNode when refNode.Name = PrimPredicateL ->
                        matchTwoTypes refNode p // match signatures with parameters
                    | Some refNode when refNode.Name = PrimIntrinsicUndef -> 
                        None, Parameter.Consumed // mapping pred(...) accepting undef
                    | Some refNode when refNode.Name = PrimMandatoryPredicateL ->
                        matchTwoTypes refNode p // match signatures with parameters
                    | Some refNode when refNode.Name = PrimVariableL && refNode.TypeId = LiteralPred ->
                        matchTwoTypes refNode p // match signatures with parameters
                    | Some refNode when refNode.Name = PrimExtensionObj ->
                        matchTwoTypes refNode p // match signatures with parameters
                    | Some refNode when pTypeName = PrimMappingL ->
                        // a node was referenced but is not matching return 
                        errWrongReturnType aName (refNode.Type SignatureType.Type) pType p, Parameter.Consumed
                    | _ ->
                        // in all other cases, 
                        errTypeMismatchStandard aName aType pName pType, Parameter.Consumed
                elif aIsCallByReference && isPredWithoutParentheses p then
                    // match a not-by-value-reference with pred mapping without parameters
                    match refNodeOpt with 
                    | Some refNode when refNode.Name = PrimTrue ->
                        None, Parameter.Consumed // pred accepting intrinsic predicates
                    | Some refNode when refNode.Name = PrimFalse ->
                        None, Parameter.Consumed // pred accepting intrinsic predicates
                    | Some refNode when refNode.Name = PrimIntrinsicUndef -> 
                        None, Parameter.Consumed // mapping pred accepting undef
                    | Some refNode when refNode.Name = PrimPredicateL ->
                        None, Parameter.Consumed // pred accepting predicate nodes
                    | Some refNode when refNode.Name = PrimMandatoryPredicateL ->
                        None, Parameter.Consumed // pred accepting predicate properties
                    | Some refNode when refNode.Name = LiteralPrfL ->
                        None, Parameter.Consumed // pred accepting proofs
                    | Some refNode when refNode.Name = LiteralAxL ->
                        None, Parameter.Consumed // pred accepting axioms
                    | Some refNode when refNode.Name = LiteralThmL ->
                        None, Parameter.Consumed // pred accepting theorems
                    | Some refNode when refNode.Name = LiteralLemL ->
                        None, Parameter.Consumed // pred accepting lemmas
                    | Some refNode when refNode.Name = LiteralPropL ->
                        None, Parameter.Consumed // pred accepting propositions
                    | Some refNode when refNode.Name = LiteralCorL ->
                        None, Parameter.Consumed // pred accepting corollaries
                    | Some refNode when refNode.Name = LiteralConjL ->
                        None, Parameter.Consumed // pred accepting conjectures
                    | Some refNode when refNode.Name = PrimVariableL && refNode.TypeId = LiteralPred ->
                        None, Parameter.Consumed // pred accepting pred variables
                    | Some refNode when refNode.Name = PrimExtensionObj ->
                        matchTwoTypes refNode p // match signatures with parameters
                    | Some refNode when pTypeName = PrimMappingL ->
                        // a node was referenced but is not matching return 
                        errWrongReturnType aName (refNode.Type SignatureType.Type) pType p, Parameter.Consumed
                    | _ ->
                        // in all other cases, error
                        errTypeMismatchStandard aName aType pName pType, Parameter.Consumed
                elif aIsCallByReference && isFuncWithParentheses p then
                    // match a not-by-value-reference with func mapping with parameters
                    match refNodeOpt with 
                    | Some refNode when refNode.Name = PrimIntrinsicUndef -> 
                        None, Parameter.Consumed // mapping func(...)->.. accepting undef
                    | Some refNode when refNode.Name = PrimFunctionalTermL ->
                        matchTwoTypes refNode p // match signatures with parameters
                    | Some refNode when refNode.Name = PrimMandatoryFunctionalTermL ->
                        matchTwoTypes refNode p // match signatures with parameters
                    | Some refNode when refNode.Name = PrimVariableL && refNode.TypeId = LiteralFunc ->
                        matchTwoTypes refNode p // match signatures with parameters
                    | Some refNode when refNode.Name = PrimExtensionObj ->
                        matchTwoTypes refNode p // match signatures with parameters
                    | Some refNode when pTypeName = PrimMappingL ->
                        // a node was referenced but is not a functional term block
                        errWrongReturnType aName (refNode.Type SignatureType.Type) pType p, Parameter.Consumed
                    | _ ->
                        // in all other cases, error
                        errTypeMismatchStandard aName aType pName pType, Parameter.Consumed
                elif aIsCallByReference && isFuncWithoutParentheses p then 
                    // match a not-by-value-reference with func mapping with parameters
                    match refNodeOpt with 
                    | Some refNode when refNode.Name = PrimIntrinsicUndef -> 
                        None, Parameter.Consumed // mapping func accepting undef
                    | Some refNode when refNode.Name = PrimFunctionalTermL ->
                        None, Parameter.Consumed // func accepting functional term nodes
                    | Some refNode when refNode.Name = PrimMandatoryFunctionalTermL ->
                        None, Parameter.Consumed // func accepting functional term properties
                    | Some refNode when refNode.Name = PrimVariableL && refNode.TypeId = LiteralFunc ->
                        None, Parameter.Consumed // func accepting func variables
                    | Some refNode when refNode.Name = PrimExtensionObj ->
                        matchTwoTypes refNode p // match signatures with parameters
                    | Some refNode when pTypeName = PrimMappingL ->
                        // a node was referenced but is not a functional term block
                        errWrongReturnType aName (refNode.Type SignatureType.Type) pType p, Parameter.Consumed
                    | _ ->
                        // in all other cases, error
                        errWrongReturnType aName aType pType p, Parameter.Consumed
                elif aIsCallByReference && pTypeName = PrimMappingL then 
                    let map = p :?> FplMapping
                    match map.RefersTo, refNodeOpt with
                    | Some def, Some refNode when def.Name= PrimExtensionL && refNode.Name = PrimDelegateDecrementL && def.FplId = refNode.TypeId -> 
                        None, Parameter.Consumed // extension parameter accepting Decrement with same TypeId as the extension's FplId
                    | Some def, Some refNode when refNode.Name = PrimInstanceL -> 
                        matchTwoTypes a def
                    | Some _, Some refNode when refNode.Name = PrimIntrinsicUndef -> 
                        None, Parameter.Consumed // definition accepting undef
                    | Some def, Some (:? FplGenericVariable as refNode) -> 
                        matchTwoTypes a def
                    | Some def, Some extObj when extObj.Name = PrimExtensionObj -> 
                        matchTwoTypes extObj def
                    | Some (:? FplClass as pCl), Some (:? FplClass as aCl) -> 
                        matchTwoTypes pCl aCl
                    | None, Some refNode when map.TypeId = LiteralObj && refNode.Name = PrimInstanceL -> 
                        None, Parameter.Consumed // obj accepting instance
                    | None, Some (:? FplGenericVariable as refNode) when map.TypeId = LiteralObj -> 
                        let refNodeOpt1 = referencedNodeOpt refNode
                        match refNodeOpt1 with 
                        | Some (:? FplClass as cl) -> None, Parameter.Consumed // obj accepting instance
                        | _ when refNode.TypeId = LiteralObj && aType = pType -> None, Parameter.Consumed // obj accepting obj variable
                        | _ when pType = LiteralObj && refNode.TypeId.StartsWith($"{pType}:") -> None, Parameter.Consumed // obj accepting obj:<some regex> (relevant for FplExtension and FplExtensionObj only)
                        | _ -> errTypeMismatchStandard aName aType pName pType, Parameter.Consumed
                    | None, Some refNode when refNode.Name = PrimIntrinsicUndef -> 
                        None, Parameter.Consumed // anything accepting undef
                    | None, Some refNode -> 
                        matchTwoTypes refNode map
                    | None, None when aType = pType && isUpper aType -> 
                        Some $"`{aName}:{aType}` matches the expected type `{pType}` but the type is undefined.", Parameter.Consumed
                    | None, None when pType = LiteralObj && aType.StartsWith($"{pType}:") -> 
                        None, Parameter.Consumed // obj accepting obj:<some regex> (relevant for FplExtension and FplExtensionObj only)
                    | None, None when aType = pType -> 
                        None, Parameter.Consumed // obj accepting obj, ind accepting ind, pred accepting pred, func accepting func
                    | _, _ -> 
                        errTypeMismatchStandard aName aType pName pType, Parameter.Consumed
                else 
                    matchByTypeStringRepresentation a aName aType aTypeName p pName pType pTypeName
            | _, PrimVariableL when isCompoundPredicate a ->
                FplTypeMatcher.ComparisonBasedOnOpenFormulas a p
            | PrimPredicateL, PrimVariableL ->
                FplTypeMatcher.ComparisonBasedOnOpenFormulas a p
            | _ ,_ -> 
                matchByTypeStringRepresentation a aName aType aTypeName p pName pType pTypeName
        matchTwoTypes a p

    /// <summary>
    /// Transform an expression into an "open formula" capturing only distinct free variables.
    /// </summary>
    /// <param name="expr">Expression node to convert.</param>
    /// <returns>
    /// Some node representing the open formula (a top-level synthetic variable node carrying the free variables),
    /// or None if the input is not a predicate/functional-style expression.
    /// </returns>
    /// <remarks>
    /// This helper is used when matching open-formula style predicate/function comparisons.
    /// The returned node contains the distinct free variables in its scope.
    /// </remarks>
    static member GetOpenFormulaOfExpression (expr:FplGenericNode) =
        let outputType = 
            match isArgPred expr with
            | _, true -> LiteralPred
            | argType, false when argType.StartsWith(LiteralFunc) || argType.StartsWith(LiteralFuncL) -> LiteralFunc
            | argType, false when argType.StartsWith(LiteralTpl) || argType.StartsWith(LiteralTplL) -> argType
            | _, _ -> PrimNone

        match outputType with
        | PrimNone -> None
        | _ ->
            let topLevel =
                match expr.Name with
                | PrimVariableL ->
                    expr // do not mock expressions being variables as variables
                | _ ->
                    let topLevelVar = new FplVariable ("_",(expr.StartPos, expr.EndPos), expr)
                    topLevelVar.TypeId <- outputType
                    topLevelVar.ArgType <- ArgType.Parentheses
                    match getMapping expr with
                    | Some mapping -> topLevelVar.ArgList.Add mapping
                    | _ -> ()
                    topLevelVar

            let isRoot (fv:FplGenericNode) =
                match fv.Parent with
                | Some parent when isCompoundPredicate parent -> false
                | _ -> true

            let rec extractDistinctFreeVariables (fv:FplGenericNode) rootRecursion =
                match fv.Name with
                | PrimRefL when fv.ExpressionType.IsParen ->
                    // delegate parenthesized arguments to the contents of the parentheses
                    // fv has always a single argument due to symbol table structure of Ast.Parens
                    extractDistinctFreeVariables fv.ArgList[0] false
                | PrimRefL when fv.RefersTo.IsSome ->
                    match fv.RefersTo with
                    | Some (:? FplVariable as varCast) when rootRecursion ->
                        varCast.GetVariables()
                        |> List.map (fun v -> v :?> FplVariable)
                        |> List.filter(fun v -> not v.IsBound)
                        |> List.map (fun v -> topLevel.Scope.TryAdd(v.FplId, v))
                        |> ignore
                    | Some (:? FplVariable as varCast) when not rootRecursion
                        // only free variables
                        && (not varCast.IsBound) 
                        // and not variables with the type pred or func - in this case, 
                        // they syntactically stand for complex formulas, not for free variables 
                        && (varCast.TypeId <> LiteralPred)
                        && (varCast.TypeId <> LiteralFunc)
                        // and not variables with params - in this case, 
                        // they syntactically stand for complex formulas, not for free variables 
                        && not (varCast.Scope.Count > 0)
                        ->
                            topLevel.Scope.TryAdd(varCast.FplId, varCast) |> ignore
                            if fv.ArgList.Count = 0 then
                                varCast.GetVariables()
                                |> List.map (fun v -> topLevel.Scope.TryAdd(v.FplId,v)) |> ignore
                    | Some (:? FplPredicate) 
                    | Some (:? FplFunctionalTerm) ->
                        fv.ArgList |> Seq.iter (fun arg -> extractDistinctFreeVariables arg false)
                    | _ -> ()
                | PrimPredicateL ->
                    fv.GetVariables()
                    |> List.filter (fun v -> isSignatureVar v)
                    |> List.map (fun v -> topLevel.Scope.TryAdd(v.FplId,v)) |> ignore
                | _ when isCompoundPredicate fv ->
                    fv.ArgList |> Seq.iter (fun arg -> extractDistinctFreeVariables arg false)
                | _ -> ()

            let rootRecursion = isRoot expr
            extractDistinctFreeVariables expr rootRecursion
            Some topLevel

    /// <summary>
    /// Compare two nodes as open-formula expressions by converting both to open formulas and matching their free-variable signatures.
    /// </summary>
    /// <param name="a">Left expression.</param>
    /// <param name="p">Right expression (parameter).</param>
    /// <returns>
    /// A tuple (optionErrorMessage, Parameter) where the first component is an optional error message and the second indicates consumption semantics.
    /// </returns>
    static member ComparisonBasedOnOpenFormulas (a:FplGenericNode) (p:FplGenericNode) = 
        let aOpenFormulaOpt = FplTypeMatcher.GetOpenFormulaOfExpression a
        let pOpenFormulaOpt = FplTypeMatcher.GetOpenFormulaOfExpression p

        match aOpenFormulaOpt, pOpenFormulaOpt with
        | Some aOpenFormula, Some pOpenFormula ->
            let aFreeVars = getParameters aOpenFormula
            let pFreeVars = getParameters pOpenFormula
            match FplTypeMatcher.MatchPwA aFreeVars pFreeVars with
            | Some _ ->
                errExprMismatchOpenFormulasWrapper a aOpenFormula aFreeVars p pOpenFormula pFreeVars, Parameter.Consumed
            | None when aOpenFormula.TypeId <> pOpenFormula.TypeId ->
                errExprMismatchOpenFormulasWrapper a aOpenFormula aFreeVars p pOpenFormula pFreeVars, Parameter.Consumed
            | _ -> 
                errExprMismatchOK, Parameter.Consumed
        | _, _ ->
            // fallback, should never happen unless open formula calculation somehow fails
            errExprMismatchMsgStandard (a.Type SignatureType.Name) (p.Type SignatureType.Name), Parameter.Consumed


/// <summary>
/// Try all candidate signatures and accumulate errors.
/// </summary>
/// <param name="toBeMatched">The node whose signature is being matched.</param>
/// <param name="candidates">Candidate parameter providers.</param>
/// <param name="accResultList">Accumulator for collected error messages.</param>
/// <returns>
/// A pair (Some matchingCandidate, []) when a candidate matched, or (None, accumulatedErrors) when none matched.
/// </returns>
let rec checkCandidates (toBeMatched: FplGenericNode) (candidates: FplGenericNode list) (accResultList: string list) =
    match candidates with
    | [] -> (None, accResultList)
    | candidate :: candidates ->
        match FplTypeMatcher.MatchArgumentsWithParameters toBeMatched candidate with
        | None -> (Some candidate, [])
        | Some errMsg -> checkCandidates toBeMatched candidates (accResultList @ [ errMsg ])

/// <summary>
/// Check SIG04 diagnostics for a calling node against a set of candidate definitions.
/// </summary>
/// <param name="calling">The calling node.</param>
/// <param name="candidates">Candidate definitions to try.</param>
/// <returns>
/// Optionally the candidate that matched, otherwise None after emitting SIG04 diagnostic on the calling node.
/// </returns>
let checkSIG04Diagnostics (calling:FplGenericNode) (candidates: FplGenericNode list) = 
    if candidates.Length = 0 then
        None
    else
        match checkCandidates calling candidates [] with
        | (Some candidate,_) -> Some candidate // no error occurred
        | (None, errList) -> 
            let errListStr = numbered errList
            calling.ErrorOccurred <- emitSIG04Diagnostics (calling.Type SignatureType.Mixed) errListStr calling.StartPos calling.EndPos
            None

/// <summary>
/// Check operand/argument compatibility for an infix operator and emit SIG04 diagnostics on mismatches.
/// </summary>
/// <param name="infixOp">Reference node to operator (callable).</param>
/// <param name="firstOp">Left operand.</param>
/// <param name="secondOp">Right operand.</param>
/// <remarks>
/// The function inspects the operator's referenced definition, extracts signature variables and attempts per-operand matching.
/// </remarks>
let checkSIG04DiagnosticsForInfixOperator (infixOp:FplGenericNode) (firstOp:FplGenericNode) (secondOp:FplGenericNode) = 
    let refNodeOpt = referencedNodeOpt infixOp
    match refNodeOpt with 
    | Some refNode when refNode.Arity = 2 ->
        let pars = 
            refNode.GetVariables() 
            |> List.map (fun var -> var :?> FplGenericVariable)
            |> List.filter (fun var -> var.IsSignatureVariable)
        // try to issue SIG04 diagnostics per argument of the binary operator
        if pars.Length = 2 then 
            match FplTypeMatcher.MatchPwA [firstOp] [pars[0]] with
            | Some errMsg -> 
                let extendedErrMsg = $"{errMsg} Location: {qualifiedNameSimple refNode}"
                firstOp.ErrorOccurred <- emitSIG04Diagnostics (infixOp.Type SignatureType.Mixed) extendedErrMsg firstOp.StartPos firstOp.EndPos
            | _ -> ()
            match FplTypeMatcher.MatchPwA [secondOp] [pars[1]] with
            | Some errMsg -> 
                let extendedErrMsg = $"{errMsg} Location: {qualifiedNameSimple refNode}"
                secondOp.ErrorOccurred <- emitSIG04Diagnostics (infixOp.Type SignatureType.Mixed) extendedErrMsg secondOp.StartPos secondOp.EndPos
            | _ -> ()
        else
            // if something went wrong (for instance, wrong arity), issue SIG04 with fallback using the operand 
            // together with its referenced node
            checkSIG04Diagnostics infixOp [refNode] |> ignore
    | _ -> ()


/// <summary>
/// Validate reference-to-array indexing with respect to declared mapping dimensions and emit SIG08/SIG09/SIG10 diagnostics.
/// </summary>
/// <param name="referenceToArray">Reference node that addresses an array variable.</param>
/// <remarks>
/// The function compares the number of provided indexes and their types with the mapping's declared dimension types.
/// Mismatches are reported using SIG08 (index type mismatch), SIG09 (missing index) or SIG10 (extra index).
/// </remarks>
let checkSIG08_SIG10Diagnostics (referenceToArray:FplGenericNode) =
    let rec matchIndexesWithDimensions (refToArray:FplReference) =
        match refToArray.RefersTo with
        | Some (:? FplVariableArray as varArray) ->
            let rec matchAllIndexes (indexes:FplGenericNode list) (dims:FplGenericNode list) dimNumber =
                match indexes, dims with
                | i::ixs, d::dms ->
                    match FplTypeMatcher.MatchPwA [i] [d] with
                    | Some errMsg ->
                        // type mismatch between dimension and index
                        refToArray.ErrorOccurred <- emitSIG08Diagnostics varArray.FplId i.FplId (i.Type SignatureType.Type) (d.Type SignatureType.Type) dimNumber i.StartPos i.EndPos 
                        matchAllIndexes ixs dms (dimNumber + 1) 
                    | _ -> matchAllIndexes ixs dms (dimNumber + 1) 
                | [], d::dms -> 
                    // missing index for dimension dimOrdinal
                    refToArray.ErrorOccurred <- emitSIG09Diagnostics varArray.FplId (d.Type SignatureType.Type) dimNumber d.StartPos d.EndPos
                    matchAllIndexes [] dms (dimNumber + 1) 
                | i::ixs, [] -> 
                    // array has less dimensions, index at dimOrdinal not supported
                    refToArray.ErrorOccurred <- emitSIG10Diagnostics varArray.FplId (i.FplId) dimNumber i.StartPos i.EndPos
                    matchAllIndexes ixs [] (dimNumber + 1)  
                | [], [] -> ()

            let dims = varArray.DimensionTypes |> Seq.toList
            let indexes = refToArray.ArgList |> Seq.toList
            matchAllIndexes indexes dims 1
        | _ -> ()
    match referenceToArray with 
    | :? FplReference as refToArray -> matchIndexesWithDimensions refToArray
    | _ -> ()



