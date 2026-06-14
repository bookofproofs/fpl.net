/// This module contains all functions used by the FplInterpreter
/// to compare / match two FPL types

(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)
module FplInterpreterFplTypeMatching
open System
open System.Collections.Generic
open FplInterpreterDiagnosticsEmitter
open Fpl.Primitives
open ErrMessages
open FplInterpreterBasicTypes
open FplInterpreterChecks
open FplInterpreterReferences
open FplInterpreterVariables
open FplInterpreterDefinitions


/// Gets the list of parameters of an FplValue if any
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

/// Gets the list of arguments of an FplValue if any
let getArguments (fv:FplGenericNode) =
    match fv.Name, fv.RefersTo with
    | PrimRefL, Some var when var.Name = PrimVariableL && fv.ArgList.Count = 0 ->
        // fallback to variable parameters,
        // if reference points to a variable and has no own arguments
        getParameters var
    | _ ->
        fv.ArgList 
        |> Seq.toList

/// Gets the list of distinct variables used in an expression 
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

/// Checks, if an FplValue uses parentheses or brackets
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

    /// Generates a string of a FplGenericNode list based on their SignatureType.
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


/// Checks if the baseNode is contained in the roots's base nodes (it derives from).
/// If so, the function will produce Some path where path equals a string of base nodes concatenated by ":".
/// The baseNode is required to be a definition (i.e., FplClass, FplFunctionalTerm, or FplPredicate)
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

/// Checks if a node inherits from some type (or is already that type).
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


let private errWrongReturnType aIsCallByReference aName aType pType (p:FplGenericNode) =
    let pBlockOpt = p.UltimateBlockNode
    let blockName = 
        match pBlockOpt with 
        | Some block -> block.Name
        | _ -> "undentified block" // should never occur
    errTypeMismatchReturnType aIsCallByReference aName aType pType blockName

let private matchClassInheritance aIsCallByReference (clOpt:FplGenericNode option) aName aType (pName:string) (pType:string) = 
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
            errTypeMismatchInheritanceWrongBase aIsCallByReference aName aType pName pType
    | _ -> 
        errTypeMismatchInheritanceUndetermined aIsCallByReference aName aType pName pType

let private matchByTypeStringRepresentation aIsCallByReference (a:FplGenericNode) aName (aType:string) aTypeName (p:FplGenericNode) pName (pType:string) pTypeName = 

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
            errTypeMismatchStandard aIsCallByReference aName aType pName pType, Parameter.Consumed
    elif pType.StartsWith($"{aType}:") && aType = LiteralObj then 
        None, Parameter.Consumed // extenion type matching object type
    elif isUpper aType && aTypeName = PrimRefL && a.RefersTo.IsSome then
        let aReferencedNode = a.RefersTo.Value
        if aReferencedNode.RefersTo.IsSome then
            let aRef = aReferencedNode.RefersTo.Value
            match aRef.Name with
            | PrimClassL ->
                matchClassInheritance aIsCallByReference (Some aRef) aName aType pName pType, Parameter.Consumed 
            | PrimExtensionL ->
                let map = (getMapping aRef).Value
                matchClassInheritance aIsCallByReference map.RefersTo aName aType pName pType, Parameter.Consumed  
            | _ ->
                // this case does should not occur but we cover it as a fallback case
                errTypeMismatchUndefined aIsCallByReference aName pName pType, Parameter.Consumed
        elif aReferencedNode.Name = PrimDefaultConstructor || aReferencedNode.Name = LiteralCtorL then 
            let ctor = aReferencedNode :?> FplGenericConstructor
            matchClassInheritance aIsCallByReference ctor.ToBeConstructedClass aName aType pName pType, Parameter.Consumed
        elif aReferencedNode.Name = PrimFunctionalTermL || aReferencedNode.Name = PrimMandatoryFunctionalTermL then 
            let mapOpt = getMapping aReferencedNode
            let map = mapOpt.Value :?> FplMapping 
            matchClassInheritance aIsCallByReference map.RefersTo aName aType pName pType, Parameter.Consumed
        elif aReferencedNode.Name = PrimVariableL then 
            matchClassInheritance aIsCallByReference aReferencedNode.RefersTo aName aType pName pType, Parameter.Consumed
        else
            errTypeMismatchUndefined aIsCallByReference aName pName pType, Parameter.Consumed
    elif aType.StartsWith(pType + "(") then
        None, Parameter.Consumed
    elif aType.StartsWith(LiteralPred) && pType = LiteralPred then
        None, Parameter.Consumed
    elif aType.StartsWith(LiteralFunc) && pType = LiteralFunc then
        None, Parameter.Consumed
    elif aTypeName = PrimVariableL then
        let clOpt = a.Scope.Values |> Seq.tryHead
        match clOpt with 
        | Some (:? FplClass) -> matchClassInheritance aIsCallByReference clOpt aName aType pName pType, Parameter.Consumed
        | _ -> errTypeMismatchStandard aIsCallByReference aName aType pName pType, Parameter.Consumed
    elif aTypeName = PrimDefaultConstructor || aTypeName = LiteralCtorL then
        let ctor = a :?> FplGenericConstructor
        matchClassInheritance aIsCallByReference ctor.ToBeConstructedClass aName aType pName pType, Parameter.Consumed
    elif pTypeName = PrimFunctionalTermL || pTypeName = PrimMandatoryFunctionalTermL then
        let mappingOpt = getMapping p 
        match mappingOpt with 
        | Some mapping ->
            let newTypeAssignedValue = mapping.Type SignatureType.Type
            if aType <> newTypeAssignedValue then 
                errTypeMismatchStandard aIsCallByReference aName aType pName pType, Parameter.Consumed
            else 
                None, Parameter.Consumed
        | None -> None, Parameter.Consumed
    elif a.Parent.IsSome && a.Parent.Value.Name = PrimReturn then 
        errWrongReturnType aIsCallByReference aName aType pType p, Parameter.Consumed
    else
        errTypeMismatchStandard aIsCallByReference aName aType pName pType, Parameter.Consumed

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

/// Checks if an FplValue is a reference to a variable that points to a class, and at the same time is marked as 'initialized' and still does not any values.
/// (this is the convention flagging that a variable has been assigned to its class instead of the constructor of the class generating an instance value).
/// If the function returns a non-empty string, it contains the identifier of the referenced class (that has not been instantiated).
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


/// Type matching in FPL is complex and requires referencing functions declared later in code.
/// Since top‑level let bindings of pure functions does not allow this,
/// we use a class to match types in FPL. Inside an F# class,
/// members can freely reference other members that are declared later.
type FplTypeMatcher() =

    /// Tries to match the arguments of `fva` FplValue with the parameters of the `fvp` FplValue and returns
    /// Some(specific error message) or None, if the match succeeded.
    static member MatchArgumentsWithParameters (fva: FplGenericNode) (fvp: FplGenericNode) =
        let parameters = getParameters fvp
        let arguments = getArguments fva

        let aHasBracketsOrParentheses = hasBracketsOrParentheses fva
        let pHasBracketsOrParentheses = hasBracketsOrParentheses fvp

        // Compute the initial result: either the special parentheses/brackets mismatch error
        // or the recursive arguments-vs-parameters match.
        let baseResult = 
            if aHasBracketsOrParentheses <> pHasBracketsOrParentheses && arguments.Length = 0 && parameters.Length = 0 then 
                Some $"calling `{fva.Type SignatureType.Name}` and called `{fvp.Type SignatureType.Name}` nodes have mismatching use of parentheses"
            else
                FplTypeMatcher.MatchPwA arguments parameters 

        // Helper to attach location/context to an error and to handle the special
        // fallback used when the parameter is a variable: try matching the whole
        // caller `fva` against the variable parameter `fvp`.
        let formatErrorWithContext err =
            match fvp.Name with
            | PrimVariableArrayL ->
                Some($"{err} in {qualifiedName fvp true}:{fvp.Type SignatureType.Type}")
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
                Some($"{err} in {qualifiedName fvp true}")

        match baseResult with
        | Some err -> formatErrorWithContext err
        | None -> None

    /// Tries to match a list of arguments with a list of parameters by their type recursively.
    /// The comparison depends on MatchingMode.
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
                        errWrongReturnType aIsCallByReference aName (refNode.Type SignatureType.Type) pType p, Parameter.Consumed
                    | _ ->
                        // in all other cases, 
                        errTypeMismatchStandard aIsCallByReference aName aType pName pType, Parameter.Consumed
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
                        errWrongReturnType aIsCallByReference aName (refNode.Type SignatureType.Type) pType p, Parameter.Consumed
                    | _ ->
                        // in all other cases, error
                        errTypeMismatchStandard aIsCallByReference aName aType pName pType, Parameter.Consumed
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
                        errWrongReturnType aIsCallByReference aName (refNode.Type SignatureType.Type) pType p, Parameter.Consumed
                    | _ ->
                        // in all other cases, error
                        errTypeMismatchStandard aIsCallByReference aName aType pName pType, Parameter.Consumed
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
                        errWrongReturnType aIsCallByReference aName (refNode.Type SignatureType.Type) pType p, Parameter.Consumed
                    | _ ->
                        // in all other cases, error
                        errWrongReturnType aIsCallByReference aName aType pType p, Parameter.Consumed
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
                        | _ -> errTypeMismatchStandard aIsCallByReference aName aType pName pType, Parameter.Consumed
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
                        errTypeMismatchStandard aIsCallByReference aName aType pName pType, Parameter.Consumed
                else 
                    matchByTypeStringRepresentation aIsCallByReference a aName aType aTypeName p pName pType pTypeName
            | _, PrimVariableL when isCompoundPredicate a ->
                FplTypeMatcher.ComparisonBasedOnOpenFormulas a p
            | PrimPredicateL, PrimVariableL ->
                FplTypeMatcher.ComparisonBasedOnOpenFormulas a p
            | _ ,_ -> 
                matchByTypeStringRepresentation true a aName aType aTypeName p pName pType pTypeName
        matchTwoTypes a p

    /// Transforms a given expression to its open formula - a named
    /// formula that contains only the distinct free variables of the expression
    /// while preserving the expression's input FPL type (being either pred or func)
    /// The function returns None, if the input type is not pred or func.
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


/// Tries to match the signatures of toBeMatched with the signatures of all candidates and accumulates any
/// error messages in accResultList.
let rec checkCandidates (toBeMatched: FplGenericNode) (candidates: FplGenericNode list) (accResultList: string list) =
    match candidates with
    | [] -> (None, accResultList)
    | candidate :: candidates ->
        match FplTypeMatcher.MatchArgumentsWithParameters toBeMatched candidate with
        | None -> (Some candidate, [])
        | Some errMsg -> checkCandidates toBeMatched candidates (accResultList @ [ errMsg ])

/// Checks if there is a candidate among the candidates that matches the signature of a calling FplValue and returns this as an option.
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

/// Checks type consistency of an infix operation with respect to its operands
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
                let extendedErrMsg = $"{errMsg} in {qualifiedName refNode true}"
                firstOp.ErrorOccurred <- emitSIG04Diagnostics (infixOp.Type SignatureType.Mixed) extendedErrMsg firstOp.StartPos firstOp.EndPos
            | _ -> ()
            match FplTypeMatcher.MatchPwA [secondOp] [pars[1]] with
            | Some errMsg -> 
                let extendedErrMsg = $"{errMsg} in {qualifiedName refNode true}"
                secondOp.ErrorOccurred <- emitSIG04Diagnostics (infixOp.Type SignatureType.Mixed) extendedErrMsg secondOp.StartPos secondOp.EndPos
            | _ -> ()
        else
            // if something went wrong (for instance, wrong arity), issue SIG04 with fallback using the operand 
            // together with its referenced node
            checkSIG04Diagnostics infixOp [refNode] |> ignore
    | _ -> ()


/// Checks if a reference to an array matches its dimensions (in terms of number and types)
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
                        refToArray.ErrorOccurred <- emitSIG08diagnostics varArray.FplId i.FplId (i.Type SignatureType.Type) (d.Type SignatureType.Type) dimNumber i.StartPos i.EndPos 
                        matchAllIndexes ixs dms (dimNumber + 1) 
                    | _ -> matchAllIndexes ixs dms (dimNumber + 1) 
                | [], d::dms -> 
                    // missing index for dimension dimOrdinal
                    refToArray.ErrorOccurred <- emitSIG09diagnostics varArray.FplId (d.Type SignatureType.Type) dimNumber d.StartPos d.EndPos
                    matchAllIndexes [] dms (dimNumber + 1) 
                | i::ixs, [] -> 
                    // array has less dimensions, index at dimOrdinal not supported
                    refToArray.ErrorOccurred <- emitSIG10diagnostics varArray.FplId (i.FplId) dimNumber i.StartPos i.EndPos
                    matchAllIndexes ixs [] (dimNumber + 1)  
                | [], [] -> ()

            let dims = varArray.DimensionTypes |> Seq.toList
            let indexes = refToArray.ArgList |> Seq.toList
            matchAllIndexes indexes dims 1
        | _ -> ()
    match referenceToArray with 
    | :? FplReference as refToArray -> matchIndexesWithDimensions refToArray
    | _ -> ()



