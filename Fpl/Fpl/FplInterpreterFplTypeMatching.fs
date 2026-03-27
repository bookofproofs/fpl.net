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
open FplPrimitives
open FplInterpreterBasicTypes
open FplInterpreterUtils
open FplInterpreterChecks
open FplInterpreterReferences
open FplInterpreterVariables
open FplInterpreterDefinitions


/// Gets the list of arguments of an FplValue if any
let getArguments (fv:FplGenericNode) =
    fv.ArgList 
    |> Seq.toList

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

/// Checks, if an FplValue uses parentheses or brackets
let hasBracketsOrParentheses (fv:FplGenericNode) = 
    match fv.Name with 
    | PrimVariableL ->
        let vars = fv.GetVariables()
        vars.Length > 0
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

/// Checks if the baseClassName is contained in the classRoot's base classes (it derives from).
/// If so, the function will produce Some path where path equals a string of base classes concatenated by ":".
/// The classRoot is required to have an FplValueType.Class.
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
                paths[newPath] <- $"cycle detected" 
            else
                // a cross-inheritance
                let cross = predecessors[currName] |> Seq.distinct |> String.concat "` and `"
                if cross.Contains " and " then 
                    paths[newPath] <- $"cross-inheritance not supported, `{currName}` is base for `{cross}`."
                else 
                    paths[newPath] <- $"duplicate inheritance from `{currName}` detected." 
        | false -> // a node encountered the very first time
            // add node name to distinct names
            distinctNames.Add currName |> ignore
            // add predecessor to node name
            predecessors.Add (currName, List<string>())
            predecessors[currName].Add predecessorName
            match baseNode.Name, bNode.Name with 
            | PrimFunctionalTermL, PrimFunctionalTermL 
            | PrimClassL, PrimClassL ->
                bNode.ArgList
                |> Seq.filter (fun subNode -> subNode :? FplBase)
                |> Seq.iter (fun subNode ->
                    findChains subNode currName newPath 
                )
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
                        paths[newPath] <- $"duplicate inheritance detected, `{newPath}`." 
                    else
                        paths.Add (newPath, "ok")
                | None ->
                    if paths.ContainsKey newPath then 
                        paths[newPath] <- $"duplicate inheritance detected, `{newPath}`." 
                    else
                        paths.Add (newPath, "ok")
            | _ -> ()
            
    match baseNode.Name with 
    | PrimClassL
    | PrimPredicateL
    | PrimFunctionalTermL -> ()
    | _ -> failwith ($"Expecting a class, a functional term, or a predicate node, got {baseNode.Name}")
    
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

let private refTypeName aName =
    if isUpper aName then 
        "reference"
    else 
        "variable"

let private errMsgStandard aIsCallByReference aName aType pName pType = 
    if aIsCallByReference then 
        Some $"The {refTypeName aName} `{aName}` typed `{aType}` doesn't match the parameter `{pName}` typed `{pType}`"
    else
        Some $"The application `{aName}` typed `{aType}` doesn't match the parameter `{pName}` typed `{pType}`"

let private errMsgMissingArgument pName pType = Some $"Missing argument for the parameter `{pName}` typed `{pType}`"
let private errMsgMissingParameter aName aType = Some $"No matching parameter for the argument `{aName}` typed `{aType}`"
let private errMsgClassValueNotAllowed actualClassType = Some $"A class `{actualClassType}` cannot be passed directly as a value. Use a class constructor `{actualClassType}(...)` instead"
let private errWrongReturnType aIsCallByReference aName aType pType (p:FplGenericNode) =
    let pBlockOpt = p.UltimateBlockNode
    let blockName = 
        match pBlockOpt with 
        | Some block -> block.Name
        | _ -> "undentified block" // should never occur
    
    if aIsCallByReference then 
        Some $"The returned {refTypeName aName} `{aName}` typed `{aType}` doesn't match the type `{pType}` this {blockName} returns."
    else 
        Some $"The returned application `{aName}` typed `{aType}` doesn't match the type `{pType}` this {blockName} returns."
let private errWrongClassInheritance aIsCallByReference aName aType pName pType = 
    if aIsCallByReference then 
        Some $"The {refTypeName aName} `{aName}` to the class `{aType}` neither matches the parameter `{pName}` typed `{pType}` nor the base classes of this type."
    else
        Some $"The application `{aName}` instantiating the class `{aType}` neither matches the parameter `{pName}` typed `{pType}` nor the base classes of this type."
let private errClassInheritanceUndetermined aIsCallByReference aName aType pName pType = 
    if aIsCallByReference then 
        Some $"The type `{aType}` of the {refTypeName aName} `{aName}` could not be determined. The parameter `{pName}` requires the type `{pType}` or any type derived from it"
    else
        Some $"The type `{aType}` of the application `{aName}` could not be determined. The parameter `{pName}` requires the type `{pType}` or any type derived from it"
let private errUndefined aIsCallByReference aName pName pType = 
    if aIsCallByReference then 
        Some $"The type of the {refTypeName aName} `{aName}` could not be determined. The parameter `{pName}` requires the type `{pType}"
    else
        Some $"The type of application `{aName}` could not be determined. The parameter `{pName}` requires the type `{pType}"
let private errVariadic aName aType pName pType pTypeId = 
    Some $"Variadic enumeration of `{aName}` typed `{aType}` doesn't match the parameter `{pName}` typed `{pType}`, try `{aName}:{pType}` as argument or use `{pName}:{pTypeId}[{LiteralInd}]` as parameter type"

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
            errWrongClassInheritance aIsCallByReference aName aType pName pType
    | _ -> 
        errClassInheritanceUndetermined aIsCallByReference aName aType pName pType

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
        errVariadic aName aType pName pType p.TypeId, Parameter.Consumed
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
            errMsgStandard aIsCallByReference aName aType pName pType, Parameter.Consumed
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
                errUndefined aIsCallByReference aName pName pType, Parameter.Consumed
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
            errUndefined aIsCallByReference aName pName pType, Parameter.Consumed
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
        | _ -> errMsgStandard aIsCallByReference aName aType pName pType, Parameter.Consumed
    elif aTypeName = PrimDefaultConstructor || aTypeName = LiteralCtorL then
        let ctor = a :?> FplGenericConstructor
        matchClassInheritance aIsCallByReference ctor.ToBeConstructedClass aName aType pName pType, Parameter.Consumed
    elif pTypeName = PrimFunctionalTermL || pTypeName = PrimMandatoryFunctionalTermL then
        let mappingOpt = getMapping p 
        match mappingOpt with 
        | Some mapping ->
            let newTypeAssignedValue = mapping.Type SignatureType.Type
            if aType <> newTypeAssignedValue then 
                errMsgStandard aIsCallByReference aName aType pName pType, Parameter.Consumed
            else 
                None, Parameter.Consumed
        | None -> None, Parameter.Consumed
    elif a.Parent.IsSome && a.Parent.Value.Name = PrimReturn then 
        errWrongReturnType aIsCallByReference aName aType pType p, Parameter.Consumed
    else
        errMsgStandard aIsCallByReference aName aType pName pType, Parameter.Consumed

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

let rec private matchTwoTypes (a:FplGenericNode) (p:FplGenericNode) =
    let aName, aType, aTypeName = getNames a 
    let pName, pType, pTypeName = getNames p

    match aTypeName, pTypeName with 
    | PrimClassL, PrimClassL 
    | PrimClassL, PrimVariableL ->
        errMsgClassValueNotAllowed aType, Parameter.Consumed
    | PrimVariableL, PrimMappingL
    | PrimRefL, PrimVariableL
    | PrimRefL, PrimMappingL ->
        let aIsCallByReference = isCallByReference a
        let callByReferenceToClass = getCallByReferenceToClass a
        let refNodeOpt = referencedNodeOpt a
        if callByReferenceToClass <> String.Empty then 
            errMsgClassValueNotAllowed callByReferenceToClass, Parameter.Consumed
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
            | Some refNode ->
                // a node was referenced but is not a predicate
                errWrongReturnType aIsCallByReference aName (refNode.Type SignatureType.Type) pType p, Parameter.Consumed
            | _ ->
                // in all other cases, 
                errMsgStandard aIsCallByReference aName aType pName pType, Parameter.Consumed
        elif aIsCallByReference && isPredWithoutParentheses p then
            // match a not-by-value-reference with pred mapping without parameters
            match refNodeOpt with 
            | Some refNode when refNode.Name = PrimIntrinsicPred ->
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
            | Some refNode ->
                // a node was referenced not a predicate node
                errMsgStandard aIsCallByReference aName (refNode.Type SignatureType.Type) pName pType, Parameter.Consumed
            | _ ->
                // in all other cases, error
                errMsgStandard aIsCallByReference aName aType pName pType, Parameter.Consumed
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
            | Some refNode ->
                // a node was referenced but is not a functional term block
                errWrongReturnType aIsCallByReference aName (refNode.Type SignatureType.Type) pType p, Parameter.Consumed
            | _ ->
                // in all other cases, error
                errMsgStandard aIsCallByReference aName aType pName pType, Parameter.Consumed
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
            | Some refNode ->
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
                | _ -> errMsgStandard aIsCallByReference aName aType pName pType, Parameter.Consumed
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
                errMsgStandard aIsCallByReference aName aType pName pType, Parameter.Consumed
        else 
            matchByTypeStringRepresentation aIsCallByReference a aName aType aTypeName p pName pType pTypeName
    | _ ,_ -> 
        matchByTypeStringRepresentation true a aName aType aTypeName p pName pType pTypeName

/// Tries to match a list of arguments with a list of parameters by their type recursively.
/// The comparison depends on MatchingMode.
let rec mpwa (args: FplGenericNode list) (pars: FplGenericNode list) =
    match (args, pars) with
    | (a :: ars, p :: prs) ->
        match matchTwoTypes (a:FplGenericNode) (p:FplGenericNode) with
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
                errMsgMissingArgument pName pType
        | _ when pTypeName = PrimVariableArrayL ->
            None
        | _ when p.ArgType = ArgType.Brackets ->
            // when p is an indexed array and being assigned a value, 
            // do not expect missing arguments
            None
        | _ -> 
            errMsgMissingArgument pName pType
    | (a :: _, []) ->
        let aName, aType, aTypeName = getNames a
        errMsgMissingParameter aName aType
    | ([], []) -> None

/// Tries to match the arguments of `fva` FplValue with the parameters of the `fvp` FplValue and returns
/// Some(specific error message) or None, if the match succeeded.
let matchArgumentsWithParameters (fva: FplGenericNode) (fvp: FplGenericNode) =
    let parameters = getParameters fvp
    let arguments = getArguments fva

    let aHasBracketsOrParentheses = hasBracketsOrParentheses fva
    let pHasBracketsOrParentheses = hasBracketsOrParentheses fvp
        

    let argResult = 
        if aHasBracketsOrParentheses <> pHasBracketsOrParentheses && arguments.Length = 0 && parameters.Length = 0 then 
            Some $"calling `{fva.Type SignatureType.Name}` and called `{fvp.Type SignatureType.Name}` nodes have mismatching use of parentheses"
        else
            mpwa arguments parameters 

    match argResult with
    | Some aErr -> 
        match fvp.Name with 
        | PrimVariableArrayL ->
            Some($"{aErr} in {qualifiedName fvp true}:{fvp.Type SignatureType.Type}")
        | _ -> 
            Some($"{aErr} in {qualifiedName fvp true}")
    | None -> None

