/// This module contains all functions used by the FplInterpreter
/// for type and consistency checking

(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)
module FplInterpreterChecks
open System
open ErrDiagnostics
open FplPrimitives
open FplInterpreterDiagnosticsEmitter
open FplInterpreterUtils
open FplInterpreterBasicTypes
open FplInterpreterGlobals

let isArgPred (arg:FplGenericNode) = 
    let argType = arg.Type SignatureType.Type
    (argType, argType.StartsWith(LiteralPred))

/// Checks if an argument of an FplValue is a predicate and issues LG001Diagnostics if its not.
let checkArgPred (fv:FplGenericNode) (arg:FplGenericNode)  = 
    match fv.UltimateBlockNode with 
    | Some node when node.Name = LiteralLocL -> () // skip this check for localizations
    | _ ->
        let argType, isPred = isArgPred (arg:FplGenericNode) 
        if isPred then 
            () 
        else
            let argName = arg.Type SignatureType.Name
            fv.ErrorOccurred <- emitLG001Diagnostics argType argName fv.Name arg.StartPos arg.StartPos

/// Checks if a predicate expression is actually being interpreted as an predicate
let checkPredicateExpressionReturnsPredicate (fv:FplGenericNode) =
    let exprOpt = fv.ArgList |> Seq.tryLast
    match exprOpt with 
    | Some expr -> checkArgPred fv expr
    | None -> ()

/// Indicates if fv is an FplGenericNode that is callable with parameters.
let isCallableWithParams (fv:FplGenericNode) =
    match fv.Name with
    | LiteralCtorL
    | PrimDefaultConstructor
    | PrimBaseConstructorCall
    | PrimExtensionL
    | PrimPredicateL
    | PrimFunctionalTermL
    | PrimMandatoryFunctionalTermL
    | PrimMandatoryPredicateL -> true
    | _ -> false

/// Indicates if fv is an FplGenericNode that is callable without parameters.
let isCallableWithoutParams (fv:FplGenericNode) =
    match fv.Name with
    | LiteralAxL
    | LiteralConjL
    | LiteralCorL
    | LiteralPrfL
    | LiteralLemL
    | LiteralPropL
    | LiteralThmL -> true
    | _ -> false

/// Indicates if an FplValue is the root of the SymbolTable.
let isRoot (fv:FplGenericNode) = 
    match fv.Name with
    | PrimRoot -> true
    | _ -> false

/// Indicates if an FplValue is the root of the SymbolTable.
let isTheory (fv:FplGenericNode) = 
    match fv.Name with
    | PrimTheoryL -> true
    | _ -> false

let isDefinition (fv1:FplGenericNode) =
    match fv1.Name with
    | PrimClassL
    | PrimPredicateL
    | PrimFunctionalTermL -> true
    | _ -> false

/// Checks if an fv is provable. This will only be true if
/// it is a theorem, a lemma, a proposition, or a corollary
let isProvable (fv: FplGenericNode) =
    match fv.Name with
    | LiteralThmL
    | LiteralLemL
    | LiteralPropL
    | LiteralCorL -> true
    | _ -> false

/// Checks if an fplValue is a conjecture or an axiom. This is used to decide whether or
/// not it is not provable.
let isAxiomOrConnjecture (fv:FplGenericNode) = 
    match fv.Name with
    | LiteralConjL 
    | LiteralAxL -> true
    | _ -> false

let isLanguage (fv:FplGenericNode) =
    match fv.Name with
    | PrimLanguageL -> true
    | _ -> false

let isUpper (name:string) =  
    name.Length > 0 && System.Char.IsUpper(name[0])

/// Determines if the FplValue has parentheses and has an upper case FplId
let isCallByValue (fv:FplGenericNode) =
    match fv.ArgType with 
    | ArgType.Parentheses when isUpper fv.FplId -> true
    | _ -> false

let hasSignature (fv1:FplGenericNode) =
    match box fv1 with
    | :? IHasSignature -> true
    | _ -> false

let isSignatureVar (fv1:FplGenericNode) = 
    match box fv1 with 
    | :? IVariable as var when var.IsSignatureVariable -> true
    | _ -> false

let isVar (fv1:FplGenericNode) =
    match fv1.Name with
    | PrimVariableL
    | PrimVariableArrayL -> true
    | _ -> false

let isExtension (fv:FplGenericNode) =
    match fv.Name with
    | PrimExtensionL -> true
    | _ -> false


/// Qualified name of this FplValue
let qualifiedName (fplValue:FplGenericNode) determined =
    let rec getFullName (fv: FplGenericNode) (first: bool) =
        let fplValueType =
            match fv.Name with
            | LiteralLocL
            | PrimExclusiveOr 
            | PrimConjunction
            | PrimDisjunction 
            | PrimNegation
            | PrimImplication
            | PrimEquivalence 
            | PrimIsOperator 
            | PrimExtensionObj 
            | PrimDelegateEqualL 
            | PrimDelegateDecrementL 
            | PrimRefL -> fv.Type(SignatureType.Name)
            | LiteralCtorL
            | PrimBaseConstructorCall
            | PrimDefaultConstructor
            | PrimQuantorAll
            | PrimQuantorExists
            | PrimQuantorExistsN
            | PrimClassL
            | PrimPredicateL
            | PrimFunctionalTermL
            | PrimMandatoryPredicateL
            | PrimMandatoryFunctionalTermL -> fv.Type(SignatureType.Mixed)
            | _ -> fv.FplId

        match fv.Name with
        | PrimRoot -> ""
        | _ -> 


            if first then
                if fv.Parent.Value.Name = PrimRoot then
                    getFullName fv.Parent.Value false + fplValueType
                else if (isVar fv) && not (isVar fv.Parent.Value) then
                    fplValueType
                else
                    getFullName fv.Parent.Value false + "." + fplValueType
            elif fv.Parent.Value.Name = PrimRoot then
                getFullName fv.Parent.Value false + fplValueType
            elif (isVar fv) && not (isVar fv.Parent.Value) then
                fplValueType
            else
                getFullName fv.Parent.Value false + "." + fplValueType

    $"{getEnglishName fplValue.Name determined} {getFullName fplValue true}"


let checkSIG11Diagnostics (fv:FplGenericNode) =
    let mapOpt = getMapping fv
    match mapOpt with
    | Some map ->
        match map.RefersTo with 
        | Some ref ->
            match ref.Name with 
            | PrimClassL -> ()
                // mappings can point to classes 
            | PrimExtensionL ->
                let mapOfExtOpt = getMapping ref
                match mapOfExtOpt with
                | Some mapOfExt when mapOfExt.RefersTo.IsSome && Object.ReferenceEquals(ref, mapOfExt.RefersTo.Value) -> 
                // if a mapping points to an extension definition,
                // it is only allowed, if this extension does not delegate the mapping to another type
                // and, instead, points to itself
                    ()
                | _ ->
                    map.ErrorOccurred <- emitSIG11diagnostics (qualifiedName ref false) map.StartPos map.EndPos
            | _ ->
                // otherwise issue SIG11
                map.ErrorOccurred <- emitSIG11diagnostics (qualifiedName ref false) map.StartPos map.EndPos       
        | _ -> ()
    | _ -> ()

/// Checks if a predicate expected to be true was evaluated to false
let checkLG003Diagnostics (fv:FplGenericNode) =
    match box fv with
    | :? IHasSignature as hasSignature ->
        let nodeRepr = fv.Represent()
        if nodeRepr = LiteralFalse then
            fv.ErrorOccurred <- emitLG003diagnostic (fv.Type(SignatureType.Name)) fv.Name nodeRepr hasSignature.SignStartPos hasSignature.SignEndPos
    | _ -> ()

/// Checks if a reference to a Symbol, Prefix, PostFix, or Infix exists
let checkSIG01Diagnostics (fv: FplGenericNode)  =
    match fv.Name with
    | PrimRefL ->
        // collect candidates to match this reference from all theories and
        // add them to fplValues's scope
        let expressionId = fv.FplId

        (root fv).Scope
        |> Seq.map (fun kv -> kv.Value)
        |> Seq.iter (fun theory ->
            theory.Scope
            |> Seq.map (fun kv -> kv.Value)
            |> Seq.filter (fun fv1 -> isDefinition fv1)
            |> Seq.iter (fun block ->
                match block.ExpressionType with
                | FixType.Prefix symbol
                | FixType.Symbol symbol
                | FixType.Postfix symbol ->
                    if expressionId = symbol then
                        fv.RefersTo <- Some block 
                        fv.TypeId <- block.TypeId
                | FixType.Infix(symbol, precedence) ->
                    if expressionId = symbol then
                        fv.RefersTo <- Some block 
                        fv.TypeId <- block.TypeId
                | _ -> ()))

        if fv.RefersTo.IsNone then
            fv.ErrorOccurred <- emitSIG01Diagnostics expressionId fv.StartPos fv.EndPos
    | _ -> ()
