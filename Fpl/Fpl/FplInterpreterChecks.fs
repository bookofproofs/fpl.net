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
open System.Collections.Generic
open ErrDiagnostics
open FplPrimitives
open FplInterpreterDiagnosticsEmitter
open FplInterpreterBasicTypes
open FplInterpreter.Globals.Root

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

let checkSIG02Diagnostics (fv:FplGenericNode) symbol precedence pos1 pos2 = 
    let precedences = Dictionary<int, FplGenericNode>()
    let precedenceWasAlreadyThere precedence fv =
        if not (precedences.ContainsKey(precedence)) then
            precedences.Add(precedence, fv)
            false
        else
            true
    (root fv).Scope
    |> Seq.map (fun kv -> kv.Value)
    |> Seq.iter (fun theory ->
        theory.Scope
        |> Seq.map (fun kv1 -> kv1.Value)
        |> Seq.iter (fun block ->
            match block.ExpressionType with
            | FixType.Infix(_, precedence) -> precedenceWasAlreadyThere precedence block |> ignore
            | _ -> ()))
    if precedences.ContainsKey(precedence) then
        let conflict = precedences[precedence].QualifiedStartPos
        precedences[precedence].ErrorOccurred <- emitSIG02Diagnostics symbol precedence conflict pos1 pos2

/// Issue VAR10, if the formula in an FplValue uses 
/// quantor(s) and the variables bound by these quantor(s) are used elsewhere in the same formula
/// VAR10 => formula should be cleaned up by renaming the bound variables
let checkCleanedUpFormula (fv:FplGenericNode) =
    let formulaCreationInSymbolTableCompleted (formula:FplGenericNode) =
        match formula.Parent with 
        | Some parent ->
            match parent.Name with 
            | PrimConjunction
            | PrimDisjunction
            | PrimImplication
            | PrimEquivalence
            | PrimExclusiveOr
            | PrimNegation
            | PrimQuantorAll
            | PrimQuantorExists
            | PrimQuantorExistsN
            | PrimIsOperator
            | PrimRefL -> false
            | _ -> true
        | _ -> true

    let rec usedVariablesInFormula (formula:FplGenericNode) = 
        let extractFromSubFormula (subFormula:FplGenericNode) =
            subFormula.ArgList 
            |> Seq.map (fun subF -> usedVariablesInFormula subF)
            |> List.concat
        match formula.Name with 
        | PrimRefL when formula.RefersTo.IsSome ->
            match formula.RefersTo with
            | Some ref when ref.Name = PrimVariableL -> [formula] 
            | None when checkStartsWithLowerCase formula.FplId -> 
                [formula]  
            | _ -> extractFromSubFormula formula 
        | PrimQuantorAll
        | PrimQuantorExists
        | PrimQuantorExistsN -> (formula.Scope.Values |> Seq.toList) @ extractFromSubFormula formula.ArgList[0]  
        | _ ->
            extractFromSubFormula formula 

    let rec extractQuantors (formula:FplGenericNode) =
        let extractFromSubFormula (subFormula:FplGenericNode) =
            (subFormula.ArgList |> Seq.map (fun subF -> extractQuantors subF) |> List.concat)
        match formula.Name with 
        | PrimQuantorAll
        | PrimQuantorExists
        | PrimQuantorExistsN -> 
            [formula] @ extractFromSubFormula formula
        | _ -> 
            extractFromSubFormula formula

    let rec checkQuantors (formula:FplGenericNode) =
        let varUsedInQuantor (varInFormula:FplGenericNode) (quantor:FplGenericNode) =
            let varLStart = varInFormula.StartPos.Line
            let varCStart = varInFormula.StartPos.Column
            let varLEnd = varInFormula.EndPos.Line
            let varCEnd = varInFormula.EndPos.Column
            let quantorLStart = quantor.StartPos.Line
            let quantorCStart = quantor.StartPos.Column
            let quantorLEnd = quantor.EndPos.Line
            let quantorCEnd = quantor.EndPos.Column
            (
               (quantorLStart < varLStart && quantorLEnd > varLEnd) // lines(quantor) contain lines(variable)
            || (quantorLStart = varLStart && quantorLEnd > varLEnd && quantorCStart <= varCStart ) // if start line(q) = start line line(v) && end line(q) > end line(v), compare starting columns
            || (quantorLStart = varLStart && quantorLEnd = varLEnd && quantorCStart <= varCStart && quantorCEnd >= varCEnd) // if line(q) = line(v) for start and end, compare starting and ending columns
            )

        let varIsBoundByQuantor (varInFormula:FplGenericNode) (quantor:FplGenericNode) =
            quantor.Scope.ContainsKey(varInFormula.FplId)

        let quantors = extractQuantors formula
        let usedVariables = usedVariablesInFormula formula
        if quantors.Length > 0 then 
            usedVariables
            |> List.iter(fun varInFormula ->
                quantors 
                |> List.iter (fun quantor ->
                    if varIsBoundByQuantor varInFormula quantor && 
                        not (varUsedInQuantor varInFormula quantor) then 
                        let quantorVar = quantor.Scope[varInFormula.FplId]
                        fv.ErrorOccurred <- emitVAR10diagnostics varInFormula.FplId varInFormula.QualifiedStartPos quantorVar.StartPos quantorVar.EndPos
                )
            )                
            
    if formulaCreationInSymbolTableCompleted fv then
        // here, this reference points to a formula, which is final in the symbol table
        checkQuantors fv

let rec isInQuantor (fv:FplGenericNode) =
    match fv.Name with 
    | PrimQuantorAll
    | PrimQuantorExists
    | PrimQuantorExistsN -> true
    | _ ->
        match fv.Parent with 
        | Some parent -> isInQuantor parent
        | _ -> false

/// Checks if a variable is defined in the scope of block, if any
/// looking for it recursively, up the symbol tree.
let variableInBlockScopeByName (fplValue: FplGenericNode) name withNestedVariableSearch =
    let rec firstBlockParent (fv: FplGenericNode) =

        let qualifiedVar (fv1: FplGenericNode) =
            let allVarsInScope = fv1.GetVariables()

            // try out all variables in scope
            let foundList =
                allVarsInScope
                |> Seq.map (fun (var: FplGenericNode) ->
                    if var.Scope.ContainsKey name then
                        ScopeSearchResult.Found(var.Scope[name])
                    else
                        ScopeSearchResult.NotFound)
                |> Seq.filter (fun ssr -> ssr <> ScopeSearchResult.NotFound)
                |> Seq.toList

            if foundList.IsEmpty then
                firstBlockParent fv1.Parent.Value
            else
                foundList.Head
        if isTheory fv then 
            ScopeSearchResult.NotFound
        else
            match fv.Name with 
            | LiteralThmL 
            | LiteralLemL
            | LiteralPropL 
            | LiteralCorL
            | LiteralConjL 
            | PrimPredicateL
            | LiteralAxL
            | PrimRuleOfInference -> 
                if fv.Scope.ContainsKey name then
                    ScopeSearchResult.Found(fv.Scope[name])
                elif fv.Parent.IsSome then
                    if withNestedVariableSearch then
                        match qualifiedVar fv with
                        | ScopeSearchResult.NotFound -> firstBlockParent fv.Parent.Value
                        | s -> s
                    else
                        firstBlockParent fv.Parent.Value
                else
                    ScopeSearchResult.NotFound
            | _ ->
                match fv.Name with
                | LiteralCtorL
                | LiteralLocL
                | PrimQuantorAll
                | PrimQuantorExists
                | PrimQuantorExistsN
                | PrimMandatoryFunctionalTermL
                | PrimMandatoryPredicateL
                | LiteralPrfL
                | PrimExtensionL
                | PrimFunctionalTermL
                | PrimClassL ->
                    if fv.Scope.ContainsKey name then
                        ScopeSearchResult.Found(fv.Scope[name])
                    elif fv.Parent.IsSome then
                        if withNestedVariableSearch then
                            match qualifiedVar fv with
                            | ScopeSearchResult.NotFound -> firstBlockParent fv.Parent.Value
                            | s -> s
                        else
                            firstBlockParent fv.Parent.Value
                    else
                        ScopeSearchResult.NotFound
                | _ ->
                    if fv.Parent.IsSome then
                        firstBlockParent fv.Parent.Value
                    else
                        ScopeSearchResult.NotFound

    firstBlockParent fplValue

