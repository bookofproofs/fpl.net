/// This module centralizes string-based error messages of FPL.
(* MIT License

Copyright (c) 2023 bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)

module Fpl.Errors.Messages
open System
open Fpl.Primitives


/// Transforms a whole number into English ordinal
let englishOrdinal dimNumber = 
    match dimNumber with
    | 1 -> "1st"
    | 2 -> "2nd"
    | 3 -> "3rd"
    | _ -> $"{dimNumber}th"

let numbered inputLst =
    inputLst
    |> Seq.mapi (fun i cand -> sprintf "%s  %d) %s" Environment.NewLine (i + 1) cand)
    |> String.concat ", "

let capitalize (word: string) =
    if String.IsNullOrEmpty word then word
    else word.[0].ToString().ToUpper() + word.Substring(1)

// Diagnostics related-errors
// -----------------------------------------------------------------
// interpreter error messages
let errGEN00 message = $"Unexpected error occurred: {message}" 
let errNSP00 fileNamePattern = $"The theory `{fileNamePattern}` was not found." 
let errNSP01 fileName innerErrMsg = $"The theory `{fileName}` was found but could not be loaded: {innerErrMsg}"  
let errNSP02 url innerErrMsg = $"The theory `{url}` was found but could not be downloaded: {innerErrMsg}" 
let errNSP03 alias = $"Alias `{alias}` was already declared in this namespace." 
let errNSP04 path = $"Circular theory reference detected: `{path}`." 
let errNSP05 pathTypes theory chosenSource = $"Multiple sources {pathTypes} for theory `{theory}` were detected (`{chosenSource}` was selected)."
    // identifier-related error codes 
let errID001 signature conflict = $"Signature `{signature}` was already declared in {conflict}."  
let errID002 signature incorrectBlockType = $"No compatible block found for proof `{signature}`; found a similar name of {incorrectBlockType}."  
let errID003 signature = $"Proof `{signature}` has no associated block."  
let errID005 signature incorrectBlockType = $"No compatible block found for corollary `{signature}`; found a similar name of {incorrectBlockType}."  
let errID006 signature = $"Corollary `{signature}` has no associated block."  
let errID007 nodeType signatureNode baseType signatureBase = $"{nodeType} `{signatureNode}` cannot inherit from incompatible base `{signatureBase}` ({baseType})."  
let errID008 constructorId classId  = $"Misspelled constructor name `{constructorId}`; expected `{classId}`."  
let errID009 name = $"Circular base-type dependency detected involving `{name}`." 
let errID010 name = $"The type `{name}` could not be found. Are you missing a uses clause?" 
let errID011 chain errorMsg = $"The inheritance chain `{chain}` is invalid: {errorMsg}"  
let errID012 prtyName varName varType candidates =
    if candidates = String.Empty then 
        $"{capitalize varName} `{varType}` does not define `{prtyName}`. No candidates found."  
    else
        $"{capitalize varName} `{varType}` does not define `{prtyName}`. Candidates considered:{candidates}."  
let errID013 delegateDiagnostic = sprintf "%s" delegateDiagnostic // just emit the delegate's diagnostic
let errID014 signature conflict = sprintf "Language code `%s` was already declared in %s." signature conflict
let errID015 signature = $"`parent` cannot be referenced in {signature}." 
let errID016 signature = $"`self` cannot be referenced in {signature}." 
let errID017 name (candidates:string) incompatible =

    if candidates = String.Empty then
        $"The type `{name}` was not found, no candidates found."  
    elif incompatible then
        $"The type `{name}` is not a base type of this class. Candidates considered:{candidates}."  
    else
        $"The type `{name}` could not be resolved due to ambiguity or incompatibility. Candidates considered:{candidates}."  
let errID018 name = $"No declared extension matches `{name}`. Declare an extension with this pattern." 
let errID020 name = $"Base constructor `{name}` was not invoked." 
let errID021 name = $"Base constructor `{name}` was invoked more than once."
let errID022 name = $"`{name}` is intrinsic and has no parameterized constructors. This call supplies parameters."
let errID023 candidates  = $"Cannot associate the justification with a single block due to ambiguity. Candidates considered:{candidates}." 
let errID024 signature conflict = sprintf "Expression `%s` was already localized in %s." signature conflict
let errID025 candidate nodeType  = $"Cannot reference to {candidate} inside {nodeType}." 
let errID027 name = $"Illegal recursion detected in `for` statement. The entity `{name}` cannot serve as its own iteration domain." 
// logic-related error codes
let errLG001 typeOfPredicate argument typeOfExpression = 
    if argument = typeOfExpression then 
        $"Cannot evaluate `{typeOfPredicate}` because its argument `{argument}` could not be evaluated as a predicate."
    else
        $"Cannot evaluate `{typeOfPredicate}` because its argument `{argument}` of type `{typeOfExpression}` could not be evaluated as a predicate."
let errLG002 nodeTypeName times = $"Possible infinite recursion detected, `{nodeTypeName}` was called more than {times} times.`."
let errLG003 nodeTypeName nodeName = $"Evaluation of `{nodeTypeName}` produced `false`; the node cannot be accepted as {nodeName}."
let errLG004 nodeType = $"A statement within {nodeType} may introduce side effects."
let errLG005 name = $"Unnecessary assignment to `{name}` detected; the statement will be ignored."
// proof-related error codes
let errPR001 incorrectBlockType justificatinItemName = $"Expected a {justificatinItemName}, but {incorrectBlockType} was found instead."
let errPR003 name conflict = $"Argument identifier `{name}` was previously declared in {conflict}."  
let errPR004 name = $"The justification item `{name}` is a duplicate. It was first declared earlier in the same argument scope." 
let errPR005 name =  $"Argument identifier `{name}` is not declared in this proof."
let errPR006 proofName argumentName =  $"Proof {proofName} exists, but it declares no argument with the identifier `{argumentName}`."
let errPR007 nodeName nodeTypeName =  $"{nodeTypeName} `{nodeName}` requires a proof, but none was provided."
let errPR008 byInfName numbPrem expectedPremise mismatchingCandidates =
    if numbPrem = 1 then 
        $"The subsequent `{LiteralByInf} {byInfName}` step requires a premise pattern `{expectedPremise}`. The provided justification does not match it. Candidates considered:{mismatchingCandidates}."
    else
        $"The subsequent `{LiteralByInf} {byInfName}` step requires {numbPrem} premise patterns `{expectedPremise}`.{Environment.NewLine}The provided justification does not match them. Candidates considered:{mismatchingCandidates}."

let errPR009 = "Not all arguments in the proof could be verified."
let errPR010 keyword expectedRef = $"Justification `{keyword}` expects a reference to {expectedRef}, but the provided reference points to a proof or a corollary."
let errPR011 keyword expectedRef = $"Justification `{keyword}` expects a reference to {expectedRef}, but the provided reference points to an argument in some proof."
let errPR012 providedIdentifer = $"Justification `{LiteralByCor}` expects a reference to a corollary, but `{providedIdentifer}` is not one."
let errPR013 = $"Add the keyword `{LiteralByCor}` when referencing corollaries to improve readability."
let errPR014 = "The justification must reference the top‑level theorem‑like statement, but the given reference targets a sub‑item."
let errPR015 argId = $"Cannot revoke argument `{argId}` because it was not assumed in this proof."
let errPR016 argId = $"Cannot revoke argument `{argId}` because revocation follows LIFO order."
let errPR017 = $"The `{LiteralTrivial}` justification may only be used on the final argument of a proof."
let errPR019 justificationType1 justificationType2 = $"Mixed justification types in a single argument are not supported (`{justificationType1}` with `{justificationType2}`)."
let errPR020 expectedNum actualNum = $"Justification `{PrimJIByInf}` requires {expectedNum} premise expressions, but it received {actualNum}."
let errPR021 mismatchingCandidates inferredFormula justificationName = $"The argument `{inferredFormula}` cannot be inferred from the preceding results. {justificationName} found the following candidates:{mismatchingCandidates}."
let errPR022 reason = $"The argument inference was prevented. Reason: {reason}"

// signature-related error codes
let errSIG00 fixType arity = sprintf $"Illegal arity `{arity}` using `{fixType}` notation."
let errSIG01 symbol = $"The symbol `{symbol}` was not declared." 
let errSIG02 symbol precedence conflict = $"The symbol `{symbol}` was declared with the same precedence of `{precedence}` in {conflict}." 
let errSIG03 errMsg = errMsg // Returned type is mismatching the mapping type
let errSIG04 signature candidates = $"No overload matching `{signature}`. Candidates considered:{Environment.NewLine}{candidates}." 
let errSIG05 errMsg = $"Cannot execute assignment; {errMsg}"
let errSIG06 name oldFromNode newFromNode typeName = 
    match typeName with 
    | PrimClassL -> $"Property `{name}` of base class `{oldFromNode} will be overshadowed by `{newFromNode}`."
    | PrimFunctionalTermL -> $"Property `{name}` of base functional term `{oldFromNode} will be overshadowed by `{newFromNode}`."
    | _ -> $"Property `{name}` of (unknown type) `{oldFromNode} will be overshadowed by `{newFromNode}`."
let errSIG07 assigneeName assigneeType nodeType = $"`{assigneeName}` is {nodeType} ({assigneeType}) and is not assignable."
let errSIG08 arrName indexVarName indexVarType dimType dimNumber = $"Type mismatch in array's `{arrName}` {englishOrdinal dimNumber} dimension; expected `{dimType}`, got `{indexVarName}:{indexVarType}`."
let errSIG09 arrName dimType dimNumber = $"Missing index for array's `{arrName}` {englishOrdinal dimNumber} dimension `{dimType}`"
let errSIG10 arrName indexVarName indexNumber = $"Array `{arrName}` has less dimensions, {englishOrdinal indexNumber} index `{indexVarName}` not supported"
let errSIG11 qualifiedWrongCandidate = $"Mapping to {qualifiedWrongCandidate} delegates the map to another type. Map to the other type directly, instead."
let errSIG12 templateName secondUsage firstUsage firstUsagePos = $"The template `{templateName}` was inconsistently used with `{secondUsage}`, expecting `{firstUsage}` as it was used in `{firstUsagePos}`."
let errSIG13 stmtName secondUsage firstUsage firstUsagePos = $"Every branch of the {stmtName} must return a value with a type of the first case in `{firstUsagePos}`, which was `{firstUsage}`. This branch returns `{secondUsage}`."
let errSIG14 = $"This case will never be matched."
// structure-related error codes
let errST001 nodeName = $"The {nodeName} does nothing."
let errST002 nodeName = $"The {nodeName} does nothing."
let errST004 langCode = $"The language `{langCode}` not implemented."
let errST005 domain nodeType = $"An enumerator for the domain `{domain}` being {nodeType} could not be established."
// interpreter syntax-related error codes for error-tolerant parser
let errSY000 errMsg = $"Syntax error: {errMsg}"
let errSY001 errMsg = $"Syntax error (backtracked): {errMsg}"
let errSY002 errMsg chain = $"Syntax error chain {chain}: {errMsg}"

let errSY010 = $"These parentheses can be safely removed."
let errSY011 = $"Replace `∃!0` by `¬∃` quantor."
let errSY012 = $"Expression `∃!1` can be simplified with `∃!`."
let errSY013 innerInfixSymbol innerPrecedence outerInfixSymbol outerPrecedence = $"These parentheses can be safely removed because the symbol's `{innerInfixSymbol}` precedence {innerPrecedence} is higher than the symbol's `{outerInfixSymbol}` precedence {outerPrecedence}."
let errSY014 infixSymbol1 infixSymbol2 precedence =
    match precedence with
    | -1 -> 
        $"This expression is ambiguous. The infix operators `{infixSymbol1}` and `{infixSymbol2}` are not defined. To resolve the ambiguity, either use parentheses to indicate the intended grouping or define binary operations (either a function or a predicate) for these symbols and assign them a precedence."
    | _ -> 
        $"This expression is ambiguous. The infix operators `{infixSymbol1}` and `{infixSymbol2}` have the same precedence {precedence}. To resolve the ambiguity, either use parentheses to indicate the intended grouping or assign different precedences to the symbols."
// variable-related error codes
let errVAR00 =  "Declaring multiple arrays at once may cause ambiguities."
let errVAR01 name = $"Variable `{name}` not declared in this scope."
let errVAR02 name = $"Variable `{name}` was already bound in this quantor."
let errVAR03 identifier conflict = $"Variable `{identifier}` was already declared in {conflict}."  
let errVAR04 name = $"Declared variable `{name}` not used in this scope."
let errVAR05 name = $"Bound variable `{name}` was not used in this quantor."
let errVAR06 name oldFromNode newFromNode typeName = 
    match typeName with 
    | PrimClassL -> $"Variable `{name}` of base class `{oldFromNode} will be overshadowed by `{newFromNode}`."
    | PrimFunctionalTermL -> $"Variable `{name}` of base functional term `{oldFromNode} will be overshadowed by `{newFromNode}`."
    | _ -> $"Variable `{name}` of (unknown type) `{oldFromNode} will be overshadowed by `{newFromNode}`."
let errVAR07 name = $"The {PrimQuantorExistsN} accepts only one bound variable `{name}`."
let errVAR08 = "Variadic variables cannot be bound in a quantor."
let errVAR09 varName varType = $"The variable {varName}:{varType} is free and cannot be used to evaluate this expression."
let errVAR10 identifier formulaName = $"The variable `{identifier}` is bound more than once in the formula `{formulaName}`."  
let errVAR11 identifier conflict = $"All variables in a {LiteralLocL} have to be different. The `{identifier}` was used in {conflict}."

// type matching-related errors
// -----------------------------------------------------------------
let errTypeMismatchStandard aIsCallByReference aName aType pName pType = 
    if aIsCallByReference then 
        Some $"The expression `{aName}` of type `{aType}` doesn't match the parameter `{pName}` of type `{pType}`"
    else
        Some $"The application `{aName}` of type `{aType}` doesn't match the parameter `{pName}` of type `{pType}`"

let errTypeMismatchMissingArgument pName pType = Some $"Missing argument for the parameter `{pName}` of type `{pType}`"
let errTypeMismatchMissingParameter aName aType = Some $"No matching parameter for the argument `{aName}` of type `{aType}`"
let errTypeMismatchClassValueNotAllowed actualClassType = Some $"A class `{actualClassType}` cannot be passed directly as a value. Use a class constructor `{actualClassType}(...)` instead"
let errTypeMismatchReturnType aIsCallByReference aName aType pType blockName =
    if aIsCallByReference then 
        Some $"The returned expression `{aName}` of type `{aType}` doesn't match the type `{pType}` this {blockName} returns."
    else 
        Some $"The returned application `{aName}` of type `{aType}` doesn't match the type `{pType}` this {blockName} returns."
let errTypeMismatchInheritanceCycle = "cycle detected"
let errTypeMismatchInheritanceCrossing currName crossName = $"cross-inheritance is not supported. `{currName}` is already a base type of `{crossName}`."
let errTypeMismatchInheritanceDuplicate duplicate = $"duplicate inheritance from `{duplicate}` detected."
let errTypeMismatchInheritanceFromNonDefinition blockName = $"Expecting a class, a functional term, or a predicate node, got {blockName}"
let errTypeMismatchInheritanceWrongBase aIsCallByReference aName aType pName pType = 
    if aIsCallByReference then 
        Some $"The expression `{aName}` to the class `{aType}` neither matches the parameter `{pName}` of type `{pType}` nor the base classes of this type."
    else
        Some $"The application `{aName}` instantiating the class `{aType}` neither matches the parameter `{pName}` of type `{pType}` nor the base classes of this type."
let errTypeMismatchInheritanceUndetermined aIsCallByReference aName aType pName pType = 
    if aIsCallByReference then 
        Some $"The type `{aType}` of the expression `{aName}` could not be determined. The parameter `{pName}` requires the type `{pType}` or any type derived from it"
    else
        Some $"The type `{aType}` of the application `{aName}` could not be determined. The parameter `{pName}` requires the type `{pType}` or any type derived from it"
let errTypeMismatchUndefined aIsCallByReference aName pName pType = 
    if aIsCallByReference then 
        Some $"The type of the expression `{aName}` could not be determined. The parameter `{pName}` requires the type `{pType}"
    else
        Some $"The type of application `{aName}` could not be determined. The parameter `{pName}` requires the type `{pType}"
let errTypeMismatchVariadic aName aType pName pType pTypeId = 
    Some $"Variadic enumeration of `{aName}` of type `{aType}` doesn't match the parameter `{pName}` of type `{pType}`, try `{aName}:{pType}` as argument or use `{pName}:{pTypeId}[{LiteralInd}]` as parameter type"

// expression-matching-related errors
// -----------------------------------------------------------------
let errExprMismatchExistsN aFplId aName pFplId pName = Some $"type mismatch in exists quantor: `{aFplId}` was provided in `{aName}`, but type `{pFplId}` was required in `{pName}`"


let errExprMismatchQuantorVariableTypes aName pName xName yName index = Some $"type mismatch: `{xName}` was provided in the {englishOrdinal index} bound variable in `{aName}`, but `{yName}` was required in `{pName}`"

let errExprMismatchQuantorVariableCounts aName pName aVarsCount pVarsCount = Some $"found {aVarsCount} bound variables in `{aName}`, expected {pVarsCount} in `{pName}`" 

let errExprMismatchOpenFormulas aName aVarsOpenClosedStr aOpenFormulaType pName pVarsOpenClosedStr pOpenFormulaType = Some $"found expression `{aName}` ({aVarsOpenClosedStr} of type `{aOpenFormulaType}`), expected `{pName}` which is {pVarsOpenClosedStr} of type `{pOpenFormulaType}`"

let errExprMismatchExpectedEndOfFormula (aName) = Some $"`found {aName}`, expected end of formula"
let errExprMismatchFoundEndOfFormula pName = Some $"found end of formula, expected `{pName}`"
let errExprMismatchVarMatchedDifferently varName expectedExpr actualExpr = Some $"variable `{varName}` was matched with different formulas `{expectedExpr}` and `{actualExpr}`"
let errExprMismatchVarMatchedDifferentlyQuantor varName expectedExpr actualExpr = Some $"variable `{varName}` was matched with different quantor formulas `{expectedExpr}` and `{actualExpr}`.{Environment.NewLine}Both formulas were different even using placeholders for bound variables."
let errExprMismatchMsgStandard aName pName = Some $"found `{aName}`, expected `{pName}`"
let errExprMismatchMsgParensOnlyLeft aName pName = Some $"found `{aName}` in parens, expected `{pName}` without parens"
let errExprMismatchMsgParensOnlyRight aName pName = Some $"found `{aName}` without parens, expected `{pName}` in parens"
let errExprMismatchVarNumbDifferent numA varsA numP pName =
    let plural = if numA > 1 then "variables" else "variable"
    Some $"found {numA} {plural} ({varsA}), expected {numP} in {pName}"
let (errExprMismatchOK:string option) = None



