/// This module centralizes string-based error messages of FPL.
(* MIT License

Copyright (c) 2023 bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)

module ErrMessages
open System
open FplPrimitives


/// Transforms a whole number into English ordinal
let englishOrdinal dimNumber = 
    match dimNumber with
    | 1 -> "1st"
    | 2 -> "2nd"
    | 3 -> "3rd"
    | _ -> $"{dimNumber}th"

let numbered inputLst =
    inputLst
    |> Seq.mapi (fun i cand -> sprintf "%s%d) %s" Environment.NewLine (i + 1) cand)
    |> String.concat ", "

// Diagnostics related-errors
// -----------------------------------------------------------------
// parser error messages
let errSYN000 = "Other syntax error"
let errSYN001 = "Characters found after namespace"
let errDEF000 = "Syntax error in definition"
let errPRP000 = "Syntax error in property"
let errAXI000 = "Syntax error in axiom"
let errTHM000 = "Syntax error in theorem"
let errCOR000 = "Syntax error in corollary"
let errLEM000 = "Syntax error in lemma"
let errPPS000 = "Syntax error in proposition"
let errCNJ000 = "Syntax error in conjecture"
let errVAR000 = "Syntax error in variable declaration and/or specification"
let errCTR000 = "Syntax error in constructor"
let errPRF000 = "Syntax error in proof"
let errINF000 = "Syntax error in rule of inference"
let errLOC000 = "Syntax error in localization"
let errEXT000 = "Syntax error in extension"
let errUSE000 = "Syntax error in uses clause"
let errPRD000 = "Syntax error in predicate definition"
let errSTMASE = "Syntax error in assertion"
let errSTMASU = "Syntax error in assumption"
let errSTMCAL = "Syntax error in call to a constructor"
let errSTMCAS = "Syntax error in cases statement"
let errSTMDEL = "Syntax error in delegate"
let errSTMFOI = "Syntax error in domain"
let errSTMFOR = "Syntax error in for statement"
let errSTMMAP = "Syntax error in mcases statement"
let errSTMREV = "Syntax error in revocation"
let errSTMRET = "Syntax error in return statement"
let errAGI000 = "Syntax error in proof argument"
let errPRE000 = "Syntax error in premise"
let errCON000 = "Syntax error in conclusion"
let errTYD000 = "Syntax error in type declaration"
// interpreter error messages
let errGEN00 message = sprintf "Unexpected error occurred: %s" message
let errNSP00 fileNamePattern = sprintf "The theory `%s` could not be found" fileNamePattern
let errNSP01 fileName innerErrMsg = sprintf "The theory `%s` was found but could not be loaded: %s" fileName innerErrMsg
let errNSP02 url innerErrMsg = sprintf "The theory `%s` was found but could not be downloaded: %s" url innerErrMsg
let errNSP03 alias = sprintf "Alias `%s` appeared previously in this namespace" alias
let errNSP04 path = sprintf "Circular theory reference detected: `%s`" path
let errNSP05 pathTypes theory chosenSource = sprintf "Multiple sources %A for theory %s detected (%s was chosen)." pathTypes theory chosenSource
    // identifier-related error codes 
let errID001 signature conflict = $"Signature `{signature}` was already declared at {conflict}."  
let errID002 signature incorrectBlockType = $"Cannot find a block to be associated with the proof `{signature}`, found only {incorrectBlockType}."  
let errID003 signature = $"The proof `{signature}` is missing a block to be associated with."  
let errID005 signature incorrectBlockType = $"Cannot find a block to be associated with the corollary `{signature}`, found only {incorrectBlockType}."  
let errID006 signature = $"The corollary `{signature}` is missing a block to be associated with."  
let errID007 nodeType signatureNode baseType signatureBase = $"The {nodeType} `{signatureNode}` cannot inherit from {baseType} `{signatureBase}`."  
let errID008 constructorId classId  = $"Misspelled constructor name `{constructorId}`, expecting `{classId}`."  
let errID009 name = $"Circular base type dependency involving `{name}`." 
let errID010 name = $"The type `{name}` could not be found. Are you missing a uses clause?" 
let errID011 chain errorMsg = $"The inheritance chain `{chain}` causes the following error: {errorMsg}."  
let errID012 prtyName varName varType candidates = 
    if candidates = String.Empty then 
        $"The {varName} `{varType}` does not define the variable or property `{prtyName}`. No candidates found."  
    else
        $"The {varName} `{varType}` does not define the variable or property `{prtyName}`. Candidate(s) tried:{Environment.NewLine}{candidates}."  
let errID013 delegateDiagnostic = sprintf "%s" delegateDiagnostic // just emit the delegate's diagnostic
let errID014 signature conflict = sprintf "Language code `%s` was already declared at %s." signature conflict
let errID015 signature = $"`parent` cannot be referenced from {signature}." 
let errID016 signature = $"`self` cannot be referenced from {signature}." 
let errID017 name (candidates:string) = 
    if candidates.Length > 0 then
        $"The type `{name}` could not be determined. Candidate(s) tried:{Environment.NewLine}{candidates}."  
    else
        $"The type `{name}` not found, no candidates found."  
let errID018 name = sprintf "The extension `%s` could not be matched. Declare an extension with this pattern." name
let errID020 name = $"Missing call of base constructor `{name}`." 
let errID021 name = $"Duplicate call of base constructor `{name}`."
let errID022 name = $"`{name}` is intrinsic, it has no parameterized constructors. This call uses parameters."
let errID023 candidates  = $"Cannot associate a justification with a single block. Found more candidate(s):{Environment.NewLine}{candidates}." 
let errID024 signature conflict = sprintf "Expression `%s` was already localized at %s." signature conflict
let errID025 candidate nodeType  = $"Cannot reference to `{candidate}` inside {nodeType}." 
let errID027 name = $"Illegal recursion in for statement. The entity `{name}` cannot be used as its own domain." 
// logic-related error codes
let errLG001 typeOfPredicate argument typeOfExpression = 
    if argument = typeOfExpression then 
        $"Cannot evaluate `{typeOfPredicate}` because its argument `{argument}` could not be evaluated as a predicate."
    else
        $"Cannot evaluate `{typeOfPredicate}` because its argument `{argument}` typed `{typeOfExpression}` could not be evaluated as a predicate."
let errLG002 nodeTypeName times = $"Possible infinite recursion detected, `{nodeTypeName}` was called for more than {times} times.`."
let errLG003 nodeTypeName nodeName = $"`{nodeTypeName}` evaluates to `false` and cannot be {nodeName}."
let errLG004 nodeType = $"`Statement inside {nodeType} might cause side effects."
let errLG005 name = $"Unnecessary assignment of `{name}` detected (will be implicitly ignored)."
// proof-related error codes
let errPR001 incorrectBlockType justificatinItemName = $"Cannot find a `{justificatinItemName}`, found {incorrectBlockType} instead."
let errPR003 name conflict = $"Argument identifier `{name}` was already declared at {conflict}."  
let errPR004 name conflict  = $"Justification `{name}` was already declared at {conflict}." 
let errPR005 name =  $"Argument identifier `{name}` not declared in this proof."
let errPR006 proofName argumentName =  $"A proof {proofName} was found, but it has no argument with the identifier `{argumentName}`."
let errPR007 nodeTypeName nodeName =  $"{nodeTypeName} is {nodeName} and is missing a proof."
let errPR008 byInfName numbPrem expectedPremise mismatchingCandidates =
    if numbPrem = 1 then 
        $"The subsequent `{LiteralByInf} {byInfName}` step requires a premise of the form `{expectedPremise}`. The provided justification does not match this structure. Candidate(s) tried:{mismatchingCandidates}."
    else
        $"The subsequent `{LiteralByInf} {byInfName}` step requires {numbPrem} premises of the form `{expectedPremise}`.{Environment.NewLine}The provided justification does not match this structure. Candidate(s) tried:{mismatchingCandidates}."

let errPR009 = "Not all arguments of the proof could be verified."
let errPR010 keyword expectedRef = $"Justification `{keyword}` expects a reference to {expectedRef}, not to a proof or corollary."
let errPR011 keyword expectedRef = $"Justification `{keyword}` expects a reference to {expectedRef}, not to an argument in some proof."
let errPR012 = $"Justification `{LiteralByCor}` expects a reference to a corollary."
let errPR013 = $"Add the keyword `{LiteralByCor}` when referencing to corollaries to increase readability."
let errPR014 = "Justification expects a reference to a theorem-like statement without any more specific references."
let errPR015 argId = $"Cannot revoke the argument `{argId}` because it wasn't assumed."
let errPR016 argId = $"Cannot revoke the argument `{argId}` because it wasn't the last assumed one."
let errPR017 = $"Do not use `{LiteralTrivial}` if the argument is not the last one the proof."
let errPR018 = $"A `{LiteralTrivial}` argument missing exactly one justification."
let errPR019 justificationType1 justificationType2 = $"Unsupported mix of justifications in a single argument (`{justificationType1}` with `{justificationType2}`)."
let errPR020 expectedNum actualNum = $"This {PrimJIByInf} requires {expectedNum} proceeding expressions, got {actualNum}."
let errPR021 expectedFormula foundFormula = $"Proof argument mismatches the inferred one: Expected `{expectedFormula}`, found `{foundFormula}`."
// signature-related error codes
let errSIG00 fixType arity = sprintf $"Illegal arity `{arity}` using `{fixType}` notation."
let errSIG01 symbol = $"The symbol `{symbol}` was not declared." 
let errSIG02 symbol precedence conflict = $"The symbol `{symbol}` was declared with the same precedence of `{precedence}` in {conflict}." 
let errSIG03 errMsg = errMsg // Returned type is mismatching the mapping type
let errSIG04 signature candidates = $"No overload matching `{signature}`. Candidate(s) tried:{Environment.NewLine}{candidates}." 
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
let errSIG12 templateName secondUsage firstUsage firstUsagePos = $"The template `{templateName}` was inconsistently used with `{secondUsage}`, expecting `{firstUsage}` as it was used at `{firstUsagePos}`."
let errSIG13 stmtName secondUsage firstUsage firstUsagePos = $"Every branch of the {stmtName} must return a value with a type of the first case at `{firstUsagePos}`, which was `{firstUsage}`. This branch returns `{secondUsage}`."
let errSIG14 = $"This case will never be matched."
// structure-related error codes
let errST001 nodeName = $"The {nodeName} does nothing."
let errST002 nodeName = $"The {nodeName} does nothing."
let errST004 langCode = $"The language `{langCode}` not implemented."
let errST005 domain nodeType = $"An enumerator for the domain `{domain}` being {nodeType} could not be established."
// interpreter syntax-related error codes for error-tolerant parser
let errSY000 infixOp = $"The infix operator `{infixOp}` is missing a second operand."
let errSY001 = $"Replace `∃!0` by `¬∃` quantor."
let errSY002 = $"Expression `∃!1` can be simplified with `∃!`."
let errSY003 = "{ expected."
let errSY004 = "} expected."
let errSY005 = "FPL type expected, must be PascalCase (starts with a capital letter, no spaces, hyphens, or underscores)."
// variable-related error codes
let errVAR00 =  "Declaring multiple arrays at once may cause ambiguities."
let errVAR01 name = $"Variable `{name}` not declared in this scope."
let errVAR02 name = $"Variable `{name}` was already bound in this quantor."
let errVAR03 identifier conflict = $"Variable `{identifier}` was already declared at {conflict}."  
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
let errVAR11 identifier conflict = $"All variables in a {LiteralLocL} have to be different. The `{identifier}` was used at {conflict}."

// type matching-related errors
// -----------------------------------------------------------------
let errTypeMismatchStandard aIsCallByReference aName aType pName pType = 
    if aIsCallByReference then 
        Some $"The expression `{aName}` typed `{aType}` doesn't match the parameter `{pName}` typed `{pType}`"
    else
        Some $"The application `{aName}` typed `{aType}` doesn't match the parameter `{pName}` typed `{pType}`"

let errTypeMismatchMissingArgument pName pType = Some $"Missing argument for the parameter `{pName}` typed `{pType}`"
let errTypeMismatchMissingParameter aName aType = Some $"No matching parameter for the argument `{aName}` typed `{aType}`"
let errTypeMismatchClassValueNotAllowed actualClassType = Some $"A class `{actualClassType}` cannot be passed directly as a value. Use a class constructor `{actualClassType}(...)` instead"
let errTypeMismatchReturnType aIsCallByReference aName aType pType blockName =
    if aIsCallByReference then 
        Some $"The returned expression `{aName}` typed `{aType}` doesn't match the type `{pType}` this {blockName} returns."
    else 
        Some $"The returned application `{aName}` typed `{aType}` doesn't match the type `{pType}` this {blockName} returns."
let errTypeMismatchInheritanceCycle = "cycle detected"
let errTypeMismatchInheritanceCrossing currName crossName = $"cross-inheritance not supported, `{currName}` is base for `{crossName}`."
let errTypeMismatchInheritanceDuplicate duplicate = $"duplicate inheritance from `{duplicate}` detected."
let errTypeMismatchInheritanceFromNonDefinition blockName = $"Expecting a class, a functional term, or a predicate node, got {blockName}"
let errTypeMismatchInheritanceWrongBase aIsCallByReference aName aType pName pType = 
    if aIsCallByReference then 
        Some $"The expression `{aName}` to the class `{aType}` neither matches the parameter `{pName}` typed `{pType}` nor the base classes of this type."
    else
        Some $"The application `{aName}` instantiating the class `{aType}` neither matches the parameter `{pName}` typed `{pType}` nor the base classes of this type."
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
    Some $"Variadic enumeration of `{aName}` typed `{aType}` doesn't match the parameter `{pName}` typed `{pType}`, try `{aName}:{pType}` as argument or use `{pName}:{pTypeId}[{LiteralInd}]` as parameter type"

// expression-matching-related errors
// -----------------------------------------------------------------
let errExprMismatchExistsN aFplId aName pFplId pName = Some $"found mismatching exists `{aFplId}` in `{aName}`, expecting type `{pFplId}` in `{pName}`"


let errExprMismatchQuantorVariableTypes aName pName xName yName index = Some $"found mismatching type `{xName}` at {englishOrdinal index} quantor variable in `{aName}`, expecting type `{yName}` in `{pName}`"

let errExprMismatchQuantorVariableCounts aName pName aVarsCount pVarsCount = Some $"found {aVarsCount} quantor variables in `{aName}`, expected {pVarsCount} in `{pName}`" 

let errExprMismatchOpenFormulas aName aVarsOpenClosedStr aOpenFormulaType pName pVarsOpenClosedStr pOpenFormulaType = Some $"found expression `{aName}` ({aVarsOpenClosedStr} typed `{aOpenFormulaType}`), expected `{pName}` which is {pVarsOpenClosedStr} typed `{pOpenFormulaType}`"

let errExprMismatchExpectedEndOfFormula (aName) = Some $"`found {aName}`, expected end of formula"
let errExprMismatchFoundEndOfFormula pName = Some $"found end of formula, expected `{pName}`"
let errExprMismatchVarMatchedDifferently varName expectedExpr actualExpr = Some $"variable `{varName}` matched with different formulas `{expectedExpr}` and `{actualExpr}`"
let errExprMismatchVarMatchedDifferentlyQuantor varName expectedExpr actualExpr = Some $"variable `{varName}` matched with different quantor formulas `{expectedExpr}` and `{actualExpr}`.{Environment.NewLine}Both formulas were different even using placeholders for bound variables."
let errExprMismatchMsgStandard aName pName = Some $"found `{aName}`, expected `{pName}`"
let errExprMismatchVarNumbDifferent numA varsA numP pName =
    let plural = if numA > 1 then "variables" else "variable"
    Some $"found {numA} {plural} ({varsA}), expected {numP} in {pName}"
let (errExprMismatchOK:string option) = None

