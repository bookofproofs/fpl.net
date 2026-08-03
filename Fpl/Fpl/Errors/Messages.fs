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
let errSIG00 fixType arity = sprintf $"Illegal arity {arity} when using {fixType} notation."
let errSIG01 symbol = $"The symbol `{symbol}` was not declared." 
let errSIG02 symbol precedence conflict = $"The symbol `{symbol}` was declared with the same precedence `{precedence}` in {conflict}." 
let errSIG03 errMsg = errMsg // Returned type is mismatching the mapping type
let errSIG04 signature candidates = $"No overload matching `{signature}`. Candidates considered:{Environment.NewLine}{candidates}." 
let errSIG05 errMsg = $"Cannot execute assignment. {errMsg}"
let errSIG06 name oldFromNode newFromNode typeName = $"Property `{name}` inherited from {typeName} `{oldFromNode}` is overshadowed by the declaration in `{newFromNode}`."
let errSIG07 assigneeName assigneeType = $"`{assigneeName}` is not an assignable expression ({assigneeType})."
let errSIG08 arrName indexVarName indexVarType dimType dimNumber = $"Type mismatch in the {englishOrdinal dimNumber} dimension of array `{arrName}`; expected `{dimType}`, got `{indexVarName}:{indexVarType}`."
let errSIG09 arrName dimType dimNumber = $"Missing index for the {englishOrdinal dimNumber} dimension of array `{arrName}`; expected type `{dimType}`."
let errSIG10 arrName indexVarName indexNumber = $"Array `{arrName}` has fewer dimensions, the {englishOrdinal indexNumber} index `{indexVarName}` is not supported."
let errSIG11 qualifiedWrongCandidate typeOfCandidate =
    if typeOfCandidate = PrimFunctionalTermL || typeOfCandidate = PrimExtensionL then 
        $"Mapping to {typeOfCandidate} `{qualifiedWrongCandidate}` delegates the mapping to another type. Map directly to that type instead."
    else
        $"Illegal mapping to {typeOfCandidate} `{qualifiedWrongCandidate}`. Only class types or predefined types (`{LiteralObj}`, `{LiteralInd}`, `{LiteralPred}`, `{LiteralFunc}`) may be used as mapping targets."
let errSIG12 templateName secondUsage firstUsage firstUsagePos = $"The template `{templateName}` was used with type `{secondUsage}` inconsistently; expected type `{firstUsage}`, as used in `{firstUsagePos}`."
let errSIG13 stmtName secondUsage firstUsage firstUsagePos = $"Every branch of the {stmtName} must return a value with the same type as the first case in `{firstUsagePos}`, whose type was `{firstUsage}`. This branch returns `{secondUsage}`."
let errSIG14 = $"This case cannot be matched because an earlier case has the same condition signature."
// structure-related error codes
let errST001 nodeName = $"The {nodeName} contains no executable or structural elements."
let errST002 nodeName = $"The {nodeName} contains no executable or structural elements."
let errST004 langCode = $"The localization block does not provide an entry for language `{langCode}`." 
let errST005 domain nodeType = $"An enumerator for the domain `{domain}`, which is {nodeType}, could not be established."
// interpreter syntax-related error codes for error-tolerant parser
let errSY000 errMsg = $"Syntax error: {errMsg}"
let errSY001 errMsg = $"Syntax error (backtracked): {errMsg}"
let errSY002 errMsg chain = $"Syntax error chain {chain}: {errMsg}"

let errSY010 = $"These parentheses can be safely removed."
let errSY011 = $"Replace `∃!0` by `¬∃` quantifier."
let errSY012 = $"Expression `∃!1` can be simplified with `∃!`."
let errSY013 innerInfixSymbol innerPrecedence outerInfixSymbol outerPrecedence = $"These parentheses can be safely removed because the symbol's `{innerInfixSymbol}` precedence {innerPrecedence} is higher than the symbol's `{outerInfixSymbol}` precedence {outerPrecedence}."
let errSY014 infixSymbol1 infixSymbol2 precedence =
    match precedence with
    | -1 -> 
        $"This expression is ambiguous. The infix operators `{infixSymbol1}` and `{infixSymbol2}` are not defined. To resolve the ambiguity, either use parentheses to indicate the intended grouping or define binary operations (either a function or a predicate) for these symbols and assign them a precedence."
    | _ -> 
        $"This expression is ambiguous. The infix operators `{infixSymbol1}` and `{infixSymbol2}` have the same precedence {precedence}. To resolve the ambiguity, either use parentheses to indicate the intended grouping or assign different precedences to the symbols."
// variable-related error codes
let errVAR00 =  "Declaring multiple variadic variables together may cause ambiguous bindings."
let errVAR01 name = $"Variable `{name}` is not declared in the current scope."
let errVAR02 name = $"Variable `{name}` is already bound in this quantifier."
let errVAR03 identifier conflict = $"Variable `{identifier}` is already declared in {conflict}."  
let errVAR04 name = $"Variable `{name}` is declared but not used in the current scope."
let errVAR05 name = $"Variable `{name}` is bound but never referenced in this quantifier."
let errVAR06 name oldFromNode newFromNode typeName = $"Variable `{name}` inherited from {typeName} `{oldFromNode}` is overshadowed by the declaration in `{newFromNode}`."
let errVAR07 name = $"The {PrimQuantifierExistsN} accepts only one bound variable `{name}`."
let errVAR08 = "Variadic variables cannot be bound in a quantifier."
let errVAR09 varName varType = $"The variable {varName}:{varType} is free and cannot be used to evaluate this expression."
let errVAR10 identifier formulaName = $"The variable `{identifier}` is bound more than once in the formula `{formulaName}`."  
let errVAR11 identifier conflict = $"All variables in a {LiteralLocL} have to be different. The `{identifier}` was used in {conflict}."

// type matching-related errors
// -----------------------------------------------------------------
let errTypeMismatchStandard aName aType pName pType = Some $"The expression `{aName}` has type `{aType}`, which does not match the expected type `{pType}` for the parameter `{pName}`."
let errTypeMismatchMissingArgument pName pType = Some $"Missing argument for the parameter `{pName}` of type `{pType}`."
let errTypeMismatchMissingParameter aName aType = Some $"No parameter matches the argument `{aName}` of type `{aType}`."
let errTypeMismatchClassValueNotAllowed actualClassType = Some $"A class `{actualClassType}` cannot be used as a value. Use the class constructor `{actualClassType}(...)` instead."
let errTypeMismatchReturnType aName aType pType blockName = Some $"The return expression `{aName}` has type `{aType}`, which does not match the expected return type `{pType}` of this {blockName}."
let errTypeMismatchInheritanceCycle = "Cycle detected."
let errTypeMismatchInheritanceCrossing currName crossName = $"Cross-inheritance is not supported. `{currName}` is already a base type of `{crossName}`."
let errTypeMismatchInheritanceDuplicate duplicate = $"Duplicate inheritance from `{duplicate}` detected."
let errTypeMismatchInheritanceFromNonDefinition blockName = $"Expected a class, functional term, or predicate node, got {blockName}."
let errTypeMismatchInheritanceWrongBase aName aType pName pType = Some $"The expression `{aName}` of type `{aType}` matches neither the parameter `{pName}` of type `{pType}` nor any base class of this type."
let errTypeMismatchInheritanceUndetermined aName aType pName pType = Some $"The type `{aType}` of the expression `{aName}` could not be determined. The parameter `{pName}` requires type `{pType}` or a type derived from it."
let errTypeMismatchUndefined aName pName pType = Some $"The type of the expression `{aName}` could not be determined. The parameter `{pName}` requires type `{pType}."
let errTypeMismatchVariadic aName aType pName pType pTypeId = Some $"Variadic enumeration of `{aName}` of type `{aType}` does not match the parameter `{pName}` of type `{pType}`. Try `{aName}:{pType}` as an argument, or use `{pName}:{pTypeId}[{LiteralInd}]` as parameter type."

// expression-matching-related errors
// -----------------------------------------------------------------
let errExprMismatchExistsN aFplId aName pFplId pName = Some $"Type mismatch in ∃‑quantifier: `{aFplId}` was provided in `{aName}`, but type `{pFplId}` was required in `{pName}`."
let errExprMismatchQuantifierVariableTypes aName pName xName yName index = Some $"Type mismatch: `{xName}` was used as the {englishOrdinal index} bound variable in `{aName}`, but `{yName}` was required in `{pName}`."
let errExprMismatchQuantifierVariableCounts aName pName aVarsCount pVarsCount = Some $"Found {aVarsCount} bound variables in `{aName}`, expected {pVarsCount} in `{pName}`." 
let errExprMismatchOpenFormulas aName aVarsOpenClosedStr aOpenFormulaType pName pVarsOpenClosedStr pOpenFormulaType = Some $"Found expression `{aName}` ({aVarsOpenClosedStr}, type `{aOpenFormulaType}`), expected `{pName}` ({pVarsOpenClosedStr}, type `{pOpenFormulaType}`)."
let errExprMismatchExpectedEndOfFormula (aName) = Some $"Found {aName}`, expected end of formula."
let errExprMismatchFoundEndOfFormula pName = Some $"Found end of formula, expected `{pName}`."
let errExprMismatchVarMatchedDifferently varName expectedExpr actualExpr = Some $"Variable `{varName}` was matched with different quantifier formulas `{expectedExpr}` and `{actualExpr}`."
let errExprMismatchVarMatchedDifferentlyQuantfier varName expectedExpr actualExpr = Some $"Variable `{varName}` was matched with different quantifier formulas `{expectedExpr}` and `{actualExpr}`.{Environment.NewLine}Both formulas differed even when using placeholders for bound variables."
let errExprMismatchMsgStandard aName pName = Some $"Found `{aName}`, expected `{pName}`."
let errExprMismatchMsgParensOnlyLeft aName pName = Some $"Found `{aName}` in parentheses, expected `{pName}` without parentheses."
let errExprMismatchMsgParensOnlyRight aName pName = Some $"Found `{aName}` without parentheses, expected `{pName}` in parentheses."
let errExprMismatchVarNumbDifferent numA varsA numP pName =
    let plural = if numA > 1 then "variables" else "variable"
    Some $"Found {numA} {plural} ({varsA}), expected {numP} in {pName}."
let (errExprMismatchOK:string option) = None



