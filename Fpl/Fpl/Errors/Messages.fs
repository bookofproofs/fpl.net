(* Copyright (c) 2021+ bookofproofs See LICENSE in the project root for license terms. *)

/// <summary>
/// Centralized string-based error messages and small helper utilities
/// used by the FPL compiler, parser and diagnostics subsystems.
/// </summary>
/// <remarks>
/// This module contains message templates and helper formatting functions
/// so that diagnostics remain consistent across the codebase.
/// </remarks>
module Fpl.Errors.Messages

open System
open Fpl.Primitives

/// <summary>
/// Transforms a whole number into its English ordinal representation
/// (1 -> "1st", 2 -> "2nd", 3 -> "3rd", otherwise "Nth").
/// </summary>
/// <param name="dimNumber">Whole number to convert to ordinal.</param>
/// <returns>Ordinal string for <paramref name="dimNumber"/>.</returns>
let englishOrdinal dimNumber =
    match dimNumber with
    | 1 -> "1st"
    | 2 -> "2nd"
    | 3 -> "3rd"
    | _ -> $"{dimNumber}th"

/// <summary>
/// Returns true if <paramref name="input"/> starts with any of the provided <paramref name="prefixes"/>.
/// </summary>
/// <param name="prefixes">List of prefixes to check.</param>
/// <param name="input">Input string to test.</param>
/// <returns>True when any prefix matches the start of <paramref name="input"/>.</returns>
let startsWithAny (prefixes: string list) (input: string) =
    prefixes |> List.exists input.StartsWith

/// <summary>
/// Prepends an appropriate English article ("a", "an", or "the") to a noun.
/// </summary>
/// <param name="someString">The noun to format.</param>
/// <param name="determined">If true, uses "the" regardless of the noun.</param>
/// <returns>String containing the chosen article and the original noun.</returns>
let getEnglishName someString determined =
    let isEnglishAn = startsWithAny ["a"; "e"; "i"; "o"; "u"; "`a"; "`e"; "`i"; "`o"; "`u"] someString
    if determined then
        $"the {someString}"
    elif isEnglishAn then
        $"an {someString}"
    else
        $"a {someString}"


/// <summary>
/// Produces a numbered list string from a sequence of candidate strings.
/// Each candidate is prefixed with a newline and a numeric index.
/// </summary>
/// <param name="inputLst">Sequence of candidate strings.</param>
/// <returns>A single formatted string enumerating the candidates.</returns>
let numbered inputLst =
    inputLst
    |> Seq.mapi (fun i cand -> $"{Environment.NewLine}  {i + 1}) {cand}")
    |> String.concat ", "

/// <summary>
/// Inserts a lightning caret symbol ("⚡") at the given column position within <paramref name="input"/>.
/// If <paramref name="col"/> is beyond the previous line length, the symbol is appended.
/// </summary>
/// <param name="input">The input line where the marker should be inserted.</param>
/// <param name="col">Column index at which to insert the marker.</param>
/// <param name="prevLen">Previous line length used to decide append vs. insert.</param>
/// <returns>New string with the lightning marker inserted.</returns>
let insertLightningAtCol (input:string) (col:int) (prevLen:int) =
    if col < prevLen then
        // insert ⚡ at caretCol
        input.[0..col-1] + "⚡" + input.[col..]
    else
        // previous line too short → append ⚡
        input + "⚡"

/// <summary>
/// Finds the first index where two strings differ.
/// </summary>
/// <param name="str1">First input string.</param>
/// <param name="str2">Second input string.</param>
/// <returns>
/// Some index of the first mismatch, Some minLength if one is a strict prefix of the other,
/// or None if strings are equal.
/// </returns>
let findMismatchPosition (str1: string) (str2: string) =
    let minLength = min str1.Length str2.Length
    let indexMatch = 
        [0 .. minLength - 1] 
        |> Seq.tryFind (fun i -> str1.[i] <> str2.[i])

    match indexMatch with
    | Some idx -> 
        Some idx // Mismatch found within the overlapping part
    | None -> 
        if str1.Length <> str2.Length then
            Some minLength // Mismatch pos is the start of the extra characters
        else
            None // no mismatch

/// <summary>
/// Capitalizes the first character of a string. Returns the original string when null or empty.
/// </summary>
/// <param name="word">String to capitalize.</param>
/// <returns>Capitalized string when possible.</returns>
let capitalize (word: string) =
    if String.IsNullOrEmpty word then word
    else word.[0].ToString().ToUpper() + word.Substring(1)


// Diagnostics related-errors
// -----------------------------------------------------------------
// interpreter error messages

/// <summary>Generic runtime error message formatter.</summary>
let errGEN00 message = $"An unexpected error occurred: {message}"

/// <summary>Theory not found message.</summary>
let errNSP00 fileNamePattern = $"The theory `{fileNamePattern}` was not found."

/// <summary>Theory found but could not be loaded message.</summary>
let errNSP01 fileName innerErrMsg = $"The theory `{fileName}` was found but could not be loaded: {innerErrMsg}"

/// <summary>Theory found but could not be downloaded message.</summary>
let errNSP02 url innerErrMsg = $"The theory `{url}` was found but could not be downloaded: {innerErrMsg}"

/// <summary>Duplicate alias in namespace.</summary>
let errNSP03 alias = $"Alias `{alias}` was already declared in this namespace."

/// <summary>Circular theory reference detected.</summary>
let errNSP04 path = $"Circular theory reference detected: `{path}`."

/// <summary>Multiple sources for a theory detected.</summary>
let errNSP05 pathTypes theory chosenSource = $"Multiple sources {pathTypes} for theory `{theory}` were detected (`{chosenSource}` was selected)."


// identifier-related error codes

/// <summary>Signature already declared diagnostic.</summary>
let errID001 signature conflict = $"Signature `{signature}` was already declared in {conflict}."

/// <summary>No compatible block found for a proof diagnostic.</summary>
let errID002 signature incorrectBlockType = $"No compatible block was found for proof `{signature}`; a similar name of {incorrectBlockType} was found instead. A theorem-like statement (theorem, lemma, proposition, corollary) is required."

/// <summary>Proof has no associated block diagnostic.</summary>
let errID003 signature = $"Proof `{signature}` has no associated block. Expected a theorem-like statement (theorem, lemma, proposition, corollary)."


/// <summary>No compatible block was found for corollary diagnostic.</summary>
let errID005 signature incorrectBlockType = $"No compatible block was found for corollary `{signature}`; a similar name of {incorrectBlockType} was found instead. Expected a theorem-like statement (theorem, lemma, proposition, corollary), an axiom, or a conjecture."

/// <summary>Corollary has no associated block diagnostic.</summary>
let errID006 signature = $"Corollary `{signature}` has no associated block. Expected a theorem-like statement (theorem, lemma, proposition, corollary), an axiom, or a conjecture."

/// <summary>Inheritance incompatibility diagnostic.</summary>
let errID007 nodeType signatureNode baseType signatureBase = $"{nodeType} `{signatureNode}` cannot inherit from incompatible base `{signatureBase}` ({baseType})."

/// <summary>Misspelled constructor name diagnostic.</summary>
let errID008 constructorId classId = $"Misspelled constructor name `{constructorId}`; expected `{classId}`."

/// <summary>Circular base-type dependency diagnostic.</summary>
let errID009 name = $"Circular base-type dependency detected involving `{name}`."

/// <summary>Type not found; suggests missing uses clause.</summary>
let errID010 name = $"The type `{name}` could not be found. Are you missing a uses clause?"

/// <summary>Invalid inheritance chain diagnostic.</summary>
let errID011 chain errorMsg = $"The inheritance chain `{chain}` is invalid: {errorMsg}"

/// <summary>Property not found candidates message.</summary>
let errID012 prtyName varName varType candidates =
    if candidates = String.Empty then
         $"{capitalize varName} `{varType}` does not define `{prtyName}`. No candidates were found."
    else
         $"{capitalize varName} `{varType}` does not define `{prtyName}`. Candidates considered:{candidates}."

/// <summary>Delegate diagnostic pass-through.</summary>
let errID013 delegateDiagnostic = delegateDiagnostic

/// <summary>Language code already declared diagnostic.</summary>
let errID014 signature conflict = $"Language code `{signature}` was already declared in {conflict}."

/// <summary>Invalid use of `parent` in non-constructor/property contexts.</summary>
let errID015 signature = $"`{LiteralParent}` cannot be referenced in {signature}. Use `{LiteralParent}` only inside {getEnglishName LiteralCtorL false} or {getEnglishName LiteralPrtyL false}."

/// <summary>Invalid use of `self` in incorrect contexts.</summary>
let errID016 signature = $"`{LiteralSelf}` cannot be referenced in {signature}. Use `{LiteralSelf}` only inside {getEnglishName PrimClassL false}, {getEnglishName PrimPredicateL false}, or {getEnglishName PrimFunctionalTermL false}."

/// <summary>Type resolution diagnostic with candidates and compatibility flag.</summary>
let errID017 name (candidates: string) incompatible =
    if candidates = String.Empty then
        $"The type `{name}` was not found. No candidates were found."
    elif incompatible then
        $"The type `{name}` is not a base type of this class. Candidates considered:{candidates}."
    else
        $"The type `{name}` could not be resolved due to ambiguity or incompatibility. Candidates considered:{candidates}."

/// <summary>No declared extension matches diagnostic.</summary>
let errID018 name = $"No declared extension matches `{name}`. Declare an extension with this pattern."

/// <summary>Base constructor not called diagnostic.</summary>
let errID020 name = $"Base constructor `{name}` was not called. Remove the parameters or add an appropriate constructor to class `{name}`."

/// <summary>Base constructor called multiple times diagnostic.</summary>
let errID021 name = $"Base constructor `{name}` was called more than once."

/// <summary>Intrinsic constructor parameter diagnostic.</summary>
let errID022 name = $"`{name}` is intrinsic and has no parameterized constructors. This call supplies parameters."

/// <summary>Ambiguous justification reference diagnostic.</summary>
let errID023 candidates = $"Cannot associate the justification with a single block because the reference is ambiguous. Candidates considered:{candidates}."

/// <summary>Expression already localized diagnostic.</summary>
let errID024 signature conflict = $"Expression `{signature}` was already localized in {conflict}."  

/// <summary>Invalid reference inside node diagnostic.</summary>
let errID025 candidate nodeType = $"Cannot reference {candidate} inside {nodeType}."

/// <summary>Illegal recursion in for statement diagnostic.</summary>
let errID027 name = $"Illegal recursion detected in `for` statement. The entity `{name}` cannot serve as its own iteration domain."


// logic-related error codes

/// <summary>Predicate evaluation diagnostic with explanatory text.</summary>
let errLG001 typeOfPredicate argument typeOfExpression =
    let mainMsg =
        if argument = typeOfExpression then
            $"Cannot evaluate `{typeOfPredicate}` because its argument `{argument}` could not be evaluated as a predicate."
        else
            $"Cannot evaluate `{typeOfPredicate}` because its argument `{argument}` of type `{typeOfExpression}` could not be evaluated as a predicate."
    $"{mainMsg} This issue may be caused by earlier errors."

/// <summary>Possible infinite recursion diagnostic.</summary>
let errLG002 nodeTypeName times = $"Possible infinite recursion detected: `{nodeTypeName}` was called more than {times} times."

/// <summary>Node evaluation returned false diagnostic.</summary>
let errLG003 nodeTypeName nodeName = $"Evaluation of `{nodeTypeName}` returned `{LiteralFalse}`; the node cannot be accepted as {nodeName}."

/// <summary>Statement may introduce side effects diagnostic.</summary>
let errLG004 nodeType = $"A statement in {nodeType} may introduce side effects."

/// <summary>Unnecessary assignment diagnostic.</summary>
let errLG005 name = $"Unnecessary assignment to `{name}` detected; the statement will be ignored."

/// proof-related error codes
/// <summary>Proof justification item type mismatch diagnostic.</summary>
let errPR001 incorrectBlockType justificatinItemName alternative = $"Expected a {justificatinItemName}, but found {incorrectBlockType}. {alternative}"

/// <summary>Argument identifier previously declared diagnostic.</summary>
let errPR003 name conflict = $"Argument identifier `{name}` was previously declared in {conflict}."

/// <summary>Duplicate justification item diagnostic.</summary>
let errPR004 name = $"The justification item `{name}` is a duplicate. It was first declared earlier in the same argument scope."

/// <summary>Argument identifier not declared diagnostic.</summary>
let errPR005 name = $"Argument identifier `{name}` is not declared in this proof."

/// <summary>Proof declares no argument with the given name diagnostic.</summary>
let errPR006 proofName argumentName = $"Proof {proofName} exists, but it declares no argument named `{argumentName}`."

/// <summary>Missing proof provided diagnostic.</summary>
let errPR007 nodeName nodeTypeName = $"{nodeTypeName} `{nodeName}` requires a proof, but none was provided."

/// <summary>By-inference justification candidate mismatch diagnostic.</summary>
let errPR008 byInfName numbPrem expectedPremise mismatchingCandidates =
    if numbPrem = 1 then
        $"The subsequent `{LiteralByInf} {byInfName}` step requires a premise pattern `{expectedPremise}`. The provided justification does not match it. Candidates considered:{mismatchingCandidates}"
    else
        $"The subsequent `{LiteralByInf} {byInfName}` step requires {numbPrem} premise patterns {expectedPremise}.{Environment.NewLine}The provided justification does not match them. Candidates considered:{mismatchingCandidates}"

// Simple proof messages

/// <summary>Not all proof arguments verified message.</summary>
let errPR009 = "Not all proof arguments could be verified."

/// <summary>Justification expects a reference to a specific kind of block diagnostic.</summary>
let errPR010 keyword expectedRef = $"Justification `{keyword}` expects a reference to {expectedRef}, but the provided reference points to a proof or a corollary."

/// <summary>Justification expects a non-proof reference diagnostic.</summary>
let errPR011 keyword expectedRef = $"Justification `{keyword}` expects a reference to {expectedRef}, but the provided reference points to an argument in some proof."

/// <summary>Justification expects a corollary reference diagnostic.</summary>
let errPR012 providedIdentifer = $"Justification `{LiteralByCor}` expects a reference to a corollary, but `{providedIdentifer}` is not one."

/// <summary>Hint to add ByCor for readability.</summary>
let errPR013 = $"Add the keyword `{LiteralByCor}` when referencing corollaries to improve readability."

/// <summary>Justification must reference top-level theorem diagnostic.</summary>
let errPR014 = "The justification must reference the top-level theorem-like statement, but the given reference targets a sub-item."

/// <summary>Cannot revoke non-assumed argument diagnostic.</summary>
let errPR015 argId = $"Cannot revoke argument `{argId}` because it was not assumed in this proof."

/// <summary>Revocation must follow LIFO order diagnostic.</summary>
let errPR016 argId lastAssumedArgumentId = $"Cannot revoke argument `{argId}` because revocation follows LIFO order. The most recent assumption is `{lastAssumedArgumentId}`."

/// <summary>Trivial justification may only be used on final argument diagnostic.</summary>
let errPR017 = $"The `{LiteralTrivial}` justification may only be used on the final argument of a proof."

/// <summary>Mixed justification types in a single argument diagnostic.</summary>
let errPR019 justificationType1 justificationType2 = $"Mixed justification types in a single argument are not supported (`{justificationType1}` with `{justificationType2}`). Split the argument so each one uses only a single justification type."

/// <summary>Justification requires fixed number of premise expressions diagnostic.</summary>
let errPR020 expectedNum actualNum = $"Justification `{PrimJIByInf}` requires {expectedNum} premise expressions, but it received {actualNum}."

/// <summary>Argument inference failed with candidate list diagnostic.</summary>
let errPR021 mismatchingCandidates inferredFormula justificationName = $"The argument{Environment.NewLine}     `{inferredFormula}`{Environment.NewLine}cannot be inferred from the preceding results. {justificationName} found the following candidates:{mismatchingCandidates}."

/// <summary>Argument inference prevented diagnostic.</summary>
let errPR022 reason = $"The argument inference was prevented. Reason: {reason}"

// signature-related error codes

/// <summary>Illegal arity when using fix notation diagnostic.</summary>
let errSIG00 fixType arity expectedArity = sprintf $"Illegal arity {arity} when using {fixType} notation. Expected arity: {expectedArity}."

/// <summary>Undeclared symbol diagnostic.</summary>
let errSIG01 symbol = $"The symbol `{symbol}` was not declared. Declare a functional term, predicate, or class using this symbol."

/// <summary>Symbol declared with conflicting precedence diagnostic.</summary>
let errSIG02 symbol precedence conflict = $"The symbol `{symbol}` was declared with the same precedence `{precedence}` in {conflict}. Consider choosing a different precedence to avoid ambiguity."

/// <summary>Pass-through error message.</summary>
let errSIG03 errMsg = errMsg // Returned type is mismatching the mapping type

/// <summary>No overload matches diagnostic with candidate listing.</summary>
let errSIG04 signature candidates = $"No overload matches `{signature}`. Candidates considered:{Environment.NewLine}{candidates}."

/// <summary>Cannot execute assignment diagnostic wrapper.</summary>
let errSIG05 errMsg = $"Cannot execute assignment. {errMsg}"

/// <summary>Property shadowing diagnostic.</summary>
let errSIG06 name oldFromNode newFromNode typeName = $"Property `{name}` inherited from {typeName} `{oldFromNode}` is shadowed by the declaration in `{newFromNode}`. Consider renaming one of the properties to avoid a name conflict."

/// <summary>Assignee is not assignable diagnostic.</summary>
let errSIG07 assigneeName assigneeType = $"`{assigneeName}` is not an assignable expression ({assigneeType}). Expected a variable or an array."

/// <summary>Array dimension type mismatch diagnostic.</summary>
let errSIG08 arrName indexVarName indexVarType dimType dimNumber = $"Type mismatch in the {englishOrdinal dimNumber} dimension of array `{arrName}`; expected `{dimType}`, got `{indexVarName}:{indexVarType}`."

/// <summary>Missing index for array dimension diagnostic.</summary>
let errSIG09 arrName dimType dimNumber = $"Missing index for the {englishOrdinal dimNumber} dimension of array `{arrName}`; expected type `{dimType}`."

/// <summary>Array has fewer dimensions than requested diagnostic.</summary>
let errSIG10 arrName indexVarName indexNumber = $"Array `{arrName}` has fewer dimensions; the {englishOrdinal indexNumber} index `{indexVarName}` is not supported."

/// <summary>Illegal mapping target diagnostic with special-case handling for delegation.</summary>
let errSIG11 qualifiedWrongCandidate typeOfCandidate =
    if typeOfCandidate = PrimFunctionalTermL || typeOfCandidate = PrimExtensionL then
        $"Mapping to {typeOfCandidate} `{qualifiedWrongCandidate}` delegates the mapping to another type. Map directly to that type instead."
    else
        $"Illegal mapping to {typeOfCandidate} `{qualifiedWrongCandidate}`. Only class types or predefined types (`{LiteralObj}`, `{LiteralInd}`, `{LiteralPred}`, `{LiteralFunc}`) may be used as mapping targets."

/// <summary>Template used inconsistently diagnostic.</summary>
let errSIG12 templateName secondUsage firstUsage firstUsagePos = $"The template `{templateName}` was used inconsistently: type `{secondUsage}` was provided, but type `{firstUsage}` was previously used in `{firstUsagePos}`."

/// <summary>Case branch return type mismatch diagnostic.</summary>
let errSIG13 stmtName secondUsage firstUsage firstUsagePos = $"Every branch of the {stmtName} must return a value with the same type as the first case in `{firstUsagePos}`, whose type was `{firstUsage}`. This branch returns `{secondUsage}`."

/// <summary>Duplicate case condition diagnostic.</summary>
let errSIG14 = $"This case cannot be matched because an earlier case has the same condition signature."

// structure-related error codes

/// <summary>Empty block diagnostic suggesting removal.</summary>
let errST001 nodeName = $"The {nodeName} contains no executable or structural elements. Consider removing the block."

/// <summary>Empty block diagnostic (alternative wording).</summary>
let errST002 nodeName = $"The {nodeName} contains no structural or executable elements. Consider removing the block."

/// <summary>Missing localization entry diagnostic.</summary>
let errST004 langCode = $"The localization block does not provide an entry for language `{langCode}`."

/// <summary>Domain enumerator could not be established diagnostic.</summary>
let errST005 domain nodeType = $"An enumerator for the domain `{domain}`, which is {nodeType}, could not be established."

// interpreter syntax-related error codes for error-tolerant parser

/// <summary>Generic syntax error wrapper.</summary>
let errSY000 errMsg = $"Syntax error: {errMsg}"

/// <summary>Syntax error after backtracking wrapper.</summary>
let errSY001 errMsg = $"Syntax error (after backtracking): {errMsg}"

/// <summary>Syntax error within a chain diagnostic.</summary>
let errSY002 errMsg chain = $"Syntax error (in chain {chain}): {errMsg}"

/// <summary>Parentheses removal hint.</summary>
let errSY010 = $"These parentheses can be removed safely."

/// <summary>Quantifier simplification hint (replace ∃!0 with ¬∃).</summary>
let errSY011 = $"Replace `∃!0` with the quantifier `¬∃`."

/// <summary>Quantifier simplification hint for ∃!1.</summary>
let errSY012 = $"The expression `∃!1` can be simplified using the `∃!` quantifier."

/// <summary>Parentheses removal hint based on operator precedence.</summary>
let errSY013 innerInfixSymbol innerPrecedence outerInfixSymbol outerPrecedence = $"These parentheses can be removed safely because `{innerInfixSymbol}` has higher precedence ({innerPrecedence}) than `{outerInfixSymbol}` ({outerPrecedence})."

/// <summary>Ambiguity diagnostic for infix operators with precedence information.</summary>
let errSY014 infixSymbol1 infixSymbol2 precedence =
    match precedence with
    | -1 ->
        $"This expression is ambiguous. The infix operators `{infixSymbol1}` and `{infixSymbol2}` are not defined. To resolve the ambiguity, either use parentheses to indicate the intended grouping or define binary operations (either a function or a predicate) for these symbols and assign them a precedence."
    | _ ->
        $"This expression is ambiguous. The infix operators `{infixSymbol1}` and `{infixSymbol2}` have the same precedence {precedence}. To resolve the ambiguity, either use parentheses to indicate the intended grouping or assign different precedences to the symbols."

// variable-related error codes

/// <summary>Declaring multiple arrays together warning.</summary>
let errVAR00 = "Declaring multiple arrays together may cause ambiguous bindings."

/// <summary>Variable not declared in scope diagnostic.</summary>
let errVAR01 varName = $"Variable `{varName}` is not declared in the current scope."

/// <summary>Variable already bound by quantifier diagnostic.</summary>
let errVAR02 varName = $"Variable `{varName}` is already bound by this quantifier."

/// <summary>Variable previously declared diagnostic.</summary>
let errVAR03 varName conflict = $"Variable `{varName}` is already declared in {conflict}. Remove this declaration or rename the variable."

/// <summary>Variable declared but not used diagnostic.</summary>
let errVAR04 varName = $"Variable `{varName}` is declared but not used in the current scope."

/// <summary>Variable bound but never referenced diagnostic.</summary>
let errVAR05 varName = $"Variable `{varName}` is bound but never referenced in this quantifier."

/// <summary>Variable shadowing diagnostic caused by inheritance.</summary>
let errVAR06 varName oldFromNode newFromNode typeName = $"Variable `{varName}` inherited from {typeName} `{oldFromNode}` is shadowed by the declaration in `{newFromNode}`. Consider renaming one of the variables to avoid a name conflict."

/// <summary>Exists-quantifier accepts only one bound variable diagnostic.</summary>
let errVAR07 varName = $"The {PrimQuantifierExistsN} accepts only one bound variable; extra variable `{varName}` was supplied."

/// <summary>Arrays cannot be bound in a quantifier diagnostic.</summary>
let errVAR08 varName = $"The arrays `{varName}` cannot be bound in a quantifier."

/// <summary>Variable is free in this context diagnostic.</summary>
let errVAR09 varName = $"The variable `{varName}` is free in this context and cannot be used to evaluate the expression."

/// <summary>Variable bound more than once diagnostic.</summary>
let errVAR10 varName formulaName = $"The variable `{varName}` is bound more than once in the formula `{formulaName}`. Rename one of the bound occurrences of `{varName}` to avoid duplicate bindings."

/// <summary>Variable used more than once before localization diagnostic.</summary>
let errVAR11 varName conflict = $"Variable `{varName}` is used more than once in this localization block before `:=`. First occurrence at {conflict}. Consider renaming one of the variables to avoid a name conflict."

// type matching-related errors
// -----------------------------------------------------------------
/// <summary>Standard type mismatch diagnostic pointing to parameter and argument types.</summary>
let errTypeMismatchStandard aName aType pName pType = Some $"The expression `{aName}` has type `{aType}`, which does not match the expected type `{pType}` for the parameter `{pName}`."

/// <summary>Missing argument for a parameter diagnostic.</summary>
let errTypeMismatchMissingArgument pName pType = Some $"Missing argument for the parameter `{pName}` of type `{pType}`."

/// <summary>No parameter matches the provided argument diagnostic.</summary>
let errTypeMismatchMissingParameter aName aType = Some $"No parameter matches the argument `{aName}` of type `{aType}`."

/// <summary>Class used as value diagnostic.</summary>
let errTypeMismatchClassValueNotAllowed actualClassType = Some $"A class `{actualClassType}` cannot be used as a value. Use the class constructor `{actualClassType}(...)` instead."

/// <summary>Return type mismatch diagnostic.</summary>
let errTypeMismatchReturnType aName aType pType blockName = Some $"The return expression `{aName}` has type `{aType}`, which does not match the expected return type `{pType}` of this {blockName}."

/// <summary>Inheritance cycle detected message.</summary>
let errTypeMismatchInheritanceCycle = "Inheritance cycle detected."

/// <summary>Cross-inheritance not supported diagnostic.</summary>
let errTypeMismatchInheritanceCrossing currName crossName = $"Cross-inheritance is not supported. `{currName}` is already a base type of `{crossName}`."

/// <summary>Duplicate inheritance diagnostic.</summary>
let errTypeMismatchInheritanceDuplicate duplicate = $"Duplicate inheritance from `{duplicate}` detected."

/// <summary>Expected a definition-type node diagnostic.</summary>
let errTypeMismatchInheritanceFromNonDefinition blockName = $"Expected a class, functional term, or predicate node, got {blockName}."

/// <summary>Expression does not match parameter or any base class diagnostic.</summary>
let errTypeMismatchInheritanceWrongBase aName aType pName pType = Some $"The expression `{aName}` of type `{aType}` matches neither the parameter `{pName}` of type `{pType}` nor any base class of this type."

/// <summary>Undetermined type in inheritance diagnostic.</summary>
let errTypeMismatchInheritanceUndetermined aName aType pName pType = Some $"The type `{aType}` of the expression `{aName}` could not be determined. The parameter `{pName}` requires type `{pType}` or a type derived from it."

/// <summary>Undefined type diagnostic.</summary>
let errTypeMismatchUndefined aName pName pType = Some $"The type of the expression `{aName}` could not be determined. The parameter `{pName}` requires type `{pType}`."

/// <summary>Variadic parameter type mismatch diagnostic with suggestions.</summary>
let errTypeMismatchVariadic aName aType pName pType pTypeId = Some $"Variadic enumeration of `{aName}` of type `{aType}` does not match the parameter `{pName}` of type `{pType}`. Try `{aName}:{pType}` as an argument, or use `{pName}:{pTypeId}[{LiteralInd}]` as parameter type."

// expression-matching-related errors
// -----------------------------------------------------------------

/// <summary>Type mismatch in existential quantifier diagnostic.</summary>
let errExprMismatchExistsN aFplId aName pFplId pName = Some $"Type mismatch in the ∃-quantifier: `{aFplId}` was provided in `{aName}`, but type `{pFplId}` was required in `{pName}`."

/// <summary>Bound variable type mismatch diagnostic for quantifiers.</summary>
let errExprMismatchQuantifierVariableTypes aName pName xName yName index = Some $"Type mismatch: `{xName}` was used as the {englishOrdinal index} bound variable in `{aName}`, but `{yName}` was required in `{pName}`."

/// <summary>Bound variable count mismatch diagnostic.</summary>
let errExprMismatchQuantifierVariableCounts aName pName aVarsCount pVarsCount = Some $"Found {aVarsCount} bound variables in `{aName}`, expected {pVarsCount} in `{pName}`."

/// <summary>Open formula mismatch diagnostic with types.</summary>
let errExprMismatchOpenFormulas aName aVarsOpenClosedStr aOpenFormulaType pName pVarsOpenClosedStr pOpenFormulaType = Some $"Found expression `{aName}` ({aVarsOpenClosedStr}, type `{aOpenFormulaType}`), expected `{pName}` ({pVarsOpenClosedStr}, type `{pOpenFormulaType}`)."

/// <summary>Expected end of formula diagnostic.</summary>
let errExprMismatchExpectedEndOfFormula (aName) = Some $"Found `{aName}`, expected end of formula."

/// <summary>Found end of formula but expected other token diagnostic.</summary>
let errExprMismatchFoundEndOfFormula pName = Some $"Found end of formula, expected `{pName}`."

/// <summary>Variable matched with different formulas diagnostic.</summary>
let errExprMismatchVarMatchedDifferently varName expectedExpr actualExpr = Some $"Variable `{varName}` was matched with different formulas `{expectedExpr}` and `{actualExpr}`."

/// <summary>Variable matched with different quantifier formulas diagnostic.</summary>
let errExprMismatchVarMatchedDifferentlyQuantfier varName expectedExpr actualExpr = Some $"Variable `{varName}` was matched with different quantifier formulas `{expectedExpr}` and `{actualExpr}`.{Environment.NewLine}Both formulas differed even when using placeholders for bound variables."

/// <summary>Standard expression mismatch message.</summary>
let errExprMismatchMsgStandard aName pName = Some $"Found `{aName}`, expected `{pName}`."

/// <summary>Expected an instance of a premise diagnostic.</summary>
let errExprMismatchMsgNotAnInstanceOfPremise aName pName = Some $"Found `{aName}`, expected an instance of the premise `{pName}`."

/// <summary>Parentheses-only left mismatch diagnostics.</summary>
let errExprMismatchMsgParensOnlyLeft aName pName = Some $"Found `{aName}` in parentheses, expected `{pName}` without parentheses."

/// <summary>Parentheses-only right mismatch diagnostics.</summary>
let errExprMismatchMsgParensOnlyRight aName pName = Some $"Found `{aName}` without parentheses, expected `{pName}` in parentheses."

/// <summary>Variable number mismatch diagnostic with pluralization.</summary>
let errExprMismatchVarNumbDifferent numA varsA numP pName =
    let plural = if numA > 1 then "variables" else "variable"
    Some $"Found {numA} {plural} ({varsA}), expected {numP} in {pName}."

/// <summary>OK sentinel for expression mismatch diagnostics.</summary>
let (errExprMismatchOK: string option) = None



