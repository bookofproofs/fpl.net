(* Copyright (c) 2021+ bookofproofs See LICENSE in the project root for license terms. *)

/// <summary>
/// Parser combinators that produce the FPL abstract syntax tree (AST).
/// This module exposes the concrete-combinator definitions used by the front-end
/// to parse FPL source code into <c>Fpl.Parser.Types.Ast</c> nodes.
/// </summary>
module Fpl.Parser.Grammar

open FParsec
open Fpl.Parser.Basic
open Fpl.Primitives
open Fpl.Parser.Types
open Fpl.Parser.Debug

// -----------------------------------------------------
(* Identifiers *)
/// <summary>
/// Regex parser matching identifiers starting with a lower-case letter (used for variables).
/// </summary>
let IdStartsWithSmallCase = regex @"[a-z]\w*" 

/// <summary>
/// Regex parser matching identifiers starting with an upper-case letter (PascalCase).
/// </summary>
let idStartsWithCap = (regex @"[A-Z]\w*") <?> "<PascalCaseId>"

/// <summary>
/// Parser that records positions for a PascalCase identifier and constructs <c>Ast.PascalCaseId</c>.
/// </summary>
let pascalCaseId = positions (idStartsWithCap) |>> Ast.PascalCaseId <!> "PascalCaseId"

/// <summary>
/// Parser for a qualified namespace identifier (a sequence of PascalCase names separated by dots).
/// Produces <c>Ast.NamespaceIdentifier</c>.
/// </summary>
let namespaceIdentifier = positions (sepBy1 pascalCaseId dot) .>> IW |>> Ast.NamespaceIdentifier <!> "NamespaceIdentifier"

/// <summary>
/// Parser for a predicate identifier (PascalCase) producing <c>Ast.PredicateIdentifier</c>.
/// </summary>
let predicateIdentifier = positions (idStartsWithCap) |>> Ast.PredicateIdentifier <!> "PredicateIdentifier" 

// -----------------------------------------------------
(* Extensions *)
/// <summary>
/// Regex matching the body of an extension string (characters allowed after the @ literal).
/// </summary>
let extensionString = regex @"[^,;\s()\[\]{}\:]+" <?> "<extensionString>" 

/// <summary>
/// Parser for an extension token: the '@' followed by an <see cref="extensionString"/>.
/// Produces an <c>Ast.Extension</c> node annotated with positions.
/// </summary>
let extension = positions (at >>. extensionString) |>> Ast.Extension <!> "Extension"

/// <summary>
/// Parser for the 'ext' keyword that introduces an extension block.
/// </summary>
let keywordExtension = (skipString LiteralExtL <|> skipString LiteralExt) .>> SW

/// <summary>
/// Parser for the name of an extension producing <c>Ast.ExtensionName</c>.
/// </summary>
let extensionName = positions (idStartsWithCap) .>> SW |>> Ast.ExtensionName <!> "ExtensionName"

// -----------------------------------------------------
(* Variables *)
/// <summary>
/// Low-level parser for a variable identifier string (ensures not a keyword or template).
/// </summary>
let variableX: Parser<string,unit> = 
    IdStartsWithSmallCase 
    <?> "<variable>" 
    |> resultSatisfies (fun s -> keyWordSet.Contains(s) |> not) "<variable> (got <keyword>)>" 
    |> resultSatisfies (fun s -> tplRegex.IsMatch(s) |> not) "<variable> (got <template>)>"
    >>= (fun s -> preturn s) 

/// <summary>
/// Parser that returns a positioned <c>Ast.Var</c> node for a variable identifier.
/// </summary>
let variable = positions variableX |>> Ast.Var <!> "Var" 

/// <summary>
/// Parser for a comma-separated list of variables, consuming trailing whitespace.
/// </summary>
let variableList = (sepBy1 variable comma) .>> IW

// -----------------------------------------------------
(* Main Keywords *)

/// <summary>
/// Parser for the positional token 'self', producing <c>Ast.Self</c>.
/// </summary>
let keywordSelf = positions (skipString LiteralSelf) .>> IW |>> Ast.Self <!> "Self"

/// <summary>
/// Parser for the positional token 'parent', producing <c>Ast.Parent</c>.
/// </summary>
let keywordParent = positions (skipString LiteralParent) .>> IW |>> Ast.Parent <!> "Parent"

/// <summary>
/// Parser for the 'base' class reference keyword (used in constructor calls).
/// </summary>
let keywordBaseClassReference = skipString LiteralBase .>> IW

(* FplBlock-related Keywords *)
/// <summary>
/// Parser that recognizes the premise keyword (used inside rule-of-inference signatures).
/// </summary>
let keywordPremise = (skipString LiteralPreL <|> skipString LiteralPre) >>. IW 
/// <summary>
/// Parser that recognizes the conclusion keyword (used inside rule-of-inference signatures).
/// </summary>
let keywordConclusion = (skipString LiteralConL <|> skipString LiteralCon) >>. IW

(* Statement-related Keywords *)
/// <summary>
/// Parser for the delegate/definition 'del' keyword.
/// </summary>
let keywordDel = skipString LiteralDelL <|> skipString LiteralDel 

/// <summary>
/// Parser for the 'for' keyword used by for-in statements.
/// </summary>
let keywordFor = skipString LiteralFor .>> SW 

/// <summary>
/// Parser for the 'in' keyword used by for-in statements.
/// </summary>
let keywordIn = skipString LiteralIn .>> SW 

/// <summary>
/// Parser for the 'cases' keyword introducing a case statement.
/// </summary>
let keywordCases = skipString LiteralCases .>> IW 

/// <summary>
/// Parser for the 'mapcases' keyword introducing a map-cases construct.
/// </summary>
let keywordMapCases = skipString LiteralMapCases .>> IW 

/// <summary>
/// Parser for the 'assert' keyword introducing an assertion statement.
/// </summary>
let keywordAssert = (skipString LiteralAssert <|> skipString LiteralAss) .>> SW

(* Predicate-related Keywords *)
/// <summary>
/// Parser for the 'undefined' keyword producing <c>Ast.Undefined</c>.
/// </summary>
let keywordUndefined = positions (skipString LiteralUndefL <|> skipString LiteralUndef) |>> Ast.Undefined <!> "Undefined"

/// <summary>
/// Parser for the 'true' literal producing <c>Ast.True</c>.
/// </summary>
let keywordTrue = positions (skipString LiteralTrue) |>> Ast.True <!> "True"  

/// <summary>
/// Parser for the 'false' literal producing <c>Ast.False</c>.
/// </summary>
let keywordFalse = positions (skipString LiteralFalse) |>>  Ast.False  

/// <summary>
/// Parser for the 'bydef' keyword token used in justifications.
/// </summary>
let keywordByDef = pstring LiteralByDef 

/// <summary>
/// Parser for the 'byax' keyword token used in justifications.
/// </summary>
let keywordByAx = pstring LiteralByAx 

/// <summary>
/// Parser for the 'byinf' keyword token used in justifications.
/// </summary>
let keywordByInf = pstring LiteralByInf

/// <summary>
/// Parser for the 'byconj' keyword token used in justifications.
/// </summary>
let keywordByConj = pstring LiteralByConj

/// <summary>
/// Parser for the 'bycor' keyword token used in justifications.
/// </summary>
let keywordByCor = pstring LiteralByCor

/// <summary>
/// Parser that recognizes any of the by-modifiers used before a justification identifier.
/// </summary>
let byModifier = choice [keywordByAx; keywordByConj; keywordByCor; keywordByDef; keywordByInf] .>> SW 

/// <summary>
/// Parser for the conjunction operator token(s).
/// </summary>
let keywordAnd = choice [skipString LiteralAnd; skipString LiteralAndSymbol] .>> IW 

/// <summary>
/// Parser for the disjunction operator token(s).
/// </summary>
let keywordOr = choice  [skipString LiteralOr; skipString LiteralOrSymbol] .>> IW 

/// <summary>
/// Parser for the implication operator token(s).
/// </summary>
let keywordImpl = choice [skipString LiteralImpl; skipString LiteralImplSymbol] .>> IW 

/// <summary>
/// Parser for the bi-implication/equivalence token(s).
/// </summary>
let keywordIif = choice [skipString LiteralIif; skipString LiteralIifSymbol] .>> IW 

/// <summary>
/// Parser for the exclusive-or operator token(s).
/// </summary>
let keywordXor = choice [skipString LiteralXor; skipString LiteralXorSymbol] .>> IW 

/// <summary>
/// Parser for negation (prefix) tokens. Uses attempt/whitespace helpers to bind correctly.
/// </summary>
let keywordNot = choice [skipString LiteralNot .>> attemptSW; skipString LiteralNotSymbol .>> IW]  

/// <summary>
/// Parser for universal-quantifier tokens (all / symbol forms).
/// </summary>
let keywordAll = choice [skipString LiteralAll .>> SW; skipString LiteralAllSymbol .>> IW]  

/// <summary>
/// Parser for existential-quantifier tokens (ex / symbol forms).
/// </summary>
let keywordEx = choice [skipString LiteralEx .>> SW; skipString LiteralExSymbol .>> IW]

/// <summary>
/// Parser for the 'exN' keyword variant used to express 'exists N times'.
/// </summary>
let keywordExN = skipString LiteralExN .>> IW

/// <summary>
/// Parser for symbolic 'exists N' token (non-whitespace consuming).
/// </summary>
let keywordExNSymbolic = skipString LiteralExNSymbol 

/// <summary>
/// Parser for the 'is' keyword used by the is-operator syntax.
/// </summary>
let keywordIs = skipString LiteralIs 

// -----------------------------------------------------
(* In-built types *)
/// <summary>
/// Parser for index/type keyword (ind / index) producing <c>Ast.IndexType</c>.
/// </summary>
let keywordIndex = positions (skipString LiteralIndL <|> skipString LiteralInd) |>> Ast.IndexType <!> "IndexType"

/// <summary>
/// Parser for object type keywords producing <c>Ast.ObjectType</c>.
/// </summary>
let keywordObject = positions (skipString LiteralObjL <|> skipString LiteralObj) |>> Ast.ObjectType <!> "ObjectType" 

/// <summary>
/// Parser for predicate type keywords producing <c>Ast.PredicateType</c>.
/// </summary>
let keywordPredicate = positions (skipString LiteralPredL <|> skipString LiteralPred) |>> Ast.PredicateType <!> "PredicateType"

/// <summary>
/// Parser for function type keywords producing <c>Ast.FunctionalTermType</c>.
/// </summary>
let keywordFunction = positions (skipString LiteralFuncL <|> skipString LiteralFunc) |>> Ast.FunctionalTermType <!> "FunctionalTermType"

// -----------------------------------------------------
(* Templates and Template Types
Via templates, FPL supports generic types, which make it possible to define abstract mathematical
objects and their properties that defer the concrete specification of one or more types
until the definition or method is declared and instantiated client code.
*)

/// <summary>
/// Parser for template keywords (tpl / template) used to denote template markers.
/// </summary>
let keywordTemplate = (pstring LiteralTplL <|> pstring LiteralTpl) 

/// <summary>
/// Tail part of a template name (either PascalCase id or a numeric tail).
/// </summary>
let templateTail = choice [ idStartsWithCap; (regex @"\d+") ]

/// <summary>
/// Parser composing template tokens with possible tails (e.g. Tpl1, TplX).
/// </summary>
let templateWithTail = (many1Strings2 (pstring LiteralTplL <|> pstring LiteralTpl) templateTail) 

/// <summary>
/// Parser for template-type constructs producing <c>Ast.TemplateType</c>.
/// </summary>
let templateType = positions ((attempt templateWithTail) <|> keywordTemplate) |>>  Ast.TemplateType

// -----------------------------------------------------
(* Namespaces *)

/// <summary>
/// Parser for an alias declaration token (e.g. the 'as' alias) producing <c>Ast.Alias</c>.
/// </summary>
let alias = positions (skipString LiteralAlias >>. SW >>. idStartsWithCap) |>> Ast.Alias <!> "Alias"

/// <summary>
/// Parser for a star token with positions, producing <c>Ast.Star</c>.
/// </summary>
let star = positions (skipChar '*') |>> Ast.Star <!> "Star"

/// <summary>
/// Parser for an aliased namespace: a namespace optionally followed by an alias or a star.
/// Produces <c>Ast.AliasedNamespaceIdentifier</c>.
/// </summary>
let aliasedNamespaceIdentifier = positions (namespaceIdentifier .>>. opt (alias <|> star)) |>> Ast.AliasedNamespaceIdentifier <!> "AliasedNamespaceIdentifier"

/// <summary>
/// Parser for a namespace reference used by the 'uses' clause.
/// </summary>
let theoryNamespace = aliasedNamespaceIdentifier <|> namespaceIdentifier .>> IW

/// <summary>
/// Parser for the 'uses' keyword.
/// </summary>
let keywordUses = (skipString LiteralUses) .>> SW

/// <summary>
/// Parser for a 'uses' clause producing <c>Ast.UsesClause</c>.
/// </summary>
let usesClause = positions (keywordUses >>. theoryNamespace) |>> Ast.UsesClause <!> "UsesClause"


// -----------------------------------------------------
(* More complex FPL productions *)

/// <summary>
/// Parser for dollar-prefixed numeric tokens (e.g. `$5`) producing <c>Ast.DollarDigits</c>.
/// </summary>
let dollarDigits = positions (regex "\$" >>. puint32 <?> "<dollarDigits>") |>> Ast.DollarDigits <!> "DollarDigits"

/// <summary>
/// Parser that accepts either 'self' or 'parent' and produces <c>Ast.SelfOrParent</c>.
/// </summary>
let selfOrParent = positions (choice [keywordSelf ; keywordParent]) |>> Ast.SelfOrParent <!> "SelfOrParent"

/// <summary>
/// Forward-declared parser pair for statement lists. The second element is the mutable reference used to supply the real parser.
/// </summary>
let statementList, statementListRef = createParserForwardedToRef()

/// <summary>
/// Forward-declared parser pair for primary predicates (as opposed to compound predicates).
/// </summary>
let primePredicate, primePredicateRef = createParserForwardedToRef()

/// <summary>
/// Forward-declared parser pair for general predicate (expressions).
/// </summary>
let predicate, predicateRef = createParserForwardedToRef()

/// <summary>
/// Forward-declared parser pair for predicate lists (comma-separated predicates).
/// </summary>
let predicateList, predicateListRef = createParserForwardedToRef()

/// <summary>
/// Forward-declared parser pair for predicates with dotted "." qualification suffixes.
/// </summary>
let predicateWithQualification, predicateWithQualificationRef = createParserForwardedToRef()

/// <summary>
/// Forward-declared parser pair for parameter tuples used in signatures.
/// </summary>
let paramTuple, paramTupleRef = createParserForwardedToRef()

/// <summary>
/// Parser for object-symbol tokens producing <c>Ast.ObjectSymbolWithPos</c>.
/// </summary>
let objectSymbol = positions ( objectMathSymbols ) |>> Ast.ObjectSymbolWithPos <!> "ObjectSymbol"

/// <summary>
/// General identifier parser used by several productions: recognizes self/parent, variable, predicate id, extension or object symbol.
/// </summary>
let fplIdentifier = choice [ selfOrParent ; variable ; predicateIdentifier; extension; objectSymbol ] 

/// <summary>
/// Parser producing a possibly-empty list of predicates.
/// </summary>
let pExprList : Parser<Ast list,unit> =
    (pipe2
        predicate
        (many (attempt (comma >>. predicate)))
        (fun first rest -> first :: rest))
    <|> preturn [] <!> "pExprList"

/// <summary>
/// Parser for a bracketed list of coordinates: '[' predicate* ']' producing a list of AST nodes.
/// </summary>
let pCoords : Parser<Ast list,unit> =
    leftBracket >>. pExprList .>> rightBracket 

/// <summary>
/// Parser that wraps coordinates with positions and yields <c>Ast.BrackedCoordList</c>.
/// </summary>
let bracketedCoords = positions pCoords |>> Ast.BrackedCoordList <!> "bracketedCoords"

/// <summary>
/// Forward-declared parser pair for named variable declaration lists.
/// </summary>
let namedVariableDeclarationList, namedVariableDeclarationListRef = createParserForwardedToRef()

// The classType is the last type in FPL we can derive FPL classes from.
// It therefore excludes the in-built FPL-types keywordPredicate, keywordFunction, and keywordIndex
// to restrict it to pure objects.
// In contrast to variableType which can also be used for declaring variables 
// in the scope of FPL building blocks

/// <summary>
/// Forward-declared parser pair for mapping forms (arrow types).
/// </summary>
let mapping, mappingRef = createParserForwardedToRef()

/// <summary>
/// Parser for compound predicate types (predicate + optional parameters) producing <c>Ast.CompoundPredicateType</c>.
/// </summary>
let predicateType = positions (keywordPredicate .>>. opt paramTuple) |>> Ast.CompoundPredicateType <!> "CompoundPredicateType"

/// <summary>
/// Parser for compound functional term types producing <c>Ast.CompoundFunctionalTermType</c>.
/// </summary>
let functionalTermType = positions (keywordFunction .>>. opt (paramTuple .>>. (IW >>. mapping))) |>> Ast.CompoundFunctionalTermType <!> "CompoundFunctionalTermType"

/// <summary>
/// Parser for simple variable type forms producing <c>Ast.SimpleVariableType</c>.
/// </summary>
let simpleVariableType = positions (choice [ keywordIndex; keywordObject; predicateIdentifier; templateType; functionalTermType; predicateType ]) |>> Ast.SimpleVariableType <!> "SimpleVariableType"

/// <summary>
/// Parser restricting types allowed as array index types producing <c>Ast.IndexAllowedType</c>.
/// </summary>
let indexAllowedType = positions (choice [ keywordIndex; keywordObject; predicateIdentifier; templateType; keywordPredicate; keywordFunction]) |>> Ast.IndexAllowedType <!> "IndexAllowedType"

/// <summary>
/// Parser for a comma-separated list of index-allowed types.
/// </summary>
let indexAllowedTypeList = (sepBy1 indexAllowedType comma) .>> IW

/// <summary>
/// Parser for an array type (star-annotated) producing <c>Ast.ArrayType</c>.
/// </summary>
let arrayType = positions (star >>. IW >>. simpleVariableType .>>. (IW >>. leftBracket >>. indexAllowedTypeList .>> rightBracket)) |>> Ast.ArrayType <!> "ArrayType"

/// <summary>
/// Parser that recognizes either a simple variable type or an array type.
/// </summary>
let variableType = choice [ simpleVariableType; arrayType ]

/// <summary>
/// Parser for a named variable declaration: (varList : type).
/// Produces <c>Ast.NamedVarDecl</c>.
/// </summary>
let namedVariableDeclaration = positions ((variableList .>> colon) .>>. variableType .>> IW) |>> Ast.NamedVarDecl <!> "NamedVarDecl"

namedVariableDeclarationListRef.Value <- sepBy namedVariableDeclaration comma

paramTupleRef.Value <- (leftParen >>. namedVariableDeclarationList) .>> rightParen |>> Ast.ParamTuple <!> "ParamTuple"

/// <summary>
/// Parser for a simple signature (PascalCase identifier) consumed with trailing whitespace.
/// </summary>
let simpleSignature = pascalCaseId .>> IW 

/// <summary>0
/// Parser for localization strings inside quotes producing <c>Ast.LocalizationString</c>.
/// </summary>
let localizationString = positions (regex "[^\"\n]*") <?> "<language-specific string>" |>> Ast.LocalizationString <!> "LocalizationString"

/// <summary>
/// Parser for the 'symbol' keyword used to declare user-defined object symbols.
/// </summary>
let keywordSymbol = pstring LiteralSymbol .>> IW

/// <summary>
/// Parser for a quoted object-symbol string.
/// </summary>
let objectSymbolString = pchar '"' >>. objectMathSymbols .>> pchar '"'

/// <summary>
/// Parser for quoted infix-symbol text.
/// </summary>
let infixString = pchar '"' >>. infixMathSymbols .>> pchar '"'

/// <summary>
/// Parser for the 'infix' keyword followed by precedence information.
/// </summary>
let keywordInfix = pstring LiteralInfix >>. IW

/// <summary>
/// Parser for quoted postfix-symbol text.
/// </summary>
let postfixString = pchar '"' >>. postfixMathSymbols .>> pchar '"' 

/// <summary>
/// Parser for the 'postfix' keyword.
/// </summary>
let keywordPostfix = pstring LiteralPostFix >>. IW

/// <summary>
/// Parser for quoted prefix-symbol text.
/// </summary>
let prefixString = pchar '"' >>. prefixMathSymbols .>> pchar '"' 

/// <summary>
/// Parser for the 'prefix' keyword.
/// </summary>
let keywordPrefix = pstring LiteralPrefix >>. IW

/// <summary>
/// Parser for user-defined object symbol declarations producing <c>Ast.SymbolDecl</c>.
/// </summary>
let userDefinedObjSym = positions (keywordSymbol >>. objectSymbolString) .>> IW |>> Ast.SymbolDecl <!> "Symbol"

/// <summary>
/// Parser for integer precedence values producing <c>Ast.Precedence</c>.
/// </summary>
let precedence = positions (pint32) .>> IW |>> Ast.Precedence <!> "Precedence"

/// <summary>
/// Parser for user-defined infix declarations producing <c>Ast.InfixDeclWithPrecedence</c>.
/// </summary>
let userDefinedInfix = positions (keywordInfix >>. (infixString .>>. (IW >>. precedence))) .>> IW |>> Ast.InfixDeclWithPrecedence <!> "Infix"

/// <summary>
/// Parser for user-defined postfix declarations producing <c>Ast.PostfixDecl</c>.
/// </summary>
let userDefinedPostfix = positions (keywordPostfix >>. postfixString) .>> IW |>> Ast.PostfixDecl <!> "Postfix"

/// <summary>
/// Parser for user-defined prefix declarations producing <c>Ast.PrefixDecl</c>.
/// </summary>
let userDefinedPrefix = positions (keywordPrefix >>. prefixString) .>> IW |>> Ast.PrefixDecl <!> "Prefix"

/// <summary>
/// Optional parser that accepts a user-defined symbol declaration if present.
/// </summary>
let userDefinedSymbol = opt (attempt (IW >>. choice [userDefinedPrefix; userDefinedInfix; userDefinedPostfix ]))

// -----------------------------------------------------
(* Statements *)
/// <summary>
/// Parser for parenthesized argument lists (used by function/constructor calls).
/// </summary>
let pArgs : Parser<Ast list,unit> =
    leftParen >>. pExprList .>> rightParen <!> "pArgs"

/// <summary>
/// Parser that wraps arguments with positions producing <c>Ast.ArgumentTuple</c>.
/// </summary>
let argumentTuple = positions pArgs |>> Ast.ArgumentTuple <!> "ArgumentTuple" 

/// <summary>
/// Parser for a delegate name token producing <c>Ast.DelegateName</c>.
/// </summary>
let delegateName = positions (idStartsWithCap) .>> IW |>> Ast.DelegateName <!> "DelegateName"

/// <summary>
/// Parser for a delegate expression: 'del . DelegateName (args)' producing <c>Ast.Delegate</c>.
/// </summary>
let fplDelegate = keywordDel >>. (dot >>. delegateName .>>. argumentTuple) .>> IW |>> Ast.Delegate <!> "Delegate"

/// <summary>
/// Parser for the 'return' keyword used inside function bodies.
/// </summary>
let keywordReturn = IW >>. (skipString LiteralRetL <|> skipString LiteralRet) .>> SW 

/// <summary>
/// Parser for a case-else branch producing <c>Ast.CaseElse</c>.
/// </summary>
let caseElse = positions (IW >>. elseCase >>. IW >>. statementList)  |>> Ast.CaseElse <!> "CaseElse"

/// <summary>
/// Parser for a single case branch producing <c>Ast.CaseSingle</c>.
/// </summary>
let caseSingle = positions ((case >>. predicate .>> IW .>> colon) .>>. statementList) .>> IW |>> Ast.CaseSingle <!> "CaseSingle"

/// <summary>
/// Parser for a non-empty list of case-single entries.
/// </summary>
let caseSingleList = many1 caseSingle

/// <summary>
/// Parser for the complete cases statement producing <c>Ast.Cases</c>.
/// </summary>
let casesStatement = positions (((keywordCases >>. leftParen >>. IW >>. caseSingleList .>>. caseElse .>> rightParen))) |>> Ast.Cases <!> "Cases"

/// <summary>
/// Parser for a map-case-else producing <c>Ast.MapCaseElse</c>.
/// </summary>
let mapCaseElse = positions (IW >>. elseCase >>. predicate) |>> Ast.MapCaseElse <!> "MapCaseElse"

/// <summary>
/// Parser for a single map-case entry producing <c>Ast.MapCaseSingle</c>.
/// </summary>
let mapCaseSingle = positions ((case >>. predicate .>> IW .>> colon) .>>. (IW >>. predicate)) .>> IW |>> Ast.MapCaseSingle <!> "MapCaseSingle"

/// <summary>
/// Parser for a non-empty list of map-case entries.
/// </summary>
let mapCaseSingleList = many1 mapCaseSingle

/// <summary>
/// Parser for a complete mapCases construct producing <c>Ast.MapCases</c>.
/// </summary>
let mapCases = positions (((keywordMapCases >>. leftParen >>. IW >>. mapCaseSingleList .>>. mapCaseElse .>> rightParen))) |>> Ast.MapCases <!> "MapCases"

/// <summary>
/// Parser for an assignment statement producing <c>Ast.Assignment</c>.
/// </summary>
let assignmentStatement = positions ((predicateWithQualification .>> IW .>> colonEqual) .>>. predicate) |>> Ast.Assignment <!> "Assignment"

/// <summary>
/// Parser for 'in entity' usage producing <c>Ast.InEntity</c>.
/// </summary>
let inEntity = keywordIn >>. positions (predicateWithQualification) |>> Ast.InEntity <!> "InEntity"

/// <summary>
/// Helper that pairs an entity with its domain for for-in constructs.
/// </summary>
let entityInDomain = (variable .>> IW) .>>. inEntity 

/// <summary>
/// Parser for the body of a for-in statement.
/// </summary>
let forInBody = (entityInDomain .>> IW) .>>. (leftBrace >>. statementList) .>> rightBrace

/// <summary>
/// Parser for a for-in statement producing <c>Ast.ForIn</c>.
/// </summary>
let forStatement = positions (keywordFor >>. forInBody) |>> Ast.ForIn <!> "ForIn"

/// <summary>
/// Parser for assertion statements producing <c>Ast.Assertion</c>.
/// </summary>
/// <remarks>
/// Semantically, an assertion (keyword '<c>assert</c>') has the same effect as an axiom. Syntactically, it is declared as a predicate inside the definition block of an entity (e.g. a class definition).
/// Assertions are not to be confused with assumptions (keyword '<c>assume</c>') that are used only as proof arguments that can be revoked.
/// </remarks>
let assertionStatement = positions (keywordAssert >>. predicate) |>> Ast.Assertion <!> "Assertion"

/// <summary>
/// Parser for a base-class name producing <c>Ast.BaseClassName</c>.
/// </summary>
let baseClassName = positions (idStartsWithCap) .>> IW |>> Ast.BaseClassName <!> "BaseClassName"

/// <summary>
/// Parser for a base-constructor call producing <c>Ast.BaseConstructorCall</c>.
/// </summary>
let baseConstructorCall = positions (keywordBaseClassReference >>. dot >>. baseClassName .>>. argumentTuple .>> IW) |>> Ast.BaseConstructorCall <!> "BaseConstructorCall"

/// <summary>
/// The main single-statement parser combining all statement forms.
/// </summary>
let statement = 
    IW >>. (choice [
        baseConstructorCall
        casesStatement
        mapCases
        assertionStatement
        forStatement
        assignmentStatement
    ]) .>> IW

/// <summary>
/// Assign the statement-list forward reference value (actual definition resides elsewhere).
/// </summary>
statementListRef.Value <- many statement

/// <summary>
/// Parser for optional specification (bracketed coords or argument tuple).
/// </summary>
let optionalSpecification = opt (choice [bracketedCoords; argumentTuple])

/// <summary>
/// Parser that pairs an identifier with an optional specification producing <c>Ast.PredicateWithOptSpecification</c>.
/// </summary>
let predicateWithOptSpecification = positions (fplIdentifier .>>. optionalSpecification) |>> Ast.PredicateWithOptSpecification <!> "PredicateWithOptSpecification"

/// <summary>
/// Parser for a dotted predicate form producing <c>Ast.DottedPredicate</c>.
/// </summary>
let dottedPredicate = positions (dot >>. predicateWithOptSpecification) |>> Ast.DottedPredicate <!> "DottedPredicate"

/// <summary>
/// Parser for a list of dotted predicates producing <c>Ast.QualificationList</c>.
/// </summary>
let qualificationList = positions (many dottedPredicate) |>> Ast.QualificationList <!> "QualificationList"

/// <summary>
/// Parser for a list of dollar-digit tokens.
/// </summary>
let dollarDigitList = many1 dollarDigits

/// <summary>
/// Parser for referencing identifiers with dollar digits producing <c>Ast.ReferencingIdentifier</c>.
/// </summary>
let referencingIdentifier = positions (predicateIdentifier .>>. dollarDigitList) .>> IW |>> Ast.ReferencingIdentifier <!> "ReferencingIdentifier"

/// <summary>
/// Parser to reference proofs or corollaries producing <c>Ast.ReferenceToProofOrCorollary</c>.
/// </summary>
let referenceToProofOrCorollary = positions referencingIdentifier |>> Ast.ReferenceToProofOrCorollary <!> "ReferenceToProofOrCorollary"

/// <summary>
/// Assigns the predicate-with-qualification forward reference.
/// </summary>
predicateWithQualificationRef.Value <- predicateWithOptSpecification .>>. qualificationList |>> Ast.PredicateWithQualification <!> "PredicateWithQualification" 

/// <summary>
/// Forwarded prime predicate choices (true, false, delegate, references, etc.).
/// </summary>
primePredicateRef.Value <- choice [
    keywordTrue
    keywordFalse
    keywordUndefined
    fplDelegate 
    dollarDigits
    attempt referenceToProofOrCorollary
    predicateWithQualification
    objectSymbol
    extension
]

/// <summary>
/// Parser for an argument identifier (simple alphanumeric token) used in proof arguments.
/// </summary>
let argIdX: Parser<string,unit> = 
    regex @"\w+" <?> "<argument ID>" 
    |> resultSatisfies (fun s -> keyWordSet.Contains(s) |> not) "<argument ID> (got <keyword>)" 
    |> resultSatisfies (fun s -> tplRegex.IsMatch(s) |> not) "<argument ID> (got <template>)"
    >>= (fun s -> preturn s) 

/// <summary>
/// Parser for dotted argument identifier tokens ending in '.'.
/// </summary>
let argIdDottedX: Parser<string,unit> = 
    regex @"\w+\." <?> "<argument ID> '.'" 
    |> resultSatisfies (fun s ->
        let s1 = s.Substring(0, s.Length-1)
        keyWordSet.Contains(s1) |> not
        ) "<argument ID> '.' (got <keyword> '.')" 
    |> resultSatisfies (fun s ->
        let s1 = s.Substring(0, s.Length-1)
        tplRegex.IsMatch(s1) |> not
        ) "<argument ID> '.' (got <template> '.')"
    >>= (fun s -> preturn s) 

/// <summary>
/// Parser for argument identifiers followed by ':'.
/// </summary>
let argIdColonX: Parser<string,unit> = 
    regex @"\w+:" <?> "<argument ID> ':'" 
    |> resultSatisfies (fun s ->
        let s1 = s.Substring(0, s.Length-1)
        keyWordSet.Contains(s1) |> not
        ) "<argument ID> ':' (got <keyword> ':')" 
    |> resultSatisfies (fun s ->
        let s1 = s.Substring(0, s.Length-1)
        tplRegex.IsMatch(s1) |> not
        ) "<argument ID> ':' (got <template> ':')"
    >>= (fun s -> preturn s) 

/// <summary>
/// Positioned parser for an argument identifier producing <c>Ast.ArgumentIdentifier</c>.
/// </summary>
let argumentIdentifierDotted = positions (argIdDottedX) |>> Ast.ArgumentIdentifier <!> "ArgumentIdentifier"

/// <summary>
/// Positioned parser for an argument identifier producing <c>Ast.ArgumentIdentifier</c>.
/// </summary>
let argumentIdentifierColon = positions (argIdColonX) |>> Ast.ArgumentIdentifier <!> "ArgumentIdentifier"

/// <summary>
/// Positioned parser for a referenced argument identifier producing <c>Ast.RefArgumentIdentifier</c>.
/// </summary>
let refArgumentIdentifier = positions argIdX |>> Ast.RefArgumentIdentifier <!> "RefArgumentIdentifier"

/// <summary>
/// Parser for the structured justification identifier used by proofs producing <c>Ast.JustificationIdentifier</c>.
/// </summary>
let justificationIdentifier = positions (opt byModifier .>>. predicateIdentifier .>>. opt dollarDigitList .>>. opt (colon >>. refArgumentIdentifier)) |>> Ast.JustificationIdentifier <!> "JustificationIdentifier"

/// <summary>
/// Parser for 'by def' justifications producing <c>Ast.ByDef</c>.
/// </summary>
let byDef = positions (keywordByDef >>. SW >>. variable) |>> Ast.ByDef <!> "ByDef"

/// <summary>
/// General justification item parser used inside proof argument headers.
/// </summary>
let justificationItem = positions (choice [attempt byDef ; justificationIdentifier ; refArgumentIdentifier ]) |>> Ast.JustificationItem <!> "JustificationItem"

/// <summary>
/// Helper that extracts two comma-separated predicates inside parentheses for binary operators.
/// </summary>
let twoPredicatesInParens = (leftParen >>. predicate) .>>. (comma >>. predicate) .>> rightParen 

/// <summary>
/// Helper that chooses the binary operator and applies the pair extractor.
/// </summary>
let chooseBinaryOp p = p >>. twoPredicatesInParens
 
/// <summary>
/// Parser for conjunction form producing <c>Ast.And</c>.
/// </summary>
let conjunction = positions (chooseBinaryOp keywordAnd)  |>> Ast.And <!> "And"

/// <summary>
/// Parser for disjunction form producing <c>Ast.Or</c>.
/// </summary>
let disjunction = positions (chooseBinaryOp keywordOr) |>> Ast.Or <!> "Or"

/// <summary>
/// Parser for exclusive-or producing <c>Ast.Xor</c>.
/// </summary>
let exclusiveOr = positions (chooseBinaryOp keywordXor) |>> Ast.Xor <!> "Xor"

/// <summary>
/// Parser for implication producing <c>Ast.Impl</c>.
/// </summary>
let implication = positions (chooseBinaryOp keywordImpl) |>> Ast.Impl <!> "Impl"

/// <summary>
/// Parser for equivalence producing <c>Ast.Iif</c>.
/// </summary>
let equivalence = positions (chooseBinaryOp keywordIif) |>> Ast.Iif <!> "Iif"

/// <summary>
/// Forward-declared parser pair for prefix expressions.
/// </summary>
/// <remarks>
/// Forward declaration is required because negation must bind to a prefix expression,
/// while pPrefixExpr is defined after pAtom and pPostfixExpr.
/// </remarks>
let pPrefixExpr, pPrefixExprRef = createParserForwardedToRef()

/// <summary>
/// Parser for negation (binds to the prefix-expression form) producing <c>Ast.Not</c>.
/// </summary>
let negation = positions (keywordNot >>. pPrefixExpr) |>> Ast.Not <!> "Not"

/// <summary>
/// Parser for the universal quantifier form producing <c>Ast.All</c>.
/// </summary>
let all = positions ((keywordAll >>. namedVariableDeclarationList) .>>. (leftBrace >>. predicate .>> rightBrace)) |>> Ast.All <!> "All"

/// <summary>
/// Parser for the existential quantifier form producing <c>Ast.Exists</c>.
/// </summary>
let exists = positions ((keywordEx >>. namedVariableDeclarationList) .>>. (leftBrace >>. predicate .>> rightBrace)) |>> Ast.Exists <!> "Exists"

/// <summary>
/// Parser for exists-N symbolic forms and dollar-based counts.
/// </summary>
let existsNTimes = choice [
                        attempt (keywordExNSymbolic .>> SW) |>> Ast.Exists1  
                        keywordExNSymbolic >>. positions puint32 .>> SW |>> Ast.DollarDigits 
                    ] <!> "existsNTimes"

/// <summary>
/// Parser helper for exists-time-n quantifier head.
/// </summary>
let existsTimeNQuantifier = choice [
    (keywordExN >>. dollarDigits .>> SW)
    existsNTimes
]

/// <summary>
/// Parser for 'exists times N' producing <c>Ast.ExistsN</c>.
/// </summary>
let existsTimesN = positions ((existsTimeNQuantifier .>>. namedVariableDeclarationList) .>>. (leftBrace >>. predicate .>> rightBrace)) |>> Ast.ExistsN <!> "ExistsN"

/// <summary>
/// Parser for the 'is' operator forms; returns pair (predicate, type) representation.
/// </summary>
let isOp = choice [
        attempt ((predicateWithQualification .>> SW .>> keywordIs) .>>. (SW >>. variableType))
        (keywordIs >>. attemptSW >>. leftParen >>. predicate) .>>. (comma >>. variableType) .>> rightParen
    ]

/// <summary>
/// Parser that wraps isOp into positioned <c>Ast.IsOperator</c>.
/// </summary>
let isOperator = positions isOp |>> Ast.IsOperator <!> "IsOperator"

/// <summary>
/// Parser for infix symbol tokens producing <c>Ast.InfixSymbolWithPos</c>.
/// </summary>
let infixSymbolWithPos = positions ( infixMathSymbols ) |>> Ast.InfixSymbolWithPos <!> "infixSymbolWithPos"

// ------------------------------------------------------------
/// <summary>
/// Parser for parenthesized expressions producing <c>Ast.Parens</c>.
/// </summary>
let pParens : Parser<Ast,unit> =
    positions (leftParen >>. predicate .>> rightParen) 
    |>> Ast.Parens <!> "<pParens>"

/// <summary>
/// Choice parser for compound predicate forms (boolean connectives and quantifiers).
/// </summary>
let compoundPredicate = choice [
    conjunction
    disjunction
    implication
    equivalence
    exclusiveOr
    negation
    all
    existsTimesN
    exists
    isOperator
]

/// <summary>
/// Parser for postfix symbol tokens producing <c>Ast.PostFixSymbolWithPos</c>.
/// </summary>
let postfixSymbolWithPos = positions ( postfixMathSymbols ) |>> Ast.PostFixSymbolWithPos <!> "PostFixSymbolWithPos" 

/// <summary>
/// Parser for prefix symbol tokens producing <c>Ast.PrefixSymbolWithPos</c>.
/// </summary>
let prefixSymbolWithPos = positions ( prefixMathSymbols ) |>> Ast.PrefixSymbolWithPos <!> "PrefixSymbolWithPos"

/// <summary>
/// Atomic predicate parser combining compound predicates and primary predicate forms.
/// </summary>
let pAtom : Parser<Ast,unit> =
    choice [compoundPredicate; primePredicate; mapCases]
    <|> pParens <!> "pAtom"

/// <summary>
/// Parser for postfix expressions (atom followed by zero or more postfix operators).
/// </summary>
let pPostfixExpr : Parser<Ast,unit> =
    pipe2
        pAtom
        (many (attempt (NW >>. postfixSymbolWithPos)) <?> "<postfix symbol>")
        (fun expr postfixes ->
            List.fold (fun acc op -> Ast.PostfixOp(op, acc)) expr postfixes
        ) <!> "pPostfixExpr"

/// <summary>
/// Assigns the prefix-expression reference value using a fold of prefix operators over postfix expressions.
/// </summary>
pPrefixExprRef.Value <-
    pipe2
        (many (attempt (prefixSymbolWithPos .>> NW)) <?> "<prefix symbol>")
        pPostfixExpr
        (fun prefixes expr ->
            List.foldBack (fun op acc -> Ast.PrefixOp(op, acc)) prefixes expr
        ) <!> "pPrefixExpr"

/// <summary>
/// Parser for infix expressions composed of prefix expressions combined by infix operators.
/// </summary>
let pInfixExpr : Parser<Ast,unit> =
    positions(
    pipe2
        pPrefixExpr
        (many (attempt (SW >>. infixSymbolWithPos .>> SW .>>. pPrefixExpr)))
        (fun first rest ->
            // first = [(A, "+"); ("-", C); ("*", D)]
            // rest = [("+", B); ("-", C); ("*", D)]

            let operands =
                first :: (rest |> List.map snd)
                // [A; B; C; D]

            let ops =
                (rest |> List.map (fun (op, _) -> Some op))
                @ [None]
                // [Some "+"; Some "-"; Some "*"; None]
            
            List.zip operands ops
            // [ (A, Some "+"); (B, Some "-"); (C, Some "*"); (D, None) ]
        ) <!> "pInfixExpr"
    )
    |>> fun (pos,chain) -> Ast.InfixOp(pos, chain)


/// <summary>
/// The top-level expression parser alias used for general predicates.
/// </summary>
let expression = pInfixExpr

/// <summary>
/// Assigns the main predicate reference value to the expression parser.
/// </summary>
predicateRef.Value <- expression

/// <summary>
/// Assigns the predicate-list forward reference value to the pExprList implementation.
/// </summary>
predicateListRef.Value <- pExprList 

// ------------------------------------------------------------
(* FPL building blocks *)
/// <summary>
/// Parser for the 'dec' keyword used by declarations.
/// </summary>
let keywordDeclaration = (skipString LiteralDecL <|> skipString LiteralDec) .>> SW 

/// <summary>
/// Alias for named variable declaration used by the varDecl productions.
/// </summary>
let varDecl = namedVariableDeclaration

/// <summary>
/// Parser for a block that can be either a statement or a variable declaration, consumed with whitespace.
/// </summary>
let varDeclBlock = (attempt statement <|> varDecl) .>> IW 

/// <summary>
/// Parser for an optional series of variable declaration blocks used by definitions, producing <c>Ast.VarDeclBlock</c>.
/// </summary>
let varDeclOrSpecList = IW >>. opt (keywordDeclaration >>. many1 varDeclBlock .>> semiColon) .>> IW |>> Ast.VarDeclBlock <!> "VarDeclBlock" 

/// <summary>
/// Parser that consumes optional whitespace and then a predicate (used by spacesPredicate).
/// </summary>
let spacesPredicate = IW >>. predicate

/// <summary>
/// Parser for a premise list inside an inference producing <c>Ast.PremiseList</c>.
/// </summary>
let premiseList = positions (IW >>. (keywordPremise >>. colon >>. predicateList)) |>> Ast.PremiseList <!> "PremiseList"

/// <summary>
/// Parser for the conclusion expression inside a rule (consumes the conclusion keyword).
/// </summary>
let conclusion = IW >>. (keywordConclusion >>. colon >>. predicate)

/// <summary>
/// Composite parser for the inside of a rule of inference: variable/spec list, premise list and conclusion.
/// </summary>
let insideRuleOfOnference = (varDeclOrSpecList .>>. (premiseList .>>. conclusion))

/// <summary>
/// Parser that wraps the inside-of-inference block into <c>Ast.PremiseConclusionBlock</c>.
/// </summary>
let premiseConclusionBlock = leftBrace >>. insideRuleOfOnference .>> rightBrace |>> Ast.PremiseConclusionBlock <!> "PremiseConclusionBlock"

(* FPL building blocks - rules of reference *)

/// <summary>
/// Parser for the 'inf' keyword that introduces a rule of inference.
/// </summary>
let keywordInference = (skipString LiteralInfL <|> skipString LiteralInf) .>> SW 

/// <summary>
/// Parser for a rule-of-inference signature producing <c>Ast.RuleOfInferenceSignature</c>.
/// </summary>
let ruleOfInferenceSignature = positions (keywordInference >>. pascalCaseId) .>> IW |>> Ast.RuleOfInferenceSignature <!> "RuleOfInferenceSignature"

/// <summary>
/// Parser for a full rule-of-inference producing <c>Ast.RuleOfInference</c>.
/// </summary>
let ruleOfInference = positions (ruleOfInferenceSignature .>>. premiseConclusionBlock) |>> Ast.RuleOfInference <!> "RuleOfInference"

(* FPL building blocks - Theorem-like statements and conjectures *)

/// <summary>
/// Keyword parser for theorem-like block: here theorem
/// </summary>
let keywordTheorem = (skipString LiteralThmL <|> skipString LiteralThm) .>> SW

/// <summary>
/// Keyword parser for theorem-like block: here lemma
/// </summary>
let keywordLemma = (skipString LiteralLemL <|> skipString LiteralLem) .>> SW

/// <summary>
/// Keyword parser for theorem-like block: here proposition
/// </summary>
let keywordProposition = (skipString LiteralPropL <|> skipString LiteralProp) .>> SW

/// <summary>
/// Keyword parser for theorem-like block: here corollary
/// </summary>
let keywordCorollary = (skipString LiteralCorL <|> skipString LiteralCor) .>> SW

/// <summary>
/// Keyword parser for theorem-like block: here conjecture
/// </summary>
let keywordConjecture = (skipString LiteralConjL <|> skipString LiteralConj) .>> SW

/// <summary>
/// Parser for the generic theorem-like block content used by theorem/lemma/proposition forms.
/// </summary>
let theoremLikeBlock = leftBrace >>. (varDeclOrSpecList .>>. spacesPredicate) .>> rightBrace

/// <summary>
/// The signature for theorems producing <c>Ast.TheoremSignature</c>.
/// </summary>
let theoremSignature = positions (keywordTheorem >>. pascalCaseId) .>> IW |>> Ast.TheoremSignature <!> "TheoremSignature"

/// <summary>
/// The full parser for theorems producing <c>Ast.Theorem</c>.
/// </summary>
let theorem = positions (theoremSignature .>>. theoremLikeBlock) |>> Ast.Theorem <!> "Theorem"

/// <summary>
/// The signature for theorems producing <c>Ast.LemmaSignature</c>.
/// </summary>
let lemmaSignature = positions (keywordLemma >>. pascalCaseId) .>> IW |>> Ast.LemmaSignature <!> "LemmaSignature"

/// <summary>
/// The full parser for theorems producing <c>Ast.Lemma</c>.
/// </summary>
let lemma = positions (lemmaSignature .>>. theoremLikeBlock) |>> Ast.Lemma <!> "Lemma"

/// <summary>
/// The signature for theorems producing <c>Ast.PropositionSignature</c>.
/// </summary>
let propositionSignature = positions (keywordProposition >>. pascalCaseId) .>> IW |>> Ast.PropositionSignature <!> "PropositionSignature"

/// <summary>
/// The full parser for theorems producing <c>Ast.Proposition</c>.
/// </summary>
let proposition = positions (propositionSignature .>>. theoremLikeBlock) |>> Ast.Proposition <!> "Proposition"

/// <summary>
/// The signature for theorems producing <c>Ast.ConjectureSignature</c>.
/// </summary>
let conjectureSignature = positions (keywordConjecture >>. pascalCaseId) .>> IW |>> Ast.ConjectureSignature <!> "ConjectureSignature"

/// <summary>
/// The full parser for theorems producing <c>Ast.Conjecture</c>.
/// </summary>
let conjecture = positions (conjectureSignature .>>. theoremLikeBlock) |>> Ast.Conjecture <!> "Conjecture"

/// <summary>
/// The signature for theorems producing <c>Ast.CorollarySignature</c>.
/// </summary>
let corollarySignature = positions (keywordCorollary >>. simpleSignature .>>. dollarDigitList) .>> IW |>> Ast.CorollarySignature <!> "CorollarySignature"

/// <summary>
/// The full parser for theorems producing <c>Ast.Corollary</c>.
/// </summary>
let corollary = positions (corollarySignature .>>. theoremLikeBlock) |>> Ast.Corollary <!> "Corollary"

(* FPL building blocks - Axioms *)
/// <summary>
/// Keyword parser for axiom.
/// </summary>
let keywordAxiom = (skipString LiteralAxL <|> skipString LiteralAx <|> skipString LiteralPostL <|> skipString LiteralPost) >>. SW

/// <summary>
/// The signature for axioms producing <c>Ast.AxiomSignature</c>.
/// </summary>
let axiomSignature = positions (keywordAxiom >>. pascalCaseId) .>> IW |>> Ast.AxiomSignature <!> "AxiomSignature"

/// <summary>
/// The full parser for axioms <c>Ast.Axiom</c>.
/// </summary>
let axiom = positions (axiomSignature .>>. theoremLikeBlock) |>> Ast.Axiom <!> "Axiom"

(* FPL building blocks - Constructors *)
/// <summary>
/// Parser for the 'intrinsic' keyword producing <c>Ast.Intrinsic</c>.
/// </summary>
let keywordIntrinsic = positions (skipString LiteralIntrL <|> skipString LiteralIntr) |>> Ast.Intrinsic <!> "Intrinsic"

/// <summary>
/// Parser for predicate definition content producing <c>Ast.DefPredicateContent</c>.
/// </summary>
let predContent = varDeclOrSpecList .>>. spacesPredicate |>> Ast.DefPredicateContent <!> "DefPredicateContent"

/// <summary>
/// Parser for the 'constructor' keyword used for class constructors.
/// </summary>
let keywordConstructor = (skipString LiteralCtorL <|> skipString LiteralCtor) .>> SW

/// <summary>
/// Parser for the body of a class constructor.
/// </summary>
let constructorBlock = leftBrace >>. varDeclOrSpecList .>> rightBrace |>> Ast.ConstructorBlock <!> "ConstructorBlock"

/// <summary>
/// The signature for constructors producing <c>Ast.ConstructorSignature</c>.
/// </summary>
let constructorSignature = positions (keywordConstructor >>. simpleSignature .>>. paramTuple) .>> IW |>> Ast.ConstructorSignature <!> "ConstructorSignature"

/// <summary>
/// The full parser for constructors <c>Ast.Constructor</c>.
/// </summary>
let constructor = positions (constructorSignature .>>. constructorBlock) |>> Ast.Constructor <!> "Constructor"

(* FPL building blocks - Properties *)
/// <summary>
/// Parser for the 'property' keyword used to declare predicate instances.
/// </summary>
let keywordProperty = (skipString LiteralPrtyL <|> skipString LiteralPrty) .>> SW 

/// <summary>
/// Parser for a predicate-instance body. 
/// </summary>
let predicateInstanceBlock = opt (leftBrace >>. (keywordIntrinsic <|> predContent) .>> rightBrace)

/// <summary>
/// The signature for predicate-instances producing <c>Ast.PredicateInstanceSignature</c>.
/// </summary>
let predicateInstanceSignature = positions (keywordPredicate >>. SW >>. simpleSignature .>>. paramTuple) .>> IW |>> Ast.PredicateInstanceSignature <!> "PredicateInstanceSignature"

/// <summary>
/// The full parser for predicate-instances producing <c>Ast.PredicateInstance</c>.
/// </summary>
let predicateInstance = positions (keywordProperty >>. predicateInstanceSignature .>>. predicateInstanceBlock) |>> Ast.PredicateInstance <!> "PredicateInstance"

/// <summary>
/// Assigns the mapping forward reference value to the arrow/mapping parser.
/// </summary>
mappingRef.Value <- toArrow >>. IW >>. positions (keywordUndefined <|> variableType) |>> Ast.Mapping <!> "Mapping"

/// <summary>
/// Parser for a return statement inside functional-term and functional-term-instance contents producing <c>Ast.Return</c>.
/// </summary>
let returnStatement = positions (keywordReturn >>. predicate) .>> IW |>> Ast.Return <!> "Return"

/// <summary>
/// Parser for functional-term and functional-term-instances contents (var dec + return) producing <c>Ast.DefFunctionContent</c>.
/// </summary>
let funcContent = varDeclOrSpecList .>>. returnStatement |>> Ast.DefFunctionContent <!> "DefFunctionContent"

/// <summary>
/// Parser for a functional-term-instances body. 
/// </summary>
let functionalTermInstanceBlock = opt (leftBrace >>. (keywordIntrinsic <|> funcContent) .>> rightBrace)

/// <summary>
/// The signature for functional-term-instances producing <c>Ast.FunctionalTermInstanceSignature</c>.
/// </summary>
let functionalTermInstanceSignature = positions (keywordFunction >>. SW >>. simpleSignature .>>. paramTuple .>>. (IW >>. mapping)) .>> IW |>> Ast.FunctionalTermInstanceSignature <!> "FunctionalTermInstanceSignature"

/// <summary>
/// The full parser for functional-term-instances producing <c>Ast.FunctionalTermInstance</c>.
/// </summary>
let functionalTermInstance = positions (keywordProperty >>. functionalTermInstanceSignature .>>. functionalTermInstanceBlock) |>> Ast.FunctionalTermInstance <!> "FunctionalTermInstance"

/// <summary>
/// Parser for extension regex used by extension declarations producing <c>Ast.ExtensionRegex</c>.
/// </summary>
let extensionRegex = regex "[^\/]+" <?> "<extension regex>" |>> Ast.ExtensionRegex <!> "ExtensionRegex"

/// <summary>
/// Parser for an extension assignment 'var @ /regex/' producing <c>Ast.ExtensionAssignment</c>.
/// </summary>
let extensionAssignment = positions ((variable .>> IW .>> at .>> IW) .>>. (slash >>. extensionRegex .>> slash)) |>> Ast.ExtensionAssignment <!> "ExtensionAssignment"

/// <summary>
/// Parser for an extension signature pairing assignment and mapping producing <c>Ast.ExtensionSignature</c>.
/// </summary>
let extensionSignature = positions ((extensionAssignment .>> IW) .>>. mapping) .>> IW |>> Ast.ExtensionSignature <!> "ExtensionSignature"

/// <summary>
/// Parser for the body of an extension term (braced content).
/// </summary>
let extensionTerm = leftBrace >>. ((funcContent <|> mapCases) .>> rightBrace)

/// <summary>
/// Parser for a definition extension producing <c>Ast.DefinitionExtension</c>.
/// </summary>
let definitionExtension = positions (keywordExtension >>. extensionName .>>. extensionSignature .>>. extensionTerm) |>> Ast.DefinitionExtension <!> "DefinitionExtension"

/// <summary>
/// Parser for property definitions: either predicateInstance or functionalTermInstance.
/// </summary>
let definitionProperty = choice [
    attempt predicateInstance
    functionalTermInstance
]

/// <summary>
/// Optional list of properties used inside definitions.
/// </summary>
let propertyList = opt (many1 (safeProceedingSpace definitionProperty)) 

// ------------------------------------------------------------
(* FPL building blocks - Proofs 

    A Proof relates to the PredicateIdentifier of the Theorem.
    Because proofs are named, they can stand anywhere in the theory after their theorem,
    not only immediately after the theorem they prove. This is to enable the users to mix
    with natural language an provide a proof long after the theorem was stated.

*)

/// <summary>
/// Keyword parser for 'revoke' used in proof argument inference.
/// </summary>
let keywordRevoke = (skipString LiteralRevL <|> skipString LiteralRev) .>> SW 

/// <summary>
/// Parser for a revoke argument used inside proofs producing <c>Ast.RevokeArgument</c>.
/// </summary>
let revokeArgument = positions (keywordRevoke >>. refArgumentIdentifier) |>> Ast.RevokeArgument <!> "RevokeArgument" 
    
/// <summary>
/// Parser for 'assume' keyword used in proofs.
/// </summary>
let keywordAssume = skipString LiteralAssL <|> skipString LiteralAss .>> SW 

/// <summary>
/// Parser for 'assume' combined with some expression used in proofs producing <c>Ast.AssumeArgument</c>.
/// </summary>
let assumeArgument = positions (keywordAssume >>. predicate) |>> Ast.AssumeArgument <!> "AssumeArgument"

/// <summary>
/// Parser for the 'trivial' justification token producing <c>Ast.TrivialArgument</c>.
/// </summary>
let keywordTrivial  = positions (skipString LiteralTrivial) .>> IW |>> Ast.TrivialArgument <!> "Trivial"

/// <summary>
/// Parser for the 'qed' token producing <c>Ast.Qed</c>.
/// </summary>
let keywordQed  = positions (skipString LiteralQed) .>> IW |>> Ast.Qed <!> "Qed"

// <summary>
/// Parser for a derived predicate used as a derived argument producing <c>Ast.DeriveArgument</c>.
/// </summary>
let derivedPredicate = positions predicate |>> Ast.DeriveArgument <!> "DeriveArgument"

/// <summary>
/// Choice parser for derived arguments inside proofs (trivial or derived predicate).
/// </summary>
let derivedArgument = choice [
    keywordTrivial 
    derivedPredicate
]

/// <summary>
/// Choice parser for any argument inference form (assume, revoke, derived).
/// </summary>
let argumentInference = (assumeArgument <|> revokeArgument <|> derivedArgument)

/// <summary>
/// Parser for a comma-separated list of justification items used by proof argument headers.
/// </summary>
let justificationItemList = sepBy1 justificationItem comma

/// <summary>
/// Parser for a strictly started proof argument header (dotted id + justifiers + turnstile), returns <c>Ast.StartArgumentStictly</c>.
/// </summary>
let proofArgumentBeginningStrict = (argumentIdentifierDotted .>> IW) .>>. (justificationItemList .>> IW .>> vDash .>> IW) |>> Ast.StartArgumentStictly <!> "StartArgumentStictly"

/// <summary>
/// Parser for a simple proof argument start (colon-ended argument id), producing <c>Ast.StartArgument</c>.
/// </summary>
let proofArgumentBeginningNoJust = (argumentIdentifierColon .>> IW) |>> Ast.StartArgument <!> "StartArgument"

/// <summary>
/// Parser for a justification header and its inferred/attached inference form producing <c>Ast.Justification</c>.
/// </summary>
let justification = positions (choice [proofArgumentBeginningStrict; proofArgumentBeginningNoJust]) |>> Ast.Justification <!> "Justification"

/// <summary>
/// Parser for a single justified argument producing <c>Ast.JustArgInf</c>.
/// </summary>
let justifiedArgument = positions (justification .>>. argumentInference) |>> Ast.JustArgInf <!> "JustArgInf"

/// <summary>
/// Parser for a proof argument (positioned) producing <c>Ast.Argument</c>.
/// </summary>
let proofArgument = positions (justifiedArgument) .>> IW |>> Ast.Argument <!> "Argument"

/// <summary>
/// Parser for a possibly-empty list of proof arguments.
/// </summary>
let proofArgumentList = many1 (IW >>. proofArgument)

/// <summary>
/// Parser for the 'proof' keyword and the proof signature/content combinators.
/// </summary>
let keywordProof = (skipString LiteralPrfL <|> skipString LiteralPrf) .>> SW

/// <summary>
/// Parser for the content of a proof producing <c>Ast.ProofContent</c>.
/// </summary>
let proofContent = varDeclOrSpecList .>>. proofArgumentList .>>. opt keywordQed |>> Ast.ProofContent <!> "ProofContent"

/// <summary>
/// Parser for a proof block (braced) producing <c>Ast.ProofBlock</c>.
/// </summary>
let proofBlock = leftBrace >>. proofContent .>> rightBrace |>> Ast.ProofBlock <!> "ProofBlock"

/// <summary>
/// Parser for a proof signature (name + dollar digits) producing <c>Ast.ProofSignature</c>.
/// </summary>
let proofSignature = positions (keywordProof >>. simpleSignature .>>. dollarDigitList) .>> IW |>> Ast.ProofSignature <!> "ProofSignature"

/// <summary>
/// Parser for a complete proof producing <c>Ast.Proof</c>.
/// </summary>
let proof = positions (proofSignature .>>. proofBlock) |>> Ast.Proof <!> "Proof"

// ------------------------------------------------------------
(* FPL building blocks - Definitions *)

(* Predicate definitions *)
/// <summary>
/// Parser for the body of predicate definitions.
/// </summary>
let predicateDefinitionBlock = opt (leftBrace  >>. (keywordIntrinsic <|> predContent) .>>. propertyList .>> rightBrace)

/// <summary>
/// Parser for inherited type references used in class/predicate signatures producing <c>Ast.InheritedType</c>.
/// </summary>
let inheritedType = positions idStartsWithCap |>> Ast.InheritedType <!> "InheritedType" 

/// <summary>
/// Parser for list of inherited type references producing <c>Ast.InheritedTypeList</c>.
/// </summary>
let inheritedTypeList = sepBy1 inheritedType comma |>> Ast.InheritedTypeList <!> "InheritedTypeList"

/// <summary>
/// Parser for the signature of predicate definitions producing <c>Ast.PredicateSignature</c>.
/// </summary>
let predicateSignature = positions (keywordPredicate >>. SW >>. (simpleSignature .>>. opt (colon >>. inheritedTypeList) .>> IW) .>>. paramTuple) .>>. userDefinedSymbol .>> IW |>> Ast.PredicateSignature <!> "PredicateSignature"

/// <summary>
/// Parser for full predicate definition producing <c>Ast.DefinitionPredicate</c>.
/// </summary>
let definitionPredicate = positions (predicateSignature .>>. predicateDefinitionBlock) |>> Ast.DefinitionPredicate <!> "DefinitionPredicate"

(* Functional-term definitions *)
/// <summary>
/// Parser for the body of functional-term definitions producing <c>Ast.FunctionalTermDefinitionBlock</c>.
/// </summary>
let functionalTermDefinitionBlock = positions (opt (leftBrace  >>. ((keywordIntrinsic <|> funcContent) .>> IW) .>>. propertyList .>> rightBrace))  |>> Ast.FunctionalTermDefinitionBlock <!> "FunctionalTermDefinitionBlock"

/// <summary>
/// Parser for the signature of functional-term definitions producing <c>Ast.FunctionalTermSignature</c>.
/// </summary>
let functionalTermSignature = positions (keywordFunction >>. SW >>. (simpleSignature .>>. opt (colon >>. inheritedTypeList) .>> IW) .>>. paramTuple .>>. (IW >>. mapping)) .>>. userDefinedSymbol .>> IW |>> Ast.FunctionalTermSignature <!> "FunctionalTermSignature"

/// <summary>
/// Parser for full functional-term definition producing <c>Ast.DefinitionFunctionalTerm</c>.
/// </summary>
let definitionFunctionalTerm = positions (functionalTermSignature .>>. functionalTermDefinitionBlock) |>> Ast.DefinitionFunctionalTerm <!> "DefinitionFunctionalTerm"

(* Class definitions *)
/// <summary>
/// Parser for 'class' keyword used in class definitions.
/// </summary>
let keywordClass = (skipString LiteralClL <|> skipString LiteralCl)

/// <summary>
/// Parser for a list of constructors that can be included in the body of class definitions.
/// </summary>
let constructorList = many1 (constructor .>> IW)

/// <summary>
/// Parser for the body content of a class definition producing <c>Ast.ClassDefinitionBlock</c>.
/// </summary>
let classCompleteContent = varDeclOrSpecList .>>. constructorList |>> Ast.DefClassCompleteContent <!> "DefClassCompleteContent"

/// <summary>
/// Parser for the body of a class definition producing <c>Ast.ClassDefinitionBlock</c>.
/// </summary>
let classDefinitionBlock = positions (opt (leftBrace  >>. ((keywordIntrinsic <|> classCompleteContent) .>> IW) .>>. propertyList .>> rightBrace)) |>> Ast.ClassDefinitionBlock <!> "ClassDefinitionBlock"

/// <summary>
/// Parser for the signature of class definition producing <c>Ast.ClassSignature</c>.
/// </summary>
let classSignature = positions (keywordClass >>. SW >>. pascalCaseId) .>> IW |>> Ast.ClassSignature <!> "ClassSignature"

/// <summary>
/// Parser for the extended signature of class definition involving optional inheritance list and optional user-defined symbol for the defined class.
/// </summary>
let classSignatureExtended = classSignature .>>. opt (colon >>. inheritedTypeList) .>>. opt (attempt (IW >>. userDefinedObjSym)) .>> IW

/// <summary>
/// Parser for full class definition producing <c>Ast.DefinitionClass</c>.
/// </summary>
let definitionClass = positions (classSignatureExtended .>>. classDefinitionBlock) |>> Ast.DefinitionClass <!> "DefinitionClass" 

/// <summary>
/// Parser for 'def' keyword that introduces a definition block.
/// </summary>
let keywordDefinition = (skipString LiteralDefL <|> skipString LiteralDef) >>. SW

/// <summary>
/// Parser for for full definition block.
/// </summary>
let definition = keywordDefinition >>. choice [
    definitionClass
    definitionPredicate
    definitionFunctionalTerm
]

// ------------------------------------------------------------
(* Localizations provide a possibility to automatically translate FPL expressions into natural languages *)
/// <summary>
/// Parser for 'loc' keyword used in localizations.
/// </summary>
let keywordLocalization = (skipString LiteralLocL <|> skipString LiteralLoc) >>. SW

/// <summary>
/// Parser for ISO 639-like language codes (omits consistency check for the code, just 3 letters).
/// </summary>
let localizationLanguageCode = positions (regex @"[a-z]{3}" <?> "<ISO 639 language code>") |>> Ast.LanguageCode <!> "LanguageCode"

/// <summary>
/// Forward-declared parser pair for extended Backus-Naur forms of translations. 
/// </summary>
let ebnfTransl, ebnfTranslRef = createParserForwardedToRef()

/// <summary>
/// Parser for parenthesized translation tuples.
/// </summary>
let ebnfTranslTuple = (leftParen >>. IW >>. ebnfTransl) .>> rightParen

/// <summary>
/// Choice parser for translation factors
/// </summary>
let ebnfFactor = choice [
    variable
    quote >>. localizationString .>> quote
    ebnfTranslTuple
] 

/// <summary>
/// Parser for translation factors separated by whitespace <c>Ast.TranslationTerm</c>.
/// </summary>
let ebnfTerm = positions (sepEndBy1 ebnfFactor SW) |>> Ast.TranslationTerm <!> "TranslationTerm"

/// <summary>
/// Actual value of the forward-declared parser for extended Backus-Naur forms of translations. 
/// </summary>
ebnfTranslRef.Value <-  positions (sepBy1 ebnfTerm (IW >>. case >>. IW)) |>> Ast.TranslationTermList <!> "TranslationTermList"

/// <summary>
/// Parser for a language followed by its translation block producing <c>Ast.Language</c>.
/// </summary>
let language = positions ((exclamationMark >>. localizationLanguageCode .>> IW .>> colon) .>>. ebnfTransl) |>> Ast.Language <!> "Language"

/// <summary>
/// Parser for language lists separated by whitespace.
/// </summary>
let languageList = many1 (IW >>. language .>> IW)

/// <summary>
/// Parser for full localization blocks producing <c>Ast.Localization</c>.
/// </summary>
let localization = positions (keywordLocalization >>. predicate) .>> (IW .>> colonEqual) .>>. (languageList .>> (IW .>> semiColon)) .>> IW |>> Ast.Localization <!> "Localization"

/// <summary>
/// Parser that accepts any building block of a theory producing <c>Ast.BuildingBlock</c>.
/// </summary>
let buildingBlock = positions(choice [definition; axiom; theorem; lemma; proposition; corollary; conjecture; proof; ruleOfInference; localization; usesClause; definitionExtension]) .>> IW |>> Ast.BuildingBlock <!> "BuildingBlock"

/// <summary>
/// Parser for an entire FPL namespace (a sequence of building blocks) producing <c>Ast.Namespace</c>.
/// </summary>
let fplNamespace = many buildingBlock |>> Ast.Namespace <!> "Namespace"

/// <summary>
/// Standard top-level parser for entire FPL source files producing the AST wrapper <c>Ast.AST</c>.
/// </summary>
let stdParser = positions (IW >>. fplNamespace) |>> Ast.AST <!> "AST"
