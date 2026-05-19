/// This module contains the FPL parser producing an abstract syntax tree out of a given FPL code 
module FplParser
open System
open System.Text
open System.Text.RegularExpressions
open FplPrimitives
open FplGrammarTypes
open ErrDiagnostics
open FParsec

(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)

/// A helper parser that consume any input and can be combined with existing parsers to enrich them with 
/// the parsing position.
let private _position: Parser<_,_> = fun stream -> Reply stream.Position

/// Takes the parser `p` and returns a tuple with it starting parsing position
let private _startingPosition p = _position .>>. p

/// Takes the parser `p` and returns a tuple with it starting parsing position
let private _endingPosition p = 
    let result = p .>>. _position
    result 
    >>= fun (p, pos) ->
    preturn (pos, p)


/// Takes the parser `p` and returns a tuple of its result, together with its starting and ending position.
let positions (p: Parser<_,_>): Parser<Positions * _,_> =
    pipe2
        (_position .>>. p)
        (_position)
        (
            // correct columns to keep the convention of jumping to
            // the beginning and not to the end of a diagnostics in an IDE
            let offset = (int64)1
            fun (startPos, result) endPos ->
            let pos1 = Position("", startPos.Index, startPos.Line, startPos.Column-offset)
            let pos2 = Position("", endPos.Index, endPos.Line, endPos.Column-offset)
            (Positions(pos1, pos2), result)
        )


(* Literals *)

let leftBrace = skipChar '{' >>. spaces
let leftBraceOpt = positions (opt (skipChar '{')) .>> spaces |>> Ast.LeftBraceOpt
let rightBraceOpt = positions (opt (skipChar '}')) |>> Ast.RightBraceOpt
let leftParen = skipChar '(' >>. spaces 
let leftParenOpt = positions (opt (skipChar '(')) .>> spaces |>> Ast.LeftParenOpt
let rightParenOpt = positions (opt (skipChar ')')) |>> Ast.RightParenOpt
let comma = skipChar ',' >>. spaces 
let dot = skipChar '.' |>> Ast.Dot
let colon = skipChar ':' .>> spaces 
let colonEqual = skipString ":=" >>. spaces 
let at = pchar '@'
let case = skipChar '|' >>. spaces
let elseCase = skipChar '?' >>. spaces
let leftBracket = skipChar '[' >>. spaces 
let leftBracketOpt = positions (opt (skipChar '[')) .>> spaces |>> Ast.LeftBracketOpt
let rightBracketOpt = positions (opt (skipChar ']')) |>> Ast.RightBracketOpt
//let tilde = skipChar '~' .>> spaces
let semiColonOpt = positions (opt (skipChar ';')) .>> spaces |>> Ast.SemicolonOpt
let exclamationMark = skipChar '!' 
let toArrow = skipString "->"
let vDash = skipString "|-"
let quote = skipChar '"' 
let slash = skipChar '/' 

(* Whitespaces and Comments *)

let IW = spaces <?> "<whitespace>"

let SW = spaces1 <?> "<significant whitespace>"

let attemptSW = SW <|> (IW .>> attempt (lookAhead (choice [skipChar '('; skipChar ')'; skipChar '{'; skipChar ','; skipChar ';'; skipChar '[' ])))

// -----------------------------------------------------
// Extensions of the FPL language allow syntax injections as long as they match the following regex expression.
// The FPL interpreter will try to match extensionString after the @ Literal
// by tryoing out the regex expressions of all user-declared ExtensionBlocks (in their declaration order)
// until none or the first of then matches this string. Then, the matched string will get the named type of the ExtensionBlock.
let extensionString = regex @"[^,;\s()\[\]{}\:]+" <?> "<extensionString>" 
let extension = positions (at >>. extensionString) |>> Ast.Extension

(* Identifiers *)


let IdStartsWithSmallCase = regex @"[a-z]\w*" 
let idStartsWithCap = (regex @"[A-Z]\w*") <?> "<PascalCaseId>"
let pascalCaseId = positions (opt idStartsWithCap) |>> Ast.PascalCaseId

let namespaceIdentifier = positions (sepBy1 pascalCaseId dot) .>> IW |>> Ast.NamespaceIdentifier

let predicateIdentifier = positions (idStartsWithCap) |>> Ast.PredicateIdentifier 

let alias = positions (skipString LiteralAlias >>. SW >>. opt idStartsWithCap) |>> Ast.Alias
let star = positions (skipChar '*') |>> Ast.Star

let aliasedNamespaceIdentifier = positions (namespaceIdentifier .>>. opt (alias <|> star)) |>> Ast.AliasedNamespaceIdentifier
let tplRegex = Regex(@"^(tpl|template)(([A-Z]\w*)|\d*)$", RegexOptions.Compiled)


let withBacktrackedError p: Parser<_,_> =
    fun stream ->
        let mutable oldState = stream.State
        match p stream with
        | Success(result, restInput, userState) ->
            Reply(result, restInput)
        | _ ->
            Reply(oldState)

let variableX: Parser<string,unit> = 
    IdStartsWithSmallCase 
    <?> "<variable>" 
    |> resultSatisfies (fun s -> keyWordSet.Contains(s) |> not) "Expecting: <variable (got keyword)>" 
    |> resultSatisfies (fun s -> tplRegex.IsMatch(s) |> not) "Expecting: <variable (got template)>"
    >>= (fun s -> preturn s) 

let variable = positions variableX |>> Ast.Var 

let variableList = (sepBy1 (variable .>> IW) comma) .>> IW

let keywordSelf = positions (skipString LiteralSelf) .>> IW |>> Ast.Self
let keywordParent = positions (skipString LiteralParent) .>> IW |>> Ast.Parent
let keywordBaseClassReference = skipString LiteralBase .>> IW
let keywordIndex = positions (skipString LiteralIndL <|> skipString LiteralInd) |>> Ast.IndexType


(* FplBlock-related Keywords *)
let keywordPremise = (skipString LiteralPreL <|> skipString LiteralPre) >>. IW 
let keywordConclusion = (skipString LiteralConL <|> skipString LiteralCon) >>. IW


(* Statement-related Keywords *)
let keywordDel = skipString LiteralDelL <|> skipString LiteralDel 
let keywordFor = skipString LiteralFor .>> SW 
let keywordIn = skipString LiteralIn .>> SW 
let keywordCases = skipString LiteralCases .>> IW 
let keywordMapCases = skipString LiteralMapCases .>> IW 
let keywordAssert = (skipString LiteralAssert <|> skipString LiteralAss) .>> SW

(* Predicate-related Keywords *)
let keywordUndefined = positions (skipString LiteralUndefL <|> skipString LiteralUndef) .>> IW |>> Ast.Undefined
let keywordTrue = positions (skipString LiteralTrue) .>> IW  |>> Ast.True  
let keywordFalse = positions (skipString LiteralFalse) .>> IW |>>  Ast.False  
let keywordByDef = pstring LiteralByDef 
let keywordByAx = pstring LiteralByAx 
let keywordByInf = pstring LiteralByInf
let keywordByConj = pstring LiteralByConj
let keywordByCor = pstring LiteralByCor
let byModifier = choice [keywordByAx; keywordByConj; keywordByCor; keywordByDef; keywordByInf] .>> SW 
let keywordAnd = choice [skipString LiteralAnd; skipString LiteralAndSymbol] .>> IW 
let keywordOr = choice  [skipString LiteralOr; skipString LiteralOrSymbol] .>> IW 
let keywordImpl = choice [skipString LiteralImpl; skipString LiteralImplSymbol] .>> IW 
let keywordIif = choice [skipString LiteralIif; skipString LiteralIifSymbol] .>> IW 
let keywordXor = choice [skipString LiteralXor; skipString LiteralXorSymbol] .>> IW 
let keywordNot = choice [skipString LiteralNot .>> attemptSW; skipString LiteralNotSymbol .>> IW]  
let keywordAll = choice [skipString LiteralAll .>> SW; skipString LiteralAllSymbol .>> IW]  
let keywordEx = choice [skipString LiteralEx .>> SW; skipString LiteralExSymbol .>> IW]
let keywordExN = skipString LiteralExN .>> IW
let keywordExNSymbolic = skipString LiteralExNSymbol 
let keywordIs = skipString LiteralIs .>> attemptSW 


// Via templates, FPL supports generic types, which make it possible to define abstract mathematical
// objects and their properties that defer the concrete
// specification of one or more types until the definition or method is declared and instantiated by
// client code
let keywordTemplate = (pstring LiteralTplL <|> pstring LiteralTpl) 

let templateTail = choice [ idStartsWithCap; (regex @"\d+") ]

let templateWithTail = (many1Strings2 (pstring "template" <|> pstring LiteralTpl) templateTail) 

let keywordObject = positions (skipString LiteralObjL <|> skipString LiteralObj) |>> Ast.ObjectType 

let templateType = positions ((attempt templateWithTail) <|> keywordTemplate) |>>  Ast.TemplateType

let keywordPredicate = positions (skipString LiteralPredL <|> skipString LiteralPred) |>> Ast.PredicateType
let keywordFunction = positions (skipString LiteralFuncL <|> skipString LiteralFunc) |>> Ast.FunctionalTermType


let theoryNamespace = aliasedNamespaceIdentifier <|> namespaceIdentifier .>> IW

let keywordUses = (skipString LiteralUses) .>> SW
let usesClause = positions (keywordUses >>. theoryNamespace) |>> Ast.UsesClause

(* Signatures, Variable Declarations, and Types, and Coordinates *)
// convention: All syntax production rules of FPL syntax extensions have to start with LiteralExt, followed by
// a Pascal Case id.
// This ensures that they will not be mixed-up with original FPL ebnf productions
// that are all PascalCase as well as FPL keywords, that are all small case.

let dollarDigits = positions (regex "\$" >>. puint32 <?> "<dollarDigits>") |>> Ast.DollarDigits

let selfOrParent = positions (choice [keywordSelf ; keywordParent]) |>> Ast.SelfOrParent

////// resolving recursive parsers
let statementList, statementListRef = createParserForwardedToRef()
let primePredicate, primePredicateRef = createParserForwardedToRef()
let predicate, predicateRef = createParserForwardedToRef()
let predicateList, predicateListRef = createParserForwardedToRef()
let predicateWithQualification, predicateWithQualificationRef = createParserForwardedToRef()
let paramTuple, paramTupleRef = createParserForwardedToRef()
let paramTupleWithOptLeftParen, paramTupleWithOptLeftParenRef = createParserForwardedToRef()

let coord = choice [ predicateWithQualification; dollarDigits ] .>> IW 

// infix operators like the equality operator 
let objectSymbol = positions ( objectMathSymbols ) |>> Ast.ObjectSymbol

let fplIdentifier = choice [ selfOrParent ; variable ; predicateIdentifier; extension; objectSymbol ] 

let coordList = (sepBy1 coord comma) .>> IW

let bracketedCoords = positions (leftBracket >>. coordList .>>. rightBracketOpt) |>> Ast.BrackedCoordList

let namedVariableDeclarationList, namedVariableDeclarationListRef = createParserForwardedToRef()

let keywordExtension = (skipString LiteralExtL <|> skipString LiteralExt) .>> SW

let extensionName = choice [
    attempt (positions (idStartsWithCap) .>> SW) |>> Ast.ExtensionName
    positions (skipString "") |>> Ast.ExtensionNameErr
    ]

// The classType is the last type in FPL we can derive FPL classes from.
// It therefore excludes the in-built FPL-types keywordPredicate, keywordFunction, and keywordIndex
// to restrict it to pure objects.
// In contrast to variableType which can also be used for declaring variables 
// in the scope of FPL building blocks

let mapping, mappingRef = createParserForwardedToRef()
let predicateType = positions (keywordPredicate .>>. opt paramTuple) |>> Ast.CompoundPredicateType
let functionalTermType = positions (keywordFunction .>>. opt (paramTuple .>>. (IW >>. mapping))) |>> Ast.CompoundFunctionalTermType

let simpleVariableType = positions (choice [ keywordIndex; keywordObject; predicateIdentifier; templateType; functionalTermType; predicateType ]) |>> Ast.SimpleVariableType
// indexAllowedType is used to restrict Fpl types allowed to be used as indexes in arrayType
let indexAllowedType = positions (choice [ keywordIndex; keywordObject; predicateIdentifier; templateType; keywordPredicate; keywordFunction]) |>> Ast.IndexAllowedType

let indexAllowedTypeList = (sepBy1 (indexAllowedType .>> IW) comma) .>> IW
// arrayType is used to define arrays in Fpl
let arrayType = positions (star >>. IW >>. simpleVariableType .>>. (IW >>. leftBracketOpt .>>. indexAllowedTypeList .>>. rightBracketOpt)) |>> Ast.ArrayType
let variableType = choice [ simpleVariableType; arrayType ]

let namedVariableDeclaration = positions ((variableList .>> colon) .>>. variableType .>> IW) |>> Ast.NamedVarDecl
namedVariableDeclarationListRef.Value <- sepBy namedVariableDeclaration comma

paramTupleRef.Value <- (leftParen >>. namedVariableDeclarationList) .>>. (IW >>. rightParenOpt) |>> Ast.ParamTuple
paramTupleWithOptLeftParenRef.Value <- (leftParenOpt .>>. namedVariableDeclarationList) .>>. (IW >>. rightParenOpt) |>> Ast.ParamTupleWithOptLeftParen

let simpleSignature = pascalCaseId .>> IW 

let localizationString = positions (regex "[^\"\n]*") <?> "<language-specific string>" |>> Ast.LocalizationString

let keywordSymbol = pstring LiteralSymbol .>> IW
let objectSymbolString = pchar '"' >>. objectMathSymbols .>> pchar '"'
let infixString = pchar '"' >>. infixMathSymbols .>> pchar '"'
let keywordInfix = pstring LiteralInfix >>. IW
let postfixString = pchar '"' >>. postfixMathSymbols .>> pchar '"' 
let keywordPostfix = pstring LiteralPostFix >>. IW
let prefixString = pchar '"' >>. prefixMathSymbols .>> pchar '"' 
let keywordPrefix = pstring LiteralPrefix >>. IW
let userDefinedObjSym = positions (keywordSymbol >>. objectSymbolString) .>> IW |>> Ast.Symbol
let precedence = positions (pint32) .>> IW |>> Ast.Precedence

let userDefinedInfix = positions (keywordInfix >>. (infixString .>>. (IW >>. precedence))) .>> IW |>> Ast.Infix
let userDefinedPostfix = positions (keywordPostfix >>. postfixString) .>> IW |>> Ast.Postfix
let userDefinedPrefix = positions (keywordPrefix >>. prefixString) .>> IW |>> Ast.Prefix
let userDefinedSymbol = opt (attempt (IW >>. choice [userDefinedPrefix; userDefinedInfix; userDefinedPostfix ]))

(* Statements *)
let argumentTupleWithOptLeftParen = positions ((leftParenOpt .>>. predicateList) .>>. (IW >>. rightParenOpt)) |>> Ast.ArgumentTupleWithOptLeftParen
let argumentTuple = positions ((leftParen >>. predicateList) .>>. (IW >>. rightParenOpt)) |>> Ast.ArgumentTuple 

let delegateName = choice [
    attempt (positions (idStartsWithCap) .>> IW) |>> Ast.DelegateName
    positions (IW) |>> Ast.DelegateNameErr
    ]

let dotWithErr = choice [
    attempt dot
    positions (IW) |>> Ast.DotErr
    ]

let fplDelegate = keywordDel >>. (dotWithErr .>>. delegateName .>>. argumentTupleWithOptLeftParen .>> IW) |>> Ast.Delegate

let spacesRightBrace = (IW >>. rightBraceOpt) 

let keywordReturn = IW >>. (skipString LiteralRetL <|> skipString LiteralRet) .>> SW 



let caseElse = positions (elseCase >>. IW >>. statementList .>> IW)  |>> Ast.CaseElse
let caseSingle = positions ((case >>. predicate .>> colon) .>>. statementList) |>> Ast.CaseSingle
let caseSingleList = many1 (IW >>. caseSingle)
let casesStatement = positions (((keywordCases >>. leftParen >>. IW >>. caseSingleList .>>. caseElse .>> rightParenOpt))) |>> Ast.Cases

let mapCaseElse = positions (elseCase >>. predicate .>> IW) |>> Ast.MapCaseElse
let mapCaseSingle = positions ((case >>. predicate .>> colon) .>>. (IW >>. predicate)) |>> Ast.MapCaseSingle
let mapCaseSingleList = many1 (IW >>. mapCaseSingle)
let mapCases = positions (((keywordMapCases >>. leftParen >>. IW >>. mapCaseSingleList .>>. mapCaseElse .>> rightParenOpt))) |>> Ast.MapCases

let assignmentStatement = positions ((predicateWithQualification .>> IW .>> colonEqual) .>>. predicate) |>> Ast.Assignment

let inEntity = keywordIn >>. positions (predicateWithQualification) .>> IW |>> Ast.InEntity

let entityInDomain = ( variable .>> IW .>>. inEntity ) .>> IW
let forInBody = (entityInDomain .>> IW) .>>. (leftBraceOpt .>>. statementList) .>>. (IW >>. rightBraceOpt)
let forStatement = positions (keywordFor >>. forInBody) |>> Ast.ForIn

//// Difference of assertion to an axiom: axiom is named predicate, while an assertion uses a predicated to assert it.
//// Difference of assertion to a mandatory property: a mandatory property introduces a completely new identifier inside
//// the scope of a definition. An assertion uses a predicate referring to existing identifiers in the whole theory
//// Difference of assertion to assume: the latter will be used only in the scope of proofs
let assertionStatement = positions (keywordAssert >>. predicate) |>> Ast.Assertion


let baseClassName = choice [
    attempt (positions (idStartsWithCap) .>> IW) |>> Ast.BaseClassName
    positions (IW) |>> Ast.BaseClassNameErr
    ]

let baseConstructorCall = positions (keywordBaseClassReference >>. dotWithErr .>>. baseClassName .>>. argumentTupleWithOptLeftParen .>> IW) |>> Ast.BaseConstructorCall

let statement = 
    IW >>. (choice [
        baseConstructorCall
        casesStatement
        mapCases
        assertionStatement
        forStatement
        assignmentStatement
    ]) .>> IW

statementListRef.Value <- many statement

(* Predicates *)
let optionalSpecification = opt (choice [bracketedCoords; argumentTuple])
let predicateWithOptSpecification = positions (fplIdentifier .>>. optionalSpecification) |>> Ast.PredicateWithOptSpecification
let dottedPredicate = positions (dot >>. predicateWithOptSpecification) |>> Ast.DottedPredicate
let qualificationList = positions (many dottedPredicate) |>> Ast.QualificationList
let dollarDigitList = many1 dollarDigits
let referencingIdentifier = positions (predicateIdentifier .>>. dollarDigitList) .>> IW |>> Ast.ReferencingIdentifier
let referenceToProofOrCorollary = positions referencingIdentifier |>> Ast.ReferenceToProofOrCorollary

predicateWithQualificationRef.Value <- predicateWithOptSpecification .>>. qualificationList |>> Ast.PredicateWithQualification 


primePredicateRef.Value <- choice [
    keywordTrue
    keywordFalse
    keywordUndefined
    fplDelegate 
    dollarDigits
    attempt referenceToProofOrCorollary
    predicateWithQualification
    objectSymbol
]

let argumentIdentifier = positions (regex @"\w+\.") <?> "<argument identifier>" |>> Ast.ArgumentIdentifier
let refArgumentIdentifier = positions (regex @"\w+") <?> "<refargument identifier>" |>> Ast.RefArgumentIdentifier
let justificationIdentifier = positions (opt byModifier .>>. predicateIdentifier .>>. opt dollarDigitList .>>. opt (colon >>. refArgumentIdentifier)) |>> Ast.JustificationIdentifier
let byDef = positions (keywordByDef >>. SW >>. variable) |>> Ast.ByDef

let justificationReference = choice [
    attempt byDef
    justificationIdentifier
    refArgumentIdentifier
]

let twoPredicatesInParens = (leftParen >>. predicate) .>>. (comma >>. predicate) .>> rightParenOpt 
let twoPredicatesWithInfix p = (dot >>. (predicate .>> p) .>>. predicate)
let chooseBinaryOp p = choice [
        attempt (twoPredicatesWithInfix p)
        p >>. twoPredicatesInParens
    ]

let conjunction = positions (chooseBinaryOp keywordAnd)  |>> Ast.And
let disjunction = positions (chooseBinaryOp keywordOr) |>> Ast.Or
let exclusiveOr = positions (chooseBinaryOp keywordXor) |>> Ast.Xor
let implication = positions (chooseBinaryOp keywordImpl) |>> Ast.Impl
let equivalence = positions (chooseBinaryOp keywordIif) |>> Ast.Iif
let negation = positions (keywordNot >>. predicate) |>> Ast.Not

let all = positions ((keywordAll >>. namedVariableDeclarationList) .>>. (leftBraceOpt .>>. predicate .>>. rightBraceOpt)) |>> Ast.All
let exists = positions ((keywordEx >>. namedVariableDeclarationList) .>>. (leftBraceOpt .>>. predicate .>>. rightBraceOpt)) |>> Ast.Exists

let existsNTimes = choice [
        attempt (keywordExNSymbolic .>> SW) |>> Ast.Exists1 
        keywordExNSymbolic >>. positions puint32 .>> SW |>> Ast.DollarDigits 
    ] 

let existsTimeNQuantifier = choice [
    (keywordExN >>. dollarDigits .>> SW)
    existsNTimes
]

let existsTimesN = positions ((existsTimeNQuantifier .>>. namedVariableDeclarationList) .>>. (leftBraceOpt .>>. predicate .>>. rightBraceOpt)) |>> Ast.ExistsN
let isOp = choice [
    attempt (dot >>. (predicate .>> keywordIs) .>>. variableType) 
    (keywordIs >>. leftParen >>. predicate .>> IW) .>>. (comma >>. variableType) .>> rightParenOpt
    ]
let isOperator = positions isOp |>> Ast.IsOperator

// infix operators like the equality operator 
let infixOp = positions ( infixMathSymbols ) .>> attemptSW |>> Ast.InfixOperator

let pWithSep p separator =
    let combinedParser = pipe2 p (opt separator) (fun a b -> (a, b))
    combinedParser |> many

let infixOperation = positions (leftParen >>. pWithSep predicate infixOp .>> rightParenOpt) |>> Ast.InfixOperation

// A compound Predicate has its own boolean expressions to avoid mixing up with Pl0Propositions
let compoundPredicate = choice [
    infixOperation
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

let postfixOp = positions ( postfixMathSymbols ) .>> IW |>> Ast.PostfixOperator
let prefixOp = positions ( prefixMathSymbols ) .>> IW |>> Ast.PrefixOperator
let expression = positions (opt prefixOp .>>. choice [compoundPredicate; primePredicate; mapCases] .>>. opt postfixOp .>>. optionalSpecification .>>. qualificationList) .>> IW |>> Ast.Expression

predicateRef.Value <- expression

predicateListRef.Value <- sepBy predicate comma



(* FPL building blocks *)
let keywordDeclaration = (skipString LiteralDecL <|> skipString LiteralDec) .>> SW 

let varDecl = namedVariableDeclaration
let varDeclBlock = IW >>. keywordDeclaration >>. (many ((attempt statement <|> varDecl) .>> IW)) .>>. semiColonOpt .>> IW |>> Ast.VarDeclBlock 

let varDeclOrSpecList = opt (many1 (varDeclBlock)) 
let spacesPredicate = IW >>. predicate
let premiseList = positions (IW >>. (keywordPremise >>. colon >>. predicateList)) |>> Ast.PremiseList
let conclusion = IW >>. (keywordConclusion >>. colon >>. predicate)
let insideRuleOfOnference = (varDeclOrSpecList .>>. (premiseList .>>. conclusion))
let premiseConclusionBlock = leftBraceOpt .>>. insideRuleOfOnference .>>. spacesRightBrace |>> Ast.PremiseConclusionBlock

(* FPL building blocks - rules of reference *)
let keywordInference = (skipString LiteralInfL <|> skipString LiteralInf) .>> SW 
let ruleOfInferenceSignature = positions (keywordInference >>. pascalCaseId) .>> IW |>> Ast.RuleOfInferenceSignature
let ruleOfInference = positions (ruleOfInferenceSignature .>>. premiseConclusionBlock) |>> Ast.RuleOfInference

(* FPL building blocks - Theorem-like statements and conjectures *)
let keywordTheorem = (skipString LiteralThmL <|> skipString LiteralThm) .>> SW
let keywordLemma = (skipString LiteralLemL <|> skipString LiteralLem) .>> SW
let keywordProposition = (skipString LiteralPropL <|> skipString LiteralProp) .>> SW
let keywordCorollary = (skipString LiteralCorL <|> skipString LiteralCor) .>> SW
let keywordConjecture = (skipString LiteralConjL <|> skipString LiteralConj) .>> SW

let theoremLikeBlock = leftBraceOpt .>>. (varDeclOrSpecList .>>. spacesPredicate) .>>. spacesRightBrace

let theoremSignature = positions (keywordTheorem >>. pascalCaseId) .>> IW |>> Ast.TheoremSignature
let theorem = positions (theoremSignature .>>. theoremLikeBlock) |>> Ast.Theorem
let lemmaSignature = positions (keywordLemma >>. pascalCaseId) .>> IW |>> Ast.LemmaSignature
let lemma = positions (lemmaSignature .>>. theoremLikeBlock) |>> Ast.Lemma
let propositionSignature = positions (keywordProposition >>. pascalCaseId) .>> IW |>> Ast.PropositionSignature
let proposition = positions (propositionSignature .>>. theoremLikeBlock) |>> Ast.Proposition
let conjectureSignature = positions (keywordConjecture >>. pascalCaseId) .>> IW |>> Ast.ConjectureSignature
let conjecture = positions (conjectureSignature .>>. theoremLikeBlock) |>> Ast.Conjecture
let corollarySignature = positions (keywordCorollary >>. simpleSignature .>>. dollarDigitList) .>> IW |>> Ast.CorollarySignature
let corollary = positions (corollarySignature .>>. theoremLikeBlock) |>> Ast.Corollary

(* FPL building blocks - Axioms *)

let keywordAxiom = (skipString LiteralAxL <|> skipString LiteralAx <|> skipString LiteralPostL <|> skipString LiteralPost) >>. SW

let axiomSignature = positions (keywordAxiom >>. pascalCaseId) .>> IW |>> Ast.AxiomSignature
let axiom = positions (axiomSignature .>>. theoremLikeBlock) |>> Ast.Axiom

(* FPL building blocks - Constructors *)

let keywordIntrinsic = positions (skipString LiteralIntrL <|> skipString LiteralIntr) .>> IW |>> Ast.Intrinsic

let predContent = varDeclOrSpecList .>>. spacesPredicate |>> Ast.DefPredicateContent

let keywordConstructor = (skipString LiteralCtorL <|> skipString LiteralCtor) .>> SW
let constructorBlock = leftBraceOpt .>>. varDeclOrSpecList .>>. spacesRightBrace |>> Ast.ConstructorBlock
let constructorSignature = positions (keywordConstructor >>. simpleSignature .>>. paramTupleWithOptLeftParen) .>> IW |>> Ast.ConstructorSignature
let constructor = positions (constructorSignature .>>. constructorBlock) |>> Ast.Constructor

(* FPL building blocks - Properties *)
let keywordProperty = (skipString LiteralPrtyL <|> skipString LiteralPrty) .>> SW 

let predicateInstanceBlock = opt (leftBrace >>. (keywordIntrinsic <|> predContent) .>> spacesRightBrace)
let predicateInstanceSignature = positions (keywordPredicate >>. SW >>. simpleSignature .>>. paramTupleWithOptLeftParen) .>> IW |>> Ast.PredicateInstanceSignature
let predicateInstance = positions (keywordProperty >>. predicateInstanceSignature .>>. predicateInstanceBlock) |>> Ast.PredicateInstance

mappingRef.Value <- toArrow >>. IW >>. positions (keywordUndefined <|> variableType) |>> Ast.Mapping

let returnStatement = positions (keywordReturn >>. predicate) .>> IW |>> Ast.Return
let funcContent = varDeclOrSpecList .>>. returnStatement |>> Ast.DefFunctionContent
let functionalTermInstanceBlock = opt (leftBrace >>. (keywordIntrinsic <|> funcContent) .>> spacesRightBrace)
let functionalTermInstanceSignature = positions (keywordFunction >>. SW >>. simpleSignature .>>. paramTupleWithOptLeftParen .>>. (IW >>. mapping)) .>> IW |>> Ast.FunctionalTermInstanceSignature
let functionalTermInstance = positions (keywordProperty >>. functionalTermInstanceSignature .>>. functionalTermInstanceBlock) |>> Ast.FunctionalTermInstance


let extensionRegex = regex "[^\/]+" <?> "<extension regex>" |>> Ast.ExtensionRegex

let extensionAssignment = positions ((variable .>> IW .>> at .>> IW) .>>. (slash >>. extensionRegex .>> slash)) |>> Ast.ExtensionAssignment

let extensionSignature = positions ((extensionAssignment .>> IW) .>>. mapping) .>> IW |>> Ast.ExtensionSignature
let extensionTerm = leftBraceOpt .>>. ((funcContent <|> mapCases) .>>. spacesRightBrace)
let definitionExtension = positions (keywordExtension >>. extensionName .>>. extensionSignature .>>. extensionTerm) |>> Ast.DefinitionExtension

let definitionProperty = choice [
    attempt predicateInstance
    functionalTermInstance
]
let propertyList = opt (many1 (definitionProperty .>> IW)) 

(* FPL building blocks - Proofs 

    # A Proof relates to the PredicateIdentifier of the Theorem.
    # Because proofs are named, they can stand anywhere inside the theory, not only immediately
    # after the Theorem they prove. This is to enable the users to mix
    # with natural language an provide a proof long after the theorem was stated.

*)
// justifying proof arguments can be the identifiers of Rules of References, conjectures, theorem-like statements, or axioms
let keywordRevoke = (skipString LiteralRevL <|> skipString LiteralRev) .>> SW 
let revokeArgument = positions (keywordRevoke >>. refArgumentIdentifier) |>> Ast.RevokeArgument 
    
let keywordAssume = skipString LiteralAssL <|> skipString LiteralAss .>> SW 
let assumeArgument = positions (keywordAssume >>. predicate) |>> Ast.AssumeArgument
let keywordTrivial  = positions (skipString LiteralTrivial) .>> IW |>> Ast.Trivial
let keywordQed  = positions (skipString LiteralQed) .>> IW |>> Ast.Qed
let derivedPredicate = positions predicate |>> Ast.DerivedPredicate
let derivedArgument = choice [
    keywordTrivial 
    derivedPredicate
]

let argumentInference = vDash >>. IW >>. (assumeArgument <|> revokeArgument <|> derivedArgument)
let justificationItem = positions justificationReference |>> Ast.JustificationItem
let justificationItemList = sepBy justificationItem comma
let justification = positions (justificationItemList .>> IW) |>> Ast.Justification
let justifiedArgument = positions (justification .>>. argumentInference) |>> Ast.JustArgInf
let proofArgument = positions ((argumentIdentifier .>> IW) .>>. justifiedArgument) .>> IW |>> Ast.Argument
let proofArgumentList = many1 (IW >>. (proofArgument <|> varDeclBlock))
let keywordProof = (skipString LiteralPrfL <|> skipString LiteralPrf) .>> SW
let proofContent = proofArgumentList .>>. opt keywordQed |>> Ast.ProofContent
let proofBlock = (leftBraceOpt .>>. proofContent) .>>. spacesRightBrace |>> Ast.ProofBlock
let proofSignature = positions (keywordProof >>. simpleSignature .>>. dollarDigitList) .>> IW |>> Ast.ProofSignature

let proof = positions (proofSignature .>>. proofBlock) |>> Ast.Proof

(* FPL building blocks - Definitions *)

// Predicate building blocks can be defined similarly to classes, they can have properties but they cannot be derived any parent type
let predicateDefinitionBlock = opt (leftBrace  >>. ((keywordIntrinsic <|> predContent) .>> IW) .>>. propertyList .>>. spacesRightBrace)
let inheritedType = positions (opt idStartsWithCap) .>> IW |>> Ast.InheritedType 
let inheritedTypeList = sepBy1 inheritedType comma |>> Ast.InheritedTypeList
let predicateSignature = positions (keywordPredicate >>. SW >>. (simpleSignature .>>. opt (colon >>. inheritedTypeList) .>> IW) .>>. paramTupleWithOptLeftParen) .>>. userDefinedSymbol .>> IW |>> Ast.PredicateSignature
let definitionPredicate = positions (predicateSignature .>>. predicateDefinitionBlock) |>> Ast.DefinitionPredicate

// Functional term building blocks can be defined similarly to classes, they can have properties but they cannot be derived any parent type 
let functionalTermDefinitionBlock = positions (opt (leftBrace  >>. ((keywordIntrinsic <|> funcContent) .>> IW) .>>. propertyList .>>. spacesRightBrace))  |>> Ast.FunctionalTermDefinitionBlock

let functionalTermSignature = positions (keywordFunction >>. SW >>. (simpleSignature .>>. opt (colon >>. inheritedTypeList) .>> IW) .>>. paramTupleWithOptLeftParen .>>. (IW >>. mapping)) .>>. userDefinedSymbol .>> IW |>> Ast.FunctionalTermSignature
let definitionFunctionalTerm = positions (functionalTermSignature .>>. functionalTermDefinitionBlock) |>> Ast.DefinitionFunctionalTerm

// Class definitions
let keywordClass = (skipString LiteralClL <|> skipString LiteralCl)

let constructorList = many1 (constructor .>> IW)
let classCompleteContent = varDeclOrSpecList .>>. constructorList |>> Ast.DefClassCompleteContent
let classDefinitionBlock = positions (opt (leftBrace  >>. ((keywordIntrinsic <|> classCompleteContent) .>> IW) .>>. propertyList .>>. spacesRightBrace)) |>> Ast.ClassDefinitionBlock

let classSignature = positions (keywordClass >>. SW >>. pascalCaseId) .>> IW |>> Ast.ClassSignature
let classSignatureExtended = classSignature .>>. opt (colon >>. inheritedTypeList) .>>. opt (attempt (IW >>. userDefinedObjSym)) .>> IW
let definitionClass = positions (classSignatureExtended .>>. classDefinitionBlock) |>> Ast.DefinitionClass 

let keywordDefinition = (skipString LiteralDefL <|> skipString LiteralDef) >>. SW
let definition = keywordDefinition >>. choice [
    definitionClass
    definitionPredicate
    definitionFunctionalTerm
]
(* Gathering together all Building Blocks to a theory *)

(* Localizations *)
// Localizations provide a possibility to automatically translate FPL expressions into natural languages
let keywordLocalization = (skipString LiteralLocL <|> skipString LiteralLoc) >>. SW
let localizationLanguageCode = positions (regex @"[a-z]{3}" <?> "<ISO 639 language code>") |>> Ast.LanguageCode

let ebnfTransl, ebnfTranslRef = createParserForwardedToRef()
let ebnfTranslTuple = (leftParen >>. IW >>. ebnfTransl) .>> (IW .>> rightParenOpt) 
let ebnfFactor = choice [
    variable
    quote >>. localizationString .>> quote
    ebnfTranslTuple
] 
let ebnfTerm = positions (sepEndBy1 ebnfFactor SW) |>> Ast.TranslationTerm
ebnfTranslRef.Value <-  positions (sepBy1 ebnfTerm (IW >>. case >>. IW)) |>> Ast.TranslationTermList
let language = positions ((exclamationMark >>. localizationLanguageCode .>> IW .>> colon) .>>. ebnfTransl) |>> Ast.Language
let languageList = many1 (IW >>. language .>> IW)
let localization = positions (keywordLocalization >>. predicate) .>> (IW .>> colonEqual) .>>. (languageList .>>. (IW >>. semiColonOpt)) .>> IW |>> Ast.Localization

// FPL building blocks can be definitions, axioms, Theorem-proof blocks and conjectures
let buildingBlock = choice [
    definition
    axiom
    theorem
    lemma
    proposition
    corollary
    conjecture
    proof
    ruleOfInference
    localization
    usesClause
    definitionExtension
]

let buildingBlockList = many (buildingBlock .>> IW)

(* Namespaces *)
let fplNamespace = buildingBlockList .>>. (IW >>. semiColonOpt) |>> Ast.Namespace
(* Final Parser *)
let ast =  positions (IW >>. fplNamespace) |>> Ast.AST

//------------
/// Regex used for the error recovery of FPL blocks using their keywords to be matched as whole words (\b option), followed by space (\s+ option)
let errRecoveryBlocks = $"\\b({LiteralDefL}|{LiteralDef}|{LiteralAxL}|{LiteralAx}|{LiteralPostL}|{LiteralPost}|{LiteralThmL}|{LiteralThm}|{LiteralPropL}|{LiteralProp}|{LiteralLemL}|{LiteralLem}|{LiteralCorL}|{LiteralCor}|{LiteralConjL}|{LiteralConj}|{LiteralPrfL}|{LiteralPrf}|{LiteralInfL}|{LiteralInf}|{LiteralLocL}|{LiteralLoc}|{LiteralExtL}|{LiteralExt}|{LiteralUses})\\s+"

let masked (input:String) =
    input
    |> Seq.map (fun ch -> if Char.IsWhiteSpace(ch) then ch else ' ')
    |> Seq.toArray
    |> fun arr -> System.String(arr)

let keepOnlyRange (startIdx: int) (endIdx: int) (input: string) =
    input
    |> Seq.mapi (fun i c ->
        if i >= startIdx && i < endIdx then
            // inside visible range: keep everything
            c
        else
            // outside: keep whitespace, mask non-whitespace
            if System.Char.IsWhiteSpace c then c else ' ')
    |> Seq.toArray
    |> System.String

let getMatches (pattern: string) (input: string) =
    Regex.Matches(input, pattern)
    |> Seq.cast<Match>
    |> Seq.toList

let maskAroundIndex (matches: Match list) (i: int) (input: string) =

    match matches with
    | [] ->
        // no matches → nothing to do
        input

    | _ when i = 0 && matches.Length <= 1 ->
        input
    | _ when i = 0 && matches.Length > 1 ->
        input.Substring(0, matches[1].Index)
    | ms when i >= ms.Length - 1 ->
        // i >= last index: mask everything up to this match
        let m = ms[min i (ms.Length - 1)]
        keepOnlyRange m.Index input.Length input

    | ms ->
        // middle case: keep only the i-th match visible
        let m = ms.[i]
        let startIdx = m.Index
        let endIdx   = m.Index + m.Length
        keepOnlyRange startIdx endIdx input

/// Splits an FParsec error message to sub-errors (if it contains many)
let private splitByBacktrackMarker (input: string) =
    let pattern = @"\s*The parser backtracked after:\s*Error"
    let split = Regex.Split(input, pattern, RegexOptions.Multiline)
    split
    |> Array.mapi (fun i s ->
        match i with
        | 0 -> s.Replace("Error in ", "FPL syntax error in ")
        | _ -> $"The parser backtracked after FPL syntax error{s}")
    |> Array.toList

/// Takes the string list output of splitByBacktrackMarker and extracts FParsec positions from the error messages producing a list of tuples (Position, error string).
let private extractPositions (lines: string list) =
    let pattern = @"syntax error in Ln:\s*(\d+)\s*Col:\s*(\d+)\s*"

    lines
    |> List.map (fun line ->
        let m = Regex.Match(line, pattern)
        if m.Success then
            let ln  = int m.Groups.[1].Value
            let col = int m.Groups.[2].Value - 1   // one less than reported
            let pos = Position("", 0L, int64 ln, int64 col)
            pos, line
        else
            // no match → default position
            let pos = Position("", 0L, 0L, 0L)
            pos, line
    )

/// Groups the output list of tuples from the extractPositions function
/// by identical Position, but only aggregates those whose message ends with an 'Expecting:' clause,
/// and then merges all the expectation‑tails into one combined message.
let private aggregateExpecting (items: (Position * string) list) =
    let expectingKey = "Expecting:"

    // Helper: split a message into (prefix, tail) if it contains "Expecting:"
    let trySplitExpecting (s: string) =
        let idx = s.IndexOf(expectingKey)
        if idx >= 0 then
            let prefix = s.Substring(0, idx + expectingKey.Length)
            let tail   = s.Substring(idx + expectingKey.Length).Trim()
            Some(prefix, tail)
        else None

    items
    |> List.groupBy fst
    |> List.collect (fun (pos, group) ->
        // Extract only those with an Expecting: clause
        let withExp =
            group
            |> List.choose (fun (p, s) ->
                match trySplitExpecting s with
                | Some(prefix, tail) -> Some(prefix, tail)
                | None -> None)

        match withExp with
        | [] ->
            // No aggregatable entries → keep original group unchanged
            group

        | (prefix, _) :: _ ->
            // Aggregate all tails
            let tails =
                withExp
                |> List.map snd
                |> String.concat ", "

            let combined = prefix + " " + tails
            [pos, combined])

/// Scans input line‑by‑line, detecting a line that trims to "^",
/// and inserts ⚡ into the preceding line at the same column,
/// skipping the caret‑line, and preserving all other lines unchanged.
let private insertLightning (input: string) =
    let lines = input.Split(Environment.NewLine) |> Array.toList

    // Process line-by-line with access to previous output line
    let rec loop acc remaining =
        match remaining with
        | [] ->
            acc |> List.rev |> String.concat Environment.NewLine

        | (line:string) :: rest ->
            let trimmed = line.Trim()

            if trimmed = "^" && acc <> [] then
                // caret-line: determine caret column in the *original* line
                let caretCol = line.IndexOf("^")

                // modify the previous line
                let prev = List.head acc
                let prevLen = prev.Length

                let updatedPrev =
                    if caretCol < prevLen then
                        // insert ⚡ at caretCol
                        prev.[0..caretCol-1] + "⚡" + prev.[caretCol..]
                    else
                        // previous line too short → append ⚡
                        prev + "⚡"

                // replace previous line, skip caret-line
                loop (updatedPrev :: (List.tail acc)) rest

            else
                // normal line → append, but trimmed
                loop (trimmed :: acc) rest

    loop [] lines

/// Computes FParsec’s Position.Index based on Line and Column in an input string.
let private computeIndex (pos: Position) (lines: string array) inputLength =
    
    // FParsec Position.Line and Column are 1-based
    let lineIdx  = int pos.Line - 1
    let colIdx   = int pos.Column

    if lineIdx >= 0 && lineIdx < lines.Length then
        let line = lines.[lineIdx]

        if colIdx >= 0 && colIdx <= line.Length then
            // sum of lengths of all previous lines + newline characters
            let prefixLength =
                lines
                |> Seq.take lineIdx
                |> Seq.sumBy (fun l -> l.Length + 1)   // +1 for '\n'

            int64 prefixLength + int64 colIdx
        else
            int64 inputLength
    else
        int64 inputLength

/// Transforms an error message of FParserc preserving the first two lines, and if the third line starts with "Expecting:",
/// then flattening all remaining lines into that third line by concatenating them without line breaks.
let private collapseExpectingBlock (input: string) : string =
    let lines = input.Split(Environment.NewLine) |> Array.toList

    match lines with
    | [] -> ""
    | [l1] -> l1
    | [l1; l2] -> l1 + Environment.NewLine + l2
    | l1 :: l2 :: l3 :: rest ->
        if l3.TrimStart().StartsWith("Expecting:") then
            // Concatenate line 3 with all remaining lines (no newlines)
            let merged =
                l3 + (
                    rest
                    |> List.map (fun s -> s.Trim())
                    |> List.map (fun s -> s.Replace("Other error messages:", ""))
                    |> List.map (fun s -> s.Replace("Expecting:", ", "))
                    |> List.map (fun s -> s.Replace("' or '", ", "))
                    |> String.concat " "
                )
            String.concat Environment.NewLine [l1; "    " + l2; merged]
        else
            // No special rule → return unchanged
            String.concat Environment.NewLine lines

let correctPositionIndexBasedOnLineAndColumn (lines:string array) length (items: (Position * string) list) =
    items
    |> List.map (fun (pos, errMsg) ->
        (Position("", computeIndex pos lines length, pos.Line, pos.Column), errMsg)
    )

/// Tries to parse all chunks of input of FPL building blocks. If a chunk produces a syntax error,
/// a diagnosics will be issued and the chunk will be replaced by a masked chunk, where
/// all non-whitespace characters are replaced with spaces while preserving line breaks and line lengths.
/// The syntax error is generated using an input starting with the chunk, so syntax error messages
/// realistically reflect the remaining code after the chunk.
let cleanInputAndIssueSyntaxErrors fplCode =
    let input = fplCode |> removeFplComments 
    let matches = getMatches errRecoveryBlocks input
    let errorFreeInput = StringBuilder()
    let maskedPrefix = StringBuilder()
    let lines = input.Split(Environment.NewLine)
    let length = input.Length

    let rec tryGetAst1 i =
        let remainder = StringBuilder()
        let chunk, maskedChunk = 
            if i = 0 && matches.Length>0 && matches[i].Index>0 then
                let chuPrefix = input.Substring(0, matches[i].Index)
                let mChuPrefix = masked chuPrefix
                errorFreeInput.Append(mChuPrefix) |> ignore
                let pos = matches[i].Index
                let length =
                    if i < matches.Length - 1 then
                        matches[i+1].Index - matches[i].Index
                    else
                        input.Length - matches[i].Index
                let chu = input.Substring(pos, length)
                let mChu = masked chu
                maskedPrefix.Append(mChu) |> ignore
                remainder.Append(mChuPrefix) |> ignore
                remainder.Append(chu) |> ignore
                chu, mChu 
            elif i < matches.Length-1 then
                let pos = matches[i].Index
                let length = matches[i+1].Index - matches[i].Index
                let chu = input.Substring(pos, length)
                let mChu = masked chu
                maskedPrefix.Append(mChu) |> ignore
                remainder.Append(maskedPrefix) |> ignore
                remainder.Append(chu) |> ignore
                chu, mChu
            elif i = matches.Length-1 then
                let pos = matches[i].Index
                let chu = input.Substring(pos)
                let mChu = masked chu
                remainder.Append(input.Substring(pos)) |> ignore
                chu, mChu
            elif matches.Length = 0 then
                let chu = input
                let mChu = masked chu
                chu, mChu
            else
                "", ""
        if i >= matches.Length && i > 0 then
            ()
        else
            let trimedInput = chunk.Trim()
            // run buildingBlock parser until eof of trimedInput
            match run (buildingBlock .>> eof) trimedInput with
            | Success(_, _, _) when trimedInput.Length > 0 ->
                // if successed when trimed not empty, add chunk to error-free input
                errorFreeInput.Append(chunk) |> ignore
            | _ ->
                // in all other cases (trimedInput empty or syntax error in building block)
                // try the buildingBlock parser without eof
                let posSuccess, successButNotTheEndOfChunk =
                    match run buildingBlock trimedInput with
                    | Success(_, _, userState) when trimedInput.Length > 0 -> (int)userState.Index, true
                    | _ -> -1, false
                if successButNotTheEndOfChunk then
                    // add maskedChunk up to the parsed position to the error-free input
                    errorFreeInput.Append(trimedInput.Substring(0, posSuccess)) |> ignore
                    if maskedChunk.Length > posSuccess then
                        if matches.Length = 1 then
                            // handle the special case when we have only one building block with a closing ";"
                            let rest = chunk.Substring(posSuccess).Trim()
                            if rest=";" then 
                                errorFreeInput.Append(chunk.Substring(posSuccess)) |> ignore
                        else
                            errorFreeInput.Append(maskedChunk.Substring(posSuccess)) |> ignore
                    else
                        errorFreeInput.Append(maskedChunk) |> ignore
                elif i = 0 && matches.Length=0 then
                    errorFreeInput.Append(chunk) |> ignore
                else
                    // add maskedChunk to error-free input
                    errorFreeInput.Append(maskedChunk) |> ignore
                // now, run the whole "ast" parser on the remainer
                // that starts ends the chunk we found to be faulty in the outer match,
                // possibly prefixed with whitespaces from masked previous chunks
                let rest = remainder.ToString()
                match run (ast .>> eof) rest with 
                | Failure(errorMsg, originalErrPos, _) ->
                    // in case of syntax error, issue diagnostics
                    errorMsg
                    |> insertLightning
                    |> splitByBacktrackMarker
                    |> extractPositions
                    |> aggregateExpecting
                    |> correctPositionIndexBasedOnLineAndColumn lines length 
                    |> List.map (fun (pos, errMsg) ->
                        let diagnostic =
                            { 
                                Diagnostic.Uri = ad.CurrentUri
                                Diagnostic.Emitter = DiagnosticEmitter.FplParser 
                                Diagnostic.Severity = DiagnosticSeverity.Error
                                Diagnostic.StartPos = pos
                                Diagnostic.EndPos = pos
                                Diagnostic.Code = SY999 (collapseExpectingBlock errMsg)
                                Diagnostic.Alternatives = None
                            }
                        ad.AddDiagnostic diagnostic
                    )
                    |> ignore
                | _ -> () // if success, all good
            tryGetAst1 (i + 1)

    tryGetAst1 0
    errorFreeInput.ToString()
//-----------------












let stdParser = ast
let stdCode = SXN000
let stdCode1 = SXN001
let stdParser1 = ast

let calculateCurrentContext (matchList:System.Collections.Generic.List<int>) i = 
    let index = matchList[i]
    if i + 1 < matchList.Count then
        let nextIndex = matchList[i+1]
        index, nextIndex
    else
        index, index

let errRecPattern = $"({LiteralDefL}|{LiteralDef}|{LiteralPrtyL}|{LiteralPrty}|{LiteralAxL}|{LiteralAx}|{LiteralPostL}|{LiteralPost}|{LiteralThmL}|{LiteralThm}|{LiteralPropL}|{LiteralProp}|{LiteralLemL}|{LiteralLem}|{LiteralCorL}|{LiteralCor}|{LiteralConjL}|{LiteralConj}|{LiteralDecL}|{LiteralDec}|{LiteralCtorL}|{LiteralCtor}|{LiteralPrfL}|{LiteralPrf}|{LiteralInfL}|{LiteralInf}|{LiteralLocL}|{LiteralLoc}|{LiteralExtL}|{LiteralExt}|{LiteralUses}|{LiteralAnd}|{LiteralOr}|{LiteralImpl}|{LiteralIif}|{LiteralXor}|{LiteralNot}|{LiteralAll}|{LiteralExN}|{LiteralEx}|{LiteralIs}|{LiteralAssert}|{LiteralAssL}|{LiteralAss}|{LiteralBase}|{LiteralCases}|{LiteralMapCases}|{LiteralFor}|{LiteralIn}|{LiteralDelL}|{LiteralDel}|\|\-|{LiteralRevL}|{LiteralRev}|{LiteralRetL}|{LiteralRet})\W|({LiteralConL}|{LiteralCon}|{LiteralPreL}|{LiteralPre})\s*\:|~[a-z]+"

let errInformation = [
    (DEF000, [LiteralDefL; LiteralDef], definition)
    (PRP000, [LiteralPrtyL; LiteralPrty], definitionProperty)
    (AXI000, [LiteralAxL; LiteralAx; LiteralPostL; LiteralPost], axiom)
    (THM000, [LiteralThmL; LiteralThm], theorem)
    (COR000, [LiteralCorL; LiteralCor], corollary)
    (LEM000, [LiteralLemL; LiteralLem], lemma)
    (PPS000, [LiteralPropL; LiteralProp], proposition)
    (CNJ000, [LiteralConjL; LiteralConj], conjecture)
    (VAR000, [LiteralDecL; LiteralDec], varDeclBlock)
    (CTR000, [LiteralCtorL; LiteralCtor], constructor)
    (PRF000, [LiteralPrfL; LiteralPrf], proof)
    (INF000, [LiteralInfL; LiteralInf], ruleOfInference)
    (LOC000, [LiteralLocL; LiteralLoc], localization)
    (EXT000, [LiteralExtL; LiteralExt], extension)
    (USE000, [LiteralUses], usesClause)
    (PRD000, [LiteralAnd], conjunction)
    (PRD000, [LiteralOr], disjunction)
    (PRD000, [LiteralImpl], implication)
    (PRD000, [LiteralIif], equivalence)
    (PRD000, [LiteralXor], exclusiveOr)
    (PRD000, [LiteralNot], negation)
    (PRD000, [LiteralAll], all)
    (PRD000, [LiteralEx], exists)
    (PRD000, [LiteralExN], existsTimesN)
    (PRD000, [LiteralIs], isOperator)
    (STMASE, [LiteralAssert], assertionStatement)
    (STMCAL, [LiteralBase], baseConstructorCall)
    (STMCAS, [LiteralCases], casesStatement)
    (STMMAP, [LiteralMapCases], mapCases)
    (STMFOI, [LiteralIn], inEntity)
    (STMFOR, [LiteralFor], forStatement)
    (STMDEL, [LiteralDelL; LiteralDel], fplDelegate)
    (STMASU, [LiteralAssL; LiteralAss], assumeArgument)
    (STMREV, [LiteralRevL; LiteralRev], revokeArgument)
    (STMRET, [LiteralRetL; LiteralRet], returnStatement)
    (AGI000, ["|-"], argumentInference)
    (PRE000, [LiteralPreL; LiteralPre], premiseList)
    (CON000, [LiteralConL; LiteralCon], conclusion)
    (TYD000, ["~"], varDecl)
]
/// Finds the error information tuple based on a prefix of a string from the errInformation list. 
/// If no prefix matches than the SYN000 tuple will be returned.
let findErrInfoTuple (str:string) =
    match List.tryFind (fun (_, prefixes, _) -> List.exists (fun prefix -> str.StartsWith(prefix : string)) prefixes) errInformation with
    | Some tuple -> 
        tuple
    | None -> 
        (stdCode, [], stdParser)


let findFirstIndexInMatches (matchList:System.Collections.Generic.List<int>) pIndex kMax =
    let rec loop i last =
        if i >= matchList.Count then 
            kMax
        else 
            let index = matchList[i]
            if index > pIndex then 
                last
            else 
                loop (i + 1) i
    loop 0 0

let maxIntervalBound (intervals:System.Collections.Generic.List<Interval>) =
    let mutable maxBound = -1
    for interval in intervals do
        if interval.End > maxBound then
            maxBound <- interval.End
        if interval.End = -1 && interval.Start > maxBound then
            maxBound <- interval.Start
    maxBound

let fplParser (input:string) =

    let cleanUpInput = cleanInputAndIssueSyntaxErrors input
    match run (stdParser .>> eof) cleanUpInput with
    | Success(result, restInput, userState) ->
        result
    | Failure(errMsg, restInput, _) ->
        Ast.BuildingBlockError((restInput.Position, restInput.Position), errMsg)



let fplParserOld (input:string) = 
    let preProcessedInput = input |> removeFplComments
    let matchList = stringMatches preProcessedInput errRecPattern

    let intervals = new System.Collections.Generic.List<Interval>()

    let parseResult, pIndex = tryParseFirstError stdParser preProcessedInput stdCode  
    intervals.Add(Interval(0, pIndex))

    let mutable lastParserIndex = 0
    let mutable lastParser = stdParser
    let mutable lastCode = stdCode
    let mutable lastMsg = stdCode.Message
    if parseResult = Ast.Error then
        let mutable lastSuccess = false
        // skip parsing any matches until the first error index (stored in pIndex)
        let firstIndex = findFirstIndexInMatches matchList (int pIndex) preProcessedInput.Length
        for i in [firstIndex..matchList.Count-1] do
            let index, nextIndex = calculateCurrentContext matchList i
            let subString = preProcessedInput.Substring(index)
            if (-1 < lastParserIndex) && (lastParserIndex < index) && not lastSuccess && lastCode <> stdCode then
                // the last parsing process hasn't consumed all the input between lastParserIndex and index
                let remainingChunk = preProcessedInput.Substring(int lastParserIndex, (index - int lastParserIndex))
                // emit error messages for this chunk of input string using the last parser  
                tryParseRemainingChunk lastParser remainingChunk lastParserIndex index lastCode -1 ""
                intervals.Add(Interval(lastParserIndex, nextIndex))
                lastParserIndex <- nextIndex
            else
                // otherwise, find the next error info tuple based on the current substring
                let code, prefixList, errRecParser = findErrInfoTuple subString
                // try to parse substring using the parser from the error info and emitting diagnostics (if any)
                let pResult, pIndex, pSuccess = tryParse errRecParser subString index nextIndex code -1 ""
                intervals.Add(Interval(index, pIndex))
                lastParserIndex <- pIndex
                lastParser <- errRecParser
                lastCode <- code
                lastMsg <- code.Message
                lastSuccess <- pSuccess

    // emit diagnostics for any error positions that are not overlayed by the intervals
    tryParseRemainingOnly stdParser preProcessedInput stdCode intervals -1 ""
    // Return an ast on a best effort basis even if there were some errors 
    let resultingAst = tryGetAst stdParser preProcessedInput -1

    let maxBound = maxIntervalBound intervals
    let remaingString = preProcessedInput.Substring(maxBound).TrimEnd()
    if not (remaingString.EndsWith("}") && Regex.Matches(preProcessedInput, "\}").Count = Regex.Matches(preProcessedInput, "\{").Count) then
        // prevent emitting "false-positive" errors of characters found after namespace using the heuristic that 
        // the last character of a namespace is "}" and then looks "good"
        tryParseRemainingChunk stdParser1 (preProcessedInput.Substring(maxBound)) maxBound (preProcessedInput.Length) stdCode1 -1 ""
    resultingAst


let parserDiagnostics = ad

/// Returns the parser choices at position (if any).
let getParserChoicesAtPosition (input:string) index =
    let newInput = input |> removeFplComments
    match run ast (newInput.Substring(0, index)) with
    | Success(result, restInput, userState) -> 
        // In the success case, we always return the current parser position in the input
        List.empty, userState.Index
    | Failure(errorMsg, restInput, userState) ->
        let newErrMsg, choices = mapErrMsgToRecText input errorMsg restInput.Position
        choices, restInput.Position.Index

/// Used only to test FplLS CompletionItems correct syntax, which is written in C# and requires this module to run
let testParser (parserType:string) (input:string) =
    let trimmed = (input.Trim()) |> removeFplComments
    match parserType with 
    | LiteralLoc -> 
        let result = run (localization .>> eof) trimmed
        sprintf "%O" result
    | LiteralAx -> 
        let result = run (axiom .>> eof) trimmed 
        sprintf "%O" result
    | LiteralCases ->
        let result = run (casesStatement .>> eof) trimmed 
        sprintf "%O" result
    | LiteralMapCases ->
        let result = run (mapCases .>> eof) trimmed 
        sprintf "%O" result
    | LiteralCtor ->
        let result = run (constructor .>> eof) trimmed 
        sprintf "%O" result
    | LiteralCor ->
        let result = run (corollary .>> eof) trimmed 
        sprintf "%O" result
    | LiteralDec ->
        let result = run (varDeclBlock .>> eof) trimmed 
        sprintf "%O" result
    | LiteralDef ->
        let result = run (definition .>> eof) trimmed 
        sprintf "%O" result
    | LiteralDel ->
        let result = run (fplDelegate .>> eof) trimmed 
        sprintf "%O" result
    | LiteralExt ->
        let result = run (definitionExtension .>> eof) trimmed 
        sprintf "%O" result
    | LiteralFor ->
        let result = run (forStatement .>> eof) trimmed 
        sprintf "%O" result
    | LiteralIs ->
        let result = run (isOperator .>> eof) trimmed 
        sprintf "%O" result
    | PrimPascalCaseId ->
        let result = run (pascalCaseId .>> eof) trimmed 
        sprintf "%O" result
    | PrimPredicate ->
        let result = run (predicate .>> eof) trimmed 
        sprintf "%O" result
    | LiteralPrf ->
        let result = run (proof .>> eof) trimmed 
        sprintf "%O" result
    | LiteralPrty ->
        let result = run (definitionProperty .>> eof) trimmed 
        sprintf "%O" result
    | PrimQuantor ->
        let result = run (compoundPredicate .>> eof) trimmed 
        sprintf "%O" result
    | PrimTheoremLike ->
        let result = run (buildingBlock .>> eof) trimmed 
        sprintf "%O" result
    | _ -> $"testParser {parserType} not implemented"
