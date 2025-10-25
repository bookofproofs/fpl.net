/// This module contains the FPL parser producing an abstract syntax tree out of a given FPL code 
module FplParser
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

let tokenizer = Tokenizer()

/// Takes the parser `p` and returns a tuple of its result, together with its starting and ending position.
let positions (tokenName:string) (p: Parser<_,_>): Parser<Positions * _,_> =
    pipe2
        (_position .>>. p)
        (_position)
        (fun (startPos, result) endPos -> 
            let pos1 = Position("", startPos.Index, startPos.Line, startPos.Column)
            let pos2 = Position("", endPos.Index, endPos.Line, endPos.Column)
            let token = { Token.Name = tokenName; Token.StartPos = pos1; Token.EndPos = pos2}
            tokenizer.Push(token)
            (Positions(pos1, pos2), result)
        )

/// Takes the parser `p` and returns a result with the side effect of remembering the parsed token 
let tokenize (tokenName:string) (p: Parser<_,_>): Parser<_,_> =
    pipe2
        (_position .>>. p)
        (_position)
        (fun (startPos, result) endPos -> 
            let pos1 = Position("", startPos.Index, startPos.Line, startPos.Column)
            let pos2 = Position("", endPos.Index, endPos.Line, endPos.Column)
            let token = { Token.Name = tokenName; Token.StartPos = pos1; Token.EndPos = pos2}
            tokenizer.Push(token)
            result
        )

(* Literals *)

let leftBrace = tokenize "LeftBrace" (skipChar '{') >>. spaces 
let rightBrace = tokenize "RightBrace" (skipChar '}') 
let leftParen = tokenize "LeftParen" (skipChar '(') >>. spaces 
let rightParen = tokenize "RightParen" (skipChar ')') 
let comma = tokenize "Comma" (skipChar ',') >>. spaces 
let dot = positions "Dot" (skipChar '.') |>> Ast.Dot
let colon = positions "One" (skipChar ':') .>> spaces |>> Ast.One
let colonStar = positions "Many" (skipString ":*") .>> spaces |>> Ast.Many
let colonPlus = positions "Many1" (skipString ":+") .>> spaces |>> Ast.Many1
let colonEqual = tokenize ":=" (skipString ":=") >>. spaces 
let at = pchar '@'
let case = skipChar '|' >>. spaces
let elseCase = skipChar '?' >>. spaces
let leftBracket = skipChar '[' >>. spaces 
let rightBracket = skipChar ']' >>. spaces  
let tilde = skipChar '~' .>> spaces
let semiColon = skipChar ';' >>. spaces
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
let extensionString = regex @"[^,\s()\[\]{}\:]+" <?> "<extensionString>" 
let extension = positions "Extension" (at >>. extensionString) |>> Ast.Extension

(* Identifiers *)


let IdStartsWithSmallCase = regex @"[a-z]\w*" 
let idStartsWithCap = (regex @"[A-Z]\w*") <?> "<PascalCaseId>"
let pascalCaseId = positions "PascalCaseId" idStartsWithCap |>> Ast.PascalCaseId

let namespaceIdentifier = positions "NamespaceIdentifier" (sepBy1 pascalCaseId dot) .>> IW |>> Ast.NamespaceIdentifier
let predicateIdentifier = positions "PredicateIdentifier" (sepBy1 pascalCaseId dot) |>> Ast.PredicateIdentifier 

let alias = positions "Alias" (skipString LiteralAlias >>. SW >>. idStartsWithCap) |>> Ast.Alias
let star = positions "Star" (skipChar '*') |>> Ast.Star

let aliasedNamespaceIdentifier = positions "AliasedNamespaceIdentifier" (namespaceIdentifier .>>. opt (alias <|> star)) |>> Ast.AliasedNamespaceIdentifier
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

let variable = positions "Var" variableX |>> Ast.Var 

let variableList = (sepBy1 (variable .>> IW) comma) .>> IW

let keywordSelf = positions "Self" (skipString LiteralSelf) .>> IW |>> Ast.Self
let keywordParent = positions "Parent" (skipString LiteralParent) .>> IW |>> Ast.Parent
let keywordBaseClassReference = skipString LiteralBase .>> IW
let keywordIndex = positions "IndexType" (skipString LiteralIndL <|> skipString LiteralInd) |>> Ast.IndexType


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
let keywordUndefined = positions "Undefined" (skipString LiteralUndefL <|> skipString LiteralUndef) .>> IW |>> Ast.Undefined
let keywordTrue = positions "True" (skipString LiteralTrue) .>> IW  |>> Ast.True  
let keywordFalse = positions "False" (skipString LiteralFalse) .>> IW |>>  Ast.False  
let keywordByDef = pstring LiteralByDef 
let keywordByAx = pstring LiteralByAx 
let keywordByInf = pstring LiteralByInf
let keywordByConj = pstring LiteralByConj
let keywordByCor = pstring LiteralByCor
let byModifier = choice [keywordByAx; keywordByConj; keywordByCor; keywordByDef; keywordByInf] .>> SW 
let keywordAnd = skipString LiteralAnd .>> IW 
let keywordOr = skipString LiteralOr .>> IW 
let keywordImpl = skipString LiteralImpl .>> IW 
let keywordIif = skipString LiteralIif .>> IW 
let keywordXor = skipString LiteralXor .>> IW 
let keywordNot = skipString LiteralNot .>> attemptSW 
let keywordAll = skipString LiteralAll .>> SW 
let keywordEx = skipString LiteralEx .>> SW
let keywordExN = skipString LiteralExN .>> IW
let keywordIs = skipString LiteralIs .>> attemptSW 


// Via templates, FPL supports generic types, which make it possible to define abstract mathematical
// objects and their properties that defer the concrete
// specification of one or more types until the definition or method is declared and instantiated by
// client code
let keywordTemplate = (pstring LiteralTplL <|> pstring LiteralTpl) 

let templateTail = choice [ idStartsWithCap; (regex @"\d+") ]

let templateWithTail = (many1Strings2 (pstring "template" <|> pstring LiteralTpl) templateTail) 

let keywordObject = positions "ObjectType" (skipString LiteralObjL <|> skipString LiteralObj) |>> Ast.ObjectType 

let templateType = positions "TemplateType" ((attempt templateWithTail) <|> keywordTemplate) |>>  Ast.TemplateType

let keywordPredicate = positions "PredicateType" (skipString LiteralPredL <|> skipString LiteralPred) |>> Ast.PredicateType
let keywordFunction = positions "FunctionalTermType" (skipString LiteralFuncL <|> skipString LiteralFunc) |>> Ast.FunctionalTermType


let theoryNamespace = aliasedNamespaceIdentifier <|> namespaceIdentifier .>> IW

let keywordUses = (skipString LiteralUses) .>> SW
let usesClause = positions "UsesClause" (keywordUses >>. theoryNamespace) |>> Ast.UsesClause

(* Signatures, Variable Declarations, and Types, and Coordinates *)
// convention: All syntax production rules of FPL syntax extensions have to start with LiteralExt, followed by
// a Pascal Case id.
// This ensures that they will not be mixed-up with original FPL ebnf productions
// that are all PascalCase as well as FPL keywords, that are all small case.

let dollarDigits = positions "DollarDigits" (regex "\$" >>. puint32 <?> "<dollarDigits>") |>> Ast.DollarDigits

let selfOrParent = positions "SelfOrParent" (choice [keywordSelf ; keywordParent]) |>> Ast.SelfOrParent

let entity = choice [ selfOrParent ; variable ] 

////// resolving recursive parsers
let statementList, statementListRef = createParserForwardedToRef()
let primePredicate, primePredicateRef = createParserForwardedToRef()
let predicate, predicateRef = createParserForwardedToRef()
let predicateList, predicateListRef = createParserForwardedToRef()
let predicateWithQualification, predicateWithQualificationRef = createParserForwardedToRef()
let paramTuple, paramTupleRef = createParserForwardedToRef()
let classType, classTypeRef = createParserForwardedToRef()

let coord = choice [ predicateWithQualification; dollarDigits ] .>> IW 

// infix operators like the equality operator 
let objectSymbol = positions "ObjectSymbol" ( objectMathSymbols ) .>> IW |>> Ast.ObjectSymbol

let fplIdentifier = choice [ entity; predicateIdentifier; extension; objectSymbol ] 

let coordList = (sepBy1 coord comma) .>> IW

let bracketedCoords = positions "BrackedCoordList" (leftBracket >>. coordList .>> rightBracket) |>> Ast.BrackedCoordList

let namedVariableDeclarationList, namedVariableDeclarationListRef = createParserForwardedToRef()

let keywordExtension = (skipString LiteralExtL <|> skipString LiteralExt) .>> SW

let extensionName = positions "ExtensionName" (idStartsWithCap) |>> Ast.ExtensionName

let xId = positions "ExtensionType" (at >>. extensionName) |>> Ast.ExtensionType 

let specificClassType = choice [ keywordObject; predicateIdentifier ] 

//// later semantics: Star: 0 or more occurrences, Plus: 1 or more occurrences
let varDeclModifier = choice [ colonStar; colonPlus; colon ] .>> IW

// The classType is the last type in FPL we can derive FPL classes from.
// It therefore excludes the in-built FPL-types keywordPredicate, keywordFunction, and keywordIndex
// to restrict it to pure objects.
// In contrast to variableType which can also be used for declaring variables 
// in the scope of FPL building blocks

classTypeRef.Value <- positions "ClassType" (specificClassType) |>> Ast.ClassType

let mapping, mappingRef = createParserForwardedToRef()
let predicateType = positions "CompoundPredicateType" (keywordPredicate .>>. opt paramTuple) |>> Ast.CompoundPredicateType
let functionalTermType = positions "CompoundFunctionalTermType" (keywordFunction .>>. opt (paramTuple .>>. (IW >>. mapping))) |>> Ast.CompoundFunctionalTermType
let variableType = positions "VariableType" (choice [ keywordIndex; xId; classType; templateType; functionalTermType; predicateType ]) |>> Ast.VariableType

let namedVariableDeclaration = positions "NamedVarDecl" (variableList .>>. varDeclModifier .>>. variableType .>> IW) |>> Ast.NamedVarDecl
namedVariableDeclarationListRef.Value <- sepBy namedVariableDeclaration comma

paramTupleRef.Value <- positions "ParamTuple" ((leftParen >>. IW >>. namedVariableDeclarationList) .>> (IW .>> rightParen)) |>> Ast.ParamTuple
let simpleSignature = pascalCaseId .>> IW 

let localizationString = positions "LocalizationString" (regex "[^\"\n]*") <?> "<language-specific string>" |>> Ast.LocalizationString

let keywordSymbol = pstring LiteralSymbol .>> IW
let objectSymbolString = pchar '"' >>. objectMathSymbols .>> pchar '"'
let infixString = pchar '"' >>. infixMathSymbols .>> pchar '"'
let keywordInfix = pstring LiteralInfix >>. IW
let postfixString = pchar '"' >>. postfixMathSymbols .>> pchar '"' 
let keywordPostfix = pstring LiteralPostFix >>. IW
let prefixString = pchar '"' >>. prefixMathSymbols .>> pchar '"' 
let keywordPrefix = pstring LiteralPrefix >>. IW
let userDefinedObjSym = positions "Symbol" (keywordSymbol >>. objectSymbolString) .>> IW |>> Ast.Symbol
let precedence = positions "Precedence" (pint32) .>> IW |>> Ast.Precedence

let userDefinedInfix = positions "Infix" (keywordInfix >>. (infixString .>>. (IW >>. precedence))) .>> IW |>> Ast.Infix
let userDefinedPostfix = positions "Postfix" (keywordPostfix >>. postfixString) .>> IW |>> Ast.Postfix
let userDefinedPrefix = positions "Prefix" (keywordPrefix >>. prefixString) .>> IW |>> Ast.Prefix
let userDefinedSymbol = opt (attempt (IW >>. choice [userDefinedPrefix; userDefinedInfix; userDefinedPostfix ]))

(* Statements *)
let argumentTuple = positions "ArgumentTuple" ((leftParen >>. predicateList) .>> (IW .>> rightParen)) |>> Ast.ArgumentTuple 

let fplDelegate = positions "Delegate" (keywordDel >>. dot >>. idStartsWithCap .>>. (IW >>. argumentTuple)) |>> Ast.Delegate

let spacesRightBrace = (IW .>> rightBrace) 

let keywordReturn = IW >>. (skipString LiteralRetL <|> skipString LiteralRet) .>> SW 


let caseElse = positions "CaseElse" (elseCase >>. IW >>. statementList .>> IW)  |>> Ast.CaseElse
let caseSingle = positions "CaseSingle" ((case >>. predicate .>> colon) .>>. statementList) |>> Ast.CaseSingle
let caseSingleList = many1 (IW >>. caseSingle)
let casesStatement = positions "Cases" (((keywordCases >>. leftParen >>. IW >>. caseSingleList .>>. caseElse .>> rightParen))) |>> Ast.Cases

let mapCaseElse = positions "MapCaseElse" (elseCase >>. IW >>. predicate .>> IW) |>> Ast.MapCaseElse
let mapCaseSingle = positions "MapCaseSingle" ((case >>. predicate .>> colon) .>>. (IW >>. predicate)) |>> Ast.MapCaseSingle
let mapCaseSingleList = many1 (IW >>. mapCaseSingle)
let mapCases = positions "MapCases" (((keywordMapCases >>. leftParen >>. IW >>. mapCaseSingleList .>>. mapCaseElse .>> rightParen))) |>> Ast.MapCases

let assignmentStatement = positions "Assignment" ((predicateWithQualification .>> IW .>> colonEqual) .>>. predicate) |>> Ast.Assignment

let inEntity = keywordIn >>. positions "InEntity" (predicateWithQualification) .>> IW |>> Ast.InEntity

let entityInDomain = ( entity .>> IW .>>. inEntity ) .>> IW
let forInBody = (entityInDomain .>> IW) .>>. (leftBrace >>. IW >>. statementList) .>> (IW >>. rightBrace)
let forStatement = positions "ForIn" (keywordFor >>. forInBody) |>> Ast.ForIn

//// Difference of assertion to an axiom: axiom is named predicate, while an assertion uses a predicated to assert it.
//// Difference of assertion to a mandatory property: a mandatory property introduces a completely new identifier inside
//// the scope of a definition. An assertion uses a predicate referring to existing identifiers in the whole theory
//// Difference of assertion to assume: the latter will be used only in the scope of proofs
let assertionStatement = positions "Assertion" (keywordAssert >>. predicate) |>> Ast.Assertion
let inheritedClassType = predicateIdentifier
let baseConstructorCall = positions "BaseConstructorCall" (keywordBaseClassReference >>. dot >>. (inheritedClassType .>> IW).>>. argumentTuple .>> IW) |>> Ast.BaseConstructorCall

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
let predicateWithOptSpecification = positions "PredicateWithOptSpecification" (fplIdentifier .>>. optionalSpecification) |>> Ast.PredicateWithOptSpecification
let dottedPredicate = positions "DottedPredicate" (dot >>. predicateWithOptSpecification) |>> Ast.DottedPredicate
let qualificationList = positions "QualificationList" (many dottedPredicate) |>> Ast.QualificationList
let dollarDigitList = many1 dollarDigits
let referencingIdentifier = positions "ReferencingIdentifier" (predicateIdentifier .>>. dollarDigitList) .>> IW |>> Ast.ReferencingIdentifier
let referenceToProofOrCorollary = positions "ReferenceToProofOrCorollary" referencingIdentifier |>> Ast.ReferenceToProofOrCorollary

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

let argumentIdentifier = positions "ArgumentIdentifier" (regex @"\w+\.") <?> "<argument identifier>" |>> Ast.ArgumentIdentifier
let refArgumentIdentifier = positions "RefArgumentIdentifier" (regex @"\w+") <?> "<refargument identifier>" |>> Ast.RefArgumentIdentifier
let justificationIdentifier = positions "JustificationIdentifier" (opt byModifier .>>. predicateIdentifier .>>. opt dollarDigitList .>>. opt (colon >>. refArgumentIdentifier)) |>> Ast.JustificationIdentifier
let byDef = positions "ByDef" (keywordByDef >>. SW >>. variable) |>> Ast.ByDef

let justificationReference = choice [
    attempt byDef
    justificationIdentifier
    refArgumentIdentifier
]

let twoPredicatesInParens = (leftParen >>. predicate) .>>. (comma >>. predicate) .>> rightParen 
let conjunction = positions "And" (keywordAnd >>. twoPredicatesInParens)  |>> Ast.And
let disjunction = positions "Or" (keywordOr >>. twoPredicatesInParens) |>> Ast.Or
let exclusiveOr = positions "Xor" (keywordXor >>. twoPredicatesInParens) |>> Ast.Xor
let implication = positions "Impl" (keywordImpl >>. twoPredicatesInParens) |>> Ast.Impl
let equivalence = positions "Iif" (keywordIif >>. twoPredicatesInParens) |>> Ast.Iif
let negation = positions "Not" (keywordNot >>. predicate) |>> Ast.Not

let all = positions "All" ((keywordAll >>. namedVariableDeclarationList) .>>. (leftBrace >>. predicate .>> rightBrace)) |>> Ast.All
let exists = positions "Exists" ((keywordEx >>. namedVariableDeclarationList) .>>. (leftBrace >>. predicate .>> rightBrace)) |>> Ast.Exists

let existsTimesN = positions "ExistsN" (((keywordExN >>. dollarDigits .>> SW) .>>. namedVariableDeclarationList) .>>. (leftBrace >>. predicate .>> rightBrace)) |>> Ast.ExistsN
let isOpArg = choice [ objectSymbol; predicateIdentifier; variable; selfOrParent ] .>> IW
let isOperator = positions "IsOperator" ((keywordIs >>. leftParen >>. isOpArg) .>>. (comma >>. variableType) .>> rightParen) |>> Ast.IsOperator

// infix operators like the equality operator 
let infixOp = positions "InfixOperator" ( infixMathSymbols ) .>> SW |>> Ast.InfixOperator

let pWithSep p separator =
    let combinedParser = pipe2 p (opt separator) (fun a b -> (a, b))
    combinedParser |> many

let infixOperation = positions "InfixOperation" (leftParen >>. pWithSep predicate infixOp .>> rightParen) |>> Ast.InfixOperation

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

let postfixOp = positions "PostfixOperator" ( postfixMathSymbols ) .>> IW |>> Ast.PostfixOperator
let prefixOp = positions "PrefixOperator" ( prefixMathSymbols ) .>> IW |>> Ast.PrefixOperator
let expression = positions "Expression" (opt prefixOp .>>. choice [compoundPredicate; primePredicate; mapCases] .>>. opt postfixOp .>>. optionalSpecification .>>. qualificationList) .>> IW |>> Ast.Expression

predicateRef.Value <- expression

predicateListRef.Value <- sepBy predicate comma



(* FPL building blocks *)
let keywordDeclaration = (skipString LiteralDecL <|> skipString LiteralDec) .>> SW 

let varDecl = tilde >>. namedVariableDeclaration
let varDeclBlock = positions "VarDeclBlock" (IW >>. keywordDeclaration >>. (many ((varDecl <|> statement) .>> IW)) .>> semiColon) .>> IW |>> Ast.VarDeclBlock 

let varDeclOrSpecList = opt (many1 (varDeclBlock)) 
let spacesPredicate = IW >>. predicate
let premiseList = positions "PremiseList" (IW >>. (keywordPremise >>. colon >>. predicateList)) |>> Ast.PremiseList
let conclusion = IW >>. (keywordConclusion >>. colon >>. predicate) 
let premiseConclusionBlock = positions "PremiseConclusionBlock" (leftBrace >>. varDeclOrSpecList .>>. premiseList .>>. conclusion .>> spacesRightBrace) |>> Ast.PremiseConclusionBlock

(* FPL building blocks - rules of reference *)
let keywordInference = (skipString LiteralInfL <|> skipString LiteralInf) .>> SW 
let ruleOfInferenceSignature = positions "RuleOfInferenceSignature" (keywordInference >>. pascalCaseId) .>> IW |>> Ast.RuleOfInferenceSignature
let ruleOfInference = positions "RuleOfInference" (ruleOfInferenceSignature .>>. premiseConclusionBlock) |>> Ast.RuleOfInference

(* FPL building blocks - Theorem-like statements and conjectures *)
let keywordTheorem = (skipString LiteralThmL <|> skipString LiteralThm) .>> SW
let keywordLemma = (skipString LiteralLemL <|> skipString LiteralLem) .>> SW
let keywordProposition = (skipString LiteralPropL <|> skipString LiteralProp) .>> SW
let keywordCorollary = (skipString LiteralCorL <|> skipString LiteralCor) .>> SW
let keywordConjecture = (skipString LiteralConjL <|> skipString LiteralConj) .>> SW

let theoremLikeBlock = leftBrace >>. varDeclOrSpecList .>>. spacesPredicate .>> spacesRightBrace

let theoremSignature = positions "TheoremSignature" (keywordTheorem >>. pascalCaseId) .>> IW |>> Ast.TheoremSignature
let theorem = positions "Theorem" (theoremSignature .>>. theoremLikeBlock) |>> Ast.Theorem
let lemmaSignature = positions "LemmaSignature" (keywordLemma >>. pascalCaseId) .>> IW |>> Ast.LemmaSignature
let lemma = positions "Lemma" (lemmaSignature .>>. theoremLikeBlock) |>> Ast.Lemma
let propositionSignature = positions "PropositionSignature" (keywordProposition >>. pascalCaseId) .>> IW |>> Ast.PropositionSignature
let proposition = positions "Proposition" (propositionSignature .>>. theoremLikeBlock) |>> Ast.Proposition
let conjectureSignature = positions "Conjecture" (keywordConjecture >>. pascalCaseId) .>> IW |>> Ast.ConjectureSignature
let conjecture = positions "Conjecture" (conjectureSignature .>>. theoremLikeBlock) |>> Ast.Conjecture
let corollarySignature = positions "proofSignature" (keywordCorollary >>. simpleSignature .>>. dollarDigitList) .>> IW |>> Ast.CorollarySignature
let corollary = positions "Corollary" (corollarySignature .>>. theoremLikeBlock) |>> Ast.Corollary

(* FPL building blocks - Axioms *)

let keywordAxiom = (skipString LiteralAxL <|> skipString LiteralAx <|> skipString LiteralPostL <|> skipString LiteralPost) >>. SW

let axiomSignature = positions "Axiom" (keywordAxiom >>. pascalCaseId) .>> IW |>> Ast.AxiomSignature
let axiom = positions "Axiom" (axiomSignature .>>. theoremLikeBlock) |>> Ast.Axiom

(* FPL building blocks - Constructors *)

let keywordIntrinsic = positions "Intrinsic" (skipString LiteralIntrL <|> skipString LiteralIntr) .>> IW |>> Ast.Intrinsic

let predContent = varDeclOrSpecList .>>. spacesPredicate |>> Ast.DefPredicateContent

let keywordConstructor = (skipString LiteralCtorL <|> skipString LiteralCtor) .>> SW
let constructorBlock = positions "ConstructorBlock" (leftBrace >>. varDeclOrSpecList .>> spacesRightBrace) |>> Ast.ConstructorBlock
let constructorSignature = positions "ConstructorSignature" (keywordConstructor >>. simpleSignature .>>. paramTuple) .>> IW |>> Ast.ConstructorSignature
let constructor = positions "Constructor" (constructorSignature .>>. constructorBlock) |>> Ast.Constructor

(* FPL building blocks - Properties *)
let keywordProperty = (skipString LiteralPrtyL <|> skipString LiteralPrty) .>> SW 

let predicateInstanceBlock = leftBrace >>. (keywordIntrinsic <|> predContent) .>> spacesRightBrace
let predicateInstanceSignature = positions "PredicateInstanceSignature" (keywordPredicate >>. SW >>. simpleSignature .>>. paramTuple) .>> IW |>> Ast.PredicateInstanceSignature
let predicateInstance = positions "PredicateInstance" (keywordProperty >>. predicateInstanceSignature .>>. predicateInstanceBlock) |>> Ast.PredicateInstance

mappingRef.Value <- toArrow >>. IW >>. positions "Mapping" (variableType) |>> Ast.Mapping

let returnStatement = positions "Return" (keywordReturn >>. predicate) .>> IW |>> Ast.Return
let funcContent = varDeclOrSpecList .>>. returnStatement |>> Ast.DefFunctionContent
let functionalTermInstanceBlock = leftBrace >>. (keywordIntrinsic <|> funcContent) .>> spacesRightBrace
let functionalTermInstanceSignature = positions "FunctionalTermInstanceSignature" (keywordFunction >>. SW >>. simpleSignature .>>. paramTuple .>>. (IW >>. mapping)) .>> IW |>> Ast.FunctionalTermInstanceSignature
let functionalTermInstance = positions "FunctionalTermInstance" (keywordProperty >>. functionalTermInstanceSignature .>>. functionalTermInstanceBlock) |>> Ast.FunctionalTermInstance


let extensionRegex = regex "[^\/]+" <?> "<extension regex>" |>> Ast.ExtensionRegex

let extensionAssignment = positions "ExtensionAssignment" ((variable .>> IW .>> at .>> IW) .>>. (slash >>. extensionRegex .>> slash)) |>> Ast.ExtensionAssignment

let extensionSignature = positions "ExtensionSignature" ((extensionAssignment .>> IW) .>>. mapping) .>> IW |>> Ast.ExtensionSignature
let extensionTerm = leftBrace >>. (funcContent <|> mapCases) .>> spacesRightBrace
let definitionExtension = positions "DefinitionExtension" (keywordExtension >>. (extensionName .>> SW) .>>. extensionSignature .>>. extensionTerm) |>> Ast.DefinitionExtension

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
let revokeArgument = positions "RevokeArgument" (keywordRevoke >>. refArgumentIdentifier) |>> Ast.RevokeArgument 
    
let keywordAssume = skipString LiteralAssL <|> skipString LiteralAss .>> SW 
let assumeArgument = positions "AssumeArgument" (keywordAssume >>. predicate) |>> Ast.AssumeArgument
let keywordTrivial  = positions "Trivial" (skipString LiteralTrivial) .>> IW |>> Ast.Trivial
let keywordQed  = positions "Qed" (skipString LiteralQed) .>> IW |>> Ast.Qed
let derivedPredicate = positions "DerivedPredicate" predicate |>> Ast.DerivedPredicate
let derivedArgument = choice [
    keywordTrivial 
    derivedPredicate
]

let argumentInference = vDash >>. IW >>. (assumeArgument <|> revokeArgument <|> derivedArgument)
let justificationItem = positions "JustificationItem" justificationReference |>> Ast.JustificationItem
let justificationItemList = sepBy justificationItem comma
let justification = positions "Justification" (justificationItemList .>> IW) |>> Ast.Justification
let justifiedArgument = positions "JustArgInf" (justification .>>. argumentInference) |>> Ast.JustArgInf
let proofArgument = positions "Argument" ((argumentIdentifier .>> IW) .>>. justifiedArgument) .>> IW |>> Ast.Argument
let proofArgumentList = many1 (IW >>. (proofArgument <|> varDeclBlock))
let keywordProof = (skipString LiteralPrfL <|> skipString LiteralPrf) .>> SW 
let proofBlock = leftBrace >>. proofArgumentList .>>. opt keywordQed .>> spacesRightBrace
let proofSignature = positions "proofSignature" (keywordProof >>. simpleSignature .>>. dollarDigitList) .>> IW |>> Ast.ProofSignature

let proof = positions "Proof" (proofSignature .>>. proofBlock) |>> Ast.Proof

(* FPL building blocks - Definitions *)

// Predicate building blocks can be defined similarly to classes, they can have properties but they cannot be derived any parent type 
let predicateDefinitionBlock = opt (leftBrace  >>. ((keywordIntrinsic <|> predContent) .>> IW) .>>. propertyList .>> spacesRightBrace)
let predicateSignature = positions "PredicateSignature" (keywordPredicate >>. SW >>. simpleSignature .>>. paramTuple) .>>. userDefinedSymbol .>> IW |>> Ast.PredicateSignature
let definitionPredicate = positions "DefinitionPredicate" (predicateSignature .>>. predicateDefinitionBlock) |>> Ast.DefinitionPredicate

// Functional term building blocks can be defined similarly to classes, they can have properties but they cannot be derived any parent type 
let functionalTermDefinitionBlock = positions "FunctionalTermDefinitionBlock" (opt (leftBrace  >>. ((keywordIntrinsic <|> funcContent) .>> IW) .>>. propertyList .>> spacesRightBrace))  |>> Ast.FunctionalTermDefinitionBlock

let inheritedFunctionalType = predicateIdentifier
let inheritedFunctionalTypeList = sepBy1 (inheritedFunctionalType) (attempt (IW >>. comma)) |>> Ast.InheritedFunctionalTypeList
let functionalTermSignature = positions "FunctionalTermSignature" (keywordFunction >>. SW >>. (simpleSignature .>>. opt (colon >>. inheritedFunctionalTypeList) .>> IW) .>>. paramTuple .>>. (IW >>. mapping)) .>>. userDefinedSymbol .>> IW |>> Ast.FunctionalTermSignature
let definitionFunctionalTerm = positions "DefinitionFunctionalTerm" (functionalTermSignature .>>. functionalTermDefinitionBlock) |>> Ast.DefinitionFunctionalTerm

// Class definitions
let keywordClass = (skipString LiteralClL <|> skipString LiteralCl)

let constructorList = many1 (constructor .>> IW)
let classCompleteContent = varDeclOrSpecList .>>. constructorList|>> Ast.DefClassCompleteContent
let classDefinitionBlock = positions "ClassDefinitionBlock" (opt (leftBrace  >>. ((keywordIntrinsic <|> classCompleteContent) .>> IW) .>>. propertyList .>> spacesRightBrace)) |>> Ast.ClassDefinitionBlock
let inheritedClassTypeList = sepBy1 (inheritedClassType) (attempt (IW >>. comma)) |>> Ast.InheritedClassTypeList

let classSignature = positions "ClassSignature" (keywordClass >>. SW >>. pascalCaseId) .>> IW |>> Ast.ClassSignature
let classSignatureExtended = classSignature .>>. opt (colon >>. inheritedClassTypeList) .>>. opt (attempt (IW >>. userDefinedObjSym)) .>> IW
let definitionClass = positions "DefinitionClass" (classSignatureExtended .>>. classDefinitionBlock) |>> Ast.DefinitionClass 

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
let localizationLanguageCode = positions "LanguageCode" (regex @"[a-z]{3}" <?> "<ISO 639 language code>") |>> Ast.LanguageCode

let ebnfTransl, ebnfTranslRef = createParserForwardedToRef()
let ebnfTranslTuple = (leftParen >>. IW >>. ebnfTransl) .>> (IW .>> rightParen) 
let ebnfFactor = choice [
    variable
    quote >>. localizationString .>> quote
    ebnfTranslTuple
] 
let ebnfTerm = positions "TranslationTerm" (sepEndBy1 ebnfFactor SW) |>> Ast.TranslationTerm
ebnfTranslRef.Value <-  positions "TranslationTermList" (sepBy1 ebnfTerm (IW >>. case >>. IW)) |>> Ast.TranslationTermList
let language = positions "Language" ((exclamationMark >>. localizationLanguageCode .>> IW .>> colon) .>>. ebnfTransl) |>> Ast.Language
let languageList = many1 (IW >>. language .>> IW)
let localization = positions "Localization" (keywordLocalization >>. predicate) .>> (IW .>> colonEqual) .>>. (languageList .>> IW .>> semiColon) .>> IW |>> Ast.Localization

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
let fplNamespace = buildingBlockList .>> IW .>> semiColon |>> Ast.Namespace
(* Final Parser *)
let ast =  positions "AST" (IW >>. fplNamespace) |>> Ast.AST
let stdParser = ast
let stdCode = SYN000
let stdCode1 = SYN001
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
    let preProcessedInput = preParsePreProcess input
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
    let newInput = preParsePreProcess input
    match run ast (newInput.Substring(0, index)) with
    | Success(result, restInput, userState) -> 
        // In the success case, we always return the current parser position in the input
        List.empty, userState.Index
    | Failure(errorMsg, restInput, userState) ->
        let newErrMsg, choices = mapErrMsgToRecText input errorMsg restInput.Position
        choices, restInput.Position.Index

/// Used to test FplLS CompletionItems correct syntax
let testParser (parserType:string) (input:string) =
    let trimmed = preParsePreProcess (input.Trim())
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