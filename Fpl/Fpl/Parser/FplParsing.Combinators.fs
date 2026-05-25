/// This module contains the FPL parser conbinators producing an abstract syntax tree out of a given FPL code.
module FplParsing.Combinators
open System.Text.RegularExpressions
open FParsec
open FplPrimitives
open FplGrammarTypes

(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)

// A simple helper function for printing trace information to the console (taken from FParsec Docs)
let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
    fun stream ->
        printfn "%A: Entering %s" stream.Position label
        let reply = p stream
        printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
        reply

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
let rightBrace = skipChar '}'
let leftParen = skipChar '(' >>. spaces 
let rightParen = skipChar ')' 
let comma = skipChar ',' >>. spaces 
let dot = skipChar '.' |>> Ast.Dot
let colon = skipChar ':' .>> spaces 
let colonEqual = skipString ":=" >>. spaces 
let at = pchar '@'
let case = skipChar '|' >>. spaces
let elseCase = skipChar '?' >>. spaces
let leftBracket = skipChar '[' >>. spaces 
let rightBracket = skipChar ']' 
//let tilde = skipChar '~' .>> spaces
let semiColon = skipChar ';' .>> spaces 
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
let pascalCaseId = positions (idStartsWithCap) |>> Ast.PascalCaseId

let namespaceIdentifier = positions (sepBy1 pascalCaseId dot) .>> IW |>> Ast.NamespaceIdentifier

let predicateIdentifier = positions (idStartsWithCap) |>> Ast.PredicateIdentifier 

let alias = positions (skipString LiteralAlias >>. SW >>. idStartsWithCap) |>> Ast.Alias
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

/// Taken from https://www.quanttec.com/fparsec/users-guide/looking-ahead-and-backtracking.html#parser-predicates
let resultSatisfies predicate msg (p: Parser<_,_>) : Parser<_,_> =
    let error = messageError msg
    fun stream ->
        let state = stream.State
        let reply = p stream
        if reply.Status <> Ok || predicate reply.Result then reply
        else
            stream.BacktrackTo(state) // backtrack to beginning
            Reply(Primitives.Error, error)

let variableX: Parser<string,unit> = 
    IdStartsWithSmallCase 
    <?> "<variable>" 
    |> resultSatisfies (fun s -> keyWordSet.Contains(s) |> not) "<variable> (got <keyword>)>" 
    |> resultSatisfies (fun s -> tplRegex.IsMatch(s) |> not) "<variable> (got <template>)>"
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

let templateWithTail = (many1Strings2 (pstring LiteralTplL <|> pstring LiteralTpl) templateTail) 

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

let coord = choice [ predicateWithQualification; dollarDigits ] .>> IW 

// infix operators like the equality operator 
let objectSymbol = positions ( objectMathSymbols ) |>> Ast.ObjectSymbol

let fplIdentifier = choice [ selfOrParent ; variable ; predicateIdentifier; extension; objectSymbol ] 

let coordList = (sepBy1 coord comma) .>> IW

let bracketedCoords = positions (leftBracket >>. coordList .>> rightBracket) |>> Ast.BrackedCoordList

let namedVariableDeclarationList, namedVariableDeclarationListRef = createParserForwardedToRef()

let keywordExtension = (skipString LiteralExtL <|> skipString LiteralExt) .>> SW

let extensionName = positions (idStartsWithCap) .>> SW |>> Ast.ExtensionName

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
let arrayType = positions (star >>. IW >>. simpleVariableType .>>. (IW >>. leftBracket >>. indexAllowedTypeList .>> rightBracket)) |>> Ast.ArrayType
let variableType = choice [ simpleVariableType; arrayType ]

let namedVariableDeclaration = positions ((variableList .>> colon) .>>. variableType .>> IW) |>> Ast.NamedVarDecl
namedVariableDeclarationListRef.Value <- sepBy namedVariableDeclaration comma

paramTupleRef.Value <- (leftParen >>. namedVariableDeclarationList) .>> (IW >>. rightParen) |>> Ast.ParamTuple

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
let argumentTuple = positions ((leftParen >>. predicateList) .>> (IW >>. rightParen)) |>> Ast.ArgumentTuple 

let delegateName = positions (idStartsWithCap) .>> IW |>> Ast.DelegateName

let fplDelegate = keywordDel >>. (dot >>. delegateName .>>. argumentTuple .>> IW) |>> Ast.Delegate

let spacesRightBrace = (IW >>. rightBrace) 

let keywordReturn = IW >>. (skipString LiteralRetL <|> skipString LiteralRet) .>> SW 



let caseElse = positions (elseCase >>. IW >>. statementList .>> IW)  |>> Ast.CaseElse
let caseSingle = positions ((case >>. predicate .>> colon) .>>. statementList) |>> Ast.CaseSingle
let caseSingleList = many1 (IW >>. caseSingle)
let casesStatement = positions (((keywordCases >>. leftParen >>. IW >>. caseSingleList .>>. caseElse .>> rightParen))) |>> Ast.Cases

let mapCaseElse = positions (elseCase >>. predicate .>> IW) |>> Ast.MapCaseElse
let mapCaseSingle = positions ((case >>. predicate .>> colon) .>>. (IW >>. predicate)) |>> Ast.MapCaseSingle
let mapCaseSingleList = many1 (IW >>. mapCaseSingle)
let mapCases = positions (((keywordMapCases >>. leftParen >>. IW >>. mapCaseSingleList .>>. mapCaseElse .>> rightParen))) |>> Ast.MapCases

let assignmentStatement = positions ((predicateWithQualification .>> IW .>> colonEqual) .>>. predicate) |>> Ast.Assignment

let inEntity = keywordIn >>. positions (predicateWithQualification) .>> IW |>> Ast.InEntity

let entityInDomain = ( variable .>> IW .>>. inEntity ) .>> IW
let forInBody = (entityInDomain .>> IW) .>>. (leftBrace >>. statementList) .>> IW .>> rightBrace
let forStatement = positions (keywordFor >>. forInBody) |>> Ast.ForIn

//// Difference of assertion to an axiom: axiom is named predicate, while an assertion uses a predicated to assert it.
//// Difference of assertion to a mandatory property: a mandatory property introduces a completely new identifier inside
//// the scope of a definition. An assertion uses a predicate referring to existing identifiers in the whole theory
//// Difference of assertion to assume: the latter will be used only in the scope of proofs
let assertionStatement = positions (keywordAssert >>. predicate) |>> Ast.Assertion


let baseClassName = positions (idStartsWithCap) .>> IW |>> Ast.BaseClassName

let baseConstructorCall = positions (keywordBaseClassReference >>. dot >>. baseClassName .>>. argumentTuple .>> IW) |>> Ast.BaseConstructorCall

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

let argIdX: Parser<string,unit> = 
    regex @"\w+" <?> "<argument ID>" 
    |> resultSatisfies (fun s -> keyWordSet.Contains(s) |> not) "<argument ID> (got <keyword>)" 
    |> resultSatisfies (fun s -> tplRegex.IsMatch(s) |> not) "<argument ID> (got <template>)"
    >>= (fun s -> preturn s) 

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

let argumentIdentifierDotted = positions (argIdDottedX) |>> Ast.ArgumentIdentifier
let argumentIdentifierColon = positions (argIdColonX) |>> Ast.ArgumentIdentifier
let refArgumentIdentifier = positions argIdX |>> Ast.RefArgumentIdentifier
let justificationIdentifier = positions (opt byModifier .>>. predicateIdentifier .>>. opt dollarDigitList .>>. opt (colon >>. refArgumentIdentifier)) |>> Ast.JustificationIdentifier
let byDef = positions (keywordByDef >>. SW >>. variable) |>> Ast.ByDef

let justificationItem = positions (choice [attempt byDef ; justificationIdentifier ; refArgumentIdentifier ]) |>> Ast.JustificationItem

let twoPredicatesInParens = (leftParen >>. predicate) .>>. (comma >>. predicate) .>> rightParen 
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

let all = positions ((keywordAll >>. namedVariableDeclarationList) .>>. (leftBrace >>. predicate .>> rightBrace)) |>> Ast.All
let exists = positions ((keywordEx >>. namedVariableDeclarationList) .>>. (leftBrace >>. predicate .>> rightBrace)) |>> Ast.Exists

let existsNTimes = choice [
        attempt (keywordExNSymbolic .>> SW) |>> Ast.Exists1 
        keywordExNSymbolic >>. positions puint32 .>> SW |>> Ast.DollarDigits 
    ] 

let existsTimeNQuantifier = choice [
    (keywordExN >>. dollarDigits .>> SW)
    existsNTimes
]

let existsTimesN = positions ((existsTimeNQuantifier .>>. namedVariableDeclarationList) .>>. (leftBrace >>. predicate .>> rightBrace)) |>> Ast.ExistsN
let isOp = choice [
    attempt (dot >>. (predicate .>> keywordIs) .>>. variableType) 
    (keywordIs >>. leftParen >>. predicate .>> IW) .>>. (comma >>. variableType) .>> rightParen
    ]
let isOperator = positions isOp |>> Ast.IsOperator

// infix operators like the equality operator 
let infixOp = positions ( infixMathSymbols ) .>> attemptSW |>> Ast.InfixOperator

let pWithSep p separator =
    let combinedParser = pipe2 p (opt separator) (fun a b -> (a, b))
    combinedParser |> many

let infixOperation = positions (leftParen >>. pWithSep predicate infixOp .>> rightParen) |>> Ast.InfixOperation

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
let varDeclBlock = (attempt statement <|> varDecl) .>> IW 

let varDeclOrSpecList = IW >>. opt (keywordDeclaration >>. many1 varDeclBlock .>> semiColon) .>> IW |>> Ast.VarDeclBlock 

let spacesPredicate = IW >>. predicate
let premiseList = positions (IW >>. (keywordPremise >>. colon >>. predicateList)) |>> Ast.PremiseList
let conclusion = IW >>. (keywordConclusion >>. colon >>. predicate)
let insideRuleOfOnference = (varDeclOrSpecList .>>. (premiseList .>>. conclusion))
let premiseConclusionBlock = leftBrace >>. insideRuleOfOnference .>> spacesRightBrace |>> Ast.PremiseConclusionBlock

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

let theoremLikeBlock = leftBrace >>. (varDeclOrSpecList .>>. spacesPredicate) .>> spacesRightBrace

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
let constructorBlock = leftBrace >>. varDeclOrSpecList .>> spacesRightBrace |>> Ast.ConstructorBlock
let constructorSignature = positions (keywordConstructor >>. simpleSignature .>>. paramTuple) .>> IW |>> Ast.ConstructorSignature
let constructor = positions (constructorSignature .>>. constructorBlock) |>> Ast.Constructor

(* FPL building blocks - Properties *)
let keywordProperty = (skipString LiteralPrtyL <|> skipString LiteralPrty) .>> SW 

let predicateInstanceBlock = opt (leftBrace >>. (keywordIntrinsic <|> predContent) .>> spacesRightBrace)
let predicateInstanceSignature = positions (keywordPredicate >>. SW >>. simpleSignature .>>. paramTuple) .>> IW |>> Ast.PredicateInstanceSignature
let predicateInstance = positions (keywordProperty >>. predicateInstanceSignature .>>. predicateInstanceBlock) |>> Ast.PredicateInstance

mappingRef.Value <- toArrow >>. IW >>. positions (keywordUndefined <|> variableType) |>> Ast.Mapping

let returnStatement = positions (keywordReturn >>. predicate) .>> IW |>> Ast.Return
let funcContent = varDeclOrSpecList .>>. returnStatement |>> Ast.DefFunctionContent
let functionalTermInstanceBlock = opt (leftBrace >>. (keywordIntrinsic <|> funcContent) .>> spacesRightBrace)
let functionalTermInstanceSignature = positions (keywordFunction >>. SW >>. simpleSignature .>>. paramTuple .>>. (IW >>. mapping)) .>> IW |>> Ast.FunctionalTermInstanceSignature
let functionalTermInstance = positions (keywordProperty >>. functionalTermInstanceSignature .>>. functionalTermInstanceBlock) |>> Ast.FunctionalTermInstance


let extensionRegex = regex "[^\/]+" <?> "<extension regex>" |>> Ast.ExtensionRegex

let extensionAssignment = positions ((variable .>> IW .>> at .>> IW) .>>. (slash >>. extensionRegex .>> slash)) |>> Ast.ExtensionAssignment

let extensionSignature = positions ((extensionAssignment .>> IW) .>>. mapping) .>> IW |>> Ast.ExtensionSignature
let extensionTerm = leftBrace >>. ((funcContent <|> mapCases) .>> spacesRightBrace)
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

let argumentInference = (assumeArgument <|> revokeArgument <|> derivedArgument)
let justificationItemList = sepBy1 justificationItem comma

let proofArgumentBeginningStrict = (argumentIdentifierDotted .>> IW) .>>. (justificationItemList .>> IW .>> vDash .>> IW) |>> Ast.StartArgumentStictly
let proofArgumentBeginningNoJust = (argumentIdentifierColon .>> IW) |>> Ast.StartArgument

let justification = positions (choice [proofArgumentBeginningStrict; proofArgumentBeginningNoJust]) |>> Ast.Justification
let justifiedArgument = positions (justification .>>. argumentInference) |>> Ast.JustArgInf

let proofArgument = positions (justifiedArgument) .>> IW |>> Ast.Argument
let proofArgumentList = many1 (IW >>. proofArgument)
let keywordProof = (skipString LiteralPrfL <|> skipString LiteralPrf) .>> SW
let proofContent = varDeclOrSpecList .>>. proofArgumentList .>>. opt keywordQed |>> Ast.ProofContent
let proofBlock = leftBrace >>. proofContent .>> spacesRightBrace |>> Ast.ProofBlock
let proofSignature = positions (keywordProof >>. simpleSignature .>>. dollarDigitList) .>> IW |>> Ast.ProofSignature

let proof = positions (proofSignature .>>. proofBlock) |>> Ast.Proof

(* FPL building blocks - Definitions *)

// Predicate building blocks can be defined similarly to classes, they can have properties but they cannot be derived any parent type
let predicateDefinitionBlock = opt (leftBrace  >>. ((keywordIntrinsic <|> predContent) .>> IW) .>>. propertyList .>> spacesRightBrace)
let inheritedType = positions (opt idStartsWithCap) .>> IW |>> Ast.InheritedType 
let inheritedTypeList = sepBy1 inheritedType comma |>> Ast.InheritedTypeList
let predicateSignature = positions (keywordPredicate >>. SW >>. (simpleSignature .>>. opt (colon >>. inheritedTypeList) .>> IW) .>>. paramTuple) .>>. userDefinedSymbol .>> IW |>> Ast.PredicateSignature
let definitionPredicate = positions (predicateSignature .>>. predicateDefinitionBlock) |>> Ast.DefinitionPredicate

// Functional term building blocks can be defined similarly to classes, they can have properties but they cannot be derived any parent type 
let functionalTermDefinitionBlock = positions (opt (leftBrace  >>. ((keywordIntrinsic <|> funcContent) .>> IW) .>>. propertyList .>> spacesRightBrace))  |>> Ast.FunctionalTermDefinitionBlock

let functionalTermSignature = positions (keywordFunction >>. SW >>. (simpleSignature .>>. opt (colon >>. inheritedTypeList) .>> IW) .>>. paramTuple .>>. (IW >>. mapping)) .>>. userDefinedSymbol .>> IW |>> Ast.FunctionalTermSignature
let definitionFunctionalTerm = positions (functionalTermSignature .>>. functionalTermDefinitionBlock) |>> Ast.DefinitionFunctionalTerm

// Class definitions
let keywordClass = (skipString LiteralClL <|> skipString LiteralCl)

let constructorList = many1 (constructor .>> IW)
let classCompleteContent = varDeclOrSpecList .>>. constructorList |>> Ast.DefClassCompleteContent
let classDefinitionBlock = positions (opt (leftBrace  >>. ((keywordIntrinsic <|> classCompleteContent) .>> IW) .>>. propertyList .>> spacesRightBrace)) |>> Ast.ClassDefinitionBlock

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
let ebnfTranslTuple = (leftParen >>. IW >>. ebnfTransl) .>> (IW .>> rightParen) 
let ebnfFactor = choice [
    variable
    quote >>. localizationString .>> quote
    ebnfTranslTuple
] 
let ebnfTerm = positions (sepEndBy1 ebnfFactor SW) |>> Ast.TranslationTerm
ebnfTranslRef.Value <-  positions (sepBy1 ebnfTerm (IW >>. case >>. IW)) |>> Ast.TranslationTermList
let language = positions ((exclamationMark >>. localizationLanguageCode .>> IW .>> colon) .>>. ebnfTransl) |>> Ast.Language
let languageList = many1 (IW >>. language .>> IW)
let localization = positions (keywordLocalization >>. predicate) .>> (IW .>> colonEqual) .>>. (languageList .>> (IW .>> semiColon)) .>> IW |>> Ast.Localization

// FPL building blocks can be definitions, axioms, Theorem-proof blocks and conjectures
let buildingBlock = positions(choice [definition; axiom; theorem; lemma; proposition; corollary; conjecture; proof; ruleOfInference; localization; usesClause; definitionExtension]) .>> IW |>> Ast.BuildingBlock

(* Namespaces *)
let fplNamespace = many buildingBlock |>> Ast.Namespace

(* Final Parser *)
let stdParser = positions (IW >>. fplNamespace) |>> Ast.AST
