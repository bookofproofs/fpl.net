/// This module contains the FPL parser conbinators producing an abstract syntax tree out of a given FPL code.
module FplParsing.Combinators

open FParsec
open FplParsing.Basic
open FplPrimitives
open FplGrammarTypes
open FplParsing.Debug

(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)

// -----------------------------------------------------
// Extensions of the FPL language allow syntax injections as long as they match the following regex expression.
// The FPL interpreter will try to match extensionString after the @ Literal
// by tryoing out the regex expressions of all user-declared ExtensionBlocks (in their declaration order)
// until none or the first of then matches this string. Then, the matched string will get the named type of the ExtensionBlock.
let extensionString = regex @"[^,;\s()\[\]{}\:]+" <?> "<extensionString>" 
let extension = positions (at >>. extensionString) |>> Ast.Extension <!> "Extension"

(* Identifiers *)


let IdStartsWithSmallCase = regex @"[a-z]\w*" 
let idStartsWithCap = (regex @"[A-Z]\w*") <?> "<PascalCaseId>"
let pascalCaseId = positions (idStartsWithCap) |>> Ast.PascalCaseId <!> "PascalCaseId"

let namespaceIdentifier = positions (sepBy1 pascalCaseId dot) .>> IW |>> Ast.NamespaceIdentifier <!> "NamespaceIdentifier"

let predicateIdentifier = positions (idStartsWithCap) |>> Ast.PredicateIdentifier <!> "PredicateIdentifier" 

let alias = positions (skipString LiteralAlias >>. SW >>. idStartsWithCap) |>> Ast.Alias <!> "Alias"
let star = positions (skipChar '*') |>> Ast.Star <!> "Star"

let aliasedNamespaceIdentifier = positions (namespaceIdentifier .>>. opt (alias <|> star)) |>> Ast.AliasedNamespaceIdentifier <!> "AliasedNamespaceIdentifier"




let variableX: Parser<string,unit> = 
    IdStartsWithSmallCase 
    <?> "<variable>" 
    |> resultSatisfies (fun s -> keyWordSet.Contains(s) |> not) "<variable> (got <keyword>)>" 
    |> resultSatisfies (fun s -> tplRegex.IsMatch(s) |> not) "<variable> (got <template>)>"
    >>= (fun s -> preturn s) 

let variable = positions variableX |>> Ast.Var <!> "Var" 

let variableList = (sepBy1 variable comma) .>> IW

let keywordSelf = positions (skipString LiteralSelf) .>> IW |>> Ast.Self <!> "Self"
let keywordParent = positions (skipString LiteralParent) .>> IW |>> Ast.Parent <!> "Parent"
let keywordBaseClassReference = skipString LiteralBase .>> IW
let keywordIndex = positions (skipString LiteralIndL <|> skipString LiteralInd) |>> Ast.IndexType <!> "IndexType"

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
let keywordUndefined = positions (skipString LiteralUndefL <|> skipString LiteralUndef) |>> Ast.Undefined <!> "Undefined"
let keywordTrue = positions (skipString LiteralTrue) |>> Ast.True <!> "True"  
let keywordFalse = positions (skipString LiteralFalse) |>>  Ast.False  
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
let keywordIs = skipString LiteralIs 


// Via templates, FPL supports generic types, which make it possible to define abstract mathematical
// objects and their properties that defer the concrete
// specification of one or more types until the definition or method is declared and instantiated by
// client code
let keywordTemplate = (pstring LiteralTplL <|> pstring LiteralTpl) 

let templateTail = choice [ idStartsWithCap; (regex @"\d+") ]

let templateWithTail = (many1Strings2 (pstring LiteralTplL <|> pstring LiteralTpl) templateTail) 

let keywordObject = positions (skipString LiteralObjL <|> skipString LiteralObj) |>> Ast.ObjectType <!> "ObjectType" 

let templateType = positions ((attempt templateWithTail) <|> keywordTemplate) |>>  Ast.TemplateType

let keywordPredicate = positions (skipString LiteralPredL <|> skipString LiteralPred) |>> Ast.PredicateType <!> "PredicateType"
let keywordFunction = positions (skipString LiteralFuncL <|> skipString LiteralFunc) |>> Ast.FunctionalTermType <!> "FunctionalTermType"


let theoryNamespace = aliasedNamespaceIdentifier <|> namespaceIdentifier .>> IW

let keywordUses = (skipString LiteralUses) .>> SW
let usesClause = positions (keywordUses >>. theoryNamespace) |>> Ast.UsesClause <!> "UsesClause"

(* Signatures, Variable Declarations, and Types, and Coordinates *)
// convention: All syntax production rules of FPL syntax extensions have to start with LiteralExt, followed by
// a Pascal Case id.
// This ensures that they will not be mixed-up with original FPL ebnf productions
// that are all PascalCase as well as FPL keywords, that are all small case.

let dollarDigits = positions (regex "\$" >>. puint32 <?> "<dollarDigits>") |>> Ast.DollarDigits <!> "DollarDigits"

let selfOrParent = positions (choice [keywordSelf ; keywordParent]) |>> Ast.SelfOrParent <!> "SelfOrParent"

////// resolving recursive parsers
let statementList, statementListRef = createParserForwardedToRef()
let primePredicate, primePredicateRef = createParserForwardedToRef()
let predicate, predicateRef = createParserForwardedToRef()
let predicateList, predicateListRef = createParserForwardedToRef()
let predicateWithQualification, predicateWithQualificationRef = createParserForwardedToRef()
let paramTuple, paramTupleRef = createParserForwardedToRef()



// infix operators like the equality operator 
let objectSymbol = positions ( objectMathSymbols ) |>> Ast.ObjectSymbolWithPos <!> "ObjectSymbol"

let fplIdentifier = choice [ selfOrParent ; variable ; predicateIdentifier; extension; objectSymbol ] 


let pExprList : Parser<Ast list,unit> =
    (pipe2
        predicate
        (many (attempt (comma >>. predicate)))
        (fun first rest -> first :: rest))
    <|> preturn [] <!> "pExprList"


let pCoords : Parser<Ast list,unit> =
    leftBracket >>. pExprList .>> rightBracket 

let bracketedCoords = positions pCoords |>> Ast.BrackedCoordList <!> "bracketedCoords"

let namedVariableDeclarationList, namedVariableDeclarationListRef = createParserForwardedToRef()

let keywordExtension = (skipString LiteralExtL <|> skipString LiteralExt) .>> SW

let extensionName = positions (idStartsWithCap) .>> SW |>> Ast.ExtensionName <!> "ExtensionName"

// The classType is the last type in FPL we can derive FPL classes from.
// It therefore excludes the in-built FPL-types keywordPredicate, keywordFunction, and keywordIndex
// to restrict it to pure objects.
// In contrast to variableType which can also be used for declaring variables 
// in the scope of FPL building blocks

let mapping, mappingRef = createParserForwardedToRef()
let predicateType = positions (keywordPredicate .>>. opt paramTuple) |>> Ast.CompoundPredicateType <!> "CompoundPredicateType"
let functionalTermType = positions (keywordFunction .>>. opt (paramTuple .>>. (IW >>. mapping))) |>> Ast.CompoundFunctionalTermType <!> "CompoundFunctionalTermType"

let simpleVariableType = positions (choice [ keywordIndex; keywordObject; predicateIdentifier; templateType; functionalTermType; predicateType ]) |>> Ast.SimpleVariableType <!> "SimpleVariableType"
// indexAllowedType is used to restrict Fpl types allowed to be used as indexes in arrayType
let indexAllowedType = positions (choice [ keywordIndex; keywordObject; predicateIdentifier; templateType; keywordPredicate; keywordFunction]) |>> Ast.IndexAllowedType <!> "IndexAllowedType"

let indexAllowedTypeList = (sepBy1 indexAllowedType comma) .>> IW
// arrayType is used to define arrays in Fpl
let arrayType = positions (star >>. IW >>. simpleVariableType .>>. (IW >>. leftBracket >>. indexAllowedTypeList .>> rightBracket)) |>> Ast.ArrayType <!> "ArrayType"
let variableType = choice [ simpleVariableType; arrayType ]

let namedVariableDeclaration = positions ((variableList .>> colon) .>>. variableType .>> IW) |>> Ast.NamedVarDecl <!> "NamedVarDecl"
namedVariableDeclarationListRef.Value <- sepBy namedVariableDeclaration comma

paramTupleRef.Value <- (leftParen >>. namedVariableDeclarationList) .>> rightParen |>> Ast.ParamTuple <!> "ParamTuple"

let simpleSignature = pascalCaseId .>> IW 

let localizationString = positions (regex "[^\"\n]*") <?> "<language-specific string>" |>> Ast.LocalizationString <!> "LocalizationString"

let keywordSymbol = pstring LiteralSymbol .>> IW
let objectSymbolString = pchar '"' >>. objectMathSymbols .>> pchar '"'
let infixString = pchar '"' >>. infixMathSymbols .>> pchar '"'
let keywordInfix = pstring LiteralInfix >>. IW
let postfixString = pchar '"' >>. postfixMathSymbols .>> pchar '"' 
let keywordPostfix = pstring LiteralPostFix >>. IW
let prefixString = pchar '"' >>. prefixMathSymbols .>> pchar '"' 
let keywordPrefix = pstring LiteralPrefix >>. IW
let userDefinedObjSym = positions (keywordSymbol >>. objectSymbolString) .>> IW |>> Ast.SymbolDecl <!> "Symbol"
let precedence = positions (pint32) .>> IW |>> Ast.Precedence <!> "Precedence"

let userDefinedInfix = positions (keywordInfix >>. (infixString .>>. (IW >>. precedence))) .>> IW |>> Ast.InfixDeclWithPrecedence <!> "Infix"
let userDefinedPostfix = positions (keywordPostfix >>. postfixString) .>> IW |>> Ast.PostfixDecl <!> "Postfix"
let userDefinedPrefix = positions (keywordPrefix >>. prefixString) .>> IW |>> Ast.PrefixDecl <!> "Prefix"
let userDefinedSymbol = opt (attempt (IW >>. choice [userDefinedPrefix; userDefinedInfix; userDefinedPostfix ]))

(* Statements *)
let pArgs : Parser<Ast list,unit> =
    leftParen >>. pExprList .>> rightParen <!> "pArgs"

let argumentTuple = positions pArgs |>> Ast.ArgumentTuple <!> "ArgumentTuple" 

let delegateName = positions (idStartsWithCap) .>> IW |>> Ast.DelegateName <!> "DelegateName"


let fplDelegate = keywordDel >>. (dot >>. delegateName .>>. argumentTuple) .>> IW |>> Ast.Delegate <!> "Delegate"


let keywordReturn = IW >>. (skipString LiteralRetL <|> skipString LiteralRet) .>> SW 



let caseElse = positions (IW >>. elseCase >>. IW >>. statementList)  |>> Ast.CaseElse <!> "CaseElse"
let caseSingle = positions ((case >>. predicate .>> IW .>> colon) .>>. statementList) .>> IW |>> Ast.CaseSingle <!> "CaseSingle"
let caseSingleList = many1 caseSingle
let casesStatement = positions (((keywordCases >>. leftParen >>. IW >>. caseSingleList .>>. caseElse .>> rightParen))) |>> Ast.Cases <!> "Cases"

let mapCaseElse = positions (IW >>. elseCase >>. predicate) |>> Ast.MapCaseElse <!> "MapCaseElse"
let mapCaseSingle = positions ((case >>. predicate .>> IW .>> colon) .>>. (IW >>. predicate)) .>> IW |>> Ast.MapCaseSingle <!> "MapCaseSingle"
let mapCaseSingleList = many1 mapCaseSingle
let mapCases = positions (((keywordMapCases >>. leftParen >>. IW >>. mapCaseSingleList .>>. mapCaseElse .>> rightParen))) |>> Ast.MapCases <!> "MapCases"

let assignmentStatement = positions ((predicateWithQualification .>> IW .>> colonEqual) .>>. predicate) |>> Ast.Assignment <!> "Assignment"

let inEntity = keywordIn >>. positions (predicateWithQualification) |>> Ast.InEntity <!> "InEntity"

let entityInDomain = (variable .>> IW) .>>. inEntity 
let forInBody = (entityInDomain .>> IW) .>>. (leftBrace >>. statementList) .>> rightBrace
let forStatement = positions (keywordFor >>. forInBody) |>> Ast.ForIn <!> "ForIn"

//// Difference of assertion to an axiom: axiom is named predicate, while an assertion uses a predicated to assert it.
//// Difference of assertion to a mandatory property: a mandatory property introduces a completely new identifier inside
//// the scope of a definition. An assertion uses a predicate referring to existing identifiers in the whole theory
//// Difference of assertion to assume: the latter will be used only in the scope of proofs
let assertionStatement = positions (keywordAssert >>. predicate) |>> Ast.Assertion <!> "Assertion"


let baseClassName = positions (idStartsWithCap) .>> IW |>> Ast.BaseClassName <!> "BaseClassName"

let baseConstructorCall = positions (keywordBaseClassReference >>. dot >>. baseClassName .>>. argumentTuple .>> IW) |>> Ast.BaseConstructorCall <!> "BaseConstructorCall"

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

let optionalSpecification = opt (choice [bracketedCoords; argumentTuple])
let predicateWithOptSpecification = positions (fplIdentifier .>>. optionalSpecification) |>> Ast.PredicateWithOptSpecification <!> "PredicateWithOptSpecification"
let dottedPredicate = positions (dot >>. predicateWithOptSpecification) |>> Ast.DottedPredicate <!> "DottedPredicate"
let qualificationList = positions (many dottedPredicate) |>> Ast.QualificationList <!> "QualificationList"
let dollarDigitList = many1 dollarDigits
let referencingIdentifier = positions (predicateIdentifier .>>. dollarDigitList) .>> IW |>> Ast.ReferencingIdentifier <!> "ReferencingIdentifier"
let referenceToProofOrCorollary = positions referencingIdentifier |>> Ast.ReferenceToProofOrCorollary <!> "ReferenceToProofOrCorollary"

predicateWithQualificationRef.Value <- predicateWithOptSpecification .>>. qualificationList |>> Ast.PredicateWithQualification <!> "PredicateWithQualification" 


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

let argumentIdentifierDotted = positions (argIdDottedX) |>> Ast.ArgumentIdentifier <!> "ArgumentIdentifier"
let argumentIdentifierColon = positions (argIdColonX) |>> Ast.ArgumentIdentifier <!> "ArgumentIdentifier"
let refArgumentIdentifier = positions argIdX |>> Ast.RefArgumentIdentifier <!> "RefArgumentIdentifier"
let justificationIdentifier = positions (opt byModifier .>>. predicateIdentifier .>>. opt dollarDigitList .>>. opt (colon >>. refArgumentIdentifier)) |>> Ast.JustificationIdentifier <!> "JustificationIdentifier"
let byDef = positions (keywordByDef >>. SW >>. variable) |>> Ast.ByDef <!> "ByDef"

let justificationItem = positions (choice [attempt byDef ; justificationIdentifier ; refArgumentIdentifier ]) |>> Ast.JustificationItem <!> "JustificationItem"

let twoPredicatesInParens = (leftParen >>. predicate) .>>. (comma >>. predicate) .>> rightParen 

let chooseBinaryOp p = p >>. twoPredicatesInParens
 

let conjunction = positions (chooseBinaryOp keywordAnd)  |>> Ast.And <!> "And"
let disjunction = positions (chooseBinaryOp keywordOr) |>> Ast.Or <!> "Or"
let exclusiveOr = positions (chooseBinaryOp keywordXor) |>> Ast.Xor <!> "Xor"
let implication = positions (chooseBinaryOp keywordImpl) |>> Ast.Impl <!> "Impl"
let equivalence = positions (chooseBinaryOp keywordIif) |>> Ast.Iif <!> "Iif"
let negation = positions (keywordNot >>. predicate) |>> Ast.Not <!> "Not"

let all = positions ((keywordAll >>. namedVariableDeclarationList) .>>. (leftBrace >>. predicate .>> rightBrace)) |>> Ast.All <!> "All"
let exists = positions ((keywordEx >>. namedVariableDeclarationList) .>>. (leftBrace >>. predicate .>> rightBrace)) |>> Ast.Exists <!> "Exists"

let existsNTimes = choice [
                        attempt (keywordExNSymbolic .>> SW) |>> Ast.Exists1  
                        keywordExNSymbolic >>. positions puint32 .>> SW |>> Ast.DollarDigits 
                    ] <!> "existsNTimes"

let existsTimeNQuantifier = choice [
    (keywordExN >>. dollarDigits .>> SW)
    existsNTimes
]

let existsTimesN = positions ((existsTimeNQuantifier .>>. namedVariableDeclarationList) .>>. (leftBrace >>. predicate .>> rightBrace)) |>> Ast.ExistsN <!> "ExistsN"
let isOp = choice [
        attempt ((predicateWithQualification .>> SW .>> keywordIs) .>>. (SW >>. variableType))
        (keywordIs >>. attemptSW >>. leftParen >>. predicate) .>>. (comma >>. variableType) .>> rightParen
    ]
let isOperator = positions isOp |>> Ast.IsOperator <!> "IsOperator"

// infix operators like the equality operator 
let infixSymbolWithPos = positions ( infixMathSymbols ) |>> Ast.InfixSymbolWithPos <!> "infixSymbolWithPos"

// ------------------------------------------------------------
// Parenthesized expression
let pParens : Parser<Ast,unit> =
    positions (leftParen >>. predicate .>> rightParen) 
    |>> Ast.Parens <!> "<pParens>"

// A compound Predicate has its own boolean expressions to avoid mixing up with Pl0Propositions
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

let postfixSymbolWithPos = positions ( postfixMathSymbols ) |>> Ast.PostFixSymbolWithPos <!> "PostFixSymbolWithPos" 
let prefixSymbolWithPos = positions ( prefixMathSymbols ) |>> Ast.PrefixSymbolWithPos <!> "PrefixSymbolWithPos"


let pAtom : Parser<Ast,unit> =
    choice [compoundPredicate; primePredicate; mapCases]
    <|> pParens <!> "pAtom"

// POSTFIX: atom postfix*
let pPostfixExpr : Parser<Ast,unit> =
    pipe2
        pAtom
        (many (attempt (NW >>. postfixSymbolWithPos)) <?> "<postfix symbol>")
        (fun expr postfixes ->
            List.fold (fun acc op -> Ast.PostfixOp(op, acc)) expr postfixes
        ) <!> "pPostfixExpr"

let pPrefixExpr : Parser<Ast,unit> =
    pipe2
        (many (attempt (prefixSymbolWithPos .>> NW)) <?> "<prefix symbol>")
        pPostfixExpr
        (fun prefixes expr ->
            List.foldBack (fun op acc -> Ast.PrefixOp(op, acc)) prefixes expr
        ) <!> "pPrefixExpr"

// INFIX: prefixExpr (space infixOp space prefixExpr)*
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

let expression = pInfixExpr

predicateRef.Value <- expression

predicateListRef.Value <- pExprList 



(* FPL building blocks *)
let keywordDeclaration = (skipString LiteralDecL <|> skipString LiteralDec) .>> SW 

let varDecl = namedVariableDeclaration
let varDeclBlock = (attempt statement <|> varDecl) .>> IW 

let varDeclOrSpecList = IW >>. opt (keywordDeclaration >>. many1 varDeclBlock .>> semiColon) .>> IW |>> Ast.VarDeclBlock <!> "VarDeclBlock" 

let spacesPredicate = IW >>. predicate
let premiseList = positions (IW >>. (keywordPremise >>. colon >>. predicateList)) |>> Ast.PremiseList <!> "PremiseList"
let conclusion = IW >>. (keywordConclusion >>. colon >>. predicate)
let insideRuleOfOnference = (varDeclOrSpecList .>>. (premiseList .>>. conclusion))
let premiseConclusionBlock = leftBrace >>. insideRuleOfOnference .>> rightBrace |>> Ast.PremiseConclusionBlock <!> "PremiseConclusionBlock"

(* FPL building blocks - rules of reference *)
let keywordInference = (skipString LiteralInfL <|> skipString LiteralInf) .>> SW 
let ruleOfInferenceSignature = positions (keywordInference >>. pascalCaseId) .>> IW |>> Ast.RuleOfInferenceSignature <!> "RuleOfInferenceSignature"
let ruleOfInference = positions (ruleOfInferenceSignature .>>. premiseConclusionBlock) |>> Ast.RuleOfInference <!> "RuleOfInference"

(* FPL building blocks - Theorem-like statements and conjectures *)
let keywordTheorem = (skipString LiteralThmL <|> skipString LiteralThm) .>> SW
let keywordLemma = (skipString LiteralLemL <|> skipString LiteralLem) .>> SW
let keywordProposition = (skipString LiteralPropL <|> skipString LiteralProp) .>> SW
let keywordCorollary = (skipString LiteralCorL <|> skipString LiteralCor) .>> SW
let keywordConjecture = (skipString LiteralConjL <|> skipString LiteralConj) .>> SW

let theoremLikeBlock = leftBrace >>. (varDeclOrSpecList .>>. spacesPredicate) .>> rightBrace

let theoremSignature = positions (keywordTheorem >>. pascalCaseId) .>> IW |>> Ast.TheoremSignature <!> "TheoremSignature"
let theorem = positions (theoremSignature .>>. theoremLikeBlock) |>> Ast.Theorem <!> "Theorem"
let lemmaSignature = positions (keywordLemma >>. pascalCaseId) .>> IW |>> Ast.LemmaSignature <!> "LemmaSignature"
let lemma = positions (lemmaSignature .>>. theoremLikeBlock) |>> Ast.Lemma <!> "Lemma"
let propositionSignature = positions (keywordProposition >>. pascalCaseId) .>> IW |>> Ast.PropositionSignature <!> "PropositionSignature"
let proposition = positions (propositionSignature .>>. theoremLikeBlock) |>> Ast.Proposition <!> "Proposition"
let conjectureSignature = positions (keywordConjecture >>. pascalCaseId) .>> IW |>> Ast.ConjectureSignature <!> "ConjectureSignature"
let conjecture = positions (conjectureSignature .>>. theoremLikeBlock) |>> Ast.Conjecture <!> "Conjecture"
let corollarySignature = positions (keywordCorollary >>. simpleSignature .>>. dollarDigitList) .>> IW |>> Ast.CorollarySignature <!> "CorollarySignature"
let corollary = positions (corollarySignature .>>. theoremLikeBlock) |>> Ast.Corollary <!> "Corollary"

(* FPL building blocks - Axioms *)

let keywordAxiom = (skipString LiteralAxL <|> skipString LiteralAx <|> skipString LiteralPostL <|> skipString LiteralPost) >>. SW

let axiomSignature = positions (keywordAxiom >>. pascalCaseId) .>> IW |>> Ast.AxiomSignature <!> "AxiomSignature"
let axiom = positions (axiomSignature .>>. theoremLikeBlock) |>> Ast.Axiom <!> "Axiom"

(* FPL building blocks - Constructors *)

let keywordIntrinsic = positions (skipString LiteralIntrL <|> skipString LiteralIntr) |>> Ast.Intrinsic <!> "Intrinsic"

let predContent = varDeclOrSpecList .>>. spacesPredicate |>> Ast.DefPredicateContent <!> "DefPredicateContent"

let keywordConstructor = (skipString LiteralCtorL <|> skipString LiteralCtor) .>> SW
let constructorBlock = leftBrace >>. varDeclOrSpecList .>> rightBrace |>> Ast.ConstructorBlock <!> "ConstructorBlock"
let constructorSignature = positions (keywordConstructor >>. simpleSignature .>>. paramTuple) .>> IW |>> Ast.ConstructorSignature <!> "ConstructorSignature"
let constructor = positions (constructorSignature .>>. constructorBlock) |>> Ast.Constructor <!> "Constructor"

(* FPL building blocks - Properties *)
let keywordProperty = (skipString LiteralPrtyL <|> skipString LiteralPrty) .>> SW 

let predicateInstanceBlock = opt (leftBrace >>. (keywordIntrinsic <|> predContent) .>> rightBrace)
let predicateInstanceSignature = positions (keywordPredicate >>. SW >>. simpleSignature .>>. paramTuple) .>> IW |>> Ast.PredicateInstanceSignature <!> "PredicateInstanceSignature"
let predicateInstance = positions (keywordProperty >>. predicateInstanceSignature .>>. predicateInstanceBlock) |>> Ast.PredicateInstance <!> "PredicateInstance"

mappingRef.Value <- toArrow >>. IW >>. positions (keywordUndefined <|> variableType) |>> Ast.Mapping <!> "Mapping"

let returnStatement = positions (keywordReturn >>. predicate) .>> IW |>> Ast.Return <!> "Return"
let funcContent = varDeclOrSpecList .>>. returnStatement |>> Ast.DefFunctionContent <!> "DefFunctionContent"
let functionalTermInstanceBlock = opt (leftBrace >>. (keywordIntrinsic <|> funcContent) .>> rightBrace)
let functionalTermInstanceSignature = positions (keywordFunction >>. SW >>. simpleSignature .>>. paramTuple .>>. (IW >>. mapping)) .>> IW |>> Ast.FunctionalTermInstanceSignature <!> "FunctionalTermInstanceSignature"
let functionalTermInstance = positions (keywordProperty >>. functionalTermInstanceSignature .>>. functionalTermInstanceBlock) |>> Ast.FunctionalTermInstance <!> "FunctionalTermInstance"


let extensionRegex = regex "[^\/]+" <?> "<extension regex>" |>> Ast.ExtensionRegex <!> "ExtensionRegex"

let extensionAssignment = positions ((variable .>> IW .>> at .>> IW) .>>. (slash >>. extensionRegex .>> slash)) |>> Ast.ExtensionAssignment <!> "ExtensionAssignment"

let extensionSignature = positions ((extensionAssignment .>> IW) .>>. mapping) .>> IW |>> Ast.ExtensionSignature <!> "ExtensionSignature"
let extensionTerm = leftBrace >>. ((funcContent <|> mapCases) .>> rightBrace)
let definitionExtension = positions (keywordExtension >>. extensionName .>>. extensionSignature .>>. extensionTerm) |>> Ast.DefinitionExtension <!> "DefinitionExtension"

let definitionProperty = choice [
    attempt predicateInstance
    functionalTermInstance
]
let propertyList = opt (many1 (safeProceedingSpace definitionProperty)) 

(* FPL building blocks - Proofs 

    # A Proof relates to the PredicateIdentifier of the Theorem.
    # Because proofs are named, they can stand anywhere inside the theory, not only immediately
    # after the Theorem they prove. This is to enable the users to mix
    # with natural language an provide a proof long after the theorem was stated.

*)
// justifying proof arguments can be the identifiers of Rules of References, conjectures, theorem-like statements, or axioms
let keywordRevoke = (skipString LiteralRevL <|> skipString LiteralRev) .>> SW 
let revokeArgument = positions (keywordRevoke >>. refArgumentIdentifier) |>> Ast.RevokeArgument <!> "RevokeArgument" 
    
let keywordAssume = skipString LiteralAssL <|> skipString LiteralAss .>> SW 
let assumeArgument = positions (keywordAssume >>. predicate) |>> Ast.AssumeArgument <!> "AssumeArgument"
let keywordTrivial  = positions (skipString LiteralTrivial) .>> IW |>> Ast.Trivial <!> "Trivial"
let keywordQed  = positions (skipString LiteralQed) .>> IW |>> Ast.Qed <!> "Qed"
let derivedPredicate = positions predicate |>> Ast.DerivedPredicate <!> "DerivedPredicate"
let derivedArgument = choice [
    keywordTrivial 
    derivedPredicate
]

let argumentInference = (assumeArgument <|> revokeArgument <|> derivedArgument)
let justificationItemList = sepBy1 justificationItem comma

let proofArgumentBeginningStrict = (argumentIdentifierDotted .>> IW) .>>. (justificationItemList .>> IW .>> vDash .>> IW) |>> Ast.StartArgumentStictly <!> "StartArgumentStictly"
let proofArgumentBeginningNoJust = (argumentIdentifierColon .>> IW) |>> Ast.StartArgument <!> "StartArgument"

let justification = positions (choice [proofArgumentBeginningStrict; proofArgumentBeginningNoJust]) |>> Ast.Justification <!> "Justification"
let justifiedArgument = positions (justification .>>. argumentInference) |>> Ast.JustArgInf <!> "JustArgInf"

let proofArgument = positions (justifiedArgument) .>> IW |>> Ast.Argument <!> "Argument"
let proofArgumentList = many1 (IW >>. proofArgument)
let keywordProof = (skipString LiteralPrfL <|> skipString LiteralPrf) .>> SW
let proofContent = varDeclOrSpecList .>>. proofArgumentList .>>. opt keywordQed |>> Ast.ProofContent <!> "ProofContent"
let proofBlock = leftBrace >>. proofContent .>> rightBrace |>> Ast.ProofBlock <!> "ProofBlock"
let proofSignature = positions (keywordProof >>. simpleSignature .>>. dollarDigitList) .>> IW |>> Ast.ProofSignature <!> "ProofSignature"

let proof = positions (proofSignature .>>. proofBlock) |>> Ast.Proof <!> "Proof"

(* FPL building blocks - Definitions *)

// Predicate building blocks can be defined similarly to classes, they can have properties but they cannot be derived any parent type

let predicateDefinitionBlock = opt (leftBrace  >>. (keywordIntrinsic <|> predContent) .>>. propertyList .>> rightBrace)
let inheritedType = positions idStartsWithCap |>> Ast.InheritedType <!> "InheritedType" 
let inheritedTypeList = sepBy1 inheritedType comma |>> Ast.InheritedTypeList <!> "InheritedTypeList"
let predicateSignature = positions (keywordPredicate >>. SW >>. (simpleSignature .>>. opt (colon >>. inheritedTypeList) .>> IW) .>>. paramTuple) .>>. userDefinedSymbol .>> IW |>> Ast.PredicateSignature <!> "PredicateSignature"
let definitionPredicate = positions (predicateSignature .>>. predicateDefinitionBlock) |>> Ast.DefinitionPredicate <!> "DefinitionPredicate"

// Functional term building blocks can be defined similarly to classes, they can have properties but they cannot be derived any parent type 
let functionalTermDefinitionBlock = positions (opt (leftBrace  >>. ((keywordIntrinsic <|> funcContent) .>> IW) .>>. propertyList .>> rightBrace))  |>> Ast.FunctionalTermDefinitionBlock <!> "FunctionalTermDefinitionBlock"

let functionalTermSignature = positions (keywordFunction >>. SW >>. (simpleSignature .>>. opt (colon >>. inheritedTypeList) .>> IW) .>>. paramTuple .>>. (IW >>. mapping)) .>>. userDefinedSymbol .>> IW |>> Ast.FunctionalTermSignature <!> "FunctionalTermSignature"
let definitionFunctionalTerm = positions (functionalTermSignature .>>. functionalTermDefinitionBlock) |>> Ast.DefinitionFunctionalTerm <!> "DefinitionFunctionalTerm"

// Class definitions
let keywordClass = (skipString LiteralClL <|> skipString LiteralCl)

let constructorList = many1 (constructor .>> IW)
let classCompleteContent = varDeclOrSpecList .>>. constructorList |>> Ast.DefClassCompleteContent <!> "DefClassCompleteContent"
let classDefinitionBlock = positions (opt (leftBrace  >>. ((keywordIntrinsic <|> classCompleteContent) .>> IW) .>>. propertyList .>> rightBrace)) |>> Ast.ClassDefinitionBlock <!> "ClassDefinitionBlock"

let classSignature = positions (keywordClass >>. SW >>. pascalCaseId) .>> IW |>> Ast.ClassSignature <!> "ClassSignature"
let classSignatureExtended = classSignature .>>. opt (colon >>. inheritedTypeList) .>>. opt (attempt (IW >>. userDefinedObjSym)) .>> IW
let definitionClass = positions (classSignatureExtended .>>. classDefinitionBlock) |>> Ast.DefinitionClass <!> "DefinitionClass" 

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
let localizationLanguageCode = positions (regex @"[a-z]{3}" <?> "<ISO 639 language code>") |>> Ast.LanguageCode <!> "LanguageCode"

let ebnfTransl, ebnfTranslRef = createParserForwardedToRef()
let ebnfTranslTuple = (leftParen >>. IW >>. ebnfTransl) .>> rightParen
let ebnfFactor = choice [
    variable
    quote >>. localizationString .>> quote
    ebnfTranslTuple
] 
let ebnfTerm = positions (sepEndBy1 ebnfFactor SW) |>> Ast.TranslationTerm <!> "TranslationTerm"
ebnfTranslRef.Value <-  positions (sepBy1 ebnfTerm (IW >>. case >>. IW)) |>> Ast.TranslationTermList <!> "TranslationTermList"
let language = positions ((exclamationMark >>. localizationLanguageCode .>> IW .>> colon) .>>. ebnfTransl) |>> Ast.Language <!> "Language"
let languageList = many1 (IW >>. language .>> IW)
let localization = positions (keywordLocalization >>. predicate) .>> (IW .>> colonEqual) .>>. (languageList .>> (IW .>> semiColon)) .>> IW |>> Ast.Localization <!> "Localization"

// FPL building blocks can be definitions, axioms, Theorem-proof blocks and conjectures
let buildingBlock = positions(choice [definition; axiom; theorem; lemma; proposition; corollary; conjecture; proof; ruleOfInference; localization; usesClause; definitionExtension]) .>> IW |>> Ast.BuildingBlock <!> "BuildingBlock"

(* Namespaces *)
let fplNamespace = many buildingBlock |>> Ast.Namespace <!> "Namespace"

(* Final Parser *)
let stdParser = positions (IW >>. fplNamespace) |>> Ast.AST <!> "AST"
