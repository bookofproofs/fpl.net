/// This module contains the FPL parser producing an abstract syntax tree out of a given FPL code 
module FplParser
open System.Text.RegularExpressions
open FplGrammarCommons
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
let rightBrace = tokenize "RightBrace" (skipChar '}') >>. spaces
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
// The FPL interpreter will try to match extensionString after the @ literal
// by tryoing out the regex expressions of all user-declared ExtensionBlocks (in their declaration order)
// until none or the first of then matches this string. Then, the matched string will get the named type of the ExtensionBlock.
let extensionString = regex @"[^,\s()\[\]{}]+" <?> "<extensionString>" 
let extension = positions "Extension" (at >>. extensionString) |>> Ast.Extension

(* Identifiers *)


let IdStartsWithSmallCase = regex @"[a-z]\w*" 
let idStartsWithCap = (regex @"[A-Z]\w*") <?> "<PascalCaseId>"
let pascalCaseId = idStartsWithCap |>> Ast.PascalCaseId
let argumentIdentifier = positions "ArgumentIdentifier" (regex @"\d+\w*\.") <?> "<argument identifier>" |>> Ast.ArgumentIdentifier

let namespaceIdentifier = positions "NamespaceIdentifier" (sepBy1 pascalCaseId dot) .>> IW |>> Ast.NamespaceIdentifier
let predicateIdentifier = positions "PredicateIdentifier" (sepBy1 pascalCaseId dot) |>> Ast.PredicateIdentifier 

let alias = positions "Alias" (skipString literalAlias >>. SW >>. idStartsWithCap) |>> Ast.Alias
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

let keywordSelf = positions "Self" (skipString "self") .>> IW |>> Ast.Self
let keywordParent = positions "Parent" (skipString "parent") .>> IW |>> Ast.Parent
let keywordBaseClassReference = skipString literalBase .>> IW
let keywordIndex = positions "IndexType" (skipString "index" <|> skipString "ind") |>> Ast.IndexType


(* FplBlock-related Keywords *)
let keywordPremise = (skipString "premise" <|> skipString "pre") >>. IW 
let keywordConclusion = (skipString literalConL <|> skipString literalCon) >>. IW


(* Statement-related Keywords *)
let keywordDel = skipString literalDelL <|> skipString literalDel 
let keywordFor = skipString literalFor .>> SW 
let keywordIn = skipString "in" .>> SW 
let keywordCases = skipString literalCases .>> IW 
let keywordAssert = skipString literalAssL .>> SW

(* Predicate-related Keywords *)
let keywordUndefined = positions "Undefined" (skipString "undefined" <|> skipString "undef") .>> IW |>> Ast.Undefined
let keywordTrue = positions "True" (skipString "true") .>> IW  |>> Ast.True  
let keywordFalse = positions "False" (skipString literalFalse) .>> IW |>>  Ast.False  
let keywordBydef = positions literalByDef (skipString literalByDef) .>> SW  
let keywordAnd = skipString literalAnd .>> IW 
let keywordOr = skipString "or" .>> IW 
let keywordImpl = skipString "impl" .>> IW 
let keywordIif = skipString "iif" .>> IW 
let keywordXor = skipString "xor" .>> IW 
let keywordNot = skipString "not" .>> attemptSW 
let keywordAll = skipString literalAll .>> SW 
let keywordEx = skipString literalEx .>> SW
let keywordExN = skipString literalExN .>> IW
let keywordIs = skipString "is" .>> attemptSW 


// Via templates, FPL supports generic types, which make it possible to define abstract mathematical
// objects and their properties that defer the concrete
// specification of one or more types until the definition or method is declared and instantiated by
// client code
let keywordTemplate = positions "TemplateType" (pstring "template" <|> pstring "tpl") |>> Ast.TemplateType

let templateTail = choice [ idStartsWithCap; (regex @"\d+") ]

let templateWithTail = positions "TemplateType" (many1Strings2 (pstring "template" <|> pstring "tpl") templateTail) |>>  Ast.TemplateType

let keywordObject = positions "ObjectType" (skipString "object" <|> skipString "obj") |>> Ast.ObjectType 

let objectHeader = choice [
    keywordObject
    (attempt templateWithTail) <|> keywordTemplate
] 

let keywordPredicate = positions "PredicateType" (skipString "predicate" <|> skipString "pred") |>> Ast.PredicateType
let keywordFunction = positions "FunctionalTermType" (skipString literalFuncL <|> skipString literalFunc) |>> Ast.FunctionalTermType


let theoryNamespace = aliasedNamespaceIdentifier <|> namespaceIdentifier .>> IW

let keywordUses = (skipString "uses") .>> SW
let usesClause = positions "UsesClause" (keywordUses >>. theoryNamespace) |>> Ast.UsesClause

(* Signatures, Variable Declarations, and Types, and Coordinates *)
// convention: All syntax production rules of FPL syntax extensions have to start with literalExt, followed by
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

let keywordExtension = (skipString literalExtL <|> skipString literalExt) .>> SW

let extensionName = positions "ExtensionName" (idStartsWithCap) |>> Ast.ExtensionName

let xId = positions "ExtensionType" (at >>. extensionName) |>> Ast.ExtensionType 

let specificClassType = choice [ objectHeader; xId; predicateIdentifier ] 

//// later semantics: Star: 0 or more occurrences, Plus: 1 or more occurrences
let varDeclModifier = choice [ colonStar; colonPlus; colon ] .>> IW

let bracketedCoordsInType = positions "BracketedCoordsInType" (leftBracket >>. namedVariableDeclarationList .>> rightBracket) |>> Ast.BracketedCoordsInType

// The classType is the last type in FPL we can derive FPL classes from.
// It therefore excludes the in-built FPL-types keywordPredicate, keywordFunction, and keywordIndex
// to restrict it to pure objects.
// In contrast to variableType which can also be used for declaring variables 
// in the scope of FPL building blocks
let bracketModifier = choice [bracketedCoordsInType; paramTuple ]
classTypeRef.Value <- positions "ClassType" (specificClassType .>>. opt bracketModifier) |>> Ast.ClassType

let mapping, mappingRef = createParserForwardedToRef()
let predicateType = positions "CompoundPredicateType" (keywordPredicate .>>. opt paramTuple) |>> Ast.CompoundPredicateType
let functionalTermType = positions "CompoundFunctionalTermType" (keywordFunction .>>. opt (paramTuple .>>. (IW >>. mapping))) |>> Ast.CompoundFunctionalTermType

let compoundVariableType = choice [ keywordIndex; xId; classType; functionalTermType; predicateType ] 
let variableType = positions "VariableType" (compoundVariableType) |>> Ast.VariableType

let namedVariableDeclaration = positions "NamedVarDecl" (variableList .>>. varDeclModifier .>>. variableType .>> IW) |>> Ast.NamedVarDecl
namedVariableDeclarationListRef.Value <- sepBy namedVariableDeclaration comma

paramTupleRef.Value <- positions "ParamTuple" ((leftParen >>. IW >>. namedVariableDeclarationList) .>> (IW .>> rightParen)) |>> Ast.ParamTuple
let signature = positions "Signature" ((predicateIdentifier .>> IW) .>>. paramTuple) .>> IW |>> Ast.Signature
let localizationString = positions "LocalizationString" (regex "[^\"\n]*") <?> "<language-specific string>" |>> Ast.LocalizationString

let keywordSymbol = pstring "symbol" .>> IW
let objectSymbolString = pchar '"' >>. objectMathSymbols .>> pchar '"'
let infixString = pchar '"' >>. infixMathSymbols .>> pchar '"'
let keywordInfix = pstring "infix" >>. IW
let postfixString = pchar '"' >>. postfixMathSymbols .>> pchar '"' 
let keywordPostfix = pstring "postfix" >>. IW
let prefixString = pchar '"' >>. prefixMathSymbols .>> pchar '"' 
let keywordPrefix = pstring "prefix" >>. IW
let userDefinedObjSym = positions "Symbol" (keywordSymbol >>. objectSymbolString) .>> IW |>> Ast.Symbol
let precedence = positions "Precedence" (pint32) .>> IW |>> Ast.Precedence

let userDefinedInfix = positions "Infix" (keywordInfix >>. (infixString .>>. (IW >>. precedence))) .>> IW |>> Ast.Infix
let userDefinedPostfix = positions "Postfix" (keywordPostfix >>. postfixString) .>> IW |>> Ast.Postfix
let userDefinedPrefix = positions "Prefix" (keywordPrefix >>. prefixString) .>> IW |>> Ast.Prefix
let userDefinedSymbol = opt (choice [userDefinedPrefix; userDefinedInfix; userDefinedPostfix ])

let signatureWithUserDefinedString = positions "SignatureWithUserDefinedString" (predicateIdentifier .>> IW .>>. userDefinedSymbol .>>. paramTuple) .>> IW |>> Ast.SignatureWithUserDefinedString
(* Statements *)
let argumentTuple = positions "ArgumentTuple" ((leftParen >>. predicateList) .>> (IW .>> rightParen)) |>> Ast.ArgumentTuple 

let word = regex @"\w+" <?> "<word>" .>> IW
let fplDelegateIdentifier = positions "DelegateId" (keywordDel >>. dot >>. word) .>> IW |>> Ast.DelegateId
let fplDelegate = positions "Delegate" (fplDelegateIdentifier .>>. argumentTuple) |>> Ast.Delegate
let assignmentStatement = positions "Assignment" ((predicateWithQualification .>> IW .>> colonEqual) .>>. predicate) |>> Ast.Assignment

let spacesRightBrace = (IW .>> rightBrace) 

let keywordReturn = IW >>. (skipString "return" <|> skipString "ret") .>> SW 

let defaultResult = positions "DefaultResult" (IW >>. statementList) |>> Ast.DefaultResult
let conditionFollowedByResult = positions "ConditionFollowedByResult" ((case >>. predicate .>> colon) .>>. (IW >>. statementList)) |>> Ast.ConditionFollowedByResult
let conditionFollowedByResultList = many1 (IW >>. conditionFollowedByResult)

let elseStatement = elseCase >>. IW >>. defaultResult .>> IW
let casesStatement = positions "Cases" (((keywordCases >>. leftParen >>. IW >>. conditionFollowedByResultList .>>. elseStatement .>> rightParen))) |>> Ast.Cases

let inEntity = keywordIn >>. positions "InEntity" (predicateWithQualification) .>> IW |>> Ast.InEntity

let entityInDomain = ( entity .>> IW .>>. inEntity ) .>> IW
let forInBody = (entityInDomain .>> IW) .>>. (leftBrace >>. IW >>. statementList) .>> (IW >>. rightBrace)
let forStatement = positions "ForIn" (keywordFor >>. forInBody) |>> Ast.ForIn

//// Difference of assertion to an axiom: axiom's is followed by a signature of a predicate (i.e. with possible parameters),
//// not by a predicate (i.e. with possible arguments)
//// Difference of assertion to a mandatory property: a mandatory property introduces a completely new identifier inside
//// the scope of a definition. An assertion uses a predicate referring to existing identifiers in the whole theory
//// Difference of assertion to assume: the latter will be used only in the scope of proofs
let assertionStatement = positions "Assertion" (keywordAssert >>. predicate) |>> Ast.Assertion
let inheritedClassType = positions "InheritedClassType" (choice [keywordObject; predicateIdentifier]) .>> IW |>> Ast.InheritedClassType
let callConstructorParentClass = positions "ParentConstructorCall" (keywordBaseClassReference >>. dot >>. inheritedClassType .>>. argumentTuple .>> IW) |>> Ast.ParentConstructorCall

let statement = 
    (choice [
        callConstructorParentClass
        casesStatement
        assertionStatement
        forStatement
        assignmentStatement
    ])

statementListRef.Value <- many (IW >>. statement .>> IW)

(* Predicates *)
let optionalSpecification = opt (choice [bracketedCoords; argumentTuple])
let predicateWithOptSpecification = positions "PredicateWithOptSpecification" (fplIdentifier .>>. optionalSpecification) |>> Ast.PredicateWithOptSpecification
let dottedPredicate = positions "DottedPredicate" (dot >>. predicateWithOptSpecification) |>> Ast.DottedPredicate
let qualificationList = positions "QualificationList" (many dottedPredicate) |>> Ast.QualificationList

predicateWithQualificationRef.Value <- predicateWithOptSpecification .>>. qualificationList |>> Ast.PredicateWithQualification 

let dollarDigitList = many1 dollarDigits
let referencingIdentifier = positions "ReferencingIdentifier" (predicateIdentifier .>>. dollarDigitList) .>> IW |>> Ast.ReferencingIdentifier
let referenceToProofOrCorollary = positions "ReferenceToProofOrCorollary" (referencingIdentifier .>>. opt argumentTuple) .>> IW |>> Ast.ReferenceToProofOrCorollary

let byDefinition = positions "ByDef" (keywordBydef >>. predicateWithQualification ) |>> Ast.ByDef 

primePredicateRef.Value <- choice [
    keywordTrue
    keywordFalse
    keywordUndefined
    byDefinition
    attempt argumentIdentifier
    fplDelegate 
    dollarDigits
    attempt referenceToProofOrCorollary
    predicateWithQualification
    objectSymbol
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

let existsTimesN = positions "ExistsN" (((keywordExN >>. dollarDigits .>> SW) .>>. namedVariableDeclaration) .>>. (leftBrace >>. predicate .>> rightBrace)) |>> Ast.ExistsN
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
let expression = positions "Expression" (opt prefixOp .>>. choice [compoundPredicate; primePredicate] .>>. opt postfixOp .>>. optionalSpecification .>>. qualificationList) .>> IW |>> Ast.Expression

predicateRef.Value <- expression

predicateListRef.Value <- sepBy predicate comma

(* FPL building blocks *)
let keywordDeclaration = (skipString literalDecL <|> skipString literalDec) .>> SW 

let varDecl = tilde >>. namedVariableDeclaration
let varDeclBlock = positions "VarDeclBlock" (IW >>. keywordDeclaration >>. (many ((varDecl <|> statement) .>> IW)) .>> semiColon) .>> IW |>> Ast.VarDeclBlock 

let varDeclOrSpecList = opt (many1 (varDeclBlock)) 
(*To simplify the syntax definition, we do not define separate
FplPremiseConclusionBlocks for rules of inference and theorem-like blocks.
The first have a simplified, PL0 semantics, the latter have a more complex, predicative semantics.
However, there is a syntactical simplification of the signature*)
let spacesPredicate = IW >>. predicate
let premise = IW >>. (keywordPremise >>. colon >>. predicate) 
let conclusion = IW >>. (keywordConclusion >>. colon >>. predicate) 
let premiseConclusionBlock = positions "PremiseConclusionBlock" (leftBrace >>. varDeclOrSpecList .>>. premise .>>. conclusion .>> spacesRightBrace) |>> Ast.PremiseConclusionBlock

(* FPL building blocks - rules of reference *)
let keywordInference = (skipString "inference" <|> skipString "inf") .>> SW 
let ruleOfInference = positions "RuleOfInference" (keywordInference >>. signature .>>. premiseConclusionBlock) |>> Ast.RuleOfInference

(* FPL building blocks - Theorem-like statements and conjectures *)
let keywordTheorem = (skipString "theorem" <|> skipString "thm") .>> SW
let keywordLemma = (skipString "lemma" <|> skipString "lem") .>> SW
let keywordProposition = (skipString "proposition" <|> skipString "prop") .>> SW
let keywordCorollary = (skipString literalCorL <|> skipString literalCor) .>> SW
let keywordConjecture = (skipString literalConjL <|> skipString literalConj) .>> SW

let theoremLikeBlock = leftBrace >>. varDeclOrSpecList .>>. spacesPredicate .>> spacesRightBrace
let signatureWithTheoremLikeBlock = signature .>>. theoremLikeBlock

let theorem = positions "Theorem" (keywordTheorem >>. signatureWithTheoremLikeBlock) |>> Ast.Theorem
let lemma = positions "Lemma" (keywordLemma >>. signatureWithTheoremLikeBlock) |>> Ast.Lemma
let proposition = positions "Proposition" (keywordProposition >>. signatureWithTheoremLikeBlock) |>> Ast.Proposition
let conjecture = positions "Conjecture" (keywordConjecture >>. signatureWithTheoremLikeBlock) |>> Ast.Conjecture

let corollarySignature = referencingIdentifier .>>. paramTuple .>> IW |>> Ast.CorollarySignature
let corollary = positions "Corollary" (keywordCorollary >>. corollarySignature .>>. theoremLikeBlock) |>> Ast.Corollary

(* FPL building blocks - Axioms *)

let keywordAxiom = (skipString literalAxL <|> skipString literalAx <|> skipString "postulate" <|> skipString "post") >>. SW

let axiom = positions "Axiom" (keywordAxiom >>. signatureWithTheoremLikeBlock) |>> Ast.Axiom

(* FPL building blocks - Constructors *)

let keywordIntrinsic = positions "Intrinsic" (skipString "intrinsic" <|> skipString "intr") .>> IW |>> Ast.Intrinsic

let predContent = varDeclOrSpecList .>>. spacesPredicate |>> Ast.DefPredicateContent

let keywordConstructor = (skipString literalCtorL <|> skipString literalCtor) .>> SW
let constructorBlock = leftBrace >>. varDeclOrSpecList .>>. selfOrParent .>> spacesRightBrace 
let constructor = positions "Constructor" (keywordConstructor >>. signature .>>. constructorBlock) |>> Ast.Constructor

(* FPL building blocks - Properties *)
let keywordOptional = positions "Optional" (skipString "optional" <|> skipString "opt") .>> SW |>> Ast.Optional
let keywordProperty = positions "Property" (skipString "property" <|> skipString "prty") .>> SW |>> Ast.Property

let predInstanceBlock = leftBrace >>. (keywordIntrinsic <|> predContent) .>> spacesRightBrace
let predicateInstance = positions "PredicateInstance" ((keywordPredicate >>. SW >>. opt keywordOptional) .>>. signature .>>. (IW >>. predInstanceBlock)) |>> Ast.PredicateInstance

mappingRef.Value <- toArrow >>. IW >>. positions "Mapping" (variableType) |>> Ast.Mapping
let functionalTermSignature = positions "FunctionalTermSignature" ((keywordFunction >>. SW >>. opt keywordOptional) .>>. signatureWithUserDefinedString .>>. (IW >>. mapping)) .>> IW |>> Ast.FunctionalTermSignature

let returnStatement = positions "Return" (keywordReturn >>. predicate) .>> IW |>> Ast.Return
let funcContent = varDeclOrSpecList .>>. returnStatement |>> Ast.DefFunctionContent
let functionalTermInstanceBlock = leftBrace >>. (keywordIntrinsic <|> funcContent) .>> spacesRightBrace
let functionalTermInstance = positions "FunctionalTermInstance" (functionalTermSignature .>>. functionalTermInstanceBlock) |>> Ast.FunctionalTermInstance


let extensionRegex = regex "[^\/]+" <?> "<extension regex>" |>> Ast.ExtensionRegex

let extensionAssignment = positions "ExtensionAssignment" ((variable .>> IW .>> at .>> IW) .>>. (slash >>. extensionRegex .>> slash)) |>> Ast.ExtensionAssignment

let extensionSignature = positions "ExtensionSignature" ((extensionAssignment .>> IW) .>>. mapping .>> IW) |>> Ast.ExtensionSignature
let extensionTerm = leftBrace >>. funcContent .>> spacesRightBrace
let extensionBlock = positions "ExtensionBlock" (keywordExtension >>. (extensionName .>> SW) .>>. extensionSignature .>>. extensionTerm) |>> Ast.DefinitionExtension

let definitionProperty = choice [
    predicateInstance
    functionalTermInstance
]
let propertyHeader = IW >>. keywordProperty 
let property = positions "PropertyBlock" (propertyHeader .>>. definitionProperty) |>> Ast.PropertyBlock
let propertyList = opt (many1 (property .>> IW)) 

(* FPL building blocks - Proofs 

    # A Proof relates to the PredicateIdentifier of the Theorem.
    # Because proofs are named, they can stand anywhere inside the theory, not only immediately
    # after the Theorem they prove. This is to enable the users to mix
    # with natural language an provide a proof long after the theorem was stated.

*)
// justifying proof arguments can be the identifiers of Rules of References, conjectures, theorem-like statements, or axioms
let keywordRevoke = (skipString "revoke" <|> skipString "rev") .>> SW 
let revokeArgument = positions "RevokeArgument" (keywordRevoke >>. argumentIdentifier) |>> Ast.RevokeArgument 
    
let keywordAssume = skipString literalAssume <|> skipString literalAss .>> SW 
let assumeArgument = positions "AssumeArgument" (keywordAssume >>. predicate) |>> Ast.AssumeArgument
let keywordTrivial  = positions "Trivial" (skipString "trivial") .>> IW |>> Ast.Trivial
let keywordQed  = positions "Qed" (skipString "qed") .>> IW |>> Ast.Qed
let derivedPredicate = positions "DerivedPredicate" predicate |>> Ast.DerivedPredicate
let derivedArgument = choice [
    keywordTrivial 
    derivedPredicate
]

let argumentInference = vDash >>. IW >>. (assumeArgument <|> revokeArgument <|> derivedArgument)
let justification = positions "Justification" (predicateList .>> IW) |>> Ast.Justification
let justifiedArgument = positions "JustArgInf" (justification .>>. argumentInference) |>> Ast.JustArgInf
let proofArgument = positions "Argument" ((argumentIdentifier .>> IW) .>>. justifiedArgument) .>> IW |>> Ast.Argument
let proofArgumentList = many1 (IW >>. (proofArgument <|> varDeclBlock))
let keywordProof = (skipString "proof" <|> skipString "prf") .>> SW 
let proofBlock = leftBrace >>. proofArgumentList .>>. opt keywordQed .>> spacesRightBrace
let proof = positions "Proof" ((keywordProof >>. referencingIdentifier) .>>. (IW >>. proofBlock)) |>> Ast.Proof

(* FPL building blocks - Definitions *)

// Predicate building blocks can be defined similarly to classes, they can have properties but they cannot be derived any parent type 
let predicateDefinitionBlock = leftBrace  >>. ((keywordIntrinsic <|> predContent) .>> IW) .>>. propertyList .>> spacesRightBrace 
let definitionPredicate = positions "DefinitionPredicate" (keywordPredicate >>. SW >>. (signatureWithUserDefinedString .>> IW) .>>. predicateDefinitionBlock) |>> Ast.DefinitionPredicate

// Functional term building blocks can be defined similarly to classes, they can have properties but they cannot be derived any parent type 
let functionalTermDefinitionBlock = leftBrace  >>. ((keywordIntrinsic <|> funcContent) .>> IW) .>>. propertyList .>> spacesRightBrace
let definitionFunctionalTerm = positions "DefinitionFunctionalTerm" ((functionalTermSignature .>> IW) .>>. functionalTermDefinitionBlock) |>> Ast.DefinitionFunctionalTerm

// Class definitions
let keywordClass = (skipString literalClL <|> skipString literalCl)

let constructorList = many1 (constructor .>> IW)
let classCompleteContent = varDeclOrSpecList .>>. constructorList|>> Ast.DefClassCompleteContent
let classDefinitionBlock = leftBrace  >>. ((keywordIntrinsic <|> classCompleteContent) .>> IW) .>>. propertyList .>> spacesRightBrace
let inheritedClassTypeList = sepBy1 inheritedClassType comma

let classIdentifier = positions "ClassIdentifier" (predicateIdentifier .>> IW) |>> Ast.ClassIdentifier
let classSignature = classIdentifier .>>. opt userDefinedObjSym .>>. (colon >>. inheritedClassTypeList)
let definitionClass = positions "DefinitionClass" ((keywordClass >>. SW >>. classSignature .>> IW) .>>. classDefinitionBlock) |>> Ast.DefinitionClass 

let keywordDefinition = (skipString literalDefL <|> skipString literalDef) >>. SW
let definition = keywordDefinition >>. choice [
    definitionClass
    definitionPredicate
    definitionFunctionalTerm
]
(* Gathering together all Building Blocks to a theory *)

(* Localizations *)
// Localizations provide a possibility to automatically translate FPL expressions into natural languages
let keywordLocalization = (skipString "localization" <|> skipString "loc") >>. SW
let localizationLanguageCode = positions "LanguageCode" (regex @"[a-z]{3}" <?> "<ISO 639 language code>") |>> Ast.LanguageCode

let ebnfTransl, ebnfTranslRef = createParserForwardedToRef()
let ebnfTranslTuple = (leftParen >>. IW >>. ebnfTransl) .>> (IW .>> rightParen) 
let ebnfFactor = choice [
    variable
    quote >>. localizationString .>> quote
    ebnfTranslTuple
] 
let ebnfTerm = positions "LocalizationTerm" (sepEndBy1 ebnfFactor SW) |>> Ast.LocalizationTerm
ebnfTranslRef.Value <-  positions "LocalizationTermList" (sepBy1 ebnfTerm (IW >>. case >>. IW)) |>> Ast.LocalizationTermList
let translation = positions "Translation" ((exclamationMark >>. localizationLanguageCode .>> IW .>> colon) .>>. ebnfTransl) |>> Ast.Translation
let translationList = many1 (IW >>. translation .>> IW)
let localization = positions "Localization" (keywordLocalization >>. (predicate .>> IW .>> colonEqual) .>>. (translationList .>> IW .>> semiColon)) .>> IW |>> Ast.Localization

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
    extensionBlock
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

let errRecPattern = "(definition|def|mandatory|mand|optional|opt|axiom|ax|postulate|post|theorem|thm|proposition|prop|lemma|lem|corollary|cor|conjecture|conj|declaration|dec|constructor|ctor|proof|prf|inference|inf|localization|loc|uses|and|or|impl|iif|xor|not|all|extension|ext|exn|ex|is|assert|cases|self\!|for|delegate|del|\|\-|\||\?|assume|ass|revoke|rev|return|ret)\W|(conclusion|con|premise|pre)\s*\:|(~|\!)[a-z]"

let errInformation = [
    (DEF000, [literalDef], definition)
    (PRP000, ["mand"; "opt"], property)
    (AXI000, [literalAx; "post"], axiom)
    (THM000, ["theorem"; "thm"], theorem)
    (COR000, ["theorem"; literalCor], corollary)
    (LEM000, ["lem"], lemma)
    (PPS000, ["prop"], proposition)
    (CNJ000, [literalConj], conjecture)
    (VAR000, [literalDec], varDeclBlock)
    (CTR000, [literalCtorL; literalCtor], constructor)
    (PRF000, ["proof"; "prf"], proof)
    (INF000, ["inf"], ruleOfInference)
    (LOC000, ["loc"], localization)
    (USE000, ["uses"], usesClause)
    (PRD000, [literalAnd; "or"; "impl"; "iif"; "xor"; "not"; literalAll; literalEx; "is"], compoundPredicate)
    (SMT000, [literalAssL; literalCases; literalBase; literalFor; literalDel], statement)
    (AGI000, ["|-"], argumentInference)
    (CAS000, ["|"], conditionFollowedByResult)
    (DCS000, ["?"], elseStatement)
    (ASS000, [literalAss], assumeArgument)
    (REV000, ["rev"], revokeArgument)
    (RET000, ["ret"], returnStatement)
    (PRE000, ["pre"], premise)
    (CON000, [literalCon], conclusion)
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
