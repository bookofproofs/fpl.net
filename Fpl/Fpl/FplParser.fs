﻿module FplParser
open System.Text.RegularExpressions
open FplGrammarCommons
open FplGrammarTypes
open ErrDiagnostics
open FParsec


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
        (fun (startPos, result) endPos -> 
        (Positions(startPos, endPos), result))


(* Literals *)

let leftBrace = skipChar '{' >>. spaces 
let rightBrace = skipChar '}' >>. spaces
let leftParen = skipChar '(' >>. spaces 
let rightParen = skipChar ')' 
let comma = skipChar ',' >>. spaces 
let dot = skipChar '.' >>% Ast.Dot
let colon = skipChar ':' >>. spaces >>% Ast.One
let colonStar = skipString ":*" >>. spaces >>% Ast.Many
let colonPlus = skipString ":+" >>. spaces >>% Ast.Many1
let colonEqual = skipString ":=" >>. spaces 
let at = pchar '@'
let case = skipChar '|' >>. spaces
let elseCase = skipChar '?' >>. spaces
let leftBracket = skipChar '[' >>. spaces 
let rightBracket = skipChar ']' >>. spaces  
let leftClosedBracket = skipString "[[" >>. spaces <?> "<(closed) left bound>"
let leftOpenBracket = skipString "[(" >>. spaces <?> "<(open) left bound>"
let rightOpenBracket = skipString ")]" >>. spaces <?> "<(open) right bound>" 
let rightClosedBracket = skipString "]]" >>. spaces <?> "<(closed) right bound>" 
let tilde = skipChar '~' .>> spaces
let semiColon = skipChar ';' >>. spaces
let exclamationMark = skipChar '!' 
let toArrow = skipString "->"
let vDash = skipString "|-"

(* Whitespaces and Comments *)

let IW = spaces <?> "<whitespace>"

let SW = spaces1 <?> "<significant whitespace>"

let attemptSW = SW <|> (IW .>> lookAhead (choice [skipChar '('; skipChar ')'; skipChar '{'; skipChar ','; skipChar ';'; skipChar '[' ]))

// -----------------------------------------------------
// Extensions of the FPL language (have to be dynamic)! Lacking a pre-processor, we put the rules
// from the Proof of Concept of FPL code manually into the EBNF of the core FPL grammar.
// note that this has to be inserted into:
// the IsOperand choice
// the PredicateOrFunctionalTerm choice
let digits = regex @"\d+" <?> "<digits>" |>> Ast.Digits
let extDigits: Parser<_, unit> = positions (digits) |>> Ast.ExtDigits

(* Identifiers *)


let IdStartsWithSmallCase = regex @"[a-z]\w*" 
let idStartsWithCap = (regex @"[A-Z]\w*") <?> "<PascalCaseId>"
let pascalCaseId = idStartsWithCap |>> Ast.PascalCaseId
let argumentIdentifier = positions (regex @"\d+\w*\.") <?> "<argument identifier>" |>> Ast.ArgumentIdentifier

let namespaceIdentifier = positions (sepBy1 pascalCaseId dot) .>> IW |>> Ast.NamespaceIdentifier
let predicateIdentifier = positions (sepBy1 pascalCaseId dot) |>> Ast.PredicateIdentifier 

let alias = positions (skipString "alias" >>. SW >>. idStartsWithCap) |>> Ast.Alias
let star = positions (skipChar '*') >>% Ast.Star

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

let keywordSelf = positions (skipString "self") .>> IW |>> Ast.Self
let keywordBaseClassReference = skipString "base" .>> IW
let keywordIndex = (skipString "index" <|> skipString "ind") .>> attemptSW >>% Ast.IndexType


(* FplBlock-related Keywords *)
let keywordPremise = (skipString "premise" <|> skipString "pre") >>. IW 
let keywordConclusion = (skipString "conclusion" <|> skipString "con") >>. IW


(* Statement-related Keywords *)
let keywordDel = skipString "delegate" <|> skipString "del" 
let keywordFor = skipString "for" .>> SW 
let keywordIn = skipString "in" .>> SW 
let keywordCases = skipString "cases" .>> IW 
let keywordAssert = skipString "assert" .>> SW

(* Predicate-related Keywords *)
let keywordUndefined = positions (skipString "undefined" <|> skipString "undef") .>> IW |>> Ast.Undefined
let keywordTrue = positions (skipString "true") .>> IW  |>> Ast.True  
let keywordFalse = positions (skipString "false") .>> IW |>>  Ast.False  
let keywordBydef = positions (skipString "bydef") .>> SW  
let keywordAnd = skipString "and" .>> IW 
let keywordOr = skipString "or" .>> IW 
let keywordImpl = skipString "impl" .>> IW 
let keywordIif = skipString "iif" .>> IW 
let keywordXor = skipString "xor" .>> IW 
let keywordNot = skipString "not" .>> attemptSW 
let keywordAll = skipString "all" .>> SW 
let keywordEx = skipString "ex" .>> SW
let keywordExN = skipString "exn" .>> IW
let keywordIs = skipString "is" .>> IW 


// Via templates, FPL supports generic types, which make it possible to define abstract mathematical
// objects and their properties that defer the concrete
// specification of one or more types until the definition or method is declared and instantiated by
// client code
let keywordTemplate = positions (pstring "template" <|> pstring "tpl") .>> IW |>> Ast.TemplateType

let templateTail = choice [ idStartsWithCap; (regex @"\d+") ]

let templateWithTail = positions (many1Strings2 (pstring "template" <|> pstring "tpl") templateTail) .>> IW |>>  Ast.TemplateType

let keywordObject = (skipString "object" <|> skipString "obj") .>> attemptSW >>% Ast.ObjectType 

let objectHeader = choice [
    keywordObject
    (attempt templateWithTail) <|> keywordTemplate
] 

let keywordPredicate = (skipString "predicate" <|> skipString "pred") .>> attemptSW >>% Ast.PredicateType
let keywordFunction = (skipString "function" <|> skipString "func") .>> attemptSW >>% Ast.FunctionalTermType


let theoryNamespace = aliasedNamespaceIdentifier <|> namespaceIdentifier .>> IW

let keywordUses = (skipString "uses") .>> SW
let usesClause = positions (keywordUses >>. theoryNamespace) |>> Ast.UsesClause

let extensionTail: Parser<unit,unit> = skipString ":end" >>. SW

let extensionHeader: Parser<unit,unit> = skipString ":ext" >>. SW

let extensionName = positions (idStartsWithCap .>> IW) |>> Ast.Extensionname

let extReg = regex "\/.*\/\s" <?> "<extension regex>"
let extensionRegex: Parser<_, unit>  = skipChar ':' >>. IW >>. extReg .>> IW |>> Ast.ExtensionRegex

let extensionBlock = positions (extensionHeader >>. IW >>. extensionName .>>. extensionRegex .>> extensionTail) |>> Ast.ExtensionBlock


(* Signatures, Variable Declarations, and Types, Ranges and Coordinates *)
// convention: All syntax production rules of FPL syntax extensions have to start with "ext", followed by
// a Pascal Case id.
// This ensures that they will not be mixed-up with original FPL ebnf productions
// that are all PascalCase as well as FPL keywords, that are all small case.

let xId = positions (at >>. extensionName) |>> Ast.ExtensionType 

let dollarDigits = positions (regex "\$\d+" <?> "<dollarDigits>") |>> Ast.DollarDigits

let atList = many at

let self = positions (atList .>> keywordSelf) |>> Ast.SelfAts

let entity = choice [ self ; variable ] 

let leftOpen = positions leftOpenBracket >>% Ast.LeftOpen
let leftClosed = positions leftClosedBracket >>% Ast.LeftClosed

let rightOpen = positions rightOpenBracket >>% Ast.RightOpen
let rightClosed = positions rightClosedBracket >>% Ast.RightClosed

let leftBound = leftOpen <|> leftClosed
let rightBound = rightOpen <|> rightClosed 
 
////// resolving recursive parsers
let statementList, statementListRef = createParserForwardedToRef()
let primePredicate, primePredicateRef = createParserForwardedToRef()
let predicate, predicateRef = createParserForwardedToRef()
let predicateList, predicateListRef = createParserForwardedToRef()
let predicateWithQualification, predicateWithQualificationRef = createParserForwardedToRef()
let paramTuple, paramTupleRef = createParserForwardedToRef()
let classType, classTypeRef = createParserForwardedToRef()

let coord = choice [ predicateWithQualification; dollarDigits ] .>> IW 

let fplIdentifier = choice [ entity; predicateIdentifier; extDigits ] 

let coordList = (sepBy1 coord comma) .>> IW

let bracketedCoords = positions (leftBracket >>. coordList .>> rightBracket) |>> Ast.BrackedCoordList

let fplRange = (opt coord.>> comma >>. opt coord) .>> IW

let boundedRange = positions (leftBound .>>. fplRange .>>. rightBound) |>> Ast.ClosedOrOpenRange

let coordInType = choice [ classType; variable; dollarDigits ] .>> IW 

let coordInTypeList = (sepBy1 coordInType comma) .>> IW 

let rangeInType = positions ((opt coordInType .>> comma) .>>. opt coordInType) |>> Ast.RangeInType

let specificClassType = choice [ objectHeader; xId; predicateIdentifier ] .>> IW

//// later semantics: Star: 0 or more occurrences, Plus: 1 or more occurrences
let varDeclModifier = choice [ colonStar; colonPlus; colon ] .>> IW

let bracketedCoordsInType = positions (leftBracket >>. coordInTypeList .>> rightBracket) |>> Ast.BracketedCoordsInType
let boundedRangeInType = positions (leftBound .>>. rangeInType .>>. rightBound) |>> Ast.BoundedRangeInType

// The classType is the last type in FPL we can derive FPL classes from.
// It therefore excludes the in-built FPL-types keywordPredicate, keywordFunction, and keywordIndex
// to restrict it to pure objects.
// In contrast to variableType which can also be used for declaring variables 
// in the scope of FPL building blocks
let bracketModifier = choice [boundedRangeInType ; bracketedCoordsInType ; paramTuple ]
classTypeRef.Value <- positions (specificClassType .>>. opt bracketModifier) |>> Ast.ClassType

let simpleVariableType = positions (choice [ keywordIndex; keywordFunction; keywordPredicate; classType ] .>> IW) |>> Ast.SimpleVariableType
let optionalTypeSpecification = opt (choice [boundedRange ; bracketedCoords; paramTuple])
let variableType = positions (simpleVariableType .>>. opt bracketModifier .>> IW) |>> Ast.VariableType

let namedVariableDeclaration = positions (variableList .>>. varDeclModifier .>>. variableType .>> IW) |>> Ast.NamedVarDecl
let namedVariableDeclarationList = sepBy namedVariableDeclaration comma

paramTupleRef.Value <- positions ((leftParen >>. IW >>. namedVariableDeclarationList) .>> (IW .>> rightParen)) |>> Ast.ParamTuple
let signature = positions ((predicateIdentifier .>> IW) .>>. paramTuple) .>> IW |>> Ast.Signature
let localizationString = positions (regex "\"[^\"\n]*\"") <?> "<language-specific string>" |>> Ast.LocalizationString

let keywordSymbol = pstring "symbol" .>> IW
let objectSymbolString = pchar '"' >>. objectMathSymbols .>> pchar '"'
let infixString = pchar '"' >>. infixMathSymbols .>> pchar '"'
let keywordInfix = pstring "infix" >>. IW
let postfixString = pchar '"' >>. postfixMathSymbols .>> pchar '"' 
let keywordPostfix = pstring "postfix" >>. IW
let prefixString = pchar '"' >>. prefixMathSymbols .>> pchar '"' 
let keywordPrefix = pstring "prefix" >>. IW
let userDefinedObjSym = positions (keywordSymbol >>. objectSymbolString) .>> IW |>> Ast.Symbol
let userDefinedInfix = positions (keywordInfix >>. infixString) .>> IW |>> Ast.Infix
let userDefinedPostfix = positions (keywordPostfix >>. postfixString) .>> IW |>> Ast.Postfix
let userDefinedPrefix = positions (keywordPrefix >>. prefixString) .>> IW |>> Ast.Prefix
let userDefinedSymbol = opt choice[ userDefinedPrefix; userDefinedInfix; userDefinedPostfix ]

let signatureWithUserDefinedString = positions (predicateIdentifier .>> IW .>>. userDefinedSymbol .>>. paramTuple) .>> IW |>> Ast.SignatureWithUserDefinedString
(* Statements *)
let argumentTuple = positions ((leftParen >>. predicateList) .>> (IW .>> rightParen)) |>> Ast.ArgumentTuple 

let word = regex @"\w+" <?> "<word>" .>> IW
let fplDelegateIdentifier = positions (keywordDel >>. dot >>. word) .>> IW |>> Ast.DelegateId
let fplDelegate = positions (fplDelegateIdentifier .>>. argumentTuple) |>> Ast.Delegate
let assignmentStatement = positions ((predicateWithQualification .>> IW .>> colonEqual) .>>. predicate) |>> Ast.Assignment

let variableRange = choice [ entity ; boundedRange] 

let spacesRightBrace = (IW .>> rightBrace) 

let keywordReturn = IW >>. (skipString "return" <|> skipString "ret") .>> SW 

let defaultResult = positions (IW >>. statementList) |>> Ast.DefaultResult
let conditionFollowedByResult = positions ((case >>. predicate .>> colon) .>>. (IW >>. statementList)) |>> Ast.ConditionFollowedByResult
let conditionFollowedByResultList = many1 (IW >>. conditionFollowedByResult)

let elseStatement = elseCase >>. IW >>. defaultResult .>> IW
let casesStatement = positions (((keywordCases >>. leftParen >>. IW >>. conditionFollowedByResultList .>>. elseStatement .>> rightParen))) |>> Ast.Cases

let allowedInDomainType = choice [keywordIndex; keywordFunction; keywordPredicate; specificClassType]
let inDomain = positions (keywordIn >>. (allowedInDomainType <|> variableRange) .>> IW) |>> Ast.Domain
let variableInOptDomain = ( (variable .>> IW) .>>. opt inDomain) .>> IW
let variableListInOptDomain = ( variableList .>>. opt inDomain) .>> IW
let variableListInOptDomainList = (sepBy1 variableListInOptDomain comma) .>> IW

let entityInDomain = ( entity .>> IW .>>. inDomain ) .>> IW
let forInBody = (entityInDomain .>> IW) .>>. (leftParen >>. IW >>. statementList) .>> (IW >>. rightParen)
let forStatement = positions (keywordFor >>. forInBody) |>> Ast.ForIn

//// Difference of assertion to an axiom: axiom's is followed by a signature of a predicate (i.e. with possible parameters),
//// not by a predicate (i.e. with possible arguments)
//// Difference of assertion to a mandatory property: a mandatory property introduces a completely new identifier inside
//// the scope of a definition. An assertion uses a predicate referring to existing identifiers in the whole theory
//// Difference of assertion to assume: the latter will be used only in the scope of proofs
let assertionStatement = positions (keywordAssert >>. predicate) |>> Ast.Assertion

let callConstructorParentClass = positions (keywordBaseClassReference >>. dot >>. specificClassType .>>. argumentTuple .>> IW) |>> Ast.ParentConstructorCall

let statement = 
    (choice [
        callConstructorParentClass
        casesStatement
        assertionStatement
        forStatement
        assignmentStatement
        fplDelegate
    ])

statementListRef.Value <- many (IW >>. statement .>> IW)

(* Predicates *)
let optionalSpecification = opt (choice [boundedRange ; bracketedCoords; argumentTuple])
let predicateWithOptSpecification = positions (fplIdentifier .>>. optionalSpecification) |>> Ast.PredicateWithOptSpecification
let dottedPredicate = positions (dot >>. predicateWithOptSpecification) |>> Ast.DottedPredicate
let qualificationList = positions (many dottedPredicate) |>> Ast.QualificationList

predicateWithQualificationRef.Value <- predicateWithOptSpecification .>>. qualificationList |>> Ast.PredicateWithQualification 

let dollarDigitList = many1 dollarDigits
let referencingIdentifier = positions (predicateIdentifier .>>. dollarDigitList) .>> IW |>> Ast.ReferencingIdentifier
let referenceToProofOrCorollary = positions (referencingIdentifier .>>. opt paramTuple) .>> IW |>> Ast.ReferenceToProofOrCorollary

let byDefinition = positions (keywordBydef >>. predicateWithQualification ) |>> Ast.ByDef 

// infix operators like the equality operator 
//let objectSymbol = positions (regex "[∟∠∞∅0-9]+" |>> fun (a:string) -> a.Trim()) .>> IW <?> "<object symbol>" |>> Ast.ObjectSymbol
let objectSymbol = positions ( objectMathSymbols ) .>> IW |>> Ast.ObjectSymbol

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

let conjunction = positions ((keywordAnd >>. leftParen >>. predicateList) .>> rightParen) |>> Ast.And
let disjunction = positions ((keywordOr >>. leftParen >>. predicateList) .>> rightParen) |>> Ast.Or
let exclusiveOr = positions ((keywordXor >>. leftParen >>. predicateList) .>> rightParen) |>> Ast.Xor

let twoPredicatesInParens = (leftParen >>. predicate) .>>. (comma >>. predicate) .>> rightParen 
let implication = positions (keywordImpl >>. twoPredicatesInParens) |>> Ast.Impl
let equivalence = positions (keywordIif >>. twoPredicatesInParens) |>> Ast.Iif
let negation = positions (keywordNot >>. predicate) |>> Ast.Not
let all = positions ((keywordAll >>. variableListInOptDomainList) .>>. predicate) |>> Ast.All
let exists = positions ((keywordEx >>. variableListInOptDomainList) .>>. predicate) |>> Ast.Exists

let existsTimesN = positions (((keywordExN >>. dollarDigits .>> SW) .>>. variableInOptDomain) .>>. predicate) |>> Ast.ExistsN
let isOpArg = choice [ objectSymbol; predicateIdentifier; variable; self ] .>> IW
let isOperator = positions ((keywordIs >>. leftParen >>. isOpArg) .>>. (comma >>. variableType) .>> rightParen) |>> Ast.IsOperator

// infix operators like the equality operator 
let infixOp = positions ( infixMathSymbols ) .>> SW |>> Ast.InfixOperator

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

//let fixCharacters = regex "['%!&~\+\-\*\/]+" |>> fun (a:string) -> a.Trim()
let postfixOp = positions ( postfixMathSymbols ) .>> IW |>> Ast.PostfixOperator
let prefixOp = positions ( prefixMathSymbols ) .>> IW |>> Ast.PrefixOperator
let expression = positions (opt prefixOp .>>. choice [compoundPredicate; primePredicate] .>>. opt postfixOp .>>. optionalSpecification .>>. qualificationList) .>> IW |>> Ast.Expression

predicateRef.Value <- expression

predicateListRef.Value <- sepBy predicate comma

(* FPL building blocks *)
let keywordDeclaration = (skipString "declaration" <|> skipString "dec") .>> SW 

let varDecl = tilde >>. namedVariableDeclaration
let varDeclBlock = positions (IW >>. keywordDeclaration >>. (many ((varDecl <|> statement) .>> IW)) .>> semiColon) .>> IW |>> Ast.VarDeclBlock 

let varDeclOrSpecList = opt (many1 (varDeclBlock)) 
(*To simplify the syntax definition, we do not define separate
FplPremiseConclusionBlocks for rules of inference and theorem-like blocks.
The first have a simplified, PL0 semantics, the latter have a more complex, predicative semantics.
However, there is a syntactical simplification of the signature*)
let spacesPredicate = IW >>. predicate
let premise = IW >>. (keywordPremise >>. colon >>. predicate) 
let conclusion = IW >>. (keywordConclusion >>. colon >>. predicate) 
let premiseConclusionBlock = leftBrace >>. varDeclOrSpecList .>>. premise .>>. conclusion .>> spacesRightBrace

(* FPL building blocks - rules of reference *)
let keywordInference = (skipString "inference" <|> skipString "inf") .>> SW 
let signatureWithPremiseConclusionBlock = signature .>>. premiseConclusionBlock |>> Ast.SignatureWithPreConBlock
let ruleOfInference = positions (keywordInference >>. signatureWithPremiseConclusionBlock) |>> Ast.RuleOfInference

(* FPL building blocks - Theorem-like statements and conjectures *)
let keywordTheorem = (skipString "theorem" <|> skipString "thm") .>> SW
let keywordLemma = (skipString "lemma" <|> skipString "lem") .>> SW
let keywordProposition = (skipString "proposition" <|> skipString "prop") .>> SW
let keywordCorollary = (skipString "corollary" <|> skipString "cor") .>> SW
let keywordConjecture = (skipString "conjecture" <|> skipString "conj") .>> SW

let theoremLikeBlock = leftBrace >>. varDeclOrSpecList .>>. spacesPredicate .>> spacesRightBrace
let signatureWithTheoremLikeBlock = signature .>>. theoremLikeBlock

let theorem = positions (keywordTheorem >>. signatureWithTheoremLikeBlock) |>> Ast.Theorem
let lemma = positions (keywordLemma >>. signatureWithTheoremLikeBlock) |>> Ast.Lemma
let proposition = positions (keywordProposition >>. signatureWithTheoremLikeBlock) |>> Ast.Proposition
let conjecture = positions (keywordConjecture >>. signatureWithTheoremLikeBlock) |>> Ast.Conjecture

let corollarySignature = referencingIdentifier .>>. paramTuple .>> IW
let corollary = positions (keywordCorollary >>. corollarySignature .>>. theoremLikeBlock) |>> Ast.Corollary

(* FPL building blocks - Axioms *)

let keywordAxiom = (skipString "axiom" <|> skipString "ax" <|> skipString "postulate" <|> skipString "post") >>. SW

let axiom = positions (keywordAxiom >>. signatureWithTheoremLikeBlock) |>> Ast.Axiom

(* FPL building blocks - Constructors *)

let keywordIntrinsic = (skipString "intrinsic" <|> skipString "intr") .>> IW >>% Ast.Intrinsic

let predContent = varDeclOrSpecList .>>. spacesPredicate |>> Ast.DefPredicateContent

let classContent = varDeclOrSpecList .>>. keywordSelf |>> Ast.DefClassContent
let keywordConstructor = (skipString "constructor" <|> skipString "ctor") .>> SW
let constructorBlock = leftBrace >>. varDeclOrSpecList .>>. keywordSelf .>> spacesRightBrace 
let constructor = positions (keywordConstructor >>. signature .>>. constructorBlock) |>> Ast.Constructor

(* FPL building blocks - Properties *)
let keywordOptional = positions (skipString "optional" <|> skipString "opt") .>> SW >>% Ast.Optional
let keywordProperty = positions (skipString "property" <|> skipString "prty") .>> SW >>% Ast.Property

let predInstanceBlock = leftBrace >>. (keywordIntrinsic <|> predContent) .>> spacesRightBrace
let predicateInstance = positions (keywordPredicate >>. signature .>>. (IW >>. predInstanceBlock)) |>> Ast.PredicateInstance

let classInstanceBlock = leftBrace >>. (keywordIntrinsic <|> classContent) .>> spacesRightBrace
let classInstance = positions (variableType .>>. signature .>>. classInstanceBlock) |>> Ast.ClassInstance
let mapping = toArrow >>. IW >>. variableType
let functionalTermSignature = (keywordFunction >>. signatureWithUserDefinedString) .>>. (IW >>. mapping) .>> IW 

let returnStatement = positions (keywordReturn >>. (fplDelegate <|> predicateWithQualification)) .>> IW |>> Ast.Return
let funcContent = varDeclOrSpecList .>>. returnStatement |>> Ast.DefFunctionContent
let functionalTermInstanceBlock = leftBrace >>. (keywordIntrinsic <|> funcContent) .>> spacesRightBrace
let functionalTermInstance = positions (functionalTermSignature .>>. functionalTermInstanceBlock) |>> Ast.FunctionalTermInstance

let definitionProperty = choice [
    predicateInstance
    functionalTermInstance
    classInstance
]
let propertyHeader = IW >>. keywordProperty .>>. opt keywordOptional 
let property = positions (propertyHeader .>>. definitionProperty) |>> Ast.PropertyBlock
let propertyList = opt (many1 (property .>> IW)) 

(* FPL building blocks - Proofs 

    # A Proof relates to the PredicateIdentifier of the Theorem.
    # Because proofs are named, they can stand anywhere inside the theory, not only immediately
    # after the Theorem they prove. This is to enable the users to mix
    # with natural language an provide a proof long after the theorem was stated.

*)
// justifying proof arguments can be the identifiers of Rules of References, conjectures, theorem-like statements, or axioms
let keywordRevoke = (skipString "revoke" <|> skipString "rev") .>> SW 
let revokeArgument = positions (keywordRevoke >>. argumentIdentifier) |>> Ast.RevokeArgument 
    
let keywordAssume = skipString "assume" <|> skipString "ass" .>> SW 
let assumeArgument = positions (keywordAssume >>. predicate) |>> Ast.AssumeArgument
let keywordTrivial  = positions (skipString "trivial") .>> IW |>> Ast.Trivial
let keywordQed  = positions (skipString "qed") .>> IW |>> Ast.Qed
let derivedPredicate = predicate |>> Ast.DerivedPredicate
let derivedArgument = choice [
    keywordTrivial 
    derivedPredicate
]

let argumentInference = vDash >>. IW >>. (assumeArgument<|> revokeArgument <|> derivedArgument)
let justification = positions (predicateList .>> IW) |>> Ast.Justification
let argument = positions (justification .>>. argumentInference) |>> Ast.JustifiedArgument
let proofArgument = positions ((argumentIdentifier .>> IW) .>>. argument) .>> IW |>> Ast.Argument
let proofArgumentList = many1 (IW >>. (proofArgument <|> varDeclBlock))
let keywordProof = (skipString "proof" <|> skipString "prf") .>> SW 
let proofBlock = leftBrace >>. proofArgumentList .>>. opt keywordQed .>> spacesRightBrace
let proof = positions ((keywordProof >>. referencingIdentifier) .>>. (IW >>. proofBlock)) |>> Ast.Proof

(* FPL building blocks - Definitions *)

// Predicate building blocks can be defined similarly to classes, they can have properties but they cannot be derived any parent type 
let predicateDefinitionBlock = leftBrace  >>. ((keywordIntrinsic <|> predContent) .>> IW) .>>. propertyList .>> spacesRightBrace 
let definitionPredicate = positions (keywordPredicate >>. (signatureWithUserDefinedString .>> IW) .>>. predicateDefinitionBlock) |>> Ast.DefinitionPredicate

// Functional term building blocks can be defined similarly to classes, they can have properties but they cannot be derived any parent type 
let functionalTermDefinitionBlock = leftBrace  >>. ((keywordIntrinsic <|> funcContent) .>> IW) .>>. propertyList .>> spacesRightBrace
let definitionFunctionalTerm = positions ((functionalTermSignature .>> IW) .>>. functionalTermDefinitionBlock) |>> Ast.DefinitionFunctionalTerm

// Class definitions
let keywordClass = (skipString "class" <|> skipString "cl") >>. SW

let constructorList = many1 (constructor .>> IW)
let classCompleteContent = varDeclOrSpecList .>>. constructorList|>> Ast.DefClassCompleteContent
let classDefinitionBlock = leftBrace  >>. ((keywordIntrinsic <|> classCompleteContent) .>> IW) .>>. propertyList .>> spacesRightBrace
let classTypeWithModifier = positions (varDeclModifier .>>. classType .>> IW) |>> Ast.ClassTypeWithModifier
let classTypeWithModifierList = sepBy1 classTypeWithModifier comma


let classSignature = (keywordClass >>. predicateIdentifier .>> IW) .>>. opt userDefinedObjSym .>>. classTypeWithModifierList
let definitionClass = positions ((classSignature .>> IW) .>>. classDefinitionBlock) |>> Ast.DefinitionClass 

let keywordDefinition = (skipString "definition" <|> skipString "def") >>. SW
let definition = keywordDefinition >>. choice [
    definitionClass
    definitionPredicate
    definitionFunctionalTerm
]
(* Gathering together all Building Blocks to a theory *)

(* Localizations *)
// Localizations provide a possibility to automatically translate FPL expressions into natural languages
let keywordLocalization = (skipString "localization" <|> skipString "loc") >>. SW
let localizationLanguageCode: Parser<string,unit> = regex @"[a-z]{3}" <?> "<ISO 639 language code>"

let ebnfTransl, ebnfTranslRef = createParserForwardedToRef()
let ebnfTranslTuple = (leftParen >>. IW >>. ebnfTransl) .>> (IW .>> rightParen) 
let ebnfFactor = choice [
    variable
    localizationString
    ebnfTranslTuple
] 
let ebnfTerm = positions (sepEndBy1 ebnfFactor SW) |>> Ast.LocalizationTerm
ebnfTranslRef.Value <-  positions (sepBy1 ebnfTerm (IW >>. case >>. IW)) |>> Ast.LocalizationTermList
let translation = (exclamationMark >>. localizationLanguageCode .>> IW .>> colon) .>>. ebnfTransl |>> Ast.Translation
let translationList = many1 (IW >>. translation .>> IW)
let localization = positions (keywordLocalization >>. (predicate .>> IW .>> colonEqual) .>>. (translationList .>> IW .>> semiColon)) .>> IW |>> Ast.Localization

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
]

let buildingBlockList = many (buildingBlock .>> IW)

(* Namespaces *)
let fplNamespace = ((opt extensionBlock .>> IW) .>>. buildingBlockList) .>> IW .>> semiColon |>> Ast.Namespace
(* Final Parser *)
let ast =  positions (IW >>. fplNamespace) |>> Ast.AST

let stdErrInfo = ("SYN000", "Other syntax error", [], ast)
let stdCode, stdErrMsg, _, stdParser = stdErrInfo
let stdErrInfo1 = ("SYN001", "Characters found after namespace", [], ast)
let stdCode1, stdErrMsg1, _, stdParser1 = stdErrInfo1

let calculateCurrentContext (matchList:System.Collections.Generic.List<int>) i = 
    let index = matchList[i]
    if i + 1 < matchList.Count - 1 then
        let nextIndex = matchList[i+1]
        index, nextIndex
    else
        index, index

let errRecPattern = "(definition|def|mandatory|mand|optional|opt|axiom|ax|postulate|post|theorem|thm|proposition|prop|lemma|lem|corollary|cor|conjecture|conj|declaration|dec|constructor|ctor|proof|prf|inference|inf|localization|loc|uses|and|or|impl|iif|xor|not|all|exn|ex|is|assert|cases|self\!|for|delegate|del|\|\-|\||\?|assume|ass|revoke|rev|return|ret)\W|(conclusion|con|premise|pre)\s*\:|(~|\!)[a-z]"

let errInformation = [
    ("DEF000", "Syntax error in definition", ["def"], definition)
    ("PRP000", "Syntax error in property", ["mand"; "opt"], property)
    ("AXI000", "Syntax error in axiom", ["ax"; "post"], axiom)
    ("THM000", "Syntax error in theorem", ["theorem"; "thm"], theorem)
    ("COR000", "Syntax error in corollary", ["theorem"; "cor"], corollary)
    ("LEM000", "Syntax error in lemma", ["lem"], lemma)
    ("PPS000", "Syntax error in proposition", ["prop"], proposition)
    ("CNJ000", "Syntax error in conjecture", ["conj"], conjecture)
    ("VAR000", "Syntax error in variable declaration and/or specification", ["dec"], varDeclBlock)
    ("CTR000", "Syntax error in constructor", ["constructor"; "ctor"], constructor)
    ("PRF000", "Syntax error in proof", ["proof"; "prf"], proof)
    ("INF000", "Syntax error in rule of inference", ["inf"], ruleOfInference)
    ("LOC000", "Syntax error in rule of localization", ["loc"], localization)
    ("USE000", "Syntax error in uses clause", ["uses"], usesClause)
    ("PRE000", "Syntax error in predicate", ["and"; "or"; "impl"; "iif"; "xor"; "not"; "all"; "ex"; "is"], compoundPredicate)
    ("SMT000", "Syntax error in statement", ["assert"; "cases"; "base"; "for"; "del"], statement)
    ("AGI000", "Syntax error in proof argument", ["|-"], argumentInference)
    ("CAS000", "Syntax error in case block", ["|"], conditionFollowedByResult)
    ("DCS000", "Syntax error in default case block", ["?"], elseStatement)
    ("ASS000", "Syntax error in assumption", ["ass"], assumeArgument)
    ("REV000", "Syntax error in revocation", ["rev"], revokeArgument)
    ("RET000", "Syntax error in return statement", ["ret"], returnStatement)
    ("PRE000", "Syntax error in premise", ["pre"], premise)
    ("CON000", "Syntax error in conclusion", ["con"], conclusion)
    ("TRL000", "Syntax error in translation", ["!"], translation)
    ("TYD000", "Syntax error in type declaration", ["~"], varDecl)
]

/// Finds the error information tuple based on a prefix of a string from the errInformation list. 
/// If no prefix matches than the SYN000 tuple will be returned.
let findErrInfoTuple (str:string) =
    match List.tryFind (fun (_, _, prefixes, _) -> List.exists (fun prefix -> str.StartsWith(prefix : string)) prefixes) errInformation with
    | Some tuple -> tuple
    | None -> stdErrInfo


let findFirstIndexInMatches (matchList:System.Collections.Generic.List<int>) pIndex kMax =
    let rec loop i =
        if i >= matchList.Count then 
            kMax
        else 
            let index = matchList[i]
            if index > pIndex then 
                i
            else 
                loop (i + 1)
    loop 0

let maxIntervalBound (intervals:System.Collections.Generic.List<Interval>) =
    let mutable maxBound = -1
    for interval in intervals do
        if interval.End > maxBound then
            maxBound <- interval.End
    maxBound

let fplParser (input:string) = 
    let preProcessedInput = preParsePreProcess input
    let matchList = stringMatches preProcessedInput errRecPattern

    let intervals = new System.Collections.Generic.List<Interval>()

    let parseResult, pIndex = tryParseFirstError stdParser ad preProcessedInput stdCode stdErrMsg 
    intervals.Add(Interval(0, pIndex))

    let mutable lastParserIndex = 0
    let mutable lastParser = stdParser
    let mutable lastCode = stdCode
    let mutable lastMsg = stdErrMsg
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
                // emit error messages for for this chunk of input string using the last parser  
                tryParseRemainingChunk lastParser ad remainingChunk lastParserIndex index lastCode lastMsg -1 ""
                intervals.Add(Interval(lastParserIndex, nextIndex))
                lastParserIndex <- nextIndex
            else
                // otherwise, find the next error info tuple based on the current substring
                let code, errMsg, prefixList, errRecParser = findErrInfoTuple subString
                // try to parse substring using the parser from the error info and emitting diagnostics (if any)
                let pResult, pIndex, pSuccess = tryParse errRecParser ad subString index nextIndex code errMsg -1 ""
                intervals.Add(Interval(index, pIndex))
                lastParserIndex <- pIndex
                lastParser <- errRecParser
                lastCode <- code
                lastMsg <- errMsg
                lastSuccess <- pSuccess
    // emit diagnostics for any error positions that are not overlayed by the intervals
    tryParseRemainingOnly stdParser ad preProcessedInput stdCode stdErrMsg intervals -1 ""
    // Return an ast on a best effort basis even if there were some errors 
    let resultingAst = tryGetAst stdParser preProcessedInput -1

    let maxBound = maxIntervalBound intervals
    let remaingString = preProcessedInput.Substring(maxBound).TrimEnd()
    if not (remaingString.EndsWith("}") && Regex.Matches(preProcessedInput, "\}").Count = Regex.Matches(preProcessedInput, "\{").Count) then
        // prevent emitting "false-positive" errors of characters found after namespace using the heuristic that 
        // the last character of a namespace is "}" and then looks "good"
        tryParseRemainingChunk stdParser1 ad (preProcessedInput.Substring(maxBound)) maxBound (preProcessedInput.Length) stdCode1 stdErrMsg1 -1 ""
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
