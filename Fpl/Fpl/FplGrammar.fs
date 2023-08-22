module FplGrammar
open FParsec
open FplGrammarTypes

(* Literals *)

let rightBrace: Parser<_, unit>= skipChar '}'
let leftBrace: Parser<_, unit> = skipChar '{'
let leftParen: Parser<_, unit> = skipChar '('
let rightParen: Parser<_, unit> = skipChar ')'
let comma: Parser<_, unit> = skipChar ','
let star: Parser<_, unit> = skipChar '*'
let plus: Parser<_, unit> = skipChar '+'
let dot: Parser<_, unit> = skipChar '.'
let colon: Parser<_, unit> = skipChar ':'
let colonEqual: Parser<_, unit> = skipString ":="
let at: Parser<char, unit> = pchar '@'
let exclamationMark: Parser<_, unit> = skipChar '!'
let leftBracket: Parser<_, unit> = skipChar '['
let rightBracket: Parser<_, unit> = skipChar ']'
let tilde: Parser<_, unit> = skipChar '~'
let dollar: Parser<_, unit> = skipChar '$'
let slash: Parser<_, unit> = skipChar '/'

(* Whitespaces and Comments *)

let IW = spaces

let SW = spaces1

let Comment: Parser<_, unit> = regex @"\/\/[^\n]*" |>> ignore

let LongComment: Parser<_, unit> = regex @"\/\*((?:.|\n)*?)\*\/" |>> ignore

let CW = choice [
    SW
    Comment
    LongComment
]

let spacesRightParenSpaces = spaces >>. rightParen >>. spaces
let spacesLeftParenSpaces = spaces >>. leftParen >>. spaces
let commaSpaces = comma >>. spaces

// -----------------------------------------------------
// Extensions of the FPL language (have to be dynamic)! Lacking a pre-processor, we put the rules
// from the Proof of Concept of FPL code manually into the EBNF of the core FPL grammar.
// note that this has to be inserted into:
// the IsOperand choice
// the PredicateOrFunctionalTerm choice
let extDigits: Parser<_, unit> = regex @"\d+" |>> Predicate.ExtDigits

(* Identifiers *)

let IdStartsWithSmallCase: Parser<string,unit>  = regex @"[a-z][a-z0-9A-Z_]*" 
let IdStartsWithCap: Parser<string,unit> = regex @"[A-Z][a-z0-9A-Z_]*" 
let digits: Parser<string,unit> = regex @"\d+" 
let digitsIdSmallCase: Parser<string, unit> = regex @"\d+[a-z][a-z0-9A-Z_]*"
let namespaceIdentifier = sepBy1 IdStartsWithCap dot |>> FplIdentifierType.NamespaceIdentifier
let wildcardedNamespaceIdentifier = many1 (IdStartsWithCap .>> dot) .>> skipString "*" |>> FplIdentifierType.WildcaredNamespaceIdentifier
let alias = SW .>> skipString "alias" >>. SW >>. IdStartsWithCap
let aliasedNamespaceIdentifier = sepBy1 IdStartsWithCap dot .>>. alias |>> FplIdentifierType.AliasedNamespaceIdentifier
let variable = IdStartsWithSmallCase |>> FplIdentifierType.Var
let variableList = sepEndBy1 variable commaSpaces
let argumentIdentifier = choice [
    digitsIdSmallCase
    digits
]
let argumentParam = slash >>. argumentIdentifier

(* Fpl Keywords *)
let keywordDel: Parser<_,unit> = skipString "del"
let keywordInference: Parser<_,unit> = skipString "inference" 
let keywordInf: Parser<_,unit> = skipString "inf"
let keywordSelf: Parser<_,unit> = skipString "self"

let inference = choice [
    keywordInference
    keywordInf
] 

let keywordUndefined: Parser<_,unit> = skipString "undefined" >>% Predicate.Undefined
let keywordUndef: Parser<_,unit> = skipString "undef" >>% Predicate.Undefined
let undefined = choice [
    keywordUndefined
    keywordUndef
] 

let keywordReturn: Parser<_,unit> = skipString "return" 
let keywordRet: Parser<_,unit> = skipString "ret" 
let returnHeader = choice [
    keywordReturn
    keywordRet
] 

let keywordIndex: Parser<_,unit> = skipString "index" 
let keywordInd: Parser<_,unit> = skipString "ind" 

let indexHeader = choice [
    keywordIndex
    keywordInd
] 

(* Statement-related Keywords *)
let keywordRange: Parser<_,unit> = skipString "range"
let keywordLoop: Parser<_,unit> = skipString "loop"
let keywordCases: Parser<_,unit> = skipString "cases"
let keywordCase: Parser<_,unit> = skipString "case"
let keywordElse: Parser<_,unit> = skipString "else"
let keywordAssert: Parser<_,unit> = skipString "assert"

(* Predicate-related Keywords *)
let keywordTrue: Parser<_,unit> = skipString "true" >>% Predicate.True  
let keywordFalse: Parser<_,unit> = skipString "false" >>% Predicate.False  
let keywordAnd: Parser<_,unit> = skipString "and"
let keywordOr: Parser<_,unit> = skipString "or"
let keywordImpl: Parser<_,unit> = skipString "impl"
let keywordIif: Parser<_,unit> = skipString "iif"
let keywordXor: Parser<_,unit> = skipString "xor"
let keywordNot: Parser<_,unit> = skipString "not"
let keywordAll: Parser<_,unit> = skipString "all"
let keywordEx: Parser<_,unit> = skipString "ex" 
let keywordIs: Parser<_,unit> = skipString "is"

// Via templates, FPL supports generic types, which make it possible to define abstract mathematical
// objects and their properties that defer the concrete
// specification of one or more types until the definition or method is declared and instantiated by
// client code
let keywordTemplate: Parser<_,unit> = pstring "template"  |>> FplIdentifierType.Template  
let keywordTpl: Parser<_,unit> = pstring "tpl"   |>> FplIdentifierType.Template
let templateHeader = choice [
    keywordTemplate
    keywordTpl
]

let templateTail = choice [
    IdStartsWithCap
    digits
]

let longTemplateWithTail = manyStrings2 (pstring "template") templateTail |>> FplIdentifierType.LongTemplate
let shortTemplateWithTail = manyStrings2 (pstring "tpl") templateTail |>> FplIdentifierType.LongTemplate

let longTemplate = choice [
    longTemplateWithTail
    shortTemplateWithTail
]

let keywordObject: Parser<_,unit> = skipString "object" >>% FplIdentifierType.Object 
let keywordObj: Parser<_,unit> = skipString "obj" >>% FplIdentifierType.Object

let object = choice [
    keywordObject
    keywordObj
]

let objectHeader = choice [
    object
    longTemplate
    templateHeader
] 

let keywordTheorem: Parser<_,unit> = skipString "theorem" 
let keywordThm: Parser<_,unit> = skipString "thm" 

let theorem = choice [
    keywordTheorem
    keywordThm
] 

let keywordLemma: Parser<_,unit> = skipString "lemma"  
let keywordLem: Parser<_,unit>  = skipString "lem" 

let lemma = choice [
    keywordLemma
    keywordLem
]

let keywordProposition: Parser<_,unit> = skipString "proposition" 
let keywordProp: Parser<_,unit> = skipString "prop" 

let proposition = choice [
    keywordProposition
    keywordProp
]

let keywordCorollary: Parser<_,unit> = skipString "corollary" 
let keywordCor: Parser<_,unit> = skipString "cor" 

let corollary = choice [
    keywordCorollary
    keywordCor
]

let keywordConjecture: Parser<_,unit> = skipString "conjecture" 
let keywordConj: Parser<_,unit> = skipString "conj" 

let conjecture = choice [
    keywordConjecture
    keywordConj
]

let keywordPredicate: Parser<_,unit> = skipString "predicate" >>% FplIdentifierType.PredicateHeader
let keywordPred: Parser<_,unit> = skipString "pred" >>% FplIdentifierType.PredicateHeader

let predicateHeader = choice [
    keywordPredicate
    keywordPred
]

let keywordFunction: Parser<_,unit> = skipString "function" >>% FplIdentifierType.FunctionalTermHeader
let keywordFunc: Parser<_,unit> = skipString "func" >>% FplIdentifierType.FunctionalTermHeader

let functionalTermHeader = choice [
    keywordFunction
    keywordFunc
]


let TheoremLikeStatementOrConjectureHeader = choice [
    conjecture
    corollary
    proposition
    lemma
    theorem
]

let wildcardTheoryNamespace = 
    (attempt ((attempt aliasedNamespaceIdentifier) <|>
    wildcardedNamespaceIdentifier) <|>    
    namespaceIdentifier) .>> IW

let wildcardTheoryNamespaceList = sepEndBy1 wildcardTheoryNamespace (comma >>. IW) 

let usesClause = skipString "uses" >>. SW >>. wildcardTheoryNamespaceList |>> UsesClause.UsesClause

let extensionTail: Parser<unit,unit> = skipString ":end" >>. SW

let extensionHeader: Parser<unit,unit> = skipString ":ext" 

let extensionName = IW >>. skipString "ext" >>. IdStartsWithCap .>> IW |>> Extensions.Extensionname

let extensionRegex: Parser<_, unit>  = skipChar ':' >>. IW >>. regex @"\/(?!:end).*" .>> IW |>> Extensions.ExtensionRegex

let extensionBlock = IW >>. extensionHeader >>. extensionName .>>. extensionRegex .>> extensionTail |>> ExtensionBlock.ExtensionBlock


(* Signatures, Variable Declarations, and Types, Ranges and Coordinates *)
let xId = at >>. extensionName |>> FplIdentifierType.Xid

let predicateIdentifier = sepBy1 IdStartsWithCap dot |>> FplIdentifierType.AliasedId

let indexValue = (IdStartsWithSmallCase .>> dollar) .>>. ( digits <|> IdStartsWithSmallCase ) |>> FplIdentifierType.IndexVariable

let atList = many at

let self = atList .>> keywordSelf |>> FplIdentifierType.Self

let entity = choice [
    self
    variable
]

let leftOpen = leftBracket >>. IW >>. exclamationMark >>% FplIdentifierType.LeftOpen
let leftClosed = leftBracket >>. IW >>% FplIdentifierType.LeftClosed

let leftBound = ((attempt leftOpen) <|> leftClosed)

let rightBound = choice [
    exclamationMark >>. IW >>. rightBracket >>% FplIdentifierType.RightOpen
    IW >>. rightBracket >>% FplIdentifierType.RightClosed
]
 


////// resolving recursive parsers
//let statementList, statementListRef = createParserForwardedToRef()
let primePredicate, primePredicateRef = createParserForwardedToRef()
let coordOfEntity, coordOfEntityRef = createParserForwardedToRef()
let predicate, predicateRef = createParserForwardedToRef()
let predicateList, predicateListRef = createParserForwardedToRef()
let predicateWithArguments, predicateWithArgumentsRef = createParserForwardedToRef()
//let paramTuple, paramTupleRef = createParserForwardedToRef()

let entityWithCoord = entity .>>. coordOfEntity |>> FplIdentifierType.EntityWithCoord

let assignee = choice [
    entity
    entityWithCoord
]

let fplIdentifier = choice [
    predicateIdentifier
    assignee
]


let coordList = sepEndBy1 assignee commaSpaces

let bracketedCoordList = (leftBracket >>. IW >>. coordList) .>> (IW >>. rightBracket) |>> FplIdentifierType.BrackedCoordList

let fplRange = ((opt assignee) .>> IW .>> tilde) .>>. (IW >>. opt assignee)

let closedOrOpenRange = (leftBound .>> IW) .>>. fplRange .>>. (IW >>. rightBound) |>> FplIdentifierType.ClosedOrOpenRange

coordOfEntityRef := choice [
    closedOrOpenRange
    bracketedCoordList
]

let coordInType = choice [
    fplIdentifier
    indexValue
    variable
]

let rangeInType = (opt coordInType .>> IW) .>>. (tilde >>. IW >>. opt coordInType) |>> FplIdentifierType.RangeInType

let specificType = choice [
    predicateIdentifier
    xId
    predicateHeader
    functionalTermHeader
    objectHeader
]

//// later semantics: Star: 0 or more occurrences, Plus: 1 or more occurrences
let callModifier = choice [
    star
    plus
]

let specificTypeWithCoord = ((specificType .>> IW) .>>. leftBound) .>>. (( rangeInType <|> coordInType ) .>>. rightBound) |>> FplIdentifierType.SpecificTypeWithCoord

let generalType = opt callModifier .>>. (specificTypeWithCoord <|> specificType)

//let parenthesisedGeneralType = generalType .>> IW >>. paramTuple

//let variableType = choice [
//    parenthesisedGeneralType
//    generalType
//    indexHeader
//]

//let namedVariableDeclaration = variableList .>> IW >>. colon >>. variableType

//let namedVariableDeclarationList = sepEndBy1 namedVariableDeclaration (IW >>. comma >>. IW)

//let paramTupleRef = leftParen >>. IW >>. namedVariableDeclarationList .>> IW .>> rightParen

//let signature = predicateIdentifier .>> IW >>. paramTuple |>> BlockHeader.Signature

////(* Statements *)
//let fplDelegateIdentifier = regex @"[a-z_]+"
//let fplDelegate = keywordDel >>. dot >>. fplDelegateIdentifier .>> IW >>. leftParen .>> IW >>. predicateList .>> IW .>> rightParen
//let assignmentStatement = assignee .>> IW >>. colonEqual >>. IW >>. predicate
//let returnStatement = returnHeader >>. SW >>. predicate
//let keysOfVariadicVariable = variable .>> dollar

//let variableRange = choice [
//    keysOfVariadicVariable
//    closedOrOpenRange
//    assignee
//]


//let defaultResult = keywordElse >>. IW >>. colon >>. many CW >>. statementList
//let conditionFollowedByResult = keywordCase >>. SW >>. predicate .>> colon >>. many CW >>. statementList
//let conditionFollowedByResultList = sepBy1 conditionFollowedByResult ( many CW )


//let caseStatement = keywordCases >>. many CW >>. leftParen >>. many CW >>. conditionFollowedByResultList .>> many CW >>. defaultResult .>> many CW >>. rightParen
//let rangeOrLoopBody = assignee .>> SW >>. variableRange .>> many CW >>. leftParen >>. many CW >>. statementList .>> many CW >>. rightParen
//let loopStatement = keywordLoop >>. SW >>. rangeOrLoopBody
//let rangeStatement = keywordRange >>. SW >>. rangeOrLoopBody

//// Difference of assertion to an axiom: axiom's is followed by a signature of a predicate (i.e. with possible parameters),
//// not by a predicate (i.e. with possible arguments)
//// Difference of assertion to a mandatory property: a mandatory property introduces a completely new identifier inside
//// the scope of a definition. An assertion uses a predicate referring to existing identifiers in the whole theory
//let assertionStatement = keywordAssert >>. SW >>. predicate

//let statement = choice [
//    fplDelegate
//    caseStatement
//    assertionStatement
//    assignmentStatement
//    rangeStatement
//    loopStatement
//    returnStatement
//]

//let statementListRef =  sepBy1 (statement .>> SW)  (many CW )

//(* Predicates *)

predicateWithArgumentsRef := (fplIdentifier .>> spacesLeftParenSpaces) .>>. (predicateList .>> spacesRightParenSpaces) |>> Predicate.PredicateWithArgs

let isOperator = optional CW >>. keywordIs >>. spacesLeftParenSpaces >>. ( indexValue <|> fplIdentifier ) .>> IW >>. generalType .>> spacesRightParenSpaces

primePredicateRef := choice [
    keywordTrue
    keywordFalse
    undefined
    predicateWithArguments
    //statement
    //indexValue
    //isOperator
    //argumentParam
    extDigits
]

let conjunction = (many CW >>. keywordAnd >>. spacesLeftParenSpaces >>. predicateList) .>> spacesRightParenSpaces |>> Predicate.And
let disjunction = (many CW >>. keywordOr >>. spacesLeftParenSpaces >>. predicateList) .>> spacesRightParenSpaces |>> Predicate.Or

let twoPredicatesInParens = (spacesLeftParenSpaces >>. predicate) .>>. (commaSpaces >>. predicate) .>> spacesRightParenSpaces 
let onePredicateInParens = (spacesLeftParenSpaces >>. predicate) .>> spacesRightParenSpaces
let implication = many CW >>. keywordImpl >>. twoPredicatesInParens |>> Predicate.Impl
let equivalence = many CW >>. keywordIif >>. twoPredicatesInParens |>> Predicate.Iif
let exclusiveOr = many CW >>. keywordXor >>. twoPredicatesInParens |>> Predicate.Xor
let negation = many CW >>. keywordNot >>. onePredicateInParens |>> Predicate.Not
let all = many CW >>. (keywordAll >>. SW >>. variableList) .>>. onePredicateInParens |>> Predicate.All
let exists = many CW >>. (keywordEx >>. SW >>. variableList) .>>. onePredicateInParens |>> Predicate.Exists
let existsTimesN = many CW >>. ((keywordEx >>. dollar >>. digits) .>>. (SW >>. variableList)) .>>. onePredicateInParens |>> Predicate.ExistsN

// A compound Predicate has its own boolean expressions to avoid mixing up with Pl0Propositions
let compoundPredicate = choice [
    conjunction
    disjunction
    implication
    equivalence
    exclusiveOr
    negation
    all
    exists
    existsTimesN
]

predicateRef := choice [
    compoundPredicate
    primePredicate
]

predicateListRef := sepBy1 predicate commaSpaces 
//(* FPL building blocks *)

//(*To simplify the syntax definition, we do not define separate
//FplPremiseConclusionBlocks for rules of inference and theorem-like blocks.
//The first have a simplified, PL0 semantics, the latter have a more complex, predicative semantics.
//However, there is a syntactical simplification of the signature*)
//let ruleOfInference = signature .>>. IW >>. premiseConclusionBlock |>> BlockHeader.Inference

//let ruleOfInferenceList = sepEndBy1 ruleOfInference (many CW)

//let rulesOfInferenceBlock = inference >>. many CW >>. leftBrace >>. many CW >>. ruleOfInferenceList >>. many CW >>. rightBrace

