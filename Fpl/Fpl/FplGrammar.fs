module FplGrammar
open FParsec
open FplGrammarTypes

(* FPL Version 2.0.0 (combined Language Grammar and working Parser version) *)

(* Literals *)

let rightBrace: Parser<_, unit>= skipChar '}'
let leftBrace: Parser<_, unit> = skipChar '{'
let leftParen: Parser<_, unit> = skipChar '('
let rightParen: Parser<_, unit> = skipChar ')'
let comma: Parser<_, unit> = skipChar ','
let star: Parser<_, unit> = skipChar '*' >>% FplType.Many
let plus: Parser<_, unit> = skipChar '+' >>% FplType.Many1
let dot: Parser<_, unit> = skipChar '.'
let colon: Parser<_, unit> = skipChar ':'
let colonEqual: Parser<_, unit> = skipString ":="
let at: Parser<char, unit> = pchar '@'
let exclamationMark: Parser<_, unit> = skipChar '!'
let case: Parser<_,unit> = skipChar '|'
let leftBracket: Parser<_, unit> = skipChar '['
let rightBracket: Parser<_, unit> = skipChar ']'
let tilde: Parser<_, unit> = skipChar '~'
let dollar: Parser<_, unit> = skipChar '$'
let slash: Parser<_, unit> = skipChar '/'
let toArrow: Parser<_, unit> = skipString "->"

(* Whitespaces and Comments *)

let IW = spaces

let SW = spaces1

let Comment: Parser<_, unit> = regex @"\/\/[^\n]*" |>> ignore <?> "<line-comment>"

let LongComment: Parser<_, unit> = regex @"\/\*((?:.|\n)*?)\*\/" |>> ignore <?> "<multiline-comment>"

let CW = choice [
    LongComment
    Comment
    SW
]

let spacesLeftParenSpaces = spaces >>. leftParen >>. spaces
let spacesRightParenSpaces = spaces >>. rightParen >>. spaces
let commaSpaces = comma >>. spaces

// -----------------------------------------------------
// Extensions of the FPL language (have to be dynamic)! Lacking a pre-processor, we put the rules
// from the Proof of Concept of FPL code manually into the EBNF of the core FPL grammar.
// note that this has to be inserted into:
// the IsOperand choice
// the PredicateOrFunctionalTerm choice
let extDigits: Parser<_, unit> = regex @"\d+" |>> FplIdentifier.ExtDigits

(* Identifiers *)
(* Fpl Keywords *)

let keyWordSet =
    System.Collections.Generic.HashSet<_>(
        [|
        "alias"; 
        "all"; 
        "and,"; 
        "assert"; 
        "ass"; "assume"; 
        "ax"; 
        "axiom"; 
        "case";
        "cl"; "class"; 
        "conj"; "conjecture"; 
        "con"; "conclusion"; 
        "cor"; "corollary";
        "del"; "delegate"
        "else";
        "end";
        "ext";
        "ex";
        "false";
        "func"; "function";
        "iif";
        "impl";
        "ind"; "index";
        "inf"; "inference";
        "is";
        "lem"; "lemma";
        "loc"; "localization";
        "loop";
        "mand"; "mandatory";
        "not";
        "obj"; "object";
        "opt"; "optional";
        "or";
        "post"; "postulate";
        "pred"; "predicate";
        "pre"; "premise";
        "prf"; "proof";
        "prop"; "proposition";
        "qed";
        "range";
        "ret"; "return";
        "rev"; "revoke";
        "self";
        "thm"; "theorem";
        "th"; "theory";
        "tpl"; "template";
        "trivial";
        "true";
        "undef"; "undefined";
        "uses";
        "xor";
        |]
    )

let fplKeyword: Parser<string,unit>  = 
    regex @"[a-z]+" >>= (
        fun s -> if keyWordSet.Contains(s) then (preturn s) else fail "not an FPL keyword"
    ) 

let IdStartsWithSmallCase: Parser<string,unit> = regex @"[a-z][a-z0-9A-Z_]*" >>= (
            fun s -> 
                if keyWordSet.Contains(s) then fail "keyword not applicable in this context" 
                else if s.StartsWith("tpl") then fail "use of FPL templates is not allowed in this context" 
                else (preturn s) 
        ) 


let IdStartsWithCap: Parser<string,unit> = regex @"[A-Z][a-z0-9A-Z_]*" <?> "<PascalCaseId>"
let digits: Parser<string,unit> = regex @"\d+" <?> "digits"
let digitsIdSmallCase: Parser<string, unit> = regex @"\d+[a-z][a-z0-9A-Z_]*" <?> "<digits><camelCaseId>"
let namespaceIdentifier = sepBy1 IdStartsWithCap dot |>> FplIdentifier.NamespaceIdentifier
let wildcardedNamespaceIdentifier = many1 (IdStartsWithCap .>> dot) .>> skipString "*" |>> FplIdentifier.WildcaredNamespaceIdentifier
let alias = SW .>> skipString "alias" >>. SW >>. IdStartsWithCap
let aliasedNamespaceIdentifier = sepBy1 IdStartsWithCap dot .>>. alias |>> FplIdentifier.AliasedNamespaceIdentifier
let variable = IdStartsWithSmallCase |>> FplIdentifier.Var
let variableList = sepEndBy1 variable commaSpaces
let argumentIdentifier = choice [
    digitsIdSmallCase
    digits
]
let argumentParam = slash >>. argumentIdentifier



let keywordInference: Parser<_,unit> = skipString "inference" <|> skipString "inf"
let keywordSelf: Parser<_,unit> = skipString "self"
let keywordIndex: Parser<_,unit> = skipString "index" <|> skipString "ind" >>% FplType.IndexType


(* FplBlock-related Keywords *)
let keywordPremise: Parser<_,unit> = skipString "premise" <|> skipString "pre"
let keywordConclusion: Parser<_,unit> = skipString "conclusion" <|> skipString "con"


(* Statement-related Keywords *)
let keywordDel: Parser<_,unit> = skipString "delegate" <|> skipString "del"
let keywordReturn: Parser<_,unit> = skipString "return" <|> skipString "ret"
let keywordRange: Parser<_,unit> = skipString "range"
let keywordLoop: Parser<_,unit> = skipString "loop"
let keywordCases: Parser<_,unit> = skipString "cases"
let keywordAssert: Parser<_,unit> = skipString "assert"

(* Predicate-related Keywords *)
let keywordUndefined: Parser<_,unit> = skipString "undefined" <|> skipString "undef" >>% Predicate.Undefined
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
let keywordTemplate: Parser<_,unit> = (pstring "template" <|> pstring "tpl") |>> FplType.TemplateType

let templateTail = choice [
    IdStartsWithCap
    digits
]

let templateWithTail = many1Strings2 (pstring "template" <|> pstring "tpl") templateTail |>>  FplType.TemplateType

let keywordObject: Parser<_,unit> = skipString "object" <|> skipString "obj" >>% FplType.ObjectType 

let objectHeader = choice [
    keywordObject
    (attempt templateWithTail) <|> keywordTemplate
] 

let keywordPredicate: Parser<_,unit> = skipString "predicate" <|> skipString "pred" >>% FplType.PredicateType
let keywordFunction: Parser<_,unit> = skipString "function" <|> skipString "func" >>% FplType.FunctionalTermType


let wildcardTheoryNamespace = 
    (attempt ((attempt aliasedNamespaceIdentifier) <|>
    wildcardedNamespaceIdentifier) <|>    
    namespaceIdentifier) .>> IW

let wildcardTheoryNamespaceList = sepEndBy1 wildcardTheoryNamespace (comma >>. IW) 

let usesClause = skipString "uses" >>. SW >>. wildcardTheoryNamespaceList |>> UsesClause.UsesClause

let extensionTail: Parser<unit,unit> = skipString ":end" >>. SW

let extensionHeader: Parser<unit,unit> = skipString ":ext" 

let extensionName = IW >>. skipString "ext" >>. IdStartsWithCap .>> IW |>> Extension.Extensionname

let extensionRegex: Parser<_, unit>  = skipChar ':' >>. IW >>. regex @"\/(?!:end).*" .>> IW |>> Extension.ExtensionRegex

let extensionBlock = IW >>. extensionHeader >>. extensionName .>>. extensionRegex .>> extensionTail |>> ExtensionBlock.ExtensionBlock


(* Signatures, Variable Declarations, and Types, Ranges and Coordinates *)
// convention: All syntax production rules of FPL syntax extensions have to start with "ext", followed by
// a Pascal Case id.
// This ensures that they will not be mixed-up with original FPL ebnf productions
// that are all PascalCase as well as FPL keywords, that are all small case.

let xId = at >>. extensionName |>> FplType.ExtensionType <?> "@ext<PascalCaseId>"

let predicateIdentifier = sepBy1 IdStartsWithCap dot |>> FplIdentifier.AliasedId

let classIdentifier = sepBy1 IdStartsWithCap dot |>> FplType.ClassHeaderType

let indexVariable = (IdStartsWithSmallCase .>> dollar) .>>. ( digits <|> IdStartsWithSmallCase ) |>> FplIdentifier.IndexVariable

let atList = many at

let self = atList .>> keywordSelf |>> FplIdentifier.Self

let entity = choice [
    self
    variable
]

let leftOpen = leftBracket >>. IW >>. exclamationMark >>% FplIdentifier.LeftOpen
let leftClosed = leftBracket >>. IW >>% FplIdentifier.LeftClosed

let leftBound = ((attempt leftOpen) <|> leftClosed)

let rightBound = choice [
    exclamationMark >>. IW >>. rightBracket >>% FplIdentifier.RightOpen
    IW >>. rightBracket >>% FplIdentifier.RightClosed
]
 


////// resolving recursive parsers
let statementList, statementListRef = createParserForwardedToRef()
let primePredicate, primePredicateRef = createParserForwardedToRef()
let coordOfEntity, coordOfEntityRef = createParserForwardedToRef()
let predicate, predicateRef = createParserForwardedToRef()
let predicateList, predicateListRef = createParserForwardedToRef()
let predicateWithArguments, predicateWithArgumentsRef = createParserForwardedToRef()
let paramTuple, paramTupleRef = createParserForwardedToRef()

let entityWithCoord = entity .>>. coordOfEntity |>> FplIdentifier.EntityWithCoord

let assignee = (attempt entityWithCoord) <|> entity

let coord = choice [
    extDigits
    assignee
]

let fplDelegateIdentifier: Parser<_, unit> = keywordDel >>. dot >>. regex @"[a-z_]+" |>> FplIdentifier.DelegateId

let fplIdentifier = choice [
    predicateIdentifier
    fplDelegateIdentifier
    coord
]

let coordList = sepEndBy1 coord commaSpaces

let bracketedCoordList = (leftBracket >>. IW >>. coordList) .>> (IW >>. rightBracket) |>> FplIdentifier.BrackedCoordList

let fplRange = ((opt coord) .>> IW .>> tilde) .>>. (IW >>. opt coord)

let closedOrOpenRange = (leftBound .>> IW) .>>. fplRange .>>. (IW >>. rightBound) |>> FplIdentifier.ClosedOrOpenRange

coordOfEntityRef := choice [
    closedOrOpenRange
    bracketedCoordList
]

let coordInType = choice [
    fplIdentifier
    indexVariable
]

let coordInTypeList = sepBy1 coordInType commaSpaces

let rangeInType = (opt coordInType .>> IW) .>>. (tilde >>. IW >>. opt coordInType) |>> FplIdentifier.RangeInType

let specificClassType = choice [
    objectHeader
    xId
    classIdentifier
]

//// later semantics: Star: 0 or more occurrences, Plus: 1 or more occurrences
let callModifier = choice [
    star
    plus
]

let classTypeWithCoord = ((specificClassType .>> IW) .>> leftBracket) .>>. (coordInTypeList .>> (IW >>. rightBracket)) |>> FplType.FplTypeWithCoords
let classTypeWithRange = ((specificClassType .>> IW) .>>. leftBound) .>>. (rangeInType .>>. rightBound) |>> FplType.FplTypeWithRange

// The classType is the last type in FPL we can derive FPL classes from.
// It therefore excludes the in-built FPL-types keywordPredicate, keywordFunction, and keywordIndex
// to restrict it to pure objects.
// In contrast to variableType which can also be used for declaring variables 
// in the scope of FPL building blocks
let classType = (((attempt classTypeWithRange) <|> (attempt classTypeWithCoord)) <|> specificClassType)

let modifieableClassType = opt callModifier .>>. classType |>> FplType.VariableTypeWithModifier
let modifieablePredicateType = opt callModifier .>>. keywordPredicate |>> FplType.VariableTypeWithModifier
let modifieableFunctionType = opt callModifier .>>. keywordFunction |>> FplType.VariableTypeWithModifier
let modifieableIndexType = opt callModifier .>>. keywordIndex |>> FplType.VariableTypeWithModifier

let variableTypeWithModifier = (((attempt modifieableIndexType) <|> attempt modifieableFunctionType) <|> attempt modifieablePredicateType) <|> modifieableClassType

let parenthesisedType = variableTypeWithModifier .>> IW >>. paramTuple |>> FplType.VariableType

let variableType = ((attempt parenthesisedType) <|> attempt variableTypeWithModifier) <|> classType

let namedVariableDeclaration = (variableList .>> IW) .>>. ((colon >>. IW) >>. variableType)
let namedVariableDeclarationList = sepEndBy namedVariableDeclaration commaSpaces 

paramTupleRef := (leftParen >>. IW >>. namedVariableDeclarationList) .>> (IW .>> rightParen) 

let signature = (predicateIdentifier .>> IW) .>>. paramTuple |>> FplBlock.Signature

(* Statements *)
let argumentTuple = (spacesLeftParenSpaces >>. IW >>. predicateList) .>> (IW .>> spacesRightParenSpaces)  

let fplDelegate = fplDelegateIdentifier .>>. argumentTuple |>> Predicate.Delegate
let assignmentStatement = (assignee .>> IW .>> colonEqual) .>>. (IW >>. predicate) |>> Statement.Assignment
let returnStatement = keywordReturn >>. SW >>. predicate |>> Statement.Return
let keysOfVariadicVariable = variable .>> dollar

let variableRange = choice [
    keysOfVariadicVariable
    closedOrOpenRange
    assignee
]

let elseMark = skipString ":>"
let defaultResult = elseMark >>. IW >>.   many CW >>. statementList |>> Statement.DefaultResult
let conditionFollowedByResult = (case >>. IW >>. predicate .>> colon) .>>. (many CW >>. statementList) |>> Statement.ConditionFollowedByResult
let conditionFollowedByResultList = many1 (many CW >>. conditionFollowedByResult)


let casesStatement = (keywordCases >>. many CW >>. leftParen >>. many CW >>. conditionFollowedByResultList .>> many CW) .>>. (defaultResult .>> many CW .>> rightParen) |>> Statement.Cases
let rangeOrLoopBody = ((assignee .>> SW) .>>. variableRange .>> many CW) .>>. (leftParen >>. many CW >>. statementList) .>> (many CW >>. rightParen)
let loopStatement = keywordLoop >>. SW >>. rangeOrLoopBody |>> Statement.Loop
let rangeStatement = keywordRange >>. SW >>. rangeOrLoopBody |>> Statement.Range

//// Difference of assertion to an axiom: axiom's is followed by a signature of a predicate (i.e. with possible parameters),
//// not by a predicate (i.e. with possible arguments)
//// Difference of assertion to a mandatory property: a mandatory property introduces a completely new identifier inside
//// the scope of a definition. An assertion uses a predicate referring to existing identifiers in the whole theory
//// Difference of assertion to assume: the latter will be used only in the scope of proofs
let assertionStatement = keywordAssert >>. SW >>. predicate |>> Statement.Assertion

let statement = choice [
    casesStatement
    assertionStatement
    rangeStatement
    loopStatement
    returnStatement
    assignmentStatement
]

statementListRef := many (many CW >>. statement .>> IW)

(* Predicates *)

predicateWithArgumentsRef := fplIdentifier .>>. argumentTuple |>> Predicate.PredicateWithArgs

let qualifiedIdentifier = fplIdentifier .>>. many1 (dot >>. predicateWithArguments) |>> Predicate.QualifiedIdentifier

let predicateWithoutArgs = fplIdentifier |>> Predicate.PredicateWithoutArgs

primePredicateRef := choice [
    keywordTrue
    keywordFalse
    keywordUndefined
    (attempt predicateWithArguments) <|> (attempt qualifiedIdentifier) <|> predicateWithoutArgs    
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
let isOperator = (many CW >>. keywordIs >>. spacesLeftParenSpaces >>. coordInType) .>>. (IW >>. commaSpaces >>. variableType) .>> spacesRightParenSpaces |>> Predicate.IsOperator

// A compound Predicate has its own boolean expressions to avoid mixing up with Pl0Propositions
let compoundPredicate = choice [
    conjunction
    disjunction
    implication
    equivalence
    exclusiveOr
    negation
    all
    (attempt existsTimesN) <|> exists
    isOperator
]

predicateRef := choice [
    compoundPredicate
    primePredicate
]

predicateListRef := sepBy predicate commaSpaces 

(* FPL building blocks *)

let commentedNamedVariableDeclaration = many CW >>. namedVariableDeclaration |>> FplBlock.BlockVariableDeclaration
let commentedStatement = many CW >>. statement |>> FplBlock.BlockStatement

let variableSpecification = (attempt commentedStatement) <|> (attempt commentedNamedVariableDeclaration)

let variableSpecificationList = many variableSpecification

(*To simplify the syntax definition, we do not define separate
FplPremiseConclusionBlocks for rules of inference and theorem-like blocks.
The first have a simplified, PL0 semantics, the latter have a more complex, predicative semantics.
However, there is a syntactical simplification of the signature*)
let commentedPredicate = many CW >>. predicate
let premise = many CW >>. (keywordPremise >>. IW >>. colon >>. commentedPredicate) 
let conclusion = many CW >>. (keywordConclusion >>. IW >>. colon >>. commentedPredicate) 
let leftBraceCommented = (leftBrace >>. many CW)
let commentedRightBrace = (many CW .>> rightBrace)
let premiseConclusionBlock = leftBraceCommented >>. variableSpecificationList .>>. (premise .>> many CW) .>>. conclusion .>> commentedRightBrace

(* FPL building blocks - rules of reference *)
let ruleOfInference = (signature .>> IW) .>>. premiseConclusionBlock |>> FplBlock.RuleOfInference
let ruleOfInferenceList = sepEndBy1 ruleOfInference (many CW)
let rulesOfInferenceBlock = (keywordInference >>. many CW >>. leftBrace >>. many CW >>. ruleOfInferenceList) .>> commentedRightBrace

(* FPL building blocks - Theorem-like statements and conjectures *)
let keywordTheorem: Parser<_,unit> = skipString "theorem" <|> skipString "thm" 
let keywordLemma: Parser<_,unit> = skipString "lemma" <|> skipString "lem" 
let keywordProposition: Parser<_,unit> = skipString "proposition" <|> skipString "prop" 
let keywordCorollary: Parser<_,unit> = skipString "corollary" <|> skipString "cor" 
let keywordConjecture: Parser<_,unit> = skipString "conjecture" <|> skipString "conj" 

let theorem = keywordTheorem >>. SW >>. (signature .>> IW) .>>. premiseConclusionBlock |>> FplBlock.Theorem
let lemma = keywordLemma >>. SW >>. (signature .>> IW) .>>. premiseConclusionBlock |>> FplBlock.Lemma
let proposition = keywordProposition >>. SW >>. (signature .>> IW) .>>. premiseConclusionBlock |>> FplBlock.Proposition
let conjecture = keywordConjecture >>. SW >>. (signature .>> IW) .>>. premiseConclusionBlock |>> FplBlock.Conjecture

let dollarDigitList = many1 (dollar >>. digits)
let referencingIdentifier = predicateIdentifier .>>. dollarDigitList
let corollarySignature = (referencingIdentifier .>> IW) .>>. paramTuple
let corrolary = keywordCorollary >>. SW >>. (corollarySignature .>> IW) .>>. premiseConclusionBlock |>> FplBlock.Corollary

(* FPL building blocks - Axioms *)

let keywordAxiom: Parser<_,unit> = (skipString "axiom" <|> skipString "ax") <|> (skipString "postulate" <|> skipString "post") 
let axiomBlock = leftBraceCommented >>. variableSpecificationList .>>. commentedPredicate .>> commentedRightBrace

let axiom = keywordAxiom >>. SW >>. signature .>>. (IW >>. axiomBlock) |>> FplBlock.Axiom

(* FPL building blocks - Constructors *)

let instanceBlock = leftBrace >>. many CW >>. variableSpecificationList .>> commentedRightBrace
let callConstructorParentClass = opt predicateWithArguments |>> FplBlock.ClassConstructorCall
let constructorBlock = leftBraceCommented >>. callConstructorParentClass .>>. variableSpecificationList .>> commentedRightBrace
let constructor = (signature .>> IW) .>>. constructorBlock |>> FplBlock.Constructor

(* FPL building blocks - Properties *)
let keywordMandatory: Parser<_,unit> = (skipString "mandatory" <|> skipString "mand") >>% FplBlock.Mandatory
let keywordOptional: Parser<_,unit> = (skipString "optional" <|> skipString "opt") >>% FplBlock.Optional
let predicateInstanceBlock = (leftBraceCommented >>. variableSpecificationList) .>>. (commentedPredicate .>> commentedRightBrace)
let predicateInstance = (keywordPredicate >>. SW >>. signature) .>>. (many CW >>. predicateInstanceBlock) |>> FplBlock.PredicateInstance
let classInstance = (variableType .>> SW) .>>. signature .>>. (many CW >>. instanceBlock) |>> FplBlock.ClassInstance
let mapping = toArrow >>. IW >>. variableType
let functionalTermSignature = (keywordFunction >>. SW >>. signature) .>>. (IW >>. mapping)
let functionalTermInstance = functionalTermSignature .>>. (many CW >>. instanceBlock) |>> FplBlock.FunctionalTermInstance
let definitionProperty = choice [
    predicateInstance
    functionalTermInstance
    classInstance
]
let propertyHeader = (many CW >>. (keywordMandatory <|> keywordOptional)) 
let property = propertyHeader .>>. (SW >>. definitionProperty)
let propertyList = many1 property
