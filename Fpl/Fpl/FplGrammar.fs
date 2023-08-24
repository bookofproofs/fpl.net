module FplGrammar
open FParsec
open FplGrammarTypes

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
            if keyWordSet.Contains(s) then fail "reserved FPL keyword" 
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



let keywordDel: Parser<_,unit> = skipString "delegate" <|> skipString "del"
let keywordInference: Parser<_,unit> = skipString "inference" <|> skipString "inf"
let keywordSelf: Parser<_,unit> = skipString "self"

let keywordUndefined: Parser<_,unit> = skipString "undefined" <|> skipString "undef" >>% Predicate.Undefined

let keywordReturn: Parser<_,unit> = skipString "return" <|> skipString "ret"

let keywordIndex: Parser<_,unit> = skipString "index" <|> skipString "ind" >>% FplType.IndexType

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

let keywordTheorem: Parser<_,unit> = skipString "theorem" <|> skipString "thm" 
let keywordLemma: Parser<_,unit> = skipString "lemma" <|> skipString "lem" 
let keywordProposition: Parser<_,unit> = skipString "proposition" <|> skipString "prop" 
let keywordCorollary: Parser<_,unit> = skipString "corollary" <|> skipString "cor" 
let keywordConjecture: Parser<_,unit> = skipString "conjecture" <|> skipString "conj" 

let keywordPredicate: Parser<_,unit> = skipString "predicate" <|> skipString "pred" >>% FplType.PredicateType
let keywordFunction: Parser<_,unit> = skipString "function" <|> skipString "func" >>% FplType.FunctionalTermType


let theoremLikeStatementOrConjectureHeader = choice [
    keywordConjecture
    keywordCorollary
    keywordProposition
    keywordLemma
    keywordTheorem
]

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
//let statementList, statementListRef = createParserForwardedToRef()
let primePredicate, primePredicateRef = createParserForwardedToRef()
let coordOfEntity, coordOfEntityRef = createParserForwardedToRef()
let predicate, predicateRef = createParserForwardedToRef()
let predicateList, predicateListRef = createParserForwardedToRef()
let predicateWithArguments, predicateWithArgumentsRef = createParserForwardedToRef()
let paramTuple, paramTupleRef = createParserForwardedToRef()

let entityWithCoord = entity .>>. coordOfEntity |>> FplIdentifier.EntityWithCoord

let assignee = choice [
    entity
    entityWithCoord
]

let coord = choice [
    extDigits
    assignee
]

let fplIdentifier = choice [
    predicateIdentifier
    coord
]


let coordList = sepEndBy1 coord commaSpaces

let bracketedCoordList = (leftBracket >>. IW >>. coordList) .>> (IW >>. rightBracket) |>> FplIdentifier.BrackedCoordList

let fplRange = ((opt coord) .>> IW .>> tilde) .>>. (IW >>. opt coord)

let leftOpenRange = choice [
    ((leftBracket >>. IW >>. exclamationMark >>. coord) .>> IW)
    
]

let closedOrOpenRange = (leftBound .>> IW) .>>. fplRange .>>. (IW >>. rightBound) |>> FplIdentifier.ClosedOrOpenRange

coordOfEntityRef := choice [
    closedOrOpenRange
    bracketedCoordList
]

let coordInType = choice [
    fplIdentifier
    indexVariable
    variable
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

////(* Statements *)
let argumentTuple = (spacesLeftParenSpaces >>. IW >>. predicateList) .>> (IW .>> spacesRightParenSpaces)  

//let fplDelegateIdentifier: Parser<_, unit> = regex @"[a-z_]+"
//let fplDelegate = (keywordDel >>. dot >>. fplDelegateIdentifier) .>>. argumentTuple
//let assignmentStatement = (assignee .>> IW .>> colonEqual) .>>. (IW >>. predicate)
//let returnStatement = keywordReturn >>. SW >>. predicate
//let keysOfVariadicVariable = variable .>> dollar

//let variableRange = choice [
//    keysOfVariadicVariable
//    closedOrOpenRange
//    assignee
//]


//let defaultResult = keywordElse >>. IW >>. colon >>. many CW >>. statementList
//let conditionFollowedByResult = (keywordCase >>. SW >>. predicate .>> colon) .>>. (many CW >>. statementList)
//let conditionFollowedByResultList = sepBy1 conditionFollowedByResult ( many CW )


//let caseStatement = (keywordCases >>. many CW >>. leftParen >>. many CW >>. conditionFollowedByResultList .>> many CW) .>>. (defaultResult .>> many CW .>> rightParen)
//let rangeOrLoopBody = ((assignee .>> SW) .>>. variableRange .>> many CW) .>>. (leftParen >>. many CW >>. statementList) .>> (many CW >>. rightParen)
//let loopStatement = keywordLoop >>. SW >>. rangeOrLoopBody
//let rangeStatement = keywordRange >>. SW >>. rangeOrLoopBody

//// Difference of assertion to an axiom: axiom's is followed by a signature of a predicate (i.e. with possible parameters),
//// not by a predicate (i.e. with possible arguments)
//// Difference of assertion to a mandatory property: a mandatory property introduces a completely new identifier inside
//// the scope of a definition. An assertion uses a predicate referring to existing identifiers in the whole theory
//// Difference of assertion to assume: the latter will be used only in the scope of proofs
let assertionStatement = keywordAssert >>. SW >>. predicate

//let statement = choice [
//    fplDelegate
//    caseStatement
//    assertionStatement
//    assignmentStatement
//    rangeStatement
//    loopStatement
//    returnStatement
//]

//statementListRef :=  sepBy1 (statement .>> SW)  (many CW)

//(* Predicates *)

predicateWithArgumentsRef := fplIdentifier .>>. argumentTuple |>> Predicate.PredicateWithArgs

primePredicateRef := choice [
    keywordTrue
    keywordFalse
    keywordUndefined
    predicateWithArguments
    //statement
    //argumentParam
    
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

predicateListRef := sepBy1 predicate commaSpaces 
//(* FPL building blocks *)

//(*To simplify the syntax definition, we do not define separate
//FplPremiseConclusionBlocks for rules of inference and theorem-like blocks.
//The first have a simplified, PL0 semantics, the latter have a more complex, predicative semantics.
//However, there is a syntactical simplification of the signature*)
//let ruleOfInference = signature .>>. IW >>. premiseConclusionBlock |>> BlockHeader.Inference

//let ruleOfInferenceList = sepEndBy1 ruleOfInference (many CW)

//let rulesOfInferenceBlock = inference >>. many CW >>. leftBrace >>. many CW >>. ruleOfInferenceList >>. many CW >>. rightBrace

