module FplGrammar
open System.Text.RegularExpressions
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
let case: Parser<_,unit> = skipChar '|'
let leftBracket: Parser<_, unit> = skipChar '['
let rightBracket: Parser<_, unit> = skipChar ']'
let tilde: Parser<_, unit> = skipChar '~'
let semiColon: Parser<_, unit> = skipChar ';'
let dollar: Parser<_, unit> = skipChar '$'
let toArrow: Parser<_, unit> = skipString "->"
let vDash: Parser<_, unit> = skipString "|-"

(* Whitespaces and Comments *)

let IW = spaces

let SW = spaces1

let inlineComment: Parser<_, unit> = pstring "//" >>. skipManyTill anyChar (skipNewline <|> eof) |>> ignore <?> "<line-comment>"

let blockComment: Parser<_, unit> = (pstring "/*" >>. (skipManyTill anyChar (pstring "*/"))) |>> ignore <?> "<multiline-comment>"

let CW = choice [
    blockComment
    inlineComment
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
let extDigits: Parser<_, unit> = regex @"\d+" <?> "<extDigits>" |>> FplIdentifier.ExtDigits

(* Identifiers *)
(* Fpl Keywords *)

let keyWordSet =
    System.Collections.Generic.HashSet<_>(
        [|
        "alias"; 
        "all"; 
        "and"; 
        "assert"; 
        "ass"; "assume"; 
        "ax"; "axiom";
        "cases"; 
        "cl"; "class"; 
        "conj"; "conjecture"; 
        "con"; "conclusion"; 
        "cor"; "corollary";
        "del"; "delegate"
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

let isLowercaseLetter c = c >= 'a' && c <= 'z'
let isUppercaseLetter c = c >= 'A' && c <= 'Z'
let isDigit c = c >= '0' && c <= '9'
let isUnderscore c = c = '_'
let IdStartsWithSmallCase: Parser<string, unit> =
    many1Satisfy2L isLowercaseLetter (fun c -> isLowercaseLetter c || isUppercaseLetter c || isDigit c || isUnderscore c) "<camelCaseId>"
let IdStartsWithCap: Parser<string,unit> = 
    many1Satisfy2L isUppercaseLetter (fun c -> isLowercaseLetter c || isUppercaseLetter c || isDigit c || isUnderscore c) "<PascalCaseId>"
let digits: Parser<string,unit> = regex @"\d+" <?> "digits"
let argumentIdentifier = regex @"\d+([a-z][a-z0-9A-Z_])*\." <?> "<ArgumentIdentifier>" |>> Predicate.ArgumentIdentifier
let namespaceIdentifier = sepBy1 IdStartsWithCap dot |>> FplIdentifier.NamespaceIdentifier
let alias = skipString "alias" >>. SW >>. IdStartsWithCap
let aliasedNamespaceIdentifier = sepBy1 IdStartsWithCap dot .>>. (IW >>. alias) |>> FplIdentifier.AliasedNamespaceIdentifier
let tplRegex = Regex(@"^(tpl|template)(([A-Z][a-z0-9A-Z_]*)|\d*)$", RegexOptions.Compiled)
let variableX: Parser<string,unit> = regex @"[a-z][a-z0-9A-Z_]*" >>= (
            fun s -> 
                if keyWordSet.Contains(s) then fail ("Cannot use keyword '" + s + "' as a variable") 
                else if tplRegex.IsMatch(s) then fail ("Cannot use template '" + s + "' as a variable") 
                else (preturn s)
        ) 
let variable = variableX |>> FplIdentifier.Var 

let variableList = sepEndBy1 variable commaSpaces

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


let theoryNamespace = (attempt aliasedNamespaceIdentifier <|> namespaceIdentifier) .>> IW

let theoryNamespaceList = sepEndBy1 theoryNamespace (comma >>. IW) 

let usesClause = skipString "uses" >>. SW >>. theoryNamespaceList |>> UsesClause.UsesClause

let extensionTail: Parser<unit,unit> = skipString ":end" >>. SW

let extensionHeader: Parser<unit,unit> = skipString ":ext" 

let extensionName = skipString "ext" >>. IdStartsWithCap .>> IW |>> Extension.Extensionname

let extensionRegex: Parser<_, unit>  = skipChar ':' >>. IW >>. regex @"\/(?!:end).*" .>> IW |>> Extension.ExtensionRegex

let extensionBlock = extensionHeader >>. IW >>. extensionName .>>. extensionRegex .>> extensionTail |>> ExtensionBlock.ExtensionBlock


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

let entity = (attempt (attempt self <|> indexVariable)) <|> variable

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
    assignee
    extDigits
]

let fplDelegateIdentifier: Parser<_, unit> = keywordDel >>. dot >>. regex @"[a-z_A-Z][a-z_A-Z0-9]+" |>> FplIdentifier.DelegateId

let fplIdentifier = choice [
    fplDelegateIdentifier
    coord
    predicateIdentifier
]

let coordList = sepEndBy1 coord commaSpaces

let bracketedCoordList = (leftBracket >>. IW >>. coordList) .>> (IW >>. rightBracket) |>> FplIdentifier.BrackedCoordList

let fplRange = ((opt coord) .>> IW .>> tilde) .>>. (IW >>. opt coord)

let closedOrOpenRange = (leftBound .>> IW) .>>. fplRange .>>. (IW >>. rightBound) |>> FplIdentifier.ClosedOrOpenRange

coordOfEntityRef := attempt closedOrOpenRange <|> bracketedCoordList

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
let argumentTuple = (spacesLeftParenSpaces >>. predicateList) .>> (IW .>> spacesRightParenSpaces)  

let fplDelegate = fplDelegateIdentifier .>>. argumentTuple |>> Predicate.Delegate
let assignmentStatement = (assignee .>> IW .>> colonEqual) .>>. (IW >>. predicate) |>> Statement.Assignment
let returnStatement = keywordReturn >>. SW >>. predicate |>> Statement.Return

let variableRange = choice [
    closedOrOpenRange
    assignee
]

let elseMark = skipString ":>"
let defaultResult = elseMark >>. IW >>.   many CW >>. statementList |>> Statement.DefaultResult
let conditionFollowedByResult = (case >>. IW >>. predicate .>> colon) .>>. (many CW >>. statementList) |>> Statement.ConditionFollowedByResult
let conditionFollowedByResultList = many1 (many CW >>. conditionFollowedByResult)


let casesStatement = ((keywordCases >>. many CW >>. leftParen >>. many CW >>. conditionFollowedByResultList .>> many CW) .>>. (defaultResult .>> many CW .>> rightParen)) <?> "<cases statement>" |>> Statement.Cases
let rangeOrLoopBody = ((assignee .>> SW) .>>. variableRange .>> many CW) .>>. (leftParen >>. many CW >>. statementList) .>> (many CW >>. rightParen)
let loopStatement = keywordLoop >>. SW >>. rangeOrLoopBody |>> Statement.Loop
let rangeStatement = keywordRange >>. SW >>. rangeOrLoopBody |>> Statement.Range

//// Difference of assertion to an axiom: axiom's is followed by a signature of a predicate (i.e. with possible parameters),
//// not by a predicate (i.e. with possible arguments)
//// Difference of assertion to a mandatory property: a mandatory property introduces a completely new identifier inside
//// the scope of a definition. An assertion uses a predicate referring to existing identifiers in the whole theory
//// Difference of assertion to assume: the latter will be used only in the scope of proofs
let assertionStatement = keywordAssert >>. SW >>. predicate |>> Statement.Assertion

let statement = 
    (choice [
        casesStatement
        assertionStatement
        rangeStatement
        loopStatement
        returnStatement
        assignmentStatement
    ]) <?> "<statement>"

statementListRef := many (many CW >>. statement .>> IW)

(* Predicates *)

predicateWithArgumentsRef := fplIdentifier .>>. argumentTuple |>> Predicate.PredicateWithArgs

let qualifiedIdentifier = fplIdentifier .>>. many1 (dot >>. predicateWithArguments) |>> Predicate.QualifiedIdentifier

let predicateWithoutArgs = fplIdentifier |>> Predicate.PredicateWithoutArgs

primePredicateRef := choice [
    keywordTrue
    keywordFalse
    keywordUndefined
    (attempt predicateWithArguments) <|> (attempt qualifiedIdentifier) <|> argumentIdentifier <|> predicateWithoutArgs
    
]

let conjunction = (keywordAnd >>. spacesLeftParenSpaces >>. predicateList) .>> spacesRightParenSpaces |>> Predicate.And
let disjunction = (keywordOr >>. spacesLeftParenSpaces >>. predicateList) .>> spacesRightParenSpaces |>> Predicate.Or

let twoPredicatesInParens = (spacesLeftParenSpaces >>. predicate) .>>. (commaSpaces >>. predicate) .>> spacesRightParenSpaces 
let onePredicateInParens = (spacesLeftParenSpaces >>. predicate) .>> spacesRightParenSpaces
let implication = keywordImpl >>. twoPredicatesInParens |>> Predicate.Impl
let equivalence = keywordIif >>. twoPredicatesInParens |>> Predicate.Iif
let exclusiveOr = keywordXor >>. twoPredicatesInParens |>> Predicate.Xor
let negation = keywordNot >>. onePredicateInParens |>> Predicate.Not
let all = (keywordAll >>. SW >>. variableList) .>>. onePredicateInParens |>> Predicate.All
let exists = (keywordEx >>. SW >>. variableList) .>>. onePredicateInParens |>> Predicate.Exists
let existsTimesN = ((keywordEx >>. dollar >>. digits) .>>. (SW >>. variableList)) .>>. onePredicateInParens |>> Predicate.ExistsN
let isOperator = (keywordIs >>. spacesLeftParenSpaces >>. coordInType) .>>. (commaSpaces >>. variableType) .>> spacesRightParenSpaces |>> Predicate.IsOperator

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

predicateRef := (compoundPredicate <|> primePredicate) .>> IW

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
let keywordInference: Parser<_,unit> = skipString "inference" <|> skipString "inf"
let ruleOfInference = (signature .>> IW) .>>. premiseConclusionBlock |>> FplBlock.RuleOfInference
let ruleOfInferenceList = sepEndBy1 ruleOfInference (many CW)
let rulesOfInferenceBlock = (keywordInference >>. IW >>. leftBraceCommented >>. many CW >>. ruleOfInferenceList) .>> commentedRightBrace

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
let corollary = keywordCorollary >>. SW >>. (corollarySignature .>> IW) .>>. premiseConclusionBlock |>> FplBlock.Corollary

(* FPL building blocks - Axioms *)

let keywordAxiom: Parser<_,unit> = (skipString "axiom" <|> skipString "ax") <|> (skipString "postulate" <|> skipString "post") 
let axiomBlock = leftBraceCommented >>. variableSpecificationList .>>. commentedPredicate .>> commentedRightBrace

let axiom = keywordAxiom >>. SW >>. signature .>>. (IW >>. axiomBlock) |>> FplBlock.Axiom

(* FPL building blocks - Constructors *)

let instanceBlock = leftBrace >>. many CW >>. variableSpecificationList .>> commentedRightBrace
let callConstructorParentClass = opt predicateWithArguments |>> FplBlock.ClassConstructorCall
let constructorBlock = leftBraceCommented >>. variableSpecificationList .>>. callConstructorParentClass  .>> commentedRightBrace
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
let property = propertyHeader .>>. (SW >>. definitionProperty) |>> FplBlock.Property
let propertyList = many1 (many CW >>. property .>> IW)

(* FPL building blocks - Proofs 

    # A Proof relates to the PredicateIdentifier of the Theorem.
    # Because proofs are named, they can stand anywhere inside the theory, not only immediately
    # after the Theorem they prove. This is to enable the users to mix
    # with natural language an provide a proof long after the theorem was stated.

*)
// justifying proof arguments can be the identifiers of Rules of References, conjectures, theorem-like statements, or axioms
let keywordRevoke: Parser<_,unit> = (skipString "revoke" <|> skipString "rev") >>. SW
let revokeArgument = keywordRevoke >>. argumentIdentifier |>> Proof.RevokeArgument 
let premiseOfToBeProvedTheorem = keywordPremise >>% Predicate.PremiseReference 
let conclusionOfToBeProvedTheorem = keywordConclusion >>% Proof.ConclusionReference 
let premiseOrOtherPredicate = premiseOfToBeProvedTheorem <|> predicate
    
let keywordAssume: Parser<_,unit> = (skipString "assume" <|> skipString "ass") 
let assumeArgument = keywordAssume >>. SW >>. premiseOrOtherPredicate |>> Proof.AssumeArgument
let keywordTrivial: Parser<_,unit>  = skipString "trivial" >>% Proof.Trivial
let keywordQed: Parser<_,unit>  = skipString "qed" >>% Proof.Qed
let derivedPredicate = predicate |>> Proof.DerivedPredicate
let derivedArgument = choice [
    keywordQed 
    keywordTrivial 
    conclusionOfToBeProvedTheorem 
    derivedPredicate
]

let argumentInference = attempt revokeArgument <|> derivedArgument
let justification = predicateList .>> IW |>> Predicate.Justification
let justifiedArgument = (justification .>> vDash .>> IW) .>>. argumentInference |>> Proof.JustifiedArgument
let argument = assumeArgument <|> justifiedArgument
let proofArgument = (argumentIdentifier .>> IW) .>>. argument |>> Proof.Argument
let proofArgumentList = many1 (many CW >>. proofArgument .>> IW)
let keywordProof: Parser<_,unit> = (skipString "proof" <|> skipString "prf")
let proofBlock = (leftBraceCommented >>. variableSpecificationList) .>>. (proofArgumentList .>> commentedRightBrace)
let proof = (keywordProof >>. SW >>. referencingIdentifier) .>>. (IW >>. proofBlock) |>> FplBlock.Proof

(* FPL building blocks - Definitions *)

// Predicate building blocks can be defined similarly to classes, they can have properties but they cannot be derived any parent type 
let predicateDefinitionBlock = (leftBraceCommented  >>. variableSpecificationList .>> many CW) .>>. (opt predicate .>> many CW) .>>. opt propertyList .>> commentedRightBrace 
let definitionPredicate = (keywordPredicate >>. SW >>. signature .>> IW) .>>. predicateDefinitionBlock |>> FplBlock.DefinitionPredicate

// Functional term building blocks can be defined similarly to classes, they can have properties but they cannot be derived any parent type 
let functionalTermDefinitionBlock = (leftBraceCommented  >>. variableSpecificationList .>> many CW) .>>. opt propertyList .>> commentedRightBrace
let definitionFunctionalTerm = (functionalTermSignature .>> IW) .>>. functionalTermDefinitionBlock |>> FplBlock.DefinitionFunctionalTerm

// Class definitions
let keywordClass: Parser<_,unit> = (skipString "class" <|> skipString "cl")
let classDefinitionContent = choice [
    property
    constructor
]
let classDefinitionContentList = many (many CW >>. classDefinitionContent .>> IW)
let classDefinitionBlock = (leftBraceCommented  >>. variableSpecificationList .>> many CW) .>>. classDefinitionContentList .>> commentedRightBrace
let classSignature = (keywordClass >>. SW >>. predicateIdentifier .>> IW) .>>. (colon >>. IW >>. classType)
let definitionClass = (classSignature .>> IW) .>>. classDefinitionBlock |>> FplBlock.DefinitionClass 

let definition = choice [
    definitionClass
    definitionPredicate
    definitionFunctionalTerm
]
(* Gathering together all Building Blocks to a theory *)
let keywordTheory: Parser<_,unit> = (skipString "theory" <|> skipString "th") 
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
]

let buildingBlockList = many (many CW >>. buildingBlock .>> IW)
// A theory begins with the keywordTheory followed by a block containing a (possibly empty) sequence of building blocks
let theoryBlock = keywordTheory >>. IW >>. leftBraceCommented >>. buildingBlockList .>> commentedRightBrace

(* Localizations *)
// Localizations provide a possibility to automatically translate FPL expressions into natural languages
let keywordLocalization: Parser<_,unit> = (skipString "localization" <|> skipString "loc") 
let localizationLanguageCode: Parser<string,unit> = regex @"[a-z]{3}" <?> "<ISO 639 language code>"
let localizationString = regex "\"[^\"\n]*\"" <?> "<OneLineString>" |>> FplIdentifier.LocalizationString

let ebnfTransl, ebnfTranslRef = createParserForwardedToRef()
let ebnfTranslTuple = (leftParen >>. IW >>. ebnfTransl) .>> (IW .>> rightParen) 
let ebnfFactor = choice [
    variable
    localizationString
    ebnfTranslTuple
]
let ebnfTerm = many1 (SW >>. ebnfFactor) |>> FplIdentifier.LocalizationTerm
ebnfTranslRef.Value <- many1 (IW >>. case >>. IW >>. ebnfTerm) |>> FplIdentifier.LocalizationTermList
let translation = (tilde >>. localizationLanguageCode .>> IW .>> colon .>> IW) .>>. ebnfTransl
let translationList = many (many CW >>. translation .>> IW)
let localization = (predicate .>> IW .>> colonEqual .>> IW) .>>. (translationList .>> IW .>> semiColon)
let localizationList = many (many CW >>. localization .>> IW)
let localizationBlock = keywordLocalization >>. spacesLeftParenSpaces >>. localizationList .>> commentedRightBrace


(* Namespaces *)
let namespaceBlock = (leftBraceCommented >>. opt extensionBlock) .>>. (many CW >>. opt usesClause) .>>. (many CW >>. opt rulesOfInferenceBlock) .>>. (many CW >>. theoryBlock) .>>. (many CW >>. opt localizationBlock) .>> commentedRightBrace
let fplNamespace = namespaceIdentifier .>>. (many CW >>. namespaceBlock)
let fplNamespaceList = many1 (many CW >>. fplNamespace .>> IW)
(* Final Parser *)
let ast = (fplNamespaceList .>> eof) |>> FplParserResult.AST
let fplParser (input: string) = run ast input 

