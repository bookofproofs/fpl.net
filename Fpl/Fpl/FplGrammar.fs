module FplGrammar
open System.Text.RegularExpressions
open FplGrammarCommons
open FplGrammarTypes
open ErrRecovery
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
let rightBrace = skipChar '}' 
let leftParen = skipChar '(' >>. spaces 
let rightParen = skipChar ')' 
let comma = skipChar ',' >>. spaces 
let star = skipChar '*' >>. spaces >>% Ast.Many
let plus = skipChar '+' >>. spaces >>% Ast.Many1
let dot = skipChar '.'
let colon = skipChar ':' >>. spaces
let colonEqual = skipString ":="
let at = pchar '@'
let case = skipChar '|'
let leftBracket = skipChar '<' >>. spaces 
let rightBracket = skipChar '>' >>. spaces  
let leftClosedBracket = skipChar '[' >>. spaces <?> "<(closed) left bound '['>"
let leftOpenBracket = skipString "[!" >>. spaces <?> "<(open) left bound '[!'>"
let rightOpenBracket = skipString "!]" >>. spaces <?> "<(open) right bound '!]'>" 
let rightClosedBracket = skipChar ']' >>. spaces <?> "<(closed) right bound ']'>" 
let tilde = skipChar '~' .>> spaces >>. spaces
let semiColon = skipChar ';'
let dollar = skipChar '$'
let toArrow = skipString "->"
let vDash = skipString "|-"

(* Whitespaces and Comments *)

let IW = spaces <?> "<whitespace>"

let SW = spaces1 <?> "<significant whitespace>"

let inlineComment = pstring "//" >>. skipManyTill anyChar (skipNewline) <?> "<inline comment>" |>> ignore 

let blockComment = (pstring "/*" >>. (skipManyTill anyChar (pstring "*/"))) <?> "<block comment>" |>> ignore 

let CW = choice [ blockComment; inlineComment; SW ]

// -----------------------------------------------------
// Extensions of the FPL language (have to be dynamic)! Lacking a pre-processor, we put the rules
// from the Proof of Concept of FPL code manually into the EBNF of the core FPL grammar.
// note that this has to be inserted into:
// the IsOperand choice
// the PredicateOrFunctionalTerm choice
let digits = regex @"\d+" <?> "<digits>" 
let extDigits: Parser<_, unit> = positions (digits) |>> Ast.ExtDigits

(* Identifiers *)


let IdStartsWithSmallCase = regex @"[a-z]\w*" 
let idStartsWithCap = (regex @"[A-Z]\w*") <?> "<PascalCaseId>"
let pascalCaseId = idStartsWithCap |>> Ast.PascalCaseId
let dollarDigits = positions (dollar >>. digits) |>> Ast.DollarDigits
let argumentIdentifier = positions (regex @"\d+([a-z]\w)*\.") <?> "<argument identifier>" |>> Ast.ArgumentIdentifier

let namespaceIdentifier = positions (sepBy1 pascalCaseId dot) .>> IW |>> Ast.NamespaceIdentifier
let predicateIdentifier = positions (sepBy1 pascalCaseId dot) .>> IW |>> Ast.PredicateIdentifier 

let alias = positions (skipString "alias" >>. SW >>. idStartsWithCap) |>> Ast.Alias

let aliasedNamespaceIdentifier = positions (namespaceIdentifier .>>. opt alias) |>> Ast.AliasedNamespaceIdentifier
let tplRegex = Regex(@"^(tpl|template)(([A-Z]\w*)|\d*)$", RegexOptions.Compiled)


let withBacktrackedError p: Parser<_,_> =
    fun stream ->
        let mutable oldState = stream.State
        match p stream with
        | Success(result, restInput, userState) ->
            Reply(result, restInput)
        | _ ->
            Reply(oldState)

let variableX: Parser<string,unit> = IdStartsWithSmallCase >>= 
                                        ( fun s -> 
                                            if keyWordSet.Contains(s) then 
                                                fail ("Expecting: <variable (got keyword)>")
                                            else if tplRegex.IsMatch(s) then 
                                                fail ("Expecting: <variable (got template)>") 
                                            else 
                                            (preturn s)
                                        ) 

let variable = positions variableX <?> "<variable>" |>> Ast.Var 

let variableList = sepBy1 (variable .>> IW) comma

let keywordSelf = skipString "self" .>> IW
let keywordIndex = (skipString "index" <|> skipString "ind") .>> IW  >>% Ast.IndexType


(* FplBlock-related Keywords *)
let keywordPremise = (skipString "premise" <|> skipString "pre") >>. IW 
let keywordConclusion = (skipString "conclusion" <|> skipString "con") >>. IW


(* Statement-related Keywords *)
let keywordDel = skipString "delegate" <|> skipString "del" 
let keywordReturn = (skipString "return" <|> skipString "ret") .>> IW 
let keywordRange = skipString "range" .>> IW 
let keywordLoop = skipString "loop" .>> IW 
let keywordCases = skipString "cases" .>> IW 
let keywordAssert = skipString "assert" .>> IW

(* Predicate-related Keywords *)
let keywordUndefined = positions (skipString "undefined" <|> skipString "undef") .>> IW |>> Ast.Undefined
let keywordTrue = positions (skipString "true") .>> IW  |>> Ast.True  
let keywordFalse = positions (skipString "false") .>> IW |>>  Ast.False  
let keywordAnd = skipString "and" .>> IW 
let keywordOr = skipString "or" .>> IW 
let keywordImpl = skipString "impl" .>> IW 
let keywordIif = skipString "iif" .>> IW 
let keywordXor = skipString "xor" .>> IW 
let keywordNot = skipString "not" .>> IW 
let keywordAll = skipString "all" .>> IW 
let keywordEx = skipString "ex" .>> IW
let keywordIs = skipString "is" .>> IW 


// Via templates, FPL supports generic types, which make it possible to define abstract mathematical
// objects and their properties that defer the concrete
// specification of one or more types until the definition or method is declared and instantiated by
// client code
let keywordTemplate = positions (pstring "template" <|> pstring "tpl") .>> IW |>> Ast.TemplateType

let templateTail = choice [ idStartsWithCap; digits ]

let templateWithTail = positions (many1Strings2 (pstring "template" <|> pstring "tpl") templateTail) .>> IW |>>  Ast.TemplateType

let keywordObject = (skipString "object" <|> skipString "obj") .>> IW >>% Ast.ObjectType 

let objectHeader = choice [
    keywordObject
    (attempt templateWithTail) <|> keywordTemplate
] 

let keywordPredicate = (skipString "predicate" <|> skipString "pred") .>> IW >>% Ast.PredicateType
let keywordFunction = (skipString "function" <|> skipString "func") .>> IW >>% Ast.FunctionalTermType


let theoryNamespace = aliasedNamespaceIdentifier <|> namespaceIdentifier .>> IW

let theoryNamespaceList = sepBy1 theoryNamespace comma 

let keywordUses = (skipString "uses" .>> IW)
let usesClause = positions (keywordUses >>. leftBrace >>. IW >>. theoryNamespaceList .>> rightBrace) |>> Ast.UsesClause

let extensionTail: Parser<unit,unit> = skipString ":end" >>. SW

let extensionHeader: Parser<unit,unit> = skipString ":ext" 

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

let indexVariable = positions ((IdStartsWithSmallCase .>> dollar) .>>. ( digits <|> IdStartsWithSmallCase )) <?> "<indexed variable>" |>> Ast.IndexVariable

let atList = many at

let self = positions (atList .>> keywordSelf) |>> Ast.Self

let entity = (attempt (attempt self <|> indexVariable)) <|> variable

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
let predicateList1, predicateList1Ref = createParserForwardedToRef()
let predicateWithQualification, predicateWithQualificationRef = createParserForwardedToRef()
let paramTuple, paramTupleRef = createParserForwardedToRef()

let coord = choice [ entity; extDigits; dollarDigits ] .>> IW 

let word = regex @"\w+" <?> "<word>" .>> IW
let fplDelegateIdentifier = positions (keywordDel >>. dot >>. word) .>> IW |>> Ast.DelegateId

let fplIdentifier = choice [ fplDelegateIdentifier; entity; extDigits; predicateIdentifier ]

let coordList = (sepBy1 coord comma) .>> IW

let bracketedCoords = positions (leftBracket >>. coordList .>> rightBracket) |>> Ast.BrackedCoordList

let fplRange = (opt coord.>> tilde >>. opt coord) .>> IW

let boundedRange = positions (leftBound .>>. fplRange .>>. rightBound) |>> Ast.ClosedOrOpenRange

let coordInType = choice [ fplIdentifier; indexVariable ] .>> IW 

let coordInTypeList = (sepBy1 coordInType comma) .>> IW

let rangeInType = positions ((opt coordInType .>> tilde) .>>. opt coordInType) |>> Ast.RangeInType

let specificClassType = choice [ objectHeader; xId; predicateIdentifier ] .>> IW

//// later semantics: Star: 0 or more occurrences, Plus: 1 or more occurrences
let callModifier = opt (choice [ star;  plus ] ) 

let bracketedCoordsInType = positions (leftBracket >>. coordInTypeList .>> rightBracket) |>> Ast.BracketedCoordsInType
let boundedRangeInType = positions (leftBound .>>. rangeInType .>>. rightBound) |>> Ast.BoundedRangeInType

// The classType is the last type in FPL we can derive FPL classes from.
// It therefore excludes the in-built FPL-types keywordPredicate, keywordFunction, and keywordIndex
// to restrict it to pure objects.
// In contrast to variableType which can also be used for declaring variables 
// in the scope of FPL building blocks
let bracketModifier = boundedRangeInType <|> bracketedCoordsInType
let classType = positions (specificClassType .>>. opt bracketModifier) |>> Ast.ClassType

let variableTypeWithModifier = positions (callModifier .>>. choice [ keywordIndex; keywordFunction; keywordPredicate; classType ]) |>> Ast.VariableTypeWithModifier

let parenthesisedType = positions (variableTypeWithModifier .>> IW >>. opt paramTuple) |>> Ast.VariableType

let variableType = choice [ parenthesisedType ; variableTypeWithModifier ; classType ] .>> IW

let namedVariableDeclaration = positions ((variableList .>> IW) .>>. ((colon >>. IW) >>. variableType)) |>> Ast.NamedVarDecl
let namedVariableDeclarationList = sepBy namedVariableDeclaration comma

paramTupleRef.Value <- positions ((leftParen >>. IW >>. namedVariableDeclarationList) .>> (IW .>> rightParen)) |>> Ast.ParamTuple
let signature = positions ((predicateIdentifier .>> IW) .>>. paramTuple) |>> Ast.Signature

(* Statements *)
let argumentTuple = positions ((leftParen >>. predicateList) .>> (IW .>> rightParen))  |>> Ast.ArgumentTuple

let fplDelegate = positions (fplDelegateIdentifier .>>. argumentTuple) |>> Ast.Delegate
let assignmentStatement = positions ((predicateWithQualification .>> IW .>> colonEqual) .>>. (IW >>. predicate)) |>> Ast.Assignment
let returnStatement = positions (keywordReturn >>. predicate) |>> Ast.Return

let variableRange = choice [ predicateWithQualification ; boundedRange]

let leftBraceCommented = (leftBrace >>. many CW)
let commentedRightBrace = (many CW .>> rightBrace)

let elseKeyword = skipString "else"
let defaultResult = positions (elseKeyword >>. IW >>. many CW >>. statementList) |>> Ast.DefaultResult
let conditionFollowedByResult = positions ((case >>. IW >>. predicate .>> colon) .>>. (many CW >>. statementList)) |>> Ast.ConditionFollowedByResult
let conditionFollowedByResultList = many1 (many CW >>. conditionFollowedByResult)


let casesStatement = positions (((keywordCases >>. many CW >>. leftParen >>. many CW >>. conditionFollowedByResultList .>> semiColon .>> many CW) .>>. (defaultResult .>> many CW .>> rightParen))) |>> Ast.Cases
let entityIn = entity .>> SW
let entityInVariableRange = ( entityIn .>>. variableRange) .>> IW
let rangeOrLoopBody = entityInVariableRange .>>. (leftParen >>. many CW >>. statementList) .>> (many CW >>. rightParen)
let loopStatement = positions (keywordLoop >>. rangeOrLoopBody) |>> Ast.Loop
let rangeStatement = positions (keywordRange >>. rangeOrLoopBody) |>> Ast.Range

//// Difference of assertion to an axiom: axiom's is followed by a signature of a predicate (i.e. with possible parameters),
//// not by a predicate (i.e. with possible arguments)
//// Difference of assertion to a mandatory property: a mandatory property introduces a completely new identifier inside
//// the scope of a definition. An assertion uses a predicate referring to existing identifiers in the whole theory
//// Difference of assertion to assume: the latter will be used only in the scope of proofs
let assertionStatement = positions (keywordAssert >>. predicate) |>> Ast.Assertion

let statement = 
    (choice [
        casesStatement
        assertionStatement
        rangeStatement
        loopStatement
        returnStatement
        assignmentStatement
    ])

statementListRef.Value <- many (many CW >>. statement .>> IW)

(* Predicates *)

let dotted = dot >>. predicateWithQualification
let qualification = choice [argumentTuple ; dotted ; boundedRange ; bracketedCoords]
predicateWithQualificationRef.Value <- positions (fplIdentifier .>>. opt qualification) |>> Ast.PredicateWithQualification

primePredicateRef.Value <- choice [
    keywordTrue
    keywordFalse
    keywordUndefined
    attempt argumentIdentifier
    predicateWithQualification
    fplIdentifier
    
]

let conjunction = positions ((keywordAnd >>. leftParen >>. predicateList1) .>> rightParen) |>> Ast.And
let disjunction = positions ((keywordOr >>. leftParen >>. predicateList1) .>> rightParen) |>> Ast.Or

let twoPredicatesInParens = (leftParen >>. predicate) .>>. (comma >>. predicate) .>> rightParen 
let onePredicateInParens = (leftParen >>. predicate) .>> rightParen
let implication = positions (keywordImpl >>. twoPredicatesInParens) |>> Ast.Impl
let equivalence = positions (keywordIif >>. twoPredicatesInParens) |>> Ast.Iif
let exclusiveOr = positions (keywordXor >>. twoPredicatesInParens) |>> Ast.Xor
let negation = positions (keywordNot >>. onePredicateInParens) |>> Ast.Not
let all = positions ((keywordAll >>. variableList) .>>. onePredicateInParens) |>> Ast.All
let allAssert = positions ((keywordAll >>. entityInVariableRange) .>>. onePredicateInParens) |>> Ast.AllAssert
let exists = positions ((keywordEx >>. variableList) .>>. onePredicateInParens) |>> Ast.Exists
let existsTimesN = positions (((keywordEx >>. dollarDigits) .>>. (SW >>. variableList)) .>>. onePredicateInParens) |>> Ast.ExistsN
let isOperator = positions ((keywordIs >>. leftParen >>. coordInType) .>>. (comma >>. variableType) .>> rightParen) |>> Ast.IsOperator

// A compound Predicate has its own boolean expressions to avoid mixing up with Pl0Propositions
let compoundPredicate = choice [
    conjunction
    disjunction
    implication
    equivalence
    exclusiveOr
    negation
    (attempt allAssert) <|> all
    (attempt existsTimesN) <|> exists
    isOperator
]

predicateRef.Value <- ((compoundPredicate <|> primePredicate) .>> IW)

predicateListRef.Value <- sepBy predicate comma
predicateList1Ref.Value <- sepBy1 predicate comma

(* FPL building blocks *)

let commentedStatement = many CW >>. statement

let commentedNamedVariableDeclaration = many CW >>. namedVariableDeclaration

let variableSpecification = (attempt commentedStatement) <|> (attempt commentedNamedVariableDeclaration)

let variableSpecificationList = positions (many variableSpecification) |>> Ast.VariableSpecification

(*To simplify the syntax definition, we do not define separate
FplPremiseConclusionBlocks for rules of inference and theorem-like blocks.
The first have a simplified, PL0 semantics, the latter have a more complex, predicative semantics.
However, there is a syntactical simplification of the signature*)
let commentedPredicate = many CW >>. predicate
let premise = many CW >>. (keywordPremise >>. colon >>. predicate) 
let conclusion = many CW >>. (keywordConclusion >>. colon >>. predicate) 
let premiseConclusionBlock = leftBraceCommented >>. variableSpecificationList .>>. premise .>>. conclusion .>> commentedRightBrace

(* FPL building blocks - rules of reference *)
let keywordInference = (skipString "inference" <|> skipString "inf") 
let signatureWithPremiseConclusionBlock = (signature .>> IW) .>>. premiseConclusionBlock |>> Ast.SignatureWithPreConBlock
let ruleOfInference = positions signatureWithPremiseConclusionBlock |>> Ast.RuleOfInference
let ruleOfInferenceList = many1 (many CW >>. ruleOfInference .>> IW) 
let rulesOfInferenceBlock = (keywordInference >>. IW >>. leftBraceCommented >>. many CW >>. ruleOfInferenceList) .>> commentedRightBrace

(* FPL building blocks - Theorem-like statements and conjectures *)
let keywordTheorem = (skipString "theorem" <|> skipString "thm") .>> IW
let keywordLemma = (skipString "lemma" <|> skipString "lem") .>> IW
let keywordProposition = (skipString "proposition" <|> skipString "prop") .>> IW
let keywordCorollary = (skipString "corollary" <|> skipString "cor") .>> IW
let keywordConjecture = (skipString "conjecture" <|> skipString "conj") .>> IW

let theorem = positions (keywordTheorem >>. signatureWithPremiseConclusionBlock) |>> Ast.Theorem
let lemma = positions (keywordLemma >>. signatureWithPremiseConclusionBlock) |>> Ast.Lemma
let proposition = positions (keywordProposition >>. signatureWithPremiseConclusionBlock) |>> Ast.Proposition
let conjecture = positions (keywordConjecture >>. signatureWithPremiseConclusionBlock) |>> Ast.Conjecture

let dollarDigitList = many1 dollarDigits
let referencingIdentifier = predicateIdentifier .>>. dollarDigitList .>> IW
let corollarySignature = referencingIdentifier .>>. paramTuple .>> IW
let corollary = positions (keywordCorollary >>. corollarySignature .>>. premiseConclusionBlock) |>> Ast.Corollary

(* FPL building blocks - Axioms *)

let keywordAxiom = (skipString "axiom" <|> skipString "ax" <|> skipString "postulate" <|> skipString "post") >>. IW
let axiomBlock = leftBraceCommented >>. variableSpecificationList .>>. commentedPredicate .>> commentedRightBrace

let axiom = positions (keywordAxiom >>. signature .>>. (IW >>. axiomBlock)) |>> Ast.Axiom

(* FPL building blocks - Constructors *)

let instanceBlock = leftBrace >>. many CW >>. variableSpecificationList .>> commentedRightBrace
let callConstructorParentClass = positions (opt predicateWithQualification) |>> Ast.ClassConstructorCall
let constructorBlock = leftBraceCommented >>. variableSpecificationList .>>. callConstructorParentClass  .>> commentedRightBrace
let constructor = positions ((signature .>> IW) .>>. constructorBlock) |>> Ast.Constructor

(* FPL building blocks - Properties *)
let keywordMandatory = positions (skipString "mandatory" <|> skipString "mand") .>> IW >>% Ast.Mandatory
let keywordOptional = positions (skipString "optional" <|> skipString "opt") .>> IW >>% Ast.Optional
let predicateInstanceBlock = (leftBraceCommented >>. variableSpecificationList) .>>. (commentedPredicate .>> commentedRightBrace)
let predicateInstance = positions ((keywordPredicate >>. signature) .>>. (many CW >>. predicateInstanceBlock)) |>> Ast.PredicateInstance
let classInstance = positions (variableType .>>. signature .>>. (many CW >>. instanceBlock)) |>> Ast.ClassInstance
let mapping = toArrow >>. IW >>. variableType
let functionalTermSignature = (keywordFunction >>. signature) .>>. (IW >>. mapping)
let functionalTermInstance = positions (functionalTermSignature .>>. (many CW >>. instanceBlock)) |>> Ast.FunctionalTermInstance
let definitionProperty = choice [
    predicateInstance
    functionalTermInstance
    classInstance
]
let propertyHeader = (many CW >>. (keywordMandatory <|> keywordOptional)) 
let property = positions (propertyHeader .>>. definitionProperty) |>> Ast.Property
let propertyList = many1 (many CW >>. property .>> IW)

(* FPL building blocks - Proofs 

    # A Proof relates to the PredicateIdentifier of the Theorem.
    # Because proofs are named, they can stand anywhere inside the theory, not only immediately
    # after the Theorem they prove. This is to enable the users to mix
    # with natural language an provide a proof long after the theorem was stated.

*)
// justifying proof arguments can be the identifiers of Rules of References, conjectures, theorem-like statements, or axioms
let keywordRevoke = (skipString "revoke" <|> skipString "rev") .>> IW 
let revokeArgument = positions (keywordRevoke >>. argumentIdentifier) |>> Ast.RevokeArgument 
let premiseOfToBeProvedTheorem = positions keywordPremise |>> Ast.PremiseReference 
let conclusionOfToBeProvedTheorem = positions keywordConclusion |>> Ast.ConclusionReference 
let premiseOrOtherPredicate = premiseOfToBeProvedTheorem <|> predicate
    
let keywordAssume = skipString "assume" <|> skipString "ass" .>> IW 
let assumeArgument = positions (keywordAssume >>. premiseOrOtherPredicate) |>> Ast.AssumeArgument
let keywordTrivial  = positions (skipString "trivial") .>> IW |>> Ast.Trivial
let keywordQed  = positions (skipString "qed") .>> IW |>> Ast.Qed
let derivedPredicate = predicate |>> Ast.DerivedPredicate
let derivedArgument = choice [
    keywordQed 
    keywordTrivial 
    conclusionOfToBeProvedTheorem 
    derivedPredicate
]

let argumentInference = attempt revokeArgument <|> derivedArgument
let justification = positions (predicateList .>> IW) |>> Ast.Justification
let justifiedArgument = positions ((justification .>> vDash .>> IW) .>>. argumentInference) |>> Ast.JustifiedArgument
let argument = assumeArgument <|> justifiedArgument
let proofArgument = positions ((argumentIdentifier .>> IW) .>>. argument) .>> IW |>> Ast.Argument
let proofArgumentList = many1 (many CW >>. proofArgument)
let keywordProof = (skipString "proof" <|> skipString "prf") .>> IW 
let proofBlock = (leftBraceCommented >>. variableSpecificationList) .>>. (proofArgumentList .>> commentedRightBrace)
let proof = positions ((keywordProof >>. referencingIdentifier) .>>. (IW >>. proofBlock)) |>> Ast.Proof

(* FPL building blocks - Definitions *)

// Predicate building blocks can be defined similarly to classes, they can have properties but they cannot be derived any parent type 
let predicateDefinitionBlock = (leftBraceCommented  >>. variableSpecificationList .>> many CW) .>>. (opt predicate .>> many CW) .>>. opt propertyList .>> commentedRightBrace 
let definitionPredicate = positions ((keywordPredicate >>. signature .>> IW) .>>. predicateDefinitionBlock) |>> Ast.DefinitionPredicate

// Functional term building blocks can be defined similarly to classes, they can have properties but they cannot be derived any parent type 
let functionalTermDefinitionBlock = (leftBraceCommented  >>. variableSpecificationList .>> many CW) .>>. opt propertyList .>> commentedRightBrace
let definitionFunctionalTerm = positions ((functionalTermSignature .>> IW) .>>. functionalTermDefinitionBlock) |>> Ast.DefinitionFunctionalTerm

// Class definitions
let keywordClass = (skipString "class" <|> skipString "cl") >>. IW
let classDefinitionContent = choice [
    property
    constructor
]
let classDefinitionContentList = many (many CW >>. classDefinitionContent .>> IW)
let classDefinitionBlock = (leftBraceCommented  >>. variableSpecificationList .>> many CW) .>>. classDefinitionContentList .>> commentedRightBrace
let classSignature = (keywordClass >>. predicateIdentifier .>> IW) .>>. (colon >>. IW >>. classType)
let definitionClass = positions ((classSignature .>> IW) .>>. classDefinitionBlock) |>> Ast.DefinitionClass 

let definition = choice [
    definitionClass
    definitionPredicate
    definitionFunctionalTerm
]
(* Gathering together all Building Blocks to a theory *)
let keywordTheory = (skipString "theory" <|> skipString "th") >>. IW
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
let keywordLocalization = (skipString "localization" <|> skipString "loc") 
let localizationLanguageCode: Parser<string,unit> = regex @"[a-z]{3}" <?> "<ISO 639 language code>"
let localizationString = positions (regex "\"[^\"\n]*\"") <?> "<\"language-specific string\">" |>> Ast.LocalizationString

let ebnfTransl, ebnfTranslRef = createParserForwardedToRef()
let ebnfTranslTuple = (leftParen >>. IW >>. ebnfTransl) .>> (IW .>> rightParen) 
let ebnfFactor = choice [
    variable
    localizationString
    ebnfTranslTuple
] 
let ebnfTerm = positions (sepEndBy1 ebnfFactor SW) |>> Ast.LocalizationTerm
ebnfTranslRef.Value <-  positions (sepBy1 ebnfTerm (IW >>. case >>. IW)) |>> Ast.LocalizationTermList
let translation = (tilde >>. localizationLanguageCode .>> IW .>> colon) .>>. ebnfTransl
let translationList = many1 (many CW >>. translation .>> IW)
let localization = (predicate .>> IW .>> colonEqual .>> IW) .>>. (translationList .>> IW .>> semiColon)
let localizationList = many1 (many CW >>. localization .>> IW)
let localizationBlock = keywordLocalization >>. IW >>. leftBraceCommented >>. localizationList .>> commentedRightBrace


(* Namespaces *)
let namespaceBlock = (leftBraceCommented >>. opt extensionBlock) .>>. (many CW >>. opt usesClause) .>>. (many CW >>. opt rulesOfInferenceBlock) .>>. (many CW >>. theoryBlock) .>>. (many CW >>. opt localizationBlock) .>> commentedRightBrace
let fplNamespace = positions (namespaceIdentifier .>>. (many CW >>. namespaceBlock)) .>> IW |>> Ast.Namespace
(* Final Parser *)
let ast =  positions fplNamespace <?> "fpl code" |>> Ast.AST

let fplParser (input: string) = tryParse ast input "" (int64 0)
// let fplParser (input: string) = tryParse' ast "recovery failed;" ad input
let parserDiagnostics = ad
