module FplParser
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
let rightBrace = skipChar '}' >>. spaces
let leftParen = skipChar '(' >>. spaces 
let rightParen = skipChar ')' 
let comma = skipChar ',' >>. spaces 
let dot = skipChar '.'
let colon = skipChar ':' >>. spaces >>% Ast.One
let colonStar = skipString ":*" >>. spaces >>% Ast.Many
let colonPlus = skipString ":+" >>. spaces >>% Ast.Many1
let colonEqual = skipString ":=" >>. spaces 
let equals = skipString "=" >>. spaces
let at = pchar '@'
let case = skipChar '|' >>. spaces
let elseCase = skipChar '?' >>. spaces
let leftBracket = skipChar '<' >>. spaces 
let rightBracket = skipChar '>' >>. spaces  
let leftClosedBracket = skipChar '[' >>. spaces <?> "<(closed) left bound '['>"
let leftOpenBracket = skipString "[(" >>. spaces <?> "<(open) left bound '[('>"
let rightOpenBracket = skipString ")]" >>. spaces <?> "<(open) right bound ')]'>" 
let rightClosedBracket = skipChar ']' >>. spaces <?> "<(closed) right bound ']'>" 
let tilde = skipChar '~' .>> spaces
let semiColon = skipChar ';' >>. spaces
let exclamationMark = skipChar '!'
let toArrow = skipString "->"
let vDash = skipString "|-"

(* Whitespaces and Comments *)

let IW = spaces <?> "<whitespace>"

let SW = spaces1 <?> "<significant whitespace>"

let inlineComment = pstring "//" >>. skipManyTill anyChar (skipNewline) <?> "<inline comment>" |>> ignore 

let blockComment = (pstring "/*" >>. (skipManyTill anyChar (pstring "*/"))) <?> "<block comment>" |>> ignore 

let CW = many (choice [ blockComment; inlineComment; SW ])

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

let variableX: Parser<string,unit> = 
    IdStartsWithSmallCase 
    <?> "<variable>" 
    |> resultSatisfies (fun s -> keyWordSet.Contains(s) |> not) "Expecting: <variable (got keyword)>" 
    |> resultSatisfies (fun s -> tplRegex.IsMatch(s) |> not) "Expecting: <variable (got template)>"
    >>= (fun s -> preturn s) 

let variable = positions variableX |>> Ast.Var 

let variableList = (sepBy1 (variable .>> IW) comma) .>> IW

let keywordSelf = positions (skipString "self") .>> IW |>> Ast.Self
let keywordSelfExclamation = skipString "self!" .>> IW
let keywordIndex = (skipString "index" <|> skipString "ind") .>> IW  >>% Ast.IndexType


(* FplBlock-related Keywords *)
let keywordPremise = (skipString "premise" <|> skipString "pre") >>. IW 
let keywordConclusion = (skipString "conclusion" <|> skipString "con") >>. IW


(* Statement-related Keywords *)
let keywordDel = skipString "delegate" <|> skipString "del" 
let keywordFor = skipString "for" .>> IW 
let keywordIn = skipString "in" .>> IW 
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
let keywordExN = skipString "exn" .>> IW
let keywordIs = skipString "is" .>> IW 


// Via templates, FPL supports generic types, which make it possible to define abstract mathematical
// objects and their properties that defer the concrete
// specification of one or more types until the definition or method is declared and instantiated by
// client code
let keywordTemplate = positions (pstring "template" <|> pstring "tpl") .>> IW |>> Ast.TemplateType

let templateTail = choice [ idStartsWithCap; (regex @"\d+") ]

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
let usesClause = positions (keywordUses >>. theoryNamespaceList) |>> Ast.UsesClause

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

let exclamationDigits = positions (exclamationMark >>. digits) |>> Ast.ExclamationDigits
let exclamationVarOrDigits = positions (exclamationMark >>. (variable <|> digits)) |>> Ast.ExclamationVarOrDigits

let indexed = opt (many1 (exclamationVarOrDigits))
let indexVariable = positions (variable .>>. indexed) .>> IW |>> Ast.IndexVariable

let atList = many at

let self = positions (atList .>> keywordSelf) |>> Ast.SelfAts

let entity = choice [ self ; exclamationVarOrDigits; indexVariable ]

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


let coord = choice [ entity; extDigits; exclamationVarOrDigits ] .>> IW 

let fplIdentifier = choice [ entity; extDigits; predicateIdentifier ]

let coordList = (sepBy1 coord comma) .>> IW

let bracketedCoords = positions (leftBracket >>. coordList .>> rightBracket) |>> Ast.BrackedCoordList

let fplRange = (opt coord.>> tilde >>. opt coord) .>> IW

let boundedRange = positions (leftBound .>>. fplRange .>>. rightBound) |>> Ast.ClosedOrOpenRange

let coordInType = choice [ fplIdentifier; indexVariable ] .>> IW 

let coordInTypeList = (sepBy1 coordInType comma) .>> IW 

let rangeInType = positions ((opt coordInType .>> tilde) .>>. opt coordInType) |>> Ast.RangeInType

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
let bracketModifier = boundedRangeInType <|> bracketedCoordsInType 
let classType = positions (specificClassType .>>. opt bracketModifier) |>> Ast.ClassType

let simpleVariableType = positions (choice [ keywordIndex; keywordFunction; keywordPredicate; classType ] .>> IW) |>> Ast.SimpleVariableType

let variableType = positions (simpleVariableType .>>. opt paramTuple .>> IW) |>> Ast.VariableType

let namedVariableDeclaration = positions (variableList .>>. varDeclModifier .>>. variableType .>> CW) |>> Ast.NamedVarDecl
let namedVariableDeclarationList = sepBy namedVariableDeclaration comma

paramTupleRef.Value <- positions ((leftParen >>. IW >>. namedVariableDeclarationList) .>> (IW .>> rightParen)) |>> Ast.ParamTuple
let signature = positions ((predicateIdentifier .>> IW) .>>. paramTuple) .>> IW |>> Ast.Signature

(* Statements *)
let argumentTuple = positions ((leftParen >>. predicateList) .>> (IW .>> rightParen))  |>> Ast.ArgumentTuple 

let word = regex @"\w+" <?> "<word>" .>> IW
let fplDelegateIdentifier = positions (keywordDel >>. dot >>. word) .>> IW |>> Ast.DelegateId
let fplDelegate = positions (fplDelegateIdentifier .>>. argumentTuple) |>> Ast.Delegate
let assignmentStatement = positions ((predicateWithQualification .>> IW .>> colonEqual) .>>. predicate) |>> Ast.Assignment

let variableRange = choice [ entity ; boundedRange] 

let leftBraceCommented = (leftBrace >>. CW)
let commentedRightBrace = (CW .>> rightBrace) 

let keywordReturn = CW >>. (skipString "return" <|> skipString "ret") .>> CW 

let defaultResult = positions (CW >>. statementList) |>> Ast.DefaultResult
let conditionFollowedByResult = positions ((case >>. IW >>. predicate .>> colon) .>>. (CW >>. statementList)) |>> Ast.ConditionFollowedByResult
let conditionFollowedByResultList = many1 (CW >>. conditionFollowedByResult)


let casesStatement = positions (((keywordCases >>. CW >>. leftParen >>. CW >>. conditionFollowedByResultList .>>  elseCase .>> CW) .>>. (defaultResult .>> CW .>> rightParen))) |>> Ast.Cases

let inDomain = positions (keywordIn >>. (simpleVariableType <|> variableRange) .>> CW) |>> Ast.Domain
let variableInOptDomain = ( (variable .>> IW) .>>. opt inDomain) .>> IW
let variableListInOptDomain = ( variableList .>>. opt inDomain) .>> IW
let variableListInOptDomainList = (sepBy1 variableListInOptDomain comma) .>> IW

let entityInDomain = ( entity .>>. inDomain ) .>> IW
let entityInVariableRange = ( entity .>>. (keywordIn >>. variableRange)) .>> IW
let forInBody = (entityInVariableRange .>> CW) .>>. (leftParen >>. CW >>. statementList) .>> (CW >>. rightParen)
let forStatement = positions (keywordFor >>. forInBody) |>> Ast.ForIn

//// Difference of assertion to an axiom: axiom's is followed by a signature of a predicate (i.e. with possible parameters),
//// not by a predicate (i.e. with possible arguments)
//// Difference of assertion to a mandatory property: a mandatory property introduces a completely new identifier inside
//// the scope of a definition. An assertion uses a predicate referring to existing identifiers in the whole theory
//// Difference of assertion to assume: the latter will be used only in the scope of proofs
let assertionStatement = positions (keywordAssert >>. predicate) |>> Ast.Assertion

let callConstructorParentClass = positions (keywordSelfExclamation >>. specificClassType .>>. argumentTuple .>> CW) |>> Ast.ParentConstructorCall

let statement = 
    (choice [
        callConstructorParentClass
        casesStatement
        assertionStatement
        forStatement
        assignmentStatement
        fplDelegate
    ])

statementListRef.Value <- many (CW >>. statement .>> CW)

(* Predicates *)

let dotted = dot >>. predicateWithQualification 
let qualification = choice [boundedRange ; bracketedCoords ; argumentTuple] 
predicateWithQualificationRef.Value <- positions (fplIdentifier .>>. opt qualification) .>>. opt dotted |>> Ast.PredicateWithQualification 

primePredicateRef.Value <- choice [
    keywordTrue
    keywordFalse
    keywordUndefined
    attempt argumentIdentifier
    fplDelegate 
    predicateWithQualification
]

let conjunction = positions ((keywordAnd >>. leftParen >>. predicateList1) .>> rightParen) |>> Ast.And
let disjunction = positions ((keywordOr >>. leftParen >>. predicateList1) .>> rightParen) |>> Ast.Or

let twoPredicatesInParens = (leftParen >>. predicate) .>>. (comma >>. predicate) .>> rightParen 
let onePredicateInParens = (leftParen >>. predicate) .>> rightParen
let implication = positions (keywordImpl >>. twoPredicatesInParens) |>> Ast.Impl
let equivalence = positions (keywordIif >>. twoPredicatesInParens) |>> Ast.Iif
let exclusiveOr = positions (keywordXor >>. twoPredicatesInParens) |>> Ast.Xor
let negation = positions (keywordNot >>. onePredicateInParens) |>> Ast.Not
let all = positions ((keywordAll >>. variableListInOptDomainList) .>>. onePredicateInParens) |>> Ast.All
let exists = positions ((keywordEx >>. variableListInOptDomainList) .>>. onePredicateInParens) |>> Ast.Exists

let existsTimesN = positions (((keywordExN >>. exclamationDigits .>> SW) .>>. variableInOptDomain) .>>. onePredicateInParens) |>> Ast.ExistsN
let isOperator = positions ((keywordIs >>. leftParen >>. coordInType) .>>. (comma >>. variableType) .>> rightParen) |>> Ast.IsOperator

// equality operator
let equalityComparison = (leftBracket >>. sepBy1 predicate equals .>> rightBracket) |>> Ast.EqualityComparison

// A compound Predicate has its own boolean expressions to avoid mixing up with Pl0Propositions
let compoundPredicate = choice [
    equalityComparison
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



predicateRef.Value <- choice [compoundPredicate; primePredicate] .>> IW 

predicateListRef.Value <- sepBy predicate comma
predicateList1Ref.Value <- sepBy1 predicate comma

(* FPL building blocks *)
let keywordDeclaration = (skipString "declaration" <|> skipString "dec") .>> CW 

let varDeclBlock = positions (CW >>. keywordDeclaration >>. (many ((tilde >>. namedVariableDeclaration <|> statement) .>> CW)) .>> semiColon) .>> CW |>> Ast.VarDeclBlock 

let varDeclOrSpecList = opt (many1 (varDeclBlock))
(*To simplify the syntax definition, we do not define separate
FplPremiseConclusionBlocks for rules of inference and theorem-like blocks.
The first have a simplified, PL0 semantics, the latter have a more complex, predicative semantics.
However, there is a syntactical simplification of the signature*)
let commentedPredicate = CW >>. predicate
let premise = CW >>. (keywordPremise >>. colon >>. predicate) 
let conclusion = CW >>. (keywordConclusion >>. colon >>. predicate) 
let premiseConclusionBlock = leftBraceCommented >>. varDeclOrSpecList .>>. premise .>>. conclusion .>> commentedRightBrace

(* FPL building blocks - rules of reference *)
let keywordInference = (skipString "inference" <|> skipString "inf") .>> IW 
let signatureWithPremiseConclusionBlock = signature .>>. premiseConclusionBlock |>> Ast.SignatureWithPreConBlock
let ruleOfInference = positions (keywordInference >>. signatureWithPremiseConclusionBlock) |>> Ast.RuleOfInference

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

let exclamationDigitList = many1 exclamationDigits
let referencingIdentifier = predicateIdentifier .>>. exclamationDigitList .>> IW
let corollarySignature = referencingIdentifier .>>. paramTuple .>> IW
let corollary = positions (keywordCorollary >>. corollarySignature .>>. premiseConclusionBlock) |>> Ast.Corollary

(* FPL building blocks - Axioms *)

let keywordAxiom = (skipString "axiom" <|> skipString "ax" <|> skipString "postulate" <|> skipString "post") >>. IW
let axiomBlock = leftBraceCommented >>. varDeclOrSpecList .>>. commentedPredicate .>> commentedRightBrace

let axiom = positions (keywordAxiom >>. signature .>> IW .>>. axiomBlock) |>> Ast.Axiom

(* FPL building blocks - Constructors *)

let keywordIntrinsic = (skipString "intrinsic" <|> skipString "intr") .>> CW >>% Ast.Intrinsic

let predContent = varDeclOrSpecList .>>. commentedPredicate |>> Ast.DefPredicateContent

let classContent = varDeclOrSpecList .>>. keywordSelf |>> Ast.DefClassContent
let keywordConstructor = (skipString "constructor" <|> skipString "ctor") .>> IW
let constructorBlock = leftBraceCommented >>. varDeclOrSpecList .>>. keywordSelf .>> commentedRightBrace 
let constructor = positions (keywordConstructor >>. signature .>>. constructorBlock) |>> Ast.Constructor

(* FPL building blocks - Properties *)
let keywordOptional = positions (skipString "optional" <|> skipString "opt") .>> IW >>% Ast.Optional
let keywordMandatory = positions (skipString "mandatory" <|> skipString "mand") .>> IW >>% Ast.Optional

let predInstanceBlock = leftBraceCommented >>. (keywordIntrinsic <|> predContent) .>> commentedRightBrace
let predicateInstance = positions (keywordPredicate >>. signature .>>. (CW >>. predInstanceBlock)) |>> Ast.PredicateInstance

let classInstanceBlock = leftBraceCommented >>. (keywordIntrinsic <|> classContent) .>> commentedRightBrace
let classInstance = positions (variableType .>>. signature .>>. classInstanceBlock) |>> Ast.ClassInstance
let mapping = toArrow >>. IW >>. variableType
let functionalTermSignature = (keywordFunction >>. signature) .>>. (IW >>. mapping) .>> IW 

let funcContent = varDeclOrSpecList .>>. (keywordReturn >>. (fplDelegate <|> predicateWithQualification) .>> IW) |>> Ast.DefFunctionContent
let functionalTermInstanceBlock = leftBraceCommented >>. (keywordIntrinsic <|> funcContent) .>> commentedRightBrace
let functionalTermInstance = positions (functionalTermSignature .>>. functionalTermInstanceBlock) |>> Ast.FunctionalTermInstance

let definitionProperty = choice [
    predicateInstance
    functionalTermInstance
    classInstance
]
let propertyHeader = CW >>. (keywordOptional <|> keywordMandatory)
let property = positions (propertyHeader .>>. definitionProperty) |>> Ast.Property
let propertyList = opt (many1 (property .>> CW)) 

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
let proofArgumentList = many1 (CW >>. proofArgument)
let keywordProof = (skipString "proof" <|> skipString "prf") .>> IW 
let proofBlock = (leftBraceCommented >>. varDeclOrSpecList) .>>. (proofArgumentList .>> commentedRightBrace)
let proof = positions ((keywordProof >>. referencingIdentifier) .>>. (IW >>. proofBlock)) |>> Ast.Proof

(* FPL building blocks - Definitions *)

// Predicate building blocks can be defined similarly to classes, they can have properties but they cannot be derived any parent type 
let predicateDefinitionBlock = leftBraceCommented  >>. ((keywordIntrinsic <|> predContent) .>> CW) .>>. propertyList .>> commentedRightBrace 
let definitionPredicate = positions (keywordPredicate >>. (signature .>> CW) .>>. predicateDefinitionBlock) |>> Ast.DefinitionPredicate

// Functional term building blocks can be defined similarly to classes, they can have properties but they cannot be derived any parent type 
let functionalTermDefinitionBlock = leftBraceCommented  >>. ((keywordIntrinsic <|> funcContent) .>> CW) .>>. propertyList .>> commentedRightBrace
let definitionFunctionalTerm = positions ((functionalTermSignature .>> CW) .>>. functionalTermDefinitionBlock) |>> Ast.DefinitionFunctionalTerm

// Class definitions
let keywordClass = (skipString "class" <|> skipString "cl") >>. IW

let constructorList = many1 (constructor .>> CW)
let classCompleteContent = varDeclOrSpecList .>>. constructorList|>> Ast.DefClassCompleteContent
let classDefinitionBlock = leftBraceCommented  >>. ((keywordIntrinsic <|> classCompleteContent) .>> CW) .>>. propertyList .>> commentedRightBrace
let classTypeWithModifier = positions (varDeclModifier .>>. classType .>> IW) |>> Ast.ClassTypeWithModifier
let classTypeWithModifierList = sepBy1 classTypeWithModifier comma

let classSignature = (keywordClass >>. predicateIdentifier .>> IW) .>>. classTypeWithModifierList
let definitionClass = positions ((classSignature .>> CW) .>>. classDefinitionBlock) |>> Ast.DefinitionClass 

let keywordDefinition = (skipString "definition" <|> skipString "def") >>. IW
let definition = keywordDefinition >>. choice [
    definitionClass
    definitionPredicate
    definitionFunctionalTerm
]
(* Gathering together all Building Blocks to a theory *)

(* Localizations *)
// Localizations provide a possibility to automatically translate FPL expressions into natural languages
let keywordLocalization = (skipString "localization" <|> skipString "loc") >>. IW
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
let translationList = many1 (CW >>. translation .>> IW)
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
]

let buildingBlockList = many (CW >>. buildingBlock .>> IW)

(* Namespaces *)
let namespaceBlock = (leftBraceCommented >>. opt extensionBlock) .>>. (CW >>. opt usesClause) .>>. (CW >>. buildingBlockList) .>> commentedRightBrace
let fplNamespace = positions (namespaceIdentifier .>>. (CW >>. namespaceBlock)) .>> IW |>> Ast.Namespace
(* Final Parser *)
let ast =  positions (IW >>. fplNamespace) |>> Ast.AST

let fplParserDef (input: string) (startIndexOfInput:int) (maxIndex:int) = tryParse definition ad input startIndexOfInput maxIndex "DEF000" -1
let fplParserProperty (input: string) (startIndexOfInput:int) (maxIndex:int) = tryParse property ad input startIndexOfInput maxIndex "PRP000" -1
let fplParserAxiom (input: string)  (startIndexOfInput:int) (maxIndex:int) = tryParse axiom ad input startIndexOfInput maxIndex "AXI000" -1
let fplParserTheorem (input: string)  (startIndexOfInput:int) (maxIndex:int) = tryParse theorem ad input startIndexOfInput maxIndex "THM000" -1
let fplParserProposition (input: string)  (startIndexOfInput:int) (maxIndex:int) = tryParse proposition ad input startIndexOfInput maxIndex "THM000" -1
let fplParserLemma (input: string) (startIndexOfInput:int) (maxIndex:int) = tryParse lemma ad input startIndexOfInput maxIndex "THM000" -1
let fplParserCorollary (input: string) (startIndexOfInput:int) (maxIndex:int) = tryParse corollary ad input startIndexOfInput maxIndex "THM000" -1
let fplParserConjecture (input: string) (startIndexOfInput:int) (maxIndex:int) = tryParse conjecture ad input startIndexOfInput maxIndex "CNJ000" -1
let fplParserDeclaration (input: string) (startIndexOfInput:int) (maxIndex:int) = tryParse varDeclBlock ad input startIndexOfInput maxIndex "VAR000" -1
let fplParserConstructor (input: string) (startIndexOfInput:int) (maxIndex:int) = tryParse constructor ad input startIndexOfInput maxIndex "CTR000" -1
let fplParserProof (input: string) (startIndexOfInput:int) (maxIndex:int) = tryParse proof ad input startIndexOfInput maxIndex "PRF000" -1
let fplParserInference (input: string) (startIndexOfInput:int) (maxIndex:int) = tryParse ruleOfInference ad input startIndexOfInput maxIndex "INF000" -1
let fplParserLocalization (input: string) (startIndexOfInput:int) (maxIndex:int) = tryParse localization ad input startIndexOfInput maxIndex "LOC000" -1
let fplUsesClause (input: string) (startIndexOfInput:int) (maxIndex:int) = tryParse usesClause ad input startIndexOfInput maxIndex "USE000" -1

//let fplParser (input: string) = tryParse ast input "" (int64 0) 1 
let fplParserAst (input: string) = tryParse ast ad input 0 input.Length "SYN000" -1

let fplParser (input:string) = 
    let matchArray = stringMatches input
    let mutable lastSuccessfullIndex = 0
    let parserLastPos (_, lastIndex) =  lastSuccessfullIndex <- lastIndex
    for i in [0..matchArray.Length-1] do
        let index, subString = matchArray[i]
        let maxIndex = 
            if i + 1 < matchArray.Length-1 then
                let nextIndex, s = matchArray[i+1]
                nextIndex
            else
                subString.Length
        if index > lastSuccessfullIndex then
            match subString with
            | v when v.StartsWith("definition") || v.StartsWith("def") 
                -> fplParserDef v index maxIndex |> parserLastPos
            | v when v.StartsWith("mand") || v.StartsWith("opt") 
                -> fplParserProperty v index maxIndex |> parserLastPos
            | v when v.StartsWith("ax") || v.StartsWith("post") 
                -> fplParserAxiom v index maxIndex |> parserLastPos
            | v when v.StartsWith("theorem") || v.StartsWith("thm") 
                -> fplParserTheorem v index maxIndex |> parserLastPos
            | v when v.StartsWith("prop")
                -> fplParserProposition v index maxIndex |> parserLastPos
            | v when v.StartsWith("lem") 
                -> fplParserLemma v index maxIndex |> parserLastPos
            | v when v.StartsWith("cor") 
                -> fplParserCorollary v index maxIndex |> parserLastPos
            | v when v.StartsWith("conj") 
                -> fplParserConjecture v index maxIndex |> parserLastPos
            | v when v.StartsWith("dec") 
                -> fplParserDeclaration v index maxIndex |> parserLastPos
            | v when v.StartsWith("constructor") || v.StartsWith("ctor") 
                -> fplParserConstructor v index maxIndex |> parserLastPos
            | v when v.StartsWith("proof") || v.StartsWith("prf") 
                -> fplParserProof v index maxIndex |> parserLastPos
            | v when v.StartsWith("inf") 
                -> fplParserInference v index maxIndex |> parserLastPos
            | v when v.StartsWith("loc") 
                -> fplParserLocalization v index maxIndex |> parserLastPos
            | _ -> fplParserAst subString |> parserLastPos
            |> ignore
    fplParserAst input 

let parserDiagnostics = ad



