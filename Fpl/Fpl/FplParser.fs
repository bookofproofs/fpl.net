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
let keywordFor = skipString "for" .>> SW 
let keywordIn = skipString "in" .>> SW 
let keywordCases = skipString "cases" .>> SW 
let keywordAssert = skipString "assert" .>> SW

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

let keywordUses = (skipString "uses") .>> SW
let usesClause = positions (keywordUses >>. theoryNamespace) |>> Ast.UsesClause

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

let namedVariableDeclaration = positions (variableList .>>. varDeclModifier .>>. variableType .>> IW) |>> Ast.NamedVarDecl
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

let spacesRightBrace = (IW .>> rightBrace) 

let keywordReturn = IW >>. (skipString "return" <|> skipString "ret") .>> IW 

let defaultResult = positions (IW >>. statementList) |>> Ast.DefaultResult
let conditionFollowedByResult = positions ((case >>. predicate .>> colon) .>>. (IW >>. statementList)) |>> Ast.ConditionFollowedByResult
let conditionFollowedByResultList = many1 (IW >>. conditionFollowedByResult)

let elseStatement = elseCase >>. IW >>. defaultResult .>> IW
let casesStatement = positions (((keywordCases >>. IW >>. leftParen >>. IW >>. conditionFollowedByResultList .>>. elseStatement .>> rightParen))) |>> Ast.Cases

let inDomain = positions (keywordIn >>. (simpleVariableType <|> variableRange) .>> IW) |>> Ast.Domain
let variableInOptDomain = ( (variable .>> IW) .>>. opt inDomain) .>> IW
let variableListInOptDomain = ( variableList .>>. opt inDomain) .>> IW
let variableListInOptDomainList = (sepBy1 variableListInOptDomain comma) .>> IW

let entityInDomain = ( entity .>>. inDomain ) .>> IW
let entityInVariableRange = ( entity .>>. (keywordIn >>. variableRange)) .>> IW
let forInBody = (entityInVariableRange .>> IW) .>>. (leftParen >>. IW >>. statementList) .>> (IW >>. rightParen)
let forStatement = positions (keywordFor >>. forInBody) |>> Ast.ForIn

//// Difference of assertion to an axiom: axiom's is followed by a signature of a predicate (i.e. with possible parameters),
//// not by a predicate (i.e. with possible arguments)
//// Difference of assertion to a mandatory property: a mandatory property introduces a completely new identifier inside
//// the scope of a definition. An assertion uses a predicate referring to existing identifiers in the whole theory
//// Difference of assertion to assume: the latter will be used only in the scope of proofs
let assertionStatement = positions (keywordAssert >>. predicate) |>> Ast.Assertion

let callConstructorParentClass = positions (keywordSelfExclamation >>. specificClassType .>>. argumentTuple .>> IW) |>> Ast.ParentConstructorCall

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
let keywordDeclaration = (skipString "declaration" <|> skipString "dec") .>> SW 

let varDeclBlock = positions (IW >>. keywordDeclaration >>. (many ((tilde >>. namedVariableDeclaration <|> statement) .>> IW)) .>> semiColon) .>> IW |>> Ast.VarDeclBlock 

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

let theorem = positions (keywordTheorem >>. signatureWithPremiseConclusionBlock) |>> Ast.Theorem
let lemma = positions (keywordLemma >>. signatureWithPremiseConclusionBlock) |>> Ast.Lemma
let proposition = positions (keywordProposition >>. signatureWithPremiseConclusionBlock) |>> Ast.Proposition
let conjecture = positions (keywordConjecture >>. signatureWithPremiseConclusionBlock) |>> Ast.Conjecture

let exclamationDigitList = many1 exclamationDigits
let referencingIdentifier = predicateIdentifier .>>. exclamationDigitList .>> IW
let corollarySignature = referencingIdentifier .>>. paramTuple .>> IW
let corollary = positions (keywordCorollary >>. corollarySignature .>>. premiseConclusionBlock) |>> Ast.Corollary

(* FPL building blocks - Axioms *)

let keywordAxiom = (skipString "axiom" <|> skipString "ax" <|> skipString "postulate" <|> skipString "post") >>. SW
let axiomBlock = leftBrace >>. varDeclOrSpecList .>>. spacesPredicate .>> spacesRightBrace

let axiom = positions (keywordAxiom >>. signature .>> IW .>>. axiomBlock) |>> Ast.Axiom

(* FPL building blocks - Constructors *)

let keywordIntrinsic = (skipString "intrinsic" <|> skipString "intr") .>> IW >>% Ast.Intrinsic

let predContent = varDeclOrSpecList .>>. spacesPredicate |>> Ast.DefPredicateContent

let classContent = varDeclOrSpecList .>>. keywordSelf |>> Ast.DefClassContent
let keywordConstructor = (skipString "constructor" <|> skipString "ctor") .>> SW
let constructorBlock = leftBrace >>. varDeclOrSpecList .>>. keywordSelf .>> spacesRightBrace 
let constructor = positions (keywordConstructor >>. signature .>>. constructorBlock) |>> Ast.Constructor

(* FPL building blocks - Properties *)
let keywordOptional = positions (skipString "optional" <|> skipString "opt") .>> SW >>% Ast.Optional
let keywordMandatory = positions (skipString "mandatory" <|> skipString "mand") .>> SW >>% Ast.Optional

let predInstanceBlock = leftBrace >>. (keywordIntrinsic <|> predContent) .>> spacesRightBrace
let predicateInstance = positions (keywordPredicate >>. signature .>>. (IW >>. predInstanceBlock)) |>> Ast.PredicateInstance

let classInstanceBlock = leftBrace >>. (keywordIntrinsic <|> classContent) .>> spacesRightBrace
let classInstance = positions (variableType .>>. signature .>>. classInstanceBlock) |>> Ast.ClassInstance
let mapping = toArrow >>. IW >>. variableType
let functionalTermSignature = (keywordFunction >>. signature) .>>. (IW >>. mapping) .>> IW 

let funcContent = varDeclOrSpecList .>>. (keywordReturn >>. (fplDelegate <|> predicateWithQualification) .>> IW) |>> Ast.DefFunctionContent
let functionalTermInstanceBlock = leftBrace >>. (keywordIntrinsic <|> funcContent) .>> spacesRightBrace
let functionalTermInstance = positions (functionalTermSignature .>>. functionalTermInstanceBlock) |>> Ast.FunctionalTermInstance

let definitionProperty = choice [
    predicateInstance
    functionalTermInstance
    classInstance
]
let propertyHeader = IW >>. (keywordOptional <|> keywordMandatory)
let property = positions (propertyHeader .>>. definitionProperty) |>> Ast.Property
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
let premiseOfToBeProvedTheorem = positions keywordPremise |>> Ast.PremiseReference 
let conclusionOfToBeProvedTheorem = positions keywordConclusion |>> Ast.ConclusionReference 
let premiseOrOtherPredicate = premiseOfToBeProvedTheorem <|> predicate
    
let keywordAssume = skipString "assume" <|> skipString "ass" .>> SW 
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

let argumentInference = vDash >>. IW >>. (revokeArgument <|> derivedArgument)
let justification = positions (predicateList .>> IW) |>> Ast.Justification
let justifiedArgument = positions (justification .>>. argumentInference) |>> Ast.JustifiedArgument
let argument = assumeArgument <|> justifiedArgument
let proofArgument = positions ((argumentIdentifier .>> IW) .>>. argument) .>> IW |>> Ast.Argument
let proofArgumentList = many1 (IW >>. proofArgument)
let keywordProof = (skipString "proof" <|> skipString "prf") .>> SW 
let proofBlock = (leftBrace >>. varDeclOrSpecList) .>>. (proofArgumentList .>> spacesRightBrace)
let proof = positions ((keywordProof >>. referencingIdentifier) .>>. (IW >>. proofBlock)) |>> Ast.Proof

(* FPL building blocks - Definitions *)

// Predicate building blocks can be defined similarly to classes, they can have properties but they cannot be derived any parent type 
let predicateDefinitionBlock = leftBrace  >>. ((keywordIntrinsic <|> predContent) .>> IW) .>>. propertyList .>> spacesRightBrace 
let definitionPredicate = positions (keywordPredicate >>. (signature .>> IW) .>>. predicateDefinitionBlock) |>> Ast.DefinitionPredicate

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

let classSignature = (keywordClass >>. predicateIdentifier .>> IW) .>>. classTypeWithModifierList
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
let localizationString = positions (regex "\"[^\"\n]*\"") <?> "<\"language-specific string\">" |>> Ast.LocalizationString

let ebnfTransl, ebnfTranslRef = createParserForwardedToRef()
let ebnfTranslTuple = (leftParen >>. IW >>. ebnfTransl) .>> (IW .>> rightParen) 
let ebnfFactor = choice [
    variable
    localizationString
    ebnfTranslTuple
] 
let ebnfTerm = positions (sepEndBy1 ebnfFactor SW) |>> Ast.LocalizationTerm
ebnfTranslRef.Value <-  positions (sepBy1 ebnfTerm (IW >>. comma >>. IW)) |>> Ast.LocalizationTermList
let translation = (tilde >>. localizationLanguageCode .>> IW .>> colon) .>>. ebnfTransl
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

let buildingBlockList = many (IW >>. buildingBlock .>> IW)

(* Namespaces *)
let namespaceBlock = (leftBrace >>. opt extensionBlock) .>>. (IW >>. buildingBlockList) .>> spacesRightBrace
let fplNamespace = positions (namespaceIdentifier .>>. (IW >>. namespaceBlock)) .>> IW |>> Ast.Namespace
(* Final Parser *)
let ast =  positions (IW >>. fplNamespace) |>> Ast.AST

let fplParserAst (input: string) = tryParse ast ad input 0 input.Length "SYN000" -1

let fplParser (input:string) = 
    let preProcessedInput = preParsePreProcess input
    let matchArray = stringMatches preProcessedInput errRecPattern
    for i in [0..matchArray.Length-1] do
        let index, subString = matchArray[i]
        let nextIndex = 
            if i + 1 < matchArray.Length-1 then
                let nextIndex, s = matchArray[i+1]
                nextIndex
            else
                subString.Length
        if index > 0 then
            match subString with
            | v when v.StartsWith("definition") || v.StartsWith("def") 
                -> tryParse definition ad v index nextIndex "DEF000" -1 
            | v when v.StartsWith("mand") || v.StartsWith("opt") 
                -> tryParse property ad v index nextIndex "PRP000" -1 
            | v when v.StartsWith("ax") || v.StartsWith("post") 
                -> tryParse axiom ad v index nextIndex "AXI000" -1 
            | v when v.StartsWith("theorem") || v.StartsWith("thm") 
                -> tryParse theorem ad v index nextIndex "THM000" -1 
            | v when v.StartsWith("prop")
                -> tryParse proposition ad v index nextIndex "THM000" -1 
            | v when v.StartsWith("lem") 
                -> tryParse lemma ad v index nextIndex "THM000" -1 
            | v when v.StartsWith("cor") 
                -> tryParse corollary ad v index nextIndex "COR000" -1 
            | v when v.StartsWith("conj") 
                -> tryParse conjecture ad v index nextIndex "CNJ000" -1 
            | v when v.StartsWith("dec") 
                -> tryParse varDeclBlock ad v index nextIndex "VAR000" -1  
            | v when v.StartsWith("constructor") || v.StartsWith("ctor") 
                -> tryParse constructor ad v index nextIndex "CTR000" -1 
            | v when v.StartsWith("proof") || v.StartsWith("prf") 
                -> tryParse proof ad v index nextIndex "PRF000" -1 
            | v when v.StartsWith("inf") 
                -> tryParse ruleOfInference ad v index nextIndex "INF000" -1 
            | v when v.StartsWith("loc") 
                -> tryParse localization ad v index nextIndex "LOC000" -1 
            | v when v.StartsWith("uses") 
                -> tryParse usesClause ad v index nextIndex "USE000" -1 
            | v when v.StartsWith("and")  || v.StartsWith("or") || v.StartsWith("impl") || v.StartsWith("iif") || v.StartsWith("xor") || v.StartsWith("not") || v.StartsWith("all") || v.StartsWith("ex") || v.StartsWith("is")   
                -> tryParse compoundPredicate ad v index nextIndex "PRE000" -1 
            | v when v.StartsWith("assert")  || v.StartsWith("cases") || v.StartsWith("self!") || v.StartsWith("for") || v.StartsWith("del")  
                -> tryParse statement ad v index nextIndex "SMT000" -1 
            | v when v.StartsWith("|-") 
                -> tryParse argumentInference ad v index nextIndex "AGI000" -1 
            | v when v.StartsWith("|") 
                -> tryParse conditionFollowedByResult ad v index nextIndex "CAS000" -1 
            | v when v.StartsWith("?") 
                -> tryParse elseStatement ad v index nextIndex "DCS000" -1 
            | _ -> tryParse ast ad input index nextIndex "SYN000" -1 
            |> ignore
        else
            tryParse ast ad input index nextIndex "SYN000" -1 
            |> ignore
    fplParserAst preProcessedInput 

let parserDiagnostics = ad

