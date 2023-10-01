module FplGrammar
open System.Text.RegularExpressions
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
let star = skipChar '*' >>% Ast.Many
let plus = skipChar '+' >>% Ast.Many1
let dot = skipChar '.'
let colon = skipChar ':' >>. spaces
let colonEqual = skipString ":="
let at = pchar '@'
let case = skipChar '|'
let leftBracket = skipChar '[' >>. spaces 
let leftClosedBracket = skipChar '[' >>. spaces <?> "<(closed) left bound '['>"
let leftOpenBracket = skipString "[!" >>. spaces <?> "<(open) left bound '[!'>"
let rightBracket = skipChar ']' >>. spaces 
let rightOpenBracket = skipString "!]" >>. spaces <?> "<(open) right bound '!]'>" 
let rightClosedBracket = skipString "]" >>. spaces <?> "<(closed) right bound ']'>" 
let tilde = skipChar '~' .>> spaces
let semiColon = skipChar ';'
let dollar = skipChar '$'
let toArrow = skipString "->"
let vDash = skipString "|-"

(* Whitespaces and Comments *)

let IW = spaces

let SW = spaces1

let inlineComment = pstring "//" >>. skipManyTill anyChar (skipNewline <|> eof) |>> ignore <?> "<line-comment>"

let blockComment = (pstring "/*" >>. (skipManyTill anyChar (pstring "*/"))) |>> ignore <?> "<multiline-comment>"

let CW = choice [
    blockComment
    inlineComment
    SW
]

let spacesLeftParenSpaces = spaces >>. leftParen >>. spaces
let spacesRightParenSpaces = spaces >>. rightParen >>. spaces

// -----------------------------------------------------
// Extensions of the FPL language (have to be dynamic)! Lacking a pre-processor, we put the rules
// from the Proof of Concept of FPL code manually into the EBNF of the core FPL grammar.
// note that this has to be inserted into:
// the IsOperand choice
// the PredicateOrFunctionalTerm choice
let digits = regex @"\d+" <?> "<digits>" 
let extDigits: Parser<_, unit> = positions (digits) |>> Ast.ExtDigits

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

let IdStartsWithSmallCase = regex @"[a-z]\w*"
let idStartsWithCap = (regex @"[A-Z]\w*") <?> "<PascalCaseId>"
let pascalCaseId = idStartsWithCap |>> Ast.PascalCaseId
let dollarDigits = positions (dollar >>. digits) <?> "<'$' followed by digits>" |>> Ast.DollarDigits
let argumentIdentifier = positions (regex @"\d+([a-z]\w)*\.") <?> "argument identifier" |>> Ast.ArgumentIdentifier

let namespaceIdentifier = positions (sepBy1 pascalCaseId dot) <?> "<fpl namespace identifier, starting with a PascalCaseId>" |>> Ast.NamespaceIdentifier
let predicateIdentifier = positions (sepBy1 pascalCaseId dot) <?> "<fpl identifier, starting with a PascalCaseId>" |>> Ast.PredicateIdentifier 

let aliasKeyword = skipString "alias" <?> "<'alias' keyword>" 
let alias = positions (aliasKeyword >>. SW >>. idStartsWithCap) <?> "<aliased namespace>" |>> Ast.Alias

let aliasedNamespaceIdentifier = positions ((namespaceIdentifier .>> SW) .>>. alias) <?> "<aliased namespace identifier>"|>> Ast.AliasedNamespaceIdentifier
let tplRegex = Regex(@"^(tpl|template)(([A-Z]\w*)|\d*)$", RegexOptions.Compiled)
let variableX: Parser<string,unit> = IdStartsWithSmallCase >>= 
                                        ( fun s -> 
                                            if keyWordSet.Contains(s) then fail ("Cannot use keyword '" + s + "' as a variable") 
                                            else if tplRegex.IsMatch(s) then fail ("Cannot use template '" + s + "' as a variable") 
                                            else (preturn s)
                                        ) <?> "<variable>"
let variable = positions variableX .>> IW <?> "<variable>" |>> Ast.Var 

let variableList = sepBy1 variable comma

let keywordSelf = skipString "self" .>> IW <?> "<'self' keyword>"
let keywordIndex = (skipString "index" <|> skipString "ind") .>> IW <?> "<'index' or 'ind' keyword>" >>% Ast.IndexType


(* FplBlock-related Keywords *)
let keywordPremise = (skipString "premise" <|> skipString "pre") >>. IW <?> "<'premise' or 'pre' keyword>"
let keywordConclusion = (skipString "conclusion" <|> skipString "con") >>. IW <?> "<'conclusion' or 'con' keyword>"


(* Statement-related Keywords *)
let keywordDel = skipString "delegate" <|> skipString "del" <?> "<'delegate' or 'del' keyword>"
let keywordReturn = (skipString "return" <|> skipString "ret") .>> IW <?> "<'return' or 'ret' keyword>"
let keywordRange = skipString "range" .>> IW <?> "<'range' keyword>"
let keywordLoop = skipString "loop" .>> IW <?> "<'loop' keyword>"
let keywordCases = skipString "cases" .>> IW <?> "<'cases' keyword>"
let keywordAssert = skipString "assert" .>> IW <?> "<'assert' keyword>"

(* Predicate-related Keywords *)
let keywordUndefined = positions (skipString "undefined" <|> skipString "undef") .>> IW <?> "<'undefined' or 'undef' keyword>" |>> Ast.Undefined
let keywordTrue = positions (skipString "true") .>> IW <?> "<'true' keyword>" |>> Ast.True  
let keywordFalse = positions (skipString "false") .>> IW <?> "<'false' keyword>" |>> Ast.False  
let keywordAnd = skipString "and" .>> IW <?> "<'and' keyword>"
let keywordOr = skipString "or" .>> IW <?> "<'or' keyword>"
let keywordImpl = skipString "impl" .>> IW <?> "<'impl' keyword>"
let keywordIif = skipString "iif" .>> IW <?> "<'iif' keyword>"
let keywordXor = skipString "xor" .>> IW <?> "<'xor' keyword>"
let keywordNot = skipString "not" .>> IW <?> "<'not' keyword>"
let keywordAll = skipString "all" .>> IW <?> "<'all' keyword>"
let keywordEx = skipString "ex" .>> IW <?> "<'ex' keyword>"
let keywordIs = skipString "is" .>> IW <?> "<'is' keyword>"


// Via templates, FPL supports generic types, which make it possible to define abstract mathematical
// objects and their properties that defer the concrete
// specification of one or more types until the definition or method is declared and instantiated by
// client code
let keywordTemplate = positions (pstring "template" <|> pstring "tpl") .>> IW <?> "<'template' or 'tpl' keyword>" |>> Ast.TemplateType

let templateTail = choice [ idStartsWithCap; digits ]

let templateWithTail = positions (many1Strings2 (pstring "template" <|> pstring "tpl") templateTail) .>> IW <?> "<'template' or 'tpl' keyword, followed by digits or a PascalCaseId>" |>>  Ast.TemplateType

let keywordObject = (skipString "object" <|> skipString "obj") .>> IW <?> "<'object' or 'obj' keyword>" >>% Ast.ObjectType 

let objectHeader = choice [
    keywordObject
    (attempt templateWithTail) <|> keywordTemplate
] 

let keywordPredicate = skipString "predicate" <|> skipString "pred" <?> "<'predicate' or 'pred' keyword>" >>% Ast.PredicateType
let keywordFunction = skipString "function" <|> skipString "func" <?> "<'function' or 'func' keyword>" >>% Ast.FunctionalTermType


let theoryNamespace = (attempt aliasedNamespaceIdentifier <|> namespaceIdentifier) .>> IW <?> "<namespace or aliased namespace (starting with a PascalCaseId)>"

let theoryNamespaceList = sepBy1 theoryNamespace comma 

let keywordUses = (skipString "uses" .>> IW) <?> "<'uses' keyword>"
let usesClause = positions (keywordUses >>. leftBrace >>. IW >>. theoryNamespaceList .>> IW .>> rightBrace) |>> Ast.UsesClause

let extensionTail: Parser<unit,unit> = skipString ":end" >>. SW

let extensionHeader: Parser<unit,unit> = skipString ":ext" 

let extensionName = positions (skipString "ext" >>. idStartsWithCap .>> IW) <?> "<'ext' followed by PascalCaseId>"|>> Ast.Extensionname

let extensionRegex: Parser<_, unit>  = skipChar ':' >>. IW >>. regex @"\/(?!:end).*" .>> IW |>> Ast.ExtensionRegex

let extensionBlock = positions (extensionHeader >>. IW >>. extensionName .>>. extensionRegex .>> extensionTail) <?> "extension block" |>> Ast.ExtensionBlock


(* Signatures, Variable Declarations, and Types, Ranges and Coordinates *)
// convention: All syntax production rules of FPL syntax extensions have to start with "ext", followed by
// a Pascal Case id.
// This ensures that they will not be mixed-up with original FPL ebnf productions
// that are all PascalCase as well as FPL keywords, that are all small case.

let xId = positions (at >>. extensionName) <?> "<'@ext', followed by PascalCaseId>" |>> Ast.ExtensionType 

let indexVariable = positions ((IdStartsWithSmallCase .>> dollar) .>>. ( digits <|> IdStartsWithSmallCase )) .>> IW <?> "index variable" |>> Ast.IndexVariable

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
let coordOfEntity, coordOfEntityRef = createParserForwardedToRef()
let predicate, predicateRef = createParserForwardedToRef()
let predicateList, predicateListRef = createParserForwardedToRef()
let predicateWithArguments, predicateWithArgumentsRef = createParserForwardedToRef()
let paramTuple, paramTupleRef = createParserForwardedToRef()

let entityWithCoord = positions (entity .>>. coordOfEntity) |>> Ast.EntityWithCoord

let assignee = (attempt entityWithCoord) <|> entity

let coord = choice [ assignee; extDigits; dollarDigits ]

let word = regex @"\w+" <?> "<word>" .>> IW
let fplDelegateIdentifier = positions (keywordDel >>. dot >>. word) |>> Ast.DelegateId

let fplIdentifier = choice [ fplDelegateIdentifier ; coord; predicateIdentifier ]


let coordList = sepEndBy1 coord comma

let bracketedCoords = positions (leftBracket >>. coordList .>> rightBracket) |>> Ast.BrackedCoordList

let fplRange = (opt coord.>> tilde >>. opt coord) .>> IW

let boundedRange = positions (leftBound .>>. fplRange .>>. rightBound) |>> Ast.ClosedOrOpenRange

coordOfEntityRef.Value <- choice [boundedRange ; bracketedCoords]

let coordInType = choice [ fplIdentifier; indexVariable ] 

let coordInTypeList = (sepBy1 coordInType comma) .>> IW

let rangeInType = positions ((opt coordInType .>> tilde) .>>. opt coordInType) |>> Ast.RangeInType

let specificClassType = choice [ objectHeader; xId; predicateIdentifier ] .>> IW

//// later semantics: Star: 0 or more occurrences, Plus: 1 or more occurrences
let callModifier = opt (choice [ star;  plus ]) <?> "<optional '*' or '+'>"

let bracketedCoordsInType = positions (leftBracket >>. coordInTypeList .>> rightBracket) |>> Ast.BracketedCoordsInType
let boundedRangeInType = positions (leftBound .>>. rangeInType .>>. rightBound) |>> Ast.BoundedRangeInType

// The classType is the last type in FPL we can derive FPL classes from.
// It therefore excludes the in-built FPL-types keywordPredicate, keywordFunction, and keywordIndex
// to restrict it to pure objects.
// In contrast to variableType which can also be used for declaring variables 
// in the scope of FPL building blocks
let bracketModifier = choice [boundedRangeInType ; bracketedCoordsInType]
let classType = positions (specificClassType .>>. opt bracketModifier) |>> Ast.ClassType

let modifieableClassType = positions (callModifier .>>. classType) |>> Ast.VariableTypeWithModifier
let modifieablePredicateType = positions (callModifier .>>. keywordPredicate) |>> Ast.VariableTypeWithModifier
let modifieableFunctionType = positions (callModifier .>>. keywordFunction) |>> Ast.VariableTypeWithModifier
let modifieableIndexType = positions (callModifier .>>. keywordIndex) |>> Ast.VariableTypeWithModifier

let variableTypeWithModifier = (((attempt modifieableIndexType) <|> attempt modifieableFunctionType) <|> attempt modifieablePredicateType) <|> modifieableClassType 

let parenthesisedType = positions (variableTypeWithModifier .>> IW >>. paramTuple) |>> Ast.VariableType

let variableType = (((attempt parenthesisedType) <|> attempt variableTypeWithModifier) <|> classType) .>> IW

let namedVariableDeclaration = positions ((variableList .>> IW) .>>. ((colon >>. IW) >>. variableType)) |>> Ast.NamedVarDecl
let namedVariableDeclarationList = sepBy namedVariableDeclaration comma

paramTupleRef.Value <- positions ((leftParen >>. IW >>. namedVariableDeclarationList) .>> (IW .>> rightParen)) <?> "<tuple of parameters (open with '(')>" |>> Ast.ParamTuple
let signature = positions ((predicateIdentifier .>> IW) .>>. paramTuple) |>> Ast.Signature

(* Statements *)
let argumentTuple = (spacesLeftParenSpaces >>. predicateList) .>> (IW .>> spacesRightParenSpaces)  

let fplDelegate = positions (fplDelegateIdentifier .>>. argumentTuple) <?> "delegate identifier followed by a tuple of arguments" |>> Ast.Delegate
let assignmentStatement = positions ((assignee .>> IW .>> colonEqual) .>>. (IW >>. predicate)) <?> "assignment statement"|>> Ast.Assignment
let returnStatement = positions (keywordReturn >>. SW >>. predicate) <?> "return statement" |>> Ast.Return

let variableRange = choice [
    boundedRange
    assignee
]

let leftBraceCommented = (leftBrace >>. many CW)
let commentedRightBrace = (many CW .>> rightBrace)

let elseKeyword = skipString "else"
let defaultResult = positions (elseKeyword >>. IW >>. many CW >>. statementList) <?> "'else' keyword followed by statement list" |>> Ast.DefaultResult
let conditionFollowedByResult = positions ((case >>. IW >>. predicate .>> colon) .>>. (many CW >>. statementList)) <?> "'|' followed by predicate followed by ':' followed by statement list" |>> Ast.ConditionFollowedByResult
let conditionFollowedByResultList = many1 (many CW >>. conditionFollowedByResult)


let casesStatement = positions (((keywordCases >>. many CW >>. leftParen >>. many CW >>. conditionFollowedByResultList .>> semiColon .>> many CW) .>>. (defaultResult .>> many CW .>> rightParen))) <?> "cases statement" |>> Ast.Cases
let assigneeWithVariableRange = ((assignee .>> SW) .>>. variableRange .>> many CW)
let rangeOrLoopBody = assigneeWithVariableRange .>>. (leftParen >>. many CW >>. statementList) .>> (many CW >>. rightParen)
let loopStatement = positions (keywordLoop >>. SW >>. rangeOrLoopBody) <?> "loop statement" |>> Ast.Loop
let rangeStatement = positions (keywordRange >>. SW >>. rangeOrLoopBody) <?> "range statement" |>> Ast.Range

//// Difference of assertion to an axiom: axiom's is followed by a signature of a predicate (i.e. with possible parameters),
//// not by a predicate (i.e. with possible arguments)
//// Difference of assertion to a mandatory property: a mandatory property introduces a completely new identifier inside
//// the scope of a definition. An assertion uses a predicate referring to existing identifiers in the whole theory
//// Difference of assertion to assume: the latter will be used only in the scope of proofs
let assertionStatement = positions (keywordAssert >>. SW >>. predicate) <?> "assert statement" |>> Ast.Assertion

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

predicateWithArgumentsRef.Value <- positions (fplIdentifier .>>. argumentTuple) <?> "predicate with arguments" |>> Ast.PredicateWithArgs

let qualifiedIdentifier = positions (fplIdentifier .>>. many1 (dot >>. predicateWithArguments)) <?> "qualified identifier" |>> Ast.QualifiedIdentifier

primePredicateRef.Value <- choice [
    keywordTrue
    keywordFalse
    keywordUndefined
    (attempt predicateWithArguments) 
    <|> (attempt qualifiedIdentifier) 
    <|> argumentIdentifier
    <|> fplIdentifier
    
]

let conjunction = positions ((keywordAnd >>. spacesLeftParenSpaces >>. predicateList) .>> spacesRightParenSpaces) <?> "conjunction, i.e. and ( predicate list )" |>> Ast.And
let disjunction = positions ((keywordOr >>. spacesLeftParenSpaces >>. predicateList) .>> spacesRightParenSpaces) <?> "disjunction, i.e. or ( predicate list )" |>> Ast.Or

let twoPredicatesInParens = (spacesLeftParenSpaces >>. predicate) .>>. (comma >>. predicate) .>> spacesRightParenSpaces 
let onePredicateInParens = (spacesLeftParenSpaces >>. predicate) .>> spacesRightParenSpaces
let implication = positions (keywordImpl >>. twoPredicatesInParens) <?> "implication, i.e. impl ( predicate , predicate )" |>> Ast.Impl
let equivalence = positions (keywordIif >>. twoPredicatesInParens) <?> "equivalence, i.e. iif ( predicate, predicate )"|>> Ast.Iif
let exclusiveOr = positions (keywordXor >>. twoPredicatesInParens) <?> "exclusive disjunction, i.e. xor ( predicate, predicate )"|>> Ast.Xor
let negation = positions (keywordNot >>. onePredicateInParens) <?> "negation, i.e. not ( predicate )"|>> Ast.Not
let all = positions ((keywordAll >>. SW >>. variableList) .>>. onePredicateInParens) <?> "all quantifier, e.g. all x,y,z ( predicate )" |>> Ast.All
let allAssert = positions ((keywordAll >>. SW >>. assigneeWithVariableRange) .>>. onePredicateInParens) <?> "all quantifier, e.g. all x,y,z ( predicate )"  |>> Ast.AllAssert
let exists = positions ((keywordEx >>. SW >>. variableList) .>>. onePredicateInParens) <?> "exists quantifier, e.g. exists x,y,z ( predicate )" |>> Ast.Exists
let existsTimesN = positions (((keywordEx >>. dollarDigits) .>>. (SW >>. variableList)) .>>. onePredicateInParens) <?> "exists n times quantifier, e.g. exists$3 x,y,z ( predicate )"|>> Ast.ExistsN
let isOperator = positions ((keywordIs >>. spacesLeftParenSpaces >>. coordInType) .>>. (comma >>. variableType) .>> spacesRightParenSpaces) <?> "is operator, e.g. is (x, NaturalNumber )"|>> Ast.IsOperator

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

predicateRef.Value <- ((compoundPredicate <|> primePredicate) .>> IW) <?> "<compound predicate> or <prime predicate>"

predicateListRef.Value <- sepBy predicate comma

(* FPL building blocks *)

let commentedStatement = many CW >>. statement

let commentedNamedVariableDeclaration = many CW >>. namedVariableDeclaration

let variableSpecification = (attempt commentedStatement) <|> (attempt commentedNamedVariableDeclaration)

let variableSpecificationList = positions (many variableSpecification) <?> "<variable specification list>" |>> Ast.VariableSpecification

(*To simplify the syntax definition, we do not define separate
FplPremiseConclusionBlocks for rules of inference and theorem-like blocks.
The first have a simplified, PL0 semantics, the latter have a more complex, predicative semantics.
However, there is a syntactical simplification of the signature*)
let commentedPredicate = many CW >>. predicate
let premise = many CW >>. (keywordPremise >>. colon >>. predicate) 
let conclusion = many CW >>. (keywordConclusion >>. colon >>. predicate) 
let premiseConclusionBlock = leftBraceCommented >>. variableSpecificationList .>>. premise .>>. conclusion .>> commentedRightBrace

(* FPL building blocks - rules of reference *)
let keywordInference = (skipString "inference" <|> skipString "inf") <?> "<'inference' or 'inf' keyword>"
let signatureWithPremiseConclusionBlock = (signature .>> IW) .>>. premiseConclusionBlock |>> Ast.SignatureWithPreConBlock
let ruleOfInference = positions signatureWithPremiseConclusionBlock <?> "<rule of inference (starting with a PascalCaseId)>" |>> Ast.RuleOfInference
let ruleOfInferenceList = many1 (many CW >>. ruleOfInference .>> IW) <?> "<list of rules of inference (starting with a PascalCaseId)>" 
let rulesOfInferenceBlock = (keywordInference >>. IW >>. leftBraceCommented >>. many CW >>. ruleOfInferenceList) .>> commentedRightBrace

(* FPL building blocks - Theorem-like statements and conjectures *)
let keywordTheorem = skipString "theorem" <|> skipString "thm" <?> "<'theorem' or 'thm' keyword>"
let keywordLemma = skipString "lemma" <|> skipString "lem" <?> "<'lemma' or 'lem' keyword>"
let keywordProposition = skipString "proposition" <|> skipString "prop" <?> "<'proposition' or 'prop' keyword>"
let keywordCorollary = skipString "corollary" <|> skipString "cor" <?> "<'corollary' or 'cor' keyword>"
let keywordConjecture = skipString "conjecture" <|> skipString "conj" <?> "<'conjecture' or 'conj' keyword>"

let theorem = positions (keywordTheorem >>. SW >>. signatureWithPremiseConclusionBlock) |>> Ast.Theorem
let lemma = positions (keywordLemma >>. SW >>. signatureWithPremiseConclusionBlock) |>> Ast.Lemma
let proposition = positions (keywordProposition >>. SW >>. signatureWithPremiseConclusionBlock) |>> Ast.Proposition
let conjecture = positions (keywordConjecture >>. SW >>. signatureWithPremiseConclusionBlock) |>> Ast.Conjecture

let dollarDigitList = many1 dollarDigits
let referencingIdentifier = predicateIdentifier .>>. dollarDigitList
let corollarySignature = (referencingIdentifier .>> IW) .>>. paramTuple
let corollary = positions (keywordCorollary >>. SW >>. (corollarySignature .>> IW) .>>. premiseConclusionBlock) |>> Ast.Corollary

(* FPL building blocks - Axioms *)

let keywordAxiom = (skipString "axiom" <|> skipString "ax" <|> skipString "postulate" <|> skipString "post")  <?> "<'axiom', 'ax', 'postulate', or 'post' keyword>"
let axiomBlock = leftBraceCommented >>. variableSpecificationList .>>. commentedPredicate .>> commentedRightBrace

let axiom = positions (keywordAxiom >>. SW >>. signature .>>. (IW >>. axiomBlock)) |>> Ast.Axiom

(* FPL building blocks - Constructors *)

let instanceBlock = leftBrace >>. many CW >>. variableSpecificationList .>> commentedRightBrace
let callConstructorParentClass = positions (opt predicateWithArguments) <?> "parent's class constructor call" |>> Ast.ClassConstructorCall
let constructorBlock = leftBraceCommented >>. variableSpecificationList .>>. callConstructorParentClass  .>> commentedRightBrace
let constructor = positions ((signature .>> IW) .>>. constructorBlock) <?> "<constructor, named after the class>" |>> Ast.Constructor

(* FPL building blocks - Properties *)
let keywordMandatory = positions (skipString "mandatory" <|> skipString "mand") <?> "<'mandatory' or 'mand' keyword>" >>% Ast.Mandatory
let keywordOptional = positions (skipString "optional" <|> skipString "opt") <?> "<'optional' or 'opt' keyword>" >>% Ast.Optional
let predicateInstanceBlock = (leftBraceCommented >>. variableSpecificationList) .>>. (commentedPredicate .>> commentedRightBrace)
let predicateInstance = positions ((keywordPredicate >>. SW >>. signature) .>>. (many CW >>. predicateInstanceBlock)) |>> Ast.PredicateInstance
let classInstance = positions ((variableType .>> SW) .>>. signature .>>. (many CW >>. instanceBlock)) |>> Ast.ClassInstance
let mapping = toArrow >>. IW >>. variableType
let functionalTermSignature = (keywordFunction >>. SW >>. signature) .>>. (IW >>. mapping)
let functionalTermInstance = positions (functionalTermSignature .>>. (many CW >>. instanceBlock)) |>> Ast.FunctionalTermInstance
let definitionProperty = choice [
    predicateInstance
    functionalTermInstance
    classInstance
]
let propertyHeader = (many CW >>. (keywordMandatory <|> keywordOptional)) 
let property = positions (propertyHeader .>>. (SW >>. definitionProperty)) |>> Ast.Property
let propertyList = many1 (many CW >>. property .>> IW)

(* FPL building blocks - Proofs 

    # A Proof relates to the PredicateIdentifier of the Theorem.
    # Because proofs are named, they can stand anywhere inside the theory, not only immediately
    # after the Theorem they prove. This is to enable the users to mix
    # with natural language an provide a proof long after the theorem was stated.

*)
// justifying proof arguments can be the identifiers of Rules of References, conjectures, theorem-like statements, or axioms
let keywordRevoke = (skipString "revoke" <|> skipString "rev") <?> "<'revoke' or 'rev' keyword>" 
let revokeArgument = positions (keywordRevoke >>. SW >>. argumentIdentifier) |>> Ast.RevokeArgument 
let premiseOfToBeProvedTheorem = positions keywordPremise <?> "<'premise' or 'prem' keyword>" |>> Ast.PremiseReference 
let conclusionOfToBeProvedTheorem = positions keywordConclusion <?> "<'conclusion' or 'con' keyword>" |>> Ast.ConclusionReference 
let premiseOrOtherPredicate = premiseOfToBeProvedTheorem <|> predicate
    
let keywordAssume = (skipString "assume" <|> skipString "ass") <?> "<'assume' or 'ass' keyword>"
let assumeArgument = positions (keywordAssume >>. SW >>. premiseOrOtherPredicate) |>> Ast.AssumeArgument
let keywordTrivial  = positions (skipString "trivial") <?> "'trivial'" <?> "<'trivial' keyword>" |>> Ast.Trivial
let keywordQed  = positions (skipString "qed") <?> "<'qed' keyword>" |>> Ast.Qed
let derivedPredicate = predicate |>> Ast.DerivedPredicate
let derivedArgument = choice [
    keywordQed 
    keywordTrivial 
    conclusionOfToBeProvedTheorem 
    derivedPredicate
]

let argumentInference = attempt revokeArgument <|> derivedArgument
let justification = positions (predicateList .>> IW) <?> "justification" |>> Ast.Justification
let justifiedArgument = positions ((justification .>> vDash .>> IW) .>>. argumentInference) <?> "justified argument" |>> Ast.JustifiedArgument
let argument = assumeArgument <|> justifiedArgument
let proofArgument = positions ((argumentIdentifier .>> IW) .>>. argument) .>> IW <?> "argument" |>> Ast.Argument
let proofArgumentList = many1 (many CW >>. proofArgument)
let keywordProof = (skipString "proof" <|> skipString "prf") .>> IW <?> "<'proof' or 'prf' keyword>"
let proofBlock = (leftBraceCommented >>. variableSpecificationList) .>>. (proofArgumentList .>> commentedRightBrace)
let proof = positions ((keywordProof >>. SW >>. referencingIdentifier) .>>. (IW >>. proofBlock)) |>> Ast.Proof

(* FPL building blocks - Definitions *)

// Predicate building blocks can be defined similarly to classes, they can have properties but they cannot be derived any parent type 
let predicateDefinitionBlock = (leftBraceCommented  >>. variableSpecificationList .>> many CW) .>>. (opt predicate .>> many CW) .>>. opt propertyList .>> commentedRightBrace 
let definitionPredicate = positions ((keywordPredicate >>. SW >>. signature .>> IW) .>>. predicateDefinitionBlock) |>> Ast.DefinitionPredicate

// Functional term building blocks can be defined similarly to classes, they can have properties but they cannot be derived any parent type 
let functionalTermDefinitionBlock = (leftBraceCommented  >>. variableSpecificationList .>> many CW) .>>. opt propertyList .>> commentedRightBrace
let definitionFunctionalTerm = positions ((functionalTermSignature .>> IW) .>>. functionalTermDefinitionBlock) |>> Ast.DefinitionFunctionalTerm

// Class definitions
let keywordClass = (skipString "class" <|> skipString "cl") <?> "<'class' or 'cl' keyword>"
let classDefinitionContent = choice [
    property
    constructor
]
let classDefinitionContentList = many (many CW >>. classDefinitionContent .>> IW)
let classDefinitionBlock = (leftBraceCommented  >>. variableSpecificationList .>> many CW) .>>. classDefinitionContentList .>> commentedRightBrace
let classSignature = (keywordClass >>. SW >>. predicateIdentifier .>> IW) .>>. (colon >>. IW >>. classType)
let definitionClass = positions ((classSignature .>> IW) .>>. classDefinitionBlock) |>> Ast.DefinitionClass 

let definition = choice [
    definitionClass
    definitionPredicate
    definitionFunctionalTerm
]
(* Gathering together all Building Blocks to a theory *)
let keywordTheory = (skipString "theory" <|> skipString "th") 
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
let fplNamespace = positions (namespaceIdentifier .>>. (many CW >>. namespaceBlock)) |>> Ast.Namespace
let fplNamespaceList = many1 (many CW >>. fplNamespace .>> IW)
(* Final Parser *)
let ast =  positions (fplNamespaceList .>> eof) <?> "fpl code" |>> Ast.AST
let fplParser (input: string) = tryParse ast "recovery failed;" ad input 
let parserDiagnostics = ad
