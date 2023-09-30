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

let rightBrace: Parser<_, unit>= altTypoMsgI (skipString "}") "closing '}'" 
let leftBrace: Parser<_, unit> = altTypoMsgIPre (skipString "{") "opening '{'" >>. spaces
let leftParen: Parser<_, unit> = skipChar '(' >>. spaces
let rightParen: Parser<_, unit> = skipChar ')' 
let comma: Parser<_, unit> = skipChar ',' >>. spaces
let star: Parser<_, unit> = skipChar '*' >>% Ast.Many
let plus: Parser<_, unit> = skipChar '+' >>% Ast.Many1
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

// -----------------------------------------------------
// Extensions of the FPL language (have to be dynamic)! Lacking a pre-processor, we put the rules
// from the Proof of Concept of FPL code manually into the EBNF of the core FPL grammar.
// note that this has to be inserted into:
// the IsOperand choice
// the PredicateOrFunctionalTerm choice
let digits = regex @"\d+" 
let extDigits: Parser<_, unit> = positions (digits) <?> "digits" |>> Ast.ExtDigits

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
let idStartsWithCap = (regex @"[A-Z]\w*") <?> "PascalCaseId"
// error recovery for pascalCaseId
let idStartsWithCapAlternatives = [(regex @"[a-z0-9_@\-]\w*")]
let pascalCaseId = alternative idStartsWithCap "PascalCaseId" idStartsWithCapAlternatives ad <?> "PascalCaseId" |>> Ast.PascalCaseId 
let dollarDigits = positions (dollar >>. digits) <?> "'$' followed by digits" |>> Ast.DollarDigits
let argumentIdentifier = positions (regex @"\d+([a-z]\w)*\.") <?> "argument identifier" |>> Ast.ArgumentIdentifier

// error recovery for namespaceIdentifier with escape parsers [spaces; leftBrace; eof]
let namespaceIdentifier = positions (sequenceDiagnostics1 pascalCaseId dot [spaces; leftBrace; eof] ad "expected PascalCaseId") <?> "namespace identifier" |>> Ast.NamespaceIdentifier
let optionalnamespaceIdentifier = positions (sequenceDiagnostics pascalCaseId dot [spaces; leftBrace; eof] ad "expected PascalCaseId") <?> "namespace identifier" |>> Ast.NamespaceIdentifier
// error recovery for predicateIdentifier with escape parsers [spaces; leftParen; eof]
let predicateIdentifier = positions (sequenceDiagnostics pascalCaseId dot [spaces; leftParen; eof] ad "expected PascalCaseId") <?> "user-defined identifier" |>> Ast.PredicateIdentifier 
// error recovery for classIdentifier with escape parsers [spaces; leftBracket; eof]
let classIdentifier= positions (sequenceDiagnostics1 pascalCaseId dot [spaces; leftBracket; eof] ad "expected PascalCaseId") <?> "class identifier"|>> Ast.ClassHeaderType

// error recover for alias
let aliasKeyword = keywordAlternative "alias" " or closing '}'"// handle typos in alias
let skipAlias = aliasKeyword 
let aliasAlternativesOther = combineWithParserList (skipAlias .>> IW) (>>.) idStartsWithCapAlternatives 
let aliasAlternatives = aliasAlternativesOther @ [(pstring "alias" .>> IW)] 
let alias = positions (alternative (skipAlias >>. SW >>. idStartsWithCap) "PascalCaseId" aliasAlternatives ad) <?> "alias for namespace"|>> Ast.Alias

// error recover for aliasedNamespaceIdentifier
let aliasedNamespaceIdentifier = positions ((namespaceIdentifier .>> SW) .>>. alias) <?> "aliased namespace identifier"|>> Ast.AliasedNamespaceIdentifier
let tplRegex = Regex(@"^(tpl|template)(([A-Z]\w*)|\d*)$", RegexOptions.Compiled)
let variableX: Parser<string,unit> = IdStartsWithSmallCase >>= 
                                        ( fun s -> 
                                            if keyWordSet.Contains(s) then fail ("Cannot use keyword '" + s + "' as a variable") 
                                            else if tplRegex.IsMatch(s) then fail ("Cannot use template '" + s + "' as a variable") 
                                            else (preturn s)
                                        ) <?> "<variable>"
let variable = positions variableX <?> "variable" |>> Ast.Var 

let variableList = sepEndBy1 variable comma

let keywordSelf: Parser<_,unit> = skipString "self"
let keywordIndex: Parser<_,unit> = skipString "index" <|> skipString "ind" >>% Ast.IndexType


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
let keywordUndefined: Parser<_,unit> = positions (skipString "undefined" <|> skipString "undef") <?> "'undefined' or 'undef'" |>> Ast.Undefined
let keywordTrue: Parser<_,unit> = positions (skipString "true") <?> "'true'" |>> Ast.True  
let keywordFalse: Parser<_,unit> = positions (skipString "false") <?> "'false'" |>> Ast.False  
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
let keywordTemplate: Parser<_,unit> = positions (pstring "template" <|> pstring "tpl") <?> "'template' or 'tpl'" |>> Ast.TemplateType

let templateTail = choice [ idStartsWithCap; digits ]

let templateWithTail = positions (many1Strings2 (pstring "template" <|> pstring "tpl") templateTail) <?> "'template' or 'tpl' followed by digits or a PascalCaseId" |>>  Ast.TemplateType

let keywordObject: Parser<_,unit> = skipString "object" <|> skipString "obj" >>% Ast.ObjectType 

let objectHeader = choice [
    keywordObject
    (attempt templateWithTail) <|> keywordTemplate
] 

let keywordPredicate: Parser<_,unit> = skipString "predicate" <|> skipString "pred" >>% Ast.PredicateType
let keywordFunction: Parser<_,unit> = skipString "function" <|> skipString "func" >>% Ast.FunctionalTermType


let theoryNamespace = (attempt aliasedNamespaceIdentifier <|> namespaceIdentifier) .>> IW

// error recovery theoryNamespaceList
let theoryNamespaceList = sepBy1 theoryNamespace comma
let theoryNamespaceListMod = sequenceDiagnostics1 theoryNamespace comma [rightBrace] ad "namespace or aliased namespace" 

let usesClause = positions (keywordAlternative "uses" "" >>. IW >>. leftBrace >>. IW >>. theoryNamespaceListMod .>> IW .>> rightBrace) <?> "uses clause" |>> Ast.UsesClause

let extensionTail: Parser<unit,unit> = skipString ":end" >>. SW

let extensionHeader: Parser<unit,unit> = skipString ":ext" 

let extensionName = positions (skipString "ext" >>. idStartsWithCap .>> IW) <?> "'ext' followed by PascalCaseId"|>> Ast.Extensionname

let extensionRegex: Parser<_, unit>  = skipChar ':' >>. IW >>. regex @"\/(?!:end).*" .>> IW |>> Ast.ExtensionRegex

let extensionBlock = positions (extensionHeader >>. IW >>. extensionName .>>. extensionRegex .>> extensionTail) <?> "extension block" |>> Ast.ExtensionBlock


(* Signatures, Variable Declarations, and Types, Ranges and Coordinates *)
// convention: All syntax production rules of FPL syntax extensions have to start with "ext", followed by
// a Pascal Case id.
// This ensures that they will not be mixed-up with original FPL ebnf productions
// that are all PascalCase as well as FPL keywords, that are all small case.

let xId = positions (at >>. extensionName) <?> "'@ext' followed by PascalCaseId" |>> Ast.ExtensionType 

let indexVariable = positions ((IdStartsWithSmallCase .>> dollar) .>>. ( digits <|> IdStartsWithSmallCase )) <?> "index variable" |>> Ast.IndexVariable

let atList = many at

let self = positions (atList .>> keywordSelf) <?> "@...@self" |>> Ast.Self

let entity = (attempt (attempt self <|> indexVariable)) <|> variable

let leftOpen = positions (leftBracket >>. IW >>. exclamationMark) <?> "opening [!" >>% Ast.LeftOpen
let leftClosed = positions (leftBracket >>. IW) <?> "opening [" >>% Ast.LeftClosed

let leftBound = ((attempt leftOpen) <|> leftClosed)

let rightBound = choice [
    positions (exclamationMark >>. IW >>. rightBracket) <?> "closing !]" >>% Ast.RightOpen
    positions (IW >>. rightBracket) <?> "closing ]" >>% Ast.RightClosed
]
 


////// resolving recursive parsers
let statementList, statementListRef = createParserForwardedToRef()
let primePredicate, primePredicateRef = createParserForwardedToRef()
let coordOfEntity, coordOfEntityRef = createParserForwardedToRef()
let predicate, predicateRef = createParserForwardedToRef()
let predicateList, predicateListRef = createParserForwardedToRef()
let predicateWithArguments, predicateWithArgumentsRef = createParserForwardedToRef()
let paramTuple, paramTupleRef = createParserForwardedToRef()

let entityWithCoord = positions (entity .>>. coordOfEntity) <?> "entity with some coordinates" |>> Ast.EntityWithCoord

let assignee = (attempt entityWithCoord) <|> entity

let coord = choice [
    assignee
    extDigits
    dollarDigits
]

let fplDelegateIdentifier: Parser<_, unit> = positions (keywordDel >>. dot >>. regex @"[a-z_A-Z]\w+") <?> "delegate identifier, i.e. 'delegate' or 'del' followed by '.' and some identifier" |>> Ast.DelegateId

let fplIdentifier = choice [
    fplDelegateIdentifier
    coord
    predicateIdentifier
]

let coordList = sepEndBy1 coord comma

let bracketedCoordList = positions ((leftBracket >>. IW >>. coordList) .>> (IW >>. rightBracket)) <?> "bracketed list of coordinates" |>> Ast.BrackedCoordList

let fplRange = ((opt coord) .>> IW .>> tilde) .>>. (IW >>. opt coord)

let closedOrOpenRange = positions ((leftBound .>> IW) .>>. fplRange .>>. (IW >>. rightBound)) <?> "bracketed range" |>> Ast.ClosedOrOpenRange

coordOfEntityRef.Value <- attempt closedOrOpenRange <|> bracketedCoordList

let coordInType = choice [
    fplIdentifier
    indexVariable
]

let coordInTypeList = sepBy1 coordInType comma

let rangeInType = positions ((opt coordInType .>> IW) .>>. (tilde >>. IW >>. opt coordInType)) <?> "bracketed range of types" |>> Ast.RangeInType

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

let classTypeWithCoord = positions (((specificClassType .>> IW) .>> leftBracket) .>>. (coordInTypeList .>> (IW >>. rightBracket))) <?> "type with coordinates" |>> Ast.FplTypeWithCoords
let classTypeWithRange = positions (((specificClassType .>> IW) .>>. leftBound) .>>. (rangeInType .>>. rightBound)) <?> "type with range" |>> Ast.FplTypeWithRange

// The classType is the last type in FPL we can derive FPL classes from.
// It therefore excludes the in-built FPL-types keywordPredicate, keywordFunction, and keywordIndex
// to restrict it to pure objects.
// In contrast to variableType which can also be used for declaring variables 
// in the scope of FPL building blocks
let classType = (((attempt classTypeWithRange) <|> (attempt classTypeWithCoord)) <|> specificClassType)

let modifieableClassType = positions (opt callModifier .>>. classType) <?> "optional '+' or '*' followed by a class type" |>> Ast.VariableTypeWithModifier
let modifieablePredicateType = positions (opt callModifier .>>. keywordPredicate) <?> "optional '+' or '*' followed by 'predicate' or 'pred'" |>> Ast.VariableTypeWithModifier
let modifieableFunctionType = positions (opt callModifier .>>. keywordFunction) <?> "optional '+' or '*' followed by 'function' or 'func'" |>> Ast.VariableTypeWithModifier
let modifieableIndexType = positions (opt callModifier .>>. keywordIndex) <?> "optional '+' or '*' followed by 'index' or 'ind'" |>> Ast.VariableTypeWithModifier

let variableTypeWithModifier = (((attempt modifieableIndexType) <|> attempt modifieableFunctionType) <|> attempt modifieablePredicateType) <|> modifieableClassType

let parenthesisedType = positions (variableTypeWithModifier .>> IW >>. paramTuple) <?> "variable type" |>> Ast.VariableType

let variableType = ((attempt parenthesisedType) <|> attempt variableTypeWithModifier) <|> classType

let namedVariableDeclaration = positions ((variableList .>> IW) .>>. ((colon >>. IW) >>. variableType)) <?> "named variable declaration"|>> Ast.NamedVarDecl
let namedVariableDeclarationList = sepEndBy namedVariableDeclaration comma

paramTupleRef.Value <- positions ((leftParen >>. IW >>. namedVariableDeclarationList) .>> (IW .>> rightParen)) <?> "tuple of parameters" |>> Ast.ParamTuple

let signature = positions ((predicateIdentifier .>> IW) .>>. paramTuple) <?> "PascalCaseId followed by a tuple of parameters" |>> Ast.Signature

(* Statements *)
let argumentTuple = (spacesLeftParenSpaces >>. predicateList) .>> (IW .>> spacesRightParenSpaces)  

let fplDelegate = positions (fplDelegateIdentifier .>>. argumentTuple) <?> "delegate identifier followed by a tuple of arguments" |>> Ast.Delegate
let assignmentStatement = positions ((assignee .>> IW .>> colonEqual) .>>. (IW >>. predicate)) <?> "assignment statement"|>> Ast.Assignment
let returnStatement = positions (keywordReturn >>. SW >>. predicate) <?> "return statement" |>> Ast.Return

let variableRange = choice [
    closedOrOpenRange
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
    ]) <?> "<statement>"

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

predicateRef.Value <- ((compoundPredicate <|> primePredicate) .>> IW) <?> "predicate"

predicateListRef.Value <- sepBy predicate comma

(* FPL building blocks *)

let commentedStatement = many CW >>. statement

let commentedNamedVariableDeclaration = many CW >>. namedVariableDeclaration

let variableSpecification = (attempt commentedStatement) <|> (attempt commentedNamedVariableDeclaration)

let variableSpecificationList = positions (many variableSpecification) <?> "variable specification" |>> Ast.VariableSpecification

(*To simplify the syntax definition, we do not define separate
FplPremiseConclusionBlocks for rules of inference and theorem-like blocks.
The first have a simplified, PL0 semantics, the latter have a more complex, predicative semantics.
However, there is a syntactical simplification of the signature*)
let commentedPredicate = many CW >>. predicate
let premise = many CW >>. (keywordPremise >>. IW >>. colon >>. commentedPredicate) 
let conclusion = many CW >>. (keywordConclusion >>. IW >>. colon >>. commentedPredicate) 
let premiseConclusionBlock = leftBraceCommented >>. variableSpecificationList .>>. (premise .>> many CW) .>>. conclusion .>> commentedRightBrace

(* FPL building blocks - rules of reference *)
// error recovery inference block
let keywordInference = keywordAlternativesI "inference" "inf" 
let signatureWithPremiseConclusionBlock = (signature .>> IW) .>>. premiseConclusionBlock |>> Ast.SignatureWithPreConBlock
let ruleOfInference = positions signatureWithPremiseConclusionBlock <?> "rule of inference" |>> Ast.RuleOfInference
//let ruleOfInferenceList = many1 (many CW >>. ruleOfInference .>> IW)
let ruleOfInferenceListMod = sequenceDiagnostics1 (many CW >>. ruleOfInference .>> IW) spaces1 [rightBrace] ad "expecting rule of inference (start with a PascalCaseId followed by a tuple of parameters)" 
let rulesOfInferenceBlock = (keywordInference >>. IW >>. leftBraceCommented >>. many CW >>. ruleOfInferenceListMod) .>> commentedRightBrace

(* FPL building blocks - Theorem-like statements and conjectures *)
let keywordTheorem: Parser<_,unit> = skipString "theorem" <|> skipString "thm" 
let keywordLemma: Parser<_,unit> = skipString "lemma" <|> skipString "lem" 
let keywordProposition: Parser<_,unit> = skipString "proposition" <|> skipString "prop" 
let keywordCorollary: Parser<_,unit> = skipString "corollary" <|> skipString "cor" 
let keywordConjecture: Parser<_,unit> = skipString "conjecture" <|> skipString "conj" 

let theorem = positions (keywordTheorem >>. SW >>. signatureWithPremiseConclusionBlock) <?> "theorem" |>> Ast.Theorem
let lemma = positions (keywordLemma >>. SW >>. signatureWithPremiseConclusionBlock) <?> "lemma" |>> Ast.Lemma
let proposition = positions (keywordProposition >>. SW >>. signatureWithPremiseConclusionBlock) <?> "proposition" |>> Ast.Proposition
let conjecture = positions (keywordConjecture >>. SW >>. signatureWithPremiseConclusionBlock) <?> "conjecture" |>> Ast.Conjecture

let dollarDigitList = many1 dollarDigits
let referencingIdentifier = predicateIdentifier .>>. dollarDigitList
let corollarySignature = (referencingIdentifier .>> IW) .>>. paramTuple
let corollary = positions (keywordCorollary >>. SW >>. (corollarySignature .>> IW) .>>. premiseConclusionBlock) <?> "corollary" |>> Ast.Corollary

(* FPL building blocks - Axioms *)

let keywordAxiom: Parser<_,unit> = (skipString "axiom" <|> skipString "ax") <|> (skipString "postulate" <|> skipString "post") 
let axiomBlock = leftBraceCommented >>. variableSpecificationList .>>. commentedPredicate .>> commentedRightBrace

let axiom = positions (keywordAxiom >>. SW >>. signature .>>. (IW >>. axiomBlock)) <?> "axiom" |>> Ast.Axiom

(* FPL building blocks - Constructors *)

let instanceBlock = leftBrace >>. many CW >>. variableSpecificationList .>> commentedRightBrace
let callConstructorParentClass = positions (opt predicateWithArguments) <?> "parent's class constructor call" |>> Ast.ClassConstructorCall
let constructorBlock = leftBraceCommented >>. variableSpecificationList .>>. callConstructorParentClass  .>> commentedRightBrace
let constructor = positions ((signature .>> IW) .>>. constructorBlock) <?> "constructor" |>> Ast.Constructor

(* FPL building blocks - Properties *)
let keywordMandatory: Parser<_,unit> = positions (skipString "mandatory" <|> skipString "mand") <?> "'mandatory' or 'mand'" >>% Ast.Mandatory
let keywordOptional: Parser<_,unit> = positions (skipString "optional" <|> skipString "opt") <?> "'optional' or 'opt'" >>% Ast.Optional
let predicateInstanceBlock = (leftBraceCommented >>. variableSpecificationList) .>>. (commentedPredicate .>> commentedRightBrace)
let predicateInstance = positions ((keywordPredicate >>. SW >>. signature) .>>. (many CW >>. predicateInstanceBlock)) <?> "instance predicate " |>> Ast.PredicateInstance
let classInstance = positions ((variableType .>> SW) .>>. signature .>>. (many CW >>. instanceBlock)) <?> "instance class" |>> Ast.ClassInstance
let mapping = toArrow >>. IW >>. variableType
let functionalTermSignature = (keywordFunction >>. SW >>. signature) .>>. (IW >>. mapping)
let functionalTermInstance = positions (functionalTermSignature .>>. (many CW >>. instanceBlock)) <?> "instance functional term" |>> Ast.FunctionalTermInstance
let definitionProperty = choice [
    predicateInstance
    functionalTermInstance
    classInstance
]
let propertyHeader = (many CW >>. (keywordMandatory <|> keywordOptional)) 
let property = positions (propertyHeader .>>. (SW >>. definitionProperty)) <?> "property" |>> Ast.Property
let propertyList = many1 (many CW >>. property .>> IW)

(* FPL building blocks - Proofs 

    # A Proof relates to the PredicateIdentifier of the Theorem.
    # Because proofs are named, they can stand anywhere inside the theory, not only immediately
    # after the Theorem they prove. This is to enable the users to mix
    # with natural language an provide a proof long after the theorem was stated.

*)
// justifying proof arguments can be the identifiers of Rules of References, conjectures, theorem-like statements, or axioms
let keywordRevoke: Parser<_,unit> = (skipString "revoke" <|> skipString "rev") >>. SW
let revokeArgument = positions (keywordRevoke >>. argumentIdentifier) <?> "'revoke' or 'rev', followed by argument identifier" |>> Ast.RevokeArgument 
let premiseOfToBeProvedTheorem = positions keywordPremise <?> "'premise' or 'prem'" |>> Ast.PremiseReference 
let conclusionOfToBeProvedTheorem = positions keywordConclusion <?> "'conclusion' or 'con'" |>> Ast.ConclusionReference 
let premiseOrOtherPredicate = premiseOfToBeProvedTheorem <|> predicate
    
let keywordAssume: Parser<_,unit> = (skipString "assume" <|> skipString "ass") 
let assumeArgument = positions (keywordAssume >>. SW >>. premiseOrOtherPredicate) <?> "'assume' or 'ass', followed by 'premise' or other predicate" |>> Ast.AssumeArgument
let keywordTrivial: Parser<_,unit>  = positions (skipString "trivial") <?> "'trivial'" |>> Ast.Trivial
let keywordQed: Parser<_,unit>  = positions (skipString "qed") <?> "'qed'" |>> Ast.Qed
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
let proofArgument = positions ((argumentIdentifier .>> IW) .>>. argument) <?> "argument" |>> Ast.Argument
let proofArgumentList = many1 (many CW >>. proofArgument .>> IW)
let keywordProof: Parser<_,unit> = (skipString "proof" <|> skipString "prf")
let proofBlock = (leftBraceCommented >>. variableSpecificationList) .>>. (proofArgumentList .>> commentedRightBrace)
let proof = positions ((keywordProof >>. SW >>. referencingIdentifier) .>>. (IW >>. proofBlock)) <?> "proof" |>> Ast.Proof

(* FPL building blocks - Definitions *)

// Predicate building blocks can be defined similarly to classes, they can have properties but they cannot be derived any parent type 
let predicateDefinitionBlock = (leftBraceCommented  >>. variableSpecificationList .>> many CW) .>>. (opt predicate .>> many CW) .>>. opt propertyList .>> commentedRightBrace 
let definitionPredicate = positions ((keywordPredicate >>. SW >>. signature .>> IW) .>>. predicateDefinitionBlock) <?> "predicate definition" |>> Ast.DefinitionPredicate

// Functional term building blocks can be defined similarly to classes, they can have properties but they cannot be derived any parent type 
let functionalTermDefinitionBlock = (leftBraceCommented  >>. variableSpecificationList .>> many CW) .>>. opt propertyList .>> commentedRightBrace
let definitionFunctionalTerm = positions ((functionalTermSignature .>> IW) .>>. functionalTermDefinitionBlock) <?> "functional term definition" |>> Ast.DefinitionFunctionalTerm

// Class definitions
let keywordClass: Parser<_,unit> = (skipString "class" <|> skipString "cl")
let classDefinitionContent = choice [
    property
    constructor
]
let classDefinitionContentList = many (many CW >>. classDefinitionContent .>> IW)
let classDefinitionBlock = (leftBraceCommented  >>. variableSpecificationList .>> many CW) .>>. classDefinitionContentList .>> commentedRightBrace
let classSignature = (keywordClass >>. SW >>. predicateIdentifier .>> IW) .>>. (colon >>. IW >>. classType)
let definitionClass = positions ((classSignature .>> IW) .>>. classDefinitionBlock) <?> "class definition" |>> Ast.DefinitionClass 

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
let localizationString = positions (regex "\"[^\"\n]*\"") <?> "inline string" |>> Ast.LocalizationString

let ebnfTransl, ebnfTranslRef = createParserForwardedToRef()
let ebnfTranslTuple = (leftParen >>. IW >>. ebnfTransl) .>> (IW .>> rightParen) 
let ebnfFactor = choice [
    variable
    localizationString
    ebnfTranslTuple
] 
let ebnfTerm = positions (sepEndBy1 ebnfFactor SW) <?> "localization term" |>> Ast.LocalizationTerm
ebnfTranslRef.Value <-  positions (sepBy ebnfTerm (IW >>. case >>. IW)) <?> "list of localization terms" |>> Ast.LocalizationTermList
let translation = (tilde >>. localizationLanguageCode .>> IW .>> colon .>> IW) .>>. ebnfTransl
let translationList = many (many CW >>. translation .>> IW)
let localization = (predicate .>> IW .>> colonEqual .>> IW) .>>. (translationList .>> IW .>> semiColon)
let localizationList = many (many CW >>. localization .>> IW)
let localizationBlock = keywordLocalization >>. IW >>. leftBraceCommented >>. localizationList .>> commentedRightBrace


(* Namespaces *)
let namespaceBlock = (leftBraceCommented >>. opt extensionBlock) .>>. opt (many CW >>. usesClause) .>>. opt (many CW >>. rulesOfInferenceBlock) .>>. (many CW >>. theoryBlock) .>>. opt (many CW >>. localizationBlock) .>> commentedRightBrace
let fplNamespace = positions (optionalnamespaceIdentifier .>>. (many CW >>. namespaceBlock)) |>> Ast.Namespace
let fplNamespaceList = many1 (many CW >>. fplNamespace .>> IW)
(* Final Parser *)
let ast =  positions (fplNamespaceList .>> eof) <?> "fpl code" |>> Ast.AST
let fplParser (input: string) = tryParse ast "recovery failed;" ad input 
let parserDiagnostics = ad
