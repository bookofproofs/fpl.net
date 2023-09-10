module FplGrammarTypes

type SyntaxNode =
    // literal types
    | LeftBrace of unit option
    | RightBrace of unit option
    | LeftParen of unit option
    | RightParen of unit option
    | LeftBracket of unit option
    | RightBracket of unit option
    | Comma of unit option
    | Star of unit option
    | Plus of unit option
    | Dot of unit option
    | Colon of unit option
    | ColonEqual of unit option
    | At of unit option
    | ExclamationMark of unit option
    | Case of unit option
    | Tilde of unit option
    | Semicolon of unit option
    | Dollar of unit option
    | Map of unit option
    | VDash of unit option
    // whitespace and comment types
    | BlockCommentStart of unit option
    | BlockCommentEnd of unit option
    | BlockComment of unit option
    | InlineCommentStart of unit option
    | InlineComment of unit option
    | SignificantWS of unit option
    | CW of SyntaxNode option
    | Error

type Extension = 
    | Extensionname of string
    | ExtensionRegex of string

type ExtensionBlock = ExtensionBlock of Extension * Extension

type FplIdentifier =
    | LeftClosed
    | LeftOpen 
    | RightClosed
    | RightOpen 
    | Id of string
    | NamespaceIdentifier of string list
    | AliasedNamespaceIdentifier of string list * string
    | IndexVariable of string * string
    | Var of string
    | Self of char list 
    | ClosedOrOpenRange of (FplIdentifier * (FplIdentifier option * FplIdentifier option)) * FplIdentifier
    | BrackedCoordList of FplIdentifier list
    | AliasedId of string list
    | EntityWithCoord of FplIdentifier * FplIdentifier
    | RangeInType of FplIdentifier option * FplIdentifier option 
    | ExtDigits of string
    | DollarDigits of string
    | DelegateId of string 
    | LocalizationString of string
    | LocalizationTerm of FplIdentifier list
    | LocalizationTermList of FplIdentifier list

type UsesClause = UsesClause of FplIdentifier list

type FplType =
    | Many 
    | Many1 
    | PredicateType 
    | FunctionalTermType 
    | ObjectType 
    | TemplateType of string
    | IndexType
    | ExtensionType of Extension
    | ClassHeaderType of string list 
    | FplTypeWithCoords of (FplType * FplIdentifier list) 
    | FplTypeWithRange of (FplType * FplIdentifier ) * (FplIdentifier * FplIdentifier)
    | VariableTypeWithModifier of FplType option * FplType
    | VariableType of (FplIdentifier list * FplType) list 

type Predicate =
    | True
    | False
    | Undefined
    | PredicateWithArgs of FplIdentifier * Predicate list
    | PredicateWithoutArgs of FplIdentifier
    | And of Predicate list
    | Or of Predicate list
    | Impl of Predicate * Predicate
    | Iif of Predicate * Predicate
    | Xor of Predicate * Predicate
    | Not of Predicate 
    | All of FplIdentifier list * Predicate
    | AllAssert of (FplIdentifier * FplIdentifier) * Predicate 
    | Exists of FplIdentifier list * Predicate
    | ExistsN of (FplIdentifier * FplIdentifier list) * Predicate
    | IsOperator of FplIdentifier * FplType
    | Delegate of FplIdentifier * Predicate list
    | QualifiedIdentifier of FplIdentifier * Predicate list
    | ArgumentIdentifier of string
    | PremiseReference
    | Justification of Predicate list


type Statement = 
    | Assertion of Predicate
    | ConditionFollowedByResult of Predicate * Statement list 
    | DefaultResult of Statement list 
    | Cases of Statement list * Statement
    | Assignment of FplIdentifier * Predicate
    | Loop of (FplIdentifier * FplIdentifier) * Statement list
    | Range of (FplIdentifier * FplIdentifier) * Statement list
    | Return of Predicate


type Proof =
    | Trivial
    | Qed
    | ConclusionReference 
    | DerivedPredicate of Predicate
    | AssumeArgument of Predicate
    | RevokeArgument of Predicate
    | JustifiedArgument of Predicate * Proof
    | Argument of Predicate * Proof

type FplBlock = 
    | BlockStatement of Statement
    | BlockVariableDeclaration of (FplIdentifier list * FplType) 
    | StatementList of Statement list
    | RuleOfInference of FplBlock * ((FplBlock list * Predicate) * Predicate)
    | Theorem of FplBlock * ((FplBlock list * Predicate) * Predicate)
    | Lemma of FplBlock * ((FplBlock list * Predicate) * Predicate)
    | Proposition of FplBlock * ((FplBlock list * Predicate) * Predicate)
    | Corollary of ((FplIdentifier * FplIdentifier list) * (FplIdentifier list * FplType) list) * ((FplBlock list * Predicate) * Predicate)
    | Conjecture of FplBlock * ((FplBlock list * Predicate) * Predicate)
    | Signature of FplIdentifier * (FplIdentifier list * FplType) list
    | Axiom of FplBlock * (FplBlock list * Predicate)
    | ClassConstructorCall of Predicate option
    | Constructor of FplBlock * (FplBlock list * FplBlock)
    | Mandatory
    | Optional 
    | PredicateInstance of (FplBlock * (FplBlock list * Predicate))
    | ClassInstance of (FplType * FplBlock) * FplBlock list 
    | FunctionalTermInstance of (FplBlock * FplType) * FplBlock list
    | Proof of (FplIdentifier * FplIdentifier list) * (FplBlock list * Proof list)
    | Property of FplBlock * FplBlock
    | DefinitionPredicate of FplBlock * ((FplBlock list * Predicate option) * FplBlock list option)
    | DefinitionFunctionalTerm of (FplBlock * FplType) * (FplBlock list * FplBlock list option)
    | DefinitionClass of (FplIdentifier * FplType) * (FplBlock list * FplBlock list)


 type FplParserResult = 
    | AST of (FplIdentifier * ((((ExtensionBlock option * UsesClause) * FplBlock list) * FplBlock list) * (Predicate * (string * FplIdentifier) list) list)) list

