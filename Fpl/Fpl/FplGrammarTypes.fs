module FplGrammarTypes

type StatementKeyword =
    | Range
    | Loop
    | Else
    | Case
    | Cases
    | Assert 
    | Return 
    | Ret

type DefinitionKeyword =
    | Predicate 
    | Pred 
    | Function 
    | Func 
    | Object 
    | Obj 
    | Class 
    | Cl
    | Template 
    | Tpl 
    | Index 
    | Ind

type AxiomKeyword =
    | Axiom
    | Ax
    | Postulate
    | Post

type OtherKeyword = 
    | Localization 
    | Loc
    | Uses 
    | Alias 
    | Theory 
    | Th 
    | Premise 
    | Pre 
    | Conclusion 
    | Con 
    | End 
    | Ext
    
type PropertyKeyword = 
    | Mandatory 
    | Mand
    | Optional
    | Opt


type ProofKeyword = 
    | Proof
    | Prf 
    | Assume 
    | Ass 
    | Revoke 
    | Rev 
    | Qed 
    | Trivial

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
    | WildcaredNamespaceIdentifier of string list 
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
    | DelegateId of string 

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
    | Exists of FplIdentifier list * Predicate
    | ExistsN of (string * FplIdentifier list) * Predicate
    | IsOperator of FplIdentifier * FplType
    | Delegate of FplIdentifier * Predicate list

type Statement = 
    | Assertion of Predicate
    | ConditionFollowedByResult of Predicate * Statement list 
    | DefaultResult of Statement list 
    | Cases of Statement list * Statement
    | Assignment of FplIdentifier * Predicate
    | Loop of (FplIdentifier * FplIdentifier) * Statement list
    | Range of (FplIdentifier * FplIdentifier) * Statement list
    | Return of Predicate

type FplBlock = 
    | BlockStatement of Statement
    | BlockVariableDeclaration of (FplIdentifier list * FplType) 
    | StatementList of Statement list
    | RuleOfInference of FplBlock * ((FplBlock list * Predicate) * Predicate)
    | Theorem of FplBlock * ((FplBlock list * Predicate) * Predicate)
    | Lemma of FplBlock * ((FplBlock list * Predicate) * Predicate)
    | Proposition of FplBlock * ((FplBlock list * Predicate) * Predicate)
    | Corollary of ((FplIdentifier * string list) * (FplIdentifier list * FplType) list) * ((FplBlock list * Predicate) * Predicate)
    | Conjecture of FplBlock * ((FplBlock list * Predicate) * Predicate)
    | Signature of FplIdentifier * (FplIdentifier list * FplType) list
    | Axiom of FplBlock * (FplBlock list * Predicate)
    | ClassConstructorCall of Predicate option
    | Constructor of FplBlock * (FplBlock * FplBlock list)


