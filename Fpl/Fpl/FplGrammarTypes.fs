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

type FplBlock = 
    | Theorem
    | Lemma
    | Proposition
    | Corollary
    | Conjecture
    | Inference of string
    | Signature of FplIdentifier * (FplIdentifier list * FplType) list 

type UsesClause = UsesClause of FplIdentifier list



type PredicateKeyword =
    | Py

