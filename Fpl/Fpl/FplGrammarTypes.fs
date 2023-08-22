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

type BlockHeader = 
    | Theorem
    | Lemma
    | Proposition
    | Corollary
    | Conjecture
    | Inference of string
    | Signature

type Extensions = 
    | Extensionname of string
    | ExtensionRegex of string

type ExtensionBlock = ExtensionBlock of Extensions * Extensions

type FplIdentifierType =
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
    | ClosedOrOpenRange of (FplIdentifierType * (FplIdentifierType option * FplIdentifierType option)) * FplIdentifierType
    | BrackedCoordList of FplIdentifierType list
    | AliasedId of string list
    | EntityWithCoord of FplIdentifierType * FplIdentifierType
    | Xid of Extensions
    | PredicateHeader 
    | FunctionalTermHeader 
    | Object 
    | Template of string
    | LongTemplate of string 
    | RangeInType of FplIdentifierType option * FplIdentifierType option 
    | SpecificTypeWithCoord of (FplIdentifierType * FplIdentifierType ) * (FplIdentifierType * FplIdentifierType)


type UsesClause = UsesClause of FplIdentifierType list

    
type Predicate =
    | True
    | False
    | Undefined
    | PredicateWithArgs of FplIdentifierType * Predicate list
    | And of Predicate list
    | Or of Predicate list
    | Impl of Predicate * Predicate
    | Iif of Predicate * Predicate
    | Xor of Predicate * Predicate
    | Not of Predicate 
    | All of FplIdentifierType list * Predicate
    | Exists of FplIdentifierType list * Predicate
    | ExistsN of (string * FplIdentifierType list) * Predicate
    | Is
    | ExtDigits of string



type PredicateKeyword =
    | Py
