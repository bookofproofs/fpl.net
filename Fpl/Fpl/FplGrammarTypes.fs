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

type Obj =
    | Object of string
    | Template of string
    | LongTemplate of string 

type Identifiers =
    | LeftClosed
    | LeftOpen 
    | RightClosed
    | RightOpen 
    | Id of string
    | NamespaceIdentifier of string list
    | WildcaredNamespaceIdentifier of string list 
    | AliasedNamespaceIdentifier of string list * string
    | IndexVariable of string * string

type UsesClause = UsesClause of Identifiers list

    
type Predicate =
    | True
    | False
    | Undefined
    | AliasedId of string list
    | Qualified of Identifiers * Identifiers list
    | PredicateWithArgs of Predicate * Predicate list
    | And of Predicate list
    | Or of Predicate list
    | Impl of Predicate * Predicate
    | Iif of Predicate * Predicate
    | Xor of Predicate * Predicate
    | Not of Predicate 
    | All of Predicate list * Predicate
    | Exists of Predicate list * Predicate
    | ExistsN of (string * Predicate list) * Predicate
    | Is
    | EntityWithCoord of Predicate * Predicate
    | ClosedOrOpenRange of (Identifiers * (Predicate option * Predicate option)) * Identifiers
    | BrackedCoordList of Predicate list
    | Self of char list 
    | Var of string

type PredicateKeyword =
    | Py
