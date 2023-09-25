module FplGrammarTypes

type Ast = 
    // Identifiers
    | Digits of string
    | ExtDigits of string
    | DollarDigits of string
    | PascalCaseId of string
    | NamespaceIdentifier of Ast list
    | AliasedNamespaceIdentifier of string list * Ast
    | PredicateIdentifier of Ast list
    | DelegateId of string 
    | Alias of string
    | Self of char list 
    | LocalizationString of string
    | LocalizationTerm of Ast list
    | LocalizationTermList of Ast list
    | EntityWithCoord of Ast * Ast
    | Extensionname of string
    | ExtensionRegex of string
    | ExtensionType of Ast 
    | ExtensionBlock of Ast * Ast
    | UsesClause of Ast list
    | LeftClosed
    | LeftOpen 
    | RightClosed
    | RightOpen 
    | Id of string
    | ClosedOrOpenRange of (Ast * (Ast option * Ast option)) * Ast
    | BrackedCoordList of Ast list
    | RangeInType of Ast option * Ast option 
    // Types
    | Many 
    | Many1 
    | TemplateType of string
    | ObjectType 
    | ClassHeaderType of Ast list
    | PredicateType 
    | FunctionalTermType 
    | IndexType
    | VariableType of (Ast list * Ast) list 
    | VariableTypeWithModifier of Ast option * Ast
    | FplTypeWithCoords of (Ast * Ast list) 
    | FplTypeWithRange of (Ast * Ast ) * (Ast * Ast)
    // Variables
    | Var of string
    | IndexVariable of string * string
    // Predicates
    | True
    | False
    | Undefined
    | PredicateWithArgs of Ast * Ast list
    | PredicateWithoutArgs of Ast
    | And of Ast list
    | Or of Ast list
    | Impl of Ast * Ast
    | Iif of Ast * Ast
    | Xor of Ast * Ast
    | Not of Ast 
    | All of Ast list * Ast
    | AllAssert of (Ast * Ast) * Ast 
    | Exists of Ast list * Ast
    | ExistsN of (Ast * Ast list) * Ast
    | IsOperator of Ast * Ast
    | Delegate of Ast * Ast list
    | QualifiedIdentifier of Ast * Ast list
    | ArgumentIdentifier of string
    | PremiseReference
    | Justification of Ast list
    // Statements
    | Assertion of Ast
    | ConditionFollowedByResult of Ast * Ast list 
    | DefaultResult of Ast list 
    | Cases of Ast list * Ast
    | Assignment of Ast * Ast
    | Loop of (Ast * Ast) * Ast list
    | Range of (Ast * Ast) * Ast list
    | Return of Ast
    // FPL Blocks
    | BlockStatement of Ast
    | BlockVariableDeclaration of (Ast list * Ast) 
    | StatementList of Ast list
    | RuleOfInference of Ast * ((Ast list * Ast) * Ast)
    | Theorem of Ast * ((Ast list * Ast) * Ast)
    | Lemma of Ast * ((Ast list * Ast) * Ast)
    | Proposition of Ast * ((Ast list * Ast) * Ast)
    | Corollary of ((Ast * Ast list) * (Ast list * Ast) list) * ((Ast list * Ast) * Ast)
    | Conjecture of Ast * ((Ast list * Ast) * Ast)
    | Signature of Ast * (Ast list * Ast) list
    | Axiom of Ast * (Ast list * Ast)
    | ClassConstructorCall of Ast option
    | Constructor of Ast * (Ast list * Ast)
    | Mandatory
    | Optional 
    | PredicateInstance of (Ast * (Ast list * Ast))
    | ClassInstance of (Ast * Ast) * Ast list 
    | FunctionalTermInstance of (Ast * Ast) * Ast list
    | Property of Ast * Ast
    | DefinitionPredicate of Ast * ((Ast list * Ast option) * Ast list option)
    | DefinitionFunctionalTerm of (Ast * Ast) * (Ast list * Ast list option)
    | DefinitionClass of (Ast * Ast) * (Ast list * Ast list)
    // Proofs
    | Trivial
    | Qed
    | ConclusionReference 
    | DerivedPredicate of Ast
    | AssumeArgument of Ast
    | RevokeArgument of Ast
    | JustifiedArgument of Ast * Ast
    | Argument of Ast * Ast
    | Proof of (Ast * Ast list) * (Ast list * Ast list)

    | AST of (Ast * ((((Ast option * Ast) * Ast list) * Ast list) * (Ast * (string * Ast) list) list)) list
    | Escape // used to replace AST subnodes when we recover from an error
    | Error // used to replace the whole AST (at the root level) for severe errors the parser cannot recover from
    | Empty // used to mark empty inner inputs between enclosing ones 
    | Sequence of Ast list
