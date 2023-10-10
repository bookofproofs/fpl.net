module FplGrammarTypes
open FParsec

/// our FPL grammar needs starting and ending position for each Ast node since we
/// will need these information for the diagnostics of the interpreter even after the 
/// parsing was done
type Positions = Position * Position 


type Ast = 
    // Identifiers
    | ExtDigits of Positions * string
    | DollarDigits of Positions * string
    | PascalCaseId of string
    | NamespaceIdentifier of Positions * Ast list
    | AliasedNamespaceIdentifier of Positions * (Ast * Ast option)
    | PredicateIdentifier of Positions * Ast list
    | DelegateId of Positions * string 
    | Alias of Positions * string
    | Self of Positions * char list 
    | LocalizationString of Positions * string
    | LocalizationTerm of Positions * Ast list
    | LocalizationTermList of Positions * Ast list
    | Extensionname of Positions * string
    | ExtensionRegex of string
    | ExtensionType of Positions * Ast 
    | ExtensionBlock of Positions * (Ast * Ast)
    | UsesClause of Positions * Ast list
    | LeftClosed
    | LeftOpen 
    | RightClosed
    | RightOpen 
    | Id of string
    | ClosedOrOpenRange of Positions * ((Ast * Ast option) * Ast)
    | BrackedCoordList of Positions * Ast list
    | RangeInType of Positions * (Ast option * Ast option) 
    // Types
    | Many 
    | Many1 
    | TemplateType of Positions * string
    | ObjectType 
    | ClassIdentifier of Positions * Ast
    | PredicateType 
    | FunctionalTermType 
    | IndexType
    | VariableType of Positions * Ast option
    | VariableTypeWithModifier of Positions * (Ast option * Ast)
    | BracketedCoordsInType of Positions * Ast list 
    | BoundedRangeInType of Positions * ((Ast * Ast) * Ast)
    | ClassType of Positions * (Ast * Ast option)
    // Variables
    | Var of Positions * string
    | IndexVariable of Positions * (string * string)
    // Predicates
    | True of Positions * unit
    | False of Positions * unit 
    | Undefined of Positions * unit
    | PredicateWithQualification of Positions * (Ast * Ast option) 
    | And of Positions * Ast list
    | Or of Positions * Ast list
    | Impl of Positions * (Ast * Ast)
    | Iif of Positions * (Ast * Ast)
    | Xor of Positions * (Ast * Ast)
    | Not of Positions * Ast
    | All of Positions * (Ast list * Ast)
    | AllAssert of Positions * ((Ast * Ast) * Ast)
    | Exists of Positions * (Ast list * Ast)
    | ExistsN of Positions * ((Ast * Ast list) * Ast)
    | IsOperator of Positions * (Ast * Ast)
    | Delegate of Positions * (Ast * Ast)
    | ArgumentIdentifier of Positions * string
    | PremiseReference of Positions * unit
    | Justification of Positions * Ast list
    | ArgumentTuple of Positions * Ast list
    // Statements
    | Assertion of Positions * Ast
    | ConditionFollowedByResult of Positions * (Ast * Ast list)
    | DefaultResult of Positions * Ast list 
    | Cases of Positions * (Ast list * Ast)
    | Assignment of Positions * (Ast * Ast)
    | Loop of Positions * ((Ast * Ast) * Ast list)
    | Range of Positions * ((Ast * Ast) * Ast list)
    | Return of Positions * Ast
    // FPL Blocks
    | StatementList of Positions * Ast list
    | SignatureWithPreConBlock of Ast * ((Ast * Ast) * Ast)
    | RuleOfInference of Positions * Ast
    | Theorem of Positions * Ast
    | Lemma of Positions * Ast
    | Proposition of Positions * Ast
    | Corollary of Positions * (((Ast * Ast list) * Ast) * ((Ast * Ast) * Ast))
    | Conjecture of Positions * Ast
    | NamedVarDecl of Positions * (Ast list * Ast) 
    | VariableSpecification of Positions * Ast list
    | ParamTuple of Positions * Ast list
    | Signature of Positions * (Ast * Ast)
    | Axiom of Positions * (Ast * (Ast * Ast))
    | ClassConstructorCall of Positions * Ast option
    | Constructor of Positions * (Ast * (Ast * Ast))
    | Mandatory
    | Optional 
    | PredicateInstance of Positions * (Ast * (Ast * Ast))
    | ClassInstance of Positions * ((Ast * Ast) * Ast)
    | FunctionalTermInstance of Positions * ((Ast * Ast) * Ast)
    | Property of Positions * (Ast * Ast)
    | DefinitionPredicate of Positions * (Ast * ((Ast * Ast option) * Ast list option))
    | DefinitionFunctionalTerm of Positions * ((Ast * Ast) * (Ast * Ast list option))
    | DefinitionClass of Positions * ((Ast * Ast) * (Ast * Ast list))
    // Proofs
    | Trivial of Positions * unit
    | Qed of Positions * unit
    | ConclusionReference of Positions * unit
    | DerivedPredicate of Ast
    | AssumeArgument of Positions * Ast
    | RevokeArgument of Positions * Ast
    | JustifiedArgument of Positions * (Ast * Ast)
    | Argument of Positions * (Ast * Ast)
    | Proof of Positions * ((Ast * Ast list) * (Ast * Ast list))
    | Namespace of Positions * (Ast * ((((Ast option * Ast option) * Ast list option) * Ast list) * (Ast * (string * Ast) list) list option))
    | AST of Positions * Ast
    | Escape // used to replace AST subnodes when we recover from an error
    | SomeString of string // used to replace AST for strings subnodes when we recover from an error
    | Error // used to replace the whole AST (at the root level) for severe errors the parser cannot recover from
    | Empty // used to mark empty inner inputs between enclosing ones 
    | Sequence of Positions * Ast list

