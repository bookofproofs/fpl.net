module FplGrammarTypes
open FParsec

/// our FPL grammar needs starting and ending position for each Ast node since we
/// will need these information for the diagnostics of the interpreter even after the 
/// parsing was done
type Positions = Position * Position 


type Ast = 
    // Literals
    | Star 
    | Dot
    | Index
    | LeftClosed
    | LeftOpen 
    | RightClosed
    | RightOpen 
    // Identifiers
    | Digits of string
    | ExtDigits of Positions * Ast
    | DollarDigits of Positions * string
    | PascalCaseId of string
    | NamespaceIdentifier of Positions * Ast list
    | AliasedNamespaceIdentifier of Positions * (Ast * Ast option)
    | PredicateIdentifier of Positions * Ast list 
    | DelegateId of Positions * string 
    | Alias of Positions * string
    | SelfAts of Positions * char list 
    | Self of Positions * unit
    | LocalizationString of Positions * string
    | LocalizationTerm of Positions * Ast list
    | LocalizationTermList of Positions * Ast list
    | Translation of string * Ast
    | Extensionname of Positions * string
    | ExtensionRegex of string
    | ExtensionType of Positions * Ast 
    | ExtensionBlock of Positions * (Ast * Ast)
    | UsesClause of Positions * Ast
    | ClosedOrOpenRange of Positions * ((Ast * Ast option) * Ast)
    | BrackedCoordList of Positions * Ast list
    | RangeInType of Positions * (Ast option * Ast option) 
    // Types
    | One 
    | Many 
    | Many1 
    | TemplateType of Positions * string
    | ObjectType 
    | ClassIdentifier of Positions * Ast
    | PredicateType 
    | FunctionalTermType 
    | IndexType
    | VariableType of Positions * (Ast * Ast option)
    | SimpleVariableType of Positions * Ast 
    | BracketedCoordsInType of Positions * Ast list 
    | BoundedRangeInType of Positions * ((Ast * Ast) * Ast)
    | ClassType of Positions * (Ast * Ast option)
    | ClassTypeWithModifier of Positions * (Ast * Ast)
    // Variables
    | Var of Positions * string

    // Predicates
    | True of Positions * unit
    | False of Positions * unit 
    | Undefined of Positions * unit
    | And of Positions * Ast list
    | Or of Positions * Ast list
    | Impl of Positions * (Ast * Ast)
    | Iif of Positions * (Ast * Ast)
    | Xor of Positions * (Ast * Ast)
    | Not of Positions * Ast
    | Domain of Positions * Ast
    | All of Positions * ((Ast list * Ast option) list * Ast)
    | Exists of Positions * ((Ast list * Ast option) list * Ast)
    | ExistsN of Positions * ((Ast * (Ast * Ast option)) * Ast)
    | IsOperator of Positions * (Ast * Ast)
    | Delegate of Positions * (Ast * Ast)
    | ArgumentIdentifier of Positions * string
    | ReferenceToCorollary of Positions * ((Ast * Ast list) * Ast) 
    | Justification of Positions * Ast list
    | ArgumentTuple of Positions * Ast list
    | EqualityComparison of Positions * Ast list
    | ByDef of Positions * Ast
    | PredicateWithOptSpecification of Positions * (Ast * Ast option)
    | DottedPredicate of Positions * Ast 
    | IndexedPredicate of Positions * Ast
    | QualificationList of Positions * Ast list
    | PredicateWithQualification of Ast * Ast
    // Statements
    | Assertion of Positions * Ast
    | ConditionFollowedByResult of Positions * (Ast * Ast list)
    | DefaultResult of Positions * Ast list 
    | Cases of Positions * (Ast list * Ast)
    | Assignment of Positions * (Ast * Ast)
    | ForIn of Positions * ((Ast * Ast) * Ast list)
    | Return of Positions * Ast
    // FPL Blocks
    | Intrinsic
    | VarDeclBlock of Positions * Ast list 
    | StatementList of Positions * Ast list
    | SignatureWithPreConBlock of Ast * ((Ast list option * Ast) * Ast)
    | RuleOfInference of Positions * Ast
    | Localization of Positions * (Ast * Ast list)
    | Theorem of Positions * (Ast *(Ast list option * Ast))
    | Lemma of Positions * (Ast *(Ast list option * Ast))
    | Proposition of Positions * (Ast *(Ast list option * Ast))
    | Corollary of Positions * (((Ast * Ast list) * Ast) * (Ast list option * Ast))
    | Conjecture of Positions * (Ast *(Ast list option * Ast))
    | NamedVarDecl of Positions * ((Ast list * Ast) * Ast) 
    | ParamTuple of Positions * Ast list
    | Signature of Positions * (Ast * Ast)
    | Axiom of Positions * (Ast * (Ast list option * Ast))
    | ParentConstructorCall of Positions * (Ast * Ast)
    | Constructor of Positions * (Ast * (Ast list option * Ast)) 
    | Property
    | Optional 
    | PredicateInstance of Positions * (Ast * Ast) 
    | ClassInstance of Positions * ((Ast * Ast) * Ast)
    | FunctionalTermInstance of Positions * ((Ast * Ast) * Ast)
    | PropertyBlock of Positions * ((Ast * Ast option) * Ast)
    | DefPredicateContent of Ast list option * Ast
    | DefinitionPredicate of Positions * (Ast * (Ast * Ast list option))
    | DefFunctionContent of Ast list option * Ast
    | DefinitionFunctionalTerm of Positions * ((Ast * Ast) * (Ast * Ast list option))
    | DefClassContent of Ast list option * Ast
    | DefClassCompleteContent of Ast list option * Ast list
    | DefinitionClass of Positions * ((Ast * Ast list) * (Ast * Ast list option))

    // Proofs
    | Trivial of Positions * unit
    | Qed of Positions * unit
    | DerivedPredicate of Ast
    | AssumeArgument of Positions * Ast
    | RevokeArgument of Positions * Ast
    | JustifiedArgument of Positions * (Ast * Ast)
    | Argument of Positions * (Ast * Ast)
    | Proof of Positions * ((Ast * Ast list) * (Ast list option * Ast list))
    | Namespace of Ast option * Ast list
    | AST of Positions * Ast
    | Error // used to replace the whole AST (at the root level) for severe errors the parser cannot recover from

