module FplGrammarTypes
open System.Collections.Generic
open FParsec



/// our FPL grammar needs starting and ending position for each Ast node since we
/// will need these information for the diagnostics of the interpreter even after the 
/// parsing was done
type Positions = Position * Position 

type Token =
    { Name:string
      StartPos: Position
      EndPos:Position
    }

type Tokenizer() =
    let _parsedTokens = new System.Collections.Generic.List<Token>(); 
    
    /// Increases the context of this tokenizer
    member this.Push(token:Token) = 
        _parsedTokens.Add(token)

    /// The list of successfully parsed tokens
    member this.ParsedTokens = _parsedTokens



type Ast = 
    // Literals
    | Star of Positions * unit
    | Dot of Positions * unit
    // Identifiers
    | Digits of string
    | Extension of Positions * string
    | DollarDigits of Positions * uint
    | PascalCaseId of string
    | NamespaceIdentifier of Positions * Ast list
    | AliasedNamespaceIdentifier of Positions * (Ast * Ast option)
    | PredicateIdentifier of Positions * Ast list 
    | DelegateId of Positions * string 
    | LanguageCode of Positions * string
    | Alias of Positions * string
    | SelfOrParent of Positions * Ast
    | Self of Positions * unit
    | Parent of Positions * unit
    | LocalizationString of Positions * string
    | LocalizationTerm of Positions * Ast list
    | LocalizationTermList of Positions * Ast list
    | Translation of Positions * (Ast * Ast)
    | ExtensionName of Positions * string
    | ExtensionRegex of string
    | ExtensionType of Positions * Ast 
    | ExtensionAssignment of Positions * (Ast * Ast) 
    | ExtensionSignature of Positions * (Ast * Ast)
    | DefinitionExtension of Positions * ((Ast * Ast) * Ast)
    | UsesClause of Positions * Ast
    | BrackedCoordList of Positions * Ast list
    | ReferencingIdentifier of Positions * (Ast * Ast list)
    // Types
    | One of Positions * unit
    | Many of Positions * unit
    | Many1 of Positions * unit  
    | TemplateType of Positions * string
    | ObjectType of Positions * unit
    | ClassIdentifier of Positions * Ast
    | PredicateType of Positions * unit
    | FunctionalTermType of Positions * unit
    | IndexType of Positions * unit
    | VariableType of Positions * Ast 
    | BracketedCoordsInType of Positions * Ast list 
    | InheritedClassType of Positions * Ast
    | ClassType of Positions * (Ast * Ast option)
    | CompoundPredicateType of Positions * (Ast * Ast option)
    | CompoundFunctionalTermType of Positions * (Ast * (Ast * Ast) option)
    // Variables
    | Var of Positions * string

    // Predicates
    | True of Positions * unit
    | False of Positions * unit 
    | Undefined of Positions * unit
    | And of Positions * (Ast * Ast)
    | Or of Positions * (Ast * Ast)
    | Xor of Positions * (Ast * Ast)
    | Impl of Positions * (Ast * Ast)
    | Iif of Positions * (Ast * Ast)
    | Not of Positions * Ast
    | InEntity of Positions * Ast
    | All of Positions * (Ast list * Ast)
    | Exists of Positions * (Ast list * Ast) 
    | ExistsN of Positions * ((Ast * Ast) * Ast)
    | IsOperator of Positions * (Ast * Ast)
    | Delegate of Positions * (Ast * Ast)
    | ArgumentIdentifier of Positions * string
    | ReferenceToProofOrCorollary of Positions * (Ast * Ast option) 
    | Justification of Positions * Ast list
    | ArgumentTuple of Positions * Ast list
    | ByDef of Positions * Ast
    | PredicateWithOptSpecification of Positions * (Ast * Ast option)
    | DottedPredicate of Positions * Ast 
    | QualificationList of Positions * Ast list
    | PredicateWithQualification of (Ast * Ast) 
    | ObjectSymbol of Positions * string
    | Prefix of Positions * string
    | Precedence of Positions * int
    | Infix of Positions * (string * Ast)
    | Postfix of Positions * string
    | Symbol of Positions * string
    | InfixOperator of Positions * string
    | PostfixOperator of Positions * string
    | PrefixOperator of Positions * string
    | InfixOperation of Positions * (Ast * Ast option) list
    | Expression of Positions * ((((Ast option * Ast) * Ast option) * Ast option) * Ast)
    // Statements
    | Assertion of Positions * Ast
    | ConditionFollowedByResult of Positions * (Ast * Ast list)
    | DefaultResult of Positions * Ast list 
    | Cases of Positions * (Ast list * Ast)
    | Assignment of Positions * (Ast * Ast)
    | ForIn of Positions * ((Ast * Ast) * Ast list)
    | Return of Positions * Ast
    // FPL Blocks
    | Intrinsic of Positions * unit
    | VarDeclBlock of Positions * Ast list 
    | StatementList of Positions * Ast list
    | PremiseConclusionBlock of Positions * ((Ast list option * Ast) * Ast)
    | RuleOfInference of Positions * (Ast * Ast)
    | Localization of Positions * (Ast * Ast list)
    | Theorem of Positions * (Ast *(Ast list option * Ast))
    | Lemma of Positions * (Ast *(Ast list option * Ast))
    | Proposition of Positions * (Ast *(Ast list option * Ast))
    | Corollary of Positions * (Ast * (Ast list option * Ast))
    | CorollarySignature of (Ast * Ast)
    | Conjecture of Positions * (Ast *(Ast list option * Ast))
    | NamedVarDecl of Positions * ((Ast list * Ast) * Ast) 
    | ParamTuple of Positions * Ast list
    | Signature of Positions * (Ast * Ast)
    | Mapping of Positions * Ast
    | SignatureWithUserDefinedString of Positions * ((Ast * Ast option) * Ast)
    | Axiom of Positions * (Ast * (Ast list option * Ast))
    | ParentConstructorCall of Positions * (Ast * Ast)
    | Constructor of Positions * (Ast * (Ast list option * Ast)) 
    | Property of Positions * unit
    | Optional of Positions * unit
    | PredicateInstance of Positions * ((Ast option * Ast) * Ast) 
    | FunctionalTermInstance of Positions * (Ast * Ast)
    | PropertyBlock of Positions * (Ast * Ast)
    | DefPredicateContent of Ast list option * Ast
    | DefinitionPredicate of Positions * (Ast * (Ast * Ast list option))
    | DefFunctionContent of Ast list option * Ast
    | FunctionalTermSignature of Positions * ((Ast option * Ast) * Ast)
    | DefinitionFunctionalTerm of Positions * (Ast * (Ast * Ast list option))
    | DefClassCompleteContent of Ast list option * Ast list
    | DefinitionClass of Positions * (((Ast * Ast option) * Ast list) * (Ast * Ast list option)) 

    // Proofs
    | Trivial of Positions * unit
    | Qed of Positions * unit
    | DerivedPredicate of Positions * Ast
    | AssumeArgument of Positions * Ast
    | RevokeArgument of Positions * Ast
    | JustArgInf of Positions * (Ast * Ast)
    | Argument of Positions * (Ast * Ast)
    | Proof of Positions * (Ast * (Ast list * Ast option))
    | Namespace of Ast list
    | AST of Positions * Ast
    | Error // used to replace the whole AST (at the root level) for severe errors the parser cannot recover from

