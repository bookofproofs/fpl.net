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
    | PascalCaseId of Positions * string
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
    | ProofSignature of Positions * (Ast * Ast list)

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
    | InheritedClassType of Positions * Ast
    | ClassType of Positions * Ast
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
    | ExistsN of Positions * ((Ast * Ast list) * Ast)
    | IsOperator of Positions * (Ast * Ast)
    | Delegate of Positions * (Ast * Ast)
    | ArgumentTuple of Positions * Ast list
    | PredicateWithOptSpecification of Positions * (Ast * Ast option)
    | DottedPredicate of Positions * Ast 
    | QualificationList of Positions * Ast list
    | PredicateWithQualification of (Ast * Ast) 
        
    // Expressions
    | ObjectSymbol of Positions * string
    | InfixOperator of Positions * string
    | PostfixOperator of Positions * string
    | PrefixOperator of Positions * string
    | InfixOperation of Positions * (Ast * Ast option) list
    | Expression of Positions * ((((Ast option * Ast) * Ast option) * Ast option) * Ast)

    // Statements
    | Assertion of Positions * Ast
    | Cases of Positions * (Ast list * Ast)
    | CaseSingle of Positions * (Ast * Ast list)
    | CaseElse of Positions * Ast list 
    | MapCases of Positions * (Ast list * Ast)
    | MapCaseSingle of Positions * (Ast * Ast)
    | MapCaseElse of Positions * Ast  
    | Assignment of Positions * (Ast * Ast)
    | ForIn of Positions * ((Ast * Ast) * Ast list)
    | Return of Positions * Ast

    // FPL Blocks
    | Intrinsic of Positions * unit
    | VarDeclBlock of Positions * Ast list 
    | StatementList of Positions * Ast list
    | PremiseList of Positions * Ast list
    | PremiseConclusionBlock of Positions * ((Ast list option * Ast) * Ast)
    | RuleOfInferenceSignature of Positions * Ast
    | RuleOfInference of Positions * (Ast * Ast)
    | Localization of Positions * (Ast * Ast list)
    | TheoremSignature of Positions * Ast
    | Theorem of Positions * (Ast * (Ast list option * Ast))
    | LemmaSignature of Positions * Ast
    | Lemma of Positions * (Ast *(Ast list option * Ast))
    | PropositionSignature of Positions * Ast
    | Proposition of Positions * (Ast *(Ast list option * Ast))
    | Corollary of Positions * (Ast * (Ast list option * Ast))
    | CorollarySignature of Positions * (Ast * Ast list)
    | ConjectureSignature of Positions * Ast
    | Conjecture of Positions * (Ast *(Ast list option * Ast))
    | NamedVarDecl of Positions * ((Ast list * Ast) * Ast) 
    | ParamTuple of Positions * Ast list
    | Mapping of Positions * Ast
    | AxiomSignature of Positions * Ast
    | Axiom of Positions * (Ast * (Ast list option * Ast))
    | BaseConstructorCall of Positions * (Ast * Ast)
    | ConstructorSignature of Positions * (Ast * Ast)
    | PredicateInstanceSignature of Positions * (Ast * Ast)
    | FunctionalTermInstanceSignature of Positions * ((Ast * Ast) * Ast)
    | Constructor of Positions * (Ast * (Ast list option)) 
    | PredicateInstance of Positions * (unit option * (Ast * Ast))
    | FunctionalTermInstance of Positions * ((unit option * Ast) * Ast)
    | DefPredicateContent of Ast list option * Ast
    | DefinitionPredicate of Positions * (Ast * (Ast * Ast list option))
    | DefFunctionContent of Ast list option * Ast
    | PredicateSignature of Positions * ((Ast * Ast) * Ast option)
    | ClassSignature of Positions * Ast
    | FunctionalTermSignature of Positions * (((Ast * Ast) * Ast) * Ast option)
    | DefinitionFunctionalTerm of Positions * (Ast * (Ast * Ast list option))
    | DefClassCompleteContent of Ast list option * Ast list
    | DefinitionClass of Positions * (((Ast * Ast list) * Ast option) * (Ast * Ast list option)) 
    | Prefix of Positions * string
    | Precedence of Positions * int
    | Infix of Positions * (string * Ast)
    | Postfix of Positions * string
    | Symbol of Positions * string

    // Proofs
    | ArgumentIdentifier of Positions * string
    | RefArgumentIdentifier of Positions * string
    | ReferenceToProofOrCorollary of Positions * Ast
    | JustificationItem of Positions * Ast 
    | Justification of Positions * Ast list
    | ByDef of Positions * Ast 
    | JustificationIdentifier of Positions * (((string option * Ast) * Ast list option) * Ast option) 
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

