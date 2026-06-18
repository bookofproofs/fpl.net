/// This modules provides AST types and helper types for the FPL grammar.

module Fpl.Parser.Types
open FParsec

(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)

/// Provides starting and ending position for each Ast node since we
/// will need these information for the diagnostics of the interpreter even after the 
/// parsing was done
type Positions = Position * Position 

type Ast = 
    // Lexical / Leaf tokens
    | Alias of Positions * string
    | Dot of unit
    | Star of Positions * unit
    | Digits of string
    | DollarDigits of Positions * uint
    | ExtensionRegex of string
    | ExtensionName of Positions * string
    | LanguageCode of Positions * string
    | LocalizationString of Positions * string
    | PrefixDecl of Positions * string
    | PostfixDecl of Positions * string
    | SymbolDecl of Positions * string
    | ObjectSymbolWithPos of Positions * string
    | InfixSymbolWithPos of Positions * string
    | PostFixSymbolWithPos of Positions * string
    | PrefixSymbolWithPos of Positions * string

    // Identifiers & identifier dispatchers
    | PascalCaseId of Positions * string
    | BaseClassName of Positions * string
    | PredicateIdentifier of Positions * string
    | NamespaceIdentifier of Positions * Ast list
    | ClassIdentifier of Positions * Ast
    | AliasedNamespaceIdentifier of Positions * (Ast * Ast option)
    | ArgumentIdentifier of Positions * string
    | RefArgumentIdentifier of Positions * string
    | DelegateName of Positions * string
    | ReferencingIdentifier of Positions * (Ast * Ast list)

    // Types & type related constructs
    | IndexType of Positions * unit
    | FunctionalTermType of Positions * unit
    | ObjectType of Positions * unit
    | PredicateType of Positions * unit
    | TemplateType of Positions * string
    | ArrayType of Positions * (Ast * Ast list)
    | SimpleVariableType of Positions * Ast 
    | IndexAllowedType of Positions * Ast 
    | InheritedType of Positions * string 
    | InheritedTypeList of Ast list
    | CompoundPredicateType of Positions * (Ast * Ast option)
    | CompoundFunctionalTermType of Positions * (Ast * (Ast * Ast) option)

    // Predicates
    | True of Positions * unit
    | False of Positions * unit 
    | And of Positions * (Ast * Ast)
    | Or of Positions * (Ast * Ast)
    | Xor of Positions * (Ast * Ast)
    | Impl of Positions * (Ast * Ast)
    | Iif of Positions * (Ast * Ast)
    | Not of Positions * Ast
    | All of Positions * (Ast list * Ast) 
    | Exists of Positions * (Ast list * Ast) 
    | Exists1 of unit
    | ExistsN of Positions * ((Ast * Ast list) * Ast)
    | IsOperator of Positions * (Ast * Ast)

    // Expressions
    | PredicateWithQualification of (Ast * Ast)
    | PredicateWithOptSpecification of Positions * (Ast * Ast option)
    | PrefixOp of Ast * Ast // operator * operand
    | PostfixOp of Ast * Ast // operator * operand
    | InfixOp of Positions * ((Ast * Ast option) list) // (operand * operator option) list
    | Parens of Positions * Ast // (a)

    // Tuple-like constructs and qualifies
    | BrackedCoordList of Positions * Ast list
    | ArgumentTuple of Positions * Ast list 
    | DottedPredicate of Positions * Ast 
    | QualificationList of Positions * Ast list
    | ParamTuple of Ast list 

    // Commands 
    | Delegate of Ast * Ast
    | Assertion of Positions * Ast
    | Cases of Positions * (Ast list * Ast)
    | CaseSingle of Positions * (Ast * Ast list)
    | CaseElse of Positions * Ast list 
    | MapCases of Positions * (Ast list * Ast)
    | MapCaseSingle of Positions * (Ast * Ast)
    | MapCaseElse of Positions * Ast  
    | Assignment of Positions * (Ast * Ast)
    | ForIn of Positions * ((Ast * Ast) * Ast list)
    | InEntity of Positions * Ast
    | Return of Positions * Ast

    // Definitions
    | DefinitionClass of Positions * (((Ast * Ast option) * Ast option) * Ast) 
    | ClassSignature of Positions * Ast
    | ClassDefinitionBlock of Positions * (Ast * Ast list option) option
    | DefClassCompleteContent of Ast * Ast list
    | Constructor of Positions * (Ast * Ast) 
    | ConstructorSignature of Positions * (Ast * Ast)
    | ConstructorBlock of Ast
    | BaseConstructorCall of Positions * (Ast * Ast)

    | DefinitionPredicate of Positions * (Ast * (Ast * Ast list option) option)
    | PredicateSignature of (Positions * ((Ast * Ast option) * Ast)) * Ast option
    | DefPredicateContent of Ast * Ast

    | DefinitionFunctionalTerm of Positions * (Ast * Ast)
    | FunctionalTermSignature of (Positions * (((Ast * Ast option) * Ast) * Ast)) * Ast option
    | Mapping of Positions * Ast
    | FunctionalTermDefinitionBlock of Positions * (Ast * Ast list option) option
    | DefFunctionContent of Ast * Ast

    | PredicateInstance of Positions * (Ast * Ast option)
    | PredicateInstanceSignature of Positions * (Ast * Ast)
    | FunctionalTermInstance of Positions * (Ast * Ast option)
    | FunctionalTermInstanceSignature of Positions * ((Ast * Ast) * Ast)

    | DefinitionExtension of Positions * ((Ast * Ast) * Ast)
    | ExtensionSignature of Positions * (Ast * Ast)
    | ExtensionAssignment of Positions * (Ast * Ast) 

    // Rules of inferences
    | RuleOfInference of Positions * (Ast * Ast)
    | RuleOfInferenceSignature of Positions * Ast
    | PremiseConclusionBlock of Ast * (Ast * Ast)
    | PremiseList of Positions * Ast list

    // Statements
    | Axiom of Positions * (Ast * (Ast * Ast))
    | AxiomSignature of Positions * Ast
    | Conjecture of Positions * (Ast * (Ast * Ast))
    | ConjectureSignature of Positions * Ast
    | Theorem of Positions * (Ast * (Ast * Ast))
    | TheoremSignature of Positions * Ast
    | Lemma of Positions * (Ast * (Ast * Ast))
    | LemmaSignature of Positions * Ast
    | Proposition of Positions * (Ast * (Ast * Ast))
    | PropositionSignature of Positions * Ast
    | Corollary of Positions * (Ast * (Ast * Ast))
    | CorollarySignature of Positions * (Ast * Ast list)

    // Proofs
    | Proof of Positions * (Ast * Ast)
    | ProofSignature of Positions * (Ast * Ast list)
    | ProofBlock of Ast
    | ProofContent of (Ast * Ast list) * Ast option
    | Argument of Positions * Ast
    | JustArgInf of Positions * (Ast * Ast)
    | StartArgument of Ast 
    | StartArgumentStictly of Ast * Ast list
    | Justification of Positions * Ast
    | JustificationItem of Positions * Ast 
    | ReferenceToProofOrCorollary of Positions * Ast
    | ByDef of Positions * Ast 
    | JustificationIdentifier of Positions * (((string option * Ast) * Ast list option) * Ast option) 
    | TrivialArgument of Positions * unit
    | DeriveArgument of Positions * Ast
    | AssumeArgument of Positions * Ast
    | RevokeArgument of Positions * Ast
    | Qed of Positions * unit

    // TopLevel
    | AST of Positions * Ast
    | Namespace of Ast list
    | UsesClause of Positions * Ast
    | BuildingBlock of Positions * Ast
    | ErrorSyntax of Positions * string 
    | ErrorSyntaxBacktracking of Positions * string 
    | ErrorSyntaxChain of (Positions * Position) * (string * string)


    | InfixDeclWithPrecedence of Positions * (string * Ast) // infix symbol with precedence
    | Precedence of Positions * int

    | Intrinsic of Positions * unit
    | Undefined of Positions * unit
    | SelfOrParent of Positions * Ast
    | Self of Positions * unit
    | Parent of Positions * unit
    | Extension of Positions * string

    | Localization of (Positions * Ast) * Ast list
    | TranslationTermList of Positions * Ast list
    | TranslationTerm of Positions * Ast list
    | Language of Positions * (Ast * Ast)


    // Variables
    | Var of Positions * string
    | VarDeclBlock of Ast list option
    | NamedVarDecl of Positions * (Ast list * Ast)


    







