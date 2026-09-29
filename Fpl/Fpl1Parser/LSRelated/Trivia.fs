/// <summary>
/// Provides an exhaustive, purely structural traversal over the <c>Ast</c> discriminated union
/// used to collect the <c>Positions</c> of every position-bearing node in a parsed FPL syntax tree.
/// </summary>
/// <remarks>
/// This module exists to support comment-trivia attachment for a formatting service: after parsing,
/// the collected positions (sorted in source order) are matched against the list of comments
/// discovered during parsing so that each comment can be attached as leading/trailing trivia of the
/// nearest AST node. Unlike <c>Fpl2Interpreter.SymbolTable.Creation.Main.eval</c>, this traversal:
/// - has no dependency on interpreter state (the heap, evaluation frames, or scopes),
/// - never short-circuits or throws on semantically invalid input, and
/// - visits every node reachable from the root, regardless of semantic validity.
/// The case grouping below intentionally mirrors the grouping used in <c>Fpl1Parser.Types.Ast</c>
/// and in <c>Fpl2Interpreter.SymbolTable.Creation.Main.eval</c> to make the two traversals easy to
/// compare and keep in sync as the grammar evolves.
/// </remarks>
module Fpl1Parser.LSRelated.Trivia
open System.Collections.Generic
open Fpl1Parser.Types

/// <summary>
/// Recursively walks <paramref name="ast"/> and appends the <c>Positions</c> of every
/// position-bearing node to <paramref name="acc"/>, in a single left-to-right, depth-first pass.
/// </summary>
/// <param name="acc">Mutable accumulator collecting positions in traversal (source) order.</param>
/// <param name="ast">The AST node to visit.</param>
/// <returns>Unit. Positions are appended to <paramref name="acc"/> as a side effect.</returns>
/// <remarks>
/// Some <c>Ast</c> cases carry no <c>Positions</c> value (e.g. <c>Dot</c>, <c>Exists1</c>,
/// <c>VarDeclBlock</c>, <c>Namespace</c>). For these, the function still recurses into any child
/// <c>Ast</c> values but does not add an entry to <paramref name="acc"/>.
/// </remarks>
let rec collectPositions (acc: List<Positions>) (ast: Ast) =
    let add p = acc.Add p
    let opt f = function Some x -> f x | None -> ()
    let list f xs = xs |> List.iter f

    match ast with
    // Lexical / Leaf tokens
    | Alias(p, _) -> add p
    | Dot () -> ()
    | Star(p, _) -> add p
    | Digits _ -> ()
    | DollarDigits(p, _) -> add p
    | ObjectSymbolWithPos(p, _) -> add p
    | InfixSymbolWithPos(p, _) -> add p
    | PostFixSymbolWithPos(p, _) -> add p
    | PrefixSymbolWithPos(p, _) -> add p

    // Identifiers & identifier dispatchers
    | PascalCaseId(p, _) -> add p
    | BaseClassName(p, _) -> add p
    | PredicateIdentifier(p, _) -> add p
    | NamespaceIdentifier(p, asts) ->
        add p
        list (collectPositions acc) asts
    | ClassIdentifier(p, a) ->
        add p
        collectPositions acc a
    | AliasedNamespaceIdentifier(p, (a, aOpt)) ->
        add p
        collectPositions acc a
        opt (collectPositions acc) aOpt
    | ArgumentIdentifier(p, _) -> add p
    | RefArgumentIdentifier(p, _) -> add p
    | DelegateName(p, _) -> add p
    | ReferencingIdentifier(p, (a, asts)) ->
        add p
        collectPositions acc a
        list (collectPositions acc) asts

    // Types & type related constructs
    | IndexType(p, _) -> add p
    | FunctionalTermType(p, _) -> add p
    | ObjectType(p, _) -> add p
    | PredicateType(p, _) -> add p
    | TemplateType(p, _) -> add p
    | ArrayType(p, (a, asts)) ->
        add p
        collectPositions acc a
        list (collectPositions acc) asts
    | SimpleVariableType(p, a) ->
        add p
        collectPositions acc a
    | IndexAllowedType(p, a) ->
        add p
        collectPositions acc a
    | InheritedType(p, _) -> add p
    | InheritedTypeList asts ->
        list (collectPositions acc) asts
    | CompoundPredicateType(p, (a, aOpt)) ->
        add p
        collectPositions acc a
        opt (collectPositions acc) aOpt
    | CompoundFunctionalTermType(p, (a, tupOpt)) ->
        add p
        collectPositions acc a
        opt (fun (a1, a2) -> collectPositions acc a1; collectPositions acc a2) tupOpt

    // Variables
    | VarDeclBlock astsOpt ->
        opt (list (collectPositions acc)) astsOpt
    | NamedVarDecl(p, (asts, a)) ->
        add p
        list (collectPositions acc) asts
        collectPositions acc a
    | Var(p, _) -> add p

    // Predicates
    | True(p, _) -> add p
    | False(p, _) -> add p
    | And(p, (a1, a2)) ->
        add p
        collectPositions acc a1
        collectPositions acc a2
    | Or(p, (a1, a2)) ->
        add p
        collectPositions acc a1
        collectPositions acc a2
    | Xor(p, (a1, a2)) ->
        add p
        collectPositions acc a1
        collectPositions acc a2
    | Impl(p, (a1, a2)) ->
        add p
        collectPositions acc a1
        collectPositions acc a2
    | Iif(p, (a1, a2)) ->
        add p
        collectPositions acc a1
        collectPositions acc a2
    | Not(p, a) ->
        add p
        collectPositions acc a
    | All(p, (asts, a)) ->
        add p
        list (collectPositions acc) asts
        collectPositions acc a
    | Exists(p, (asts, a)) ->
        add p
        list (collectPositions acc) asts
        collectPositions acc a
    | Exists1 () -> ()
    | ExistsN(p, ((a1, asts), a2)) ->
        add p
        collectPositions acc a1
        list (collectPositions acc) asts
        collectPositions acc a2
    | IsOperator(p, (a1, a2)) ->
        add p
        collectPositions acc a1
        collectPositions acc a2

    // Expressions
    | PredicateWithQualification(a1, a2) ->
        collectPositions acc a1
        collectPositions acc a2
    | PredicateWithOptSpecification(p, (a, aOpt)) ->
        add p
        collectPositions acc a
        opt (collectPositions acc) aOpt
    | PrefixOp(a1, a2) ->
        collectPositions acc a1
        collectPositions acc a2
    | PostfixOp(a1, a2) ->
        collectPositions acc a1
        collectPositions acc a2
    | InfixOp(p, items) ->
        add p
        items |> List.iter (fun (a, aOpt) ->
            collectPositions acc a
            opt (collectPositions acc) aOpt)
    | Parens(p, a) ->
        add p
        collectPositions acc a

    // Tuple-like constructs and qualifies
    | BrackedCoordList(p, asts) ->
        add p
        list (collectPositions acc) asts
    | ArgumentTuple(p, asts) ->
        add p
        list (collectPositions acc) asts
    | DottedPredicate(p, a) ->
        add p
        collectPositions acc a
    | QualificationList(p, asts) ->
        add p
        list (collectPositions acc) asts
    | ParamTuple asts ->
        list (collectPositions acc) asts

    // Commands
    | Delegate(a1, a2) ->
        collectPositions acc a1
        collectPositions acc a2
    | Assertion(p, a) ->
        add p
        collectPositions acc a
    | Cases(p, (asts, a)) ->
        add p
        list (collectPositions acc) asts
        collectPositions acc a
    | CaseSingle(p, (a, asts)) ->
        add p
        collectPositions acc a
        list (collectPositions acc) asts
    | CaseElse(p, asts) ->
        add p
        list (collectPositions acc) asts
    | MapCases(p, (asts, a)) ->
        add p
        list (collectPositions acc) asts
        collectPositions acc a
    | MapCaseSingle(p, (a1, a2)) ->
        add p
        collectPositions acc a1
        collectPositions acc a2
    | MapCaseElse(p, a) ->
        add p
        collectPositions acc a
    | Assignment(p, (a1, a2)) ->
        add p
        collectPositions acc a1
        collectPositions acc a2
    | ForIn(p, ((a1, a2), asts)) ->
        add p
        collectPositions acc a1
        collectPositions acc a2
        list (collectPositions acc) asts
    | InEntity(p, a) ->
        add p
        collectPositions acc a
    | Return(p, a) ->
        add p
        collectPositions acc a

    // Symbol extensions
    | SymbolDecl(p, _) -> add p
    | PrefixDecl(p, _) -> add p
    | PostfixDecl(p, _) -> add p
    | InfixDeclWithPrecedence(p, (_, a)) ->
        add p
        collectPositions acc a
    | Precedence(p, _) -> add p
    | DefinitionExtension(p, ((a1, a2), a3)) ->
        add p
        collectPositions acc a1
        collectPositions acc a2
        collectPositions acc a3
    | ExtensionSignature(p, (a1, a2)) ->
        add p
        collectPositions acc a1
        collectPositions acc a2
    | ExtensionAssignment(p, (a1, a2)) ->
        add p
        collectPositions acc a1
        collectPositions acc a2
    | ExtensionRegex _ -> ()
    | ExtensionName(p, _) -> add p

    // Definitions
    | DefinitionClass(p, (((a1, a1Opt), a2Opt), a3)) ->
        add p
        collectPositions acc a1
        opt (collectPositions acc) a1Opt
        opt (collectPositions acc) a2Opt
        collectPositions acc a3
    | ClassSignature(p, a) ->
        add p
        collectPositions acc a
    | ClassDefinitionBlock(p, tupOpt) ->
        add p
        opt (fun (a, astsOpt) ->
            collectPositions acc a
            opt (list (collectPositions acc)) astsOpt) tupOpt
    | DefClassCompleteContent(a, asts) ->
        collectPositions acc a
        list (collectPositions acc) asts
    | Constructor(p, (a1, a2)) ->
        add p
        collectPositions acc a1
        collectPositions acc a2
    | ConstructorSignature(p, (a1, a2)) ->
        add p
        collectPositions acc a1
        collectPositions acc a2
    | ConstructorBlock a ->
        collectPositions acc a
    | BaseConstructorCall(p, (a1, a2)) ->
        add p
        collectPositions acc a1
        collectPositions acc a2

    | DefinitionPredicate(p, (a, tupOpt)) ->
        add p
        collectPositions acc a
        opt (fun (a1, astsOpt) ->
            collectPositions acc a1
            opt (list (collectPositions acc)) astsOpt) tupOpt
    | PredicateSignature((p, ((a1, a1Opt), a2)), a3Opt) ->
        add p
        collectPositions acc a1
        opt (collectPositions acc) a1Opt
        collectPositions acc a2
        opt (collectPositions acc) a3Opt
    | DefPredicateContent(a1, a2) ->
        collectPositions acc a1
        collectPositions acc a2

    | DefinitionFunctionalTerm(p, (a1, a2)) ->
        add p
        collectPositions acc a1
        collectPositions acc a2
    | FunctionalTermSignature((p, (((a1, a1Opt), a2), a3)), a4Opt) ->
        add p
        collectPositions acc a1
        opt (collectPositions acc) a1Opt
        collectPositions acc a2
        collectPositions acc a3
        opt (collectPositions acc) a4Opt
    | Mapping(p, a) ->
        add p
        collectPositions acc a
    | FunctionalTermDefinitionBlock(p, tupOpt) ->
        add p
        opt (fun (a, astsOpt) ->
            collectPositions acc a
            opt (list (collectPositions acc)) astsOpt) tupOpt
    | DefFunctionContent(a1, a2) ->
        collectPositions acc a1
        collectPositions acc a2

    | PredicateInstance(p, (a, aOpt)) ->
        add p
        collectPositions acc a
        opt (collectPositions acc) aOpt
    | PredicateInstanceSignature(p, (a1, a2)) ->
        add p
        collectPositions acc a1
        collectPositions acc a2
    | FunctionalTermInstance(p, (a, aOpt)) ->
        add p
        collectPositions acc a
        opt (collectPositions acc) aOpt
    | FunctionalTermInstanceSignature(p, ((a1, a2), a3)) ->
        add p
        collectPositions acc a1
        collectPositions acc a2
        collectPositions acc a3

    // Rules of inference
    | RuleOfInference(p, (a1, a2)) ->
        add p
        collectPositions acc a1
        collectPositions acc a2
    | RuleOfInferenceSignature(p, a) ->
        add p
        collectPositions acc a
    | PremiseConclusionBlock(a1, (a2, a3)) ->
        collectPositions acc a1
        collectPositions acc a2
        collectPositions acc a3
    | PremiseList(p, asts) ->
        add p
        list (collectPositions acc) asts

    // Statements
    | Axiom(p, (a1, (a2, a3))) ->
        add p
        collectPositions acc a1
        collectPositions acc a2
        collectPositions acc a3
    | AxiomSignature(p, a) ->
        add p
        collectPositions acc a
    | Conjecture(p, (a1, (a2, a3))) ->
        add p
        collectPositions acc a1
        collectPositions acc a2
        collectPositions acc a3
    | ConjectureSignature(p, a) ->
        add p
        collectPositions acc a
    | Theorem(p, (a1, (a2, a3))) ->
        add p
        collectPositions acc a1
        collectPositions acc a2
        collectPositions acc a3
    | TheoremSignature(p, a) ->
        add p
        collectPositions acc a
    | Lemma(p, (a1, (a2, a3))) ->
        add p
        collectPositions acc a1
        collectPositions acc a2
        collectPositions acc a3
    | LemmaSignature(p, a) ->
        add p
        collectPositions acc a
    | Proposition(p, (a1, (a2, a3))) ->
        add p
        collectPositions acc a1
        collectPositions acc a2
        collectPositions acc a3
    | PropositionSignature(p, a) ->
        add p
        collectPositions acc a
    | Corollary(p, (a1, (a2, a3))) ->
        add p
        collectPositions acc a1
        collectPositions acc a2
        collectPositions acc a3
    | CorollarySignature(p, (a, asts)) ->
        add p
        collectPositions acc a
        list (collectPositions acc) asts

    // Proofs
    | Proof(p, (a1, a2)) ->
        add p
        collectPositions acc a1
        collectPositions acc a2
    | ProofSignature(p, (a, asts)) ->
        add p
        collectPositions acc a
        list (collectPositions acc) asts
    | ProofBlock a ->
        collectPositions acc a
    | ProofContent((a1, asts), a2Opt) ->
        collectPositions acc a1
        list (collectPositions acc) asts
        opt (collectPositions acc) a2Opt
    | Argument(p, a) ->
        add p
        collectPositions acc a
    | JustArgInf(p, (a1, a2)) ->
        add p
        collectPositions acc a1
        collectPositions acc a2
    | StartArgument a ->
        collectPositions acc a
    | StartArgumentStictly(a, asts) ->
        collectPositions acc a
        list (collectPositions acc) asts
    | Justification(p, a) ->
        add p
        collectPositions acc a
    | JustificationItem(p, a) ->
        add p
        collectPositions acc a
    | ReferenceToProofOrCorollary(p, a) ->
        add p
        collectPositions acc a
    | ByDef(p, a) ->
        add p
        collectPositions acc a
    | JustificationIdentifier(p, (((_, a1), astsOpt), a2Opt)) ->
        add p
        collectPositions acc a1
        opt (list (collectPositions acc)) astsOpt
        opt (collectPositions acc) a2Opt
    | TrivialArgument(p, _) -> add p
    | DeriveArgument(p, a) ->
        add p
        collectPositions acc a
    | AssumeArgument(p, a) ->
        add p
        collectPositions acc a
    | RevokeArgument(p, a) ->
        add p
        collectPositions acc a
    | Qed(p, _) -> add p

    // Special references
    | Intrinsic(p, _) -> add p
    | Undefined(p, _) -> add p
    | SelfOrParent(p, a) ->
        add p
        collectPositions acc a
    | Self(p, _) -> add p
    | Parent(p, _) -> add p
    | Extension(p, _) -> add p

    // Localizations
    | Localization((p, a), asts) ->
        add p
        collectPositions acc a
        list (collectPositions acc) asts
    | TranslationTermList(p, asts) ->
        add p
        list (collectPositions acc) asts
    | TranslationTerm(p, asts) ->
        add p
        list (collectPositions acc) asts
    | Language(p, (a1, a2)) ->
        add p
        collectPositions acc a1
        collectPositions acc a2
    | LanguageCode(p, _) -> add p
    | LocalizationString(p, _) -> add p

    // TopLevel
    | AST(p, a) ->
        add p
        collectPositions acc a
    | Namespace asts ->
        list (collectPositions acc) asts
    | UsesClause(p, a) ->
        add p
        collectPositions acc a
    | BuildingBlock(p, a) ->
        add p
        collectPositions acc a
    | ErrorSyntax(p, _) -> add p
    | ErrorSyntaxBacktracking(p, _) -> add p
    | ErrorSyntaxChain((p, _), _) -> add p

/// <summary>
/// Convenience entry point that walks the entire tree rooted at <paramref name="ast"/> and returns
/// all collected <c>Positions</c> in source (traversal) order.
/// </summary>
/// <param name="ast">Root AST node, typically the top-level <c>Ast.AST</c> node returned by the parser.</param>
/// <returns>List of <c>Positions</c> for every position-bearing node reachable from <paramref name="ast"/>.</returns>
let getAllPositions (ast: Ast) : Positions list =
    let acc = List<Positions>()
    collectPositions acc ast
    acc |> List.ofSeq
